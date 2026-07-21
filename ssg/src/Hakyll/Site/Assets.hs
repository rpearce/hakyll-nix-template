module Hakyll.Site.Assets
  ( Manifest
  , buildManifest
  , versionAssetsCompiler
  ) where

--------------------------------------------------------------------------------

import qualified Control.Monad as Monad
import           Data.Bits (xor, (.&.))
import qualified Data.ByteString as BS
import           Data.List (dropWhileEnd, isPrefixOf, stripPrefix)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Word (Word64)
import qualified Hakyll as H
import           Numeric (showHex)
import qualified System.Directory as Dir
import           System.FilePath (makeRelative, (</>))
import qualified Text.HTML.TagSoup as TS

--------------------------------------------------------------------------------

-- | A map from an output asset path (e.g. @css/default.css@) to a short,
-- content-derived token used for cache-busting query strings.
type Manifest = Map.Map FilePath String

--------------------------------------------------------------------------------

-- | Build a `Manifest` by fingerprinting the site's static assets. Source
-- assets are read from the given provider directory (the @css@, @js@, @images@,
-- @fonts@, and @pdfs@ subdirectories, plus @favicon.ico@; missing directories
-- are skipped); @generated@ carries assets
-- Hakyll produces at build time (e.g. the highlight stylesheet) as
-- @(outputPath, content)@ pairs so they can be fingerprinted too.
--
-- Source files are hashed as-is. That's a deterministic preimage of the served
-- bytes (e.g. @css@ is compressed on the way out), so a source change still
-- moves the token even though the token isn't a hash of the exact shipped file.
-- The manifest is a build-time snapshot: under @hakyll-site watch@ you must
-- restart to pick up an edited asset's new hash.
buildManifest :: FilePath -> [(FilePath, String)] -> IO Manifest
buildManifest providerDir generated = do
  dirHashes <- concat <$> mapM hashDir [ "css", "js", "images", "fonts", "pdfs" ]
  favHashes <- hashFile "favicon.ico"
  let genHashes = [ (key, contentHash (TE.encodeUtf8 (T.pack content))) | (key, content) <- generated ]
  pure $ Map.fromList (dirHashes ++ favHashes ++ genHashes)
  where
    -- Hash every file under `providerDir/sub`, keyed by its path relative to
    -- the provider directory (i.e. its eventual output path). Missing
    -- directories are simply skipped.
    hashDir sub = do
      let dir = providerDir </> sub
      exists <- Dir.doesDirectoryExist dir
      if not exists
        then pure []
        else do
          files <- filesRecursive dir
          mapM (\f -> ((,) (makeRelative providerDir f) . contentHash) <$> BS.readFile f) files

    hashFile rel = do
      let f = providerDir </> rel
      exists <- Dir.doesFileExist f
      if not exists
        then pure []
        else (\bytes -> [ (rel, contentHash bytes) ]) <$> BS.readFile f

filesRecursive :: FilePath -> IO [FilePath]
filesRecursive dir = do
  entries <- Dir.listDirectory dir
  fmap concat . Monad.forM entries $ \entry -> do
    let path = dir </> entry
    -- Skip symlinks entirely: a symlinked directory could form a cycle and hang
    -- the build, and symlinked assets are vanishingly rare in an asset tree.
    isSymlink <- Dir.pathIsSymbolicLink path
    if isSymlink
      then pure []
      else do
        isDir <- Dir.doesDirectoryExist path
        if isDir then filesRecursive path else pure [ path ]

--------------------------------------------------------------------------------

-- | A compiler step that rewrites rendered HTML to cache-bust asset URLs. It
-- appends @?v=<hash>@ to every @href@/@src@ pointing at an asset in the
-- `Manifest`, and to the @content@ of @og:image@/@twitter:image@ tags. Because
-- it runs on the final page it covers CSS/JS from templates, images from post
-- content, and social-card images alike, with no per-asset template plumbing.
--
-- The site @root@ is passed so absolute social-image URLs can be matched — and
-- rebuilt cleanly, so a stray @./@ in the @image@ metadata (which would produce
-- e.g. @https://site.com./images/x@) can't corrupt them.
versionAssetsCompiler :: String -> Manifest -> H.Item String -> H.Compiler (H.Item String)
versionAssetsCompiler root manifest =
  pure . fmap (H.withTagList (versionAssets root manifest))

versionAssets :: String -> Manifest -> [TS.Tag String] -> [TS.Tag String]
versionAssets root manifest = map versionTag
  where
    versionTag (TS.TagOpen name attrs)
      | isImageMeta attrs = TS.TagOpen name (map versionContent attrs)
      | otherwise         = TS.TagOpen name (map versionLink attrs)
    versionTag tag = tag

    -- og:image / twitter:image carry the asset URL in their @content@ attribute.
    -- Matched by both @property@ and @name@, since Open Graph uses @property@
    -- while Twitter's canonical markup uses @name@.
    isImageMeta attrs =
      any (`elem` [ "og:image", "twitter:image" ])
        [ v | k <- [ "property", "name" ], Just v <- [ lookup k attrs ] ]

    -- Page links (@href@/@src@): keep the reference as written, append the
    -- token. Only these two attributes are handled — not @srcset@, @<source>@,
    -- @poster@, or CSS @url(...)@.
    versionLink (key, value)
      | key `elem` [ "href", "src" ]
      , Just (_, token) <- lookupAsset value = (key, value ++ "?v=" ++ token)
      | otherwise                            = (key, value)

    -- Social images: rebuild as a clean absolute URL, then append the token.
    versionContent (key, value)
      | key == "content"
      , Just (path, token) <- lookupAsset value = (key, absolute path ++ "?v=" ++ token)
      | otherwise                               = (key, value)

    -- @root@ joined to a manifest path, tolerating a trailing slash on a
    -- user-edited @siteRoot@ (so @".../"@ doesn't yield a @//@).
    absolute path = dropWhileEnd (== '/') root ++ "/" ++ path

    -- The manifest key a URL refers to (paired with its token), if any.
    -- External or unknown URLs return Nothing and are left untouched.
    lookupAsset :: String -> Maybe (FilePath, String)
    lookupAsset url =
      let path = toKey url
      in (,) path <$> Map.lookup path manifest

    -- Reduce a reference to its manifest key, tolerating a leading site root, a
    -- @./@ prefix, and leading slashes. Does not split a @?query@/@#fragment@,
    -- so a pre-parameterised URL simply won't match and is left as-is.
    toKey = dropWhile (== '/') . dropDotSlash . stripRoot
    stripRoot s    = fromMaybe s (stripPrefix root s)
    dropDotSlash s = if "./" `isPrefixOf` s then drop 2 s else s

--------------------------------------------------------------------------------

-- | A short, deterministic content fingerprint (FNV-1a, low 32 bits as hex).
-- Deliberately not `Data.Hashable`, whose salted hash changes between runs and
-- would bust every cache on every build.
contentHash :: BS.ByteString -> String
contentHash bytes =
  pad 8 (showHex (fnv1a bytes .&. 0xffffffff) "")
  where
    pad n s   = replicate (n - length s) '0' ++ s
    fnv1a     = BS.foldl' (\h b -> (h `xor` fromIntegral b) * fnvPrime) fnvOffset
    fnvOffset = 14695981039346656037 :: Word64
    fnvPrime  = 1099511628211        :: Word64
