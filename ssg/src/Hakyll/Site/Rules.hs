{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module Hakyll.Site.Rules
  ( codeCss
  , codeCssContent
  , copy
  , css
  , index
  , nojekyll
  , posts
  , templates
  ) where

--------------------------------------------------------------------------------

import qualified Hakyll as H
import qualified Hakyll.Site.Assets as HSAssets
import qualified Hakyll.Site.Configuration as HSConfig
import qualified Hakyll.Site.Post as HSPost
import qualified Text.HTML.TagSoup.Compressor as TSCompressor
import qualified Text.Pandoc as Pandoc
import qualified Text.Pandoc.Highlighting as PandocHighlight

--------------------------------------------------------------------------------

copy :: H.Rules ()
copy = do
  H.route H.idRoute
  H.compile H.copyFileCompiler

css :: H.Rules ()
css = do
  H.route H.idRoute
  H.compile H.compressCssCompiler

templates :: H.Rules ()
templates =
  H.compile H.templateBodyCompiler

--------------------------------------------------------------------------------
-- INDEX

index :: HSAssets.Manifest -> H.Rules ()
index manifest = do
  H.route H.idRoute
  H.compile $ do
    loadedPosts <- H.recentFirst =<< H.loadAll "posts/*"

    let indexCtx = H.listField "posts" HSPost.postCtx (return loadedPosts)
                <> H.constField "root" HSConfig.mySiteRoot
                <> H.constField "feedTitle" HSConfig.myFeedTitle
                <> H.constField "siteName" HSConfig.mySiteName
                <> H.defaultContext

    H.getResourceBody
      >>= H.applyAsTemplate indexCtx
      >>= H.loadAndApplyTemplate "templates/default.html" indexCtx
      >>= HSAssets.versionAssetsCompiler HSConfig.mySiteRoot manifest
      >>= compressHtmlCompiler

--------------------------------------------------------------------------------
-- POSTS

posts :: HSAssets.Manifest -> H.Rules ()
posts manifest = do
  let ctx = H.constField "type" "article" <> HSPost.postCtx
  H.route $ H.metadataRoute HSPost.titleRoute
  H.compile $
    pandocCompilerCustom
      >>= H.saveSnapshot "content"
      >>= H.loadAndApplyTemplate "templates/post.html" ctx
      >>= H.loadAndApplyTemplate "templates/default.html" ctx
      >>= HSAssets.versionAssetsCompiler HSConfig.mySiteRoot manifest
      >>= compressHtmlCompiler

--------------------------------------------------------------------------------
-- ASSETS

-- | The generated syntax-highlighting stylesheet, as a string. Exposed so the
-- asset manifest can fingerprint it alongside the source assets.
codeCssContent :: String
codeCssContent =
  H.compressCss (PandocHighlight.styleToCss pandocHighlightStyle)

-- | Generate the syntax-highlighting stylesheet from the pandoc highlight style
-- (so code blocks are themed with a class-based stylesheet, no inline styles).
codeCss :: H.Rules ()
codeCss = do
  H.route H.idRoute
  H.compile $ H.makeItem codeCssContent

-- | Emit an empty .nojekyll so GitHub Pages serves the generated output
-- verbatim instead of running it through Jekyll.
nojekyll :: H.Rules ()
nojekyll = do
  H.route H.idRoute
  H.compile $ H.makeItem ("" :: String)

--------------------------------------------------------------------------------
-- PANDOC

pandocCompilerCustom :: H.Compiler (H.Item String)
pandocCompilerCustom =
  H.pandocCompilerWith pandocReaderOpts pandocWriterOpts

pandocReaderOpts :: Pandoc.ReaderOptions
pandocReaderOpts =
  H.defaultHakyllReaderOptions
    { Pandoc.readerExtensions = pandocExtensionsCustom
    }

pandocWriterOpts :: Pandoc.WriterOptions
pandocWriterOpts =
  H.defaultHakyllWriterOptions
    { Pandoc.writerExtensions = pandocExtensionsCustom
    , Pandoc.writerHighlightStyle = Just pandocHighlightStyle
    }

pandocExtensionsCustom :: Pandoc.Extensions
pandocExtensionsCustom =
  Pandoc.githubMarkdownExtensions
    <> Pandoc.extensionsFromList
      [ Pandoc.Ext_fenced_code_attributes
      , Pandoc.Ext_gfm_auto_identifiers
      , Pandoc.Ext_implicit_header_references
      , Pandoc.Ext_smart
      , Pandoc.Ext_footnotes
      ]

-- https://hackage.haskell.org/package/pandoc/docs/Text-Pandoc-Highlighting.html
pandocHighlightStyle :: PandocHighlight.Style
pandocHighlightStyle =
  PandocHighlight.breezeDark

--------------------------------------------------------------------------------
-- HTML COMPRESSION

compressHtmlCompiler :: H.Item String -> H.Compiler (H.Item String)
compressHtmlCompiler = pure . fmap compressHtml

compressHtml :: String -> String
compressHtml = H.withTagList TSCompressor.compress
