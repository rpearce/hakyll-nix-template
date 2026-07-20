{-# LANGUAGE LambdaCase #-}

module Text.HTML.TagSoup.Compressor (compress) where

--------------------------------------------------------------------------------
import           Data.Char (toLower)
import qualified Data.Set as Set
import qualified Text.HTML.TagSoup as TS

--------------------------------------------------------------------------------
{- | Compress a stream of TagSoup tags by dropping HTML comments and collapsing
     runs of insignificant whitespace. The contents of whitespace-sensitive
     elements (@pre@, @textarea@, @script@, @style@) are left untouched.

__Examples__ (with @parse = 'TS.parseTags'@ and @render = 'TS.renderTags'@):

@
> render . compress . parse $ "<p>  hello   world  </p>"
"<p> hello world </p>"

> render . compress . parse $ "<pre>  keep\\n  me  </pre>"
"<pre>  keep\\n  me  </pre>"

> render . compress . parse $ "<p>a</p><!-- gone --><p>b</p>"
"<p>a</p><p>b</p>"
@
-}
compress :: [TS.Tag String] -> [TS.Tag String]
compress = go Set.empty
  where
    go :: Set.Set String -> [TS.Tag String] -> [TS.Tag String]
    go stack =
      \case
        [] -> []

        -- Drop HTML comments: skip the tag and continue.
        (TS.TagComment _ : rest) ->
          go stack rest

        -- Track which elements we are currently inside by pushing the
        -- (lower-cased) name on open...
        (tag@(TS.TagOpen name _) : rest) ->
          tag : go (Set.insert (lower name) stack) rest

        -- ...and popping it back off on close.
        (tag@(TS.TagClose name) : rest) ->
          tag : go (Set.delete (lower name) stack) rest

        -- Leave text inside whitespace-sensitive elements alone; collapse
        -- insignificant whitespace everywhere else.
        (tag@(TS.TagText _) : rest)
          | insideSignificant stack -> tag : go stack rest
          | otherwise               -> fmap cleanWhitespace tag : go stack rest

        -- Anything else passes through unchanged.
        (tag : rest) ->
          tag : go stack rest

    lower :: String -> String
    lower = map toLower

    -- Elements whose whitespace-significant content must be preserved verbatim.
    insideSignificant :: Set.Set String -> Bool
    insideSignificant stack =
      any (`Set.member` stack) [ "pre", "script", "style", "textarea" ]

    cleanWhitespace :: String -> String
    cleanWhitespace " " = " "
    cleanWhitespace str = cleanSurroundingWhitespace str (cleanHtmlWhitespace str)
      where
        -- Space, form feed, newline, carriage return, vertical tab. (Kept
        -- narrow on purpose so non-breaking spaces etc. survive.)
        isSpaceOrNewLineIsh :: Char -> Bool
        isSpaceOrNewLineIsh = (`elem` (" \f\n\r\v" :: String))

        -- Collapse internal runs of whitespace down to single spaces.
        cleanHtmlWhitespace :: String -> String
        cleanHtmlWhitespace = unwords . words'
          where
            words' :: String -> [String]
            words' s = case dropWhile isSpaceOrNewLineIsh s of
              "" -> []
              s' -> w : words' s''
                where (w, s'') = break isSpaceOrNewLineIsh s'

        -- Re-add a single leading/trailing space when the original text had
        -- surrounding whitespace, so adjacent inline elements do not run
        -- together (e.g. @<a>x</a> <b>y</b>@).
        cleanSurroundingWhitespace :: String -> String -> String
        cleanSurroundingWhitespace _ "" = ""
        cleanSurroundingWhitespace original trimmed =
          spaceWhen startsWithSpace ++ trimmed ++ spaceWhen endsWithSpace
          where
            spaceWhen p = if p then " " else ""
            startsWithSpace = case original of
              (c : _) -> isSpaceOrNewLineIsh c
              _       -> False
            endsWithSpace = case reverse original of
              (c : _) -> isSpaceOrNewLineIsh c
              _       -> False
