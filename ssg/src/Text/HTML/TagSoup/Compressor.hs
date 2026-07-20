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
    -- Fold over the tag stream while carrying a `Set` of the element names we
    -- are currently "inside", so we know when a text node's whitespace is
    -- significant (see `insideSignificant`) and must be preserved.
    go :: Set.Set String -> [TS.Tag String] -> [TS.Tag String]
    go stack =
      \case
        [] -> []

        -- Remove an HTML comment by *not* prepending the tag and, instead,
        -- continuing on with the rest of the tags.
        (TS.TagComment _ : rest) ->
          go stack rest

        -- On an open tag, like `<div>`, prepend it and continue, pushing its
        -- (lower-cased) name onto the stack of elements we are inside.
        (tag@(TS.TagOpen name _) : rest) ->
          tag : go (Set.insert (lower name) stack) rest

        -- On a closing tag, like `</div>`, prepend it and continue, popping its
        -- name back off the stack.
        (tag@(TS.TagClose name) : rest) ->
          tag : go (Set.delete (lower name) stack) rest

        -- On a text node: if it sits inside a whitespace-sensitive element,
        -- prepend it unchanged; otherwise, clean up its whitespace first.
        (tag@(TS.TagText _) : rest)
          | insideSignificant stack -> tag : go stack rest
          | otherwise               -> fmap cleanWhitespace tag : go stack rest

        -- Anything else is unexpected, so prepend it without change.
        (tag : rest) ->
          tag : go stack rest

    lower :: String -> String
    lower = map toLower

    -- Elements whose whitespace is significant and must be preserved verbatim.
    -- `script`/`style` matter too: collapsing newlines inside inline JS can
    -- swallow a `//` line comment or change automatic-semicolon-insertion.
    insideSignificant :: Set.Set String -> Bool
    insideSignificant stack =
      any (`Set.member` stack) [ "pre", "script", "style", "textarea" ]

    cleanWhitespace :: String -> String
    cleanWhitespace " " = " "
    cleanWhitespace str = cleanSurroundingWhitespace str (cleanHtmlWhitespace str)
      where
        -- The whitespace we treat as insignificant:
        --   ' '  (space)
        --   '\f' (form feed)
        --   '\n' (newline / line feed)
        --   '\r' (carriage return)
        --   '\v' (vertical tab)
        -- Deliberately narrower than `Data.Char.isSpace` so non-breaking spaces
        -- and similar are left alone.
        isSpaceOrNewLineIsh :: Char -> Bool
        isSpaceOrNewLineIsh = (`elem` (" \f\n\r\v" :: String))

        -- Collapse internal runs of whitespace down to single spaces.
        cleanHtmlWhitespace :: String -> String
        cleanHtmlWhitespace = unwords . words'
          where
            -- Like `words`, but splitting on `isSpaceOrNewLineIsh` rather than
            -- `isSpace`, so we don't drop the whitespace we mean to keep.
            -- https://hackage.haskell.org/package/base/docs/src/Data.OldList.html#words
            words' :: String -> [String]
            words' s = case dropWhile isSpaceOrNewLineIsh s of
              "" -> []
              s' -> w : words' s''
                where (w, s'') = break isSpaceOrNewLineIsh s'

        -- After trimming, re-add a single leading/trailing space when the
        -- original text had surrounding whitespace, so adjacent inline elements
        -- do not run together (e.g. @<a>x</a> <b>y</b>@).
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
