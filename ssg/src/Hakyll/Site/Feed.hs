{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module Hakyll.Site.Feed (createRss, createAtom) where

--------------------------------------------------------------------------------

import qualified Data.Maybe as Maybe
import qualified Hakyll as H
import qualified Hakyll.Site.Configuration as HSConfig
import qualified Hakyll.Site.CustomFields as HSCustomFields
import qualified Hakyll.Site.Post as HSPost

--------------------------------------------------------------------------------

createRss :: H.Rules ()
createRss = do
  H.route   H.idRoute
  H.compile compileRss

createAtom :: H.Rules ()
createAtom = do
  H.route   H.idRoute
  H.compile compileAtom

--------------------------------------------------------------------------------

compileRss :: H.Compiler (H.Item String)
compileRss =
  H.renderRss HSConfig.feedConfiguration rssCtx
    =<< H.recentFirst
    =<< H.loadAllSnapshots "posts/*" "content"

compileAtom :: H.Compiler (H.Item String)
compileAtom =
  H.renderAtom HSConfig.feedConfiguration atomCtx
    =<< H.recentFirst
    =<< H.loadAllSnapshots "posts/*" "content"

--------------------------------------------------------------------------------

rssCtx :: H.Context String
rssCtx =
  HSCustomFields.updatedField "updated" "%a, %d %b %Y %H:%M:%S UT"
    <> H.field "title" updatedTitle
    <> HSPost.postCtx
    <> H.bodyField "description"

atomCtx :: H.Context String
atomCtx =
  HSCustomFields.updatedField "updated" "%Y-%m-%dT%H:%M:%SZ"
    <> H.field "title" updatedTitle
    <> HSPost.postCtx
    <> H.bodyField "description"

--------------------------------------------------------------------------------
-- TITLE HELPERS

-- | Escape the XML metacharacters that would otherwise produce invalid RSS/Atom
-- feeds. The @&@ pass must run first so it does not re-escape the ampersands
-- introduced by the other entities.
escapeXml :: String -> String
escapeXml =
  H.replaceAll "\"" (const "&quot;")
    . H.replaceAll ">" (const "&gt;")
    . H.replaceAll "<" (const "&lt;")
    . H.replaceAll "&" (const "&amp;")

escapedTitle :: H.Metadata -> String
escapedTitle =
  escapeXml . safeTitle

safeTitle :: H.Metadata -> String
safeTitle =
  Maybe.fromMaybe "no title" . H.lookupString "title"

updatedTitle :: H.Item a -> H.Compiler String
updatedTitle =
  fmap escapedTitle . H.getMetadata . H.itemIdentifier
