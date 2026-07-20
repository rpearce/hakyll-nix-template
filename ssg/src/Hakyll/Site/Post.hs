{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module Hakyll.Site.Post
  ( postCtx
  , titleRoute
  ) where

--------------------------------------------------------------------------------

import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import qualified Data.Text.Slugger as Slugger
import qualified Hakyll as H
import qualified Hakyll.Site.Configuration as HSConfig
import qualified Hakyll.Site.CustomFields as HSCustomFields
import qualified System.FilePath as FP

--------------------------------------------------------------------------------

postCtx :: H.Context String
postCtx =
  H.constField "root" HSConfig.mySiteRoot
    <> H.constField "feedTitle" HSConfig.myFeedTitle
    <> H.constField "siteName" HSConfig.mySiteName
    <> H.dateField "date" "%Y-%m-%d"
    <> HSCustomFields.updatedField "updated" "%Y-%m-%d"
    <> H.defaultContext

--------------------------------------------------------------------------------
-- CUSTOM TITLES

titleRoute :: H.Metadata -> H.Routes
titleRoute =
  H.constRoute . fileNameFromTitle

fileNameFromTitle :: H.Metadata -> FP.FilePath
fileNameFromTitle =
  T.unpack . (`T.append` ".html") . Slugger.toSlug . T.pack . getTitleFromMeta

getTitleFromMeta :: H.Metadata -> String
getTitleFromMeta =
  Maybe.fromMaybe "no title" . H.lookupString "title"
