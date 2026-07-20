{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module Hakyll.Site.Sitemap (createSitemap) where

--------------------------------------------------------------------------------

import qualified Hakyll as H
import qualified Hakyll.Site.Configuration as HSConfig
import qualified Hakyll.Site.Post as HSPost

--------------------------------------------------------------------------------

createSitemap :: H.Rules ()
createSitemap = do
  H.route H.idRoute
  H.compile $ do
    posts <- H.recentFirst =<< H.loadAll "posts/*"

    let sitemapCtx =
          H.constField "root" HSConfig.mySiteRoot
            <> H.constField "siteName" HSConfig.mySiteName
            <> H.listField "pages" HSPost.postCtx (return posts)

    H.makeItem ("" :: String)
      >>= H.loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx
