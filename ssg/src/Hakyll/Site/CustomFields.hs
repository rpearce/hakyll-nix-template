module Hakyll.Site.CustomFields (updatedField) where

--------------------------------------------------------------------------------

import qualified Control.Monad as Monad
import qualified Control.Monad.Fail as MonadFail
import qualified Data.Maybe as Maybe
import qualified Data.Time.Clock as DTC
import qualified Data.Time.Format as DTF
import qualified Hakyll as H

--------------------------------------------------------------------------------

-- | A date field that reads the @updated@ metadata key, parses it against a few
-- common formats, and re-renders it with @format@. Yields no result when the
-- key is absent or unparseable, so @$if(updated)$@ stays false.
updatedField :: String -> String -> H.Context String
updatedField key format = H.field key $ \i -> do
  time <- getUpdatedUTC $ H.itemIdentifier i
  return $ DTF.formatTime DTF.defaultTimeLocale format time

--------------------------------------------------------------------------------

getUpdatedUTC :: (H.MonadMetadata m, MonadFail m)
              => H.Identifier
              -> m DTC.UTCTime
getUpdatedUTC id' = do
  metadata <- H.getMetadata id'
  let tryField k fmt = H.lookupString k metadata >>= parseTime' fmt
  Maybe.maybe empty' return $ Monad.msum [tryField "updated" fmt | fmt <- formats]
  where
    empty'     = MonadFail.fail $ "getUpdatedUTC: could not parse time for " ++ show id'
    parseTime' = DTF.parseTimeM True DTF.defaultTimeLocale
    formats    =
      [ "%Y-%m-%d"
      , "%Y-%m-%dT%H:%M:%SZ"       -- Atom feed-friendly
      , "%a, %d %b %Y %H:%M:%S UT" -- RSS feed-friendly (RFC-822)
      ]
