{-# LANGUAGE OverloadedStrings #-}

module Slug
  ( toSlug,
  )
where

import Data.Char (isAlphaNum)
import qualified Data.Text as T

keepAlphaNum :: Char -> Char
keepAlphaNum x
  | isAlphaNum x = x
  | otherwise = ' '

clean :: T.Text -> T.Text
clean =
  T.map keepAlphaNum . T.replace "'" "" . T.replace "&" "and"

toSlug :: T.Text -> T.Text
toSlug =
  T.intercalate (T.singleton '-') . T.words . T.toLower . clean
