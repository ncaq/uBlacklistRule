{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
Ubl = uBlackList
-}
module Ubl (writeUblListTxt) where

import           Import
import           RIO.Text as T

-- | uBlacklist.txtをワーキングディレクトリに書き込みます。
writeUblListTxt :: [Text] -> RIO App ()
writeUblListTxt hosts = writeFileUtf8 "uBlacklist.txt" . T.unlines $ toMatchAll <$> hosts

-- | [Match patterns in extension manifests - Mozilla | MDN](https://developer.mozilla.org/en-US/docs/Mozilla/Add-ons/WebExtensions/Match_patterns)
-- のMatch all HTTPに書き換えます
toMatchAll :: Text -> Text
toMatchAll host = "*://*." <> host <>"/*"
