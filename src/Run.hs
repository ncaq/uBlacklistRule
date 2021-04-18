{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import           Host
import           Import
import           RIO.Text as T

run :: RIO App ()
run = writeUBlackListTxt makeHosts

-- | uBlacklist.txtをワーキングディレクトリに書き込みます。
writeUBlackListTxt :: [Text] -> RIO App ()
writeUBlackListTxt hosts = writeFileUtf8 "uBlacklist.txt" . T.unlines $ toMatchAll <$> hosts

-- | [Match patterns in extension manifests - Mozilla | MDN](https://developer.mozilla.org/en-US/docs/Mozilla/Add-ons/WebExtensions/Match_patterns)
-- のMatch all HTTPに書き換えます
toMatchAll :: Text -> Text
toMatchAll host = "*://*." <> host <>"/*"
