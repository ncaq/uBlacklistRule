module UBlacklist (writeUBlacklistTxt) where

import           Import
import           RIO.Text as T

-- | `uBlacklist.txt`をワーキングディレクトリに書き込みます。
writeUBlacklistTxt :: [Text] -> RIO env ()
writeUBlacklistTxt hosts = writeFileUtf8 "uBlacklist.txt" . T.unlines $ toMatchAll <$> hosts

-- | [Match patterns in extension manifests - Mozilla | MDN](https://developer.mozilla.org/en-US/docs/Mozilla/Add-ons/WebExtensions/Match_patterns)
-- のMatch all HTTPに書き換えます
toMatchAll :: Text -> Text
toMatchAll host = "*://*." <> host <>"/*"
