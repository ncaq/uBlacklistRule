module UBlacklist (writeUBlacklistTxt) where

import           Import
import           RIO.Text as T
import           Type

-- | `uBlacklist.txt`をワーキングディレクトリに書き込みます。
writeUBlacklistTxt :: [UBlacklistPattern] -> RIO env ()
writeUBlacklistTxt patterns =
  writeFileUtf8 "uBlacklist.txt" $ T.unlines (T.unlines . toUBlacklistText <$> patterns)

toUBlacklistText :: UBlacklistPattern -> [Text]
toUBlacklistText (UBlacklistPatternHostGroup x)     = toMatchAllUrl <$> hostGroupFull x
toUBlacklistText (UBlacklistPatternTitle (Title x)) = toMatchTitle <$> x

-- | [Match patterns in extension manifests - Mozilla | MDN](https://developer.mozilla.org/en-US/docs/Mozilla/Add-ons/WebExtensions/Match_patterns)
-- のMatch all HTTPに書き換えます。
toMatchAllUrl :: Text -> Text
toMatchAllUrl host = "*://*." <> host <>"/*"

-- | タイトルだけの正規表現をuBlackListのtitle正規表現パターンを利用した形式に変換します。
toMatchTitle :: Text -> Text
toMatchTitle x = "title/" <> x <> "/"
