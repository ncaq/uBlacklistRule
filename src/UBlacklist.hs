module UBlacklist (writeUBlacklistTxt) where

import Data.Text qualified as T
import Data.Text.IO.Utf8 qualified as Utf8
import Himari
import Type

-- | `uBlacklist.txt`をワーキングディレクトリに書き込みます。
writeUBlacklistTxt :: (MonadIO m) => [UBlacklistPattern] -> m ()
writeUBlacklistTxt patterns =
  liftIO . Utf8.writeFile "uBlacklist.txt" $ T.unlines (T.unlines . toUBlacklistText <$> patterns)

toUBlacklistText :: UBlacklistPattern -> [Text]
toUBlacklistText (UBlacklistPatternHostGroup x) = toMatchAllUrl <$> hostGroupFull x
toUBlacklistText (UBlacklistPatternTitle (Title x)) = toMatchTitle <$> x

-- | [Match patterns in extension manifests - Mozilla | MDN](https://developer.mozilla.org/en-US/docs/Mozilla/Add-ons/WebExtensions/Match_patterns)
-- のMatch all HTTPに書き換えます。
toMatchAllUrl :: Text -> Text
toMatchAllUrl host = "*://*." <> host <> "/*"

-- | タイトルだけの正規表現をuBlackListのtitle正規表現パターンを利用した形式に変換します。
toMatchTitle :: Text -> Text
toMatchTitle x = "title/" <> x <> "/"
