{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module UBlockOrigin (writeUBlockOriginTxt) where

import           Import
import           RIO.Text as T

-- | `uBlockOrigin.txt`をワーキングディレクトリに書き込みます。
writeUBlockOriginTxt :: [Text] -> RIO App ()
writeUBlockOriginTxt hosts = writeFileUtf8 "uBlockOrigin.txt" . stripTextFile . (header <>) . T.unlines $ toRule <$> hosts

header :: Text
header = [r|
[uBlock Origin]
! Title: uBlacklistRule for Firefox for Android
! Description: Rules for uBlacklist will be converted to uBlock Origin rules for environments where uBlacklist is not available.
! Expires: 1 days
! Homepage: https://github.com/ncaq/uBlacklistRule/
! License: MIT
! Thanks: https://github.com/hirorpt/ubo-stackoverflow-translation

|]

toRule :: Text -> Text
toRule host =
  -- 通常のFirefox for Android向け、
  -- Google Search Fixerなどを使って、Chrome向けの画面を出している時、
  -- 検索スニペットなどまとめて消去します。
  "www.google.*##.xpd:has([href*=\"" <> host <> "\"])"

-- | 通常の`strip`だとテキストファイルとして必要な末尾改行も削除してしまうのでそれを取り付け直す。
-- 効率のことは考えていないコードです。
stripTextFile :: Text -> Text
stripTextFile = flip snoc '\n' . T.strip
