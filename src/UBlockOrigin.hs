{-# LANGUAGE QuasiQuotes #-}
module UBlockOrigin (writeUBlockOriginTxt) where

import           Data.String.Here
import           Import
import           RIO.Text         as T
import           Type

-- | `uBlockOrigin.txt`をワーキングディレクトリに書き込みます。
writeUBlockOriginTxt :: [HostGroup] -> RIO env ()
writeUBlockOriginTxt hostGroups =
  writeFileUtf8 "uBlockOrigin.txt" . stripTextFile . (header <>) $
  T.unlines (T.unlines . fmap toRule . hostGroupInfix <$> hostGroups)

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

-- | Firefox for Android向けのルール。
-- 基本的にGoogle Search Fixerなどを使っていることを想定しています。
-- Googleの仕様変更によって壊れやすいので注意。
toRule :: Text -> Text
toRule host =
  [i|www.google.*###main div > div[data-hveid]:not([class]) div[class] > div[class] > a[href*="${host}"][data-ved]:upward(3)|]

-- | 通常の`strip`だとテキストファイルとして必要な末尾改行も削除してしまうのでそれを取り付け直す。
-- 効率のことは考えていないコードです。
stripTextFile :: Text -> Text
stripTextFile = flip snoc '\n' . T.strip
