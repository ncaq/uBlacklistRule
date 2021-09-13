{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{- |
Ubo = uBlock Origin
-}
module Ubo (writeUboTxt) where

import           RIO.Text as T
import           Import

-- | uBlockOrigin.txtをワーキングディレクトリに書き込みます。
writeUboTxt :: [Text] -> RIO App ()
writeUboTxt hosts = writeFileUtf8 "uBlockOrigin.txt" . stripTextFile . (uboHeader <>) . T.unlines $ toUboRule <$> hosts

uboHeader :: Text
uboHeader = [r|
[uBlock Origin]
! Title: uBlacklistRule for Firefox for Android
! Description: Rules for uBlacklist will be converted to uBlock Origin rules for environments where uBlacklist is not available.
! Expires: 1 days
! Homepage: https://github.com/ncaq/uBlacklistRule/
! License: MIT
! Thanks: https://github.com/hirorpt/ubo-stackoverflow-translation

|]

toUboRule :: Text -> Text
toUboRule host = T.unlines
  [ "www.google.*##.kCrYT > a[href*=\"" <> host <> "\"]:upward(.xpd)" -- 通常のFirefox for Android向けの画面で必要。
  , "www.google.*##.C8nzq[href*=\"" <> host <> "\"]:upward(.xpd)"     -- Google Search Fixerなどを使って、Chrome向けの画面を出している時に必要。
  ]

-- | 通常の`strip`だとテキストファイルとして必要な末尾改行も削除してしまうのでそれを取り付け直す。
-- 効率のことは考えていないコードです。
stripTextFile :: Text -> Text
stripTextFile = flip snoc '\n' . T.strip
