{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Run (run) where

import           Host
import           Import
import           RIO.Text as T

run :: RIO App ()
run = do
  let hosts = makeHosts
  writeUblListTxt hosts
  writeUboTxt hosts

-- | uBlacklist.txtをワーキングディレクトリに書き込みます。
writeUblListTxt :: [Text] -> RIO App ()
writeUblListTxt hosts = writeFileUtf8 "uBlacklist.txt" . T.unlines $ toMatchAll <$> hosts

-- | [Match patterns in extension manifests - Mozilla | MDN](https://developer.mozilla.org/en-US/docs/Mozilla/Add-ons/WebExtensions/Match_patterns)
-- のMatch all HTTPに書き換えます
toMatchAll :: Text -> Text
toMatchAll host = "*://*." <> host <>"/*"

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
  [ "www.google.*##.kCrYT > a[href*=\"" <> host <> "\"]:upward(.xpd)"
  , "www.google.*##.C8nzq[href*=\"" <> host <> "\"]:upward(.xpd)"
  , "www.google.*##.xpdopen .sXtWJb[href*=\"" <> host <> "\"]:upward(.xpdopen)"
  , "www.google.*##.aI1xUe .sXtWJb[href*=\"" <> host <> "\"]:upward(.aI1xUe)"
  ]

-- | 通常の`strip`だとテキストファイルとして必要な末尾改行も削除してしまうのでそれを取り付け直す。
-- 効率のことは考えていないコードです。
stripTextFile :: Text -> Text
stripTextFile = flip snoc '\n' . T.strip
