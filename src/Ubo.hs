{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{- |
Ubo = uBlock Origin
-}
module Ubo (writeUboTxt) where

import           Import
import           Network.DNS
import qualified RIO.Text    as T

-- | uBlockOrigin.txtをワーキングディレクトリに書き込みます。
writeUboTxt :: [Text] -> RIO App ()
writeUboTxt hosts = do
  -- 生きているホストのみをuBlock Originのフィルタには追加する。
  -- ビルドするたびに結果が変わる恐れがあり、
  -- 定期的な再ビルドが必要ですが、
  -- そのままだとuBlock Originのフィルタ形式だとCSSセレクタの制約のせいか重すぎるので、
  -- 必要な犠牲であると判断します。
  -- 完全に除外出来なくても除外することには意味がある。
  liveHosts <- liftIO $ filterDns hosts
  writeFileUtf8 "uBlockOrigin.txt" . stripTextFile . (uboHeader <>) . T.unlines $ toUboRule <$> liveHosts

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
  -- 通常のFirefox for Android向けの画面で必要。
  [ "www.google.*##.kCrYT > a[href*=\"" <> host <> "\"]:upward(.xpd)"
  -- Google Search Fixerなどを使って、Chrome向けの画面を出している時に必要。
  , "www.google.*##.C8nzq[href*=\"" <> host <> "\"]:upward(.xpd)"
  ]

-- | 通常の`strip`だとテキストファイルとして必要な末尾改行も削除してしまうのでそれを取り付け直す。
-- 効率のことは考えていないコードです。
stripTextFile :: Text -> Text
stripTextFile = flip T.snoc '\n' . T.strip

-- | DNSレコードが存在するホストのみを抽出します。
filterDns :: [Text] -> IO [Text]
filterDns hosts = do
  rs <- makeResolvSeed defaultResolvConf
    { resolvInfo = RCHostNames ["1.1.1.1", "8.8.8.8", "8.8.4.4"]
    , resolvConcurrent = True
    , resolvCache = Just defaultCacheConf
    }
  withResolver rs f
  where f resolver = filterM (g resolver) hosts
        g resolver t = do
          let h = toByteStringStrict t
          -- `Network.URL`の使い方がよく分からなかった。
          -- アドホックな対処としてスラッシュパスがあれば有効なホストとします。
          -- こういうルールが増えてきて問題になったらちゃんと直します。
          if (== '/') `T.any` t
            then return True
            else do
            a <- rightAndSome <$> lookupA resolver h
            if a
              then return True
              else rightAndSome <$> lookupAAAA resolver h
        rightAndSome (Right (_ : _)) = True
        rightAndSome _               = False
