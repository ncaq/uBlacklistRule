{-# LANGUAGE StrictData #-}
module Type where

import           RIO
import           RIO.Process

data App = App
  { appLogFunc        :: !LogFunc
  , appProcessContext :: !ProcessContext
  -- Add other app-specific configuration information here
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })
instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x { appProcessContext = y })

-- | uBlacklistのパターンに複数種類を渡すための直和型です。
data UBlacklistPattern
  = UBlacklistPatternHostGroup HostGroup
  | UBlacklistPatternTitle     Title
  deriving (Eq, Ord, Read, Show)

-- | ホストのひとまとまりです。
data HostGroup
  = HostGroup
  { -- | ホストをシンプルに全体を表したものです。
    -- 冗長。
    -- うまく取り扱ってくれるuBlacklist向け。
    hostGroupFull  :: [Text]
    -- | ホストをなるべく簡潔にinfixで表したものです。
    -- uBlock Originなどデータ量が多いと処理できないもの向け。
  , hostGroupInfix :: [Text]
  }
  deriving (Eq, Ord, Read, Show)

-- | 正規表現パターンを表します。
-- Haskell側で処理するわけではないので弱い表現に留まっています。
newtype Title = Title [Text]
  deriving (Eq, Ord, Read, Show)
