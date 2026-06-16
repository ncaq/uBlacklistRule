{-# OPTIONS_GHC -Wno-orphans #-}

module Code (codes) where

import Data.ISO3166_CountryCodes
import Data.LanguageCodes
import Data.List qualified as L
import Data.Text qualified as T
import Himari

-- | 機械翻訳サイトが使う可能性のある地域コード一覧。
-- 単にブラックリスト一覧として見ると、分かれているのはわかりにくいだけなので混ぜてソートします。
codes :: [Text]
codes = L.sort $ countryCodes <> langCodes

-- | 国別コード。
countryCodes :: [Text]
countryCodes = T.toLower . convert . show <$> [minBound :: CountryCode .. maxBound]

instance Bounded ISO639_1 where
  minBound = AA
  maxBound = ZU

-- | 言語別コード。
langCodes :: [Text]
langCodes = T.toLower . convert . show <$> [minBound :: ISO639_1 .. maxBound]
