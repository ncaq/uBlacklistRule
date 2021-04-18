{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Code where

import           Data.ISO3166_CountryCodes
import           Data.LanguageCodes
import           Import
import           RIO.Text                  as T

-- | 機械翻訳サイトが使う可能性のある地域コード一覧。
codes :: [Text]
codes = countryCodes <> langCodes

-- | 国別コード。
countryCodes :: [Text]
countryCodes = toLower . tshow <$> [minBound :: CountryCode .. maxBound]

instance Bounded ISO639_1 where
  minBound = AA
  maxBound = ZU

-- | 言語別コード。
langCodes :: [Text]
langCodes = toLower . tshow <$> [minBound :: ISO639_1 .. maxBound]
