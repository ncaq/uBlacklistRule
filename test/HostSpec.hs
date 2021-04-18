{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module HostSpec (spec) where

import           Data.Aeson
import           Host
import           Import
import           Network.HTTP.Simple
import qualified RIO.Text            as T
import           Test.Hspec

newtype Sites
  = Sites
  { items :: [Item]
  } deriving (Eq, Ord, Read, Show, Generic, FromJSON)

newtype Item
  = Item
  { site_url :: Text
  } deriving (Eq, Ord, Read, Show, Generic, FromJSON)

spec :: Spec
spec = do
  describe "makeHosts" $ do
    it "Stack Exchangeが公式に運用しているホストをブロックしていないか" $ do
      stackExchangeSites <- liftIO getStackExchangeSites
      mapM_ (\host -> stackExchangeSites `shouldNotContain` [host]) makeHosts

-- | Stack Exchangeが公式に運用しているホスト一覧
-- [Usage of /sites [GET] - Stack Exchange API](https://api.stackexchange.com/docs/sites)
getStackExchangeSites :: IO [Text]
getStackExchangeSites = do
  sites <- getResponseBody <$> httpJSON "https://api.stackexchange.com/2.2/sites?pagesize=10000"
  return $ T.dropPrefix "https://" . site_url <$> items sites
