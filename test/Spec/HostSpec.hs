module Spec.HostSpec (tests) where

import Data.Aeson
import Host
import Import
import Network.HTTP.Simple
import RIO.Text qualified as T
import Test.Sandwich
import Type

newtype Sites
  = Sites
  { items :: [Item]
  }
  deriving (Eq, Generic, Ord, Read, Show)
instance FromJSON Sites

newtype Item
  = Item
  { site_url :: Text
  }
  deriving (Eq, Generic, Ord, Read, Show)
instance FromJSON Item

hostGroups :: [HostGroup]
hostGroups = makeHostGroups

tests :: TopSpec
tests = describe "makeHostGroups" do
  describe "whiteList" do
    it "uBlacklist基準でブロックしていないか" do
      sequence_
        [ whiteList `shouldNotContainPredicate` (suffixOne `T.isSuffixOf`)
        | hostGroup <- hostGroups
        , suffixOne <- hostGroupFull hostGroup
        ]
    it "uBlock Origin基準でブロックしていないか" do
      sequence_
        [ whiteList `shouldNotContainPredicate` (infixOne `T.isInfixOf`)
        | hostGroup <- hostGroups
        , infixOne <- hostGroupInfix hostGroup
        ]
  it "Stack Exchangeが公式に運用しているホストをブロックしていないか" do
    stackExchangeSites <- liftIO getStackExchangeSites
    mapM_ (\hostGroup -> stackExchangeSites `shouldNotContain` hostGroupFull hostGroup) hostGroups

-- | ブロックしてはいけないホストたち。
whiteList :: [Text]
whiteList =
  [ "docs.ruby-lang.org" -- 一回誤爆した。
  , "segmentfault.com" -- 一見StackOverflowのコピーサイトにしか見えないが、一応内容はオリジナルらしい。
  , "www.citizensadvice.org.uk" -- イギリスの正常なサイトらしい。
  , "www.zhihu.com" -- 内容はオリジナルらしい。
  ]

-- | Stack Exchangeが公式に運用しているホスト一覧
-- [Usage of /sites [GET] - Stack Exchange API](https://api.stackexchange.com/docs/sites)
getStackExchangeSites :: IO [Text]
getStackExchangeSites = do
  let request =
        setRequestHeader
          "User-Agent"
          ["curl/7.81.1"]
          "https://api.stackexchange.com/2.3/sites?pagesize=10000"
  sites <- getResponseBody <$> httpJSON request
  return $ T.dropPrefix "https://" . site_url <$> items sites
