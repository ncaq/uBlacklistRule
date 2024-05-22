module HostSpec (spec) where

import           Data.Aeson
import           Host
import           Import
import           Network.HTTP.Simple
import qualified RIO.Text            as T
import           Test.Hspec
import           Type

newtype Sites
  = Sites
  { items :: [Item]
  }
  deriving (Eq, Ord, Read, Show, Generic)
instance FromJSON Sites

newtype Item
  = Item
  { site_url :: Text
  }
  deriving (Eq, Ord, Read, Show, Generic)
instance FromJSON Item

spec :: Spec
spec = do
  describe "makeHostGroups" $ do
    let hostGroups = makeHostGroups
    describe "whiteList" $ do
      it "uBlacklist基準でブロックしていないか" $
        mapM_ (\hostGroup -> whiteList `shouldNotContain` hostGroupFull hostGroup) hostGroups
      it "uBlock Origin基準でブロックしていないか" $
        sequence_
        [ (infixOne, white) `shouldNotSatisfy` uncurry T.isInfixOf
        | hostGroup <- hostGroups
        , infixOne <- hostGroupInfix hostGroup
        , white <- whiteList
        ]
    it "Stack Exchangeが公式に運用しているホストをブロックしていないか" $ do
      stackExchangeSites <- liftIO getStackExchangeSites
      mapM_ (\hostGroup -> stackExchangeSites `shouldNotContain` hostGroupFull hostGroup) hostGroups

-- | ブロックしてはいけないURLたち。
whiteList :: [Text]
whiteList =
  [ "https://docs.ruby-lang.org/ja/latest/library/openssl.html" -- 一回誤爆した。
  , "https://segmentfault.com" -- 一見StackOverflowのコピーサイトにしか見えないが、一応内容はオリジナルらしい。
  , "https://www.citizensadvice.org.uk/" -- イギリスの正常なサイトらしい。
  , "https://www.zhihu.com" -- 内容はオリジナルらしい。
  ]

-- | Stack Exchangeが公式に運用しているホスト一覧
-- [Usage of /sites [GET] - Stack Exchange API](https://api.stackexchange.com/docs/sites)
getStackExchangeSites :: IO [Text]
getStackExchangeSites = do
  sites <- getResponseBody <$> httpJSON "https://api.stackexchange.com/2.2/sites?pagesize=10000"
  return $ T.dropPrefix "https://" . site_url <$> items sites
