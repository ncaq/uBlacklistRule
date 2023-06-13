{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TemplateHaskell   #-}
module Host (HostGroup(..), makeHostGroups) where

import           Code
import           Import
import qualified RIO.List as L
import qualified RIO.Text as T

-- | ホストのひとまとまり。
data HostGroup
  = HostGroup
  { -- | ホストをシンプルに全体を表したもの。
    -- 冗長。
    -- うまく取り扱ってくれるuBlacklist向け。
    hostGroupFull  :: [Text]
    -- | ホストをなるべく簡潔にinfixで表したもの。
    -- uBlock Originなどデータ量が多いと処理できないもの向け。
  , hostGroupInfix :: [Text]
  }
  deriving (Eq, Ord, Show, Read)

-- | 簡潔に表す方法が無い場合に諦めてそのままfullもinfixも作ってしまう。
fromFull :: [Text] -> HostGroup
fromFull ts = HostGroup { hostGroupFull = ts, hostGroupInfix = ts }

-- | 全てのホスト対象のURLリストを生成します。
makeHostGroups :: [HostGroup]
makeHostGroups = tech <> [thirdLevelDomain, extensionExplanationSite, ch, video, ghard, wikipedia, proxy]

-- | 技術系スパムサイト全て。
tech :: [HostGroup]
tech = [singleTechSites, itMure, itSwarm, qastack, issuecloser, coderQuestion, coderSolution]

-- | 規則性があまり無いぼぼ単発の技術系コピーサイト。
singleTechSites :: HostGroup
singleTechSites = fromFull $ T.lines $(embedStringFile "asset/single-tech-site.txt")

-- | `it-mure.jp.net` 系のサイト。
itMure :: HostGroup
itMure = HostGroup
  { hostGroupFull = L.nub $ (\code -> "it-mure." <> code <> ".net") <$> codes
  , hostGroupInfix = ["it-mure"]
  }

-- | `it-swarm.` 系のサイト。
itSwarm :: HostGroup
itSwarm = HostGroup
  { hostGroupFull = full
  , hostGroupInfix = ["it-swarm"]
  }
  where full = let topLevelDomains = ["com", "dev", "net", "tech", "xyz"]
               in L.nub $
                  (["it-swarm." <> domain | domain <- topLevelDomains <> codes]) <>
                  (["it-swarm." <> domain <> "." <> code | domain <- topLevelDomains, code <- codes]) <>
                  (["it-swarm-" <> code <> "." <> domain | domain <- topLevelDomains, code <- codes])

-- | `qastack.jp` 系のサイト。
qastack :: HostGroup
qastack = HostGroup
  { hostGroupFull = full
  , hostGroupInfix = ["qastack", "qa-stack"]
  }
  where full = L.nub $ concat
          [ ("qastack." <>) <$> codes
          , ("qastack.com." <>) <$> codes
          , ("qastack.net." <>) <$> codes
          , ("qastack.in." <>) <$> codes
          , ("qastack.info." <>) <$> codes
          , ("qa-stack." <>) <$> codes
          ]

-- | issuecloser-jp.comなど。
issuecloser :: HostGroup
issuecloser = HostGroup
  { hostGroupFull = full
  , hostGroupInfix = ["issuecloser"]
  }
  where full = let topLevelDomains = ["com", "dev", "net", "tech", "xyz"]
               in L.nub (["issuecloser-" <> code <> "." <> domain | domain <- topLevelDomains, code <- codes])

-- | `coder-question.com` 系のサイト。
coderQuestion :: HostGroup
coderQuestion = HostGroup
  { hostGroupFull = full
  , hostGroupInfix = ["coder-question"]
  }
  where full = let topLevelDomains = ["com", "dev", "net", "tech", "xyz"]
               in L.nub $
                  (["coder-question." <> domain | domain <- topLevelDomains <> codes]) <>
                  (["coder-question-" <> code <> "." <> domain | domain <- topLevelDomains, code <- codes])

-- | `coder-solution.com` 系のサイト。
coderSolution :: HostGroup
coderSolution = HostGroup
  { hostGroupFull = full
  , hostGroupInfix = ["coder-solution"]
  }
  where full = let topLevelDomains = ["com", "dev", "net", "tech", "xyz"]
               in L.nub $
                  (["coder-solution." <> domain | domain <- topLevelDomains <> codes]) <>
                  (["coder-solution-" <> code <> "." <> domain | domain <- topLevelDomains, code <- codes])

-- | `foo.com.br`のようなサードレベルドメイン。
-- マトモに使ってる例があるかもしれないと思って躊躇いましたが、
-- これまでスパム的なもの以外にマトモに使われている例を結局見たことがありませんでした。
thirdLevelDomain :: HostGroup
thirdLevelDomain = fromFull $ T.lines $(embedStringFile "asset/third-level-domain.txt")

-- | 拡張子解説サイト。
extensionExplanationSite :: HostGroup
extensionExplanationSite = fromFull $ T.lines $(embedStringFile "asset/extension-explanation-site.txt")

-- | 5chコピペサイト。
-- 全て追加するのではなく、インデックスとしても価値がないものを排除しています。
ch :: HostGroup
ch = fromFull $ T.lines $(embedStringFile "asset/ch-site.txt")

-- | 動画をiframeで埋め込んで流したり、メタ情報で検索に引っ掛けてくるもの。
video :: HostGroup
video = fromFull $ T.lines $(embedStringFile "asset/video-site.txt")

-- | ゲハブログ。
ghard :: HostGroup
ghard = fromFull $ T.lines $(embedStringFile "asset/ghard-site.txt")

-- | Wikipediaのコピーサイト。
wikipedia :: HostGroup
wikipedia = fromFull $ T.lines $(embedStringFile "asset/wikipedia-site.txt")

-- | webプロキシ。
proxy :: HostGroup
proxy = fromFull $ T.lines $(embedStringFile "asset/proxy-site.txt")
