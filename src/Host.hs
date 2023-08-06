{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE StrictData      #-}
{-# LANGUAGE TemplateHaskell #-}
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
makeHostGroups =
  [ ch
  , ghard
  , video
  , extensionExplanationSite
  , wikipedia
  , proxy
  , phishing
  , thirdLevelDomain
  ] <>
  tech

-- | 5chコピペサイト。
-- 全て追加するのではなく、インデックスとしても価値がないものを排除しています。
ch :: HostGroup
ch = fromFull $ T.lines $(embedStringFile "asset/ch-site.txt")

-- | ゲハブログ。
ghard :: HostGroup
ghard = fromFull $ T.lines $(embedStringFile "asset/ghard-site.txt")

-- | 拡張子解説サイト。
extensionExplanationSite :: HostGroup
extensionExplanationSite = fromFull $ T.lines $(embedStringFile "asset/extension-explanation-site.txt")

-- | 動画をiframeで埋め込んで流したり、メタ情報で検索に引っ掛けてくるもの。
video :: HostGroup
video = fromFull $ T.lines $(embedStringFile "asset/video-site.txt")

-- | Wikipediaのコピーサイト。
wikipedia :: HostGroup
wikipedia = fromFull $ T.lines $(embedStringFile "asset/wikipedia-site.txt")

-- | webプロキシ。
proxy :: HostGroup
proxy = fromFull $ T.lines $(embedStringFile "asset/proxy-site.txt")

-- | フィッシングサイト。
-- キリがない気もしますが、追加できるものは追懐しておきます。
phishing :: HostGroup
phishing = fromFull $ T.lines $(embedStringFile "asset/phishing.txt")

-- | 技術系スパムサイト全て。
tech :: [HostGroup]
tech = [singleTechSites, itMure, itSwarm, qastack, issuecloser, coderQuestion, coderSolution]

-- | 規則性があまり無いぼぼ単発の技術系コピーサイト。
singleTechSites :: HostGroup
singleTechSites = fromFull $ T.lines $(embedStringFile "asset/single-tech-site.txt")

-- | 通常サブドメインに使わないトップレベルドメインのリスト。
topLevelDomain :: [Text]
topLevelDomain = T.lines $(embedStringFile "asset/top-level-domain.txt")

-- | `foo.com.br`のようなサードレベルドメイン利用のサイト。
-- マトモに使ってる例があるかもしれないと思って躊躇いましたが、
-- これまでスパム的なもの以外にマトモに使われている例を結局見たことがありませんでした。
thirdLevelDomain :: HostGroup
thirdLevelDomain = fromFull $ L.nub ([domain <> "." <> code | domain <- topLevelDomain, code <- codes])

-- | `it-mure.jp.net`系のサイト。
itMure :: HostGroup
itMure = HostGroup
  { hostGroupFull = L.nub $ (\code -> "it-mure." <> code <> ".net") <$> codes
  , hostGroupInfix = ["it-mure"]
  }

-- | `it-swarm.`系のサイト。
itSwarm :: HostGroup
itSwarm = HostGroup
  { hostGroupFull = full
  , hostGroupInfix = ["it-swarm"]
  }
  where full = L.nub $
          (["it-swarm." <> domain | domain <- topLevelDomain <> codes]) <>
          (["it-swarm-" <> code <> "." <> domain | domain <- topLevelDomain, code <- codes])

-- | `qastack.jp`系のサイト。
qastack :: HostGroup
qastack = HostGroup
  { hostGroupFull = full
  , hostGroupInfix = ["qastack", "qa-stack"]
  }
  where full = L.nub $ concat
          [ ("qastack." <>) <$> codes
          , ("qastack.in." <>) <$> codes
          , ("qastack.info." <>) <$> codes
          , ("qa-stack." <>) <$> codes
          ]

-- | `issuecloser-jp.com`など。
issuecloser :: HostGroup
issuecloser = HostGroup
  { hostGroupFull = full
  , hostGroupInfix = ["issuecloser"]
  }
  where full = L.nub (["issuecloser-" <> code <> "." <> domain | domain <- topLevelDomain, code <- codes])

-- | `coder-question.com`系のサイト。
coderQuestion :: HostGroup
coderQuestion = HostGroup
  { hostGroupFull = full
  , hostGroupInfix = ["coder-question"]
  }
  where full = L.nub $
          (["coder-question." <> domain | domain <- topLevelDomain <> codes]) <>
          (["coder-question-" <> code <> "." <> domain | domain <- topLevelDomain, code <- codes])

-- | `coder-solution.com`系のサイト。
coderSolution :: HostGroup
coderSolution = HostGroup
  { hostGroupFull = full
  , hostGroupInfix = ["coder-solution"]
  }
  where full = L.nub $
          (["coder-solution." <> domain | domain <- topLevelDomain <> codes]) <>
          (["coder-solution-" <> code <> "." <> domain | domain <- topLevelDomain, code <- codes])
