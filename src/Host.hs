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
    -- | ホストをなるべく簡潔にinfixを表したもの。
    -- uBlock Originなどデータ量が多いと処理できないもの向け。
  , hostGroupInfix :: [Text]
  }
  deriving (Eq, Ord, Show, Read)

-- | 簡潔に表す方法が無い場合に諦めてそのままfullもinfixも作ってしまう。
fromFull :: [Text] -> HostGroup
fromFull ts = HostGroup { hostGroupFull = ts, hostGroupInfix = ts }

-- | 全てのホスト対象のURLリストを生成します。
makeHostGroups :: [HostGroup]
makeHostGroups = tech <> [ch, video, game, ghard, wikipedia, proxy, malware]

-- | 技術系スパムサイト全て。
tech :: [HostGroup]
tech = [singleTechSites, itMure, itSwarm, qastack]

-- | 規則性があまり無いぼぼ単発の技術系コピーサイト。
singleTechSites :: HostGroup
singleTechSites = fromFull $ T.lines $ convert $(embedFile "asset/single-tech-sites.txt")

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

-- | 5chコピペサイト。
-- 全て追加するのではなく、インデックスとしても価値がないものを排除しています。
ch :: HostGroup
ch = fromFull $ T.lines $ T.strip [r|
2ch-ranking.net
2ch.live
2ch.pet
2ch.review
2ch.sc
2ch.vet
2chmm.com
2nf2.rdy.jp
2nn.jp
5ch-ranking.com
5ch.pub
bbspink.icu
calcal.net
comedydouga.com
ikioi2ch.net
ikioi5ch.net
|]

-- | 動画をiframeで埋め込んで流したり、メタ情報で検索に引っ掛けてくるもの。
video :: HostGroup
video = fromFull $ T.lines $ T.strip [r|
nico-ran.jp
nicoapple.sub.jp
nicochart.jp
nicoco.net
nicovideo.me
nicozon.net
sub-nicoapple.ssl-lolipop.jp
|]

-- | ゲーム攻略コピペサイト。
game :: HostGroup
game = fromFull $  T.lines $ T.strip [r|
altema.jp
game8.jp
gamerch.com
gamewith.jp
gamy.jp
|]

-- | ゲハブログ。
ghard :: HostGroup
ghard = fromFull $ T.lines $ T.strip [r|
esuteru.com
ha-navi.com
jin115.com
|]

-- | Wikipediaのコピーサイト。
wikipedia :: HostGroup
wikipedia = fromFull $ T.lines $ T.strip [r|
janghan.net
japan2.wiki
jpan.wiki
linkfang.org
melayukini.net
nipponkaigi.net
unionpedia.org
wikiarabi.org
wikinew.wiki
wikiqube.net
wikituscany.com
wikiwand.com
|]

-- | webプロキシ。
proxy :: HostGroup
proxy = fromFull $ T.lines $ T.strip [r|
proxybot.cc
proxyfly.org
|]

-- | 検索ワードだけを散りばめて、詐欺サイトなどに飛ばすサイト。
malware :: HostGroup
malware = fromFull $ T.lines $ convert $(embedFile "asset/malware.txt")
