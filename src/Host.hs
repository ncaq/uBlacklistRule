{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Host (makeHosts) where

import           Code
import           Import
import           RIO.List
import qualified RIO.Text as T

-- | 全てのホスト対象のURLリストを生成します。
makeHosts :: [Text]
makeHosts = nub $ concat [tech, copy, ch, video, game, rumor]

-- | 技術系スパムサイト全て。
tech :: [Text]
tech = concat [singleTechSites, itMure, itSwarm, qastack]

-- | 規則性があまり無いぼぼ単発の技術系スパムサイト。
singleTechSites :: [Text]
singleTechSites = T.lines $ T.strip [r|
365airsoft.com
366service.com
4meahc.com
answer-id.com
askdev.info
bestecode.com
bleepcoder.com
buginfo.tech
bugsdb.com
ciupacabra.com
cloud6.net
code-adviser.com
code-examples.net
code.i-harness.com
codeday.me
codefaq.info
codefaq.ru
codeflow.site
codenong.com
coder.work
coderoad.ru
codetd.com
codingdict.com
coredump.biz
crypto-days.jp
csdn.net
de-vraag.com
devadvisor.io
developreference.com
exceptionshub.com
fixes.pub
gitmemory.com
helpex.vn
issue.life
iteramos.com
javaer101.com
jpcloud.net
knews.vip
kotaeta.com
laptrinhx.com
living-sun.com
nobis.work
ojit.com
programmerz.ru
programqa.com
python2.net
python5.com
qa-help.ru
qa.1r1g.com
qaru.site
quabr.com
quares.ru
railstoolkit.com
removemalware-jp.com
riptutorial.com
rupython.com
softonic.com
softonic.jp
soinside.com
stackoom.com
stackovernet.com
stackovernet.xyz
stackoverrun.com
steakrecords.com
switch-case.com
switch-case.ru
thinbug.com
tutorialmore.com
ubuntugeeks.com
uwenku.com
voidcc.com
while-do.com
xbuba.com
|]

-- | `it-mure.jp.net` 系のサイト。
itMure :: [Text]
itMure = (\code -> "it-mure." <> code <> ".net") <$> codes

-- | `it-swarm.com` 系のサイト。
itSwarm :: [Text]
itSwarm =
  let topLevelDomains =
        [ "com"
        , "dev"
        , "net"
        , "tech"
        , "xyz"
        ]
  in (["it-swarm." <> domain | domain <- topLevelDomains]) ++
     (["it-swarm-" <> code <> "." <> domain | domain <- topLevelDomains, code <- codes])

-- | `qastack.jp` 系のサイト。
qastack :: [Text]
qastack = concat
  [ ("qastack." <>) <$> codes
  , ("qastack.com." <>) <$> codes
  , ("qastack.in." <>) <$> codes
  , ("qastack.info." <>) <$> codes
  ]


-- | 特に対象無くコピペを行うサイト
copy :: [Text]
copy = T.lines $ T.strip [r|
proxyfly.org
proxybot.cc
|]

-- | 5chコピペサイト。
ch :: [Text]
ch = T.lines $ T.strip [r|
2ch-ranking.net
2ch.live
2ch.pet
2ch.review
2ch.sc
2ch.vet
2chmm.com
2nn.jp
5ch-ranking.com
5ch.pub
calcal.net
ikioi2ch.net
ikioi5ch.net
|]

-- | 動画をiframeで埋め込んで流したり、メタ情報で検索に引っ掛けてくるもの。
video :: [Text]
video = T.lines $ T.strip [r|
nico-ran.jp
nicoapple.sub.jp
nicochart.jp
nicoco.net
nicovideo.me
nicozon.net
sub-nicoapple.ssl-lolipop.jp
|]

-- | ゲーム攻略コピペサイト。
game :: [Text]
game = T.lines $ T.strip [r|
altema.jp
game8.jp
gamerch.com
gamewith.jp
gamy.jp
|]

-- | デマを流すサイト。
rumor :: [Text]
rumor = T.lines $ T.strip [r|
esuteru.com
jin115.com
|]
