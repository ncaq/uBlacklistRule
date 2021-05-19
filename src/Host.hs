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
makeHosts = nub $ concat [tech, ch, video, game, rumor, qa, otherCopy]

-- | 技術系スパムサイト全て。
tech :: [Text]
tech = concat [singleTechSites, itMure, itSwarm, qastack]

-- | 規則性があまり無いぼぼ単発の技術系コピーサイト。
singleTechSites :: [Text]
singleTechSites = T.lines $ T.strip [r|
365airsoft.com
366service.com
4meahc.com
888wenti.com
answer-id.com
askdev.info
bar-stools-plus.com
base64.work
bestecode.com
bleepcoder.com
buginfo.tech
bugsdb.com
ciupacabra.com
cloud6.net
cocoachina.com
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
debugcn.com
devadvisor.io
developreference.com
edureka.co
exceptionshub.com
extutorial.com
fixes.pub
geek-tips.imtqy.com
generacodice.com
ghcc.net
githubmemory.com
gitmemory.com
helloworldkb.com
helpex.vn
hi.qaru.tech
hu.qaru.tech
ichi.pro
ifaj-congress.org
isolution.pro
issue.life
issues-world.com
iteramos.com
javaer101.com
jpcloud.net
kejisen.com
knews.vip
konnichiwasekai.com
kotaeta.com
laptrinhx.com
legkovopros.ru
living-sun.com
nobis.work
ojit.com
progi.pro
programmerz.ru
programqa.com
python2.net
python5.com
pythonq.com
qa-help.ru
qa.1r1g.com
qaru.site
qathai.com
quabr.com
quares.ru
question-it.com
railstoolkit.com
removemalware-jp.com
riptutorial.com
risposta.org
runebook.dev
rupython.com
softonic.com
softonic.jp
soinside.com
stacknoob.com
stackoom.com
stackoverfill.com
stackovernet.com
stackovernet.xyz
stackoverrun.com
stackscn.com
steakrecords.com
suttonedfoundation.org
switch-case.com
switch-case.ru
thinbug.com
titanwolf.org
tutorialmore.com
ubuntu.buildwebhost.com
ubuntugeeks.com
uefir.com
usersuper.ru
uwenku.com
voidcc.com
while-do.com
xbuba.com
zhouni.net
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

-- | Yahoo!知恵袋や教えて!gooなどに偽装するサイトや、そのサイトにリダイレクトさせるサイト。
qa :: [Text]
qa = T.lines $ T.strip [r|
bmxklubben.dk
ferrum42kem.ru
proklimatshop.ru
sportfiske.org
|]

-- | その他のコピペサイト。
otherCopy :: [Text]
otherCopy = T.lines $ T.strip [r|
nipponkaigi.net
proxybot.cc
proxyfly.org
wikiarabi.org
|]
