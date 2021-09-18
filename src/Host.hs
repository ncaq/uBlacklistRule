{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE StrictData        #-}
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
singleTechSites = fromFull $ T.lines $ T.strip [r|
365airsoft.com
366service.com
4meahc.com
888wenti.com
answer-id.com
artfit-prk.com
askdev.info
athabasca-foto.com
bar-stools-plus.com
base64.work
bestecode.com
bleepcoder.com
buginfo.tech
bugsdb.com
cfadnc.org
ciupacabra.com
clearwatergardenclub.org
cloud.tencent.com/developer/ask
cloud6.net
cocoachina.com
code-404.net
code-adviser.com
code-examples.net
code.i-harness.com
codeday.me
codefaq.info
codefaq.ru
codeflow.site
codegrepper.com
codehero.jp
codenong.com
coder.work
coderoad.ru
codetd.com
codingdict.com
coredump.biz
crypto-days.jp
csdn.net
daimajiaoliu.com
de-vraag.com
debugcn.com
delavaio.com
devadvisor.io
developreference.com
edureka.co
elfishgene.com
exceptionshub.com
extutorial.com
fixes.pub
geek-qa.imtqy.com
geek-tips.imtqy.com
geeks-world.imtqy.com
generacodice.com
ghcc.net
githubmemory.com
gitmemory.com
gtainspections.com
helloworldkb.com
helpex.vn
hi.qaru.tech
hu.qaru.tech
huati365.com
ichi.pro
ifaj-congress.org
isolution.pro
issue.life
issues-world.com
iteramos.com
javaer101.com
jpcloud.net
kaifa99.com
kejisen.com
knews.vip
konnichiwasekai.com
kotaeta.com
laptrinhx.com
legkovopros.ru
living-sun.com
mlog.club
newbedev.com
nobis.work
ojit.com
orcode.com
overcoder.net
panaindustrial.com
point808.com
progi.pro
prograide.com
programmerstart.com
programmerz.ru
programqa.com
projectbackpack.org
python.engineering
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
sanchezsalvador.com
softonic.com
softonic.jp
soinside.com
stacknoob.com
stackoom.com
stackoverfill.com
stackovergo.com
stackovernet.com
stackovernet.xyz
stackoverrun.com
stackscn.com
steakrecords.com
suttonedfoundation.org
switch-case.com
switch-case.ru
tech-ja.netlify.app
thercb.org
thinbug.com
titanwolf.org
tlcpv.org
try2explore.com
tutorialguruji.com
tutorialmore.com
ubuntu.buildwebhost.com
ubuntugeeks.com
uefir.com
usersuper.ru
uwenku.com
vfwpost8762.org
visual-foxpro-programmer.com
voidcc.com
webdevqa.jp.net
wenyanet.com
while-do.com
xbuba.com
zhouni.net
|]

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
2nn.jp
5ch-ranking.com
5ch.pub
bbspink.icu
calcal.net
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
japan2.wiki
linkfang.org
melayukini.net
nipponkaigi.net
unionpedia.org
wikiarabi.org
wikinew.wiki
wikiqube.net
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
malware = fromFull $ T.lines $ T.strip [r|
007okno.ru
360comunication.it
4beacademy.it
505tankist.ru
7815671.ru
89081813030.ru
achillemannara.it
adiprodit.it
admashine.ru
agriturismoalrefolo.it
ahamse.it
aleaonlus.it
alessandropecoraro.it
alessandrototaro.it
alexanderkorolkov.ru
aliance.ru
andreafagioni.it
annuncitelelavoro.it
apollohairdesign.it
appartamentilignanoviacarinzia.it
argidro.ru
aristidecaruso.it
arnosportservice.it
artefunerariadiligas.it
artigianatoveneto.it
asisettoresoftair.i
augmentum-plumbi.it
avviso22018.siciliafse1420.it
baltpr.ru
beelinei.ru
berecycling.ru
betaniaroma.it
beyoganow.it
blueweld.ru
bmxklubben.dk
bojonerentacar.ru
bomode.it
brigantipaolo.it
bruschatkachelny.ru
cadcentroartedanza.it
caltapippo.it
cantierebaruffaldi.it
casataz.it
caterinafucili.it
centroippicoiplatani.it
cg.ru
chzorka.ru
cityshin.ru
cogestinformatica.it
conceptfoam.ru
consorziorebaude.it
cooparcadia.it
cortinovisrl.it
crazysportevents.it
credo.ru
cseclubgarden.it
ctsoldemo.it
cuchelschool.it
deluxtrade.ru
designpet.it
dgtaz698.ecomedincanto.it
diddyhome.fr
domvgorah.ru
dudakids.ru
ecomedincanto.it
edilemazzocco.it
education.ru
electrician.ru
esmalte.ru
estateinmontagna.it
eventi.it
exnovoband.it
farmaciazarla.it
fattoriaascaniocruciani.it
ferrum42kem.ru
festivalvocideuropa.it
fiscalformula.it
fkdent.ru
forumedia.ru
francogaliano.it
fundament163.ru
giveeme.ru
gloves.ru
gusi.ru
halloweentorino.it
hc.ru
higoldceramichecaltagirone.it
igras.ru
iknzr.ru
ilmioprovino.it
instatigers.it
irapido.it
italianfooding.it
jumpevolution.ru
kissswish.ru
ladabmhatch.ru
lecoccinelle-eventi.it
lev33.ru
line.ru
lucagisticiimpianti.it
magazine.ru
makeup.ru
maniabilitate.it
mattiaambrami.it
methossolutions.it
mfaux.ru
mioshi.it
missp.it
modulosnc.it
monicagargiulo.it
montanadance.ru
mosedu.ru
musiscuola.it
naplevat.ru
ninaco.it
novachem.it
ns500.ru
obmxklubben.dk
officinacordero.it
officinemalessandria.it
oleificiolama.it
olgakuts.ru
opt.ru
optioncapitalgroup.ru
ordenmasterov.ru
oseni.ru
ospedaliprivatiforli.it
parchisalentointour.it
perm.ru
portalrb.ru
postelclub.ru
pp.ru
profimed74.ru
proklimatshop.ru
prometey.ru
rezort.ru
ril.it
rosapellami.it
rotolonicartaroma.it
s.ru
saksmet.ru
sbnaj.ru
scuoladisportcoaching.it
selezioni-artigianatoveneto.it
serenissimagranloggiaunitaditaliaignis.it
sib.ru
siciliafse1420.it
spb.ru
sportfiske.org
strana.ru
studiocoppolasarzana.it
style.ru
sushi.ru
tappezzeriafusco.it
teambikevalconca.it
theredhair.it
tieressen.ru
trandt3.ru
unavocefuoricampo.it
verleih.it
victorycup.ru
viktoria52.ru
vitadamoglie.it
vl.ru
winskin.ru
yurestaurant.it
]|]
