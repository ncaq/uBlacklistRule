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
796t.com
888wenti.com
ajaxhispano.com
answer-id.com
answerlib.com
array.cyou
artfit-prk.com
article.docway.net
askdev.info
athabasca-foto.com
baksi.xyz
bar-stools-plus.com
base64.work
bestecode.com
bleepcoder.com
buginfo.tech
bugsdb.com
cfadnc.org
ciupacabra.com
classmethod.dev
clearwatergardenclub.org
cloud.tencent.com/developer/ask
cloud6.net
cndgn.com
cnpython.com
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
codeguides.site
codehero.jp
codengineering.ru
codenong.com
coder.work
coderedirect.com
coderoad.ru
codersatellite.com
codetd.com
codingdict.com
coredump.biz
crypto-days.jp
csdn.net
daimajiaoliu.com
daplus.net
de-vraag.com
debugcn.com
delavaio.com
devadvisor.io
developreference.com
docow.com
docway.net
dovov.com
edureka.co
elfishgene.com
euromar2012.org
exceptionshub.com
extutorial.com
firstlightsalon.in
fixes.pub
gajet.club
geek-qa.imtqy.com
geek-tips.imtqy.com
geekcxy.com
geeks-world.imtqy.com
generacodice.com
ghcc.net
github-dotcom.gateway.web.tr
githubhelp.com
githubmemory.com
gitmemory.com
gtainspections.com
helloworldkb.com
helpex.vn
hi.qaru.tech
hu.qaru.tech
huati365.com
hub.fastgit.org
ichi.pro
ifaj-congress.org
includestdio.com
isolution.pro
issue.life
issueexplorer.com
issues-world.com
it-gundan.net
itbloger.tistory.com
iteramos.com
itnews.org
itread01.com
izziswift.com
javaer101.com
jb51.cc
jpcloud.net
jpcodeqa.com
kaifa99.com
kejisen.com
knews.vip
konnichiwasekai.com
kotaeta.com
laptrinhx.com
legkovopros.ru
living-sun.com
lycaeum.dev
mianquan.net
mianshigee.com
mlog.club
newbedev.com
nobis.work
ojit.com
orcode.com
ostack.cn
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
qapicks.com
qaru.site
qathai.com
qedev.com
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
siwib.org
softonic.com
softonic.jp
soinside.com
sqlite.in
stackfinder.net
stacknoob.com
stackoom.com
stackoverfill.com
stackovergo.com
stackovernet.com
stackovernet.xyz
stackoverrun.com
stackscn.com
steakrecords.com
superduperartscamp.com
suttonedfoundation.org
switch-case.com
switch-case.ru
t.codebug.vip
tech-ja.netlify.app
techfeed.net
thercb.org
thinbug.com
ti-enxame.com
timeglobal.cn
titanwolf.org
tlcpv.org
try2explore.com
tutorialguruji.com
tutorialmore.com
ubuntu.buildwebhost.com
ubuntugeeks.com
uebu-kaihatsu.jp.net
uefir.com
usersuper.ru
utyatnishna.ru
uwenku.com
vfwpost8762.org
visual-foxpro-programmer.com
voidcc.com
web-dev-qa.com
webdevqa.jp.net
wenyanet.com
while-do.com
xbuba.com
younggeeks.in
zhouni.net
zweryfikowac.pl
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
agenciavimos.cl
agestion.cl
agricolachillepin.cl
agriturismoalrefolo.it
agzulueta.cl
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
attac.cl
augmentum-plumbi.it
avviso22018.siciliafse1420.it
baltpr.ru
beelinei.ru
berecycling.ru
betaniaroma.it
betarenovables.cl
beyoganow.it
blog.artwolf.in
blueweld.ru
bmxklubben.dk
bojonerentacar.ru
bomode.it
brigantipaolo.it
brn.cl
bruschatkachelny.ru
cadcentroartedanza.it
caltapippo.it
cantierebaruffaldi.it
casadelaempanada.cl
casataz.it
caterinafucili.it
centroippicoiplatani.it
cg.ru
chzorka.ru
cityshin.ru
cogestinformatica.it
colegiobalmaceda.cl
conceptfoam.ru
consorziorebaude.it
constructorweb.cl
cooparcadia.it
cortinaslincort.cl
cortinovisrl.it
crazysportevents.it
credo.ru
cseclubgarden.it
ctsoldemo.it
cuchelschool.it
davidg.cl
deluxesport.cl
deluxtrade.ru
designpet.it
dgtaz698.ecomedincanto.it
diariopatagon.cl
diddyhome.fr
domvgorah.ru
ducom.cl
dudakids.ru
ecomedincanto.it
edilemazzocco.it
education.ru
elcarpincho.cl
electrician.ru
escueladedeportes.cl
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
generaindustrial.cl
giveeme.ru
gloves.ru
gusi.ru
halloweentorino.it
hc.ru
hidratate.cl
higoldceramichecaltagirone.it
hospitalantofagasta.cl
idearteacademy.cl
igras.ru
iknzr.ru
ilmioprovino.it
instatigers.it
irapido.it
italianfooding.it
izakayayoko.cl
jumpevolution.ru
kissswish.ru
kuins.cl
ladabmhatch.ru
lahaciendarestaurante.cl
lecoccinelle-eventi.it
lejournaldec.cl
lev33.ru
line.ru
losangelesudec.cl
lucagisticiimpianti.it
lvn.cl
maderasconta.cl
magazine.ru
makeup.ru
manejateporlavida.cl
maniabilitate.it
mattiaambrami.it
megaferias.cl
metagame.cl
methossolutions.it
mfaux.ru
mioshi.it
missp.it
mnvb.cl
modulosnc.it
monicagargiulo.it
montanadance.ru
mosedu.ru
musiscuola.it
naplevat.ru
ninaco.it
novachem.it
ns500.ru
nutricionistarociosuarez.cl
obmxklubben.dk
officinacordero.it
officinemalessandria.it
oleificiolama.it
olgakuts.ru
onlystylemakeup.cl
opt.ru
optioncapitalgroup.ru
ordenmasterov.ru
oseni.ru
ospedaliprivatiforli.it
parchisalentointour.it
perm.ru
pomu.cl
portalrb.ru
postelclub.ru
pp.ru
prensamafil.cl
prismasoftware.cl
profimed74.ru
proklimatshop.ru
prometey.ru
quarentenet.cl
rail.cl
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
shippinghome.cl
sib.ru
siciliafse1420.it
sociedadteosofica.cl
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
uchile.cl
unavocefuoricampo.it
verleih.it
victorycup.ru
viktoria52.ru
vitadamoglie.it
vl.ru
winskin.ru
yurestaurant.it
|]
