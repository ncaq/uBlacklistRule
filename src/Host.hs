{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
module Host (makeHostGroups) where

import           Code
import           Import
import qualified RIO.List as L
import qualified RIO.Text as T
import           Type

-- | 簡潔に表す方法が無い場合に諦めてそのままfullもinfixも作ってしまう。
fromFull :: [Text] -> HostGroup
fromFull ts = HostGroup { hostGroupFull = ts, hostGroupInfix = ts }

-- | 全てのホスト対象のURLリストを生成します。
makeHostGroups :: [HostGroup]
makeHostGroups =
  [ abuseLikelyTopLevelDomain
  , ch
  , ghard
  , video
  , extensionExplanation
  , wikipedia
  , proxy
  , phishing
  , gtranslate
  , thirdLevelDomain
  ] <>
  tech

-- | 悪用されている可能性の高いドメイン。
-- まとめてブロックしてしまって結構副作用も大きいですが、ストレスが高いので一掃します。
-- 体感でスパム率の高いものを追加しているのと、
-- [The Spamhaus Project - The Top 10 Most Abused TLDs](https://www.spamhaus.org/statistics/tlds/)を参考にしています。
abuseLikelyTopLevelDomain :: HostGroup
abuseLikelyTopLevelDomain = fromFull $ T.lines $(embedStringFile "asset/abuse-likely-top-level-domain-site.txt")

-- | 5chコピペサイト。
-- 全て追加するのではなく、インデックスとしても価値がないものを排除しています。
ch :: HostGroup
ch = fromFull $ T.lines $(embedStringFile "asset/ch-site.txt")

-- | ゲハブログ。
ghard :: HostGroup
ghard = fromFull $ T.lines $(embedStringFile "asset/ghard-site.txt")

-- | 拡張子解説サイト。
extensionExplanation :: HostGroup
extensionExplanation = fromFull $ T.lines $(embedStringFile "asset/extension-explanation-site.txt")

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
-- キリがない気もしますが、追加できるものは追加しておきます。
phishing :: HostGroup
phishing = fromFull $ T.lines $(embedStringFile "asset/phishing.txt")

-- | 技術系スパムサイト全て。
tech :: [HostGroup]
tech = [singleTech, itMure, itSwarm, qastack, issuecloser, coderQuestion, coderSolution]

-- | 規則性があまり無いぼぼ単発の技術系コピーサイト。
singleTech :: HostGroup
singleTech = fromFull $ T.lines $(embedStringFile "asset/single-tech-site.txt")

-- | 通常サブドメインに使わないトップレベルドメインのリスト。
topLevelOnlyDomain :: [Text]
topLevelOnlyDomain = T.lines $(embedStringFile "asset/top-level-only-domain.txt")

-- | [GTranslate](https://ja.gtranslate.io/)を使った低品質な機械翻訳サイト。
-- 数値が変数に化けるのが特徴です。
-- 翻訳元のコンテンツがオリジナルの場合、
-- オリジナルに飛ぶための引っ掛かりとして価値はあるかと思って、
-- これまでブロックしてきませんでしたが、
-- このような低品質なコンテンツを配信するサイトに情報元として大した価値はないことに気がついてきたため、
-- ブロックします。
gtranslate :: HostGroup
gtranslate = fromFull $ T.lines $(embedStringFile "asset/gtranslate-site.txt")

-- | `com.br`のようなトップレベルドメインに偽装したサードレベルドメインを利用しているサイト。
-- マトモに使ってる例があるかもしれないと思って躊躇いましたが、
-- これまでスパム的なもの以外にマトモに使われている例を結局見たことがありませんでした。
thirdLevelDomain :: HostGroup
thirdLevelDomain = fromFull $ L.nub ([domain <> "." <> code | domain <- topLevelOnlyDomain, code <- codes])

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
          (["it-swarm." <> domain | domain <- topLevelOnlyDomain <> codes]) <>
          (["it-swarm-" <> code <> "." <> domain | domain <- topLevelOnlyDomain, code <- codes])

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
  where full = L.nub (["issuecloser-" <> code <> "." <> domain | domain <- topLevelOnlyDomain, code <- codes])

-- | `coder-question.com`系のサイト。
coderQuestion :: HostGroup
coderQuestion = HostGroup
  { hostGroupFull = full
  , hostGroupInfix = ["coder-question"]
  }
  where full = L.nub $
          (["coder-question." <> domain | domain <- topLevelOnlyDomain <> codes]) <>
          (["coder-question-" <> code <> "." <> domain | domain <- topLevelOnlyDomain, code <- codes])

-- | `coder-solution.com`系のサイト。
coderSolution :: HostGroup
coderSolution = HostGroup
  { hostGroupFull = full
  , hostGroupInfix = ["coder-solution"]
  }
  where full = L.nub $
          (["coder-solution." <> domain | domain <- topLevelOnlyDomain <> codes]) <>
          (["coder-solution-" <> code <> "." <> domain | domain <- topLevelOnlyDomain, code <- codes])
