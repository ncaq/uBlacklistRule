# uBlacklistRule

[uBlacklist](https://iorate.github.io/ublacklist/)向けのルールです。

# 購読

こちらから最新版の購読が可能です。

<https://raw.githubusercontent.com/ncaq/uBlacklistRule/master/uBlacklist.txt>

Google Chrome(Desktop版)を利用している場合は、
[こちら](https://iorate.github.io/ublacklist/subscribe?name=ncaq-uBlacklistRule&url=https://raw.githubusercontent.com/ncaq/uBlacklistRule/master/uBlacklist.txt)
をクリックすることで、購読する直前までの操作をスキップすることが出来ます。

[Firefox for Android](https://www.mozilla.org/ja/firefox/browsers/mobile/android/)
向けの、
[uBlock Origin](https://addons.mozilla.org/ja/firefox/addon/ublock-origin/)
で利用するためのルールはこちらです。

<https://raw.githubusercontent.com/ncaq/uBlacklistRule/master/uBlockOrigin.txt>

Firefox for Androidで、
[Google Search Fixer](https://addons.mozilla.org/ja/firefox/addon/google-search-fixer/)
を有効にした状態でしかテストしていません。

# 何故既にルールを共有するリポジトリがあるのに新しく作ったのか

[arosh/ublacklist-stackoverflow-translation: Stack Overflow の機械翻訳サイトの除外用フィルタ](https://github.com/arosh/ublacklist-stackoverflow-translation)
など、
すばらしい先駆者様が居ます。

ただ、
このリポジトリの対象はあくまで機械翻訳サイトのみなので、
もっと広くブロックする自前のルールを作っていました。

メンテナンスが結構大変になってきたので、
生成ツール付きで独立リポジトリとして公開することにしました。

# ブロックするサイト

## 技術系スパムサイト

[Stack Overflow](https://stackoverflow.com/)などから機械翻訳した内容を大量生成するサイトです。

翻訳してないで単にコピーしているサイトも対象です。

### 機械翻訳だけどブロックしないサイト

<https://blog.desdelinux.net/>
の日本語版などは明らかに低品質な機械翻訳を乱発しています。

しかし、
これらのサイトは英語版は別にどこからかコピーしているわけではないようです。
あくまで英語版のニュースをオリジナルで執筆して、
機械翻訳にかけているようです。
推測ですが。

機械翻訳が低品質すぎて理解不能なのですが、
これをブロックしてしまうと本家ニュース記事にたどり着くのが困難になるので、
あえてブロックしていません。

## 拡張子解説サイト

ファイル拡張子に関するページを機械で大量生成しているサイト。

まともに解説しているならば良いのですが、
大抵は間違っているかテキストエディタか拡張子判別ソフトみたいなものを宣伝しているだけです。

普通に検索して拡張子が意味するファイルの種別が分かれば、
そこから検索し直したほうが早いし正確。
もしくはテキストファイルとして開いて内容を確認するか、
`file`コマンドを使えば良いです。
拡張子と明示的に検索しなくても出てくることが多く有害。

## コピペサイト

特に新しい情報を生み出さないデッドコピーなサイト。
webプロキシなども対象です。
明示的にwebプロキシを使いたい場合は、
検索結果経由で行かないでブックマークなどから行けますよね?
なのでブロックしても問題ないと判断しています。

## 5chのコピーサイト

[５ちゃんねる](https://5ch.net/)などから内容をコピーした、
**機械生成の割合が高い**サイトです。

### 全てをブロックしない理由

一つは、
あまりにもサイト数が多いので対処しきれないためです。

もう一つは、
5chは条件付きでまとめサイトの生成を許可しているためです。
[5chまとめブログ・5chまとめアプリ運営者の皆さまへ](https://5ch.net/matome.html)

## YouTube、ニコニコ動画などのコピーサイト

インラインで動画を出していたりメタデータを転載しているサイトです。

## ゲハブログ

主にゲームに関するネガティブなデマを多く含む情報を撒き散らすことでPVを稼ぐサイトは、
ゲームを楽しむ上で障害になるので除外します。

## その他検索の役に立たないサイト

要らない検索結果はガンガン排除していきます。

# 存在しないサイトのURLがたくさん載っている理由

コピーサイトはドメインの一部分を変えるなどの方法でどんどん増えていきます。
増えるたびにリストに追加するのは面倒です。
機械増殖には機械増殖で対応します。
よってこちらもURLを自動生成します。

# 何故類似のサイトを正規表現で除外しないのですか?

uBlacklistの現在のバージョンでは、
ルールに正規表現を使うことが出来ます。

これを使えば同じサイトのトップレベルドメインだけ変えたものだけを簡素に1行でブロックすることが可能です。

しかし私はあえてそれを使っていません。
その理由は複数あります。

1つめの理由は、
このサイトがブロックされているか確認したいといった時に、
単純なテキストの並びならすぐにgrepやテキストエディタの検索などで検索できますが、
正規表現はどのルールがマッチするかどうか確認するツールを作るのが必要です。
それは面倒です。

2つめの理由は、
テキストデータをそのまま作るだけなら正規表現で無駄な繰り返しを除外するのは大いに有用ですが、
Haskellプログラムでデータを生成する場合、
正規表現より書きやすく除外出来るので、
わざわざ正規表現を使う意義があまり無いためです。

# このサイトが載ってないのはおかしい/このサイトが載っているのはおかしい

Issue, Pull Requestを是非ともお待ちしております。

Issueだけ建てるのも歓迎しますし、
PRを作ってくださるのも歓迎いたします。

# 関連リンク

* [iorate/ublacklist: Blocks specific sites from appearing in Google search results](https://github.com/iorate/uBlacklist)
* [uBlacklist - iorate.github.io](https://iorate.github.io/ublacklist/)
* [Subscriptions | uBlacklist](https://iorate.github.io/ublacklist/subscriptions)
* [uBlacklistを使ってポケモン徹底攻略やStack OverflowのコピペサイトをGoogle検索から除外しましょう - ncaq](https://www.ncaq.net/2019/12/18/19/18/05/)
* [アフィカスリスト作成所 - なんJ AdGuard部 Wiki*](https://wikiwiki.jp/nanj-adguard/%E3%82%A2%E3%83%95%E3%82%A3%E3%82%AB%E3%82%B9%E3%83%AA%E3%82%B9%E3%83%88%E4%BD%9C%E6%88%90%E6%89%80)
