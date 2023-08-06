{-# LANGUAGE QuasiQuotes #-}
-- | uBlacklistのtitleマッチを当てにしたルールたちです。
module Title (titlePattern) where

import           Import
import qualified RIO.Text as T
import           Type

titlePattern :: [Title]
titlePattern = [nullTitlePattern, redirectPattern, indexOfPattern]

-- | タイトルが分からない場合この表現になるのですが、
-- おそらく日本のGoogle限定の処理なので、
-- 他の言語対応はコントリビュート待ちです。
nullTitlePattern :: Title
nullTitlePattern = Title [[r|^無題$|]]

-- | リダイレクトが発生したらよくこのタイトルになります。
-- おそらく日本のGoogleBot向け限定のよくある処理なので
-- 他の言語対応はコントリビュート待ちです。
redirectPattern :: Title
redirectPattern = Title [[r|^待つ。$|]]

-- | Debianなどのソフトウェアのディストリビューターのftpライクなリポジトリが存在していて、
-- その負荷軽減のためのミラーサイトは多数存在します。
-- そしてそこのトップに普通のブラウザでアクセスすると、
-- ApacheやNginxの自動生成indexが返されるのでインデックスされます。
-- その結果少しマイナーなソフトウェアについて調べると見ても何の役にも立たないミラーサイトが多数引っかかります。
-- 不幸な行き違いです。
-- 全て除去しても良いかもしれませんが、
-- 誤爆が怖いためまずはGNU/Linuxディストリビューション系に多く見られる特徴に絞ってみます。
indexOfPattern :: Title
indexOfPattern = Title <$> T.lines $ T.strip [r|
^Ftp - \/pub
^Index of \/Linux
^Index of \/debian
^Index of \/ftp
^Index of \/gentoo
^Index of \/mirror
^Index of \/packages
^Index of \/pub
^Index of \/ubuntu
^ftp:\/\/ftp\.
^ftp\/
^https?:\/\/ftp\.
^of \/ftp
^of \/pub
|]
