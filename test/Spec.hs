{-# OPTIONS_GHC -F -pgmF sandwich-discover #-}
-- sandwich-discoverが生成するコードがIsStringの型をデフォルト解決させるため、
-- このモジュールに限り`-Wtype-defaults`を無効化する。
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Spec (tests) where

import Test.Sandwich

#insert_test_imports

tests :: TopSpec
tests = $(getSpecFromFolder defaultGetSpecFromFolderOptions)
