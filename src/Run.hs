{-# LANGUAGE NoImplicitPrelude #-}
module Run (run) where

import           Host
import           Import
import           Ubl
import           Ubo

run :: RIO App ()
run = do
  let hosts = makeHosts
  writeUblListTxt hosts
  writeUboTxt hosts
