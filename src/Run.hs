{-# LANGUAGE NoImplicitPrelude #-}
module Run (run) where

import           Host
import           Import
import qualified RIO.List as L
import           Ubl
import           Ubo

run :: RIO App ()
run = do
  let hostGroups = makeHostGroups
  writeUblListTxt (L.concat $ hostGroupFull <$> hostGroups)
  writeUboTxt     (L.concat $ hostGroupInfix <$> hostGroups)
