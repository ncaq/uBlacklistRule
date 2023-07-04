module Run (run) where

import           Host
import           Import
import qualified RIO.List     as L
import           UBlacklist
import           UBlockOrigin

run :: RIO App ()
run = do
  let hostGroups = makeHostGroups
  writeUBlacklistTxt   (L.concat $ hostGroupFull  <$> hostGroups)
  writeUBlockOriginTxt (L.concat $ hostGroupInfix <$> hostGroups)
