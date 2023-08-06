module Run (run) where

import           Host
import           Import
import           Title
import           Type
import           UBlacklist
import           UBlockOrigin

run :: RIO env ()
run = do
  let hostGroups = makeHostGroups
  writeUBlacklistTxt $ (UBlacklistPatternTitle <$> titlePattern) <> (UBlacklistPatternHostGroup <$> hostGroups)
  writeUBlockOriginTxt hostGroups
