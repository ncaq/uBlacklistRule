module Run (run) where

import Himari
import Host
import Title
import Type
import UBlacklist
import UBlockOrigin

run :: (MonadIO m) => m ()
run = do
  let hostGroups = makeHostGroups
  writeUBlacklistTxt $
    (UBlacklistPatternTitle <$> titlePattern)
      <> (UBlacklistPatternHostGroup <$> hostGroups)
  writeUBlockOriginTxt hostGroups
