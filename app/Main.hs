{-# LANGUAGE NoImplicitPrelude #-}
module Main (main) where

import           Import
import           RIO.Process
import           Run

main :: IO ()
main = do
  lo <- logOptionsHandle stderr True
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app = App
          { appLogFunc = lf
          , appProcessContext = pc
          }
     in runRIO app run
