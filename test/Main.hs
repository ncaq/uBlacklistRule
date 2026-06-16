module Main (main) where

import RIO
import Spec qualified
import Test.Sandwich

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions Spec.tests
