module Main (main) where

import Himari
import Spec qualified
import Test.Sandwich

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions Spec.tests
