module Main where

  -- NOTE from HUnit package
import Test.HUnit (
    Counts
  , runTestTT
  )

import qualified NoaaTest

main :: IO Counts
main =
  runTestTT NoaaTest.tests
