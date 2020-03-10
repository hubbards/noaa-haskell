{-| TODO module documentation -}
module Main where

  -- NOTE from HUnit package
import Test.HUnit (
    Counts
  , runTestTT
  )

import qualified Noaa.RequestTest
import qualified Noaa.ResponseTest

main :: IO Counts
main =
  runTestTT Noaa.ResponseTest.tests
