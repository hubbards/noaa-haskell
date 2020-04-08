{-| TODO module documentation -}
module Main where

-- NOTE from http-conduit package
import Network.HTTP.Simple
  ( Response
  , httpJSON
  )

-- NOTE from bytestring package
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8

import Noaa.Request
import Noaa.Response

-- TODO implement
main :: IO ()
main = putStrLn "App not yet implemented"

getDataSets :: String -> IO (Response (Collection DataSet))
getDataSets token =
  httpJSON $ dataSetsRequest (C8.pack token) defaultDataSetsParameters
