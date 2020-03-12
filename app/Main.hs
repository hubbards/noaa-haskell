{-# LANGUAGE OverloadedStrings #-}

{-| TODO module documentation -}
module Main where

-- NOTE from http-conduit package
import Network.HTTP.Simple
  ( Response
  , httpJSON
  )

-- NOTE from bytestring package
import qualified Data.ByteString as B

import Noaa.Request
import Noaa.Response

-- TODO implement
main :: IO ()
main = putStrLn "App not yet implemented"

getDataSets :: B.ByteString -> DataSetsParameters -> IO (Response (Collection DataSet))
getDataSets token params = httpJSON (dataSetsRequest token params)
