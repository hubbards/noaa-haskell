{-# LANGUAGE OverloadedStrings #-}

module Noaa
  ( DataSetsParams (..)
  , Collection (..)
  , MetaData (..)
  , ResultSet (..)
  , DataSet (..)
  , getDataSets
  ) where

-- NOTE: from http-conduit package
import Network.HTTP.Simple
  ( Request
  , Response
  , Query
  , QueryItem
  , Header
  , httpJSON
  , defaultRequest
  , setRequestHost
  , setRequestPort
  , setRequestPath
  , setRequestHeader
  , setRequestMethod
  , setRequestSecure
  , setRequestQueryString
  )

-- NOTE from aeson package
import Data.Aeson
  ( FromJSON (parseJSON)
  , withObject
  , (.:)
  )

-- NOTE from bytestring package
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

-- NOTE from text package
import qualified Data.Text as T

import Data.Maybe ( catMaybes )

--------------------------------------------------------------------------------
-- Request

-- TODO use URI host type
noaaHost :: B.ByteString
noaaHost = "www.ncdc.noaa.gov"

-- TODO use URI path type
noaaPath :: B.ByteString
noaaPath = "/cdo-web/api/v2"

-- TODO use URI path type
dataSetsPath :: B.ByteString
dataSetsPath = B.append noaaPath "/datasets"

data DataSetsParams =
  DataSetsParams
    { dataSetsParamsLimit  :: Maybe Int
    , dataSetsParamsOffset :: Maybe Int
    }

paramToQueryItem :: Show a => B.ByteString -> a -> QueryItem
paramToQueryItem name value = (name, Just $ C.pack $ show value)

dataSetsParamsQuery :: DataSetsParams -> Query
dataSetsParamsQuery (DataSetsParams limit offset) =
  catMaybes
    [ fmap (paramToQueryItem "limit") limit
    , fmap (paramToQueryItem "offset") offset
    ]

dataSetsRequest :: String -> DataSetsParams -> Request
dataSetsRequest token params =
  setRequestSecure True
  . setRequestMethod "GET"
  . setRequestHeader "token" [C.pack token]
  . setRequestHost noaaHost
  . setRequestPath dataSetsPath
  . setRequestQueryString (dataSetsParamsQuery params)
  $ defaultRequest

-- TODO rewrite this
getDataSets :: String -> DataSetsParams -> IO (Response (Collection DataSet))
getDataSets token params =
  httpJSON (dataSetsRequest token params)

--------------------------------------------------------------------------------
-- Response

data Collection a =
  Collection
    { collectionMetaData :: MetaData
    , collectionResults  :: [a]
    } deriving (Eq, Show)

instance FromJSON a => FromJSON (Collection a) where
  parseJSON =
    withObject "Collection" $ \ o ->
      Collection
        <$> o .: "metadata"
        <*> o .: "results"

data MetaData =
  MetaData { metaDataResultSet :: ResultSet } deriving (Eq, Show)

instance FromJSON MetaData where
  parseJSON =
    withObject "MetaData" $ \ o -> fmap MetaData (o .: "resultset")

data ResultSet =
  ResultSet
    { resultSetLimit  :: Int
    , resultSetOffset :: Int
    , resultSetCount  :: Int
    } deriving (Eq, Show)

instance FromJSON ResultSet where
  parseJSON =
    withObject "ResultSet" $ \ o ->
      ResultSet
        <$> o .: "limit"
        <*> o .: "offset"
        <*> o .: "count"

data DataSet =
  DataSet
    { dataSetUid          :: String
    , dataSetId           :: String
    , dataSetName         :: String
    , dataSetMinDate      :: String -- TODO use POSIX date type
    , dataSetMaxDate      :: String -- TODO use POSIX date type
    , dataSetDataCoverage :: Float
    } deriving (Eq, Show)

instance FromJSON DataSet where
  parseJSON =
    withObject "DataSet" $ \ o ->
      DataSet
        <$> (T.unpack <$> o .: "uid")
        <*> (T.unpack <$> o .: "id")
        <*> (T.unpack <$> o .: "name")
        <*> (T.unpack <$> o .: "mindate")
        <*> (T.unpack <$> o .: "maxdate")
        <*> o .: "datacoverage"
