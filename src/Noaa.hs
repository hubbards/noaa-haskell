{-# LANGUAGE OverloadedStrings #-}

{-| TODO module documentation
-}
module Noaa where

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
  , (.:?)
  )

-- NOTE from bytestring package
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8

-- NOTE from time package
import Data.Time.Calendar ( Day )

import Data.Maybe ( catMaybes )

--------------------------------------------------------------------------------
-- Request

-- TODO use URI host type
noaaHost :: B.ByteString
noaaHost =
  "www.ncdc.noaa.gov"

-- TODO use URI path type
noaaPath :: B.ByteString
noaaPath =
  "/cdo-web/api/v2"

-- TODO use URI path type
dataSetsPath :: B.ByteString
dataSetsPath =
  B.append noaaPath "/datasets"

paramToQueryItem :: Show a => B.ByteString -> a -> QueryItem
paramToQueryItem name value =
  (name, Just $ C8.pack $ show value)

defaultNoaaRequest :: String -> Request
defaultNoaaRequest token =
    setRequestSecure True
  . setRequestMethod "GET"
  . setRequestHeader "token" [C8.pack token]
  . setRequestHost noaaHost
  $ defaultRequest

-- TODO add missing parameters
data DataSetsParams =
  DataSetsParams
    { dataSetsParamsLimit  :: Maybe Int
    , dataSetsParamsOffset :: Maybe Int
    }

-- TODO add missing parameters
dataSetsParamsQuery :: DataSetsParams -> Query
dataSetsParamsQuery (DataSetsParams limit offset) =
  catMaybes
    [ fmap (paramToQueryItem "limit") limit
    , fmap (paramToQueryItem "offset") offset
    ]

dataSetsRequest :: String -> DataSetsParams -> Request
dataSetsRequest token params =
    setRequestPath dataSetsPath
  . setRequestQueryString (dataSetsParamsQuery params)
  $ defaultNoaaRequest token

-- TODO rewrite this
getDataSets :: String -> DataSetsParams -> IO (Response (Collection DataSet))
getDataSets token params =
  httpJSON (dataSetsRequest token params)

--------------------------------------------------------------------------------
-- Response

-- | Domain type for collections.
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

-- | Domain type for meta data.
newtype MetaData =
  MetaData { metaDataResultSet :: ResultSet } deriving (Eq, Show)

instance FromJSON MetaData where
  parseJSON =
    withObject "MetaData" $ fmap MetaData . flip (.:) "resultset"

-- | Domain type for result sets.
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

-- | Domain type for data sets, see
-- <https://www.ncdc.noaa.gov/cdo-web/webservices/v2#datasets>.
data DataSet =
  DataSet
    { dataSetUid          :: Maybe String
    , dataSetId           :: String
    , dataSetName         :: String
    , dataSetMinDate      :: Day
    , dataSetMaxDate      :: Day
    , dataSetDataCoverage :: Float
    } deriving (Eq, Show)

instance FromJSON DataSet where
  parseJSON =
    withObject "DataSet" $ \ o ->
      DataSet
        <$> o .:? "uid" -- missing on single item results
        <*> o .:  "id"
        <*> o .:  "name"
        <*> o .:  "mindate" -- ISO formatted date
        <*> o .:  "maxdate" -- ISO formatted date
        <*> o .:  "datacoverage"
