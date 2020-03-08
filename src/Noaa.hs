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

-- TODO use URI path type
dataSetsPath :: B.ByteString
dataSetsPath =
  B.append noaaPath "/datasets"

data SortOrder =
    Asc
  | Desc deriving Eq

instance Show SortOrder where
  show Asc  = "asc"
  show Desc = "desc"

data DataSetsSortField =
    Id
  | Name
  | MinDate
  | MaxDate
  | DataCoverage deriving Eq

instance Show DataSetsSortField where
  show Id           = "id"
  show Name         = "name"
  show MinDate      = "mindate"
  show MaxDate      = "maxdate"
  show DataCoverage = "datacoverage"

data DataSetsParams =
  DataSetsParams
    { dataSetsParamsDataTypeId :: Maybe String
    , dataSetsParamsLocationId :: Maybe String
    , dataSetsParamsStationId  :: Maybe String
    , dataSetsParamsStartDate  :: Maybe Day
    , dataSetsParamsEndDate    :: Maybe Day
    , dataSetsParamsSortField  :: Maybe DataSetsSortField
    , dataSetsParamsSortOrder  :: Maybe SortOrder
    -- default is Asc
    , dataSetsParamsLimit      :: Maybe Int
    -- default is 25 and max is 1000
    , dataSetsParamsOffset     :: Maybe Int
    -- default is 0
    }

dataSetsParamsQuery :: DataSetsParams -> Query
dataSetsParamsQuery params =
  catMaybes
    [ fmap (paramToQueryItem "datatypeid") (dataSetsParamsDataTypeId params)
    , fmap (paramToQueryItem "locationid") (dataSetsParamsLocationId params)
    , fmap (paramToQueryItem "stationid") (dataSetsParamsStationId params)
    , fmap (paramToQueryItem "startdate") (dataSetsParamsStartDate params)
    , fmap (paramToQueryItem "enddate") (dataSetsParamsEndDate params)
    , fmap (paramToQueryItem "sortfield") (dataSetsParamsSortField params)
    , fmap (paramToQueryItem "sortorder") (dataSetsParamsSortOrder params)
    , fmap (paramToQueryItem "limit") (dataSetsParamsLimit params)
    , fmap (paramToQueryItem "offset") (dataSetsParamsOffset params)
    ]

dataSetsRequest :: String -> DataSetsParams -> Request
dataSetsRequest token params =
    setRequestPath dataSetsPath
  . setRequestQueryString (dataSetsParamsQuery params)
  $ defaultNoaaRequest token

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
