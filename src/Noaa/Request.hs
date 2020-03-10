{-# LANGUAGE OverloadedStrings #-}

{-| TODO module documentation -}
module Noaa.Request
  ( SortOrder (..)
  , SortField (..)
  , DataSetsParams (..)
  , DataCatagoriesParams (..)
  , defaultDataSetsParams
  , defaultDataCatagoriesParams

  , dataSetsRequest
  , dataCatagoriesRequest
  ) where

-- NOTE from bytestring package
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8

-- NOTE from http-conduit package
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

-- NOTE from time package
import Data.Time.Calendar ( Day )

import Data.Maybe ( catMaybes )

-- TODO document
data SortField =
    Id
  | Name
  | MinDate
  | MaxDate
  | DataCoverage deriving Eq

instance Show SortField where
  show Id           = "id"
  show Name         = "name"
  show MinDate      = "mindate"
  show MaxDate      = "maxdate"
  show DataCoverage = "datacoverage"

-- TODO document
data SortOrder =
    Asc
  | Desc deriving Eq

instance Show SortOrder where
  show Asc  = "asc"
  show Desc = "desc"

-- TODO document
data DataSetsParams =
  DataSetsParams
    { dataSetsParamsDataTypeId :: Maybe String
    , dataSetsParamsLocationId :: Maybe String
    , dataSetsParamsStationId  :: Maybe String
    , dataSetsParamsStartDate  :: Maybe Day
    , dataSetsParamsEndDate    :: Maybe Day
    , dataSetsParamsSortField  :: Maybe SortField
    , dataSetsParamsSortOrder  :: Maybe SortOrder -- default is Asc
    , dataSetsParamsLimit      :: Maybe Int -- default is 25, max is 1000
    , dataSetsParamsOffset     :: Maybe Int -- default is 0
    } deriving Eq

-- TODO document
data DataCatagoriesParams =
  DataCatagoriesParams
    { dataCatagoriesParamsDataSetId  :: Maybe String
    , dataCatagoriesParamsLocationId :: Maybe String
    , dataCatagoriesParamsStationId  :: Maybe String
    , dataCatagoriesParamsStartDate  :: Maybe Day
    , dataCatagoriesParamsEndDate    :: Maybe Day
    , dataCatagoriesParamsSortField  :: Maybe SortField
    , dataCatagoriesParamsSortOrder  :: Maybe SortOrder -- default is Asc
    , dataCatagoriesParamsLimit      :: Maybe Int -- default is 25, max is 1000
    , dataCatagoriesParamsOffset     :: Maybe Int -- default is 0
    } deriving Eq

-- TODO document
defaultDataSetsParams :: DataSetsParams
defaultDataSetsParams =
  DataSetsParams
    { dataSetsParamsDataTypeId = Nothing
    , dataSetsParamsLocationId = Nothing
    , dataSetsParamsStationId  = Nothing
    , dataSetsParamsStartDate  = Nothing
    , dataSetsParamsEndDate    = Nothing
    , dataSetsParamsSortField  = Nothing
    , dataSetsParamsSortOrder  = Nothing
    , dataSetsParamsLimit      = Nothing
    , dataSetsParamsOffset     = Nothing
    }

-- TODO document
defaultDataCatagoriesParams :: DataCatagoriesParams
defaultDataCatagoriesParams =
  DataCatagoriesParams
    { dataCatagoriesParamsDataSetId  = Nothing
    , dataCatagoriesParamsLocationId = Nothing
    , dataCatagoriesParamsStationId  = Nothing
    , dataCatagoriesParamsStartDate  = Nothing
    , dataCatagoriesParamsEndDate    = Nothing
    , dataCatagoriesParamsSortField  = Nothing
    , dataCatagoriesParamsSortOrder  = Nothing
    , dataCatagoriesParamsLimit      = Nothing
    , dataCatagoriesParamsOffset     = Nothing
    }

-- TODO rewrite
buildQueryItem :: Show a => B.ByteString -> a -> QueryItem
buildQueryItem name value =
  (name, Just . C8.pack . show $ value)

-- TODO rewrite
dataSetsParamsQuery :: DataSetsParams -> Query
dataSetsParamsQuery params =
  catMaybes
    [ fmap (buildQueryItem "datatypeid") (dataSetsParamsDataTypeId params)
    , fmap (buildQueryItem "locationid") (dataSetsParamsLocationId params)
    , fmap (buildQueryItem "stationid") (dataSetsParamsStationId params)
    , fmap (buildQueryItem "startdate") (dataSetsParamsStartDate params)
    , fmap (buildQueryItem "enddate") (dataSetsParamsEndDate params)
    , fmap (buildQueryItem "sortfield") (dataSetsParamsSortField params)
    , fmap (buildQueryItem "sortorder") (dataSetsParamsSortOrder params)
    , fmap (buildQueryItem "limit") (dataSetsParamsLimit params)
    , fmap (buildQueryItem "offset") (dataSetsParamsOffset params)
    ]

-- TODO rewrite
dataCatagoriesParamsQuery :: DataCatagoriesParams -> Query
dataCatagoriesParamsQuery params =
  catMaybes
    [ fmap (buildQueryItem "datasetid") (dataCatagoriesParamsDataSetId params)
    , fmap (buildQueryItem "locationid") (dataCatagoriesParamsLocationId params)
    , fmap (buildQueryItem "stationid") (dataCatagoriesParamsStationId params)
    , fmap (buildQueryItem "startdate") (dataCatagoriesParamsStartDate params)
    , fmap (buildQueryItem "enddate") (dataCatagoriesParamsEndDate params)
    , fmap (buildQueryItem "sortfield") (dataCatagoriesParamsSortField params)
    , fmap (buildQueryItem "sortorder") (dataCatagoriesParamsSortOrder params)
    , fmap (buildQueryItem "limit") (dataCatagoriesParamsLimit params)
    , fmap (buildQueryItem "offset") (dataCatagoriesParamsOffset params)
    ]

noaaHost :: B.ByteString
noaaHost =
  "www.ncdc.noaa.gov"

noaaPath :: B.ByteString
noaaPath =
  "/cdo-web/api/v2"

dataSetsPath :: B.ByteString
dataSetsPath =
  noaaPath `B.append` "/datasets"

dataCatagoriesPath :: B.ByteString
dataCatagoriesPath =
  noaaPath `B.append` "/datacategories"

defaultNoaaRequest :: B.ByteString -> Request
defaultNoaaRequest token =
    setRequestSecure True
  . setRequestHost noaaHost
  . setRequestHeader "token" [token]
  $ defaultRequest

-- TODO document
dataSetsRequest :: B.ByteString -> DataSetsParams -> Request
dataSetsRequest token params =
    setRequestPath dataSetsPath
  . setRequestQueryString (dataSetsParamsQuery params)
  $ defaultNoaaRequest token

-- getDataSets :: String -> DataSetsParams
--                       -> IO (Response (Collection DataSet))
-- getDataSets token params =
--   httpJSON (dataSetsRequest token params)

-- TODO document
dataCatagoriesRequest :: B.ByteString -> DataCatagoriesParams -> Request
dataCatagoriesRequest token params =
    setRequestPath dataCatagoriesPath
  . setRequestQueryString (dataCatagoriesParamsQuery params)
  $ defaultNoaaRequest token

-- getDataCatagories :: String -> DataCatagoriesParams
--                             -> IO (Response (Collection DataCatagory))
-- getDataCatagories token params =
--   httpJSON (dataCatagoriesRequest token params)
