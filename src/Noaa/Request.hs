{-# LANGUAGE OverloadedStrings #-}

{-| TODO module documentation -}
module Noaa.Request
  ( SortOrder (..)
  , SortField (..)

  , DataSetsParameters (..)
  , DataCatagoriesParameters (..)
  , DataTypesParameters (..)
  , LocationCatagoriesParameters (..)
  , LocationsParameters (..)
  , StationsParameters (..)
  , defaultDataSetsParameters
  , defaultDataCatagoriesParameters
  , defaultDataTypesParameters
  , defaultLocationCatagoriesParameters
  , defaultLocationsParameters
  , defaultStationsParameters

  , dataSetsRequest
  , dataCatagoriesRequest
  , dataTypesRequest
  , locationCatagoriesRequest
  , locationsRequest
  , stationsRequest
  ) where

-- NOTE from bytestring package
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8

-- NOTE from http-conduit package
import Network.HTTP.Simple
  ( Request
  , Query
  , QueryItem
  , Header
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
import Data.Time.Calendar (Day)

import Data.Maybe (catMaybes)

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

-- | Additional parameters for data sets requests, see
-- <https://www.ncdc.noaa.gov/cdo-web/webservices/v2#datasets>.
data DataSetsParameters =
  DataSetsParameters
    { dataSetsParametersDataTypeId :: Maybe String
    , dataSetsParametersLocationId :: Maybe String
    , dataSetsParametersStationId  :: Maybe String
    , dataSetsParametersStartDate  :: Maybe Day
    , dataSetsParametersEndDate    :: Maybe Day
    , dataSetsParametersSortField  :: Maybe SortField
    , dataSetsParametersSortOrder  :: Maybe SortOrder -- default is Asc
    , dataSetsParametersLimit      :: Maybe Int -- default is 25, max is 1000
    , dataSetsParametersOffset     :: Maybe Int -- default is 0
    } deriving Eq

-- | Additional parameters for data catagories requests, see
-- <https://www.ncdc.noaa.gov/cdo-web/webservices/v2#dataCategories>.
data DataCatagoriesParameters =
  DataCatagoriesParameters
    { dataCatagoriesParametersDataSetId  :: Maybe String
    , dataCatagoriesParametersLocationId :: Maybe String
    , dataCatagoriesParametersStationId  :: Maybe String
    , dataCatagoriesParametersStartDate  :: Maybe Day
    , dataCatagoriesParametersEndDate    :: Maybe Day
    , dataCatagoriesParametersSortField  :: Maybe SortField
    , dataCatagoriesParametersSortOrder  :: Maybe SortOrder -- default is Asc
    , dataCatagoriesParametersLimit      :: Maybe Int -- default is 25, max is 1000
    , dataCatagoriesParametersOffset     :: Maybe Int -- default is 0
    } deriving Eq

-- | Additional parameters for data types requests, see
-- <https://www.ncdc.noaa.gov/cdo-web/webservices/v2#dataTypes>.
data DataTypesParameters =
  DataTypesParameters
    { dataTypesParametersDataSetId      :: Maybe String
    , dataTypesParametersLocationId     :: Maybe String
    , dataTypesParametersStationId      :: Maybe String
    , dataTypesParametersDataCatagoryId :: Maybe String
    , dataTypesParametersStartDate      :: Maybe Day
    , dataTypesParametersEndDate        :: Maybe Day
    , dataTypesParametersSortField      :: Maybe SortField
    , dataTypesParametersSortOrder      :: Maybe SortOrder -- default is Asc
    , dataTypesParametersLimit          :: Maybe Int -- default is 25, max is 1000
    , dataTypesParametersOffset         :: Maybe Int -- default is 0
    } deriving Eq

-- | Additional parameters for location catagories requests, see
-- <https://www.ncdc.noaa.gov/cdo-web/webservices/v2#locationCatagories>.
data LocationCatagoriesParameters =
  LocationCatagoryParameters
    { locationCatagoriesParametersDataSetId :: Maybe String
    , locationCatagoriesParametersStartDate :: Maybe Day
    , locationCatagoriesParametersEndDate   :: Maybe Day
    , locationCatagoriesParametersSortField :: Maybe SortField
    , locationCatagoriesParametersSortOrder :: Maybe SortOrder -- default is Asc
    , locationCatagoriesParametersLimit     :: Maybe Int -- default is 25, max is 1000
    , locationCatagoriesParametersOffset    :: Maybe Int -- default is 0
    } deriving Eq

-- | Additional parameters for locations requests, see
-- <https://www.ncdc.noaa.gov/cdo-web/webservices/v2#locations>.
data LocationsParameters =
  LocationParameters
    { locationsParametersDataSetId          :: Maybe String
    , locationsParametersDataCatagoryId     :: Maybe String
    , locationsParametersLocationCatagoryId :: Maybe String
    , locationsParametersStartDate          :: Maybe Day
    , locationsParametersEndDate            :: Maybe Day
    , locationsParametersSortField          :: Maybe SortField
    , locationsParametersSortOrder          :: Maybe SortOrder -- default is Asc
    , locationsParametersLimit              :: Maybe Int -- default is 25, max is 1000
    , locationsParametersOffset             :: Maybe Int -- default is 0
    } deriving Eq

-- | Additional parameters for stations requests, see
-- <https://www.ncdc.noaa.gov/cdo-web/webservices/v2#stations>.
-- TODO replace 'stationsParametersExtent'
data StationsParameters =
  StationParameters
    { stationsParametersDataSetId      :: Maybe String
    , stationsParametersDataCatagoryId :: Maybe String
    , stationsParametersDataTypeId     :: Maybe String
    , stationsParametersExtent         :: Maybe String
    , stationsParametersStartDate      :: Maybe Day
    , stationsParametersEndDate        :: Maybe Day
    , stationsParametersSortField      :: Maybe SortField
    , stationsParametersSortOrder      :: Maybe SortOrder -- default is Asc
    , stationsParametersLimit          :: Maybe Int -- default is 25, max is 1000
    , stationsParametersOffset         :: Maybe Int -- default is 0
    } deriving Eq

-- TODO document
defaultDataSetsParameters :: DataSetsParameters
defaultDataSetsParameters =
  DataSetsParameters
    { dataSetsParametersDataTypeId = Nothing
    , dataSetsParametersLocationId = Nothing
    , dataSetsParametersStationId  = Nothing
    , dataSetsParametersStartDate  = Nothing
    , dataSetsParametersEndDate    = Nothing
    , dataSetsParametersSortField  = Nothing
    , dataSetsParametersSortOrder  = Nothing
    , dataSetsParametersLimit      = Nothing
    , dataSetsParametersOffset     = Nothing
    }

-- TODO document
defaultDataCatagoriesParameters :: DataCatagoriesParameters
defaultDataCatagoriesParameters =
  DataCatagoriesParameters
    { dataCatagoriesParametersDataSetId  = Nothing
    , dataCatagoriesParametersLocationId = Nothing
    , dataCatagoriesParametersStationId  = Nothing
    , dataCatagoriesParametersStartDate  = Nothing
    , dataCatagoriesParametersEndDate    = Nothing
    , dataCatagoriesParametersSortField  = Nothing
    , dataCatagoriesParametersSortOrder  = Nothing
    , dataCatagoriesParametersLimit      = Nothing
    , dataCatagoriesParametersOffset     = Nothing
    }

-- TODO document
defaultDataTypesParameters :: DataTypesParameters
defaultDataTypesParameters =
  DataTypesParameters
    { dataTypesParametersDataSetId      = Nothing
    , dataTypesParametersLocationId     = Nothing
    , dataTypesParametersStationId      = Nothing
    , dataTypesParametersDataCatagoryId = Nothing
    , dataTypesParametersStartDate      = Nothing
    , dataTypesParametersEndDate        = Nothing
    , dataTypesParametersSortField      = Nothing
    , dataTypesParametersSortOrder      = Nothing
    , dataTypesParametersLimit          = Nothing
    , dataTypesParametersOffset         = Nothing
    }

-- TODO document
defaultLocationCatagoriesParameters :: LocationCatagoriesParameters
defaultLocationCatagoriesParameters =
  LocationCatagoryParameters
    { locationCatagoriesParametersDataSetId = Nothing
    , locationCatagoriesParametersStartDate = Nothing
    , locationCatagoriesParametersEndDate   = Nothing
    , locationCatagoriesParametersSortField = Nothing
    , locationCatagoriesParametersSortOrder = Nothing
    , locationCatagoriesParametersLimit     = Nothing
    , locationCatagoriesParametersOffset    = Nothing
    }

-- TODO document
defaultLocationsParameters :: LocationsParameters
defaultLocationsParameters =
  LocationParameters
    { locationsParametersDataSetId          = Nothing
    , locationsParametersDataCatagoryId     = Nothing
    , locationsParametersLocationCatagoryId = Nothing
    , locationsParametersStartDate          = Nothing
    , locationsParametersEndDate            = Nothing
    , locationsParametersSortField          = Nothing
    , locationsParametersSortOrder          = Nothing
    , locationsParametersLimit              = Nothing
    , locationsParametersOffset             = Nothing
    }

-- TODO document
defaultStationsParameters :: StationsParameters
defaultStationsParameters =
  StationParameters
    { stationsParametersDataSetId      = Nothing
    , stationsParametersDataCatagoryId = Nothing
    , stationsParametersDataTypeId     = Nothing
    , stationsParametersExtent         = Nothing
    , stationsParametersStartDate      = Nothing
    , stationsParametersEndDate        = Nothing
    , stationsParametersSortField      = Nothing
    , stationsParametersSortOrder      = Nothing
    , stationsParametersLimit          = Nothing
    , stationsParametersOffset         = Nothing
    }

buildQueryItem :: Show a => B.ByteString -> a -> QueryItem
buildQueryItem name value =
  (name, Just . C8.pack . show $ value)

-- TODO use http-types package
dataSetsQuery :: DataSetsParameters -> Query
dataSetsQuery params =
  catMaybes
    [ buildQueryItem "datatypeid" <$> dataSetsParametersDataTypeId params
    , buildQueryItem "locationid" <$> dataSetsParametersLocationId params
    , buildQueryItem "stationid"  <$> dataSetsParametersStationId params
    , buildQueryItem "startdate"  <$> dataSetsParametersStartDate params
    , buildQueryItem "enddate"    <$> dataSetsParametersEndDate params
    , buildQueryItem "sortfield"  <$> dataSetsParametersSortField params
    , buildQueryItem "sortorder"  <$> dataSetsParametersSortOrder params
    , buildQueryItem "limit"      <$> dataSetsParametersLimit params
    , buildQueryItem "offset"     <$> dataSetsParametersOffset params
    ]

-- TODO use http-types package
dataCatagoriesQuery :: DataCatagoriesParameters -> Query
dataCatagoriesQuery params =
  catMaybes
    [ buildQueryItem "datasetid"  <$> dataCatagoriesParametersDataSetId params
    , buildQueryItem "locationid" <$> dataCatagoriesParametersLocationId params
    , buildQueryItem "stationid"  <$> dataCatagoriesParametersStationId params
    , buildQueryItem "startdate"  <$> dataCatagoriesParametersStartDate params
    , buildQueryItem "enddate"    <$> dataCatagoriesParametersEndDate params
    , buildQueryItem "sortfield"  <$> dataCatagoriesParametersSortField params
    , buildQueryItem "sortorder"  <$> dataCatagoriesParametersSortOrder params
    , buildQueryItem "limit"      <$> dataCatagoriesParametersLimit params
    , buildQueryItem "offset"     <$> dataCatagoriesParametersOffset params
    ]

dataTypesQuery :: DataTypesParameters -> Query
dataTypesQuery params = undefined

locationCatagoriesQuery :: LocationCatagoriesParameters -> Query
locationCatagoriesQuery params = undefined

locationsQuery :: LocationsParameters -> Query
locationsQuery params = undefined

stationsQuery :: StationsParameters -> Query
stationsQuery params = undefined

noaaHost :: B.ByteString
noaaHost =
  "www.ncdc.noaa.gov"

-- TODO use http-types package
noaaPath :: B.ByteString
noaaPath =
  "/cdo-web/api/v2"

-- TODO use http-types package
dataSetsPath :: B.ByteString
dataSetsPath =
  noaaPath `B.append` "/datasets"

-- TODO use http-types package
dataCatagoriesPath :: B.ByteString
dataCatagoriesPath =
  noaaPath `B.append` "/datacategories"

-- TODO use http-types package
dataTypesPath :: B.ByteString
dataTypesPath =
  noaaPath `B.append` "/datatypes"

-- TODO use http-types package
locationCatagoriesPath :: B.ByteString
locationCatagoriesPath =
  noaaPath `B.append` "/locationcategories"

-- TODO use http-types package
locationsPath :: B.ByteString
locationsPath =
  noaaPath `B.append` "/locations"

-- TODO use http-types package
stationsPath :: B.ByteString
stationsPath =
  noaaPath `B.append` "/stations"

defaultNoaaRequest :: B.ByteString -> Request
defaultNoaaRequest token =
    setRequestSecure True
  . setRequestHost noaaHost
  . setRequestHeader "token" [token]
  $ defaultRequest

-- TODO document
dataSetsRequest :: B.ByteString -> DataSetsParameters -> Request
dataSetsRequest token params =
    setRequestPath dataSetsPath
  . setRequestQueryString (dataSetsQuery params)
  $ defaultNoaaRequest token

-- TODO document
dataCatagoriesRequest :: B.ByteString -> DataCatagoriesParameters -> Request
dataCatagoriesRequest token params =
    setRequestPath dataCatagoriesPath
  . setRequestQueryString (dataCatagoriesQuery params)
  $ defaultNoaaRequest token

-- TODO document
dataTypesRequest :: B.ByteString -> DataTypesParameters -> Request
dataTypesRequest token params =
    setRequestPath dataTypesPath
  . setRequestQueryString (dataTypesQuery params)
  $ defaultNoaaRequest token

-- TODO document
locationCatagoriesRequest :: B.ByteString -> LocationCatagoriesParameters -> Request
locationCatagoriesRequest token params =
    setRequestPath locationCatagoriesPath
  . setRequestQueryString (locationCatagoriesQuery params)
  $ defaultNoaaRequest token

-- TODO document
locationsRequest :: B.ByteString -> LocationsParameters -> Request
locationsRequest token params =
    setRequestPath locationsPath
  . setRequestQueryString (locationsQuery params)
  $ defaultNoaaRequest token

-- TODO document
stationsRequest :: B.ByteString -> StationsParameters -> Request
stationsRequest token params =
    setRequestPath stationsPath
  . setRequestQueryString (stationsQuery params)
  $ defaultNoaaRequest token
