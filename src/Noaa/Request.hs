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
  , DataParameters (..)
  , defaultDataSetsParameters
  , defaultDataCatagoriesParameters
  , defaultDataTypesParameters
  , defaultLocationCatagoriesParameters
  , defaultLocationsParameters
  , defaultStationsParameters
  , defaultDataParameters

  , dataSetsRequest
  , dataCatagoriesRequest
  , dataTypesRequest
  , locationCatagoriesRequest
  , locationsRequest
  , stationsRequest
  , dataRequest
  ) where

-- NOTE from bytestring package
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Builder (toLazyByteString)

-- NOTE from http-conduit package
import Network.HTTP.Simple
  ( Request
  , defaultRequest
  , setRequestHost
  , setRequestPort
  , setRequestPath
  , setRequestHeader
  , setRequestMethod
  , setRequestSecure
  , setRequestQueryString
  )

-- NOTE from http-types package
import Network.HTTP.Types.Header (Header)
import Network.HTTP.Types.QueryLike (QueryLike (toQuery))
import Network.HTTP.Types.URI
  ( Query
  , QueryItem
  , SimpleQuery
  , SimpleQueryItem
  , encodePathSegments
  )

-- NOTE from text package
import Data.Text (Text)

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
  LocationCatagoriesParameters
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
  LocationsParameters
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
  StationsParameters
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

-- | Additional parameters for data requests, see
-- <https://www.ncdc.noaa.gov/cdo-web/webservices/v2#data>.
-- TODO Units type for "standard" or "metric"
data DataParameters =
  DataParameters
    { dataParametersDataSetId  :: String
    , dataParametersDataTypeId :: Maybe String
    , dataParametersLocationId :: Maybe String
    , dataParametersStationId  :: Maybe String
    , dataParametersStartDate  :: Maybe Day
    , dataParametersEndDate    :: Maybe Day
    , dataParametersUnits      :: Maybe String
    , dataParametersSortField  :: Maybe SortField
    , dataParametersSortOrder  :: Maybe SortOrder -- default is Asc
    , dataParametersLimit      :: Maybe Int -- default is 25, max is 1000
    , dataParametersOffset     :: Maybe Int -- default is 0
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
  LocationCatagoriesParameters
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
  LocationsParameters
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
  StationsParameters
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

-- TODO document
defaultDataParameters :: String -> DataParameters
defaultDataParameters dataSetId =
  DataParameters
    { dataParametersDataSetId  = dataSetId
    , dataParametersDataTypeId = Nothing
    , dataParametersLocationId = Nothing
    , dataParametersStationId  = Nothing
    , dataParametersStartDate  = Nothing
    , dataParametersEndDate    = Nothing
    , dataParametersUnits      = Nothing
    , dataParametersSortField  = Nothing
    , dataParametersSortOrder  = Nothing
    , dataParametersLimit      = Nothing
    , dataParametersOffset     = Nothing
    }

buildQueryItem :: Show a => B.ByteString -> a -> QueryItem
buildQueryItem name value =
  (name, Just . C8.pack . show $ value)

instance QueryLike DataSetsParameters where
  toQuery params =
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

instance QueryLike DataCatagoriesParameters where
  toQuery params =
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

-- TODO implement
instance QueryLike DataTypesParameters where
  toQuery params = undefined

-- TODO implement
instance QueryLike LocationCatagoriesParameters where
  toQuery params = undefined

-- TODO implement
instance QueryLike LocationsParameters where
  toQuery params = undefined

-- TODO implement
instance QueryLike StationsParameters where
  toQuery params = undefined

-- TODO implement
instance QueryLike DataParameters where
  toQuery params =
    ("datasetid", Just $ C8.pack $ dataParametersDataSetId params) : undefined

noaaHost :: B.ByteString
noaaHost =
  "www.ncdc.noaa.gov"

noaaPath :: [Text]
noaaPath =
  [ "cdo-web"
  , "api"
  , "v2"
  ]

dataSetsPath :: [Text]
dataSetsPath =
  noaaPath ++ ["datasets"]

dataCatagoriesPath :: [Text]
dataCatagoriesPath =
  noaaPath ++ ["datacategories"]

dataTypesPath :: [Text]
dataTypesPath =
  noaaPath ++ ["datatypes"]

locationCatagoriesPath :: [Text]
locationCatagoriesPath =
  noaaPath ++ ["locationcategories"]

locationsPath :: [Text]
locationsPath =
  noaaPath ++ ["locations"]

stationsPath :: [Text]
stationsPath =
  noaaPath ++ ["stations"]

dataPath :: [Text]
dataPath =
  noaaPath ++ ["data"]

defaultNoaaRequest :: B.ByteString -> Request
defaultNoaaRequest token =
    setRequestSecure True
  . setRequestHost noaaHost
  . setRequestHeader "token" [token]
  $ defaultRequest

noaaRequest :: QueryLike a => B.ByteString -> [Text] -> a -> Request
noaaRequest token path params =
    setRequestPath (toStrict . toLazyByteString . encodePathSegments $ path)
  . setRequestQueryString (toQuery params)
  $ defaultNoaaRequest token

-- TODO document
dataSetsRequest :: B.ByteString -> DataSetsParameters -> Request
dataSetsRequest =
  flip noaaRequest dataSetsPath

-- TODO document
dataCatagoriesRequest :: B.ByteString -> DataCatagoriesParameters -> Request
dataCatagoriesRequest =
  flip noaaRequest dataCatagoriesPath

-- TODO document
dataTypesRequest :: B.ByteString -> DataTypesParameters -> Request
dataTypesRequest =
  flip noaaRequest dataTypesPath

-- TODO document
locationCatagoriesRequest :: B.ByteString -> LocationCatagoriesParameters -> Request
locationCatagoriesRequest =
  flip noaaRequest locationCatagoriesPath

-- TODO document
locationsRequest :: B.ByteString -> LocationsParameters -> Request
locationsRequest =
  flip noaaRequest locationsPath

-- TODO document
stationsRequest :: B.ByteString -> StationsParameters -> Request
stationsRequest =
  flip noaaRequest stationsPath

-- TODO document
dataRequest :: B.ByteString -> DataParameters -> Request
dataRequest =
  flip noaaRequest dataPath
