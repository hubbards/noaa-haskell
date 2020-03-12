{-# LANGUAGE OverloadedStrings #-}

{-| TODO module documentation -}
module Noaa.Response
  ( Collection (..)
  , MetaData (..)
  , ResultSet (..)

  , DataSet (..)
  , DataCatagory (..)
  , DataType (..)
  , LocationCatagory (..)
  , Location (..)
  , Station (..)
  ) where

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
import Data.Time.Calendar (Day)

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
    { dataSetId           :: String
    , dataSetName         :: String
    , dataSetMinDate      :: Day
    , dataSetMaxDate      :: Day
    , dataSetDataCoverage :: Float
    , dataSetUid          :: Maybe String
    } deriving (Eq, Show)

instance FromJSON DataSet where
  parseJSON =
    withObject "DataSet" $ \ o ->
      DataSet
        <$> o .:  "id"
        <*> o .:  "name"
        <*> o .:  "mindate" -- ISO formatted date
        <*> o .:  "maxdate" -- ISO formatted date
        <*> o .:  "datacoverage"
        <*> o .:? "uid" -- missing on single item results

-- | Domain type for data catagories, see
-- <https://www.ncdc.noaa.gov/cdo-web/webservices/v2#dataCategories>.
data DataCatagory =
  DataCatagory
    { dataCatagoryId   :: String
    , dataCatagoryName :: String
    } deriving (Eq, Show)

instance FromJSON DataCatagory where
  parseJSON =
    withObject "DataCatagory" $ \ o ->
      DataCatagory
        <$> o .: "id"
        <*> o .: "name"

-- | Domain type for data types, see
-- <https://www.ncdc.noaa.gov/cdo-web/webservices/v2#dataTypes>.
data DataType =
  DataType
    { dataTypeId           :: String
    , dataTypeName         :: Maybe String
    , dataTypeMinDate      :: Day
    , dataTypeMaxDate      :: Day
    , dataTypeDataCoverage :: Float
    } deriving (Eq, Show)

instance FromJSON DataType where
  parseJSON =
    withObject "DataType" $ \ o ->
      DataType
        <$> o .:  "id"
        <*> o .:? "name" -- missing on single item results
        <*> o .:  "mindate" -- ISO formatted date
        <*> o .:  "maxdate" -- ISO formatted date
        <*> o .:  "datacoverage"

-- | Domain type for location catagories, see
-- <https://www.ncdc.noaa.gov/cdo-web/webservices/v2#locationCategories>.
data LocationCatagory =
  LocationCatagory
    { locationCatagoryId   :: String
    , locationCatagoryName :: String
    } deriving (Eq, Show)

instance FromJSON LocationCatagory where
  parseJSON =
    withObject "LocationCatagory" $ \ o ->
      LocationCatagory
        <$> o .: "id"
        <*> o .: "name"

-- | Domain type for locations, see
-- <https://www.ncdc.noaa.gov/cdo-web/webservices/v2#locations>.
data Location =
  Location
    { locationId           :: String
    , locationName         :: String
    , locationMinDate      :: Day
    , locationMaxDate      :: Day
    , locationDataCoverage :: Float
    } deriving (Eq, Show)

instance FromJSON Location where
  parseJSON =
    withObject "Location" $ \ o ->
      Location
        <$> o .: "id"
        <*> o .: "name"
        <*> o .: "mindate" -- ISO formatted date
        <*> o .: "maxdate" -- ISO formatted date
        <*> o .: "datacoverage"

-- | Domain type for stations, see
-- <https://www.ncdc.noaa.gov/cdo-web/webservices/v2#stations>.
data Station =
  Station
    { stationId             :: String
    , stationName           :: String
    , stationMinDate        :: Day
    , stationMaxDate        :: Day
    , stationDataCoverage   :: Float
    -- TODO replace with units of measurement
    , stationElevation      :: Float
    , stationElevationUnits :: String
    , stationLatitude       :: Float
    , stationLongitude      :: Float
    } deriving (Eq, Show)

-- TODO implement
instance FromJSON Station where
  parseJSON =
    withObject "Station" $ \ o ->
      Station
        <$> o .: "id"
        <*> o .: "name"
        <*> o .: "mindate" -- ISO formatted date
        <*> o .: "maxdate" -- ISO formatted date
        <*> o .: "datacoverage"
        <*> o .: "elevation"
        <*> o .: "elevationUnit"
        <*> o .: "latitude"
        <*> o .: "longitude"
