{-# LANGUAGE OverloadedStrings #-}

{-| TODO module documentation -}
module Noaa.Response
  ( Collection (..)
  , MetaData (..)
  , ResultSet (..)

  , DataSet (..)
  , DataCatagory (..)
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
import Data.Time.Calendar ( Day )

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
