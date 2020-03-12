{-# LANGUAGE OverloadedStrings #-}

{-| TODO module documentation -}
module Noaa.ResponseTest ( tests ) where

-- NOTE from HUnit package
import Test.HUnit (
    Test
  , test
  , (~:)
  , (~?=)
  , (~=?)
  , (@?=)
  , (@=?)
  , (@?)
  )

-- NOTE from aeson package
import Data.Aeson ( decode )

-- NOTE from bytestring package
import qualified Data.ByteString.Lazy as B

-- NOTE from time package
import Data.Time.Format.ISO8601 ( iso8601ParseM )

import Data.Maybe ( fromJust )

import Noaa.Response

tests :: Test
tests =
  test
    [ decode dataSet1' ~?= Just dataSet1
    , decode dataSet2' ~?= Just dataSet2
    , decode dataSet3' ~?= Just dataSet3
    , decode dataSets' ~?= Just dataSets
    , decode dataCatagory1'  ~?= Just dataCatagory1
    , decode dataCatagory2'  ~?= Just dataCatagory2
    , decode dataCatagory3'  ~?= Just dataCatagory3
    , decode dataCatagories' ~?= Just dataCatagories
    , decode dataType1' ~?= Just dataType1
    , decode dataType2' ~?= Just dataType2
    , decode dataType3' ~?= Just dataType3
    , decode dataTypes' ~?= Just dataTypes
    ]


dataSet1 :: DataSet
dataSet1 =
  DataSet
    { dataSetId           = "GHCND"
    , dataSetName         = "Daily Summaries"
    , dataSetMinDate      = fromJust (iso8601ParseM "1763-01-01")
    , dataSetMaxDate      = fromJust (iso8601ParseM "2020-03-05")
    , dataSetDataCoverage = 1
    , dataSetUid          = Nothing
    }

dataSet2 :: DataSet
dataSet2 =
  DataSet
    { dataSetId           = "GSOM"
    , dataSetName         = "Global Summary of the Month"
    , dataSetMinDate      = fromJust (iso8601ParseM "1763-01-01")
    , dataSetMaxDate      = fromJust (iso8601ParseM "2020-01-01")
    , dataSetDataCoverage = 1
    , dataSetUid          = Nothing
    }

dataSet3 :: DataSet
dataSet3 =
  DataSet
    { dataSetId           = "GSOY"
    , dataSetName         = "Global Summary of the Year"
    , dataSetMinDate      = fromJust (iso8601ParseM "1763-01-01")
    , dataSetMaxDate      = fromJust (iso8601ParseM "2019-01-01")
    , dataSetDataCoverage = 1
    , dataSetUid          = Nothing
    }

dataSets :: Collection DataSet
dataSets =
  Collection
    { collectionMetaData =
        MetaData $ ResultSet
          { resultSetLimit  = 5
          , resultSetOffset = 1
          , resultSetCount  = 3
          }
    , collectionResults  =
      [ dataSet1 { dataSetUid = Just "gov.noaa.ncdc:C00861" }
      , dataSet2 { dataSetUid = Just "gov.noaa.ncdc:C00946" }
      , dataSet3 { dataSetUid = Just "gov.noaa.ncdc:C00947" }
      ]
    }

dataSet1' :: B.ByteString
dataSet1' =
  "{ \"id\":           \"GHCND\" \
\  , \"name\":         \"Daily Summaries\" \
\  , \"mindate\":      \"1763-01-01\" \
\  , \"maxdate\":      \"2020-03-05\" \
\  , \"datacoverage\": 1 \
\  }"

dataSet2' :: B.ByteString
dataSet2' =
  "{ \"id\":           \"GSOM\" \
\  , \"name\":         \"Global Summary of the Month\" \
\  , \"mindate\":      \"1763-01-01\" \
\  , \"maxdate\":      \"2020-01-01\" \
\  , \"datacoverage\": 1 \
\  }"

dataSet3' :: B.ByteString
dataSet3' =
  "{ \"id\":           \"GSOY\" \
\  , \"name\":         \"Global Summary of the Year\" \
\  , \"mindate\":      \"1763-01-01\" \
\  , \"maxdate\":      \"2019-01-01\" \
\  , \"datacoverage\": 1 \
\  }"

dataSets' :: B.ByteString
dataSets' =
  "{ \"metadata\": \
\    { \"resultset\": \
\      { \"offset\": 1 \
\      , \"count\":  3 \
\      , \"limit\":  5 \
\      } \
\    } \
\  , \"results\": \
\    [ \
\      { \"id\":           \"GHCND\" \
\      , \"name\":         \"Daily Summaries\" \
\      , \"mindate\":      \"1763-01-01\" \
\      , \"maxdate\":      \"2020-03-05\" \
\      , \"datacoverage\": 1 \
\      , \"uid\":          \"gov.noaa.ncdc:C00861\" \
\      } \
\    , { \"id\":           \"GSOM\" \
\      , \"name\":         \"Global Summary of the Month\" \
\      , \"mindate\":      \"1763-01-01\" \
\      , \"maxdate\":      \"2020-01-01\" \
\      , \"datacoverage\": 1 \
\      , \"uid\":          \"gov.noaa.ncdc:C00946\" \
\      } \
\    , { \"id\":           \"GSOY\" \
\      , \"name\":         \"Global Summary of the Year\" \
\      , \"mindate\":      \"1763-01-01\" \
\      , \"maxdate\":      \"2019-01-01\" \
\      , \"datacoverage\": 1 \
\      , \"uid\":          \"gov.noaa.ncdc:C00947\" \
\      } \
\    ] \
\  }"

dataCatagory1 :: DataCatagory
dataCatagory1 =
  DataCatagory
    { dataCatagoryId   = "ANNAGR"
    , dataCatagoryName = "Annual Agricultural"
    }

dataCatagory2 :: DataCatagory
dataCatagory2 =
  DataCatagory
    { dataCatagoryId   = "ANNDD"
    , dataCatagoryName = "Annual Degree Days"
    }

dataCatagory3 :: DataCatagory
dataCatagory3 =
  DataCatagory
    { dataCatagoryId   = "ANNPRCP"
    , dataCatagoryName = "Annual Precipitation"
    }

dataCatagories :: Collection DataCatagory
dataCatagories =
  Collection
    { collectionMetaData =
        MetaData $ ResultSet
          { resultSetLimit  = 5
          , resultSetOffset = 1
          , resultSetCount  = 3
          }
    , collectionResults  =
      [ dataCatagory1
      , dataCatagory2
      , dataCatagory3
      ]
    }

dataCatagory1' :: B.ByteString
dataCatagory1' =
  "{ \"id\":   \"ANNAGR\" \
\  , \"name\": \"Annual Agricultural\" \
\  }"

dataCatagory2' :: B.ByteString
dataCatagory2' =
  "{ \"id\":   \"ANNDD\" \
\  , \"name\": \"Annual Degree Days\" \
\  }"

dataCatagory3' :: B.ByteString
dataCatagory3' =
  "{ \"id\":   \"ANNPRCP\" \
\  , \"name\": \"Annual Precipitation\" \
\  }"

dataCatagories' :: B.ByteString
dataCatagories' =
  "{ \"metadata\": \
\    { \"resultset\": \
\      { \"offset\": 1 \
\      , \"count\":  3 \
\      , \"limit\":  5 \
\      } \
\    } \
\  , \"results\": \
\    [ \
\      { \"id\":   \"ANNAGR\" \
\      , \"name\": \"Annual Agricultural\" \
\      } \
\    , { \"id\":   \"ANNDD\" \
\      , \"name\": \"Annual Degree Days\" \
\      } \
\    , { \"id\":   \"ANNPRCP\" \
\      , \"name\": \"Annual Precipitation\" \
\      } \
\    ] \
\  }"

dataType1 :: DataType
dataType1 =
  DataType
    { dataTypeId           = "ACSH"
    , dataTypeName         = Nothing
    , dataTypeMinDate      = fromJust (iso8601ParseM "1965-01-01")
    , dataTypeMaxDate      = fromJust (iso8601ParseM "2005-12-31")
    , dataTypeDataCoverage = 1
    }

dataType2 :: DataType
dataType2 =
  DataType
    { dataTypeId           = "ALL"
    , dataTypeName         = Nothing
    , dataTypeMinDate      = fromJust (iso8601ParseM "1991-06-05")
    , dataTypeMaxDate      = fromJust (iso8601ParseM "2020-03-08")
    , dataTypeDataCoverage = 0.95
    }

dataType3 :: DataType
dataType3 =
  DataType
    { dataTypeId           = "ANN-DUTR-NORMAL"
    , dataTypeName         = Nothing
    , dataTypeMinDate      = fromJust (iso8601ParseM "2010-01-01")
    , dataTypeMaxDate      = fromJust (iso8601ParseM "2010-01-01")
    , dataTypeDataCoverage = 1
    }

dataTypes :: Collection DataType
dataTypes =
  Collection
    { collectionMetaData =
        MetaData $ ResultSet
          { resultSetLimit  = 5
          , resultSetOffset = 1
          , resultSetCount  = 3
          }
    , collectionResults  =
      [ dataType1 { dataTypeName = Just "Average cloudiness sunrise to sunset from manual observations" }
      , dataType2 { dataTypeName = Just "Base Data" }
      , dataType3 { dataTypeName = Just "Long-term averages of annual diurnal temperature range" }
      ]
    }

dataType1' :: B.ByteString
dataType1' =
  "{ \"id\":           \"ACSH\" \
\  , \"mindate\":      \"1965-01-01\" \
\  , \"maxdate\":      \"2005-12-31\" \
\  , \"datacoverage\": 1 \
\  }"

dataType2' :: B.ByteString
dataType2' =
  "{ \"id\":           \"ALL\" \
\  , \"mindate\":      \"1991-06-05\" \
\  , \"maxdate\":      \"2020-03-08\" \
\  , \"datacoverage\": 0.95 \
\  }"

dataType3' :: B.ByteString
dataType3' =
  "{ \"id\":           \"ANN-DUTR-NORMAL\" \
\  , \"mindate\":      \"2010-01-01\" \
\  , \"maxdate\":      \"2010-01-01\" \
\  , \"datacoverage\": 1 \
\  }"

dataTypes' :: B.ByteString
dataTypes' =
  "{ \"metadata\": \
\    { \"resultset\": \
\      { \"offset\": 1 \
\      , \"count\":  3 \
\      , \"limit\":  5 \
\      } \
\    } \
\  , \"results\": \
\    [ \
\      { \"id\":           \"ACSH\" \
\      , \"name\":         \"Average cloudiness sunrise to sunset from manual observations\" \
\      , \"mindate\":      \"1965-01-01\" \
\      , \"maxdate\":      \"2005-12-31\" \
\      , \"datacoverage\": 1 \
\      } \
\    , { \"id\":           \"ALL\" \
\      , \"name\":         \"Base Data\" \
\      , \"mindate\":      \"1991-06-05\" \
\      , \"maxdate\":      \"2020-03-08\" \
\      , \"datacoverage\": 0.95 \
\      } \
\    , { \"id\":           \"ANN-DUTR-NORMAL\" \
\      , \"name\":         \"Long-term averages of annual diurnal temperature range\" \
\      , \"mindate\":      \"2010-01-01\" \
\      , \"maxdate\":      \"2010-01-01\" \
\      , \"datacoverage\": 1 \
\      } \
\    ] \
\  }"
