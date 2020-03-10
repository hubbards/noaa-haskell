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
    ]

dataSet1 :: DataSet
dataSet1 =
  DataSet
    { dataSetUid          = Just "gov.noaa.ncdc:C00861"
    , dataSetId           = "GHCND"
    , dataSetName         = "Daily Summaries"
    , dataSetMinDate      = fromJust (iso8601ParseM "1763-01-01")
    , dataSetMaxDate      = fromJust (iso8601ParseM "2020-03-05")
    , dataSetDataCoverage = 1
    }

dataSet2 :: DataSet
dataSet2 =
  DataSet
    { dataSetUid          = Just "gov.noaa.ncdc:C00946"
    , dataSetId           = "GSOM"
    , dataSetName         = "Global Summary of the Month"
    , dataSetMinDate      = fromJust (iso8601ParseM "1763-01-01")
    , dataSetMaxDate      = fromJust (iso8601ParseM "2020-01-01")
    , dataSetDataCoverage = 1
    }

dataSet3 :: DataSet
dataSet3 =
  DataSet
    { dataSetUid          = Just "gov.noaa.ncdc:C00947"
    , dataSetId           = "GSOY"
    , dataSetName         = "Global Summary of the Year"
    , dataSetMinDate      = fromJust (iso8601ParseM "1763-01-01")
    , dataSetMaxDate      = fromJust (iso8601ParseM "2019-01-01")
    , dataSetDataCoverage = 1
    }

dataSets :: Collection DataSet
dataSets =
  Collection
    { collectionMetaData = MetaData $ ResultSet
      { resultSetLimit  = 5
      , resultSetOffset = 1
      , resultSetCount  = 3
      }
    , collectionResults  =
      [ dataSet1
      , dataSet2
      , dataSet3 ]
    }

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
    { collectionMetaData = MetaData $ ResultSet
      { resultSetLimit  = 5
      , resultSetOffset = 1
      , resultSetCount  = 3
      }
    , collectionResults  =
      [ dataCatagory1
      , dataCatagory2
      , dataCatagory3 ]
    }

dataSet1' :: B.ByteString
dataSet1' =
  "{ \"uid\":          \"gov.noaa.ncdc:C00861\" \
\  , \"id\":           \"GHCND\" \
\  , \"name\":         \"Daily Summaries\" \
\  , \"mindate\":      \"1763-01-01\" \
\  , \"maxdate\":      \"2020-03-05\" \
\  , \"datacoverage\": 1 \
\  }"

dataSet2' :: B.ByteString
dataSet2' =
  "{ \"uid\":          \"gov.noaa.ncdc:C00946\" \
\  , \"id\":           \"GSOM\" \
\  , \"name\":         \"Global Summary of the Month\" \
\  , \"mindate\":      \"1763-01-01\" \
\  , \"maxdate\":      \"2020-01-01\" \
\  , \"datacoverage\": 1 \
\  }"

dataSet3' :: B.ByteString
dataSet3' =
  "{ \"uid\":          \"gov.noaa.ncdc:C00947\" \
\  , \"id\":           \"GSOY\" \
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
\      { \"uid\":          \"gov.noaa.ncdc:C00861\" \
\      , \"id\":           \"GHCND\" \
\      , \"name\":         \"Daily Summaries\" \
\      , \"mindate\":      \"1763-01-01\" \
\      , \"maxdate\":      \"2020-03-05\" \
\      , \"datacoverage\": 1 \
\      } \
\    , { \"uid\":          \"gov.noaa.ncdc:C00946\" \
\      , \"id\":           \"GSOM\" \
\      , \"name\":         \"Global Summary of the Month\" \
\      , \"mindate\":      \"1763-01-01\" \
\      , \"maxdate\":      \"2020-01-01\" \
\      , \"datacoverage\": 1 \
\      } \
\    , { \"uid\":          \"gov.noaa.ncdc:C00947\" \
\      , \"id\":           \"GSOY\" \
\      , \"name\":         \"Global Summary of the Year\" \
\      , \"mindate\":      \"1763-01-01\" \
\      , \"maxdate\":      \"2019-01-01\" \
\      , \"datacoverage\": 1 \
\      } \
\    ] \
\  }"

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
