{-# LANGUAGE OverloadedStrings #-}

{-| TODO module documentation
-}
module NoaaTest where

-- NOTE from HUnit package
import Test.HUnit (
    Test
  , test
  , (~:)
  , (~=?)
  , (~?=)
  , (@=?)
  , (@?=)
  , (@?)
  )

-- NOTE from aeson package
import Data.Aeson ( decode )

-- NOTE from bytestring package
import Data.ByteString.Lazy ( ByteString )

-- NOTE from time package
import Data.Time.Calendar ( Day )
import Data.Time.Format.ISO8601 ( iso8601ParseM )

import Data.Maybe ( fromJust )

import Noaa

tests :: Test
tests =
  test
    [ decode dataSet1' ~?= Just dataSet1
    , decode dataSet2' ~?= Just dataSet2
    , decode dataSet3' ~?= Just dataSet3
    , decode dataSets' ~?= Just dataSets
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

dataSet1' :: ByteString
dataSet1' =
  "{ \"uid\":          \"gov.noaa.ncdc:C00861\" \
\  , \"id\":           \"GHCND\" \
\  , \"name\":         \"Daily Summaries\" \
\  , \"mindate\":      \"1763-01-01\" \
\  , \"maxdate\":      \"2020-03-05\" \
\  , \"datacoverage\": 1 \
\  }"

dataSet2' :: ByteString
dataSet2' =
  "{ \"uid\":          \"gov.noaa.ncdc:C00946\" \
\  , \"id\":           \"GSOM\" \
\  , \"name\":         \"Global Summary of the Month\" \
\  , \"mindate\":      \"1763-01-01\" \
\  , \"maxdate\":      \"2020-01-01\" \
\  , \"datacoverage\": 1 \
\  }"

dataSet3' :: ByteString
dataSet3' =
  "{ \"uid\":          \"gov.noaa.ncdc:C00947\" \
\  , \"id\":           \"GSOY\" \
\  , \"name\":         \"Global Summary of the Year\" \
\  , \"mindate\":      \"1763-01-01\" \
\  , \"maxdate\":      \"2019-01-01\" \
\  , \"datacoverage\": 1 \
\  }"

dataSets' :: ByteString
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
