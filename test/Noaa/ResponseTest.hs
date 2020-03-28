{-# LANGUAGE OverloadedStrings #-}

{-| TODO module documentation -}
module Noaa.ResponseTest (tests) where

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
import Data.Aeson (decode)

-- NOTE from bytestring package
import qualified Data.ByteString.Lazy as B

-- NOTE from time package
import Data.Time.Format.ISO8601 (iso8601ParseM)

import Data.Maybe (fromJust)

import Noaa.Response

tests :: Test
tests =
  test
    [ decode dataSet1'           ~?= Just dataSet1
    , decode dataSet2'           ~?= Just dataSet2
    , decode dataSet3'           ~?= Just dataSet3
    , decode dataSets'           ~?= Just dataSets
    , decode dataCatagory1'      ~?= Just dataCatagory1
    , decode dataCatagory2'      ~?= Just dataCatagory2
    , decode dataCatagory3'      ~?= Just dataCatagory3
    , decode dataCatagories'     ~?= Just dataCatagories
    , decode dataType1'          ~?= Just dataType1
    , decode dataType2'          ~?= Just dataType2
    , decode dataType3'          ~?= Just dataType3
    , decode dataTypes'          ~?= Just dataTypes
    , decode locationCatagory1'  ~?= Just locationCatagory1
    , decode locationCatagory2'  ~?= Just locationCatagory2
    , decode locationCatagory3'  ~?= Just locationCatagory3
    , decode locationCatagories' ~?= Just locationCatagories
    , decode location1'          ~?= Just location1
    , decode location2'          ~?= Just location2
    , decode location3'          ~?= Just location3
    , decode locations'          ~?= Just locations
    , decode station1'           ~?= Just station1
    , decode station2'           ~?= Just station2
    , decode station3'           ~?= Just station3
    , decode stations'           ~?= Just stations
    , decode data1'              ~?= Just data1
    , decode data2'              ~?= Just data2
    , decode data3'              ~?= Just data3
    , decode datas'              ~?= Just datas
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
    { dataTypeId           = "ACMH"
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
      [ dataType1 { dataTypeName = Just "Average cloudiness midnight to midnight from manual observations" }
      , dataType2 { dataTypeName = Just "Base Data" }
      , dataType3 { dataTypeName = Just "Long-term averages of annual diurnal temperature range" }
      ]
    }

dataType1' :: B.ByteString
dataType1' =
  "{ \"id\":           \"ACMH\" \
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
\      { \"id\":           \"ACMH\" \
\      , \"name\":         \"Average cloudiness midnight to midnight from manual observations\" \
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

locationCatagory1 :: LocationCatagory
locationCatagory1 =
  LocationCatagory
    { locationCatagoryId   = "CLIM_REG"
    , locationCatagoryName = "Climate Region"
    }

locationCatagory2 :: LocationCatagory
locationCatagory2 =
  LocationCatagory
    { locationCatagoryId   = "CITY"
    , locationCatagoryName = "City"
    }

locationCatagory3 :: LocationCatagory
locationCatagory3 =
  LocationCatagory
    { locationCatagoryId   = "CLIM_DIV"
    , locationCatagoryName = "Climate Division"
    }

locationCatagories :: Collection LocationCatagory
locationCatagories =
  Collection
    { collectionMetaData =
        MetaData $ ResultSet
          { resultSetLimit  = 5
          , resultSetOffset = 1
          , resultSetCount  = 3
          }
    , collectionResults  =
      [ locationCatagory1
      , locationCatagory2
      , locationCatagory3
      ]
    }

locationCatagory1' :: B.ByteString
locationCatagory1' =
  "{ \"id\":   \"CLIM_REG\" \
\  , \"name\": \"Climate Region\" \
\  }"

locationCatagory2' :: B.ByteString
locationCatagory2' =
  "{ \"id\":   \"CITY\" \
\  , \"name\": \"City\" \
\  }"

locationCatagory3' :: B.ByteString
locationCatagory3' =
  "{ \"id\":   \"CLIM_DIV\" \
\  , \"name\": \"Climate Division\" \
\  }"

locationCatagories' :: B.ByteString
locationCatagories' =
  "{ \"metadata\": \
\    { \"resultset\": \
\      { \"offset\": 1 \
\      , \"count\":  3 \
\      , \"limit\":  5 \
\      } \
\    } \
\  , \"results\": \
\    [ \
\      { \"id\":   \"CLIM_REG\" \
\      , \"name\": \"Climate Region\" \
\      } \
\    , { \"id\":   \"CITY\" \
\      , \"name\": \"City\" \
\      } \
\    , { \"id\":   \"CLIM_DIV\" \
\      , \"name\": \"Climate Division\" \
\      } \
\    ] \
\  }"

location1 :: Location
location1 =
  Location
    { locationId           = "FIPS:37"
    , locationName         = "North Carolina"
    , locationMinDate      = fromJust (iso8601ParseM "1869-03-01")
    , locationMaxDate      = fromJust (iso8601ParseM "2020-03-09")
    , locationDataCoverage = 1
    }

location2 :: Location
location2 =
  Location
    { locationId           = "CITY:AE000001"
    , locationName         = "Abu Dhabi, AE"
    , locationMinDate      = fromJust (iso8601ParseM "1983-01-01")
    , locationMaxDate      = fromJust (iso8601ParseM "2020-03-07")
    , locationDataCoverage = 1
    }

location3 :: Location
location3 =
  Location
    { locationId           = "CITY:AE000003"
    , locationName         = "Dubai, AE"
    , locationMinDate      = fromJust (iso8601ParseM "1944-03-01")
    , locationMaxDate      = fromJust (iso8601ParseM "2020-03-07")
    , locationDataCoverage = 0.9991
    }

locations :: Collection Location
locations =
  Collection
    { collectionMetaData =
        MetaData $ ResultSet
          { resultSetLimit  = 5
          , resultSetOffset = 1
          , resultSetCount  = 3
          }
    , collectionResults  =
      [ location1
      , location2
      , location3
      ]
    }

location1' :: B.ByteString
location1' =
  "{ \"id\":           \"FIPS:37\" \
\  , \"name\":         \"North Carolina\" \
\  , \"mindate\":      \"1869-03-01\" \
\  , \"maxdate\":      \"2020-03-09\" \
\  , \"datacoverage\": 1 \
\  }"

location2' :: B.ByteString
location2' =
  "{ \"id\":           \"CITY:AE000001\" \
\  , \"name\":         \"Abu Dhabi, AE\" \
\  , \"mindate\":      \"1983-01-01\" \
\  , \"maxdate\":      \"2020-03-07\" \
\  , \"datacoverage\": 1 \
\  }"

location3' :: B.ByteString
location3' =
  "{ \"id\":           \"CITY:AE000003\" \
\  , \"name\":         \"Dubai, AE\" \
\  , \"mindate\":      \"1944-03-01\" \
\  , \"maxdate\":      \"2020-03-07\" \
\  , \"datacoverage\": 0.9991 \
\  }"

locations' :: B.ByteString
locations' =
  "{ \"metadata\": \
\    { \"resultset\": \
\      { \"offset\": 1 \
\      , \"count\":  3 \
\      , \"limit\":  5 \
\      } \
\    } \
\  , \"results\": \
\    [ \
\      { \"id\":           \"FIPS:37\" \
\      , \"name\":         \"North Carolina\" \
\      , \"mindate\":      \"1869-03-01\" \
\      , \"maxdate\":      \"2020-03-09\" \
\      , \"datacoverage\": 1 \
\      } \
\    , { \"id\":           \"CITY:AE000001\" \
\      , \"name\":         \"Abu Dhabi, AE\" \
\      , \"mindate\":      \"1983-01-01\" \
\      , \"maxdate\":      \"2020-03-07\" \
\      , \"datacoverage\": 1 \
\      } \
\    , { \"id\":           \"CITY:AE000003\" \
\      , \"name\":         \"Dubai, AE\" \
\      , \"mindate\":      \"1944-03-01\" \
\      , \"maxdate\":      \"2020-03-07\" \
\      , \"datacoverage\": 0.9991 \
\      } \
\    ] \
\  }"

station1 :: Station
station1 =
  Station
    { stationId             = "COOP:010008"
    , stationName           = "ABBEVILLE, AL US"
    , stationMinDate        = fromJust (iso8601ParseM "1948-01-01")
    , stationMaxDate        = fromJust (iso8601ParseM "2014-01-01")
    , stationDataCoverage   = 0.8813
    , stationElevation      = 139
    , stationElevationUnits = "METERS"
    , stationLatitude       = 31.5702
    , stationLongitude      = -85.2482
    }

station2 :: Station
station2 =
  Station
    { stationId             = "COOP:010063"
    , stationName           = "ADDISON, AL US"
    , stationMinDate        = fromJust (iso8601ParseM "1938-01-01")
    , stationMaxDate        = fromJust (iso8601ParseM "2015-11-01")
    , stationDataCoverage   = 0.5059
    , stationElevation      = 249.3
    , stationElevationUnits = "METERS"
    , stationLatitude       = 34.2553
    , stationLongitude      = -87.1814
    }

station3 :: Station
station3 =
  Station
    { stationId             = "COOP:310301"
    , stationName           = "ASHEVILLE, NC US"
    , stationMinDate        = fromJust (iso8601ParseM "1902-08-01")
    , stationMaxDate        = fromJust (iso8601ParseM "2015-11-01")
    , stationDataCoverage   = 1
    , stationElevation      = 682.1
    , stationElevationUnits = "METERS"
    , stationLatitude       = 35.5954
    , stationLongitude      = -82.5568
    }

stations :: Collection Station
stations =
  Collection
    { collectionMetaData =
        MetaData $ ResultSet
          { resultSetLimit  = 5
          , resultSetOffset = 1
          , resultSetCount  = 3
          }
    , collectionResults  =
      [ station1
      , station2
      , station3
      ]
    }

station1' :: B.ByteString
station1' =
  "{ \"id\":            \"COOP:010008\" \
\  , \"name\":          \"ABBEVILLE, AL US\" \
\  , \"mindate\":       \"1948-01-01\" \
\  , \"maxdate\":       \"2014-01-01\" \
\  , \"datacoverage\":  0.8813 \
\  , \"elevation\":     139 \
\  , \"elevationUnit\": \"METERS\" \
\  , \"latitude\":      31.5702 \
\  , \"longitude\":     -85.2482 \
\ }"

station2' :: B.ByteString
station2' =
  "{ \"id\":            \"COOP:010063\" \
\  , \"name\":          \"ADDISON, AL US\" \
\  , \"mindate\":       \"1938-01-01\" \
\  , \"maxdate\":       \"2015-11-01\" \
\  , \"datacoverage\":  0.5059 \
\  , \"elevation\":     249.3 \
\  , \"elevationUnit\": \"METERS\" \
\  , \"latitude\":      34.2553 \
\  , \"longitude\":     -87.1814 \
\ }"

station3' :: B.ByteString
station3' =
  "{ \"id\":            \"COOP:310301\" \
\  , \"name\":          \"ASHEVILLE, NC US\" \
\  , \"mindate\":       \"1902-08-01\" \
\  , \"maxdate\":       \"2015-11-01\" \
\  , \"datacoverage\":  1 \
\  , \"elevation\":     682.1 \
\  , \"elevationUnit\": \"METERS\" \
\  , \"latitude\":      35.5954 \
\  , \"longitude\":     -82.5568 \
\ }"

stations' :: B.ByteString
stations' =
  "{ \"metadata\": \
\    { \"resultset\": \
\      { \"offset\": 1 \
\      , \"count\":  3 \
\      , \"limit\":  5 \
\      } \
\    } \
\  , \"results\": \
\    [ \
\      { \"id\":            \"COOP:010008\" \
\      , \"name\":          \"ABBEVILLE, AL US\" \
\      , \"mindate\":       \"1948-01-01\" \
\      , \"maxdate\":       \"2014-01-01\" \
\      , \"datacoverage\":  0.8813 \
\      , \"elevation\":     139 \
\      , \"elevationUnit\": \"METERS\" \
\      , \"latitude\":      31.5702 \
\      , \"longitude\":     -85.2482 \
\      } \
\    , { \"id\":            \"COOP:010063\" \
\      , \"name\":          \"ADDISON, AL US\" \
\      , \"mindate\":       \"1938-01-01\" \
\      , \"maxdate\":       \"2015-11-01\" \
\      , \"datacoverage\":  0.5059 \
\      , \"elevation\":     249.3 \
\      , \"elevationUnit\": \"METERS\" \
\      , \"latitude\":      34.2553 \
\      , \"longitude\":     -87.1814 \
\      } \
\    , { \"id\":            \"COOP:310301\" \
\      , \"name\":          \"ASHEVILLE, NC US\" \
\      , \"mindate\":       \"1902-08-01\" \
\      , \"maxdate\":       \"2015-11-01\" \
\      , \"datacoverage\":  1 \
\      , \"elevation\":     682.1 \
\      , \"elevationUnit\": \"METERS\" \
\      , \"latitude\":      35.5954 \
\      , \"longitude\":     -82.5568 \
\      } \
\    ] \
\  }"

data1 :: Data
data1 =
  Data
    { dataDate       = fromJust (iso8601ParseM "2010-05-01T00:00:00")
    , dataDataType   = "TMAX"
    , dataStation    = "GHCND:USW00013872"
    , dataAttributes = ",,0,2400"
    , dataValue      = 267
    }

data2 :: Data
data2 =
  Data
    { dataDate       = fromJust (iso8601ParseM "2010-05-01T00:00:00")
    , dataDataType   = "TMIN"
    , dataStation    = "GHCND:USW00013872"
    , dataAttributes = ",,0,2400"
    , dataValue      = 139
    }

data3 :: Data
data3 =
  Data
    { dataDate       = fromJust (iso8601ParseM "2010-05-01T00:00:00")
    , dataDataType   = "TOBS"
    , dataStation    = "GHCND:USW00013872"
    , dataAttributes = ",,0,2400"
    , dataValue      = 206
    }

datas :: Collection Data
datas =
  Collection
    { collectionMetaData =
        MetaData $ ResultSet
          { resultSetLimit  = 5
          , resultSetOffset = 1
          , resultSetCount  = 3
          }
    , collectionResults  =
      [ data1
      , data2
      , data3
      ]
    }

data1' :: B.ByteString
data1' =
  "{ \"date\":       \"2010-05-01T00:00:00\" \
\  , \"datatype\":   \"TMAX\" \
\  , \"station\":    \"GHCND:USW00013872\" \
\  , \"attributes\": \",,0,2400\" \
\  , \"value\":      267 \
\ }"

data2' :: B.ByteString
data2' =
  "{ \"date\":       \"2010-05-01T00:00:00\" \
\  , \"datatype\":   \"TMIN\" \
\  , \"station\":    \"GHCND:USW00013872\" \
\  , \"attributes\": \",,0,2400\" \
\  , \"value\":      139 \
\ }"

data3' :: B.ByteString
data3' =
  "{ \"date\":       \"2010-05-01T00:00:00\" \
\  , \"datatype\":   \"TOBS\" \
\  , \"station\":    \"GHCND:USW00013872\" \
\  , \"attributes\": \",,0,2400\" \
\  , \"value\":      206 \
\ }"

datas' :: B.ByteString
datas' =
  "{ \"metadata\": \
\    { \"resultset\": \
\      { \"offset\": 1 \
\      , \"count\":  3 \
\      , \"limit\":  5 \
\      } \
\    } \
\  , \"results\": \
\    [ \
\      { \"date\":       \"2010-05-01T00:00:00\" \
\      , \"datatype\":   \"TMAX\" \
\      , \"station\":    \"GHCND:USW00013872\" \
\      , \"attributes\": \",,0,2400\" \
\      , \"value\":      267 \
\      } \
\    , { \"date\":       \"2010-05-01T00:00:00\" \
\      , \"datatype\":   \"TMIN\" \
\      , \"station\":    \"GHCND:USW00013872\" \
\      , \"attributes\": \",,0,2400\" \
\      , \"value\":      139 \
\      } \
\    , { \"date\":       \"2010-05-01T00:00:00\" \
\      , \"datatype\":   \"TOBS\" \
\      , \"station\":    \"GHCND:USW00013872\" \
\      , \"attributes\": \",,0,2400\" \
\      , \"value\":      206 \
\      } \
\    ] \
\  }"
