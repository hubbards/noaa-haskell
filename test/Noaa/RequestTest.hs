{-# LANGUAGE OverloadedStrings #-}

{-| TODO module documentation -}
module Noaa.RequestTest (tests) where

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

-- NOTE from time package
import Data.Time.Format.ISO8601 (iso8601ParseM)

import Noaa.Request

-- TODO unit test
tests :: Test
tests = undefined

dataSetsParameters :: DataSetsParameters
dataSetsParameters  =
  DataSetsParameters
    { dataSetsParametersDataTypeId = Just "ACMH"
    , dataSetsParametersLocationId = Just "FIPS:37"
    , dataSetsParametersStationId  = Just "COOP:010957"
    , dataSetsParametersStartDate  = iso8601ParseM "1970-10-03"
    , dataSetsParametersEndDate    = iso8601ParseM "2012-09-10"
    , dataSetsParametersSortField  = Just Name
    , dataSetsParametersSortOrder  = Just Desc
    , dataSetsParametersLimit      = Just 42
    , dataSetsParametersOffset     = Just 24
    }

dataCatagoriesParameters :: DataCatagoriesParameters
dataCatagoriesParameters =
  DataCatagoriesParameters
    { dataCatagoriesParametersDataSetId  = Just "GSOM"
    , dataCatagoriesParametersLocationId = Just "FIPS:37"
    , dataCatagoriesParametersStationId  = Just "COOP:010957"
    , dataCatagoriesParametersStartDate  = iso8601ParseM "1970-10-03"
    , dataCatagoriesParametersEndDate    = iso8601ParseM "2012-09-10"
    , dataCatagoriesParametersSortField  = Just Name
    , dataCatagoriesParametersSortOrder  = Just Desc
    , dataCatagoriesParametersLimit      = Just 42
    , dataCatagoriesParametersOffset     = Just 24
    }
