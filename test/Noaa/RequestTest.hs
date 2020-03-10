{-# LANGUAGE OverloadedStrings #-}

{-| TODO module documentation -}
module Noaa.RequestTest ( tests ) where

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
import Data.Time.Format.ISO8601 ( iso8601ParseM )

import Noaa.Request

-- TODO test dataSetsParams
-- TODO test dataCatagoriesParams
tests :: Test
tests = undefined

dataSetsParams :: DataSetsParams
dataSetsParams  =
  DataSetsParams
    { dataSetsParamsDataTypeId = Just "ACMH"
    , dataSetsParamsLocationId = Just "FIPS:37"
    , dataSetsParamsStationId  = Just "COOP:010957"
    , dataSetsParamsStartDate  = iso8601ParseM "1970-10-03"
    , dataSetsParamsEndDate    = iso8601ParseM "2012-09-10"
    , dataSetsParamsSortField  = Just Name
    , dataSetsParamsSortOrder  = Just Desc
    , dataSetsParamsLimit      = Just 42
    , dataSetsParamsOffset     = Just 24
    }

dataCatagoriesParams :: DataCatagoriesParams
dataCatagoriesParams =
  DataCatagoriesParams
    { dataCatagoriesParamsDataSetId  = Just "GSOM"
    , dataCatagoriesParamsLocationId = Just "FIPS:37"
    , dataCatagoriesParamsStationId  = Just "COOP:010957"
    , dataCatagoriesParamsStartDate  = iso8601ParseM "1970-10-03"
    , dataCatagoriesParamsEndDate    = iso8601ParseM "2012-09-10"
    , dataCatagoriesParamsSortField  = Just Name
    , dataCatagoriesParamsSortOrder  = Just Desc
    , dataCatagoriesParamsLimit      = Just 42
    , dataCatagoriesParamsOffset     = Just 24
    }
