module DMS.Tests where

import Control.Monad.Except
import Test.HUnit
import HUnitExtensions
import Equals

import Datum
import DMS
import LatLng

dmsTests = TestList [
    TestLabel "dms - to latlng'" testToLatLng
    ]

testToLatLng :: Test
testToLatLng = do
  (toLatLng (North (DMS 53 21 50.5441)) (West (DMS 6 20 52.9181)) etrf89Datum) `shouldReturn` (LatLng 53.364040027777776 (-6.3480328055555555) 0 etrf89Datum)