module UTMRef.Tests where

import Test.HUnit
import HUnitExtensions
import Equals

import Datum
import LatLng
import UTMRef

utmrefTests :: Test
utmrefTests = TestList [
    TestLabel "utmref - to utmref" testToUTMRef
    , TestLabel "utmref - to latlng" testToLatLng
    ]

testToUTMRef = TestList [
--     foo -11 2 `shouldThrow` "Invalid parameter",
    (toUTMRef $ LatLng (LatLngPoint 84.0 0.0 0.0) osgb36Datum) `shouldReturn` (UTMRef 465005.344 9329005.18 'X' 31 osgb36Datum)
    , (toUTMRef $ LatLng (LatLngPoint (-80.0) 0.0 0.0) osgb36Datum) `shouldReturn` (UTMRef 441867.784 1116915.043 'C' 31 osgb36Datum)
    , (toUTMRef $ LatLng (LatLngPoint 0.0 (-180.0) 0.0) osgb36Datum) `shouldReturn` (UTMRef 166021.443 0.0 'N' 1 osgb36Datum)
    , (toUTMRef $ LatLng (LatLngPoint 0.0 180.0 0.0) osgb36Datum) `shouldReturn` (UTMRef 166021.443 0.0 'N' 1 osgb36Datum)
    --  TODO Tests for regions around Norway and Svalbard
    ]

testToLatLng :: Test
testToLatLng = (toLatLng $ UTMRef 456463.99 3335334.05 'E' 12 wgs84Datum) ~?= (LatLngPoint (-60.11669998330364) (-111.78330000818927) 0.0)