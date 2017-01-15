module LatLng.Tests where

import Test.HUnit
import HUnitExtensions
import LatLng.Equals

import Datum
import LatLng
import OSRef
import qualified UTMRef

latLngTests :: Test
latLngTests = TestList [TestLabel "osref" testToOSRef, TestLabel "utmref" testToUTMRef]


testToOSRef :: Test
testToOSRef = (toOSRef $ LatLng (LatLngPoint 52.657570301933 1.7179215806451 0) osgb36Datum) ~?= (OSRef 651409.902802228 313177.26991869917)

testToUTMRef = TestList [
--     foo -11 2 `shouldThrow` "Invalid parameter",
    (toUTMRef $ LatLng (LatLngPoint 84.0 0.0 0.0) osgb36Datum) `shouldReturn` (UTMRef.UTMRef 465005.344 9329005.18 'X' 31)
    , (toUTMRef $ LatLng (LatLngPoint (-80.0) 0.0 0.0) osgb36Datum) `shouldReturn` (UTMRef.UTMRef 441867.784 1116915.043 'C' 31)
    , (toUTMRef $ LatLng (LatLngPoint 0.0 (-180.0) 0.0) osgb36Datum) `shouldReturn` (UTMRef.UTMRef 166021.443 0.0 'N' 1)
    , (toUTMRef $ LatLng (LatLngPoint 0.0 180.0 0.0) osgb36Datum) `shouldReturn` (UTMRef.UTMRef 166021.443 0.0 'N' 1)
    --  TODO Tests for regions around Norway and Svalbard
    ]