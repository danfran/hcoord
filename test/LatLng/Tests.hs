module LatLng.Tests where

import Test.HUnit
import HUnitExtensions
import LatLng.Equals

import Datum
import LatLng
import OSRef
import qualified UTMRef

latLngTests :: Test
latLngTests = TestList [
    TestLabel "osref" testToOSRef
    , TestLabel "utmref" testToUTMRef
    , TestLabel "wgs84" testToWGS84
    , TestLabel "datum" testToDatum
    , TestLabel "osgb36" testToOSGB36
    , TestLabel "distance" testDistance
    , TestLabel "distance miles" testDistanceMiles
    , TestLabel "latitude degrees" testLatitudeDegrees
    ]


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

testToWGS84 :: Test
testToWGS84 = (toWGS84 $ LatLng (LatLngPoint 52.657570301933156 1.717921580645096 0) wgs84Datum) ~?= LatLng (LatLngPoint 52.657975812613955 1.716051773181802 0) wgs84Datum

testToDatum = TestList [
    (toDatum (LatLng (LatLngPoint 52.657570301933156 1.717921580645096 0) wgs84Datum) osgb36Datum) ~?= LatLng (LatLngPoint 52.65716468040487 1.7197915435025186 0) wgs84Datum
    , (toDatum (LatLng (LatLngPoint 52.65716468040487 1.7197915435025186 0) osgb36Datum) wgs84Datum) ~?= LatLng (LatLngPoint 52.65716468040487 1.7197915435025186 0) osgb36Datum
    , (toDatum (LatLng (LatLngPoint 52.657570301933156 1.717921580645096 0) wgs84Datum) wgs84Datum) ~?= LatLng (LatLngPoint 52.657570301933156 1.717921580645096 0) wgs84Datum
    ]

testToOSGB36 :: Test
testToOSGB36 = (toOSGB36 $ LatLng (LatLngPoint 52.657570301933 1.7179215806451 0) osgb36Datum) ~?= LatLng (LatLngPoint 52.65716471196846 1.7197915435062723 0) osgb36Datum

testDistance :: Test
testDistance = (distance (LatLngPoint 40.718119 (-73.995667) 0) (LatLngPoint 51.499981 (-0.125313) 0)) ~?= 5565.842734813126

testDistanceMiles :: Test
testDistanceMiles = (distanceMiles (LatLngPoint 40.718119 (-73.995667) 0) (LatLngPoint 51.499981 (-0.125313) 0)) ~?= 3458.4543359363356

testLatitudeDegrees = TestList [
    (latitudeDegrees $ LatLngPoint 0 0 0) ~?= 0
    , (latitudeDegrees $ LatLngPoint 10 0 0) ~?= 10
    , (latitudeDegrees $ LatLngPoint (-10) 0 0) ~?= -10
    , (latitudeDegrees $ LatLngPoint 10.5 0 0) ~?= 10
    , (latitudeDegrees $ LatLngPoint (-10.5) 0 0) ~?= -10
    ]
