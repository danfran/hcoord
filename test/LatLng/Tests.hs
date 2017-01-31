module LatLng.Tests where

import Test.HUnit
import HUnitExtensions
import Equals

import Datum
import LatLng
import OSRef

latlngTests :: Test
latlngTests = TestList [
    TestLabel "osref" testToOSRef
    , TestLabel "wgs84" testToWGS84
    , TestLabel "datum" testToDatum
    , TestLabel "osgb36" testToOSGB36
    , TestLabel "distance" testDistance
    , TestLabel "distance miles" testDistanceMiles
    , TestLabel "latitude degrees" testLatitudeDegrees
    , TestLabel "latitude minutes" testLatitudeMinutes
    , TestLabel "latitude seconds" testLatitudeSeconds
    ]

testToOSRef :: Test
testToOSRef = (toOSRef $ LatLng (LatLngPoint 52.657570301933 1.7179215806451 0) osgb36Datum) ~?= (OSRef 651409.902802228 313177.26991869917)

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


testLatitudeMinutes = TestList [
    (latitudeMinutes $ LatLngPoint 0 0 0) ~?= 0
    , (latitudeMinutes $ LatLngPoint 10 0 0) ~?= 0
    , (latitudeMinutes $ LatLngPoint (-10) 0 0) ~?= 0
    , (latitudeMinutes $ LatLngPoint 10.25 0 0) ~?= 15
    , (latitudeMinutes $ LatLngPoint (-10.25) 0 0) ~?= 15
    , (latitudeMinutes $ LatLngPoint 10.257 0 0) ~?= 15
    , (latitudeMinutes $ LatLngPoint (-10.257) 0 0) ~?= 15
    ]

testLatitudeSeconds = TestList [
    (latitudeSeconds $ LatLngPoint 0 0 0) ~?= 0
    , (latitudeSeconds $ LatLngPoint 10 0 0) ~?= 0
    , (latitudeSeconds $ LatLngPoint (-10) 0 0) ~?= 0
    , (latitudeSeconds $ LatLngPoint 10.25 0 0) ~?= 0
    , (latitudeSeconds $ LatLngPoint (-10.25) 0 0) ~?= 0
    , (latitudeSeconds $ LatLngPoint 10.257 0 0) ~?= 25.199999999998823
    , (latitudeSeconds $ LatLngPoint (-10.257) 0 0) ~?= 25.199999999998823
    ]
