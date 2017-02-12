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
testToOSRef = (toOSRef $ LatLng 52.657570301933 1.7179215806451 0 osgb36Datum) ~?= (OSRef 651409.902802228 313177.26991869917)

testToWGS84 :: Test
testToWGS84 = (toWGS84 $ LatLng 52.657570301933156 1.717921580645096 0 wgs84Datum) ~?= LatLng 52.657975812613955 1.716051773181802 0 wgs84Datum

testToDatum = TestList [
    (toDatum (LatLng 52.657570301933156 1.717921580645096 0 wgs84Datum) osgb36Datum) ~?= LatLng 52.65716468040487 1.7197915435025186 0 wgs84Datum
    , (toDatum (LatLng 52.65716468040487 1.7197915435025186 0 osgb36Datum) wgs84Datum) ~?= LatLng 52.65716468040487 1.7197915435025186 0 osgb36Datum
    , (toDatum (LatLng 52.657570301933156 1.717921580645096 0 wgs84Datum) wgs84Datum) ~?= LatLng 52.657570301933156 1.717921580645096 0 wgs84Datum
    , (toDatum (LatLng 53.364040027777776 (-8.348032805555556) 0 etrf89Datum) ireland1965Datum) ~?= LatLng 53.36292074510271 (-6.348982169358832) 0 etrf89Datum
    ]

testToOSGB36 :: Test
testToOSGB36 = (toOSGB36 $ LatLng 52.657570301933 1.7179215806451 0 osgb36Datum) ~?= LatLng 52.65716471196846 1.7197915435062723 0 osgb36Datum

testDistance :: Test
testDistance = (distance (LatLng 40.718119 (-73.995667) 0 wgs84Datum) (LatLng 51.499981 (-0.125313) 0 wgs84Datum)) ~?= 5565.842734813126

testDistanceMiles :: Test
testDistanceMiles = (distanceMiles (LatLng 40.718119 (-73.995667) 0 wgs84Datum) (LatLng 51.499981 (-0.125313) 0 wgs84Datum)) ~?= 3458.4543359363356

testLatitudeDegrees = TestList [
    (latitudeDegrees $ LatLng 0 0 0 wgs84Datum) ~?= 0
    , (latitudeDegrees $ LatLng 10 0 0 wgs84Datum) ~?= 10
    , (latitudeDegrees $ LatLng (-10) 0 0 wgs84Datum) ~?= -10
    , (latitudeDegrees $ LatLng 10.5 0 0 wgs84Datum) ~?= 10
    , (latitudeDegrees $ LatLng (-10.5) 0 0 wgs84Datum) ~?= -10
    ]


testLatitudeMinutes = TestList [
    (latitudeMinutes $ LatLng 0 0 0 wgs84Datum) ~?= 0
    , (latitudeMinutes $ LatLng 10 0 0 wgs84Datum) ~?= 0
    , (latitudeMinutes $ LatLng (-10) 0 0 wgs84Datum) ~?= 0
    , (latitudeMinutes $ LatLng 10.25 0 0 wgs84Datum) ~?= 15
    , (latitudeMinutes $ LatLng (-10.25) 0 0 wgs84Datum) ~?= 15
    , (latitudeMinutes $ LatLng 10.257 0 0 wgs84Datum) ~?= 15
    , (latitudeMinutes $ LatLng (-10.257) 0 0 wgs84Datum) ~?= 15
    ]

testLatitudeSeconds = TestList [
    (latitudeSeconds $ LatLng 0 0 0 wgs84Datum) ~?= 0
    , (latitudeSeconds $ LatLng 10 0 0 wgs84Datum) ~?= 0
    , (latitudeSeconds $ LatLng (-10) 0 0 wgs84Datum) ~?= 0
    , (latitudeSeconds $ LatLng 10.25 0 0 wgs84Datum) ~?= 0
    , (latitudeSeconds $ LatLng (-10.25) 0 0 wgs84Datum) ~?= 0
    , (latitudeSeconds $ LatLng 10.257 0 0 wgs84Datum) ~?= 25.199999999998823
    , (latitudeSeconds $ LatLng (-10.257) 0 0 wgs84Datum) ~?= 25.199999999998823
    ]
