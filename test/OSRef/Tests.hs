module OSRef.Tests where

import Test.HUnit
import HUnitExtensions
import Equals

import Datum
import LatLng
import OSRef

osrefTests :: Test
osrefTests = TestList [
    TestLabel "osref - mk osref" testMkOSRef
    , TestLabel "osref - mk osref'" testMkOSRef'
    , TestLabel "osref - to osref" testToOSRef
    , TestLabel "osref - to six digits" testGetOsRefWithSixDigitsPrecision
    , TestLabel "osref - to latlng" testToLatLng
    ]

testMkOSRef = TestList [
--     foo -11 2 `shouldThrow` "Invalid parameter",
    (mkOSRef 651409.903 313177.27) `shouldReturn` (OSRef 651409.903 313177.27 osgb36Datum)
    ]

testToOSRef :: Test
testToOSRef = (toOSRef $ LatLng 52.657570301933 1.7179215806451 0 osgb36Datum) `shouldReturn` (OSRef 651409.902802228 313177.26991869917 osgb36Datum)

testMkOSRef' :: Test
testMkOSRef' = (mkOSRef' "TG514131") `shouldReturn` (OSRef 651400.0 313100.0 osgb36Datum)

testGetOsRefWithSixDigitsPrecision = TestList [
    (getOsRefWithPrecisionOf SixDigits (OSRef 651409.903 313177.270 osgb36Datum)) ~?= "TG514131"
    , (getOsRefWithPrecisionOf SixDigits (OSRef 651409.902802228 312769.3780136908 osgb36Datum)) ~?= "TG514127"
    ]

testToLatLng = TestList [
    (toLatLng (OSRef 651409.903 313177.270 osgb36Datum))  `shouldReturn` (LatLng 52.657570301933156 1.717921580645096 0 wgs84Datum)
    ]