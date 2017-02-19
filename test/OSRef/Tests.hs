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
    , TestLabel "osref - to osref" testToOSRef
    ]

testMkOSRef = TestList [
--     foo -11 2 `shouldThrow` "Invalid parameter",
    (mkOSRef 651409.903 313177.27) `shouldReturn` (OSRef 651409.903 313177.27 osgb36Datum)
    ]

testToOSRef :: Test
testToOSRef = (toOSRef $ LatLng 52.657570301933 1.7179215806451 0 osgb36Datum) `shouldReturn` (OSRef 651409.902802228 313177.26991869917 osgb36Datum)