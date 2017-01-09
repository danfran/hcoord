module LatLng.Tests where

import Test.HUnit (Assertion, assertEqual)
import Test.Framework (Test, mutuallyExclusive, testGroup)
import Test.Framework.Providers.HUnit (testCase)

import Datum
import LatLng
import OSRef

latLngTests :: Test
latLngTests = mutuallyExclusive $ testGroup "LatLng Conversion"
        [ testToOSRef
        ]


type String = [Char]

testToOSRef :: Test
testToOSRef = testCase "Should Convert Latitude/Longitude to OS Grid Reference Using OSGB36" testConvert
  where
    testConvert :: Assertion
    testConvert = do
        let llp = LatLngPoint { latitude = 52.657570301933
                              , longitude = 1.7179215806451
                              , height = 0
                              }

            ll = LatLng llp osgb36Datum


            expected = OSRef { easting = 651409.902802228
                             , northing = 313177.26991869917
                             }

        assertEqual "osref" expected (to ll)
