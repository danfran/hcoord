module ECEFRef.Tests where

import Test.HUnit
import HUnitExtensions
import Equals

import Datum
import LatLng
import ECEFRef

ecefrefTests :: Test
ecefrefTests = TestList [
    TestLabel "ecefref - to ecefref" testToECEFRef
    , TestLabel "ecefref - to latlng" testToLatLng
    ]

testToECEFRef :: Test
testToECEFRef = (toECEFRef $ LatLng 52.65716468040487 1.7197915435025186 0.0 wgs84Datum) ~?= (ECEFRef 3875333.7837 116357.0618 5047492.1819 wgs84Datum)

testToLatLng :: Test
testToLatLng = (toLatLng $ ECEFRef 3875333.7837 116357.0618 5047492.1819 wgs84Datum) ~?= (LatLng 52.65716468040487 1.7197915435025186 (-0.0007472401484847069) wgs84Datum)
