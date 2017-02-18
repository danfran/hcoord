module IrishRef.Tests where

import Control.Monad.Except
import Test.HUnit
import HUnitExtensions
import Equals

import Datum
import qualified DMS
import IrishRef
import qualified LatLng

irishrefTests = TestList [
    TestLabel "irishref - to irishref'" testToIrishRef'
    , TestLabel "irishref - to irishref''" testToIrishRef''
    , TestLabel "irishref - to latlng''" testToLatLng
    , TestLabel "irishref - to sixfigure''" testToSixFigureString
    ]

testToIrishRef' = TestList [
--     foo -11 2 `shouldThrow` "Invalid parameter",
    (mkIrishRef' "O099361") `shouldReturn` (IrishRef 309900.0 236100.0 ireland1965Datum)
    , (mkIrishRef' "G099361") `shouldReturn` (IrishRef 109900.0 336100.0 ireland1965Datum)
    ]

testToIrishRef'' :: Test
testToIrishRef'' = do
  let ll = DMS.toLatLng (DMS.North (DMS.DMS 53 21 50.5441)) (DMS.West (DMS.DMS 6 20 52.9181)) etrf89Datum
  let d = LatLng.toDatum (extract ll) ireland1965Datum
  (mkIrishRef'' d) `shouldReturn` (IrishRef 309897.9584798501 236015.92470629397 ireland1965Datum)

testToLatLng :: Test
testToLatLng = do
  let ll = toLatLng (extract $ mkIrishRef 309958.26 236141.93)
  TestList [
    ll `shouldReturn` LatLng.LatLng 53.364040043415734 (-6.3480328719024754) 0 wgs84Datum
    , LatLng.toDatum (extract ll) etrf89Datum ~?= LatLng.LatLng 53.3640400556 (-6.34803286111) 0 wgs84Datum
    ]

testToSixFigureString = TestList [
  (toSixFigureString $ extract $ mkIrishRef 309958.26 236141.93) ~?= "O099361"
  , (toSixFigureString $ extract $ mkIrishRef 109958.26 336141.93) ~?= "G099361"
  ]
