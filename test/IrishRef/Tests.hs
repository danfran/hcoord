module IrishRef.Tests where

import Control.Monad.Except
import Test.HUnit
import HUnitExtensions
import Equals

import Datum
import DMS
import IrishRef
import LatLng

irishrefTests = TestList [
    TestLabel "irishref - to irishref'" testToIrishRef'
    , TestLabel "irishref - to irishref''" testToIrishRef''
    ]

testToIrishRef' = TestList [
--     foo -11 2 `shouldThrow` "Invalid parameter",
    (mkIrishRef' "O099361") `shouldReturn` (IrishRef 309900.0 236100.0 ireland1965Datum)
    , (mkIrishRef' "G099361") `shouldReturn` (IrishRef 109900.0 336100.0 ireland1965Datum)
    ]

testToIrishRef'' :: Test
testToIrishRef'' = do
  let ll = runExcept $ toLatLng (North (DMS 53 21 50.5441)) (West (DMS 6 20 52.9181)) etrf89Datum
  let ll2 = toDatum (either (const $ LatLng 0 0 0 ireland1965Datum) id ll) ireland1965Datum
  (mkIrishRef'' ll2) `shouldReturn` (IrishRef 309897.9584798501 236015.92470629397 ireland1965Datum)