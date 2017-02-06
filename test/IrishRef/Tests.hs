module IrishRef.Tests where

import Test.HUnit
import HUnitExtensions
import Equals

import Datum
import IrishRef

irishrefTests :: Test
irishrefTests = TestList [
    TestLabel "irishref - to irishref" testToIrishRef
    ]

testToIrishRef = TestList [
--     foo -11 2 `shouldThrow` "Invalid parameter",
    (mkIrishRef' "O099361") `shouldReturn` (IrishRef 309900.0 236100.0 ireland1965Datum)
    , (mkIrishRef' "G099361") `shouldReturn` (IrishRef 109900.0 336100.0 ireland1965Datum)
    ]
