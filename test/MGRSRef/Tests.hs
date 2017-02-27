module MGRSRef.Tests where

import Test.HUnit
import HUnitExtensions
import Equals

import Datum
import MGRSRef
import UTMRef

mgrsrefTests = TestList [
    TestLabel "mgrsref - to show with precision" testToShowWithPrecision
    , TestLabel "mgrsref - to show" testToMGRSRef
    ]


testToShowWithPrecision = TestList [
    (showWithPrecision $ MGRSRef 10000 78000 'M' 'U' 32 'U' M1000 False wgs84Datum) M1 ~?= "32UMU1000078000"
    ]

testToMGRSRef = TestList [
    (show $ toMGRSRef (extract $ mkUTMRef 443575.71 4349755.98 'S' 13) False) ~?= "13SDD4357649756"
    , (show $ toMGRSRef (extract $ mkUTMRef 500000.0 0.0 'N' 1) False) ~?= "01NEA0000000000"
    , (show $ toMGRSRef (extract $ mkUTMRef 500000.0 0.0 'N' 1) True) ~?= "01NEL0000000000"
    , (show $ toMGRSRef (extract $ mkUTMRef 500000.0 0.0 'N' 2) False) ~?= "02NNF0000000000"
    , (show $ toMGRSRef (extract $ mkUTMRef 500000.0 0.0 'N' 2) True) ~?= "02NNR0000000000"
    , (show $ toMGRSRef (extract $ mkUTMRef 500000.0 0.0 'N' 3) False) ~?= "03NWA0000000000"
    , (show $ toMGRSRef (extract $ mkUTMRef 500000.0 0.0 'N' 3) True) ~?= "03NWL0000000000"
    , (show $ toMGRSRef (extract $ mkUTMRef 500000.0 1999999.0 'Q' 1) False) ~?= "01QEV0000099999"
    , (show $ toMGRSRef (extract $ mkUTMRef 500000.0 1999999.0 'Q' 1) True) ~?= "01QEK0000099999"
    , (show $ toMGRSRef (extract $ mkUTMRef 800000.0 0.0 'N' 1) False) ~?= "01NHA0000000000"
    , (show $ toMGRSRef (extract $ mkUTMRef 800000.0 0.0 'N' 1) True) ~?= "01NHL0000000000"
    , (show $ toMGRSRef (extract $ mkUTMRef 199999.0 0.0 'N' 2) False) ~?= "02NJF9999900000"
    , (show $ toMGRSRef (extract $ mkUTMRef 199999.0 0.0 'N' 2) True) ~?= "02NJR9999900000"
    ]