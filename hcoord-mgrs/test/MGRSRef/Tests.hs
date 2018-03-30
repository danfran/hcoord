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
    , TestLabel "mgrsref - to mkMGRSRef" testToMkMGRSRef
    , TestLabel "mgrsref - to mkMGRSRef'" testToMkMGRSRef'
    , TestLabel "mgrsref - to UTMRef" testToUTMRef
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


testToMkMGRSRef = TestList [
    (show $ extract $ mkMGRSRef 0 16300 'E' 'U' 10 'U' M1 False) ~?= "10UEU0000016300"
    , (show $ extract $ mkMGRSRef 43575 49756 'D' 'D' 13 'S' M1 False) ~?= "13SDD4357549756"
    ]


testToUTMRef = TestList [
    (MGRSRef.toUTMRef $ extract $ mkMGRSRef' "13SDD4357649756") `shouldReturn` (UTMRef 443576.0 4349756.0 'S' 13 wgs84Datum)
    , (MGRSRef.toUTMRef $ extract $ mkMGRSRef' "32UMU1078") `shouldReturn` (UTMRef 410000.0 5378000.0 'U' 32 wgs84Datum)
    , (MGRSRef.toUTMRef $ MGRSRef 0 16300 'E' 'U' 10 'U' M1 False wgs84Datum) `shouldReturn` (UTMRef 500000.0 5316300.0 'U' 10 wgs84Datum)
    , (MGRSRef.toUTMRef $ MGRSRef 43575 49756 'D' 'D' 13 'S' M1 False wgs84Datum) `shouldReturn` (UTMRef 443575.0 4349756.0 'S' 13 wgs84Datum)
    ]


testToMkMGRSRef' = TestList [
    (show $ extract $ mkMGRSRef' "32UMU1078") ~?= "32UMU1078"
    , (showWithPrecision (extract $ mkMGRSRef' "32UMU1078") M1) ~?= "32UMU1000078000"
    , (showWithPrecision (extract $ mkMGRSRef' "32UMU1078") M10) ~?= "32UMU10007800"
    , (showWithPrecision (extract $ mkMGRSRef' "32UMU1078") M100) ~?= "32UMU100780"
    , (showWithPrecision (extract $ mkMGRSRef' "32UMU1078") M1000) ~?= "32UMU1078"
    , (showWithPrecision (extract $ mkMGRSRef' "32UMU1078") M10000) ~?= "32UMU17"
    ]