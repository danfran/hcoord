import Control.Monad
import Test.HUnit
import DMS.Tests (dmsTests)
import LatLng.Tests (latlngTests)
import UTMRef.Tests (utmrefTests)
import ECEFRef.Tests (ecefrefTests)
import IrishRef.Tests (irishrefTests)

main :: IO ()
main = forM_ [ dmsTests
             , latlngTests
             , utmrefTests
             , ecefrefTests
             , irishrefTests
             ] runTestTT
