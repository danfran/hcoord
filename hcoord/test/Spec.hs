import Control.Monad
import Test.HUnit
import DMS.Tests (dmsTests)
import ECEFRef.Tests (ecefrefTests)
import LatLng.Tests (latlngTests)

main :: IO ()
main = forM_ [ dmsTests
             , ecefrefTests
             , latlngTests
             ] runTestTT
