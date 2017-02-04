import Control.Monad
import Test.HUnit
import LatLng.Tests (latlngTests)
import UTMRef.Tests (utmrefTests)
import ECEFRef.Tests (ecefrefTests)

main :: IO ()
main = forM_ [ latlngTests, utmrefTests, ecefrefTests ] runTestTT
