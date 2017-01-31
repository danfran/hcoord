import Control.Monad
import Test.HUnit
import LatLng.Tests (latlngTests)
import UTMRef.Tests (utmrefTests)

main :: IO ()
main = forM_ [ latlngTests, utmrefTests ] runTestTT
