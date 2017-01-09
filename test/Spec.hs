import Test.Framework (defaultMain)
import LatLng.Tests (latLngTests)

main :: IO ()
main = defaultMain
  [ latLngTests
  ]
