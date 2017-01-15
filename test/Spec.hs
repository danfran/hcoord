import Test.HUnit
import LatLng.Tests (latLngTests)

main :: IO Counts
main = runTestTT latLngTests
