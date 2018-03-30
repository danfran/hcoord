import Control.Monad
import Test.HUnit
import UTMRef.Tests (utmrefTests)

main :: IO ()
main = forM_ [utmrefTests] runTestTT
