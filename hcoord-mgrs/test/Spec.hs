import Control.Monad
import Test.HUnit
import MGRSRef.Tests (mgrsrefTests)

main :: IO ()
main = forM_ [mgrsrefTests] runTestTT
