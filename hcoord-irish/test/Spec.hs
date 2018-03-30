import Control.Monad
import Test.HUnit
import IrishRef.Tests (irishrefTests)

main :: IO ()
main = forM_ [irishrefTests] runTestTT
