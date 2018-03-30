import Control.Monad
import Test.HUnit
import OSRef.Tests (osrefTests)

main :: IO ()
main = forM_ [osrefTests] runTestTT
