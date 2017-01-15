module HUnitExtensions where
import Test.HUnit
import Control.Monad (unless)
import Control.Monad.Except

shouldThrow :: (Eq a, Eq e, Show a, Show e) => Except e a -> e -> Test
m `shouldThrow` e = runExcept m ~?= Left e

shouldReturn :: (Eq a, Eq e, Show a, Show e) => Except e a -> a -> Test
m `shouldReturn` x = runExcept m ~?= Right x