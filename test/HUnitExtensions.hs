module HUnitExtensions where
import Test.HUnit
import Control.Monad (unless)
import Control.Monad.Except
import Data.Default
import Datum
import IrishRef
import LatLng
import MGRSRef
import UTMRef

instance Default LatLng where
    def = LatLng 0 0 0 wgs84Datum

instance Default IrishRef where
    def = IrishRef 0 0 ireland1965Datum

instance Default UTMRef where
    def = UTMRef 0 0 ' ' 0 wgs84Datum

instance Default MGRSRef where
    def = MGRSRef 0 0 ' ' ' ' 0 ' ' M1 False wgs84Datum

extract :: Default a => Except String a -> a
extract ex = either (const def) id (runExcept ex)

--------------------------

-- As alternative:

-- Since ExceptT e m is an instance of Foldable,
-- you can define Monoid instances for your types and use:
-- fold :: (Foldable f, Monoid m) => f m -> m.

-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
--
-- import Data.Monoid
--
-- newtype LatLng = LatLng { getLatLng :: Sum Int } deriving Monoid
--
-- extractLatLng :: Except e LatLng -> LatLng
-- extractLatLng = fold

--------------------------

shouldThrow :: (Eq a, Eq e, Show a, Show e) => Except e a -> e -> Test
m `shouldThrow` e = runExcept m ~?= Left e

shouldReturn :: (Eq a, Eq e, Show a, Show e) => Except e a -> a -> Test
m `shouldReturn` x = runExcept m ~?= Right x