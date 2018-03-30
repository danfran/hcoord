module Equals where

import LatLng

import Data.AEq

instance Eq LatLng where
    (LatLng latitude1 longitude1 height1 datum1) == (LatLng latitude2 longitude2 height2 datum2) =
        (latitude1 ~== latitude2) && (longitude1 ~== longitude2) && (height1 ~== height2) && (datum1 == datum2)

