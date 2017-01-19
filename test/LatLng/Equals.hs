module LatLng.Equals where

import UTMRef
import LatLng
import Datum

import Data.AEq

instance Eq UTMRef where
    (UTMRef easting1 northing1 latZone1 lngZone1) == (UTMRef easting2 northing2 latZone2 lngZone2) =
        (easting1 ~== easting2) && (northing1 ~== northing2) && (latZone1 == latZone2) && (lngZone1 == lngZone2)

instance Eq LatLng where
    (LatLng (LatLngPoint latitude1 longitude1 height1) datum1) == (LatLng (LatLngPoint latitude2 longitude2 height2) datum2) =
        (latitude1 ~== latitude2) && (longitude1 ~== longitude2) && (height1 ~== height2) && (datum1 == datum2)
