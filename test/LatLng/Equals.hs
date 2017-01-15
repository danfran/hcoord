module LatLng.Equals where

import UTMRef

import Data.AEq

instance Eq UTMRef where
    (UTMRef easting1 northing1 latZone1 lngZone1) == (UTMRef easting2 northing2 latZone2 lngZone2) =
        (easting1 ~== easting2) && (northing1 ~== northing2) && (latZone1 == latZone2) && (lngZone1 == lngZone2)

