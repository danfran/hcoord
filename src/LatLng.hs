-- | To represent a latitude/longitude pair based on a particular datum.
module LatLng where

import Datum

data CardinalDirections = North -- ^ Latitude is north of the equator.
                          | South -- ^ Latitude is south of the equator.
                          | East -- ^ Longitude is east of the prime meridian.
                          | West -- ^ Longitude is west of the prime meridian.

data LatLngCoords = LatLngCoords { latitude :: Double
                                 , longitude :: Double
                                 , height :: Double
                                 }

data LatLng = LatLng { coords :: LatLngCoords
                     , datum :: Datum
                     }

