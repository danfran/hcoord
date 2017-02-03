module DMS where

import Control.Monad.Except
import Datum
import LatLng

data LatitudeDMS = North DMS -- ^ Latitude is north of the equator.
                   | South DMS -- ^ Latitude is south of the equator.

data LongitudeDMS = East DMS -- ^ Longitude is east of the prime meridian.
                    | West DMS -- ^ Longitude is west of the prime meridian.

data DMS = DMS { degrees :: Double
               , minutes :: Double
               , seconds :: Double
               }


toLatLng :: LatitudeDMS -> LongitudeDMS -> Datum -> Except String LatLng
toLatLng lat lng dtm = do
  lt <- withExcept (const "Invalid latitude") (evalLatitude lat)
  ln <- withExcept (const "Invalid longitude") (evalLongitude lng)
  pure LatLng { latitude = lt, longitude = ln, height = 0, datum = dtm }
  where evalLatitude :: LatitudeDMS -> Except String Double
        evalLatitude (North p) = dmsToLatLngPoint p 1
        evalLatitude (South p) = dmsToLatLngPoint p (-1)

        evalLongitude :: LongitudeDMS -> Except String Double
        evalLongitude (East p) = dmsToLatLngPoint p 1
        evalLongitude (West p) = dmsToLatLngPoint p (-1)

        dmsToLatLngPoint :: DMS -> Double -> Except String Double
        dmsToLatLngPoint DMS { degrees = d, minutes = m, seconds = s } cardinal
          | d < 0.0 || m < 0.0 || s < 0.0 || d > 90.0 || m >= 60.0 || s >= 60.0 = throwError "Invalid point"
          | otherwise = pure (cardinal * (d + m / 60.0 + s / 3600.0))