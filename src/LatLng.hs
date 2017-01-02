-- | To represent a latitude/longitude pair based on a particular datum.
module LatLng where

import Control.Monad.Except
import Datum

data LatLngPoint = LatLngPoint { latitude :: Double
                               , longitude :: Double
                               , height :: Double
                               }

data LatLng = LatLng { point :: LatLngPoint
                     , datum :: Datum
                     }

data LatitudeDMS = North DMSPoint -- ^ Latitude is north of the equator.
                   | South DMSPoint -- ^ Latitude is south of the equator.

data LongitudeDMS = East DMSPoint -- ^ Longitude is east of the prime meridian.
                    | West DMSPoint -- ^ Longitude is west of the prime meridian.

data DMSPoint = DMSPoint { degrees :: Double
                         , minutes :: Double
                         , seconds :: Double
                         }

mkLatLng :: LatitudeDMS -> LongitudeDMS -> Datum -> Except String LatLng
mkLatLng lat lng dtm = do
  lt <- withExcept (const "Invalid latitude") (evalLatitude lat)
  ln <- withExcept (const "Invalid longitude") (evalLongitude lng)
  let p = LatLngPoint { latitude = lt , longitude = ln, height = 0 }
  pure LatLng { point = p , datum = dtm }

  where evalLatitude :: LatitudeDMS -> Except String Double
        evalLatitude (North p) = dmsToLatLngPoint p 1
        evalLatitude (South p) = dmsToLatLngPoint p (-1)

        evalLongitude :: LongitudeDMS -> Except String Double
        evalLongitude (East p) = dmsToLatLngPoint p 1
        evalLongitude (West p) = dmsToLatLngPoint p (-1)

        dmsToLatLngPoint :: DMSPoint -> Double -> Except String Double
        dmsToLatLngPoint DMSPoint { degrees = d, minutes = m, seconds = s } cardinal
          | d < 0.0 || m < 0.0 || s < 0.0 || d > 90.0 || m >= 60.0 || s >= 60.0 = throwError "Invalid point"
          | otherwise = pure (cardinal * (d + m / 60.0 + s / 3600.0))
