-- | To represent a latitude/longitude pair based on a particular datum.
module LatLng where

import Control.Monad.Except
import Datum
import Ellipsoid
import OSRef

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

class Convert b where
  from :: b -> LatLng
  to :: LatLng -> b

instance Convert OSRef where
  to = toOSRef

dmsToLatLng :: LatitudeDMS -> LongitudeDMS -> Datum -> Except String LatLng
dmsToLatLng lat lng dtm = do
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


latLng :: Double -> Double -> Double -> Datum -> Except String LatLng
latLng lat lng h dtm = do
  lt <- withExcept (const $ "Latitude (" ++ show lat ++ ") is invalid. Must be between -90.0 and 90.0 inclusive.") (validateLatitude lat)
  ln <- withExcept (const $ "Longitude (" ++ show lng ++ ") is invalid. Must be between -180.0 and 180.0 inclusive.") (validateLongitude lng)
  let p = LatLngPoint { latitude = lt , longitude = ln, height = h }
  pure LatLng { point = p , datum = dtm }
  where
        validateLatitude :: Double -> Except String Double
        validateLatitude l
          | l < -90.0 || l > 90.0 = throwError "Invalid latitude"
          | otherwise = pure l
        validateLongitude :: Double -> Except String Double
        validateLongitude l
          | l < -180.0 || l > 180.0 = throwError "Invalid longitude"
          | otherwise = pure l

toRadians :: Double -> Double
toRadians d = d / 180 * pi

sinSquared :: Double -> Double
sinSquared phi = sin phi ** 2

tanSquared :: Double -> Double
tanSquared phi = tan phi ** 2

{-|
  Convert latitude and longitude into an OSGB (Ordnance Survey of Great Britain) grid reference.
-}
toOSRef :: LatLng -> OSRef
toOSRef latLng = do
    let osgb_f0 = 0.9996012717 :: Double
        n0 = -100000.0 :: Double
        e0 = 400000.0 :: Double
        phi0 = toRadians 49.0
        lambda0 = toRadians (-2.0)

        a = semiMajorAxis airy1830Ellipsoid
        b = semiMinorAxis airy1830Ellipsoid
        eSquared = eccentricitySquared airy1830Ellipsoid

        llp = point latLng
        phi = toRadians $ latitude llp
        lambda = toRadians $ longitude llp

        n = (a - b) / (a + b)
        vc = a * osgb_f0 * (1.0 - eSquared * sinSquared phi) ** (-0.5)
        rho = a * osgb_f0 * (1.0 - eSquared) * (1.0 - eSquared * sinSquared phi) ** (-1.5)
        etaSquared = vc / rho - 1.0

        cp = cos phi
        sp = sin phi
        ts = tan phi ** 2

        m = (b * osgb_f0) * (((1.0 + n + (1.25 * n * n) + (1.25 * n * n * n)) * (phi - phi0))
            - (((3 * n) + (3 * n * n) + (2.625 * n * n * n)) * sin(phi - phi0) * cos(phi + phi0))
            + (((1.875 * n * n) + (1.875 * n * n * n)) * sin(2.0 * (phi - phi0)) * cos(2.0 * (phi + phi0)))
            - (((35.0 / 24.0) * n * n * n) * sin(3.0 * (phi - phi0)) * cos(3.0 * (phi + phi0))))
        i = m + n0
        ii = (vc / 2.0) * sp * cp
        iii = (vc / 24.0) * sp * (cp ** 3) * (5.0 - ts + 9.0 * etaSquared)
        iiia = (vc / 720.0) * sp * (cp ** 5) * (61.0 - 58.0 * ts + ts ** 2)
        iv = vc * cp
        v = (vc / 6.0) * (cp ** 3) * (vc / rho - ts)
        vi = (vc / 120.0) * (cp ** 5.0) * (5.0 - 18.0 * ts + ts ** 2 + 14 * etaSquared - 58 * ts * etaSquared)

    OSRef { northing = i + ii * (lambda - lambda0) ** 2 + iii * (lambda - lambda0) ** 4 + iiia * (lambda - lambda0) ** 6
          , easting = e0 + iv * (lambda - lambda0) + v * (lambda - lambda0) ** 3 + vi * (lambda - lambda0) ** 5
          }
