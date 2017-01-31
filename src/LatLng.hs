-- | To represent a latitude/longitude pair based on a particular datum.
module LatLng where

import Control.Monad.Except
import Datum
import Ellipsoid
import MathExtension
import OSRef

data LatLngPoint = LatLngPoint { latitude :: Double
                               , longitude :: Double
                               , height :: Double
                               } deriving (Eq, Show)

data LatLng = LatLng { point :: LatLngPoint
                     , datum :: Datum
                     } deriving (Show)

data LatitudeDMS = North DMSPoint -- ^ Latitude is north of the equator.
                   | South DMSPoint -- ^ Latitude is south of the equator.

data LongitudeDMS = East DMSPoint -- ^ Longitude is east of the prime meridian.
                    | West DMSPoint -- ^ Longitude is west of the prime meridian.

data DMSPoint = DMSPoint { degrees :: Double
                         , minutes :: Double
                         , seconds :: Double
                         }


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


mkLatLng :: Double -> Double -> Double -> Datum -> Except String LatLng
mkLatLng lat lng h dtm = do
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


{-|
  Convert latitude and longitude into an OSGB (Ordnance Survey of Great Britain) grid reference.
-}
toOSRef :: LatLng -> OSRef
toOSRef (LatLng (LatLngPoint latitude longitude _) _) = do
    let osgb_f0 = 0.9996012717 :: Double
        n0 = -100000.0 :: Double
        e0 = 400000.0 :: Double
        phi0 = toRadians 49.0
        lambda0 = toRadians (-2.0)

        a = semiMajorAxis airy1830Ellipsoid
        b = semiMinorAxis airy1830Ellipsoid
        eSquared = eccentricitySquared airy1830Ellipsoid

        phi = toRadians latitude
        lambda = toRadians longitude

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


calcPhiN' :: Double -> Double -> Double -> Double -> Double -> Double
calcPhiN' zB eSquared2 a2 p phiN = do
  let v = a2 / sqrt (1 - eSquared2 * sinSquared phiN)
  atan $ (zB + eSquared2 * v * sin phiN) / p


-- | Convert this LatLng from the OSGB36 datum to the WGS84 datum using an approximate Helmert transformation.
toWGS84 :: LatLng -> LatLng
toWGS84 (LatLng (LatLngPoint latitude longitude height) datum) = do
  let
      a = semiMajorAxis airy1830Ellipsoid
      eSquared = eccentricitySquared airy1830Ellipsoid

      phi = toRadians latitude
      lambda = toRadians longitude
      v = a / sqrt(1 - eSquared * sinSquared phi)
      x = v * cos phi * cos lambda
      y = v * cos phi * sin lambda
      z = (1 - eSquared) * v * sin phi

      tx = 446.448 :: Double
      -- ty : Incorrect value in v1.0 (-124.157). Corrected in v1.1.
      ty = -125.157 :: Double
      tz = 542.060 :: Double
      s = -0.0000204894 :: Double
      rx = toRadians (0.00004172222) :: Double
      ry = toRadians (0.00006861111) :: Double
      rz = toRadians (0.00023391666) :: Double

      xB = tx + x * (1 + s) + (-rx) * y + ry * z
      yB = ty + rz * x + y * (1 + s) + (-rx) * z
      zB = tz + (-ry) * x + rx * y + z * (1 + s)

      a2 = semiMajorAxis wgs84Ellipsoid
      eSquared2 = eccentricitySquared wgs84Ellipsoid

      p = sqrt (xB ** 2 + yB ** 2)
      phiN = atan(zB / (p * (1 - eSquared2)))

      calcPhiN = last $ take 10 $ iterate (calcPhiN' zB eSquared2 a2 p) phiN
--   LatLng (LatLngPoint (xB) (yB) zB) datum
  LatLng (LatLngPoint (toDegrees calcPhiN) (toDegrees $ atan (yB / xB)) height) datum


toDatum :: LatLng -> Datum -> LatLng
toDatum ll@(LatLng (LatLngPoint latitude longitude height) datum) newDatum
  | datum /= wgs84Datum && newDatum /= wgs84Datum = toDatum ll wgs84Datum
  | newDatum == wgs84Datum = ll
  | otherwise = do
      let
          a = semiMajorAxis $ ellipsoid $ datum
          eSquared = eccentricitySquared $ ellipsoid $ datum

          phi = toRadians latitude
          lambda = toRadians longitude
          v = a / sqrt(1 - eSquared * sinSquared phi)
          x = (v + height) * cos phi * cos lambda
          y = (v + height) * cos phi * sin lambda
          z = ((1 - eSquared) * v + height) * sin phi

          dx2 = (-1) * dx newDatum -- 446.448
          dy2 = (-1) * dy newDatum -- -125.157
          dz2 = (-1) * dz newDatum -- 542.060
          ds2 = (-1) * ds newDatum / 1000000.0 -- -0.0000204894
          rx2 = (-1) * toRadians (rx newDatum / 3600.0) -- toRadians(0.00004172222)
          ry2 = (-1) * toRadians (ry newDatum / 3600.0) -- toRadians(0.00006861111)
          rz2 = (-1) * toRadians (rz newDatum / 3600.0) -- toRadians(0.00023391666)

          sc = 1 + ds2
          xB = dx2 + sc * (x + (-rx2) * y + ry2 * z)
          yB = dy2 + sc * (rz2 * x + y + (-rx2) * z)
          zB = dz2 + sc * ((-ry2) * x + rx2 * y + z)

          a2 = semiMajorAxis $ ellipsoid $ newDatum
          eSquared2 = eccentricitySquared $ ellipsoid $ newDatum

          p = sqrt(xB ** 2 + yB ** 2)
          phiN = atan (zB / (p * (1 - eSquared)))

          calcPhiN = last $ take 10 $ iterate (calcPhiN' zB eSquared2 a2 p) phiN
      LatLng (LatLngPoint (toDegrees calcPhiN) (toDegrees $ atan (yB / xB)) height) datum


-- | Convert this LatLng from the WGS84 datum to the OSGB36 datum using an approximate Helmert transformation.
toOSGB36 :: LatLng -> LatLng
toOSGB36 (LatLng (LatLngPoint latitude longitude height) datum) = do
 let
     a = semiMajorAxis wgs84Ellipsoid
     eSquared = eccentricitySquared wgs84Ellipsoid

     phi = toRadians latitude
     lambda = toRadians longitude
     v = a / sqrt(1 - eSquared * sinSquared phi)
     x = v * cos phi * cos lambda
     y = v * cos phi * sin lambda
     z = (1 - eSquared) * v * sin phi

     tx = -446.448 :: Double
     -- ty : Incorrect value in v1.0 (-124.157). Corrected in v1.1.
     ty = 125.157 :: Double
     tz = -542.060 :: Double
     s = 0.0000204894 :: Double
     rx = toRadians (-0.00004172222) :: Double
     ry = toRadians (-0.00006861111) :: Double
     rz = toRadians (-0.00023391666) :: Double

     xB = tx + x * (1 + s) + (-rx) * y + ry * z
     yB = ty + rz * x + y * (1 + s) + (-rx) * z
     zB = tz + (-ry) * x + rx * y + z * (1 + s)

     a2 = semiMajorAxis airy1830Ellipsoid
     eSquared2 = eccentricitySquared airy1830Ellipsoid

     p = sqrt (xB ** 2 + yB ** 2)
     phiN = atan(zB / (p * (1 - eSquared2)))

     calcPhiN = last $ take 10 $ iterate (calcPhiN' zB eSquared2 a2 p) phiN
 LatLng (LatLngPoint (toDegrees calcPhiN) (toDegrees $ atan (yB / xB)) height) datum


-- | Calculate the surface distance in kilometres from this LatLngPoint to the given LatLngPoint.
distance :: LatLngPoint -> LatLngPoint -> Double
distance (LatLngPoint latitudeFrom longitudeFrom _) (LatLngPoint latitudeTo longitudeTo _) = do
  let
      er = 6366.707 :: Double
      latFrom = toRadians latitudeFrom
      latTo = toRadians latitudeTo
      lngFrom = toRadians longitudeFrom
      lngTo = toRadians longitudeTo
  acos (sin latFrom * sin latTo + cos latFrom * cos latTo * cos (lngTo - lngFrom)) * er


-- | Calculate the surface distance in miles from this LatLngPoint to the given LatLngPoint.
distanceMiles :: LatLngPoint -> LatLngPoint -> Double
distanceMiles from to = (distance from to) / 1.609344


latitudeDegrees, longitudeDegrees :: LatLngPoint -> Int
latitudeDegrees (LatLngPoint l _ _) = calcDegrees l
longitudeDegrees (LatLngPoint _ l _) = calcDegrees l

calcDegrees :: Double -> Int
calcDegrees l = do
  let deg = floor l
      minx = (l - fromIntegral deg) :: Double
  case l of _ | l < 0 && minx /= 0 -> (deg :: Int) + 1
              | otherwise -> deg :: Int


latitudeMinutes, longitudeMinutes :: LatLngPoint -> Int
latitudeMinutes (LatLngPoint l _ _) = calcMinutes l
longitudeMinutes (LatLngPoint _ l _) = calcMinutes l

calcMinutes :: Double -> Int
calcMinutes l = do
  let deg = floor l
      minx = (l - fromIntegral deg) :: Double
      min | l < 0 && minx /= 0 = 1 - minx
          | otherwise = minx
  floor (60 * min) :: Int


latitudeSeconds, longitudeSeconds :: LatLngPoint -> Double
latitudeSeconds (LatLngPoint l _ _) = calcSeconds l
longitudeSeconds (LatLngPoint _ l _) = calcSeconds l

calcSeconds :: Double -> Double
calcSeconds l = do
  let deg = floor l
      minx = (l - fromIntegral deg) :: Double
      min | l < 0 && minx /= 0 = 1 - minx
          | otherwise = minx
  60 * (60 * min - fromIntegral (floor (60 * min)))
