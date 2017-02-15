-- | To represent a latitude/longitude pair based on a particular datum.
module LatLng where

import Control.Monad.Except
import Datum
import Ellipsoid
import MathExtensions
import OSRef


data LatLng = LatLng { latitude :: Double
                     , longitude :: Double
                     , height :: Double
                     , datum :: Datum
                     } deriving (Show)


mkLatLng :: Double -> Double -> Double -> Datum -> Except String LatLng
mkLatLng lat lng h dtm = do
  lt <- withExcept (const $ "Latitude (" ++ show lat ++ ") is invalid. Must be between -90.0 and 90.0 inclusive.") (validateLatitude lat)
  ln <- withExcept (const $ "Longitude (" ++ show lng ++ ") is invalid. Must be between -180.0 and 180.0 inclusive.") (validateLongitude lng)
  pure LatLng { latitude = lt, longitude = ln, height = h, datum = dtm }
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
toOSRef (LatLng latitude longitude _ _) = do
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
toWGS84 (LatLng latitude longitude height datum) = do
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
  LatLng (toDegrees calcPhiN) (toDegrees $ atan (yB / xB)) height datum


toDatum :: LatLng -> Datum -> LatLng
toDatum ll@(LatLng latitude longitude height datum) newDatum
  | datum /= wgs84Datum && newDatum /= wgs84Datum = mkToDatum ll newDatum 1
  | datum == wgs84Datum && newDatum /= wgs84Datum = mkToDatum ll newDatum (-1)
  | datum /= wgs84Datum && newDatum == wgs84Datum = ll
  | otherwise = mkToDatum ll newDatum 1
  where mkToDatum :: LatLng -> Datum -> Double -> LatLng
        mkToDatum ll@(LatLng latitude longitude height datum) newDatum invert = do
          let
              a = semiMajorAxis $ ellipsoid $ datum
              eSquared = eccentricitySquared $ ellipsoid $ datum

              phi = toRadians latitude
              lambda = toRadians longitude
              v = a / sqrt(1 - eSquared * sinSquared phi)
              x = (v + height) * cos phi * cos lambda
              y = (v + height) * cos phi * sin lambda
              z = ((1 - eSquared) * v + height) * sin phi

              dx2 = invert * dx newDatum -- 446.448
              dy2 = invert * dy newDatum -- -125.157
              dz2 = invert * dz newDatum -- 542.060
              ds2 = invert * ds newDatum / 1000000.0 -- -0.0000204894
              rx2 = invert * toRadians (rx newDatum / 3600.0) -- toRadians(0.00004172222)
              ry2 = invert * toRadians (ry newDatum / 3600.0) -- toRadians(0.00006861111)
              rz2 = invert * toRadians (rz newDatum / 3600.0) -- toRadians(0.00023391666)

              sc = 1 + ds2
              xB = dx2 + sc * (x + (-rx2) * y + ry2 * z)
              yB = dy2 + sc * (rz2 * x + y + (-rx2) * z)
              zB = dz2 + sc * ((-ry2) * x + rx2 * y + z)

              a2 = semiMajorAxis $ ellipsoid $ newDatum
              eSquared2 = eccentricitySquared $ ellipsoid $ newDatum

              p = sqrt(xB ** 2 + yB ** 2)
              phiN = atan (zB / (p * (1 - eSquared2)))

              calcPhiN = last $ take 10 $ iterate (calcPhiN' zB eSquared2 a2 p) phiN
          LatLng (toDegrees calcPhiN) (toDegrees $ atan (yB / xB)) height datum


-- | Convert this LatLng from the WGS84 datum to the OSGB36 datum using an approximate Helmert transformation.
toOSGB36 :: LatLng -> LatLng
toOSGB36 (LatLng latitude longitude height datum) = do
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
 LatLng (toDegrees calcPhiN) (toDegrees $ atan (yB / xB)) height datum


-- | Calculate the surface distance in kilometres from this LatLngPoint to the given LatLngPoint.
distance :: LatLng -> LatLng -> Double
distance (LatLng latitudeFrom longitudeFrom _ _) (LatLng latitudeTo longitudeTo _ _) = do
  let
      er = 6366.707 :: Double
      latFrom = toRadians latitudeFrom
      latTo = toRadians latitudeTo
      lngFrom = toRadians longitudeFrom
      lngTo = toRadians longitudeTo
  acos (sin latFrom * sin latTo + cos latFrom * cos latTo * cos (lngTo - lngFrom)) * er


-- | Calculate the surface distance in miles from this LatLngPoint to the given LatLngPoint.
distanceMiles :: LatLng -> LatLng -> Double
distanceMiles from to = (distance from to) / 1.609344


latitudeDegrees, longitudeDegrees :: LatLng -> Int
latitudeDegrees (LatLng l _ _ _) = calcDegrees l
longitudeDegrees (LatLng _ l _ _) = calcDegrees l

calcDegrees :: Double -> Int
calcDegrees l = do
  let deg = floor l
      minx = (l - fromIntegral deg) :: Double
  case l of _ | l < 0 && minx /= 0 -> (deg :: Int) + 1
              | otherwise -> deg :: Int


latitudeMinutes, longitudeMinutes :: LatLng -> Int
latitudeMinutes (LatLng l _ _ _) = calcMinutes l
longitudeMinutes (LatLng _ l _ _) = calcMinutes l

calcMinutes :: Double -> Int
calcMinutes l = do
  let deg = floor l
      minx = (l - fromIntegral deg) :: Double
      min | l < 0 && minx /= 0 = 1 - minx
          | otherwise = minx
  floor (60 * min) :: Int


latitudeSeconds, longitudeSeconds :: LatLng -> Double
latitudeSeconds (LatLng l _ _ _) = calcSeconds l
longitudeSeconds (LatLng _ l _ _) = calcSeconds l

calcSeconds :: Double -> Double
calcSeconds l = do
  let deg = floor l
      minx = (l - fromIntegral deg) :: Double
      min | l < 0 && minx /= 0 = 1 - minx
          | otherwise = minx
  60 * (60 * min - fromIntegral (floor (60 * min)))
