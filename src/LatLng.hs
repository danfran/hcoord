-- | To represent a latitude/longitude pair based on a particular datum.
module LatLng where

import Control.Monad.Except
import Datum
import Ellipsoid
import MathExtensions


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
