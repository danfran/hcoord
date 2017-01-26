-- | To represent a latitude/longitude pair based on a particular datum.
module LatLng where

import Control.Monad.Except
import Datum
import Ellipsoid
import OSRef
import qualified UTMRef

data LatLngPoint = LatLngPoint { latitude :: Double
                               , longitude :: Double
                               , height :: Double
                               } deriving (Show)

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

toDegrees :: Double -> Double
toDegrees a = a * 180.0 / pi

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

{-|
  Convert latitude and longitude to a UTM reference.
  If an attempt is made to convert a LatLng that falls outside the area covered by the UTM grid.
  The UTM grid is only defined for latitudes south of 84&deg;N and north of 80&deg;S.
-}
toUTMRef :: LatLng -> Except String UTMRef.UTMRef
toUTMRef (LatLng (LatLngPoint latitude longitude _) _) =
  do
     lt <- withExcept (const $ "Latitude (" ++ show latitude ++ ") falls outside the UTM grid.") (validateLatitude latitude)
     let
         utm_f0 = 0.9996 :: Double
         a = semiMajorAxis wgs84Ellipsoid
         eSquared = eccentricitySquared wgs84Ellipsoid

         pifr = pi / 180.0 :: Double
         latitudeRad = lt * pifr
         ln = if longitude == 180.0 then (-180.0) else longitude
         longitudeRad = ln * pifr
         longitudeZone = evalLongitudeZone lt ln

         longitudeOrigin = fromIntegral $ 6 * longitudeZone - 183 :: Double
         longitudeOriginRad = longitudeOrigin * pifr

         utmZone = UTMRef.getUTMLatitudeZoneLetter lt

         ePrimeSquared = eSquared / (1 - eSquared)

         n = a / sqrt (1 - eSquared * sin latitudeRad * sin latitudeRad)
         t = tan latitudeRad ** 2
         c = ePrimeSquared * cos latitudeRad ** 2
         aa = cos latitudeRad * (longitudeRad - longitudeOriginRad)

         eFoured = eSquared * eSquared
         eSixed = eFoured * eSquared

         m = a * ((1 - eSquared / 4 - 3 * eFoured / 64 - 5 * eSixed / 256) * latitudeRad
             - (3 * eSquared / 8 + 3 * eFoured / 32 + 45 * eSixed / 1024) * sin (2 * latitudeRad)
             + (15 * eFoured / 256 + 45 * eSixed / 1024) * sin (4 * latitudeRad)
             - (35 * eSixed / 3072) * sin (6 * latitudeRad))

         utmEasting = (utm_f0 * n * (aa + (1 - t + c) * aa ** 3 / 6
                      + (5 - 18 * t + t * t + 72 * c - 58 * ePrimeSquared) * aa ** 5.0 / 120) + 500000.0)

         -- Adjust for the southern hemisphere
         utmNorthingAdj = if (lt < 0) then 10000000.0 else 0.0
         utmNorthing = (utm_f0 * (m + n * tan latitudeRad * (aa * aa / 2 + (5 - t + 9 * c + 4 * c * c) * aa ** 4.0 / 24
                       + (61 - 58 * t + t * t + 600 * c - 330 * ePrimeSquared) * aa ** 6.0 / 720))) + utmNorthingAdj

     pure $ UTMRef.UTMRef utmEasting utmNorthing utmZone longitudeZone
     where
        validateLatitude :: Double -> Except String Double
        validateLatitude lat
          | lat < -80.0 || lat > 84.0 = throwError "Invalid latitude"
          | lat == 180.0 = pure (-180.0)
          | otherwise = pure lat

        evalLongitudeZone :: Double -> Double -> Int
        evalLongitudeZone lat lng
          | lat >= 56.0 && lat < 64.0 && lng >= 3.0 && lng < 12.0 = 32 -- Special zone for Norway
          | lat >= 72.0 && lat < 84.0 && lng >= 0.0 && lng < 9.0 = 31 -- Special zones for Svalbard
          | lat >= 72.0 && lat < 84.0 && lng >= 9.0 && lng < 21.0 = 33
          | lat >= 72.0 && lat < 84.0 && lng >= 21.0 && lng < 33.0 = 35
          | lat >= 72.0 && lat < 84.0 && lng >= 33.0 && lng < 42.0 = 37
          | otherwise = fromIntegral $ floor (lng / 6.0 + 30.0) + 1 :: Int


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


latitudeDegrees :: LatLngPoint -> Int
latitudeDegrees (LatLngPoint l _ _) = do
  let deg = floor l
  case l of _ | l < 0 && (l - fromIntegral deg) /= 0 -> (deg :: Int) + 1
              | otherwise -> deg :: Int


latitudeMinutes :: LatLngPoint -> Int
latitudeMinutes (LatLngPoint l _ _) = do
  let deg = floor l
      minx = (l - fromIntegral deg) :: Double
      min | l < 0 && minx /= 0 = 1 - minx
          | otherwise = minx
  floor (60 * min) :: Int


latitudeSeconds :: LatLngPoint -> Double
latitudeSeconds (LatLngPoint l _ _) = do
  let deg = floor l
      minx = (l - fromIntegral deg) :: Double
      min | l < 0 && minx /= 0 = 1 - minx
          | otherwise = minx
  60 * (60 * min - fromIntegral (floor (60 * min)))
