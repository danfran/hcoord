-- | To represent a Universal Transverse Mercator (UTM) reference.
module UTMRef where

import Control.Monad.Except
import Datum
import Ellipsoid
import LatLng
import MathExtension

data UTMRef = UTMRef { easting :: Double -- ^ Easting
                     , northing :: Double -- ^ Northing
                     , latZone :: Char -- ^ Latitude zone character
                     , lngZone :: Int -- ^ Longitude zone number
                     , datum :: Datum
                     } deriving (Show)

{-|
  Create a new UTM reference object. Checks are made to make sure that the
  given parameters are roughly valid, but the checks are not exhaustive with
  regards to the easting value. Catching an exception does not necessarily
  mean that the UTM reference is well-formed. This is because that valid
  values for the easting vary depending on the latitude.
-}
mkUTMRef :: Int -- ^ The longitude zone number
         -> Char -- ^ The latitude zone character
         -> Double -- ^ The easting in metres
         -> Double -- ^ The northing in metres
         -> Datum
         -> Except String UTMRef -- | If any of the parameters are invalid. Be careful that a valid
                                 --   value for the easting does not necessarily mean that the UTM
                                 --   reference is well-formed. The current checks do not take into
                                 --   account the varying range of valid values for the easting for
                                 --   different latitudes.
mkUTMRef lngZone latZone easting northing datum
  | lngZone < 1 || lngZone > 60 = throwError $ "Longitude zone (" ++ show lngZone ++ ") is not defined on the UTM grid."
  | latZone < 'C' || latZone > 'X' = throwError $ "Latitude zone (" ++ show latZone ++ ") is not defined on the UTM grid."
  | easting < 0.0 || easting > 1000000.0 = throwError $ "Easting (" ++ show easting ++ ") is not defined on the UTM grid."
  | northing < 0.0 || northing > 10000000.0 = throwError $ "Northing (" ++ show northing ++ ") is not defined on the UTM grid."
  | otherwise = pure $ UTMRef easting northing latZone lngZone datum

-- | Convert this UTM reference to a latitude and longitude.
toLatLng :: UTMRef -> LatLngPoint
toLatLng (UTMRef east north ltz lnz datum) = do
  let
      utm_f0 = 0.9996 :: Double
      el = ellipsoid datum
      a = semiMajorAxis el
      eSquared = eccentricitySquared el

      ePrimeSquared = eSquared / (1 - eSquared)
      es1 = sqrt (1 - eSquared)
      e1 = (1 - es1) / (1 + es1)
      x = east - 500000.0
      y = if (ltz < 'N') then north - 10000000.0 else north
      longitudeOrigin = fromIntegral $ 6 * lnz - 183

      m = y / utm_f0
      mu = m / (a * (1 - eSquared / 4 - 3 / 64 * eSquared ** 2 - 5 / 256 * eSquared ** 3))

      phi1Rad = mu + (1.5 * e1 - 27 / 32 * e1 ** 3) * sin(2 * mu)
                + (21 / 16 * e1 ** 2 - 55 / 32 * e1 ** 4)
                * sin(4  * mu) + (151 / 96 * e1 ** 3) * sin(6  * mu)

      n = a / sqrt(1 - eSquared * sinSquared phi1Rad)
      t = tanSquared phi1Rad
      c = ePrimeSquared * cosSquared phi1Rad
      r = a * (1 - eSquared) / (1 - eSquared * sinSquared phi1Rad) ** 1.5
      d = x / (n * utm_f0)

      latitude = (phi1Rad - (n * tan phi1Rad / r)
                 * (d * d / 2 - (5 + 3 * t + 10 * c - 4 * c ** 2 - 9 * ePrimeSquared)
                 * d ** 4 / 24  + (61 + 90 * t + 298 * c + 45 * t ** 2
                 - 252 * ePrimeSquared - 3  * c ** 2) * d ** 6 / 720)) * 180 / pi

      longitude = longitudeOrigin
                + ((d - (1 + 2 * t + c) / 6 * d ** 3
                + (5 - 2 * c + 28 * t - 3 * c ** 2 + 8 * ePrimeSquared + 24 * t ** 2)
                * d ** 5 / 120) / cos phi1Rad) * 180 / pi

  LatLngPoint latitude longitude 0


{-|
  Convert latitude and longitude to a UTM reference.
  If an attempt is made to convert a LatLng that falls outside the area covered by the UTM grid.
  The UTM grid is only defined for latitudes south of 84&deg;N and north of 80&deg;S.
-}
toUTMRef :: LatLng -> Except String UTMRef
toUTMRef (LatLng (LatLngPoint latitude longitude _) datum) =
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

     pure $ UTMRef utmEasting utmNorthing utmZone longitudeZone datum
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


-- | Work out the UTM latitude zone from the latitude.
getUTMLatitudeZoneLetter :: Double -> Char
getUTMLatitudeZoneLetter latitude
  | 84 >= latitude && latitude >= 72 = 'X'
  | 72 > latitude && latitude >= 64 = 'W'
  | 64 > latitude && latitude >= 56 = 'V'
  | 56 > latitude && latitude >= 48 = 'U'
  | 48 > latitude && latitude >= 40 = 'T'
  | 40 > latitude && latitude >= 32 = 'S'
  | 32 > latitude && latitude >= 24 = 'R'
  | 24 > latitude && latitude >= 16 = 'Q'
  | 16 > latitude && latitude >= 8 = 'P'
  | 8 > latitude && latitude >= 0 = 'N'
  | 0 > latitude && latitude >= -8 = 'M'
  | -8 > latitude && latitude >= -16 = 'L'
  | -16 > latitude && latitude >= -24 = 'K'
  | -24 > latitude && latitude >= -32 = 'J'
  | -32 > latitude && latitude >= -40 = 'H'
  | -40 > latitude && latitude >= -48 = 'G'
  | -48 > latitude && latitude >= -56 = 'F'
  | -56 > latitude && latitude >= -64 = 'E'
  | -64 > latitude && latitude >= -72 = 'D'
  | -72 > latitude && latitude >= -80 = 'C'
  | otherwise = 'Z'