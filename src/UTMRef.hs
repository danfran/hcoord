-- | To represent a Universal Transverse Mercator (UTM) reference.
module UTMRef (UTMRef(..), getUTMLatitudeZoneLetter) where

import Control.Monad.Except
import Datum

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