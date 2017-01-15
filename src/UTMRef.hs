-- | To represent a Universal Transverse Mercator (UTM) reference.
module UTMRef (UTMRef(..), getUTMLatitudeZoneLetter) where

data UTMRef = UTMRef { easting :: Double -- ^ Easting
                     , northing :: Double -- ^ Northing
                     , latZone :: Char -- ^ Latitude zone character
                     , lngZone :: Int -- ^ Longitude zone number
                     } deriving (Show)


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