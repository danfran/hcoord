{-|
  To represent a Military Grid Reference System (MGRS) reference.

  Military Grid Reference System (MGRS)

  The Military Grid Reference System (MGRS) is an extension of the Universal
  Transverse Mercator (UTM) reference system. An MGRS reference is made from 5
  parts:

  UTM Longitude Zone
  This is a number indicating which UTM longitude zone the reference falls
  into. Zones are numbered from 1 (starting at 180&deg;W) through 60. Each zone
  is 6&deg; wide.

  UTM Latitude Zone
  Latitude is split into regions that are 8&deg; high, starting at 80&deg;S.
  Latitude zones are lettered using C through X, but omitting I and O as they
  can easily be confused with the numbers 1 and 0.

  100,000m Square identification
  Each UTM zone is treated as a square 100,000m to a side. The 50,000m easting
  is centred on the centre-point of the UTM zone. 100,000m squares are
  identified using two characters - one to identify the row and one to identify
  the column.
  Row identifiers use the characters A through V (omitting I and O again). The
  sequence is repeated every 2,000,000m from the equator. If the UTM longitude
  zone is odd, then the lettering is advanced by five characters to start at F.
  Column identifiers use the characters A through Z (again omitting I and O).

  Easting and northing
  Each 100,000m grid square is further divided into smaller squares
  representing 1m, 10m, 100m, 1,000m and 10,000m precision. The easting and
  northing are given using the numeric row and column reference of the square,
  starting at the bottom-left corner of the square.

  MGRS Reference Example
  18SUU8362601432 is an example of an MGRS reference. '18' is the UTM longitude
  zone, 'S' is the UTM latitude zone, 'UU' is the 100,000m square
  identification, 83626 is the easting reference to 1m precision and 01432 is
  the northing reference to 1m precision.

  MGRSRef
  Methods are provided to query an  MGRSRef  object for its
  parameters. As MGRS references are related to UTM references, a
  {@link MGRSRef#toUTMRef() toUTMRef()}  method is provided to
  convert an  MGRSRef  object into a  {@link UTMRef} 
  object. The reverse conversion can be made using the
  {@link #MGRSRef(UTMRef) MGRSRef(UTMRef)}  constructor.
  MGRSRef  objects can be converted to {@link LatLng LatLng}
  objects using the {@link MGRSRef#toLatLng() toLatLng()}  method. The reverse
  conversion is made using the {@link LatLng#toMGRSRef() LatLng.toMGRSRef()}  method.
  Some MGRS references use the Bessel 1841 ellipsoid rather than the Geodetic
  Reference System 1980 (GRS 1980), International or World Geodetic System 1984
  (WGS84) ellipsoids. Use the constructors with the optional boolean parameter
  to be able to specify whether your MGRS reference uses the Bessel 1841
  ellipsoid. Note that no automatic determination of the correct ellipsoid to
  use is made.

  Important note: There is currently no support for MGRS references in
  polar regions north of 84&deg;N and south of 80&deg;S. There is also no
  account made for UTM zones with slightly different sizes to normal around
  Svalbard and Norway.
-}
module MGRSRef where

import Control.Monad.Except
import Data.Char
import Data.Fixed

import Datum
import qualified UTMRef as U

data MGRSRef = MGRSRef { easting :: Int
                       , northing :: Int
                       , eastingId :: Char
                       , northingId :: Char
                       , utmZoneNumber :: Int
                       , utmZoneChar :: Char
                       , precision :: Precision
                       , isBessel :: Bool
                       , datum :: Datum
                       }


data Precision = M1 -- ^ precision of 1m
                 | M10 -- ^ precision of 10m
                 | M100 -- ^ precision of 100m
                 | M1000 -- ^ precision of 1000m (1km)
                 | M10000  -- ^ precision of 10000m (10km)


instance Enum Precision where
  fromEnum M1 = 1
  fromEnum M10 = 10
  fromEnum M100 = 100
  fromEnum M1000 = 1000
  fromEnum M10000 = 10000


-- | Northing characters
northingIds :: [Char]
northingIds = [ 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'J', 'K'
              , 'L', 'M', 'N', 'P', 'Q', 'R', 'S', 'T', 'U', 'V' ]


instance Show MGRSRef where
  show m@(MGRSRef _ _ _ _ _ _ precision _ _) = showWithPrecision m precision

-- | Return a String representation of this MGRS reference to 1m, 10m, 100m, 1000m or 10000m precision.
showWithPrecision :: MGRSRef
                     -> Precision -- ^ One of MGRSRef.PRECISION_1M, MGRSRef.PRECISION_10M, MGRSRef.PRECISION_100M, MGRSRef.PRECISION_1000M, MGRSRef.PRECISION_10000M.
                     -> String -- ^ ~ String representation of this MGRS reference to the required precision.
showWithPrecision (MGRSRef easting northing eastingId northingId utmZoneNumber utmZoneChar _ _ _) precision = do
  let
      p = fromEnum precision
      eastingR = easting `div` p
      northingR = northing `div` p
      padding = case precision of
        M10 -> 4
        M100 -> 3
        M1000 -> 2
        M10000 -> 1
        _ -> 5
      eastingRs = padWithZeros (show easting) padding
      northingRs = padWithZeros (show northing) padding
      utmZonePadding = if (utmZoneNumber < 10) then "0" else ""
  utmZonePadding ++ show utmZoneNumber ++ [utmZoneChar] ++ [eastingId] ++ [northingId] ++ eastingRs ++ northingRs
  where padWithZeros :: String -> Int -> String
        padWithZeros text padding =
          if (padding - length text > 0) then padWithZeros ('0' : text) padding
          else text


{-|
  Create a new MGRS reference object from the given UTM reference. It is
  assumed that this MGRS reference represents a point using the GRS 1980,
  International or WGS84 ellipsoids. It is assumed that the UTMRef object is
  valid.
-}
toMGRSRef :: U.UTMRef -- ^ UTM reference.
             -> Bool -- ^ True if the parameters represent an MGRS reference using the Bessel 1841 ellipsoid; false is the parameters represent an MGRS reference using the GRS 1980, International or WGS84 ellipsoids.
             -> MGRSRef
toMGRSRef (U.UTMRef easting northing latZone lngZone _) isBessel = do
  let
      set = (lngZone - 1) `mod` 6 + 1

      eId = (\x -> x + (if x > 14 then 1 else 0))
            . (\x -> x + (if x > 8 then 1 else 0))
            $ (floor (easting / 100000.0) + 8 * ((set - 1) `mod` 3))
      eIdC = chr $ eId + 64

      nId = (\x -> x - (if x > 19 then 20 else 0))
            . (\x -> x + (if isBessel then 10 else 0))
            . (\x -> x + (if even set then 5 else 0))
            $ floor ((northing  `mod'` 2000000) / 100000.0)
      nIdC = northingIds !! nId

  MGRSRef (round(easting) `mod` 100000) (round(northing) `mod` 100000) eIdC nIdC lngZone latZone M1 isBessel wgs84Datum


mkMGRSRef :: Int -- ^ The easting in metres. Must be greater than or equal to 0.0 and less than 100000.
             -> Int -- ^ The northing in metres. Must be greater than or equal to 0.0 and less than or equal to 500000.0.
             -> Char -- ^ The character representing the 100,000km easting square.
             -> Char -- ^ The character representing the 100,000km northing square.
             -> Int -- ^ The UTM zone number representing the longitude.
             -> Char -- ^ The UTM zone character representing the latitude.
             -> Precision -- ^ The precision of the given easting and northing.
             -> Bool -- ^ True if the parameters represent an MGRS reference using the Bessel 1841 ellipsoid; False is the parameters represent an MGRS reference using the GRS 1980, International or WGS84 ellipsoids.
             -> Except String MGRSRef -- ^ Throws an exception if any of the given parameters are invalid. Note that the parameters are only checked for the range of values that they can take on. Being able to create an MGRSRef object does not necessarily imply that the reference is valid.
mkMGRSRef e n eId nId un uc p b = do
  est <- withExcept (const "Invalid easting") (evalEasting e)
  nrt <- withExcept (const "Invalid northing") (evalNorthing n)
  estId <- withExcept (const "Invalid eastingId") (evalEastingId eId)
  nrtId <- withExcept (const "Invalid northingId") (evalNorthingId nId)
  uzn <- withExcept (const "Invalid utmZoneNumber") (evalUtmZoneNumber un)
  uzc <- withExcept (const "Invalid utmZoneChar") (evalUtmZoneChar uc)
  pure MGRSRef { easting = est
               , northing = nrt
               , eastingId = estId
               , northingId = nrtId
               , utmZoneNumber = uzn
               , utmZoneChar = uzc
               , precision = p
               , isBessel = b
               , datum = wgs84Datum }


evalEasting :: Int -> Except String Int
evalEasting e | e < 0 || e > 99999 = throwError ("Invalid easting (" ++ show e ++ ")")
              | otherwise = pure (e)

evalNorthing :: Int -> Except String Int
evalNorthing n | n < 0 || n > 99999 = throwError ("Invalid northing (" ++ show n ++ ")")
               | otherwise = pure (n)

evalUtmZoneNumber :: Int -> Except String Int
evalUtmZoneNumber u | u < 1 || u > 60 = throwError ("Invalid utmZoneNumber (" ++ show u ++ ")")
                    | otherwise = pure (u)

evalUtmZoneChar :: Char -> Except String Char
evalUtmZoneChar u | u < 'A' || u > 'Z' = throwError ("Invalid utmZoneChar (" ++ show u ++ ")")
                  | otherwise = pure (u)

evalEastingId :: Char -> Except String Char
evalEastingId e | e < 'A' || e > 'Z' || e == 'I' || e == 'O' = throwError ("Invalid eastingId (" ++ show e ++ ")")
                | otherwise = pure (e)

evalNorthingId :: Char -> Except String Char
evalNorthingId n | n < 'A' || n > 'Z' || n == 'I' || n == 'O' = throwError ("Invalid northingId (" ++ show n ++ ")")
                 | otherwise = pure (n)

