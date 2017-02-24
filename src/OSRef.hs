module OSRef where

import Control.Monad.Except
import Data.Char
import Datum
import Ellipsoid
import qualified LatLng as L
import MathExtensions

data OSRef = OSRef { easting :: Double -- ^ The easting in metres relative to the origin of the British National Grid.
                   , northing :: Double  -- ^ The northing in metres relative to the origin of the British National Grid.
                   , datum :: Datum
                   } deriving (Eq, Show)

data Precision = SixDigits | EightDigits

{-|
  Create a new Ordnance Survey grid reference using a given easting and
  northing. The easting and northing must be in metres and must be relative
  to the origin of the British National Grid.
-}
mkOSRef :: Double -- ^ The easting in metres. Must be greater than or equal to 0.0 and less than 800000.0.
           -> Double -- ^ The northing in metres. Must be greater than or equal to 0.0 and less than 1400000.0.
           -> Except String OSRef -- ^ Throws an exception if either the easting or the northing are invalid.
mkOSRef e n = do
  est <- withExcept (const "Invalid easting") (evalEasting e)
  nrt <- withExcept (const "Invalid northing") (evalNorthing n)
  pure OSRef { easting = est, northing = nrt, datum = osgb36Datum }


{-|
  Take a string formatted as a six-figure OS grid reference (e.g. "TG514131")
  and create a new OSRef object that represents that grid reference. The
  first character must be H, N, S, O or T. The second character can be any
  uppercase character from A through Z excluding I.
-}
mkOSRef' :: String -- ^ a String representing a six-figure Ordnance Survey grid reference in the form XY123456
            -> Except String OSRef -- ^ Throws an exception if ref is not of the form XY123456.
mkOSRef' ref = do
  -- TODO 2006-02-05 : check format
  let coords = findCoords (fromLabel ref 2) (fromLabel ref 5) (ref !! 0)
               where
                   fromLabel :: String -> Int -> Int
                   fromLabel r p = (read (take 3 $ drop p r) :: Int) * 100

                   findCoords :: Int -> Int -> Char -> (Int, Int)
                   findCoords e n c
                     | c == 'H' = (e, n + 1000000)
                     | c == 'N' = (e, n + 500000)
                     | c == 'O' = (e + 500000, n + 500000)
                     | c == 'T' = (e + 500000, n)

  let char2Ord = ord $ ref !! 1
  let c2 = if (char2Ord > 73) then char2Ord - 66 else char2Ord - 65

  let nx = c2 `mod` 5 * 100000
  let ny = (4 - floor ((fromIntegral c2 :: Double) / 5)) * 100000

  mkOSRef (fromIntegral (fst coords + nx)) (fromIntegral (snd coords + ny))


{-|
  Convert latitude and longitude into an OSGB (Ordnance Survey of Great Britain) grid reference.
-}
toOSRef :: L.LatLng -> Except String OSRef
toOSRef (L.LatLng latitude longitude _ _) = do
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

    mkOSRef
      (e0 + iv * (lambda - lambda0) + v * (lambda - lambda0) ** 3 + vi * (lambda - lambda0) ** 5)
      (i + ii * (lambda - lambda0) ** 2 + iii * (lambda - lambda0) ** 4 + iiia * (lambda - lambda0) ** 6)


getOsRefWithPrecisionOf :: Precision -> OSRef -> String
getOsRefWithPrecisionOf SixDigits (OSRef e n _) = evalOsRef 100 e n
getOsRefWithPrecisionOf EightDigits (OSRef e n _) = evalOsRef 10 e n

evalOsRef :: Double -> Double -> Double -> String
evalOsRef precision easting northing = do
  let
      hundredkmE = floor (easting / 100000)
      hundredkmN = floor (northing / 100000)

      firstLetter
        | hundredkmN < 5 = if (hundredkmE < 5) then 'S' else 'T'
        | hundredkmN < 10 = if (hundredkmE < 5) then 'N' else 'O'
        | otherwise = 'H'

      i = 85 - 5 * (hundredkmN `mod` 5) + (hundredkmE `mod` 5)
      secondLetter = chr $ if (i >= 73) then i + 1 else i

      e = floor ((easting - 100000 * fromIntegral hundredkmE) / precision)
      n = floor ((northing - 100000 * fromIntegral hundredkmN) / precision)

  firstLetter : secondLetter : compose e ++ compose n

  where compose :: Int -> String
        compose x = (if (x < 100) then "0" else "")
                    ++ (if (x < 10) then "0" else "")
                    ++ show x


-- | Validate the easting.
evalEasting :: Double                  -- ^ The easting in metres. Must be greater than or equal to 0.0 and less than 800000.0.
               -> Except String Double -- ^ Throws an Exception if the easting is invalid.
evalEasting e | e < 0.0 || e >= 800000.0 = throwError ("Easting (" ++ show e ++ ") is invalid. Must be greater than or equal to 0.0 and less than 800000.0.")
              | otherwise = pure (e)


-- | Validate the northing.
evalNorthing :: Double                  -- ^ The northing in metres. Must be greater than or equal to 0.0 and less than 1400000.0.
                -> Except String Double -- ^ Throws an Exception if the northing is invalid.
evalNorthing n | n < 0.0 || n >= 1400000.0 = throwError ("Northing (" ++ show n ++ ") is invalid. Must be greather than or equal to 0.0 and less than or equal to 1400000.0.")
               | otherwise = pure (n)
