{-|
  To represent an Irish National Grid reference.

  Projection: Transverse Mercator
  Reference ellipsoid: Modified Airy
  Units: metres
  Origin: 53&deg;30'N, 8&deg;W
  False co-ordinates of origin: 200000m east, 250000m north
-}
module IrishRef where

import Control.Monad.Except
import Datum

data IrishRef = IrishRef { easting :: Double -- ^ The easting in metres relative to the origin of the British National Grid.
                         , northing :: Double -- ^ The northing in metres relative to the origin of the British National Grid.
                         , datum :: Datum
                         }

scaleFactor, falseOriginLatitude, falseOriginLongitude, falseOriginEasting, falseOriginNorthing :: Double
scaleFactor = 1.000035
falseOriginLatitude = 53.5
falseOriginLongitude = -8.0
falseOriginEasting = 200000.0
falseOriginNorthing = 250000.0

{-|
  Create a new Ordnance Survey grid reference using a given easting and
  northing. The easting and northing must be in metres and must be relative
  to the origin of the British National Grid.
-}
mkIrishRef :: Double -- ^ The easting in metres. Must be greater than or equal to 0.0 and less than 400000.0.
              -> Double -- ^ The northing in metres. Must be greater than or equal to 0.0 and less than or equal to 500000.0.
              -> Except String IrishRef -- ^ Throws an exception if either the easting or the northing are invalid.
mkIrishRef e n = do
  est <- withExcept (const "Invalid easting") (evalEasting e)
  nrt <- withExcept (const "Invalid northing") (evalNorthing n)
  pure IrishRef { easting = est, northing = nrt, datum = ireland1965Datum }


-- | Validate the easting.
evalEasting :: Double                  -- ^ The easting in metres. Must be greater than or equal to 0.0 and less than 400000.0.
               -> Except String Double -- ^ Throws an Exception if the easting is invalid.
evalEasting e | e < 0.0 || e >= 400000.0 = throwError ("Easting (" ++ show e ++ ") is invalid. Must be greater than or equal to 0.0 and less than 400000.0.")
              | otherwise = pure (e)


-- | Validate the northing.
evalNorthing :: Double                  -- ^ The northing in metres. Must be greater than or equal to 0.0 and less than or equal to 500000.0.
                -> Except String Double -- ^ Throws an Exception if the northing is invalid.
evalNorthing n | n < 0.0 || n > 500000.0 = throwError ("Northing (" ++ show n ++ ") is invalid. Must be greather than or equal to 0.0 and less than or equal to 500000.0.")
               | otherwise = pure (n)
