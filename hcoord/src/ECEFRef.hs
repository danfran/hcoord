module ECEFRef where

import Datum
import Ellipsoid
import qualified LatLng as L
import MathExtensions

{- |
   ECEF (earth-centred, earth-fixed) Cartesian co-ordinates are used to define a
   point in three-dimensional space. ECEF co-ordinates are defined relative to
   an x-axis (the intersection of the equatorial plane and the plane defined by
   the prime meridian), a y-axis (at 90&deg; to the x-axis and its intersection
   with the equator) and a z-axis (intersecting the North Pole). All the axes
   intersect at the point defined by the centre of mass of the Earth.
-}

data ECEFRef = ECEFRef { x :: Double
                       , y :: Double
                       , z :: Double
                       , datum :: Datum
                       } deriving (Show)


-- | Create a new earth-centred, earth-fixed reference from the given latitude and longitude.
toECEFRef :: L.LatLng -> ECEFRef
toECEFRef (L.LatLng latitude longitude height datum) = do
  let
      el = ellipsoid datum
      a = semiMajorAxis el
      f = flattening el
      eSquared = f * (2 - f)
      phi = toRadians latitude
      nphi = a / sqrt (1 - eSquared * sinSquared phi)
      lambda = toRadians longitude

  ECEFRef { x = (nphi + height) * cos phi * cos lambda
          , y = (nphi + height) * cos phi * sin lambda
          , z = (nphi * (1 - eSquared) + height) * sin phi
          , datum = datum
          }

-- | Convert this ECEFRef object to a LatLng.
toLatLng :: ECEFRef -> L.LatLng
toLatLng (ECEFRef x y z datum) = do
  let
      el = ellipsoid datum
      a = semiMajorAxis el
      b = semiMinorAxis el
      e2Squared = (a / b) ** 2 - 1
      f = flattening el
      eSquared = f * (2 - f)
      p = sqrt (x ** 2 + y ** 2)
      theta = atan (z * a / (p * b))

      phi = atan $ (z + (e2Squared * b * sinCubed theta ))
            / (p - eSquared * a * cosCubed theta)
      lambda = atan2 y x

      nphi = a / sqrt(1 - eSquared * sinSquared phi )
      h = (p / cos phi ) - nphi

  L.LatLng (toDegrees phi) (toDegrees lambda) h wgs84Datum