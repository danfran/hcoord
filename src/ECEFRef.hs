module ECEFRef where

import Ellipsoid

{- |
   ECEF (earth-centred, earth-fixed) Cartesian co-ordinates are used to define a
   point in three-dimensional space. ECEF co-ordinates are defined relative to
   an x-axis (the intersection of the equatorial plane and the plane defined by
   the prime meridian), a y-axis (at 90&deg; to the x-axis and its intersection
   with the equator) and a z-axis (intersecting the North Pole). All the axes
   intersect at the point defined by the centre of mass of the Earth.
-}

data ECEFRefCoords = ECEFRefCoords { x :: Double
                                   , y :: Double
                                   , z :: Double
                                   }

data ECEFRef = ECEFRef { coords :: ECEFRefCoords
                       , ellipsoid :: Ellipsoid
                       }



