module Ellipsoid where

-- | To represent a reference ellipsoid.
data Ellipsoid =
  Ellipsoid { semiMajorAxis :: Double -- ^ Semi major axis.
            , semiMinorAxis :: Double -- ^ Semi minor axis.
            , eccentricitySquared :: Double -- ^ Eccentricity squared.
            , flattening :: Double -- ^ Flattening.
            } deriving (Eq)

-- | Parameters to build Ellipsoid.
data EllipsoidParams = SemiMajMinAxises Double Double -- ^ Semi major and semi minor axises.
                       | SemiMajAxisEccentricitySquared Double Double -- ^ Semi major axis and eccentric squared.

instance Show Ellipsoid where
  show (Ellipsoid sMajAx sMinAx _ _) =
    "[semi-major axis = " ++ show sMajAx ++ ", semi-minor axis = " ++ show sMinAx ++ "]"


-- | Create an Ellipsoid with the given parameters.
mkEllipsoid :: EllipsoidParams -- ^ Ellipsoid parameters.
            -> Ellipsoid -- ^ The created Ellipsoid.

mkEllipsoid (SemiMajMinAxises sMajAx sMinAx) =
  Ellipsoid { semiMajorAxis = sMajAx
            , semiMinorAxis = sMinAx
            , eccentricitySquared = 1 - (sMinAx / sMajAx) ** 2 :: Double
            , flattening = 1 - sMinAx / sMajAx
            }

mkEllipsoid (SemiMajAxisEccentricitySquared sMajAx eccSq) =
  Ellipsoid { semiMajorAxis = sMajAx
            , semiMinorAxis = sMinAx
            , eccentricitySquared = eccSq
            , flattening = 1 - sMinAx / sMajAx
            }
  where sMinAx = sqrt (sMajAx * sMajAx * (1 - eccSq))


-- | Pre-determined ellipsoids:

airy1830Ellipsoid :: Ellipsoid
airy1830Ellipsoid = mkEllipsoid (SemiMajMinAxises 6377563.396 6356256.909)

australianNational1966Ellipsoid :: Ellipsoid
australianNational1966Ellipsoid = mkEllipsoid (SemiMajMinAxises 6378160.0 6356774.719)

bessel1841Ellipsoid :: Ellipsoid
bessel1841Ellipsoid = mkEllipsoid (SemiMajMinAxises 6377397.155 6356078.9629)

clarke1866Ellipsoid :: Ellipsoid
clarke1866Ellipsoid = mkEllipsoid (SemiMajMinAxises 6378206.4 6356583.8)

clarke1880Ellipsoid :: Ellipsoid
clarke1880Ellipsoid = mkEllipsoid (SemiMajMinAxises 6378249.145 6356514.8696)

everest1830Ellipsoid :: Ellipsoid
everest1830Ellipsoid = mkEllipsoid (SemiMajMinAxises 6377276.34518 6356075.41511)

fischer1960Ellipsoid :: Ellipsoid
fischer1960Ellipsoid = mkEllipsoid (SemiMajMinAxises 6378166.0 6356784.284)

fischer1968Ellipsoid :: Ellipsoid
fischer1968Ellipsoid = mkEllipsoid (SemiMajMinAxises 6378150.0 6356768.337)

grs67Ellipsoid :: Ellipsoid
grs67Ellipsoid = mkEllipsoid (SemiMajMinAxises 6378160.0 6356774.51609)

grs75Ellipsoid :: Ellipsoid
grs75Ellipsoid = mkEllipsoid (SemiMajMinAxises 6378140.0 6356755.288)

grs80Ellipsoid :: Ellipsoid
grs80Ellipsoid = mkEllipsoid (SemiMajMinAxises 6378137 6356752.3141)

hayford1910Ellipsoid :: Ellipsoid
hayford1910Ellipsoid = mkEllipsoid (SemiMajMinAxises 6378388.0 6356911.946)

helmert1906Ellipsoid :: Ellipsoid
helmert1906Ellipsoid = mkEllipsoid (SemiMajMinAxises 6378200.0 6356818.17)

hough1956Ellipsoid :: Ellipsoid
hough1956Ellipsoid = mkEllipsoid (SemiMajMinAxises 6378270.0 6356794.34)

iers1989Ellipsoid :: Ellipsoid
iers1989Ellipsoid = mkEllipsoid (SemiMajMinAxises 6378136.0 6356751.302)

internationalEllipsoid :: Ellipsoid
internationalEllipsoid = mkEllipsoid (SemiMajMinAxises 6378388 6356911.9462)

krassovsky1940Ellipsoid :: Ellipsoid
krassovsky1940Ellipsoid = mkEllipsoid (SemiMajMinAxises 6378245.0 6356863.019)

modifiedAiryEllipsoid :: Ellipsoid
modifiedAiryEllipsoid = mkEllipsoid (SemiMajAxisEccentricitySquared 6377340.189 0.00667054015)

modifiedEverestEllipsoid :: Ellipsoid
modifiedEverestEllipsoid = mkEllipsoid (SemiMajMinAxises 6377304.063 6356103.039)

newInternational1967Ellipsoid :: Ellipsoid
newInternational1967Ellipsoid = mkEllipsoid (SemiMajMinAxises 6378157.5 6356772.2)

southAmerican1969Ellipsoid :: Ellipsoid
southAmerican1969Ellipsoid = mkEllipsoid (SemiMajMinAxises 6378160.0 6356774.7192)

wgs60Ellipsoid :: Ellipsoid
wgs60Ellipsoid = mkEllipsoid (SemiMajMinAxises 6378165.0 6356783.287)

wgs66Ellipsoid :: Ellipsoid
wgs66Ellipsoid = mkEllipsoid (SemiMajMinAxises 6378145.0 6356759.770)

wgs72Ellipsoid :: Ellipsoid
wgs72Ellipsoid = mkEllipsoid (SemiMajMinAxises 6378135 6356750.5)

wgs84Ellipsoid :: Ellipsoid
wgs84Ellipsoid = mkEllipsoid (SemiMajMinAxises 6378137 6356752.3142)
