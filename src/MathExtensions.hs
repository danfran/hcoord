module MathExtensions where

toRadians :: Double -> Double
toRadians d = d / 180 * pi

toDegrees :: Double -> Double
toDegrees a = a * 180.0 / pi

sinSquared, sinCubed :: Double -> Double
sinSquared phi = sin phi ** 2
sinCubed phi = sin phi ** 3

cosSquared, cosCubed :: Double -> Double
cosSquared phi = cos phi ** 2
cosCubed phi = cos phi ** 3

tanSquared :: Double -> Double
tanSquared phi = tan phi ** 2

sec :: Double -> Double
sec x = 1 / cos x
