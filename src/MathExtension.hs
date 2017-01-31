module MathExtension where

toRadians :: Double -> Double
toRadians d = d / 180 * pi

toDegrees :: Double -> Double
toDegrees a = a * 180.0 / pi

sinSquared :: Double -> Double
sinSquared phi = sin phi ** 2

cosSquared :: Double -> Double
cosSquared phi = cos phi ** 2

tanSquared :: Double -> Double
tanSquared phi = tan phi ** 2