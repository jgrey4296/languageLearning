module NumUtils where

guardFloat :: (Float -> Float) -> Float -> Float
guardFloat f x
  | x < 0.0 = 0.0
  | x > 1.0 = 1.0
  | otherwise = f x

decrease :: Float -> Float
decrease x = 1.0 - x

powerLaw :: Float -> Float
powerLaw x = x**0.5

powerCurve :: Float -> Float
powerCurve x = x**2.0
