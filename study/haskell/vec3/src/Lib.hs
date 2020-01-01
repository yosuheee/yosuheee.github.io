module Lib
  ( Vec3(..)
  , (.|.)
  , (*|*)
  , invert
  , normalize
  , scalar
  ) where

data Vec3 = Vec3 { x :: Double, y :: Double, z :: Double } deriving (Show, Eq)

instance Num (Vec3) where
  Vec3 a b c + Vec3 d e f = Vec3 (a + d) (b + e) (c + f)
  Vec3 a b c - Vec3 d e f = Vec3 (a - d) (b - e) (c - f)
  Vec3 a b c * Vec3 d e f = Vec3 (a * d) (b * e) (c * f)
  abs (Vec3 a b c) = Vec3 (abs a) (abs b) (abs c)
  signum (Vec3 a b c) = Vec3 (signum a) (signum b) (signum c)
  fromInteger i = Vec3 (fromInteger i) (fromInteger i) (fromInteger i)

(.|.) :: Vec3 -> Vec3 -> Double
v .|. u = (x v) * (x u) + (y v) * (y u) + (z v) * (z u)

(*|*) :: Vec3 -> Vec3 -> Vec3
v *|* u =
  Vec3
    ((y v) * (z u) - (z v) * (y u))
    ((z v) * (x u) - (x v) * (z u))
    ((x v) * (y u) - (y v) * (x u))

infixl 7 .|.
infixl 7 *|*

invert :: Vec3 -> Vec3
invert v = Vec3 (-(x v)) (-(y v)) (-(z v))

normalize :: Vec3 -> Vec3
normalize v = Vec3 (x v / l) (y v / l) (z v / l)
  where l = scalar v

scalar :: Vec3 -> Double
scalar v = sqrt $ (x v) ** 2 + (y v) ** 2 + (z v) ** 2
