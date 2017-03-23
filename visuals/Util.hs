-- Util.hs
-- copied from https://github.com/sleexyz/hylogen-fun/blob/master/Util.hs

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

module Util (
  module Hylogen.WithHylide,
  module Data.Profunctor,
  module Util,
) where

import Hylogen.WithHylide hiding (uv)
import Data.Profunctor
import Hylogen.Expr


type Optic p s t a b = p a b -> p s t
type Optic' p a b = Optic p a b a b
type Iso s t a b = forall p. (Profunctor p) => Optic p s t a b
type Iso' a b = Iso a b a b
type Fold r s t a b = Optic (Forget r) s t a b
type Getter s t a b = Fold a s t a b
view :: forall s t a b. Getter s t a b -> s -> a
view l = runForget (l (Forget id))

type Image = Vec2 -> Vec4

(?) :: (ToGLSLType a) => Booly -> (Expr a, Expr a) -> Expr a
(?) c (a, b) = sel c a b


bw :: Vec1 -> Vec4
bw v = vec4 (v, v, v, 1)

rot :: Vec1 -> Vec2 -> Vec2
rot phi a = vec2 ( cos phi * (x_ a)
                   + sin phi * (y_ a)
                 , (-1) * sin phi * (x_ a)
                   + cos phi * (y_ a)
                 )

over :: Vec4 -> Vec4 -> Vec4
over x y = mix a x' y'
  where
    x' = clamp 0 1 x
    y' = clamp 0 1 y
    a = 1 - w_ x'

less :: (Floating a) => Optic' (->) a a
less = dimap (id) (*0.1)

muchless :: (Floating a) => Optic' (->) a a
muchless = dimap (id) (*0.01)

norm :: (Floating a) => Iso' a a
norm = dimap (\x -> x * 0.5 + 0.5) (\x -> x * 2 - 1)

hsv :: Optic' (->) (Vec4) (Vec4)
hsv = dimap rgb2hsv hsv2rgb

rep :: forall n. Veccable n => Vec n ->  Vec n -> Vec n
rep c p = mod_  p c - 0.5 * c

-- rgbF :: Vec1 -> (Vec2 -> Vec4) -> (Vec2 -> Vec4)
rgbF :: Vec1 -> Optic' (->) Vec2 Vec4
rgbF offset q pos = vec4 (r, g, b, a)
  where
    r = q (pos + copy offset) & x_
    g = q pos & y_
    b = q (pos - copy offset) & z_
    a = q pos & w_

phi uv = atan (y_ uv/x_ uv)

scale :: Floating a => a -> Iso' a a
scale s = dimap (*s) (/s)

mirrorX :: Vec2 -> Vec2
mirrorX v = vec2 (abs $ x_ v, y_ v)


mirrorY :: Vec2 -> Vec2
mirrorY v = vec2 (x_ v, abs $ y_ v)

wiggle :: Vec2 -> Vec2
wiggle = rot (cos (time * 0.8) * 0.1)

desat = hsv $ modY (*0.9)

polar :: Iso' Vec2 Vec2
polar = dimap c2p p2c where
  c2p :: Vec2 -> Vec2
  c2p v = vec2 (len v, phi v)
  p2c :: Vec2 -> Vec2
  p2c v = x_  v *^ vec2 (cos (y_ v), sin (y_ v))


-- fixme: make polymorphic
modX :: (Vec1 -> Vec1) -> (Vec4 -> Vec4)
modX f v = vec4 (f (x_ v), y_ v, z_ v, w_ v)
modY :: (Vec1 -> Vec1) -> (Vec4 -> Vec4)
modY f v = vec4 (x_ v, f (y_ v), z_ v, w_ v)
modZ :: (Vec1 -> Vec1) -> (Vec4 -> Vec4)
modZ f v = vec4 (x_ v, y_ v, f (z_ v), w_ v)
modW :: (Vec1 -> Vec1) -> (Vec4 -> Vec4)
modW f v = vec4 (x_ v, y_ v, z_ v, f (w_ v))

beat' :: Vec1
beat' = 2 * pi * beat

rand :: Vec1 -> Vec1
rand x = x
  & sin
  & (*43758.543123)
  & fract

rand2 :: Vec2 -> Vec2
rand2 x = fract(sin(x <.> vec2(12.9898, 4.1414))) *^ 43758.5453

b2a :: Vec4 -> Vec4
b2a x = vec4 (xyz_ x, x_ x)

gate :: (Veccable n) => Vec n -> Vec n -> Vec n -> Vec n
gate s e x = ((x `geq` s) * (x `lt` e)) ? (1, 0)

diff :: Floating a => (a -> a) -> (a -> a)
diff f x = (f (x - delta) - f (x + delta))/(delta * 2) where
  delta = 0.00001


grad :: (Vec2 -> Vec1) -> (Vec2 -> Vec2)
grad f x = vec2 (dx, dy) where
  dx = f (vec2 (x_ x - delta, y_ x)) - f (vec2 (x_ x + delta, y_ x))/delta
  dy = f (vec2 (x_ x, y_ x - delta)) - f (vec2 (x_ x, y_ x - delta))/delta
  delta = 1

bpf pos delta = hsv (modX (clamp (pos - delta) (pos + delta)))
