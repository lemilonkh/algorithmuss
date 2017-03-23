{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Poop where
import Hylogen.WithHylide

rot :: Vec1 -> Vec2 -> Vec2
rot phi a = vec2 ( cos phi * (x_ a)
                   + sin phi * (y_ a)
                 , (-1) * sin phi * (x_ a)
                   + cos phi * (y_ a)
                 )

rep :: forall n. Veccable n => Vec n ->  Vec n -> Vec n
rep c p = mod_  p c - 0.5 * c

mirrorX v = vec2 (abs (x_ v), y_ v)
mirrorY v = vec2 (x_ v, abs(y_ v))

output = toProgram $ vec4 (r, g, b, 0.01) + bb
  where
    r = v
      & rep 2
      & (*0.1)
      & (/(x_ bb))
      & negate
    g = v
      & rep 2
      & (*0.01)
      & (/(x_ bb))
      & negate
    b = v
      & rep 2
      & (*0.1)
      & (/(x_ bb))
      & negate
    v =  (sin (atan ((y_ uvN' / x_ uvN' - x_ audio * len uvN')) / x_ uvN' - w_ audio)) + z_ audio
    uvN' = uvN
      & mirrorX
      & mirrorY
      & (rot (pi/2))
      -- & (\x -> x - rep (copy $ w_ audio) x)
      & (^*(y_ audio * 5))
      & (+(negate (mouse)))
    bb = texture2D backBuffer bbN
    bbN = uvN
      -- & (\x -> x - rep (copy $ w_ audio) x)
      & (*1.01)
      & mirrorX
      & mirrorY
      & rot (pi/2)
      & (\x -> x * 0.5 + 0.5)
