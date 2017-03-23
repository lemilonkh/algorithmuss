-- HelloHylogen.hs
-- (c) 2017 by Milan Gruner
-- Adapted from Hylogen example code

module HelloHylogen where
import Hylogen.WithHylide

output :: Program
output = toProgram color

color :: Vec4
color = vec4(a*0.3, a*0.9, a*0.6, 1)
  where
    k = 40
    f = (*k) . sin . cos . tan . (/k)
    a = sum [
      cos(x_ uvN * f time + x_ mouse),
      sin(y_ uvN * f time + y_ mouse),
      exp(x_ uvN * f time + x_ mouse),
      exp(y_ uvN * f time + y_ mouse)]
