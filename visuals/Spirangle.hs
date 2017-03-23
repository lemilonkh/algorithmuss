-- Spirangle.hs
-- copied from https://github.com/sleexyz/hylogen-fun/blob/master/Spirangle.hs

{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Spirangle where

import Util



output = toProgram (spirangle osc1)

spirangle draw = rgba where
  rgba = (line)
   & (over (b2a bb))
   & (clamp 0 1)

  bb = q (texture2D backBuffer) uvN

  q x = x
    & lmap (view norm)
    & lmap (rot ((negate 2 * pi /3)  + muchless sin time ))
    & lmap (*1.1)
    & rmap (clamp 0 1)

  mask :: (Veccable n) => Vec n -> Vec n
  mask x = (x `gt` 0) ? (0, 1)

  gate :: (Veccable n) => Vec n -> Vec n -> Vec n -> Vec n
  gate s e x = ((x `geq` s) * (x `lt` e)) ? (1, 0)

  line :: Vec4
  line =  vec4 (v, v, v, 1)
  v = 1
    & (*(gate (-0.35) (-0.33) (y_ uvN)))
    & (*(gate (-0.55) (0.65) (x_ uvN)))
    & (*draw)
