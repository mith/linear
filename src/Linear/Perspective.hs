{-# LANGUAGE RankNTypes #-}
---------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2014 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Common perspective transformation matrices.
---------------------------------------------------------------------------

module Linear.Perspective
  ( lookAt
  , perspective
  , infinitePerspective
  , ortho
  , _Project
  ) where

import Control.Lens hiding (index)
import Linear.V3
import Linear.V4
import Linear.Matrix
import Linear.Epsilon
import Linear.Metric

-- | Build a look at view matrix
lookAt
  :: (Epsilon a, Floating a)
  => V3 a -- ^ Eye
  -> V3 a -- ^ Center
  -> V3 a -- ^ Up
  -> M44 a
lookAt eye center up =
  V4 (V4 (xa^._x)  (xa^._y)  (xa^._z)  xd)
     (V4 (ya^._x)  (ya^._y)  (ya^._z)  yd)
     (V4 (-za^._x) (-za^._y) (-za^._z) zd)
     (V4 0         0         0          1)
  where za = normalize $ center - eye
        xa = normalize $ cross za up
        ya = cross xa za
        xd = -dot xa eye
        yd = -dot ya eye
        zd = dot za eye

-- | Build a matrix for a symmetric perspective-view frustum
perspective
  :: Floating a
  => a -- ^ Vertical FOV in radians
  -> a -- ^ Aspect ratio
  -> a -- ^ Near plane
  -> a -- ^ Far plane
  -> M44 a
perspective fovy aspect near far =
  V4 (V4 x 0 0    0)
     (V4 0 y 0    0)
     (V4 0 0 z    w)
     (V4 0 0 (-1) 0)
  where tanHalfFovy = tan $ fovy / 2
        x = 1 / (aspect * tanHalfFovy)
        y = 1 / tanHalfFovy
        z = -(far + near) / (far - near)
        w = -(2 * far * near) / (far - near)

-- | Build a matrix for a symmetric perspective-view frustum with a far plane at infinite
infinitePerspective
  :: Floating a
  => a -- ^ Vertical FOV an radians
  -> a -- ^ Aspect Ratio
  -> a -- ^ Near plane
  -> M44 a
infinitePerspective fovy aspect near =
  V4 (V4 x 0 0    0)
     (V4 0 y 0    0)
     (V4 0 0 (-1) w)
     (V4 0 0 (-1) 0)
  where range  = (tan (fovy / 2)) * near
        left   = -range * aspect
        right  = range * aspect
        bottom = -range
        top    = range
        x = (2 * near) / (right - left)
        y = (2 * near) / (top - bottom)
        w = -2 * near

-- | Build an orthographic perspective matrix from 6 clipping planes
ortho
  :: Floating a
  => a -- ^ Left
  -> a -- ^ Right
  -> a -- ^ Bottom
  -> a -- ^ Top
  -> a -- ^ Near
  -> a -- ^ Far
  -> M44 a
ortho left right bottom top near far =
  V4 (V4 (2 / a) 0       0        (negate ((right + left) / a)))
     (V4 0       (2 / b) 0        (negate ((top + bottom) / b)))
     (V4 0       0       (-2 / c) ((far + near) / c))
     (V4 0       0       0        1)
  where a = right - left
        b = top - bottom
        c = far - near
            
-- | Map the specified object coordinates into window coordinates
_Project
  :: (Epsilon a, Floating a)
  => M44 a
  -> M44 a
  -> V4 a
  -> Iso' (V3 a) (V3 a)
_Project model proj vwp =
  iso fromWindow toWindow
  where fromWindow (V3 x y z) = let Just inv = inv44 (proj * model)
                                    ndc = V3 ((x - vwp^._x) / vwp^._z) 
                                             ((y - vwp^._y) / vwp^._w)
                                             z
                                    clip = point ndc * 2 - 1                 
                                    obj = inv !* clip
                                    in normalizePoint obj
        toWindow obj = let clip = proj !*! model !* point obj
                           ndc  = (normalizePoint clip) * 0.5 + 0.5
                       in V3 (ndc^._x * vwp^._z + vwp^._x)
                             (ndc^._y * vwp^._w + vwp^._y)
                             (ndc^._z)
