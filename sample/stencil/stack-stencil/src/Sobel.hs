module Sobel
  ( sobelX
  , sobelY
  , sobelOperator
  , sobelOperatorUnfused
  , bar
  ) where

import Data.Massiv.Array as A
import Data.Default

sobelX :: Num e => Stencil Ix2 e e
sobelX =
  makeConvolutionStencil (3 :. 3) (1 :. 1) $
  \ f -> f (-1 :. -1)   1  .
         f ( 0 :. -1)   2  .
         f ( 1 :. -1)   1  .
         f (-1 :.  1) (-1) .
         f ( 0 :.  1) (-2) .
         f ( 1 :.  1) (-1)
{-# INLINE sobelX #-}


sobelY :: Num e => Stencil Ix2 e e
sobelY =
  makeConvolutionStencil (3 :. 3) (1 :. 1) $
  \ f -> f (-1 :. -1)   1  .
         f (-1 :.  0)   2  .
         f (-1 :.  1)   1  .
         f ( 1 :. -1) (-1) .
         f ( 1 :.  0) (-2) .
         f ( 1 :.  1) (-1)
{-# INLINE sobelY #-}


sobelOperator :: (Default b, Floating b) => Stencil Ix2 b b
sobelOperator = sqrt (sX + sY)
  where
    !sX = fmap (^ (2 :: Int)) sobelX
    !sY = fmap (^ (2 :: Int)) sobelY
{-# INLINE sobelOperator #-}

sobelOperatorUnfused
  :: (Unbox b, Eq b, Floating b)
  => Border b -> Array U Ix2 b -> Array U Ix2 b
sobelOperatorUnfused b arr = computeAs U $ A.map sqrt (A.zipWith (+) sX sY)
  where
    !sX = A.map (^ (2 :: Int)) (computeAs U $ mapStencil b sobelX arr)
    !sY = A.map (^ (2 :: Int)) (computeAs U $ mapStencil b sobelY arr)
{-# INLINE sobelOperatorUnfused #-}


bar :: IO ()
bar = do
  let arr = makeArrayR U Par (5 :. 6) ((fromIntegral :: Int -> Double) . toLinearIndex (5 :. 6))
  print $ A.sum $ computeAs U (mapStencil Edge sobelX arr)
  print $ A.sum $ computeAs U (mapStencil Edge sobelY arr)
  print $ A.sum $ computeAs U (mapStencil Edge sobelOperator arr)
  print $ A.sum $ sobelOperatorUnfused Edge arr
