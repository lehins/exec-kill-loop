module Average
  ( average3x3Filter
  , average3x3FilterConv
  , foo
  ) where

import Data.Massiv.Array as A
import Data.Default

average3x3Filter :: (Default a, Fractional a) => Stencil Ix2 a a
average3x3Filter = makeStencil (3 :. 3) (1 :. 1) $ \ get ->
  (  get (-1 :. -1) + get (-1 :. 0) + get (-1 :. 1) +
     get ( 0 :. -1) + get ( 0 :. 0) + get ( 0 :. 1) +
     get ( 1 :. -1) + get ( 1 :. 0) + get ( 1 :. 1)   ) / 9
{-# INLINE average3x3Filter #-}


average3x3FilterConv :: (Default a, Fractional a) => Stencil Ix2 a a
average3x3FilterConv = let _9th = 1/9 in
  makeConvolutionStencil (3 :. 3) (1 :. 1) $ \ get ->
  get (-1 :. -1) _9th . get (-1 :. 0) _9th . get (-1 :. 1) _9th .
  get ( 0 :. -1) _9th . get ( 0 :. 0) _9th . get ( 0 :. 1) _9th .
  get ( 1 :. -1) _9th . get ( 1 :. 0) _9th . get ( 1 :. 1) _9th
{-# INLINE average3x3FilterConv #-}

foo :: IO ()
foo = do
  let arr = makeArrayR U Par (5 :. 6) ((fromIntegral :: Int -> Double) . toLinearIndex (5 :. 6))
  print $ A.sum $ computeAs U (mapStencil Edge average3x3Filter arr)
  print $ A.sum $ computeAs U (mapStencil Edge average3x3FilterConv arr)
