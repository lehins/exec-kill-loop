{-# LANGUAGE BangPatterns #-}
module Main where

import Sobel
import Average

import Data.Massiv.Array as A
import Data.Default


main :: IO ()
main = do
  let arr = makeArrayR U Par (5 :. 6) ((fromIntegral :: Int -> Double) . toLinearIndex (5 :. 6))
  print $ A.sum $ computeAs U (mapStencil Edge sobelX arr)
  print $ A.sum $ computeAs U (mapStencil Edge sobelY arr)
  print $ A.sum $ computeAs U (mapStencil Edge sobelOperator arr)
  print $ A.sum $ sobelOperatorUnfused Edge arr
  bar
  print $ A.sum $ computeAs U (mapStencil Edge average3x3Filter arr)
  print $ A.sum $ computeAs U (mapStencil Edge average3x3FilterConv arr)
  foo
