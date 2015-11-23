{-# LANGUAGE UnicodeSyntax,
             TypeFamilies, ScopedTypeVariables,
             FlexibleContexts, FlexibleInstances, UndecidableInstances,
             NoMonomorphismRestriction #-}
{- A typeclass for elementary logics and some instances,
 - notably vector logic, after the work of E. Mizraji.
 - (See for instance Mizraji '07 -- "Vector Logic", Journal of Logic and Computation
 -}

module VecLogic
  ( --Logical(..)
  )
where

--import Numeric.LinearAlgebra hiding (conj)
import Control.Applicative
import Control.Monad as M
import Control.Arrow

import Data.Tensor
import Data.Tensor.LinearAlgebra
import Data.Cardinal
import Data.TypeAlgebra

import Data.Boolean

import Data.List (unfoldr)
import Data.Bits

toBits :: (Num n, Ord n, Bits n) => n -> [Bool]
toBits n = reverse $ unfoldr (getBit n) 1
  where getBit x b = if   b > x 
                     then Nothing
                     else Just (x .&. b /= 0, 2*b)
fromBool :: (Boolean b) => Bool -> b
fromBool True  = true
fromBool False = false

--combinePairs :: (a -> a -> a) -> Int -> [a] -> [a]
--combinePairs f n xs = map (foldl1 f) $ replicateM n xs


vectorize :: (Tensor Bool -> Bool) -> Tensor v -> v
vectorize f xs = foldl1 (.+.) . map term [1 .. 2^(length xs)-1]
    where --term :: Int -> Tensor v :⊗: v
          term = undefined


fromTT tt xs = undefined


-- Given an inner product space V, any W = ⊗²ᴺV produces a Boolean algebra.
instance ( n ~ Prod C2 n' 
         , VectorSpace v
         , DotProduct  v
         , Num t
         )
  => Boolean (Product n (v t) (v t))
--  notB x  = fromTT [] [x]--[true, false] [x]
--  x &&* y = fromTT [false, false, false, true] [x, y]
--  x ||* y = fromTT [false, true,  true,  true] [x, y]

