{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}
import AltLogics
import Control.Arrow
import Data.Tuple (swap)

import Data.Category (Obj, Op)
import Data.Category.Functor hiding (swap)
import Data.Category.CartesianClosed hiding (curry, uncurry)
import ClosedMonoidalCategory

import Numeric.LinearAlgebra (Matrix)

cross_couple ::
  (a -> b -> c) -> (a' -> b' -> c') ->
  ((a, a'), (b, b')) -> (c, c')
cross_couple f g = uncurry $ f *** g >>> uncurry (***)

sr :: (Logical a) =>
  a -> a -> (a, a)
sr = curry . loop $ (id &&& swap) . cross_couple nor nor
   where nor = neg . disj

gated_sr :: (Logical a) =>
  a -> a -> a -> (a, a)
gated_sr e r s = sr (e `conj` r) (e `conj` s)

safe_sr :: (Logical a) =>
  a -> a -> (a, a)
safe_sr r s = gated_sr (s `nand` r) r s

sr_mat :: Matrix Double
sr_mat = undy network
       where network :: Matrix Double
                     -> Matrix Double
                     -> Matrix Double
             network = ((.).(.)) fst safe_sr

instance (CartesianClosed k, CartesianClosed (Op k)) => Logical (Obj k a) where
