{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE NoImplicitPrelude #-}
module ClosedMonoidalCategory where

import Data.Category
import Data.Category.Functor
import Data.Category.Monoidal
import Data.Category.CartesianClosed
import Data.Category.Limit

class TensorProduct f => ClosedMonoidalCategory f where
  type MonoidalExponential f y z :: *
  
  curryMon   :: (Cod f ~ k) => f -> Obj k x -> Obj k a -> Obj k b 
                         -> (f :% (x, a)) `k` b
                         -> x `k` (MonoidalExponential f a b)
  uncurryMon :: (Cod f ~ k) => f -> Obj k x -> Obj k a -> Obj k b 
                         -> x `k` (MonoidalExponential f a b)
                         -> (f :% (x, a)) `k` b
  eval  :: (Cod f ~ k) => f -> Obj k a -> Obj k b
                       -> (f :% (MonoidalExponential f a b, a)) `k` b

instance (CartesianClosed k) => ClosedMonoidalCategory (ProductFunctor k) where
  type MonoidalExponential (ProductFunctor k) y z = Exponential k y z
  curryMon   ProductFunctor = curry
  uncurryMon ProductFunctor = uncurry
  eval       ProductFunctor = apply
