{-# LANGUAGE TypeFamilies, ScopedTypeVariables,
             FlexibleContexts, FlexibleInstances, UndecidableInstances,
             NoMonomorphismRestriction #-}
{- A typeclass for elementary logics and some instances,
 - notably vector logic, after the work of E. Mizraji.
 - (See for instance Mizraji '07 -- "Vector Logic", Journal of Logic and Computation
 -}

module AltLogics
  ( Logical(..)
  )
where

import Prelude as P

import Numeric.LinearAlgebra hiding (conj)
import Data.Bifunctor (bimap)
import Data.List
import Control.Applicative
import Control.Monad as M
import Control.Arrow


result   = (.)
argument = flip (.)

mapPairs f  = f <$> [False, True] <*> [False, True]
boolPairs   = mapPairs (,)
logicPairs = mapPairs . curry $ uncurry (,) >>> fromBool *** fromBool

mapCombinations f = concat . (zipWith (map . f) <*> inits)
majority = foldl1 disj . mapCombinations conj
threshold k = (>= k) . length . filter (== top)

-- A list of functions computing each 2-input minterm
minterms2 :: (Logical a) => [a -> a -> a]
minterms2 = map curry $ map makeTerm boolPairs
         where makeTerm = (result . result) (uncurry conj) makeVar
               makeVar  = uncurry bimap . M.join (***) toFuncs
               toFuncs False = neg
               toFuncs True  = id

class Logical a where
  type MonadicOp a
  type DyadicOp  a
  top       :: a
  bot       :: a
  fromBool :: Bool -> a
  fromBool x = if x then top else bot

  mon      :: MonadicOp a -> a -> a
  unmon    :: (a -> a) -> MonadicOp a
  dy       :: DyadicOp  a -> a -> a -> a
  undy     :: (a -> a -> a) -> DyadicOp a
  dyFromTT :: [Bool] -> a -> a -> a
  dyFromTT tt = foldl1 disj .
                map snd . filter fst $
                zip tt minterms2

  {- minimal: top, bot,
              mon, dy, undy,
              either:
                 nand
              OR neg, one of {conj, disj, implies}
   -}

  -- Logical connectives (gates)
  -- The Sheffer stroke (NAND), a universal gate
  nand :: a -> a -> a
  nand    = neg . conj
  -- Unary negation (NOT)
  neg     :: a -> a
  neg     = M.join nand
  -- Conjunction (AND)
  conj    :: a -> a -> a
  conj    = neg . nand
  -- Disjunction (OR) -
  disj    :: a -> a -> a
  disj x y = (neg x) `nand` (neg y)
  -- The Peirce connective (exclusive or)
  xor     :: a -> a -> a
  xor      = disj `conj` nand
  -- Material implication, a universal gate
  implies :: a -> a -> a
  p `implies` q = (neg p) `disj` q


instance (Logical b) => Logical (a -> b) where
  type MonadicOp (a -> b) = (a -> b)
                         ->  a -> b
  type DyadicOp  (a -> b) = (a -> b)
                         -> (a -> b)
                         ->  a -> b

  mon   = id
  unmon = id
  dy    = id
  undy  = id

  top = const top
  bot = const bot

  nand = liftA2 nand

instance Logical P.Bool where
  type MonadicOp P.Bool = P.Bool -> P.Bool
  type DyadicOp  P.Bool = P.Bool -> P.Bool -> P.Bool
  top    = True
  bot    = False

  mon   = id
  unmon = id
  dy    = id
  undy  = id

  nand x y = not (x && y)

instance
  (Num t, Num (Matrix t), Num (Vector t),
   Product t, Element t,
   Container Vector t) =>
    Logical (Matrix t) where
  type MonadicOp (Matrix t) = Matrix t
  type DyadicOp  (Matrix t) = Matrix t
  top = takeColumns 1 $ ident 2
  bot = dropColumns 1 $ ident 2

  mon         = (<>)
  unmon f     = sum . map (f &&& trans >>> uncurry (<>)) $ [bot, top]
  dy    m a b = m <> (a `kronecker` b)
  undy f      = sum . map combine $ logicPairs
              where combine = (id &&& id >>> chain (uncurry f))

  neg = mon $ n <> (trans s) + s <> (trans n)
      where s = top :: Matrix t
            n = bot :: Matrix t
  dyFromTT = dy . sum . map (chain fromBool) . flip zip logicPairs
  nand = dyFromTT [True, True, True, False]

chain :: (Product t) => (a -> Matrix t)
                     -> (a, (Matrix t, Matrix t))
                     -> Matrix t
chain f = f *** (uncurry kronecker >>> trans) >>> uncurry (<>)

s = top :: Matrix Float
n = bot :: Matrix Float
