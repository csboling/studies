{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE LiberalTypeSynonyms    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE NoImplicitPrelude      #-}

import GHC.Exts (Constraint)

import Data.Category (Category, Obj, Op)
import Data.Category.Functor hiding (swap)
import Data.Category.Limit
import Data.Category.CartesianClosed hiding (curry, uncurry)
import ClosedMonoidalCategory

class Logical a where
  type TopConstraint a b :: Constraint
  type TopConstraint a b = b
  type BotConstraint a b :: Constraint
  type BotConstraint a b = b
  type AndConstraint a b :: Constraint
  type AndConstraint a b = b
  type ImpliesConstraint a b :: Constraint
  type ImpliesConstraint a b = b
  top     :: TopConstraint a b => b
  bot     :: BotConstraint a b => b
  and     :: AndConstraint a b => a -> a -> b
  implies :: ImpliesConstraint a b => a -> a -> b

  not :: ImpliesConstraint a b => a -> b
  not = (`implies` bot)

instance (CartesianClosed k, HasInitialObject k) => 
         Logical (k a b) where
  type TopConstraint (k a b) c = c ~ Obj k (TerminalObject k)
  type BotConstraint (k a b) c = c ~ Obj k (InitialObject  k)
  type AndConstraint (k a b) c = c ~ k a (BinaryProduct k b b)
  type ImpliesConstraint (k a b) c = c ~ Exponential k a b
  top     = terminalObject
  bot     = initialObject
  and     = (&&&)
--  implies = 

  
