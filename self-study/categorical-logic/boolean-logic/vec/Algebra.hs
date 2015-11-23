{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE DataKinds, TypeOperators, RankNTypes,
             FlexibleInstances, FlexibleContexts, UndecidableInstances,
             GADTs, KindSignatures, InstanceSigs,
             MultiParamTypeClasses, FunctionalDependencies,
             DeriveDataTypeable #-}
import AltLogics
import Data.Typeable

type a :∈: s = s a

data a :⊆: b where
  Subset :: (forall x. (x :∈: a) -> (x :∈: b)) -> a :⊆: b

newtype Falsity = Falsity { elimFalsity :: forall a. a }
  deriving Typeable
data Truth = TruthProof
class Fact a where
  auto :: a
instance Fact Truth where auto = TruthProof

data Ø :: * -> * where
  Empty :: (forall x. x) -> Ø a
instance Fact (Ø a -> b)
instance Fact (Ø :⊆: a)

instance (Fact (a :⊆: b)) => Logical (a x) where
  bot = Empty undefined
