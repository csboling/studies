{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE EmptyDataDecls            #-}

import Control.Arrow hiding ((<+>))
import Control.Monad
import Math.Algebras.VectorSpace
import Math.Algebras.TensorProduct

import GHC.Exts (Constraint)

newtype Id a = Id { unId :: a }

class Category 
        (obj :: * -> *)
        (hom :: * -> * -> *)
      where
  type Obj obj hom :: (* -> *)
  type Obj obj hom = obj
  type Hom obj hom :: (* -> * -> *)
  type Hom obj hom = hom

  id  :: hom (obj a) (obj a)
  (∘) :: hom (obj b) (obj c) 
      -> hom (obj a) (obj b)
      -> hom (obj a) (obj c)
instance Category Id (->) where
  id  = Prelude.id
  (∘) = (.)

infixl 7 ∏
class (Category obj hom) => CartesianCategory obj hom prod where
  type Multipliable obj t :: Constraint
  type Multipliable obj t = ()
  projL :: (Multipliable obj x, Multipliable obj y) =>
           obj (x `prod` y) -> obj x
  projR :: (Multipliable obj x, Multipliable obj y) =>
           obj (x `prod` y) -> obj y
  (∏)   :: (Multipliable obj x, Multipliable obj y, Multipliable obj z) =>
           (obj z `hom` obj x)
        -> (obj z `hom` obj y)
        -> (obj z `hom` obj (x `prod` y))
instance CartesianCategory Id (->) (,) where
  projL = Id . fst . unId
  projR = Id . snd . unId
  f ∏ g = Id . (unId . f . Id &&& unId . g . Id) . unId

class (CartesianCategory obj hom prod) =>
      CartesianClosedCategory obj hom prod exp where
  curry    :: (obj (x `prod` y) `hom` obj z)
           -> (obj x `hom` obj (exp z y))
  uncurry  :: (obj x `hom` obj (exp z y))
           -> (obj (x `prod` y) `hom` obj z)
  eval     :: (obj (exp z y) `prod` obj y) `hom` (obj z)

class (Category obj hom) =>
      MonoidalCategory obj hom mon where


-- the category of R-modules with linear maps as morphisms
-- linearity is not enforced (yet)
-- these instances are thus sloppy since the Hom
-- (->) should really be a LinearMap type
instance (Eq r, Num r) => Category (Vect r) (->) where
  id  = Prelude.id
  (∘) = (.)

-- need a newtype wrapper for partial application
newtype SumSpace a b = SumSpace { unSumSpace :: DSum a b }

-- only vector spaces with orderable basis types are
-- instances due to constraints on the library functions
-- Although this product (⊕) is cartesian, Vect r is not a
-- cartesian closed category because this product is not
-- left-adjoint to the internal Hom.
instance (Eq r, Num r) =>
         CartesianCategory
           (Vect r) (->) SumSpace where
  type Multipliable (Vect r) t = Ord t
  projL = p1 . liftM unSumSpace
  projR = p2 . liftM unSumSpace
  f ∏ g = liftM SumSpace . (f `prodf` g)

data ExpBasis a b = ExpBasis { unExpBasis :: Tensor a (Dual b) }
instance (Eq r, Num r) =>
         CartesianClosedCategory
           (Vect r) (->) SumSpace ExpBasis where

