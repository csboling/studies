{-# LANGUAGE ExistentialQuantification #-}
module Restrict where

-- defined in a library I want to use
data ExistingType t = ExistingType t
libraryFunction :: (Eq t) => ExistingType t -> ExistingType t
libraryFunction = undefined

-- defined by me
class MyClass f where
  member :: f a -> f a

-- broken
-- instance MyClass ExistingType where
--   member = libraryFunction

-- possible solution?
data EqArgET t = forall t. Eq t => EqArgET (ExistingType t)
instance MyClass EqArgET where
  member (EqArgET t) = EqArgET $ libraryFunction t
