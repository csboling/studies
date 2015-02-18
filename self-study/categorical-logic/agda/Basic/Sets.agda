module Basic.Sets where

data ℕ : Set where
  zero : ℕ
  suc  : ℕ → ℕ

data Vec (A : Set) : ℕ -> Set where
  []   : Vec A zero
  _::_ : {n : ℕ} ->
         A -> Vec A n
         -> Vec A (suc n)

Matrix : Set ->  ℕ -> ℕ -> Set
Matrix A m n = Vec (Vec A n) m

vec : {n : ℕ}{A : Set} -> A -> Vec A n
vec {zero}  x = []
vec {suc n} x = x :: vec {n} x

infixl 90 _$_
_$_ : {n : ℕ}{A B : Set} ->
        Vec (A -> B) n -> Vec A n -> Vec B n
[] $ []               = []
(f :: fs) $ (x :: xs) = f x :: (fs $ xs)

transpose : ∀ {A m n} -> Matrix A m n -> Matrix A n m
transpose []          = vec []
transpose (xs :: xss) = {!!}
