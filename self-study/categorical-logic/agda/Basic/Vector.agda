module Basic.Vector where

open import Basic.Sets
open import Basic.Nat

infixl 20 _::_
data Vec (A : Set) : ℕ → Set where
  []   : Vec A zero
  _::_ : {n : ℕ} →
         A → Vec A n
         → Vec A (suc n)

vmap : {A B : Set}{n : ℕ} → (A → B) → Vec A n → Vec B n
vmap _ []        = []
vmap f (x :: xs) = f x :: vmap f xs

infixl 0 _$_
_$_ : {n : ℕ}{A B : Set} →
        Vec (A → B) n → Vec A n → Vec B n
[] $ []               = []
(f :: fs) $ (x :: xs) = f x :: (fs $ xs)

head : {A : Set}{n : ℕ} → Vec A (suc n) → A
head (x :: xs) = x

vec : {n : ℕ}{A : Set} → A → Vec A n
vec {zero}  x = []
vec {suc n} x = x :: vec {n} x

infixl 10 _!_
_!_ : {n : ℕ}{A : Set} →
      Vec A n → Fin n → A
[]        ! ()
(x :: xs) ! fzero    = x
(x :: xs) ! (fsuc i) = xs ! i

cons-eq : ∀ {A n}
          {x y : A}{xs ys : Vec A n} →
          (x == y) → (xs == ys) → 
          ((x :: xs) == (y :: ys))
cons-eq refl refl = refl

tabulate : {n : ℕ}{A : Set} →
           (Fin n → A) → Vec A n
tabulate {zero}  f = []
tabulate {suc n} f = f fzero :: tabulate (f ∘ fsuc)

lem-!-tab : ∀ {A n}
            (f : Fin n → A) → (i : Fin n) →
            (tabulate f ! i) == (f i)
lem-!-tab f fzero    = refl
lem-!-tab f (fsuc n) = lem-!-tab (f ∘ fsuc) n

lem-tab-! : ∀ {A n} →
            (xs : Vec A n) →
            tabulate (_!_ xs) == xs
lem-tab-! []        = refl
lem-tab-! (x :: xs) = cons-eq refl (lem-tab-! xs)

Matrix : Set →  ℕ → ℕ → Set
Matrix A m n = Vec (Vec A n) m

transpose : ∀ {A m n} → Matrix A m n → Matrix A n m
transpose []          = vec []
transpose (xs :: xss) = vmap (_::_) xs $ transpose xss

transpose-correct : ∀ {A m n}
                    {M : Matrix A m n}
                    (i : Fin m) (j : Fin n) →
                    ((M ! i) ! j) == (((transpose M ! j) ! i))
transpose-correct = {!!}


