module Basic.Sets where

infixl 30 _==_
data _==_ {A : Set}(x : A) : A → Set where
  refl : x == x

data ℕ : Set where
  zero : ℕ
  suc  : ℕ → ℕ

data Fin : ℕ → Set where
  fzero : {n : ℕ} → Fin (suc n)
  fsuc  : {n : ℕ} → Fin n → Fin (suc n)

infixl 20 _::_
data Vec (A : Set) : ℕ → Set where
  []   : Vec A zero
  _::_ : {n : ℕ} →
         A → Vec A n
         → Vec A (suc n)

infixl 10 _!_
_!_ : {n : ℕ}{A : Set} →
      Vec A n → Fin n → A
[]        ! ()
(x :: xs) ! fzero    = x
(x :: xs) ! (fsuc i) = xs ! i


vmap : {A B : Set}{n : ℕ} → (A → B) → Vec A n → Vec B n
vmap _ []        = []
vmap f (x :: xs) = f x :: vmap f xs

infixl 90 _$_
_$_ : {n : ℕ}{A B : Set} →
        Vec (A → B) n → Vec A n → Vec B n
[] $ []               = []
(f :: fs) $ (x :: xs) = f x :: (fs $ xs)

head : {A : Set}{n : ℕ} → Vec A (suc n) → A
head (x :: xs) = x

vec : {n : ℕ}{A : Set} → A → Vec A n
vec {zero}  x = []
vec {suc n} x = x :: vec {n} x

Matrix : Set →  ℕ → ℕ → Set
Matrix A m n = Vec (Vec A n) m

transpose : ∀ {A m n} → Matrix A m n → Matrix A n m
transpose []          = vec []
transpose (xs :: xss) = (vmap (_::_) xs) $ transpose xss

transpose-correct : ∀ {A m n}
                    {M : Matrix A m n}
                    {i : Fin m} {j : Fin n} →
                    ((M ! i) ! j) == (((transpose M ! j) ! i))
transpose-correct = {!!}
