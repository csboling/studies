module Basic.Nat where

open import Basic.Sets

data ℕ : Set where
  zero : ℕ
  suc  : ℕ → ℕ

infixl 3 _+_
_+_ : ℕ → ℕ → ℕ
zero  + n = n
suc m + n = suc (m + n)

+-id : (m : ℕ) → (m + zero) == m
+-id zero    = refl
+-id (suc m) = cong suc (+-id m)

lem-suc : ∀ {m n} →
          suc (m + n) == m + suc n
lem-suc {zero}  {n} = refl
lem-suc {suc m} {n} = {!!}

+-comm : ∀ {m n} →
         m + n == n + m
+-comm {zero}  {n}     = symm (+-id n)
+-comm {suc m} {zero}  = cong suc (+-id m)
+-comm {suc m} {suc n} = cong suc 
                           (trans 
                              (trans 
                                 (+-comm {m} {suc n}) 
                                 (cong suc (+-comm {n} {m}))
                              ) 
                              (+-comm {suc m} {n})
                           ) 

data Fin : ℕ → Set where
  fzero : {n : ℕ} → Fin (suc n)
  fsuc  : {n : ℕ} → Fin n → Fin (suc n)
