module Basic.Nat where

-- open import Basic.Sets
open import Relation.Binary.PropositionalEquality as PropEq
  using (_≡_; refl) renaming (cong to ≡-cong)
open PropEq.≡-Reasoning

data ℕ : Set where
  zero : ℕ
  succ : ℕ → ℕ

infixl 3 _+_
_+_ : ℕ → ℕ → ℕ
zero  + n = n
succ m + n = succ (m + n)

-- +-id : (m : ℕ) → (m + zero) == m
-- +-id zero    = refl
-- +-id (succ m) = cong succ (+-id m)

+-comm : ∀ {m n} → (m + n) ≡ (n + m)
+-comm {m} {n} =
  begin
    (m + n)
  ≡⟨ ? ⟩
    (n + m)
  ∎

-- +-comm : ∀ {m n} →
--          m + n == n + m
-- +-comm {zero}  {n}     = symm (+-id n)
-- +-comm {succ m} {zero}  = cong succ (+-id m)
-- +-comm {succ m} {succ n} = cong succ
--                            (trans
--                               (trans
--                                  (+-comm {m} {succ n})
--                                  (cong succ (+-comm {n} {m}))
--                               )
--                               (+-comm {succ m} {n})
--                            )

-- infixl 1 _≤_
-- data _≤_ : ℕ → ℕ → Set where
--   zero-≤  : {n : ℕ} → zero ≤ n
--   succ-≤  : {m n : ℕ} → m ≤ n → succ m  ≤ succ n

-- ≤-trans : {x y z : ℕ} → x ≤ y → y ≤ z → x ≤ z
-- ≤-trans zero-≤     zero-≤     = zero-≤
-- ≤-trans zero-≤     (succ-≤ _) = zero-≤
-- ≤-trans (succ-≤ p) (succ-≤ q) = succ-≤ (≤-trans p q)

-- ≤-refl : {x : ℕ} → x ≤ x
-- ≤-refl {zero}   = zero-≤
-- ≤-refl {succ x} = succ-≤ ≤-refl

-- ≤-antisymm : {x y : ℕ} → x ≤ y → y ≤ x → x == y
-- ≤-antisymm zero-≤     zero-≤     = refl
-- ≤-antisymm (succ-≤ p) (succ-≤ q) = cong succ (≤-antisymm p q)

-- ≤-assoc : {w x y z : ℕ} {h : w ≤ x} {g : x ≤ y} {f : y ≤ z} →
--           ≤-trans h (≤-trans g f) == ≤-trans (≤-trans h g) f
-- ≤-assoc = {!!}

-- data Fin : ℕ → Set where
--   fzero : {n : ℕ} → Fin (succ n)
--   fsucc  : {n : ℕ} → Fin n → Fin (succ n)
