module Data.Real where

open import Data.Product
open import Relation.Binary.Core using (Asymmetric)
open import Relation.Binary.PropositionalEquality as P using (_≡_)

data ℝ : Set where
  one : ℝ                    -- axiom 7
  _+_ : ℝ → ℝ → ℝ

data _<_ : ℝ → ℝ → Set where
  axiom8 : one < (one + one) -- axiom 8

postulate
  asymmetric : Asymmetric _<_                              -- axiom 1
  assoc₁     : {x y z : ℝ} → (x + (y + z)) ≡ ((x + z) + y) -- axiom 4
--  assoc₂ : {x y   : ℝ} → ∃ ℝ (λ ⦃z : ℝ⦄ → x + z == y)    -- axiom 5
  axiom6 : {w x y z : ℝ} 
         → (x + y) < (z + w)
         → (x < z) × (y < w)
