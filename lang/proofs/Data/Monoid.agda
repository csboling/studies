module Data.Monoid where

open import Relation.Binary.PropositionalEquality
open ≡-Reasoning
open import Data.Nat
open import Data.Nat.Properties.Simple
open import Data.Product
open import Function

--import Relation.Binary.EqReasoning as EqR

record Monoid (G : Set) : Set where
  field 
    ε   : G
    _⊗_ : G → G → G
    α   : {a b c : G} → (a ⊗ b) ⊗ c ≡ a ⊗ (b ⊗ c)
    λₘ  : {a : G}     → ε ⊗ a ≡ a
    ρₘ  : {a : G}     → a ⊗ ε ≡ a

  -- ε is the unique left and right unit element of G.
  λₘ-unique : (ε′ : G)
            → ({a : G} → ε′ ⊗ a ≡ a)
            → ε′ ≡ ε
  λₘ-unique ε′ λ-hyp = trans (sym (ρₘ {a = ε′})) λ-hyp

  ρₘ-unique : (ε′ : G)
            → ({a : G} → a ⊗ ε′ ≡ a)
            → ε′ ≡ ε
  ρₘ-unique ε′ ρ-hyp = trans (sym (λₘ {a = ε′})) ρ-hyp

  ε-commutes : ∀ a → ε ⊗ a ≡ a ⊗ ε
  ε-commutes a =
    begin
      ε ⊗ a
    ≡⟨ λₘ ⟩
      a
    ≡⟨ sym ρₘ ⟩
      a ⊗ ε
    ∎

  -- iterated products
  ⨂ : ℕ → (ℕ → G) → G
  ⨂ zero    _ = ε
  ⨂ (suc n) x = (⨂ n x) ⊗ (x (suc n))

  ⨂-ejectₗ : ∀ n → (x : ℕ → G) 
            → ⨂ (suc n) x ≡ x 1 ⊗ ⨂ n (x ∘ suc)
  ⨂-ejectₗ zero    x = ε-commutes (x 1)
  ⨂-ejectₗ (suc n) x = 
    begin
      ⨂ (suc (suc n)) x 
    ≡⟨ refl ⟩
      (⨂ (suc n) x) ⊗ x (suc (suc n))
    ≡⟨ cong (λ g → g ⊗ x (2 + n)) $ ⨂-ejectₗ n x ⟩
      (x 1 ⊗ ⨂ n (x ∘ suc)) ⊗ x (2 + n)
    ≡⟨ α ⟩
      x 1 ⊗ (⨂ n (x ∘ suc) ⊗ x (2 + n))
    ≡⟨ refl ⟩
      x 1 ⊗ ⨂ (suc n) (x ∘ suc)
    ∎

  ⨂-index-shift : ∀ m n → (x : ℕ → G)
                 →  x (suc m) ⊗ (⨂ n (λ ν → x (suc m + ν))) 
                  ≡ ⨂ (suc n) (λ ν → x (m + ν))
  ⨂-index-shift 0 0 x = 
    begin
      x 1 ⊗ (⨂ 0 (λ ν → x (0 + ν)))
    ≡⟨ ρₘ {a = x 1} ⟩
      x 1
    ≡⟨ sym (λₘ {a = x 1})⟩
      ⨂ 1 (λ ν → x (0 + ν))
    ∎
  ⨂-index-shift m n x = {!!}

  ⨂-any-parens : ∀ m n → (x : ℕ → G)
                →  (⨂ m x) ⊗ ( ⨂ n (λ ν → x (m + ν)) ) 
                  ≡ ⨂ (m + n) x
  ⨂-any-parens zero    n x = λₘ {a = ⨂ n x}
  ⨂-any-parens (suc m) n x = 
    begin
      (⨂ (suc m) x) ⊗ (⨂ n (λ ν → x (suc m + ν)))
    ≡⟨ cong (λ g → g ⊗ (⨂ n (λ ν → x (suc m + ν)))) $ refl ⟩
      ((⨂ m x) ⊗ x (suc m)) ⊗ (⨂ n (λ ν → x (suc m + ν)))
    ≡⟨ α ⟩
      (⨂ m x)  ⊗ (x (suc m) ⊗ (⨂ n (λ ν → x (suc m + ν))))
    ≡⟨ cong (λ g → (⨂ m x) ⊗ g) $ ⨂-index-shift m n x ⟩
      (⨂ m x)  ⊗ (⨂ (suc n) (λ ν → x (m + ν)))
    ≡⟨ refl ⟩
      (⨂ m x)  ⊗ ((⨂ n (λ ν → x (m + ν))) ⊗ x (m + suc n))
    ≡⟨ sym α ⟩
      ( (⨂ m x) ⊗ (⨂ n (λ ν → x (m + ν))) ) ⊗ x (m + suc n)
    ≡⟨ cong (λ g → g ⊗ x (m + suc n)) $ ⨂-any-parens m n x ⟩
      (⨂ (m + n) x) ⊗ x (m + suc n)
    ≡⟨ cong (λ i → (⨂ (m + n) x) ⊗ x i) $ +-suc m n ⟩
      (⨂ (m + n) x) ⊗ (x (suc (m + n)))
    ≡⟨ refl ⟩
      ⨂ (suc (m + n)) x
    ∎

{-
  ⨂-assoc : ∀ m n → (x : ℕ → G)
           → ⨂ (m , suc n) x ≡ x m ⊗ (⨂ (suc m , suc n) x)
{-  ⨂-assoc 0 1 x =
    begin
      (⨂ (0 , 1) x) ⊗ x 1
    ≈⟨ refl ⟩
      ε ⊗ x 1
    ≈⟨ λₘ ⟩
      x 1
-}
  ⨂-assoc 0 1 x =
    begin
      (⨂ (0 , 1) x)
    ≈⟨ ? ⟩
      (⨂ (0 , 0) x) ⊗ x 1
    ≈⟨ refl ⟩
      x 0 ⊗ x 1
    ≈⟨ refl ⟩
      x 0 ⊗ (⨂ (1 , 1) x)
    ∎
  ⨂-assoc m    n    x = ?
-}
