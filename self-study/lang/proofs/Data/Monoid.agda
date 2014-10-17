module Data.Monoid where

open import Relation.Binary.PropositionalEquality
open ≡-Reasoning
open import Data.Nat
open import Data.Nat.Properties.Simple
open import Data.Nat.Properties
open import Data.Product
open import Function

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
  ⨂₁ : ℕ → (ℕ → G) → G
  ⨂₁ zero    _ = ε
  ⨂₁ (suc n) x = (⨂₁ n x) ⊗ (x (suc n))

  ⨂ : (k m n : ℕ) → (suc n ≡ m + k) → (ℕ → G) → G
  ⨂ k m n p x = ⨂₁ k (λ ν → x (m + ν))

  absorbₗ-arith : ∀ k m n → (suc n ≡ suc m + k) ≡ (n ≡ m + k)
  absorbₗ-arith k m n =
    begin
      suc n ≡ suc m + k
    ≡⟨ cong (λ t → suc n ≡ t) $ +-comm (suc m) k ⟩
      suc n ≡ k + suc m
    ≡⟨ cong (λ t → suc n ≡ t) $ +-suc k (suc m) ⟩
      suc n ≡ suc (k + m)
    ≡⟨ refl ⟩
      n ≡ m + k
    ≡⟨ cong (λ t → n ≡ t) $ +-comm (m k) ⟩
      n ≡ k + m
    ∎

  -- associativity properties of the iterated product
  ⨂-ejectₗ : ∀ n → (x : ℕ → G)
            → ⨂₁ (suc n) x ≡ x 1 ⊗ ⨂₁ n (λ ν → x (suc ν))
  ⨂-ejectₗ zero    x = ε-commutes (x 1)
  ⨂-ejectₗ (suc n) x =
    begin
      ⨂₁ (suc (suc n)) x
    ≡⟨ refl ⟩
      (⨂₁ (suc n) x) ⊗ x (suc (suc n))
    ≡⟨ cong (λ g → g ⊗ x (2 + n)) $ ⨂-ejectₗ n x ⟩
      (x 1 ⊗ ⨂₁ n (λ ν → x (suc ν))) ⊗ x (2 + n)
    ≡⟨ α ⟩
      x 1 ⊗ (⨂₁ n (λ ν → x (suc ν)) ⊗ x (2 + n))
    ≡⟨ refl ⟩
      x 1 ⊗ ⨂₁ (suc n) (λ ν → x (suc ν))
    ∎

  suc-lemma : ∀ m n → suc (m + n) ≡ suc m + n
  suc-lemma m n =
    begin
      suc (m + n)
    ≡⟨ cong suc $ +-comm m n ⟩
      suc (n + m)
    ≡⟨ sym (+-suc n m) ⟩
      n + suc m
    ≡⟨ +-comm n (suc m) ⟩
      suc m + n
    ∎

  ⨂-index-shift : ∀ m n → (x : ℕ → G)
                 →  x (suc m) ⊗ (⨂₁ n (λ ν → x (suc m + ν)))
                  ≡ ⨂₁ (suc n) (λ ν → x (m + ν))
  ⨂-index-shift zero    n x = sym (⨂-ejectₗ n x)
  ⨂-index-shift (suc m) n x =
    begin
      x (suc m) ⊗ ⨂₁ n (λ ν → x (suc m + ν))
    ≡⟨ sym (⨂-ejectₗ (suc m) x) ⟩
      ?
--      x (suc m) ⊗ ⨂ n (suc m) (m + n) (suc-lemma m n) x
    ≡⟨ {!!} ⟩
      ⨂₁ (suc n) (λ ν → x (m + ν))
    ∎

  -- Prove that
  -- ∏_{μ=1}^{m} x_μ · ∏_{ν = 1}^{n} x_{m + ν} = ∏_{μ = 1}^{m+n} x_μ
  ⨂-any-parens : ∀ m n → (x : ℕ → G)
                →  (⨂₁ m x) ⊗ ( ⨂₁ n (λ ν → x (m + ν)) )
                  ≡ ⨂₁ (m + n) x
  ⨂-any-parens zero    n x = λₘ {a = ⨂₁ n x}
  ⨂-any-parens (suc m) n x =
    begin
      (⨂₁ (suc m) x) ⊗ (⨂₁ n (λ ν → x (suc m + ν)))
    ≡⟨ cong (λ g → g ⊗ (⨂₁ n (λ ν → x (suc m + ν)))) $ refl ⟩
      ((⨂₁ m x) ⊗ x (suc m)) ⊗ (⨂₁ n (λ ν → x (suc m + ν)))
    ≡⟨ α ⟩
      (⨂₁ m x)  ⊗ (x (suc m) ⊗ (⨂₁ n (λ ν → x (suc m + ν))))
    ≡⟨ cong (λ g → (⨂₁ m x) ⊗ g) $ ⨂-index-shift m n x ⟩
      (⨂₁ m x)  ⊗ (⨂₁ (suc n) (λ ν → x (m + ν)))
    ≡⟨ refl ⟩
      (⨂₁ m x)  ⊗ ((⨂₁ n (λ ν → x (m + ν))) ⊗ x (m + suc n))
    ≡⟨ sym α ⟩
      ( (⨂₁ m x) ⊗ (⨂₁ n (λ ν → x (m + ν))) ) ⊗ x (m + suc n)
    ≡⟨ cong (λ g → g ⊗ x (m + suc n)) $ ⨂-any-parens m n x ⟩
      (⨂₁ (m + n) x) ⊗ x (m + suc n)
    ≡⟨ cong (λ i → (⨂₁ (m + n) x) ⊗ x i) $ +-suc m n ⟩
      (⨂₁ (m + n) x) ⊗ (x (suc (m + n)))
    ≡⟨ refl ⟩
      ⨂₁ (suc (m + n)) x
    ∎
