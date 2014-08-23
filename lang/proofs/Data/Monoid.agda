module Data.Monoid where

open import Relation.Binary.PropositionalEquality

record Monoid (m : Set) : Set where
  field 
    I   : m
    _⊗_ : m → m → m
    α   : {A B C : m} → (A ⊗ B) ⊗ C ≡ A ⊗ (B ⊗ C)
    λₘ  : {A : m}     → I ⊗ A ≡ A
    ρₘ  : {A : m}     → A ⊗ I ≡ A
  unit-unique : (I' : m)
              → ({A : m} → I' ⊗ A ≡ A)
              → ({A : m} → A ⊗ I' ≡ A)
              → I' ≡ I
  unit-unique e' λ-hypoth ρ-hypoth = trans (sym (λₘ {A = e'})) ρ-hypoth
  
