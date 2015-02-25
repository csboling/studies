module Basic.List where

data List (A : Set) : Set where
  []   : List A
  _::_ : A → List A → List A

tail : {A : Set} → List A → List A
tail []        = []
tail (x :: xs) = xs

data Sublist {A : Set} : List A → Set where
  []   : Sublist []
  _::_ : ∀ x {xs} → Sublist xs → Sublist (x :: xs)
  skip : ∀ {x xs} → Sublist xs → Sublist (x :: xs)

infixl 20 _⊆_
data _⊆_ {A : Set} : List A → List A → Set where
  stop : [] ⊆ []
  drop : ∀ {x xs ys} → xs ⊆ ys →       xs  ⊆ (x :: ys)
  keep : ∀ {x xs ys} → xs ⊆ ys → (x :: xs) ⊆ (x :: ys)

empty-⊘ : {A : Set}{l : List A} → [] ⊆ l
empty-⊘ {l = []}      = stop
empty-⊘ {l = x :: xs} = drop (empty-⊘ {l = xs})

⊆-implies-tail-⊆ : {A : Set}{xs ys : List A} →
                   xs ⊆ ys → tail xs ⊆ ys
⊆-implies-tail-⊆ stop     = stop
⊆-implies-tail-⊆ (drop p) = drop (⊆-implies-tail-⊆ p)
⊆-implies-tail-⊆ (keep p) = drop p

⊆-refl : {A : Set}(xs : List A) → xs ⊆ xs
⊆-refl []        = stop
⊆-refl (x :: xs) = keep (⊆-refl xs)

⊆-trans : {A : Set}{xs ys zs : List A} →
          xs ⊆ ys → ys ⊆ zs → xs ⊆ zs
⊆-trans stop     q        = empty-⊘
⊆-trans (drop p) q        = ⊆-trans p (⊆-implies-tail-⊆ q)
⊆-trans (keep p) (drop q) = drop (⊆-trans (keep p) q)
⊆-trans (keep p) (keep q) = keep (⊆-trans p q)

