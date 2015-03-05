module Basic.List where

open import Basic.Sets

infixl 10 _::_
data List (A : Set) : Set where
  []   : List A
  _::_ : A → List A → List A

map : {A B : Set} → (A → B) → List A → List B
map f []        = []
map f (x :: xs) = f x :: map f xs

tail : {A : Set} → List A → List A
tail []        = []
tail (x :: xs) = xs

infixl 5 _∈_
data _∈_ {A : Set}(x : A) : List A → Set where
  hd : ∀ {xs}   → x ∈ x :: xs
  tl : ∀ {y xs} → x ∈ xs → x ∈ y :: xs

infixl 20 _⊆_
data _⊆_ {A : Set} : List A → List A → Set where
  stop : [] ⊆ []
  drop : ∀ {x xs ys} → xs ⊆ ys →       xs  ⊆ (x :: ys)
  keep : ∀ {x xs ys} → xs ⊆ ys → (x :: xs) ⊆ (x :: ys)

data SubList {A : Set} : List A → Set where
  []   : SubList []
  _::_ : ∀ x {xs} → SubList xs → SubList (x :: xs)
  skip : ∀ {x xs} → SubList xs → SubList (x :: xs)

forget : {A : Set}{xs : List A} → SubList xs → List A
forget []        = []
forget (x :: xs) = x :: forget xs
forget (skip xs) = forget xs

forget-inv : {A : Set}(xs : List A) → SubList xs
forget-inv []        = []
forget-inv (x :: xs) = x :: forget-inv xs

lem-forget : {A : Set}{xs : List A} →
             (zs : SubList xs) → forget zs ⊆ xs
lem-forget []        = stop
lem-forget (z :: zs) = keep (lem-forget zs)
lem-forget (skip zs) = drop (lem-forget zs)

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

{-
import Basic.Relation
⊆-preorder : Basic.Relation.Preorder (_⊆_)
⊆-preorder = record{ refl = ⊆-refl; trans = ⊆-trans }
-}

filter : {A : Set} → (A → Bool) → (xs : List A) → SubList xs
filter p []        = []
filter p (x :: xs) with p x
... | True  = x :: filter p xs
... | False = skip (filter p xs)

complement : {A : Set}{l : List A} → SubList l → SubList l
complement []                      = []
complement (y :: ys)               = skip (complement ys)
complement {l = x :: xs} (skip ys) = x :: complement ys

sublists : {A : Set}(xs : List A) → List (SubList xs)
sublists []        = [] :: []
sublists (x :: xs) = forget-inv (x :: xs) :: map skip (sublists xs)
