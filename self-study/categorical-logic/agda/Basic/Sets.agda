module Basic.Sets where

infixl 0 _==_
data _==_ {A : Set}(x : A) : A → Set where
  refl : x == x

infixl 10 _∘_
_∘_ : {A : Set}{B : A → Set}{C : (x : A) → B x → Set}
      (g : {x : A} → (y : B x) → C x y)
      (f : (x : A) → B x)
      → ((x : A) → C x (f x))
(g ∘ f) x = g (f x)

cong : ∀ {A B}
       {x y : A}
       (f : A → B) → (x == y) → (f x == f y)
cong f refl = refl

symm : ∀ {A}
       {x y : A} →
       (x == y) → (y == x)
symm refl = refl

trans : ∀ {A}
        {x y z : A} →
        (x == y) → (y == z) → (x == z)
trans refl refl = refl

{-
∘-assoc : ∀ {A B C D}
          (h : C → D)
          (g : B → C)
          (f : A → B) →
          h ∘ (g ∘ f) == (h ∘ g) ∘ f
∘-assoc h g f = {!!}
-}

data List (A : Set) : Set where
  []   : List A
  _::_ : A → List A → List A

tail : {A : Set} → List A → List A
tail []        = []
tail (x :: xs) = xs

infixl 20 _⊆_
data _⊆_ {A : Set} : List A → List A → Set where
  stop : [] ⊆ []
  drop : ∀ {x xs ys} → xs ⊆ ys →       xs  ⊆ (x :: ys)
  keep : ∀ {x xs ys} → xs ⊆ ys → (x :: xs) ⊆ (x :: ys)

empty-⊘ : {A : Set}{l : List A} → [] ⊆ l
empty-⊘ {l = []}      = stop
empty-⊘ {l = x :: xs} = drop (empty-⊘ {l = xs})

⊆-refl : {A : Set}(xs : List A) → xs ⊆ xs
⊆-refl []        = stop
⊆-refl (x :: xs) = keep (⊆-refl xs)

tail-⊆ : {A : Set}{L : List A} →
         tail L ⊆ L
tail-⊆ {L = []}      = stop
tail-⊆ {L = x :: xs} = drop (⊆-refl xs)

⊆-implies-tail-⊆ : {A : Set}{xs ys : List A} →
                   xs ⊆ ys → tail xs ⊆ ys
⊆-implies-tail-⊆ stop     = stop
⊆-implies-tail-⊆ (drop p) = drop (⊆-implies-tail-⊆ p)
⊆-implies-tail-⊆ (keep p) = drop p

⊆-trans : {A : Set}{xs ys zs : List A} →
          xs ⊆ ys → ys ⊆ zs → xs ⊆ zs
⊆-trans p q with p
⊆-trans p q | stop   = empty-⊘
⊆-trans p q | drop r = ⊆-trans r (⊆-implies-tail-⊆ q)
⊆-trans p q | keep r = ⊆-trans ? (⊆-implies-tail-⊆ q)
