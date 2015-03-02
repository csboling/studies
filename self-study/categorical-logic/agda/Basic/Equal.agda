module Basic.Equal where

infixl 10 _==_
data _==_ {A : Set} : (x y : A) → Set where
  refl : {x : A} → x == x

id : {A : Set} → A → A
id x = x

-- path induction
ind-= : {A : Set}(C : (x y : A) → x == y → Set) →
        ((x : A) → C x x refl) →
        (x y : A)(p : x == y) → C x y p
ind-= C c x .x refl = c x

-- based path induction
ind′-= : {A : Set}{a : A}(C : (x : A) → a == x → Set) →
         C a refl → (x : A)(p : a == x) → C x p
ind′-= C c x refl = c

-- indiscernability of identicals
rec-= : {A : Set}(C : A → Set) →
        (x y : A) → (x == y) → C x → C y
rec-= C x .x refl = id
