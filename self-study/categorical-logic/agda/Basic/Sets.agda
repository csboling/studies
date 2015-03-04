module Basic.Sets where

data Bool : Set where
  True  : Bool
  False : Bool

infixl 0 _==_
data _==_ {A : Set} : (x y : A) → Set where
  refl : {x : A} → x == x

infixl 10 _∘_
_∘_ : {A : Set}{B : A → Set}{C : (x : A) → B x → Set}
      (g : {x : A} → (y : B x) → C x y)
      (f : (x : A) → B x)
      → ((x : A) → C x (f x))
(g ∘ f) x = g (f x)

∘-assoc : ∀ {A B C D}
          (h : C → D)
          (g : B → C)
          (f : A → B) →
          h ∘ (g ∘ f) == (h ∘ g) ∘ f
∘-assoc h g f = refl

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
import Basic.Relation
==-preorder : Basic.Relation.Preorder (_==_)
==-preorder = record{ refl = refl; trans = trans }
-}
