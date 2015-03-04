{-# OPTIONS --without-K #-}

module Basic.Equal where

infixl 0 _==_
data _==_ {A : Set} : (x y : A) → Set where
  refl : (x : A) → x == x

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
∘-assoc h g f = refl _

-- path induction
ind-= : {A : Set}(C : (x y : A) → x == y → Set) →
        ((x : A) → C x x (refl x)) →
        (x y : A)(p : x == y) → C x y p
ind-= C c .x .x (refl x) = c x

-- based path induction
ind′-= : {A : Set}{a : A}(C : (x : A) → a == x → Set) →
         C a (refl a) → (x : A)(p : a == x) → C x p
ind′-= C c .x (refl x) = c

-- indiscernability of identicals
id : {A : Set} → A → A
id = λ x → x

const : {A B : Set} → A → B → A
const k x = k

rec-= : {A : Set}(C : A → Set) →
        (x y : A) → (x == y) → C x → C y
rec-= {A} C = ind-= D (λ a → id) where
    D : (a b : A) → (a == b) → Set
    D a b p = C a → C b

-- 1-groupoid structure
inv : {A : Set}{x y : A} →
      (x == y) → (y == x)
inv {A} {a} {b} = ind-= D refl a b where
  D : (a b : A) → (a == b) → Set
  D a b p = b == a

infixl 10 _◾_
_◾_ : {A : Set}{x y z : A} →
      (x == y) → (y == z) → (x == z)
_◾_ {A} {x} {y} {z} = rec-= (λ a → a == z) y x ∘ inv

-- 2-groupoid structure

lu : {A : Set}{x y : A} →
     (p : x == y) → p ◾ (refl y) == p
lu {A} {x} {y} = ind-= D (refl ∘ refl) x y where
 D : (a b : A) → a == b → Set
 D a b p = p ◾ (refl b) == p

ru : {A : Set}{x y : A} →
     (p : x == y) → (refl x) ◾ p == p
ru {A} {.x} .{x} (refl x)= refl (refl x)

inv-id-refl : {A : Set}{x y : A}{p : x == y} →
              (inv p) ◾ p == refl y
inv-id-refl {A} {.x} {.x} {refl x} = refl (refl x)

id-inv-refl : {A : Set}{x y : A}{p : x == y} →
              p ◾ (inv p) == refl x
id-inv-refl {A} {.y} {y} {refl .y} = refl (refl y)

inv-involution : {A : Set}{x y : A}{p : x == y} →
                 inv (inv p) == p
inv-involution {A} {.y} {y} {refl .y} = refl (refl y)

◾-assoc : {A : Set}{w x y z : A}
          (p : w == x)
          (q : x == y)
          (r : y == z) →
          p ◾ (q ◾ r) == (p ◾ q) ◾ r
◾-assoc {A} {.z} {.z} {.z} {z} (refl .z) (refl .z) (refl .z) = refl (refl z)
-- loop space
--Ω0 : (A : Set) → a → Set
--Ω0 A = {!Σ (a == a)!}
--Ω : (n : ℕ) → Set → Set
--Ω zero    A = Ω0
--Ω (suc n) A = Ω0 (Ω n A)

-- functoriality of functions
ap : {A B : Set}{x y : A} →
     (f : A → B) → (x == y) → (f x == f y)
ap {x = a} {y = b} f = ind-= (λ x y p → f x == f y)
                             (refl ∘ f)
                             a b
