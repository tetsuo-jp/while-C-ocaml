{-# OPTIONS --safe #-}
------------------------------------------------------------------------
-- Theorem A: a REVERSIBLE realization of a NON-injective specializer must
-- retain "garbage" that distinguishes the inputs the specializer merges.
-- This is the Bennett-Landauer information lower bound, specialized to
-- partial evaluation -- a constraint CLASSICAL (irreversible) PE does not
-- face, and the proposed theoretical centerpiece distinguishing this work
-- from the classical Futamura/Jones results.  (Abstract; complements
-- rwhile-C-ocaml's RWhileRevProjGen `proj1-needs-trivial`.)
--
-- A specializer is a function  f : X -> Y   (X = (program , static-input),
-- Y = residual).  A REVERSIBLE realization computes f WITHOUT destroying
-- information: an INJECTIVE  g : X -> G  ("the reversible spec") together with
-- a decode  d : G -> Y  recovering the residual  (d (g x) = f x).  `g`
-- injective = the realization is invertible (Landauer: no information erased).
--
-- A specializer is NON-injective whenever two distinct inputs yield the same
-- residual.  This holds for any genuinely OPTIMIZING spec: the residual cannot
-- encode which program (among behaviourally-equivalent ones), or which static
-- value once it has been folded away, produced it -- distinct (p,s) collapse.
--
-- The theorem (module Realization, given a collision of f):
--   separates      : g KEEPS the two inputs distinct      (g x1 != g x2)
--   same-residual  : yet they decode to the SAME residual (d(g x1) = d(g x2))
-- so g x1 and g x2 differ ONLY in the part d throws away == GARBAGE that the
-- reversible spec is forced to carry.  And `no-clean-if-merges`: a CLEAN
-- reversible spec (g = f, d = id, no garbage) exists IFF f is injective.
--
-- Corollary (the trade-off, informal): a reversible specializer is either
-- TRIVIAL (injective on the collision -> no optimization there) or it
-- ACCUMULATES garbage.  "Reversible + optimizing + clean" is impossible.
------------------------------------------------------------------------

module RevSpecGarbage where

open import Relation.Binary.PropositionalEquality using (_≡_; refl; sym; trans)
open import Relation.Nullary using (¬_)
open import Data.Product using (Σ; _,_; proj₁)

-- The fibre of a function h over a point o: the inputs that h maps to o.
Fibre : {A B : Set} → (A → B) → B → Set
Fibre {A} h o = Σ A (λ a → h a ≡ o)

-- A reversible realization of a specializer f : X -> Y.
module Realization
  {X Y G : Set}
  (f : X → Y)                              -- the specializer
  (g : X → G)                              -- the reversible spec (injective)
  (d : G → Y)                              -- decode the residual back out
  (recovers : ∀ x → d (g x) ≡ f x)         -- it really computes f ...
  (g-inj : ∀ {x x'} → g x ≡ g x' → x ≡ x') -- ... reversibly (g injective)
  where

  -- For any pair of inputs the specializer MERGES (same residual, yet distinct):
  module Collision (x₁ x₂ : X) (distinct : ¬ (x₁ ≡ x₂)) (merged : f x₁ ≡ f x₂) where

    -- the reversible realization is FORCED to keep them apart ...
    separates : ¬ (g x₁ ≡ g x₂)
    separates eq = distinct (g-inj eq)

    -- ... although they decode to the very same residual:
    -- so g x₁ and g x₂ differ only in garbage (the part `d` discards).
    same-residual : d (g x₁) ≡ d (g x₂)
    same-residual = trans (recovers x₁) (trans merged (sym (recovers x₂)))

  -- QUANTITATIVE bound (Bennett-Landauer count).  Over each residual `o`, the
  -- reversible state space (d's fibre) contains an INJECTIVE image of EVERY
  -- input the specializer maps to `o` (f's fibre): `fibre-map` lands in d's
  -- fibre and is injective.  So the garbage carried at `o` has at least as many
  -- states as there are inputs producing `o` -- the realization must store ≥
  -- log |fibre(f,o)| bits beyond the residual.
  fibre-map : (o : Y) → Fibre f o → Fibre d o
  fibre-map o (x , fx≡o) = g x , trans (recovers x) fx≡o

  fibre-map-inj : (o : Y) {p q : Fibre f o}
    → proj₁ (fibre-map o p) ≡ proj₁ (fibre-map o q) → proj₁ p ≡ proj₁ q
  fibre-map-inj o eq = g-inj eq

-- A CLEAN reversible realization (no garbage: G = Y, d = id, g = f) is exactly
-- `f` being injective.
clean-reversible : {X Y : Set} (f : X → Y) → Set
clean-reversible {X} f = ∀ {x x'} → f x ≡ f x' → x ≡ x'

-- Hence a specializer that merges two distinct inputs has NO clean reversible
-- realization: garbage is unavoidable.
no-clean-if-merges : {X Y : Set} (f : X → Y)
  → (x₁ x₂ : X) → ¬ (x₁ ≡ x₂) → f x₁ ≡ f x₂
  → ¬ (clean-reversible f)
no-clean-if-merges f x₁ x₂ distinct merged cln = distinct (cln merged)

-- Contrapositive: a clean reversible specializer is injective, i.e. it never
-- merges inputs -- it cannot optimize away any distinction (it is trivial in
-- that sense).  This is the "trivial OR garbage" dichotomy.
clean→no-merge : {X Y : Set} (f : X → Y)
  → clean-reversible f
  → ∀ {x₁ x₂} → ¬ (x₁ ≡ x₂) → ¬ (f x₁ ≡ f x₂)
clean→no-merge f cln distinct merged = distinct (cln merged)
