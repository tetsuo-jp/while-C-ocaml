{-# OPTIONS --safe #-}

------------------------------------------------------------------------
-- REVERSIBLE LEVIN, step R0: the abstract brick.
--
-- The reversible question (docs/REV-LEVIN-SKELETON.md): does a REVERSIBLE
-- search program achieve Levin optimality (Jones Thm 20.1.2), and at what
-- garbage cost?  This module pins the three facts that already hold with NO
-- new operational work, isolating exactly what the reversible track must build:
--
--   R0a  Layer A is reversible-ready: `LevinOptimal.levin-optimal` needs ONLY
--        the overhead constant -- instantiating `LevinSearch.overhead` with an
--        abstract reversible-interpreter constant `a-rev` fires the optimality
--        inequality verbatim.  (So reversible Levin TIME-optimality reduces to
--        proving `a-rev`, the analogue of IntEfficient's a = 73.)
--   R0b  Garbage UPPER bound (value level): every function has a reversible
--        realization that keeps its input -- Bennett's embedding x ↦ (x, f x)
--        is injective and decodes back by proj₂.  Instantiates Theorem A's
--        Realization module, so its fibre machinery applies to it.
--   R0c  Garbage LOWER bound: a search whose witness map MERGES two inputs
--        (the typical case -- many instances share a witness) has NO clean
--        reversible realization (Theorem A, `no-clean-if-merges`), with a
--        concrete non-vacuous instance (a constant witness map).
--
-- Together: reversible Levin is a TIME-optimality (R0a, pending a-rev) plus a
-- garbage BRACKET |fibre| ≤ garbage ≤ |input| (R0b/R0c) -- optimality and
-- cleanliness are jointly unachievable.  The remaining operational work
-- (reversible core language, step count, reversible self-interpreter, Bennett
-- TIME lemma) is laid out in docs/REV-LEVIN-SKELETON.md.
------------------------------------------------------------------------

module RevLevinAbstract where

open import Data.Nat using (ℕ; _+_; _*_; _≤_; _<_)
open import Data.Product using (Σ; _×_; _,_; proj₁; proj₂)
open import Data.Bool using (Bool; true; false)
open import Data.Unit using (⊤; tt)
open import Relation.Binary.PropositionalEquality using (_≡_; refl; cong)
open import Relation.Nullary using (¬_)

open import LevinBudget using (gsum)
open import LevinOptimal using (LevinSearch; levin-optimal; aq; c; S)
open import RevSpecGarbage using (clean-reversible; no-clean-if-merges; module Realization)

------------------------------------------------------------------------
-- R0a.  Layer A is reversible-ready: only the overhead FIELD changes.
-- `a-rev` is the (yet-to-be-proven) reversible-interpreter overhead -- the
-- reversible twin of `IntEfficient.a = 73` that a future RevIntEfficient
-- discharges (cf. LevinGrounded, which does this for the classical constant).
------------------------------------------------------------------------

module RevSearch (a-rev j Tq Tr : ℕ) where

  rev-search : LevinSearch
  rev-search = record { overhead = a-rev ; idx = j ; time-q = Tq ; time-r = Tr }

  -- Levin optimality for the reversible searcher, verbatim from Layer A:
  -- gsum k < 4 · a-rev · 2^j · (T_q + T_r).
  rev-levin-optimal : 1 ≤ c rev-search * S rev-search
    → Σ ℕ (λ k → gsum k < aq rev-search * S rev-search)
  rev-levin-optimal = levin-optimal rev-search

------------------------------------------------------------------------
-- R0b.  Garbage upper bound (value level): the Bennett embedding.
-- Keeping the input alongside the output is ALWAYS a reversible realization:
-- g = ⟨id, f⟩ is injective, and proj₂ decodes the result back out.  This is
-- the value-level content of Bennett's trick (the TIME-bounded version, over a
-- reversible operational semantics, is the R1c obligation in the skeleton).
------------------------------------------------------------------------

module Bennett {X Y : Set} (f : X → Y) where

  g : X → X × Y
  g x = x , f x

  g-inj : ∀ {x x'} → g x ≡ g x' → x ≡ x'
  g-inj = cong proj₁

  -- ⟨id, f⟩ IS a reversible realization of f in Theorem A's sense: its fibre
  -- machinery (separates / same-residual / fibre-map) applies to it, with the
  -- input as the garbage `proj₂` discards.
  open Realization f g proj₂ (λ _ → refl) g-inj public

------------------------------------------------------------------------
-- R0c.  Garbage lower bound: no clean reversible search.
-- Read Theorem A at the search's WITNESS MAP wit : X → Y (input instance ↦ the
-- witness the search returns).  A search problem is non-trivial exactly when
-- two distinct instances share a witness -- then NO clean (garbage-free)
-- reversible search program can exist.
------------------------------------------------------------------------

no-clean-rev-search : {X Y : Set} (wit : X → Y)
  → (x₁ x₂ : X) → ¬ (x₁ ≡ x₂) → wit x₁ ≡ wit x₂
  → ¬ (clean-reversible wit)
no-clean-rev-search = no-clean-if-merges

-- Concrete non-vacuity: the most merging witness map possible -- a search
-- whose answer is the same for every instance (e.g. "find any even number").
constant-witness : Bool → ⊤
constant-witness _ = tt

true≢false : ¬ (true ≡ false)
true≢false ()

no-clean-constant : ¬ (clean-reversible constant-witness)
no-clean-constant = no-clean-if-merges constant-witness true false true≢false refl

-- ... yet its Bennett realization exists (the bracket is non-degenerate: the
-- input IS the garbage that keeps the merged instances apart).
module BennettConst = Bennett constant-witness
