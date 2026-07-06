{-# OPTIONS --safe #-}

------------------------------------------------------------------------
-- Reversible Levin Track-2 (brick S1): the STORE and its walk-length cost --
-- the entire content of the measured `a-rev ≈ 364×`, interpreter-free
-- (abstraction (ii) of docs/REV-LEVIN-R3D-BRICKS.md).
--
-- `ri.rwhile`'s per-command overhead is dominated by LOOKUP/UPDATE, each a linear
-- walk of the length-`L` variable list `Vl`.  This module models the store as an
-- association list and proves exactly that: `lookupC k s ≤ length s` and
-- `updateC k s ≤ length s` (the walk is bounded by the store size `L`), and that
-- `update` preserves `length` (so `L` is invariant -- the `a-rev(L)` bound is
-- stable across a run).  No interpreter is needed; this is the store-side fact the
-- store-carrying interpreter (S4) charges its per-op cost against.  --safe.
------------------------------------------------------------------------

module RevStore where

open import Data.Nat using (ℕ; zero; suc; _≤_; z≤n; s≤s)
open import Data.Bool using (Bool; true; false)
open import Data.List using (List; []; _∷_; length)
open import Data.Product using (_×_; _,_)
open import Relation.Binary.PropositionalEquality using (_≡_; refl; cong)
open import UniversalInterp using (V; ⟨⟩; _∙_)

------------------------------------------------------------------------
-- keys = variable indices; a store is an association list of length `L`.

Store : Set
Store = List (ℕ × V)

eqℕ : ℕ → ℕ → Bool
eqℕ zero    zero    = true
eqℕ (suc m) (suc n) = eqℕ m n
eqℕ _       _       = false

------------------------------------------------------------------------
-- lookup / its cost (= entries walked) / the walk bound.

lookup : ℕ → Store → V
lookup k []             = ⟨⟩
lookup k ((k' , v) ∷ s) with eqℕ k k'
... | true  = v
... | false = lookup k s

lookupC : ℕ → Store → ℕ
lookupC k []             = 0
lookupC k ((k' , v) ∷ s) with eqℕ k k'
... | true  = 1
... | false = suc (lookupC k s)

lookupC-bound : ∀ k s → lookupC k s ≤ length s
lookupC-bound k []             = z≤n
lookupC-bound k ((k' , v) ∷ s) with eqℕ k k'
... | true  = s≤s z≤n
... | false = s≤s (lookupC-bound k s)

------------------------------------------------------------------------
-- update / its cost / the walk bound / length invariance.

update : ℕ → V → Store → Store
update k v []             = []
update k v ((k' , w) ∷ s) with eqℕ k k'
... | true  = (k' , v) ∷ s
... | false = (k' , w) ∷ update k v s

updateC : ℕ → Store → ℕ
updateC k []             = 0
updateC k ((k' , w) ∷ s) with eqℕ k k'
... | true  = 1
... | false = suc (updateC k s)

updateC-bound : ∀ k s → updateC k s ≤ length s
updateC-bound k []             = z≤n
updateC-bound k ((k' , w) ∷ s) with eqℕ k k'
... | true  = s≤s z≤n
... | false = s≤s (updateC-bound k s)

-- update preserves the store length L (so the a-rev(L) bound is stable).
update-length : ∀ k v s → length (update k v s) ≡ length s
update-length k v []             = refl
update-length k v ((k' , w) ∷ s) with eqℕ k k'
... | true  = refl
... | false = cong suc (update-length k v s)

------------------------------------------------------------------------
-- Test-first: a concrete store; lookup, its cost, and the bound.

module Examples where
  st : Store
  st = (0 , ⟨⟩) ∷ (1 , (⟨⟩ ∙ ⟨⟩)) ∷ (2 , ⟨⟩) ∷ []

  ex-lookup : lookup 2 st ≡ ⟨⟩
  ex-lookup = refl

  ex-cost : lookupC 2 st ≡ 3        -- walked all three entries
  ex-cost = refl

  ex-bound : lookupC 2 st ≤ length st
  ex-bound = lookupC-bound 2 st

  ex-update-len : length (update 1 ⟨⟩ st) ≡ length st
  ex-update-len = update-length 1 ⟨⟩ st
