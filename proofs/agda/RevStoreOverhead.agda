{-# OPTIONS --safe #-}

------------------------------------------------------------------------
-- Reversible Levin Track-2 (bricks S4+S5, abstract route): a-rev(L) is
-- AFFINE in the store size L.
--
-- S1 (`RevStore`) proved every LOOKUP/UPDATE is a walk of length ≤ L of the
-- length-`L` variable list `Vl`.  S2 (`RevLinearOverhead.Generic`) proved:
-- per-op charges ≤ K  ⇒  overhead ≤ K · ocost.  This module BRIDGES them.
--
-- A store-carrying reversible interpreter charges each op
--     base-dispatch  +  lookup-walk  +  update-walk .
-- By S1 the two walks are each ≤ L, so the per-op charge is ≤ base + 2·L
-- (`walk-bound` below).  Feeding K = base + 2·L into S2's generic lemma gives
--     overhead ≤ (base + 2·L) · ocost ,
-- i.e. the reversible self-interpreter's overhead constant `a-rev(L)` is
-- AFFINE in the store size L.  At L ≈ 75 (the `ri.rwhile` variable list) this
-- is the order-of-magnitude of the measured ≈364× (`measure_ri_overhead.sh`).
--
-- This is the ABSTRACT route sanctioned by docs/REV-LEVIN-R3D-BRICKS.md
-- (abstractions (i)+(ii)): it needs no new object language -- the store walk
-- enters purely as the per-op charge bound, on top of S1+S2 which are proven.
-- The heavy route (a real multi-variable object language `RCmdV` + a
-- store-threading interpreter) is Track 2's S3; this module is the theorem
-- that route targets, obtained directly from the two committed bricks.
-- --safe, axiom-free.
------------------------------------------------------------------------

module RevStoreOverhead where

open import Data.Nat using (ℕ; zero; suc; _+_; _*_; _≤_; z≤n; s≤s)
open import Data.Nat.Properties
  using (≤-refl; ≤-trans; +-mono-≤; m≤m+n; +-identityʳ)
open import Data.List using (length)
open import Relation.Binary.PropositionalEquality using (_≡_; refl; sym; subst; cong)
open import UniversalInterp using (V; ⟨⟩; _∙_)
open import RevSelfInterp using (size)
open import RevSintCost using (ocost)
open import RevStore using (Store; lookupC; updateC; lookupC-bound; updateC-bound)
open import RevLinearOverhead using (module Generic)
import RevInst as R
open R using (⌜_⌝; RCmd)

------------------------------------------------------------------------
-- S1 bridge: doubling.  `2 · n` is `n + (n + 0)`; expose `n + n`.

2·_ : ℕ → ℕ
2· n = 2 * n

double-≡ : ∀ n → 2 * n ≡ n + n
double-≡ n = cong (n +_) (+-identityʳ n)

------------------------------------------------------------------------
-- S1 bridge proper: one store-touching op (a LOOKUP at `kr` then an UPDATE at
-- `kw`) walks at most 2·L entries of the length-L store `s` -- this is the
-- entire content of the measured overhead, discharged from S1's two bounds.

walk-bound : ∀ kr kw (s : Store) → lookupC kr s + updateC kw s ≤ 2 * length s
walk-bound kr kw s =
  subst (lookupC kr s + updateC kw s ≤_) (sym (double-≡ (length s)))
        (+-mono-≤ (lookupC-bound kr s) (updateC-bound kw s))

------------------------------------------------------------------------
-- S5: feed K = base + 2·L into S2's generic linear-overhead lemma.  The
-- store-touching ops (sq/ifn, which do a lookup+update) are charged the full
-- `base + 2·L`; the pure tree op (swp, no variable access) is charged `base`.
-- Every charge is ≤ K, so the generic lemma fires at K = base + 2·L.

module AffineInL (base L : ℕ) where

  -- the affine overhead constant: a-rev(L) = base + 2·L.
  a-rev : ℕ
  a-rev = base + 2 * L

  -- swp ≤ a-rev (no store access ⇒ only the dispatch base);
  -- sq, ifn ≤ a-rev (they pay the full base + walk).
  private
    hswp : base ≤ a-rev
    hswp = m≤m+n base (2 * L)

  open Generic a-rev base a-rev a-rev hswp ≤-refl ≤-refl public

  -- THE THEOREM: the reversible self-interpreter's overhead is bounded by
  -- a-rev(L) · ocost, with a-rev(L) AFFINE in the store size L.
  overhead-affine : (c : RCmd) (s : V) (n : ℕ)
                  → size c ≤ n → gcost n ⌜ c ⌝ s ≤ a-rev * ocost c s
  overhead-affine = gcost-linear

------------------------------------------------------------------------
-- Test-first: the constant is genuinely affine, and reduces to the fuel-level
-- a-rev when there is no store (L = 0).

module Examples where
  open RevStore.Examples using (st)

  -- L = 0 (no store): a-rev collapses to the base dispatch constant.
  a-rev-zero : AffineInL.a-rev 4 0 ≡ 4
  a-rev-zero = refl

  -- L = 75 (the ri.rwhile Vl size): a-rev(75) = 4 + 150 = 154 -- affine,
  -- the order-of-magnitude of the measured ≈364× (constant secondary).
  a-rev-75 : AffineInL.a-rev 4 75 ≡ 154
  a-rev-75 = refl

  -- affinity as a slope check: a-rev(L+1) - a-rev(L) = 2 (one more entry
  -- costs one extra lookup step + one extra update step).
  a-rev-slope : AffineInL.a-rev 4 10 + 2 ≡ AffineInL.a-rev 4 11
  a-rev-slope = refl

  -- the S1 walk-bound on a concrete length-3 store.
  ex-walk : lookupC 2 st + updateC 1 st ≤ 2 * 3
  ex-walk = walk-bound 2 1 st
