{-# OPTIONS --safe #-}
------------------------------------------------------------------------
-- ONLINE-TERMINATION: the unroll-budget + generalization half (gap G2 / row 37).
--
-- `Cogen.ml --online` keeps specialization finite two ways: BTA widening
-- (proven: `BTAWiden`) and, per configuration, a bounded UNROLL with a
-- GENERALIZATION fallback (was differential: `test-terminate`).  This module
-- pins the second half.
--
-- The driver, at a configuration, either UNROLLS (spending one unit of the
-- unroll budget) or, when the budget is exhausted, GENERALIZES one still-static
-- slot to dynamic and resets the budget.  Model the state as `(s , b)`: `s`
-- still-generalizable static slots, `b` remaining unroll budget.  The process is
-- TOTAL — Agda accepts the recursion by the lexicographic measure `(s , b)`:
-- unrolling strictly drops `b` (same `s`), generalizing strictly drops `s`.  So
-- the online driver terminates.  Moreover it does EXACTLY `s` generalizations
-- (one per static slot), regardless of the budget — the monotone-bounded
-- argument (cf. `WhistleTerminates`) made exact, now with unrolling interleaved —
-- and at most `b + s·(1+B0)` steps total (`drive-bound`), an a-priori bound.
--
-- Referenced by docs/AGDA_CORRESPONDENCE.md (gap G2).
------------------------------------------------------------------------

module OnlineTerminate where

open import Data.Nat using (ℕ; zero; suc; _+_; _*_; _≤_; z≤n; s≤s)
open import Relation.Binary.PropositionalEquality using (_≡_; refl; cong)

-- The unroll budget reset value B0 is a parameter (any fixed bound works).
module Driver (B0 : ℕ) where

  ------------------------------------------------------------------------
  -- The driver as a TOTAL function: `drive s b` = number of steps to a fully
  -- generalized (terminating) configuration.  Totality = termination, by the
  -- lexicographic measure (s , b).
  ------------------------------------------------------------------------
  drive : ℕ → ℕ → ℕ
  drive zero    b       = zero                       -- all slots dynamic: the loop folds (done)
  drive (suc s) zero    = suc (drive s B0)           -- budget out ⇒ generalize one slot, reset budget
  drive (suc s) (suc b) = suc (drive (suc s) b)      -- budget left ⇒ unroll

  ------------------------------------------------------------------------
  -- The number of GENERALIZATIONS the driver performs.
  ------------------------------------------------------------------------
  gens : ℕ → ℕ → ℕ
  gens zero    b       = zero
  gens (suc s) zero    = suc (gens s B0)             -- this step generalizes
  gens (suc s) (suc b) = gens (suc s) b              -- unrolling does not generalize

  -- EXACTLY `s` generalizations: one per static slot, independent of the budget.
  gens-count : ∀ s b → gens s b ≡ s
  gens-count zero    b       = refl
  gens-count (suc s) zero    = cong suc (gens-count s B0)
  gens-count (suc s) (suc b) = gens-count (suc s) b

  ------------------------------------------------------------------------
  -- A closed termination bound: at most `b + s·(1+B0)` steps.  Between two
  -- generalizations there are at most B0+1 unroll steps, and there are s
  -- generalizations — so the step count is finite and bounded a priori.
  ------------------------------------------------------------------------
  drive-bound : ∀ s b → drive s b ≤ b + s * suc B0
  drive-bound zero    b       = z≤n
  drive-bound (suc s) zero    = s≤s (drive-bound s B0)
  drive-bound (suc s) (suc b) = s≤s (drive-bound (suc s) b)

  ------------------------------------------------------------------------
  -- Concrete checks.
  ------------------------------------------------------------------------
  _ : gens 3 5 ≡ 3
  _ = gens-count 3 5

-- With a concrete budget B0 = 2 the driver computes: 2 static slots, budget 3 ⇒
-- exactly 7 steps and 2 generalizations.
module Example where
  open Driver 2

  _ : drive 2 3 ≡ 7
  _ = refl

  _ : gens 2 3 ≡ 2
  _ = refl
