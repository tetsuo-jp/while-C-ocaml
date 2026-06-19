{-# OPTIONS --safe #-}
------------------------------------------------------------------------
-- Correctness of mix.while's dynamic-`if` store MERGE (the `'doif` dynamic
-- arm + the `merge` procedure).  Referenced by docs/FUTAMURA.md.
--
-- When a conditional's guard is dynamic, mix specialises BOTH branches from a
-- snapshot store, giving per-slot values v1 (then) and v2 (else), and merges:
--   * a slot equal in both branches is KEPT (no residual fix-up);
--   * a slot that DIFFERS is dynamicised ('D.(var k)) and a fix-up assignment
--     `(var k) := lift(branch value)` is appended to EACH branch.
-- At run time the residual `if` takes one branch, runs its fix-ups, and the
-- store must then hold that branch's value in every slot.
--
-- We model one slot (the merge is pointwise) and prove:
--   * merge-correct  : with both fix-ups, the post-`if` value equals the value
--                      the taken branch computed, for either branch;
--   * merge-needs-both : dropping the else fix-up leaves a differing slot
--                      reading a stale placeholder on the else branch.
------------------------------------------------------------------------

module MixMerge where

open import Data.Bool using (Bool; true; false)
open import Relation.Binary.PropositionalEquality using (_≡_; refl; sym; trans)
open import Relation.Nullary using (¬_)

module Spec (V : Set) where

  -- A slot after specialising both branches: either they agreed on a value, or
  -- they diverged with then-value v1 and else-value v2.
  data Slot : Set where
    agreed   : V → Slot
    diverged : V → V → Slot

  -- The value the taken branch (true = then, false = else) intends for the slot.
  intended : Slot → Bool → V
  intended (agreed v)     _     = v
  intended (diverged v1 v2) true  = v1
  intended (diverged v1 v2) false = v2

  -- Post-`if` value under the CORRECT merge: agreed slots keep their value (no
  -- fix-up); diverged slots are materialised by the per-branch fix-up.
  postOK : Slot → Bool → V
  postOK (agreed v)     _     = v
  postOK (diverged v1 v2) true  = v1     -- then fix-up: (var k) := lift v1
  postOK (diverged v1 v2) false = v2     -- else fix-up: (var k) := lift v2

  merge-correct : ∀ sl b → postOK sl b ≡ intended sl b
  merge-correct (agreed v)      true  = refl
  merge-correct (agreed v)      false = refl
  merge-correct (diverged _ _)  true  = refl
  merge-correct (diverged _ _)  false = refl

  ----------------------------------------------------------------------
  -- A buggy merge that emits only the THEN fix-up: on the else branch a
  -- divergent slot is never re-materialised, so it reads the stale dynamicised
  -- placeholder `p` (= the residual var's current value), not v2.
  postBuggy : V → Slot → Bool → V
  postBuggy p (agreed v)      _     = v
  postBuggy p (diverged v1 v2) true  = v1
  postBuggy p (diverged v1 v2) false = p    -- BUG: missing else fix-up

  merge-needs-both : ∀ p v1 v2 → postBuggy p (diverged v1 v2) false ≡ p
  merge-needs-both p v1 v2 = refl

  -- Consequence: whenever the placeholder differs from the else value, the
  -- buggy merge is wrong on the else branch.
  merge-buggy-wrong : ∀ p v1 v2 → ¬ (p ≡ v2)
                    → ¬ (postBuggy p (diverged v1 v2) false ≡ intended (diverged v1 v2) false)
  merge-buggy-wrong p v1 v2 p≢v2 eq = p≢v2 (trans (sym (merge-needs-both p v1 v2)) eq)
