{-# OPTIONS --safe #-}

------------------------------------------------------------------------
-- Reversible Levin Track-2 (traversal-charge plan task 5): REVERSIBLE
-- LEVIN OPTIMALITY, GROUNDED AT THE WALK-CHARGED INTERPRETER CONSTANT --
-- the `RevLevinGrounded` twin with `a-rev = 4` (driver level, no store)
-- replaced by the task-4 theorem `a-revS L = 2 + 2L` (interpreter level,
-- store walk + index decode CHARGED, side condition k < L).
--
-- `RevLevinGrounded` fed the machine-checked driver constant into Layer A;
-- `RCmdVWalkCost.rev-levin-walk` fed the OBJECT-level walk constant
-- a-revW(L) = 1 + L.  This module completes the family: the constant fed
-- into Layer A is the SELF-INTERPRETER'S OWN walk-charged step count
-- (`RevStoreSintCost.usintVC-affine`), so the reversible Levin statement
-- holds with the interpreter overhead a THEOREM at the traversal charge:
--
--     gsum k < 4 · (2 + 2L) · 2^j · (T_q + T_r) .
--
-- This supersedes `RevStoreOverhead.AffineInL`'s assumed per-op envelope:
-- the same affine shape (base + 2L, here base = 2) now arrives as a
-- theorem about a real store-carrying self-interpreter over a real
-- multi-variable language.  --safe, axiom-free.
------------------------------------------------------------------------

module RevLevinGroundedWalk where

open import Data.Nat using (ℕ; suc; _+_; _*_; _≤_; _<_; s≤s; z≤n)
open import Data.List using (length)
open import Data.Product using (Σ; _,_)
open import Relation.Binary.PropositionalEquality
  using (_≡_; refl; sym; subst)

open import RevStore using (Store)
open import RCmdV using (RCmdV; ocostV)
open import RCmdVEnc using (⌜_⌝V)
open import RevStoreInterp using (sizeV)
open import RevStoreSintCost using (usintVC; varsBelow; a-revS; usintVC-affine)
open import LevinBudget using (gsum)
open import LevinOptimal using (LevinSearch; aq; c; S)
import RevLevinAbstract as RLA

------------------------------------------------------------------------
-- The walk-charged linear-overhead property at store size L, as a
-- proposition on the constant `ov`: self-interpreting any command over
-- any length-L store (enough fuel, indices below L) costs at most
-- `ov · ocostV` of the interpreter's own walk-charged steps.

LinearOverheadRevWalk : ℕ → ℕ → Set
LinearOverheadRevWalk ov L =
  (cc : RCmdV) (s : Store) (n : ℕ)
  → length s ≡ L → sizeV cc ≤ n → varsBelow cc L
  → usintVC n ⌜ cc ⌝V s ≤ ov * ocostV cc s

-- ... and the task-4 theorem is exactly an inhabitant at ov = a-revS L.
overhead-grounded-walk : (L : ℕ) → LinearOverheadRevWalk (a-revS L) L
overhead-grounded-walk L cc s n eq le vb =
  subst (λ l → usintVC n ⌜ cc ⌝V s ≤ a-revS l * ocostV cc s) eq
        (usintVC-affine cc s n le
          (subst (varsBelow cc) (sym eq) vb))

------------------------------------------------------------------------
-- Feed the proven constant into Layer A's abstract reversible search
-- record (parametric in the overhead field, so it fires verbatim).

rev-search-walkS : (L j Tq Tr : ℕ) → LevinSearch
rev-search-walkS L j Tq Tr = RLA.RevSearch.rev-search (a-revS L) j Tq Tr

-- the overhead field is literally the interpreter-level walk constant.
overhead-is-a-revS : ∀ L j Tq Tr
  → LevinSearch.overhead (rev-search-walkS L j Tq Tr) ≡ a-revS L
overhead-is-a-revS _ _ _ _ = refl

------------------------------------------------------------------------
-- REVERSIBLE LEVIN OPTIMALITY at the walk-charged interpreter constant:
-- for every solver of index j and times T_q, T_r over a length-L store,
-- the reversible dovetail finishes a phase of cumulative cost
-- < 4·(2+2L)·2^j·(T_q+T_r) -- with the constant a theorem (task 4), not
-- an assumption (RevStoreOverhead) and not object-level only (task 1).

rev-levin-grounded-walk : ∀ L j Tq Tr
  → 1 ≤ c (rev-search-walkS L j Tq Tr) * S (rev-search-walkS L j Tq Tr)
  → Σ ℕ (λ k → gsum k < aq (rev-search-walkS L j Tq Tr)
                       * S (rev-search-walkS L j Tq Tr))
rev-levin-grounded-walk L j Tq Tr =
  RLA.RevSearch.rev-levin-optimal (a-revS L) j Tq Tr

------------------------------------------------------------------------
-- Test-first: the constant at the fibres the paper quotes, and a concrete
-- optimality instance.

module Examples where

  -- L = 0: the interpreter still pays dispatch + empty walks -- base 2.
  ex-L0 : a-revS 0 ≡ 2
  ex-L0 = refl

  -- L = 2 (the standard two-variable examples): 2 + 4.
  ex-L2 : LevinSearch.overhead (rev-search-walkS 2 0 0 0) ≡ 6
  ex-L2 = refl

  -- L = 75 (the ri.rwhile Vl size): 2 + 150 = 152.
  ex-75 : LevinSearch.overhead (rev-search-walkS 75 0 0 0) ≡ 152
  ex-75 = refl

  -- concrete optimality: L = 2, index 0, T_q = 3, T_r = 1.
  ex-opt : Σ ℕ (λ k → gsum k < aq (rev-search-walkS 2 0 3 1)
                             * S (rev-search-walkS 2 0 3 1))
  ex-opt = rev-levin-grounded-walk 2 0 3 1 (s≤s z≤n)
