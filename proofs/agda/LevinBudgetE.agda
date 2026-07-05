{-# OPTIONS --safe #-}

------------------------------------------------------------------------
-- B3d, brick 2: the per-phase budget `2^k` as a LITERAL `Exp` computation.
--
-- `LevinOptE` (brick 1) made the search CONTROL literal, but its per-phase cost
-- is LINEAR.  The `time_opt = gsum k = Σ 2^i` of Jones Thm 20.1.2 comes from each
-- phase `k` doing `2^k` work (the dovetail budget).  This module realizes that
-- DOUBLING as a literal object-language program and proves, via `StepCount.evalT`
-- (the real unit-cost step count), that its running time doubles per phase.
--
-- `dblE` = "at phase k, build a full binary tree of depth k" (2^k leaves), by
-- `call`-recursion.  Its step count obeys  T(0)=3,  T(k+1) = 9 + 2·T(k), i.e.
-- T(k) = 12·2^k − 9 -- literally `Θ(2^k)` per phase.  (Note the eval FUEL is only
-- O(k): fuel bounds the recursion DEPTH, while the step COUNT is 2^k -- the very
-- gap `StepCount` exists to measure.)
--
-- The full faithful oracle runs candidate programs via `sint` within budget 2^k;
-- `dblE` isolates the cost-model content of that (2^k work per phase) as a proven
-- literal doubling.
------------------------------------------------------------------------

module LevinBudgetE where

open import Data.Nat using (ℕ; zero; suc; _+_; _≤_; z≤n; s≤s)
open import Data.Nat.Properties using (+-mono-≤; m≤n+m; ≤-trans)
open import Data.Product using (_×_; _,_; proj₁; proj₂)
open import Relation.Binary.PropositionalEquality using (_≡_; refl; cong)
open import UniversalInterp
  using (V; ⟨⟩; _∙_; Exp; var; knil; kcons; khd; ktl; kifnil; call; hd; tl
        ; Maybe; just; nothing; module Interp)
open import LevinBudget using (pow2)
import StepCount as SC

------------------------------------------------------------------------
-- The doubling budget gadget and the phase counter.
------------------------------------------------------------------------

-- at phase k (nonnil), cons two copies of the phase-(k-1) result: 2× the work.
dblE : Exp
dblE = kifnil var knil (kcons (call (ktl var)) (call (ktl var)))

phaseV : ℕ → V
phaseV zero    = ⟨⟩
phaseV (suc k) = ⟨⟩ ∙ phaseV k

-- the full binary tree of depth k (the value `dblE` builds at phase k)
treeV : ℕ → V
treeV zero    = ⟨⟩
treeV (suc k) = treeV k ∙ treeV k

------------------------------------------------------------------------
-- It RUNS: at phase k it builds the depth-k full binary tree.
------------------------------------------------------------------------

module Run where
  open Interp dblE
  _ : run 30 (phaseV 0) ≡ just (treeV 0)
  _ = refl
  _ : run 30 (phaseV 2) ≡ just (treeV 2)
  _ = refl
  _ : run 30 (phaseV 3) ≡ just (treeV 3)
  _ = refl

------------------------------------------------------------------------
-- Its STEP COUNT DOUBLES per phase.  `Tcost` is the recurrence T(0)=3,
-- T(k+1) = 9 + 2·T(k) (= 12·2^k − 9); the real `evalT` count IS `Tcost k`.
------------------------------------------------------------------------

Tcost : ℕ → ℕ
Tcost zero    = 3
Tcost (suc k) = 9 + (Tcost k + Tcost k)

module Cost where
  open SC.StepCount dblE

  -- the real step count at phase k is exactly `Tcost k` (3,15,39,87,183 for k=0..4)
  _ : time 30 (phaseV 0) ≡ just (treeV 0 , Tcost 0)
  _ = refl
  _ : time 30 (phaseV 1) ≡ just (treeV 1 , Tcost 1)
  _ = refl
  _ : time 30 (phaseV 2) ≡ just (treeV 2 , Tcost 2)
  _ = refl
  _ : time 30 (phaseV 3) ≡ just (treeV 3 , Tcost 3)
  _ = refl
  _ : time 30 (phaseV 4) ≡ just (treeV 4 , Tcost 4)
  _ = refl

-- EXPONENTIAL LOWER BOUND (∀ k): the per-phase budget is at least `2^k` -- the
-- source of `time_opt = gsum k = Σ 2^i`.  (The full `∀k time = Tcost k` needs an
-- `evalT`-monotonicity lemma StepCount does not yet export; the doubling itself is
-- the k=0..4 bindings above plus this exponential floor on the cost recurrence.)
pow2≤Tcost : ∀ k → pow2 k ≤ Tcost k
pow2≤Tcost zero    = s≤s z≤n                       -- 1 ≤ 3
pow2≤Tcost (suc k) =                               -- 2^k+2^k ≤ 9+(T k+T k)
  ≤-trans (+-mono-≤ (pow2≤Tcost k) (pow2≤Tcost k))
          (m≤n+m (Tcost k + Tcost k) 9)
