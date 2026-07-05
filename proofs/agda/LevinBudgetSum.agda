{-# OPTIONS --safe #-}

------------------------------------------------------------------------
-- B3d: the per-phase budget cost TELESCOPES to `gsum k` (Levin's cost floor).
--
-- `LevinBudgetE` proved each phase's real step count is `Tcost k` (≥ 2^k).
-- Running the budget gadget over phases 0..k costs `totalT k = Σ_{i≤k} Tcost i`,
-- and this is `≥ gsum k = Σ_{i≤k} 2^i` -- the literal realization of the Levin
-- optimality COST `time_opt ≥ gsum k`.  Together with `LevinBudgetTime.dbl-time`
-- (Tcost i IS the real `evalT` count at phase i) this makes the whole point of
-- Jones Thm 20.1.2's cost model concrete: the doubling budget, run over all
-- phases, really costs `gsum k`.
------------------------------------------------------------------------

module LevinBudgetSum where

open import Data.Nat using (ℕ; zero; suc; _+_; _≤_)
open import Data.Nat.Properties using (+-mono-≤)
open import LevinBudget using (pow2; gsum)
open import LevinBudgetE using (Tcost; pow2≤Tcost)

-- total budget cost over phases 0..k
totalT : ℕ → ℕ
totalT zero    = Tcost zero
totalT (suc k) = Tcost (suc k) + totalT k

-- `gsum` written the same shape, for the induction
--   (gsum 0 = 1 = pow2 0;  gsum (suc k) = pow2 (suc k) + gsum k)

-- THE COST FLOOR: the literal budget's total step count over phases 0..k is at
-- least `gsum k` -- the object-language realization of `time_opt ≥ gsum k`.
gsum≤totalT : ∀ k → gsum k ≤ totalT k
gsum≤totalT zero    = pow2≤Tcost zero                       -- 1 ≤ 3
gsum≤totalT (suc k) = +-mono-≤ (pow2≤Tcost (suc k)) (gsum≤totalT k)
