{-# OPTIONS --safe #-}

------------------------------------------------------------------------
-- B3d, brick 3: the GENERAL doubling theorem for `LevinBudgetE.dblE`.
--
-- `LevinBudgetE` proved `time F (phaseV k) ≡ just (treeV k , Tcost k)` only for
-- k = 0..4 (by `refl`) plus the exponential floor `pow2 k ≤ Tcost k`.  With the
-- fuel-monotonicity of the step-counting interpreter (`StepCountMono`), the full
-- ∀k statement goes through by induction: at phase k, `dblE`'s REAL unit-cost
-- step count (`StepCount.evalT`) is exactly `Tcost k` = 12·2^k − 9 -- the Θ(2^k)
-- per-phase budget of Jones Thm 20.1.2, as a theorem about a literal program.
------------------------------------------------------------------------

module LevinBudgetTime where

open import Data.Nat using (ℕ; zero; suc; _+_; _∸_; _≤_)
open import Data.Nat.Properties using (m∸n+n≡m; m≤n+m; +-suc)
open import Data.Product using (Σ; _,_)
open import Relation.Binary.PropositionalEquality
  using (_≡_; refl; cong; trans; subst)
open import UniversalInterp using (_∙_; just)
open import LevinBudgetE using (dblE; phaseV; treeV; Tcost)
import StepCount as SC
import StepCountMono as SCM

open SC.StepCount  dblE using (evalT; time)
open SCM.Mono      dblE using (add-fuelT)

-- lift a defined step-counting run to any larger fuel (≤-based wrapper around
-- `add-fuelT`, whose fuel has the exact shape `k + n`).
timeT-≤ : ∀ {m n} x {r} → m ≤ n
        → evalT m dblE x ≡ just r → evalT n dblE x ≡ just r
timeT-≤ {m} {n} x {r} m≤n h =
  subst (λ z → evalT z dblE x ≡ just r) (m∸n+n≡m m≤n)
        (add-fuelT (n ∸ m) m dblE x h)

-- the count bookkeeping of one unfolding: 6 + (t + (3 + t)) ≡ 9 + (t + t).
arith : ∀ t → t + (3 + t) ≡ 3 + (t + t)
arith t = trans (+-suc t (2 + t))
                (cong suc (trans (+-suc t (1 + t)) (cong suc (+-suc t t))))

------------------------------------------------------------------------
-- THE THEOREM: dblE's step count at phase k is exactly Tcost k, for every k.
------------------------------------------------------------------------

dbl-time : ∀ k → Σ ℕ (λ F → time F (phaseV k) ≡ just (treeV k , Tcost k))
dbl-time zero = 4 , refl
dbl-time (suc j) with dbl-time j
... | F' , ev = 5 + F' , goal
  where
    -- the two `call (ktl var)` sub-runs recurse at fuel 2 + F'; lift the IH there.
    ev' : evalT (2 + F') dblE (phaseV j) ≡ just (treeV j , Tcost j)
    ev' = timeT-≤ (phaseV j) (m≤n+m F' 2) ev

    goal : evalT (5 + F') dblE (phaseV (suc j))
         ≡ just (treeV (suc j) , Tcost (suc j))
    goal rewrite ev' =
      cong (λ z → just (treeV j ∙ treeV j , 6 + z)) (arith (Tcost j))

------------------------------------------------------------------------
-- B3d COST COMPLETION: the literal doubling budget realizes Levin's cost floor.
-- Over phases 0..k, each phase i costs (in real `evalT` steps) exactly `Tcost i`
-- (`dbl-time`), and the total `totalT k = Σ Tcost i` is `≥ gsum k` (`gsum≤totalT`)
-- -- the object-language realization of `time_opt ≥ gsum k` (Jones Thm 20.1.2).
------------------------------------------------------------------------

open import Data.Product using (_×_)
open import LevinBudget using (gsum)
open import LevinBudgetSum using (totalT; gsum≤totalT)

budget-realizes-gsum-floor :
  ∀ k → (∀ i → Σ ℕ (λ F → time F (phaseV i) ≡ just (treeV i , Tcost i)))  -- real per-phase cost = Tcost i
      × (gsum k ≤ totalT k)                                               -- summed, ≥ gsum k (the floor)
budget-realizes-gsum-floor k = dbl-time , gsum≤totalT k
