{-# OPTIONS --safe #-}
------------------------------------------------------------------------
-- LEVIN'S OPTIMALITY -- the budget-doubling core (toward Jones Thm 20.1.2
-- `thm-levin`).
--
-- Levin's optimal search `opt` dovetails ALL programs, running program `i` under
-- a TIME BUDGET that doubles across phases, simulated by an EFFICIENT interpreter
-- (here the one-variable I self-interpreter `SelfInterp.sint`, which runs an
-- encoded program with only a constant per-step overhead).  Its optimality
-- `time_opt(x) ≤ a_q · (time_q(x) + time_r(...))` rests on two quantitative
-- facts; this module machine-checks the first (the doubling core):
--
--   THE TOTAL COST OF ALL PHASES `0..k` IS LESS THAN TWICE THE LAST PHASE.
--
-- i.e. `Σ_{i=0}^{k} 2^i = 2^{k+1} − 1 < 2·2^k`.  So once the optimal solver `q`
-- finishes in some phase `k` (when the budget `2^k` first exceeds what `q` needs),
-- the WHOLE dovetailed search up to that phase has cost < 2·2^k -- a constant
-- factor over `q`'s phase budget.  Combined with (i) the efficient interpreter's
-- constant per-step overhead and (ii) the doubling-overshoot bound (the least
-- `2^k ≥ N` is `< 2N` -- the companion fact), this yields Levin's constant `a_q`.
--
-- Referenced by docs/AGDA_CORRESPONDENCE.md and docs/BOOK-ROADMAP.md (Thm 20.1.2).
------------------------------------------------------------------------

module LevinBudget where

open import Data.Nat using (ℕ; zero; suc; _+_; _<_)
open import Data.Nat.Properties using (≤-refl; +-suc)
open import Relation.Binary.PropositionalEquality
  using (_≡_; refl; cong; sym; trans; subst)

------------------------------------------------------------------------
-- powers of two (by doubling, no multiplication needed) and the running total.
------------------------------------------------------------------------

pow2 : ℕ → ℕ
pow2 zero    = 1
pow2 (suc k) = pow2 k + pow2 k          -- 2^(k+1) = 2^k + 2^k

-- gsum k = Σ_{i=0}^{k} 2^i  (total budget spent over phases 0..k).
gsum : ℕ → ℕ
gsum zero    = 1
gsum (suc k) = pow2 (suc k) + gsum k

------------------------------------------------------------------------
-- Exact geometric sum: Σ_{i=0}^{k} 2^i = 2^{k+1} − 1.
------------------------------------------------------------------------

gsum-eq : ∀ k → suc (gsum k) ≡ pow2 (suc k)
gsum-eq zero    = refl
gsum-eq (suc k) =
  trans (sym (+-suc (pow2 (suc k)) (gsum k)))
        (cong (pow2 (suc k) +_) (gsum-eq k))

------------------------------------------------------------------------
-- THE DOUBLING CORE: the whole search up to phase k costs strictly less than
-- twice the last phase's budget (`pow2 (suc k) = 2 · pow2 k`).
------------------------------------------------------------------------

gsum<2last : ∀ k → gsum k < pow2 (suc k)
gsum<2last k = subst (λ m → gsum k < m) (gsum-eq k) ≤-refl

------------------------------------------------------------------------
-- Concrete check: phases 0..3 cost 1+2+4+8 = 15 = 2^4 − 1 < 16.
------------------------------------------------------------------------

_ : gsum 3 ≡ 15
_ = refl
