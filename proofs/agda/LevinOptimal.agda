{-# OPTIONS --safe #-}

------------------------------------------------------------------------
-- LEVIN'S OPTIMALITY as an abstract meta-theorem (Jones Thm 20.1.2, Layer A).
--
-- `LevinBudget` proved the two quantitative halves of Levin's optimal search:
--   * `gsum<2last`  -- the whole dovetail up to phase k costs < twice the last;
--   * `overshoot`   -- the phase whose budget first covers a target overshoots by
--                      at most a factor 2.
-- This module assembles them into the optimality inequality, with the two
-- operational facts left as HYPOTHESES (the repo's abstract-record pattern, cf.
-- `FutamuraProjections` relative to `mix-eq`, `HaltingUndecidable` relative to
-- `build-diagonal`):
--   * `overhead a` -- the efficient self-interpreter runs one simulated step in ≤ a
--                     real steps (Thm 19.1.3 `int-efficient`; Layer B1);
--   * `idx j`      -- q's position in the program enumeration (Layer B2).
--
-- THEOREM (`levin-optimal`).  For every solver `q` (index `j`, running time `T_q`,
-- checked by `r` in `T_r`), the total cost of Levin's dovetailed search up to the
-- phase that finds & verifies q's witness is
--        gsum k  <  a_q · (T_q + T_r),   with a_q = 4 · a · 2^j,
-- a constant factor over q -- independent of the input `x`.  This is Levin's
-- theorem relative to the linear interpreter overhead `a` and the index `j`.
--
-- See docs/LEVIN-PLAN.md; Layer B discharges `a`/`j` for the I language.
------------------------------------------------------------------------

module LevinOptimal where

open import Data.Nat using (ℕ; zero; suc; _+_; _*_; _≤_; _<_; s≤s; z≤n)
open import Data.Nat.Properties using (≤-trans; +-mono-≤; *-distribʳ-+)
open import Data.Product using (Σ; _×_; _,_)
open import Relation.Binary.PropositionalEquality
  using (_≡_; refl; sym; trans; cong₂; subst)

open import LevinBudget using (pow2; gsum; gsum<2last; overshoot)

------------------------------------------------------------------------
-- The finishing lemma (pure arithmetic).  For any target `N ≥ 1`, the dovetailed
-- search finishes in some phase `k` whose cumulative cost `gsum k` is < 4·N.
-- (The phase `k` is the first whose budget `2^k` covers `N`; `overshoot` bounds
-- `2^k ≤ 2N`, and `gsum<2last` bounds the cumulative cost by `2·2^k ≤ 4N`.)
------------------------------------------------------------------------

finish : ∀ N → 1 ≤ N → Σ ℕ (λ k → gsum k < (N + N) + (N + N))
finish (suc n) _ with overshoot n
... | k , _ , pk≤2N = k , ≤-trans (gsum<2last k) (+-mono-≤ pk≤2N pk≤2N)
finish zero ()

------------------------------------------------------------------------
-- The abstract Levin search: the two operational constants as fields.
------------------------------------------------------------------------

record LevinSearch : Set where
  field
    overhead : ℕ    -- a : efficient-interpreter overhead (real steps / simulated step)
    idx      : ℕ    -- j : q's enumeration index
    time-q   : ℕ    -- T_q = time_q(x)   (q halts on x)
    time-r   : ℕ    -- T_r = time_r(x, q(x))   (r halts)

module _ (M : LevinSearch) where
  open LevinSearch M

  S : ℕ                       -- work q + r account for
  S = time-q + time-r

  c : ℕ                       -- a · 2^j  (per-solver constant, before the factor 4)
  c = overhead * pow2 idx

  aq : ℕ                      -- a_q = 4 · a · 2^j
  aq = (c + c) + (c + c)

  -- a_q · S = 4·(c·S) laid out as the two doubled halves `finish` produces.
  aq*S : aq * S ≡ ((c * S) + (c * S)) + ((c * S) + (c * S))
  aq*S = trans (*-distribʳ-+ S (c + c) (c + c))
               (cong₂ _+_ (*-distribʳ-+ S c c) (*-distribʳ-+ S c c))

  -- LEVIN OPTIMALITY.  The cumulative cost of the search up to the finishing phase
  -- is bounded by a_q·(T_q + T_r) -- optimal up to the constant a_q = 4·a·2^j.
  -- The hypothesis `1 ≤ c · S` says the target cost is positive (a ≥ 1 and some
  -- work to do), always true for a real solver.
  levin-optimal : 1 ≤ c * S → Σ ℕ (λ k → gsum k < aq * S)
  levin-optimal pos with finish (c * S) pos
  ... | k , bnd = k , subst (gsum k <_) (sym aq*S) bnd

------------------------------------------------------------------------
-- Concrete instance: a = 1, j = 0 (2^0 = 1), T_q = 3, T_r = 1
--   ⇒ c = 1, S = 4, a_q = 4, target = 4, bound a_q·S = 16.
-- The theorem then delivers a phase whose cumulative cost is < 16.
------------------------------------------------------------------------

ex : LevinSearch
ex = record { overhead = 1 ; idx = 0 ; time-q = 3 ; time-r = 1 }

_ : aq ex ≡ 4
_ = refl

_ : Σ ℕ (λ k → gsum k < 16)
_ = levin-optimal ex (s≤s z≤n)
