{-# OPTIONS --safe #-}

------------------------------------------------------------------------
-- END-TO-END operational Levin optimality (Levin plan Layer B3c).
--
-- Combines the two proven halves at the finishing phase `k`:
--   * `LevinDovetail` (B3a): the operational search halts exactly at `k`, and its real
--     running cost is `time_opt k = gsum k` (the batch cost model);
--   * `LevinGrounded` (Phase 4 / Layer A ∘ B1): `gsum k < a_q · (T_q + T_r)`, with the
--     interpreter overhead `a = 73` a THEOREM (`int-efficient`).
-- So the operational dovetailing search's real running time is Levin-optimal:
--        time_opt(x) = gsum k  <  a_q·(T_q + T_r),   a_q = 4·73·2^j.
--
-- This is proved *relative to the phase oracle* `done` finishing (least) at the phase
-- `k` where the Layer-A bound holds -- that oracle correctness is Layer B3b (the sole
-- remaining operational discharge; abstract-record pattern, see docs/LEVIN-B3-DESIGN.md).
-- The `bnd` premise is exactly what `LevinGrounded.levin-grounded` produces for `k`.
------------------------------------------------------------------------

module LevinComplete where

open import Data.Nat using (ℕ; zero; suc; _*_; _<_; _≤_; _≤?_; s≤s; z≤n)
open import Data.Nat.Properties using (≤-<-trans)
open import Data.Bool using (Bool; true; false)
open import Data.Maybe using (just)
open import Data.Product using (Σ; _×_; _,_)
open import Relation.Binary.PropositionalEquality using (_≡_; refl)
open import Relation.Nullary.Decidable using (True; toWitness)
open import Data.Unit using (tt)

open import LevinBudget using (gsum; gsum-mono)
open import LevinOptimal using (aq; S)
open import LevinGrounded using (search)
open import LevinDovetail using (module Search)

------------------------------------------------------------------------
-- THE END-TO-END THEOREM (oracle-relative).
------------------------------------------------------------------------

module _ (done : ℕ → Bool) where
  open Search done

  -- Given the Layer-A optimality bound `bnd` at phase `k` (from `levin-grounded`) and
  -- the oracle finishing least at `k` (B3b), the operational search halts at `k` with
  -- real cost `gsum k`, which is Levin-optimal.
  levin-complete : ∀ j Tq Tr {k}
    → gsum k < aq (search j Tq Tr) * S (search j Tq Tr)
    → done k ≡ true → (∀ i → i < k → done i ≡ false)
    → (Σ ℕ λ f → find f 0 ≡ just k)                        -- halts at phase k
      × (time-opt k ≡ gsum k)                              -- with real cost gsum k
      × (gsum k < aq (search j Tq Tr) * S (search j Tq Tr))-- which is Levin-optimal
  levin-complete j Tq Tr {k} bnd dk allf =
    (suc k , find-least k dk allf) , refl , bnd

  -- The GENERAL form (B3b-ready): the operational finish `kop` need only be `≤` the
  -- abstract bound phase `k` (a faster program may verify earlier -- still optimal, since
  -- `gsum kop ≤ gsum k`).  Takes just `done k ≡ true` and the Layer-A bound at `k`; the
  -- search itself locates the least success `kop` (via `find-finish`), and `gsum-mono`
  -- carries the bound down.  This is exactly the shape B3b's finishing lemma produces.
  levin-operational : ∀ j Tq Tr {k}
    → gsum k < aq (search j Tq Tr) * S (search j Tq Tr)
    → done k ≡ true
    → Σ ℕ (λ kop → (Σ ℕ λ f → find f 0 ≡ just kop)            -- halts at kop ≤ k
                    × (time-opt kop ≡ gsum kop)                -- real cost gsum kop
                    × (gsum kop < aq (search j Tq Tr) * S (search j Tq Tr)))  -- Levin-optimal
  levin-operational j Tq Tr {k} bnd dk with find-finish k dk
  ... | kop , halts , kop≤k , dkop =
        kop , halts , refl , ≤-<-trans (gsum-mono kop≤k) bnd

------------------------------------------------------------------------
-- A concrete non-vacuous instance.  Solver (j=0, T_q=3, T_r=1): a_q = 292, S = 4,
-- a_q·S = 1168.  Take an oracle first succeeding at phase k = 9 (gsum 9 = 1023 < 1168).
-- The whole chain fires: the search halts at 9, real cost gsum 9 = 1023 < 1168.
------------------------------------------------------------------------

-- a Bool "k ≤ i" test that reduces definitionally on suc/suc (unlike stdlib `_≤ᵇ_`,
-- which routes through `_<ᵇ_`).
geb : ℕ → ℕ → Bool
geb zero    _       = true
geb (suc k) zero    = false
geb (suc k) (suc i) = geb k i

-- if i < k then `geb k i` is false (all phases below the finish phase fail).
geb-false : ∀ k i → i < k → geb k i ≡ false
geb-false zero    i       ()
geb-false (suc k) zero    _         = refl
geb-false (suc k) (suc i) (s≤s i<k) = geb-false k i i<k

private
  doneK : ℕ → Bool
  doneK i = geb 9 i

  le : ∀ {m n} → True (m ≤? n) → m ≤ n
  le = toWitness

  _ : (Σ ℕ λ f → Search.find doneK f 0 ≡ just 9)
      × (Search.time-opt doneK 9 ≡ gsum 9)
      × (gsum 9 < aq (search 0 3 1) * S (search 0 3 1))
  _ = levin-complete doneK 0 3 1 (le tt) refl (λ i i<9 → geb-false 9 i i<9)

  -- the general form only needs `done 9 ≡ true` and the bound; the search LOCATES kop=9.
  ex-op = levin-operational doneK 0 3 1 {9} (le tt) refl
