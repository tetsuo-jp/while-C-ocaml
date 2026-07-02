{-# OPTIONS --safe #-}

------------------------------------------------------------------------
-- The dovetail COST SKELETON for Levin's optimal search (Levin plan Layer B3a).
--
-- `opt` runs phases 0,1,2,… ; phase `i` is a batch of candidate runs of total budget
-- `pow2 i` (cost model (A), see docs/LEVIN-B3-DESIGN.md), and the search stops at the
-- first phase whose oracle `done` reports a verified witness.  Because each phase `i`
-- costs exactly `pow2 i`, the cumulative cost over phases `0..k` is exactly
-- `LevinBudget.gsum k` BY CONSTRUCTION -- this is the `time_opt = gsum k` half that
-- plugs straight into `LevinGrounded.levin-grounded` (which supplies `gsum k < a_q·S`).
--
-- The module is parameterised by the phase oracle `done : ℕ → Bool`; its operational
-- correctness (that `done i` really reflects "a witness verified within budget 2^i")
-- is Layer B3b, deferred as a hypothesis.  Here we prove the purely arithmetic core:
-- if `k` is the LEAST phase with `done k = true`, the search finishes exactly at `k`
-- and its cost is `gsum k`.
------------------------------------------------------------------------

module LevinDovetail where

open import Data.Nat using (ℕ; zero; suc; _+_; _≤_; _<_; s≤s; z≤n; _≤ᵇ_)
open import Data.Nat.Properties
  using (≤-refl; ≤-trans; +-suc; +-identityʳ; <⇒≤; <⇒≱; 1+n≰n; <-cmp)
open import Data.Bool using (Bool; true; false; if_then_else_)
open import Data.Maybe using (Maybe; just; nothing)
open import Data.Product using (Σ; _×_; _,_)
open import Data.Empty using (⊥-elim)
open import Relation.Binary.Definitions using (tri<; tri≈; tri>)
open import Relation.Binary.PropositionalEquality using (_≡_; refl; cong; trans; subst)

open import LevinBudget using (gsum)

------------------------------------------------------------------------
-- The dovetail driver, over an abstract phase oracle.
------------------------------------------------------------------------

module Search (done : ℕ → Bool) where

  -- scan phases `i, i+1, …` (fuel-bounded) for the first success.
  find : ℕ → ℕ → Maybe ℕ            -- fuel, current phase
  find zero    _ = nothing
  find (suc f) i = if done i then just i else find f (suc i)

  -- cost of running phases 0..k: exactly gsum k in the batch model.
  time-opt : ℕ → ℕ
  time-opt k = gsum k

  -- core: from a start `s ≤ k` where every phase in [s,k) fails and `done k` holds,
  -- `find` (given enough fuel) returns exactly `k`.
  scan : ∀ k s f → s ≤ k → done k ≡ true
       → (∀ i → s ≤ i → i < k → done i ≡ false)
       → suc k ≤ s + f
       → find f s ≡ just k
  scan k s zero s≤k dk allf fuel =
    ⊥-elim (1+n≰n (≤-trans (subst (suc k ≤_) (+-identityʳ s) fuel) s≤k))
  scan k s (suc f) s≤k dk allf fuel with <-cmp s k
  ... | tri≈ _ s≡k _ rewrite trans (cong done s≡k) dk = cong just s≡k
  ... | tri< s<k _ _ rewrite allf s ≤-refl s<k =
          scan k (suc s) f s<k dk
               (λ i ss≤i i<k → allf i (<⇒≤ ss≤i) i<k)
               (subst (suc k ≤_) (+-suc s f) fuel)
  ... | tri> _ _ k<s = ⊥-elim (<⇒≱ k<s s≤k)

  -- least-success ⇒ finishes at k with fuel `suc k`.
  find-least : ∀ k → done k ≡ true → (∀ i → i < k → done i ≡ false)
             → find (suc k) 0 ≡ just k
  find-least k dk allf = scan k 0 (suc k) z≤n dk (λ i _ i<k → allf i i<k) ≤-refl

  -- THE "time_opt = gsum k" THEOREM (oracle-relative): if `k` is the least phase with
  -- `done k = true`, the search finishes at `k` (some fuel) with cost exactly `gsum k`.
  time-opt-eq : ∀ {k} → done k ≡ true → (∀ i → i < k → done i ≡ false)
              → Σ ℕ (λ f → find f 0 ≡ just k) × (time-opt k ≡ gsum k)
  time-opt-eq {k} dk allf = (suc k , find-least k dk allf) , refl

------------------------------------------------------------------------
-- Concrete non-vacuous instance: an oracle that first succeeds at phase 3.
------------------------------------------------------------------------

private
  done3 : ℕ → Bool
  done3 i = 3 ≤ᵇ i

  open Search done3

  _ : find 8 0 ≡ just 3
  _ = refl

  _ : time-opt 3 ≡ 15            -- gsum 3 = 1+2+4+8
  _ = refl
