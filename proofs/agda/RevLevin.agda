{-# OPTIONS --safe #-}

------------------------------------------------------------------------
-- Reversible Levin R3e / Track 1 (brick E2): THE REVERSIBLE LEVIN THEOREM,
-- driver level -- the time↔garbage trade-off, assembled into one statement.
--
-- Bundles the three machine-checked legs for a search problem `wit : X → Y`:
--   * TIME-optimality at a PROVEN constant (E1 `RevLevinGrounded.rev-levin-grounded`,
--     a-rev = 4 a theorem): gsum k < 4·a-rev·2^j·(T_q+T_r).
--   * garbage LOWER bound (R0c): if `wit` merges two instances, NO clean reversible
--     search exists (`RevLevinAbstract.no-clean-rev-search`).
--   * garbage UPPER bound (R0b): the Bennett embedding ⟨id,wit⟩ is injective and the
--     input is always recoverable (garbage ≤ |input|).
--
-- Together: the reversible Levin search is TIME-OPTIMAL at a proven reversible
-- constant AND its garbage is bracketed |fibre| ≤ g ≤ |input| -- optimality and
-- cleanliness are jointly unachievable.  This is the novel content beyond Jones,
-- now CLAIMABLE at the driver level (interpreter = the semantic `usint`).  The
-- program-level `rsint` and the 364× store-walk model are Track 2/3 fidelity
-- upgrades (as classical B3d is to Thm 20.1.2).  --safe, axiom-free.
------------------------------------------------------------------------

module RevLevin where

open import Data.Nat using (ℕ; _*_; _≤_; _<_; s≤s; z≤n)
open import Data.Product using (Σ; _×_; _,_; proj₁)
open import Relation.Nullary using (¬_)
open import Relation.Binary.PropositionalEquality using (_≡_; refl; cong)

open import LevinBudget using (gsum)
open import LevinOptimal using (aq; c; S)
open import RevSpecGarbage using (clean-reversible)
open import RevLevinGrounded using (rev-search; rev-levin-grounded)
open import RevLevinAbstract using (no-clean-rev-search; constant-witness; no-clean-constant)

------------------------------------------------------------------------
-- The reversible Levin theorem for a search problem `wit` at solver params j,Tq,Tr.

record ReversibleLevin {X Y : Set} (wit : X → Y) (j Tq Tr : ℕ) : Set where
  field
    -- TIME-optimality, overhead constant a THEOREM (E1).
    time-optimal : 1 ≤ c (rev-search j Tq Tr) * S (rev-search j Tq Tr) →
                   Σ ℕ (λ k → gsum k < aq (rev-search j Tq Tr) * S (rev-search j Tq Tr))
    -- garbage LOWER: merging witnesses ⇒ no clean reversible search (R0c).
    garbage-lower : (x₁ x₂ : X) → ¬ (x₁ ≡ x₂) → wit x₁ ≡ wit x₂ → ¬ (clean-reversible wit)
    -- garbage UPPER: an injective realization whose input is recoverable (R0b).
    garbage-upper : Σ (X → X × Y)
                      (λ g → (∀ {x x'} → g x ≡ g x' → x ≡ x') × (∀ x → proj₁ (g x) ≡ x))

-- The theorem holds for EVERY search problem and solver params (time-optimality is
-- generic; the lower bound is conditional-but-present as a field, non-vacuous below).
reversible-levin : {X Y : Set} (wit : X → Y) (j Tq Tr : ℕ) → ReversibleLevin wit j Tq Tr
reversible-levin wit j Tq Tr = record
  { time-optimal  = rev-levin-grounded j Tq Tr
  ; garbage-lower = no-clean-rev-search wit
  ; garbage-upper = (λ x → x , wit x) , cong proj₁ , (λ x → refl)
  }

------------------------------------------------------------------------
-- NON-VACUITY: a concrete merging search (constant witness over Bool) -- the lower
-- bound genuinely fires (no clean reversible realization) while the upper bound
-- still keeps the input, so the garbage bracket is non-degenerate.

module NonVacuous where
  inst : ReversibleLevin constant-witness 0 3 1
  inst = reversible-levin constant-witness 0 3 1

  -- garbage is FORCED here (constant-witness merges true and false).
  forced-garbage : ¬ (clean-reversible constant-witness)
  forced-garbage = no-clean-constant

  -- yet a time-optimal phase still exists (a_q·S = 16 at j=0,Tq=3,Tr=1).
  optimal-phase : Σ ℕ (λ k → gsum k < aq (rev-search 0 3 1) * S (rev-search 0 3 1))
  optimal-phase = ReversibleLevin.time-optimal inst (s≤s z≤n)
