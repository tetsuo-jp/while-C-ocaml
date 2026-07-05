{-# OPTIONS --safe #-}

------------------------------------------------------------------------
-- Fuel-monotonicity for the STEP-COUNTING interpreter `StepCount.evalT`.
--
-- The plain interpreter has `UniversalInterp.Mono.mono`, but `StepCount` exports
-- none for `evalT` (value + step-count pair).  `evalT-mono` / `add-fuelT` are the
-- exact twins: more fuel never changes an already-defined (value, count) result.
-- Needed to lift phase sub-runs to a common fuel in the B3d cost proofs
-- (`LevinBudgetE`: the general `∀k evalT dblE = Tcost k` doubling).
------------------------------------------------------------------------

module StepCountMono where

open import Data.Nat using (ℕ; zero; suc; _+_)
open import Data.Product using (_×_; _,_; proj₁; proj₂)
open import Data.Empty using (⊥-elim)
open import Relation.Binary.PropositionalEquality using (_≡_; refl)
open import UniversalInterp
  using (V; ⟨⟩; _∙_; Exp; var; knil; kcons; khd; ktl; kifnil; call; ifnil
        ; Maybe; just; nothing; nothing≢just; _>>=_)
import StepCount as SC

module Mono (prog : Exp) where
  open SC.StepCount prog

  -- (mutually recursive: kifnil dispatches through `bump` on the taken branch)
  evalT-mono : ∀ n e x {r} → evalT n e x ≡ just r → evalT (suc n) e x ≡ just r
  bumpT-mono : ∀ n e x (c : ℕ) {r} → bump c (evalT n e x) ≡ just r
             → bump c (evalT (suc n) e x) ≡ just r

  evalT-mono (suc n) var  x h = h
  evalT-mono (suc n) knil x h = h
  evalT-mono (suc n) (kcons a b) x h with evalT n a x in eqa
  ... | nothing = ⊥-elim (nothing≢just h)
  ... | just ra with evalT n b x in eqb
  ...   | nothing = ⊥-elim (nothing≢just h)
  ...   | just rb rewrite evalT-mono n a x eqa | evalT-mono n b x eqb = h
  evalT-mono (suc n) (khd e) x h with evalT n e x in eqe
  ... | nothing = ⊥-elim (nothing≢just h)
  ... | just r  rewrite evalT-mono n e x eqe = h
  evalT-mono (suc n) (ktl e) x h with evalT n e x in eqe
  ... | nothing = ⊥-elim (nothing≢just h)
  ... | just r  rewrite evalT-mono n e x eqe = h
  evalT-mono (suc n) (kifnil c t e) x {r} h with evalT n c x in eqc
  ... | nothing = ⊥-elim (nothing≢just h)
  ... | just rc rewrite evalT-mono n c x eqc = branch (proj₁ rc) h
    where
      branch : ∀ w → ifnil w (bump (proj₂ rc) (evalT n t x)) (bump (proj₂ rc) (evalT n e x)) ≡ just r
                   → ifnil w (bump (proj₂ rc) (evalT (suc n) t x)) (bump (proj₂ rc) (evalT (suc n) e x)) ≡ just r
      branch ⟨⟩      h1 = bumpT-mono n t x (proj₂ rc) h1
      branch (_ ∙ _) h1 = bumpT-mono n e x (proj₂ rc) h1
  evalT-mono (suc n) (call e) x h with evalT n e x in eqe
  ... | nothing = ⊥-elim (nothing≢just h)
  ... | just ra with evalT n prog (proj₁ ra) in eqr
  ...   | nothing = ⊥-elim (nothing≢just h)
  ...   | just rr rewrite evalT-mono n e x eqe | evalT-mono n prog (proj₁ ra) eqr = h

  bumpT-mono n e x c hb with evalT n e x in eqe
  ... | nothing = ⊥-elim (nothing≢just hb)
  ... | just re rewrite evalT-mono n e x eqe = hb

  -- add any amount of fuel.
  add-fuelT : ∀ k n e x {r} → evalT n e x ≡ just r → evalT (k + n) e x ≡ just r
  add-fuelT zero    n e x h = h
  add-fuelT (suc k) n e x h = evalT-mono (k + n) e x (add-fuelT k n e x h)
