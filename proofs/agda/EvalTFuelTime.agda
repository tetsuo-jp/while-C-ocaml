{-# OPTIONS --safe #-}

------------------------------------------------------------------------
-- THE REPORTED STEP COUNT IS SUFFICIENT FUEL.
--
-- In `StepCount.evalT`, the fuel argument bounds the recursion DEPTH, while the
-- reported count `t` in `just (v , t)` is the TOTAL number of evaluated nodes
-- (the run's SIZE).  Since depth ≤ size, the reported time is itself enough fuel:
--
--     evalT n e x ≡ just (v , t)  →  evalT t e x ≡ just (v , t)      (`fuel-time`)
--
-- This is the bridge that makes BOUNDED-TIME halting DECIDABLE (needed to
-- instantiate `GapTheorem.gap` to actual programs): "e halts on x with time ≤ B"
-- iff `evalT B e x ≡ just (v , t')` with `t' ≤ B` -- because if the true time is
-- `≤ B` then the depth is `≤` it `≤ B`, so `evalT B` already halts and reports
-- it; and if `evalT B` reports `t'`, that IS the true time (determinism).
-- Proved by the structural fuel-induction, lifting sub-runs to the common fuel
-- `proj₂ r` with `StepCountMono` (`add-fuelT` / the derived `evalT-le`).
-- --safe, axiom-free.
------------------------------------------------------------------------

module EvalTFuelTime where

open import Data.Nat using (ℕ; zero; suc; _+_; _≤_; _≤′_; ≤′-refl; ≤′-step)
open import Data.Nat.Properties using (m≤m+n; m≤n+m; ≤⇒≤′)
open import Data.Product using (_×_; _,_; proj₁; proj₂)
open import Data.Empty using (⊥-elim)
open import Relation.Binary.PropositionalEquality using (_≡_; refl)
open import UniversalInterp
  using (V; ⟨⟩; _∙_; Exp; var; knil; kcons; khd; ktl; kifnil; call
        ; Maybe; just; nothing; nothing≢just; hd; tl; ifnil; _>>=_)
import StepCount as SC
import StepCountMono

module _ (prog : Exp) where

  open SC.StepCount prog
  open StepCountMono.Mono prog using (evalT-mono; add-fuelT)

  -- fuel-monotone ≤ lifting (from evalT-mono, via ≤′ whose +1 steps match evalT).
  evalT-le′ : ∀ {e x r} {n m} → n ≤′ m → evalT n e x ≡ just r → evalT m e x ≡ just r
  evalT-le′ ≤′-refl                     h = h
  evalT-le′ {e} {x} (≤′-step {n = k} p) h = evalT-mono k e x (evalT-le′ p h)

  evalT-le : ∀ {n m e x r} → n ≤ m → evalT n e x ≡ just r → evalT m e x ≡ just r
  evalT-le n≤m = evalT-le′ (≤⇒≤′ n≤m)

  ----------------------------------------------------------------------
  -- fuel-time: the reported count is sufficient fuel.

  fuel-time : ∀ n e x {r} → evalT n e x ≡ just r → evalT (proj₂ r) e x ≡ just r
  fuel-time (suc n) var  x refl = refl
  fuel-time (suc n) knil x refl = refl
  fuel-time (suc n) (kcons a b) x h with evalT n a x in eqa
  ... | nothing = ⊥-elim (nothing≢just h)
  ... | just (va , ta) with evalT n b x in eqb
  ...   | nothing = ⊥-elim (nothing≢just h)
  ...   | just (vb , tb) with h
  ...     | refl rewrite evalT-le (m≤m+n ta tb) (fuel-time n a x eqa)
                       | evalT-le (m≤n+m tb ta) (fuel-time n b x eqb) = refl
  fuel-time (suc n) (khd e) x h with evalT n e x in eqe
  ... | nothing = ⊥-elim (nothing≢just h)
  ... | just (ve , te) with h
  ...   | refl rewrite fuel-time n e x eqe = refl
  fuel-time (suc n) (ktl e) x h with evalT n e x in eqe
  ... | nothing = ⊥-elim (nothing≢just h)
  ... | just (ve , te) with h
  ...   | refl rewrite fuel-time n e x eqe = refl
  fuel-time (suc n) (kifnil c t e) x h with evalT n c x in eqc
  ... | nothing = ⊥-elim (nothing≢just h)
  ... | just (⟨⟩ , tc) with evalT n t x in eqt
  ...   | nothing = ⊥-elim (nothing≢just h)
  ...   | just (vt , tt) with h
  ...     | refl rewrite evalT-le (m≤m+n tc tt) (fuel-time n c x eqc)
                       | evalT-le (m≤n+m tt tc) (fuel-time n t x eqt) = refl
  fuel-time (suc n) (kifnil c t e) x h | just (a ∙ b , tc) with evalT n e x in eqe
  ...   | nothing = ⊥-elim (nothing≢just h)
  ...   | just (ve , te) with h
  ...     | refl rewrite evalT-le (m≤m+n tc te) (fuel-time n c x eqc)
                       | evalT-le (m≤n+m te tc) (fuel-time n e x eqe) = refl
  fuel-time (suc n) (call e) x h with evalT n e x in eqe
  ... | nothing = ⊥-elim (nothing≢just h)
  ... | just (arg , ta) with evalT n prog arg in eqp
  ...   | nothing = ⊥-elim (nothing≢just h)
  ...   | just (vr , tr) with h
  ...     | refl rewrite evalT-le (m≤m+n ta tr) (fuel-time n e x eqe)
                       | evalT-le (m≤n+m tr ta) (fuel-time n prog arg eqp) = refl

  ----------------------------------------------------------------------
  -- COROLLARY (the bounded-time bridge for the Gap instantiation): if `e`
  -- halts at time `t` (at any fuel) and `t ≤ B`, then `evalT B` already halts
  -- with the same result.  So `evalT B` witnesses every halt whose time is ≤ B.

  time≤-fuel : ∀ {B} n e x {v t} → evalT n e x ≡ just (v , t) → t ≤ B
             → evalT B e x ≡ just (v , t)
  time≤-fuel n e x h t≤B = evalT-le t≤B (fuel-time n e x h)
