{-# OPTIONS --safe #-}

------------------------------------------------------------------------
-- B3d, brick 1: the dovetail search LOOP as a LITERAL `Exp` program.
--
-- Jones Thm 20.1.2's optimal search `opt` is, so far, a META driver
-- (`LevinDovetail.Search.find`, a fuel-indexed Agda function).  B3d asks for
-- `opt ∈ Exp` -- a program written IN the object language.  The full `opt`
-- (Cantor unpair + `enum` decode + budget 2^k + `sint` invocation + checker,
-- all in `Exp`) is several bricks; THIS module supplies the CONTROL brick: the
-- upward phase loop `find k = if done k then k else find (k+1)` as a genuine
-- `call`-recursive `Exp` program `optE`, and proves it computes exactly the
-- abstract `find`'s least-success phase.
--
-- `optE doneE` runs the oracle expression `doneE` on the current phase (the
-- single variable `var`, a unary numeral = k nils); if it is nonnil ("done"),
-- it returns the phase, else it re-runs itself with the phase incremented, via
-- the object language's own `call`.  The remaining bricks make `doneE` the real
-- Levin oracle (unpair/enum/budget/`sint`/chk); the control is now literal.
------------------------------------------------------------------------

module LevinOptE where

open import Data.Nat using (ℕ; zero; suc; _+_; _∸_; _≤_; _<_; z≤n; s≤s)
open import Data.Bool using (Bool; true; false)
open import Relation.Binary.PropositionalEquality using (_≡_; refl; sym; trans; cong; subst)
open import UniversalInterp
  using (V; ⟨⟩; _∙_; Exp; var; knil; kcons; khd; ktl; kifnil; call; hd; tl; ifnil
        ; Maybe; just; nothing; module Interp; module Mono)

------------------------------------------------------------------------
-- The literal search loop.
------------------------------------------------------------------------

-- increment the phase counter: prepend a nil  (phase k ↦ phase (suc k))
incE : Exp
incE = kcons knil var

-- optE doneE : from the current phase `var`, return it if `doneE` is nonnil,
-- else recurse (via `call`) on the incremented phase.
optE : Exp → Exp
optE doneE = kifnil doneE (call incE) var

-- unary phase counter as a value: phase k = k nils.
phaseV : ℕ → V
phaseV zero    = ⟨⟩
phaseV (suc k) = ⟨⟩ ∙ phaseV k

------------------------------------------------------------------------
-- Concrete: oracle "phase ≥ 2" (`ktl var` is nonnil iff k ≥ 2); optE finds k=2.
------------------------------------------------------------------------

module Demo where
  open Interp (optE (ktl var))

  -- start at phase 0 (⟨⟩); the loop climbs to the least k with tl(phase k) ≠ nil.
  _ : run 40 (phaseV 0) ≡ just (phaseV 2)
  _ = refl

  -- starting already at phase 2 returns immediately
  _ : run 6 (phaseV 2) ≡ just (phaseV 2)
  _ = refl

------------------------------------------------------------------------
-- CORRECTNESS: `optE doneE` computes the abstract `find`'s least-success phase.
--
-- Parameterised by a meta oracle `done : ℕ → Bool` and its REALIZATION: the
-- expression `doneE`, evaluated at phase k under the actual global program
-- `optE doneE`, yields ⟨⟩ exactly when `done k = false` and a cons when true
-- (fuel bound `db`).  Under that hypothesis, if `k = n + j` is the least done-
-- phase ≥ j, then the literal program returns `phaseV k` -- the object-language
-- counterpart of `LevinDovetail.find-least`.
------------------------------------------------------------------------

open import Data.Product using (Σ; _,_; proj₁; proj₂)
open import Data.Nat.Properties using (+-suc; +-identityʳ; ≤-trans; m≤m+n; m≤n+m; m∸n+n≡m)

module Correct
  (doneE : Exp) (done : ℕ → Bool) (db : ℕ)
  (oracle-false : ∀ k → done k ≡ false →
                    Interp.eval (optE doneE) db doneE (phaseV k) ≡ just ⟨⟩)
  (oracle-true  : ∀ k → done k ≡ true  →
                    Σ V (λ a → Σ V (λ b → Interp.eval (optE doneE) db doneE (phaseV k) ≡ just (a ∙ b))))
  where

  open Interp (optE (doneE)) using (eval; run)
  open Mono   (optE (doneE)) using (mono; add-fuel)

  -- lift a defined evaluation to ANY larger fuel (≤-based, avoids the exact
  -- `k + n` shape that `add-fuel` alone would force on the goal).
  eval-≤ : ∀ {m n} e x {v} → m ≤ n → eval m e x ≡ just v → eval n e x ≡ just v
  eval-≤ {m} {n} e x {v} m≤n h =
    subst (λ z → eval z e x ≡ just v) (m∸n+n≡m m≤n) (add-fuel (n ∸ m) m e x h)

  -- one step of the increment sub-expression: incE turns phase j into phase (suc j)
  incE-ok : ∀ n j → eval (suc (suc n)) incE (phaseV j) ≡ just (phaseV (suc j))
  incE-ok n j = refl

  -- THE THEOREM (by induction on the number `n` of phases still to climb).
  climb : (n j : ℕ) → done (n + j) ≡ true → ((i : ℕ) → i < n → done (i + j) ≡ false)
        → Σ ℕ (λ F → run F (phaseV j) ≡ just (phaseV (n + j)))
  climb zero j dj _ with oracle-true j dj
  ... | a , b , ev = suc (suc db) , goal
    where
      goal : run (suc (suc db)) (phaseV j) ≡ just (phaseV j)
      goal rewrite mono db doneE (phaseV j) ev = refl
  climb (suc n) j dj allf with climb n (suc j) dj' allf'
    where
      dj'   : done (n + suc j) ≡ true
      dj'   rewrite +-suc n j = dj
      allf' : (i : ℕ) → i < n → done (i + suc j) ≡ false
      allf' i i<n rewrite +-suc i j = allf (suc i) (s≤s i<n)
  ... | F' , ev' = suc (suc (suc (suc (db + F')))) , goal
    where
      of : eval db doneE (phaseV j) ≡ just ⟨⟩
      of = oracle-false j (allf zero (s≤s z≤n))
      le1 : db ≤ suc (suc (suc (db + F')))
      le1 = ≤-trans (m≤m+n db F') (m≤n+m (db + F') 3)
      le2 : F' ≤ suc (suc (db + F'))
      le2 = ≤-trans (m≤n+m F' db) (m≤n+m (db + F') 2)
      goal : run (suc (suc (suc (suc (db + F'))))) (phaseV j) ≡ just (phaseV (suc n + j))
      goal rewrite eval-≤ doneE (phaseV j) le1 of
                 | eval-≤ (optE doneE) (phaseV (suc j)) le2 ev'
                 | +-suc n j = refl

  -- from phase 0: the object-language counterpart of `LevinDovetail.find-least`
  -- (`opt ∈ Exp` returns the least done-phase `k`).
  optE-least : ∀ k → done k ≡ true → (∀ i → i < k → done i ≡ false)
             → Σ ℕ (λ F → run F (phaseV 0) ≡ just (phaseV k))
  optE-least k dk allf
    with climb k 0 (subst (λ z → done z ≡ true) (sym (+-identityʳ k)) dk)
                   (λ i i<k → subst (λ z → done z ≡ false) (sym (+-identityʳ i)) (allf i i<k))
  ... | F , ev = F , subst (λ z → run F (phaseV 0) ≡ just (phaseV z)) (+-identityʳ k) ev

------------------------------------------------------------------------
-- Non-vacuity: instantiate `Correct` with the concrete "phase ≥ 2" oracle
-- (`ktl var`, `done k = k ≥ 2`); the GENERAL theorem then DERIVES the phase-2
-- result -- the same value `Demo` checks by `refl`, but now via the theorem.
------------------------------------------------------------------------

module DemoCorrect where
  done≥2 : ℕ → Bool
  done≥2 zero          = false
  done≥2 (suc zero)    = false
  done≥2 (suc (suc _)) = true

  of : ∀ k → done≥2 k ≡ false → Interp.eval (optE (ktl var)) 2 (ktl var) (phaseV k) ≡ just ⟨⟩
  of zero          _  = refl
  of (suc zero)    _  = refl
  of (suc (suc _)) ()

  ot : ∀ k → done≥2 k ≡ true
     → Σ V (λ a → Σ V (λ b → Interp.eval (optE (ktl var)) 2 (ktl var) (phaseV k) ≡ just (a ∙ b)))
  ot zero          ()
  ot (suc zero)    ()
  ot (suc (suc k)) _  = ⟨⟩ , phaseV k , refl

  open Correct (ktl var) done≥2 2 of ot
  open Interp (optE (ktl var)) using (run)

  -- the literal search program finds the least done-phase (= 2) from phase 0,
  -- DERIVED from the general theorem (not just a concrete `refl`).
  find2 : Σ ℕ (λ F → run F (phaseV 0) ≡ just (phaseV 2))
  find2 = optE-least 2 refl λ { zero _ → refl ; (suc zero) _ → refl ; (suc (suc i)) (s≤s (s≤s ())) }
