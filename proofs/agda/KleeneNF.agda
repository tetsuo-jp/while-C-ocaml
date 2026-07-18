{-# OPTIONS --safe #-}

------------------------------------------------------------------------
-- KLEENE NORMAL FORM for the I language (Jones, Thm 13.4.3
-- `thm-normal-form`): every computable function factors as
--
--     f(x) = U( μ k. T(e, x, k) )
--
-- through a DECIDABLE bounded-halting predicate `T` and a value extraction
-- `U`.  Here the step-counting interpreter `StepCount.evalT` IS the T-predicate:
-- `Halts≤ k e x = evalT k e x returns` is decidable (`T-dec`) because `evalT k e
-- x` is a concrete `Maybe`; and `U = proj₁` extracts the value from the
-- (value, step-count) pair.  The normal form is the equivalence
--
--     (∃ n, eval n e x = just v)  ⇔  (∃ k r, evalT k e x = just r ∧ U r = v)   (`knf`)
--
-- i.e. the reference semantics `eval` (UniversalInterp) halts with output `v`
-- exactly when the bounded search over the decidable T finds a witness that U
-- decodes to `v`.  Soundness is `StepCount.evalT⇒eval` (U = value component);
-- completeness needs the converse `eval⇒evalT` (whenever `eval` returns, the
-- instrumented run returns the same value with some step count), proved here by
-- the structural fuel-induction mirroring `evalT⇒eval`.  --safe, axiom-free.
------------------------------------------------------------------------

module KleeneNF where

open import Data.Nat using (ℕ; zero; suc; _+_)
open import Data.Product using (Σ; _×_; _,_; proj₁; proj₂)
open import Data.Empty using (⊥-elim)
open import Relation.Nullary using (Dec; yes; no; ¬_)
open import Relation.Binary.PropositionalEquality using (_≡_; refl; cong)
open import UniversalInterp
  using (V; ⟨⟩; _∙_; Exp; var; knil; kcons; khd; ktl; kifnil; call
        ; Maybe; just; nothing; nothing≢just; hd; tl; ifnil; _>>=_
        ; module Interp)
import StepCount as SC

module _ (prog : Exp) where

  open Interp prog using (eval)
  open SC.StepCount prog using (evalT; bump; evalT⇒eval)

  just-inj : ∀ {v v' : V} → just v ≡ just v' → v ≡ v'
  just-inj refl = refl

  ----------------------------------------------------------------------
  -- The converse of `evalT⇒eval`: whenever `eval` returns a value, the
  -- instrumented `evalT` returns the SAME value (with some step count).

  eval⇒evalT : ∀ n e x {v} → eval n e x ≡ just v
             → Σ ℕ (λ t → evalT n e x ≡ just (v , t))
  eval⇒evalT (suc n) var  x refl = 1 , refl
  eval⇒evalT (suc n) knil x refl = 1 , refl
  eval⇒evalT (suc n) (kcons a b) x {v} h with eval n a x in eqa
  ... | nothing = ⊥-elim (nothing≢just h)
  ... | just va with eval n b x in eqb
  ...   | nothing = ⊥-elim (nothing≢just h)
  ...   | just vb with eval⇒evalT n a x eqa | eval⇒evalT n b x eqb
  ...     | ta , pa | tb , pb rewrite pa | pb =
            suc (ta + tb) , cong (λ w → just (w , suc (ta + tb))) (just-inj h)
  eval⇒evalT (suc n) (khd e) x {v} h with eval n e x in eqe
  ... | nothing = ⊥-elim (nothing≢just h)
  ... | just ve with eval⇒evalT n e x eqe
  ...   | te , pe rewrite pe =
          suc te , cong (λ w → just (w , suc te)) (just-inj h)
  eval⇒evalT (suc n) (ktl e) x {v} h with eval n e x in eqe
  ... | nothing = ⊥-elim (nothing≢just h)
  ... | just ve with eval⇒evalT n e x eqe
  ...   | te , pe rewrite pe =
          suc te , cong (λ w → just (w , suc te)) (just-inj h)
  eval⇒evalT (suc n) (kifnil c t e) x {v} h with eval n c x in eqc
  ... | nothing = ⊥-elim (nothing≢just h)
  ... | just ⟨⟩ with eval⇒evalT n c x eqc
  ...   | tc , pc rewrite pc with eval n t x in eqt
  ...     | nothing = ⊥-elim (nothing≢just h)
  ...     | just vt with eval⇒evalT n t x eqt
  ...       | tt , pt rewrite pt =
              suc (tc + tt) , cong (λ w → just (w , suc (tc + tt))) (just-inj h)
  eval⇒evalT (suc n) (kifnil c t e) x {v} h
      | just (a ∙ b) with eval⇒evalT n c x eqc
  ...   | tc , pc rewrite pc with eval n e x in eqe
  ...     | nothing = ⊥-elim (nothing≢just h)
  ...     | just ve with eval⇒evalT n e x eqe
  ...       | te , pe rewrite pe =
              suc (tc + te) , cong (λ w → just (w , suc (tc + te))) (just-inj h)
  eval⇒evalT (suc n) (call e) x {v} h with eval n e x in eqe
  ... | nothing = ⊥-elim (nothing≢just h)
  ... | just arg with eval⇒evalT n e x eqe
  ...   | ta , pa rewrite pa with eval n prog arg in eqp
  ...     | nothing = ⊥-elim (nothing≢just h)
  ...     | just r with eval⇒evalT n prog arg eqp
  ...       | tr , pr rewrite pr =
              suc (ta + tr) , cong (λ w → just (w , suc (ta + tr))) (just-inj h)

  ----------------------------------------------------------------------
  -- T (decidable bounded halting) and U (value extraction).

  Halts≤ : ℕ → Exp → V → Set
  Halts≤ k e x = Σ (V × ℕ) (λ r → evalT k e x ≡ just r)

  T-dec : ∀ k e x → Dec (Halts≤ k e x)
  T-dec k e x with evalT k e x
  ... | just r  = yes (r , refl)
  ... | nothing = no (λ { (r , p) → nothing≢just p })

  U : V × ℕ → V
  U = proj₁

  ----------------------------------------------------------------------
  -- Kleene Normal Form: soundness, completeness, and the equivalence.

  knf-sound : ∀ k e x {r} → evalT k e x ≡ just r → eval k e x ≡ just (U r)
  knf-sound k e x {v , t} p = evalT⇒eval k e x p

  knf-complete : ∀ n e x {v} → eval n e x ≡ just v
               → Σ ℕ (λ k → Σ (V × ℕ) (λ r → (evalT k e x ≡ just r) × (U r ≡ v)))
  knf-complete n e x {v} h with eval⇒evalT n e x h
  ... | t , p = n , (v , t) , p , refl

  -- the normal form (both directions), packaged.
  knf : ∀ e x {v}
      → ((Σ ℕ (λ n → eval n e x ≡ just v))
          → Σ ℕ (λ k → Σ (V × ℕ) (λ r → (evalT k e x ≡ just r) × (U r ≡ v))))
      × ((Σ ℕ (λ k → Σ (V × ℕ) (λ r → (evalT k e x ≡ just r) × (U r ≡ v))))
          → Σ ℕ (λ n → eval n e x ≡ just v))
  knf e x = to , from
    where
      to : (Σ ℕ (λ n → eval n e x ≡ just _))
         → Σ ℕ (λ k → Σ (V × ℕ) (λ r → (evalT k e x ≡ just r) × (U r ≡ _)))
      to (n , h) = knf-complete n e x h
      from : (Σ ℕ (λ k → Σ (V × ℕ) (λ r → (evalT k e x ≡ just r) × (U r ≡ _))))
           → Σ ℕ (λ n → eval n e x ≡ just _)
      from (k , r , p , refl) = k , knf-sound k e x p
