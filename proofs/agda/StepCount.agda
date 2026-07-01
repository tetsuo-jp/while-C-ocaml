{-# OPTIONS --safe #-}

------------------------------------------------------------------------
-- A STEP-COUNTING semantics for the object language (Levin plan Layer B1).
--
-- `UniversalInterp.eval`'s fuel is a DEPTH bound (both branches of a `kcons`/
-- `kifnil`/`call` get the same fuel `n`), so it is NOT the total number of unit-cost
-- steps `time_p(d)` -- a call-free tree of depth `d` runs in fuel `d` but has up to
-- `2^d` nodes.  Levin's theorem needs the TOTAL step count (it sums the work of both
-- branches), so this module instruments the interpreter to thread a step counter:
-- `evalT n e x = just (v , t)` says `e` evaluates to `v` in `t` unit-cost steps
-- (one per node, summed over sub-evaluations).  `time` is the program's running
-- time `time_p(d)`.
--
-- `evalT⇒eval` ties it back to the reference semantics (same value).  The linear
-- overhead of the self-interpreter (`time_sint ≤ a · time_p`, Thm 19.1.3) is the
-- next step (B1.2), a step-accounting refinement of `SelfInterpCorrect.simF`.
------------------------------------------------------------------------

module StepCount where

open import Data.Nat using (ℕ; zero; suc; _+_)
open import Data.Product using (_×_; _,_)
open import Data.Empty using (⊥-elim)
open import Relation.Binary.PropositionalEquality using (_≡_; refl; cong)

open import UniversalInterp
  using (V; ⟨⟩; _∙_; Maybe; nothing; just; _>>=_; nothing≢just;
         hd; tl; ifnil; Exp; var; knil; kcons; khd; ktl; kifnil; call;
         module Interp)

module StepCount (prog : Exp) where
  open Interp prog using (eval)

  -- add `c` steps (and one for the current node) to an optional sub-result.
  bump : ℕ → Maybe (V × ℕ) → Maybe (V × ℕ)
  bump c nothing         = nothing
  bump c (just (v , t))  = just (v , suc (c + t))

  -- instrumented evaluation: value paired with total unit-cost step count.
  evalT : ℕ → Exp → V → Maybe (V × ℕ)
  evalT zero    _              x = nothing
  evalT (suc n) var            x = just (x , 1)
  evalT (suc n) knil           x = just (⟨⟩ , 1)
  evalT (suc n) (kcons a b)    x = evalT n a x >>= λ { (va , ta) →
                                   evalT n b x >>= λ { (vb , tb) →
                                   just (va ∙ vb , suc (ta + tb)) } }
  evalT (suc n) (khd e)        x = evalT n e x >>= λ { (v , t) →
                                   just (hd v , suc t) }
  evalT (suc n) (ktl e)        x = evalT n e x >>= λ { (v , t) →
                                   just (tl v , suc t) }
  evalT (suc n) (kifnil c t e) x = evalT n c x >>= λ { (vc , tc) →
                                   ifnil vc (bump tc (evalT n t x))
                                            (bump tc (evalT n e x)) }
  evalT (suc n) (call e)       x = evalT n e x >>= λ { (arg , ta) →
                                   evalT n prog arg >>= λ { (r , tr) →
                                   just (r , suc (ta + tr)) } }

  -- the program's running time on input `x` (the value is discarded).
  time : ℕ → V → Maybe (V × ℕ)
  time n x = evalT n prog x

  ------------------------------------------------------------------------
  -- consistency: the instrumented run computes the same value as `eval`.
  ------------------------------------------------------------------------

  fst≡ : ∀ {v v' : V} {t t' : ℕ} → just (v , t) ≡ just (v' , t') → v ≡ v'
  fst≡ refl = refl

  evalT⇒eval : ∀ n e x {v t} → evalT n e x ≡ just (v , t) → eval n e x ≡ just v
  evalT⇒eval (suc n) var  x refl = refl
  evalT⇒eval (suc n) knil x refl = refl
  evalT⇒eval (suc n) (kcons a b) x h with evalT n a x in eqa
  ... | nothing = ⊥-elim (nothing≢just h)
  ... | just (va , ta) with evalT n b x in eqb
  ...   | nothing = ⊥-elim (nothing≢just h)
  ...   | just (vb , tb) rewrite evalT⇒eval n a x eqa | evalT⇒eval n b x eqb =
          cong just (fst≡ h)
  evalT⇒eval (suc n) (khd e) x h with evalT n e x in eqe
  ... | nothing = ⊥-elim (nothing≢just h)
  ... | just (ve , te) rewrite evalT⇒eval n e x eqe = cong just (fst≡ h)
  evalT⇒eval (suc n) (ktl e) x h with evalT n e x in eqe
  ... | nothing = ⊥-elim (nothing≢just h)
  ... | just (ve , te) rewrite evalT⇒eval n e x eqe = cong just (fst≡ h)
  evalT⇒eval (suc n) (kifnil c t e) x h with evalT n c x in eqc
  ... | nothing = ⊥-elim (nothing≢just h)
  ... | just (⟨⟩ , tc) with evalT n t x in eqt
  ...   | nothing rewrite evalT⇒eval n c x eqc = ⊥-elim (nothing≢just h)
  ...   | just (vt , tt) rewrite evalT⇒eval n c x eqc | evalT⇒eval n t x eqt =
          cong just (fst≡ h)
  evalT⇒eval (suc n) (kifnil c t e) x h | just ((a ∙ b) , tc) with evalT n e x in eqe
  ...   | nothing rewrite evalT⇒eval n c x eqc = ⊥-elim (nothing≢just h)
  ...   | just (ve , te) rewrite evalT⇒eval n c x eqc | evalT⇒eval n e x eqe =
          cong just (fst≡ h)
  evalT⇒eval (suc n) (call e) x h with evalT n e x in eqe
  ... | nothing = ⊥-elim (nothing≢just h)
  ... | just (arg , ta) with evalT n prog arg in eqp
  ...   | nothing = ⊥-elim (nothing≢just h)
  ...   | just (r , tr) rewrite evalT⇒eval n e x eqe | evalT⇒eval n prog arg eqp =
          cong just (fst≡ h)

------------------------------------------------------------------------
-- concrete checks: the counter reports the exact number of nodes evaluated.
------------------------------------------------------------------------

module _ where
  open StepCount knil       -- prog irrelevant for these call-free counts

  -- ktl var on (⟨⟩ ∙ ⟨⟩): tail = ⟨⟩, in 2 steps (ktl, var).
  _ : evalT 2 (ktl var) (⟨⟩ ∙ ⟨⟩) ≡ just (⟨⟩ , 2)
  _ = refl

  -- kcons var var on ⟨⟩: (⟨⟩ ∙ ⟨⟩), in 3 steps (kcons, var, var).
  _ : evalT 2 (kcons var var) ⟨⟩ ≡ just (⟨⟩ ∙ ⟨⟩ , 3)
  _ = refl

module _ where
  open StepCount (ktl var)  -- prog = tl(input); exercise the `call` counter

  -- call var on (⟨⟩ ∙ ⟨⟩): arg = (⟨⟩ ∙ ⟨⟩), then prog = ktl var ⇒ ⟨⟩;
  -- steps: call + var(arg) + (ktl + var) = 1 + (1 + 2) = 4.
  _ : evalT 3 (call var) (⟨⟩ ∙ ⟨⟩) ≡ just (⟨⟩ , 4)
  _ = refl
