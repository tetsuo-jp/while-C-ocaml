{-# OPTIONS --safe #-}

------------------------------------------------------------------------
-- Robustness brick B4-core (docs/ROBUSTNESS-PLAN.md): a CONTROL-STACK small-step
-- machine for the whole WHILE language, and its agreement with `runW`.
--
-- The machine linearises ARBITRARY nesting/sequencing of loops into ONE loop
-- "while the control stack is non-empty, do one step".  The state is a control
-- stack (a `List WCmd`) plus the store; one step pops the head command and:
--   * skip        -> pop
--   * x := e      -> update store, pop
--   * c ; d       -> replace head with  c ∷ d ∷ rest   (run c first)
--   * if e c d    -> pop, push the taken branch
--   * while e c   -> guard nil: pop;  guard cons: push  c ∷ (while e c) ∷ rest
-- This is the SINGLE-LOOP NORMAL FORM that a program-counter dispatcher needs:
-- once the machine is one stack-driven loop, compiling it to the I language is a
-- single `call`-recursion (a later brick, reusing `WhileLoopThen`'s base-case
-- technique).
--
-- `runW⇒runM` (this brick) is the FORWARD simulation: every terminating `runW`
-- run is reproduced by the machine, in continuation-passing style (so no fuel
-- alignment is needed).  --safe, axiom-free.
------------------------------------------------------------------------

module WhileMachine where

open import Data.Nat using (ℕ; zero; suc)
open import Data.List using (List; []; _∷_)
open import Data.Product using (Σ; _,_)
open import Data.Empty using (⊥-elim)
open import Relation.Binary.PropositionalEquality using (_≡_; refl; sym)
open import UniversalInterp
  using (V; ⟨⟩; _∙_; ifnil; Maybe; just; nothing; nothing≢just; _>>=_)
open import OneVarTuring using (setR)
open import WhileLang
  using (WExp; wvar; wnil; wcons; whd; wtl;
         WCmd; wskip; wasgn; wseq; wif; wwhile; evalE; runW)

just-inj : ∀ {A : Set} {a b : A} → just a ≡ just b → a ≡ b
just-inj refl = refl

------------------------------------------------------------------------
-- The control-stack machine.  `runM n K s` runs the control stack `K` from store
-- `s` with fuel `n`; an empty stack means "done" (result = the store).

runM : ℕ → List WCmd → V → Maybe V
runM zero    _              s = nothing
runM (suc n) []             s = just s
runM (suc n) (wskip     ∷ K) s = runM n K s
runM (suc n) (wasgn i e ∷ K) s = runM n K (setR i (evalE e s) s)
runM (suc n) (wseq c d  ∷ K) s = runM n (c ∷ d ∷ K) s
runM (suc n) (wif e c d ∷ K) s = ifnil (evalE e s) (runM n (c ∷ K) s) (runM n (d ∷ K) s)
runM (suc n) (wwhile e c ∷ K) s =
  ifnil (evalE e s) (runM n K s) (runM n (c ∷ wwhile e c ∷ K) s)

------------------------------------------------------------------------
-- FORWARD simulation, in continuation-passing form: if `c` takes `s` to `s′`
-- (`runW`), and the machine on the remaining stack `K` from `s′` reaches `r`,
-- then the machine on `c ∷ K` from `s` reaches `r`.  Induction on the `runW`
-- fuel; the sequencing and while cases compose two recursive calls.

runW⇒runM : ∀ n c K s {s′} → runW n c s ≡ just s′
          → ∀ {r} m → runM m K s′ ≡ just r
          → Σ ℕ (λ m′ → runM m′ (c ∷ K) s ≡ just r)
runW⇒runM zero c K s ()
runW⇒runM (suc n) wskip K s {s′} h {r} m hk = suc m , goal
  where goal : runM (suc m) (wskip ∷ K) s ≡ just r
        goal rewrite just-inj h = hk
runW⇒runM (suc n) (wasgn i e) K s {s′} h {r} m hk = suc m , goal
  where goal : runM (suc m) (wasgn i e ∷ K) s ≡ just r
        goal rewrite just-inj h = hk
runW⇒runM (suc n) (wseq c d) K s {s′} h {r} m hk = seqhelp (runW n c s) refl h
  where
    seqhelp : ∀ w → runW n c s ≡ w → (w >>= runW n d) ≡ just s′
            → Σ ℕ (λ m′ → runM m′ (wseq c d ∷ K) s ≡ just r)
    seqhelp nothing   eq hb = ⊥-elim (nothing≢just hb)
    seqhelp (just sc) eq hb with runW⇒runM n d K sc hb m hk
    ... | md , hmd with runW⇒runM n c (d ∷ K) s eq md hmd
    ...   | mc , hmc = suc mc , hmc
runW⇒runM (suc n) (wif e c d) K s {s′} h {r} m hk = ifbranch (evalE e s) refl h
  where
    ifbranch : ∀ w → evalE e s ≡ w → ifnil w (runW n c s) (runW n d s) ≡ just s′
             → Σ ℕ (λ m′ → runM m′ (wif e c d ∷ K) s ≡ just r)
    ifbranch ⟨⟩ we≡ hb with runW⇒runM n c K s hb m hk
    ... | mc , hmc = suc mc , goal
      where goal : runM (suc mc) (wif e c d ∷ K) s ≡ just r
            goal rewrite we≡ = hmc
    ifbranch (va ∙ vb) we≡ hb with runW⇒runM n d K s hb m hk
    ... | md , hmd = suc md , goal
      where goal : runM (suc md) (wif e c d ∷ K) s ≡ just r
            goal rewrite we≡ = hmd
runW⇒runM (suc n) (wwhile e c) K s {s′} h {r} m hk = whbranch (evalE e s) refl h
  where
    whbranch : ∀ w → evalE e s ≡ w
             → ifnil w (just s) (runW n c s >>= λ s2 → runW n (wwhile e c) s2) ≡ just s′
             → Σ ℕ (λ m′ → runM m′ (wwhile e c ∷ K) s ≡ just r)
    whbranch ⟨⟩ we≡ hb = suc m , goal
      where goal : runM (suc m) (wwhile e c ∷ K) s ≡ just r
            goal rewrite we≡ | just-inj hb = hk
    whbranch (va ∙ vb) we≡ hb = whhelp (runW n c s) refl hb
      where
        whhelp : ∀ w → runW n c s ≡ w
               → (w >>= λ s2 → runW n (wwhile e c) s2) ≡ just s′
               → Σ ℕ (λ m′ → runM m′ (wwhile e c ∷ K) s ≡ just r)
        whhelp nothing   eq hb2 = ⊥-elim (nothing≢just hb2)
        whhelp (just sc) eq hb2 with runW⇒runM n (wwhile e c) K sc hb2 m hk
        ... | mw , hmw with runW⇒runM n c (wwhile e c ∷ K) s eq mw hmw
        ...   | mc , hmc = suc mc , goal
          where goal : runM (suc mc) (wwhile e c ∷ K) s ≡ just r
                goal rewrite we≡ = hmc

-- Whole-command corollary: run `c` alone (empty continuation).
runW⇒runM-top : ∀ n c s {s′} → runW n c s ≡ just s′
              → Σ ℕ (λ m → runM m (c ∷ []) s ≡ just s′)
runW⇒runM-top n c s h = runW⇒runM n c [] s h 1 refl

------------------------------------------------------------------------
-- Test-first: a NESTED loop (the case a single global `call` cannot handle
-- directly) linearised by the machine.  Outer: while r0≠nil do (drain r1 ; r0 :=
-- tl r0).  With r0 = [⟨⟩], r1 = [⟨⟩] the machine reproduces the `runW` result.

module Example where
  inner : WCmd
  inner = wwhile (wvar 1) (wasgn 1 (wtl (wvar 1)))

  nested : WCmd
  nested = wwhile (wvar 0) (wseq inner (wasgn 0 (wtl (wvar 0))))

  s : V
  s = (⟨⟩ ∙ ⟨⟩) ∙ ((⟨⟩ ∙ ⟨⟩) ∙ ⟨⟩)          -- r0 = [⟨⟩], r1 = [⟨⟩]

  -- after: r0 = ⟨⟩, r1 = ⟨⟩.
  ex : Σ ℕ (λ m → runM m (nested ∷ []) s ≡ just (⟨⟩ ∙ (⟨⟩ ∙ ⟨⟩)))
  ex = runW⇒runM-top 20 nested s refl
