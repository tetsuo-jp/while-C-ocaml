{-# OPTIONS --safe #-}

------------------------------------------------------------------------
-- Robustness brick B3b (docs/ROBUSTNESS-PLAN.md): compiling a WHILE `while`
-- loop (with a loop-free body) to the I language via recursion (`call`), and
-- its correctness against the operational `runW`.
--
-- The compiled program is `progLoop = kifnil (compileE e) var (call (compileLF
-- c))`: run it, and `call` re-invokes `progLoop` on the body's result -- exactly
-- the loop.  Correctness is an induction on the `runW` fuel, evaluating the
-- guard/body through `WhileCompile.CF.eval-callfree` (they are call-free).
--
-- This module holds the B3b PREPARATION: compiled programs are call-free, and
-- `runW`'s intermediate results coincide with the total loop-free `runLF`.
-- --safe, axiom-free.  (Kept separate from `WhileCompile` so its heavy
-- `Interp`/`Mono` import stays cached and this module type-checks fast.)
------------------------------------------------------------------------

module WhileLoop where

open import Data.Nat using (ℕ; zero; suc; _+_; _≤_)
open import Data.Nat.Properties using (m≤m+n; m≤n+m; ≤-trans)
open import Data.Product using (Σ; _,_)
open import Data.Empty using (⊥-elim)
open import Relation.Binary.PropositionalEquality using (_≡_; refl; sym; trans; cong)
open import UniversalInterp
  using (V; ⟨⟩; _∙_; ifnil; Maybe; just; nothing; nothing≢just; _>>=_;
         Exp; var; kifnil; call; module Interp; module Mono)
open import WhileLang
  using (WExp; wvar; wnil; wcons; whd; wtl;
         WCmd; wskip; wasgn; wseq; wif; wwhile; evalE; runW; runW-mono-≤)
open import WhileCompile
  using (CallFree; cf-var; cf-knil; cf-kcons; cf-khd; cf-ktl; cf-kifnil;
         compileE; compileLF; substVar; getRE; setRE';
         LoopFree; lf-skip; lf-asgn; lf-seq; lf-if; runLF; runLF-sound;
         compileE-correct; compile-correct; module CF)

------------------------------------------------------------------------
-- `just` is injective.

just-inj : ∀ {A : Set} {a b : A} → just a ≡ just b → a ≡ b
just-inj refl = refl

------------------------------------------------------------------------
-- Compiled programs are CALL-FREE (so `eval-callfree` applies to the guard and
-- the loop-free body of the compiled loop).

substVar-callfree : ∀ {t u} → CallFree t → CallFree u → CallFree (substVar t u)
substVar-callfree cf-var               cfu = cfu
substVar-callfree cf-knil              cfu = cf-knil
substVar-callfree (cf-kcons ca cb)     cfu = cf-kcons (substVar-callfree ca cfu) (substVar-callfree cb cfu)
substVar-callfree (cf-khd ce)          cfu = cf-khd (substVar-callfree ce cfu)
substVar-callfree (cf-ktl ce)          cfu = cf-ktl (substVar-callfree ce cfu)
substVar-callfree (cf-kifnil cc ct ce) cfu = cf-kifnil (substVar-callfree cc cfu) (substVar-callfree ct cfu) (substVar-callfree ce cfu)

getRE-callfree : ∀ i → CallFree (getRE i)
getRE-callfree zero    = cf-khd cf-var
getRE-callfree (suc i) = substVar-callfree (getRE-callfree i) (cf-ktl cf-var)

setRE'-callfree : ∀ i {ve se} → CallFree ve → CallFree se → CallFree (setRE' i ve se)
setRE'-callfree zero    cfve cfse = cf-kcons cfve (cf-ktl cfse)
setRE'-callfree (suc i) cfve cfse = cf-kcons (cf-khd cfse) (setRE'-callfree i cfve (cf-ktl cfse))

compileE-callfree : ∀ e → CallFree (compileE e)
compileE-callfree (wvar i)    = getRE-callfree i
compileE-callfree wnil        = cf-knil
compileE-callfree (wcons a b) = cf-kcons (compileE-callfree a) (compileE-callfree b)
compileE-callfree (whd e)     = cf-khd (compileE-callfree e)
compileE-callfree (wtl e)     = cf-ktl (compileE-callfree e)

compileLF-callfree : ∀ c → LoopFree c → CallFree (compileLF c)
compileLF-callfree wskip       lf-skip        = cf-var
compileLF-callfree (wasgn i e) lf-asgn        = setRE'-callfree i (compileE-callfree e) cf-var
compileLF-callfree (wseq c d)  (lf-seq lc ld) = substVar-callfree (compileLF-callfree d ld) (compileLF-callfree c lc)
compileLF-callfree (wif e c d) (lf-if lc ld)  = cf-kifnil (compileE-callfree e) (compileLF-callfree c lc) (compileLF-callfree d ld)

------------------------------------------------------------------------
-- Whatever `runW` computes for a loop-free command, it is the `runLF` value
-- (via `runLF-sound` + fuel monotonicity + injectivity of `just`).

runW-runLF : ∀ c → LoopFree c → ∀ m x {r} → runW m c x ≡ just r → r ≡ runLF c x
runW-runLF c lf m x {r} eqc with runLF-sound c lf x
... | n , hn = just-inj (trans (sym (runW-mono-≤ c x (m≤m+n m n) eqc))
                                (runW-mono-≤ c x (m≤n+m n m) hn))

------------------------------------------------------------------------
-- The WHILE `while` loop (with a loop-free body) compiled to a RECURSIVE I
-- program `progLoop = kifnil (compileE e) var (call (compileLF c))`, and its
-- correctness: every terminating run of the loop is reproduced by `run`.

module Loop (we : WExp) (wc : WCmd) (lfc : LoopFree wc) where

  guardE bodyE progLoop : Exp
  guardE   = compileE we
  bodyE    = compileLF wc
  progLoop = kifnil guardE var (call bodyE)

  open Interp progLoop using (eval; run) public
  open CF progLoop using (eval-callfree; eval-≤)

  guard-eval : ∀ x → Σ ℕ (λ n → eval n guardE x ≡ just (evalE we x))
  guard-eval x with eval-callfree guardE (compileE-callfree we) x
  ... | n , h = n , trans h (cong just (compileE-correct we x))

  body-eval : ∀ x → Σ ℕ (λ n → eval n bodyE x ≡ just (runLF wc x))
  body-eval x with eval-callfree bodyE (compileLF-callfree wc lfc) x
  ... | n , h = n , trans h (cong just (compile-correct wc lfc x))

  loop-correct : ∀ m x {r} → runW m (wwhile we wc) x ≡ just r
               → Σ ℕ (λ n → run n x ≡ just r)
  loop-correct zero    x ()
  loop-correct (suc m) x {r} h with evalE we x in eqg | guard-eval x
  ... | ⟨⟩ | ng , hg = suc (suc ng) , pf
    where
      pf : run (suc (suc ng)) x ≡ just r
      pf rewrite eval-≤ guardE x (m≤n+m ng 1) hg = h
  ... | (a ∙ b) | ng , hg with runW m wc x in eqc | body-eval x
  ...   | nothing | _ = ⊥-elim (nothing≢just h)
  ...   | just x″ | nb , hb with loop-correct m x″ h
  ...     | n′ , hn′ = suc (suc ((ng + nb) + n′)) , pf
    where
      M = (ng + nb) + n′
      x″≡ : x″ ≡ runLF wc x
      x″≡ = runW-runLF wc lfc m x eqc
      ng≤M : ng ≤ M
      ng≤M = ≤-trans (m≤m+n ng nb) (m≤m+n (ng + nb) n′)
      -- outer fuel `suc (suc M)`: guard at `suc M`; the `call` at `suc M` unfolds
      -- body and the recursive `progLoop` to fuel `M`.
      pf : run (suc (suc M)) x ≡ just r
      pf rewrite eval-≤ guardE x (≤-trans ng≤M (m≤n+m M 1)) hg
               | eval-≤ bodyE x (≤-trans (m≤n+m nb ng) (m≤m+n (ng + nb) n′)) hb
               | sym x″≡
         = eval-≤ progLoop x″ (m≤n+m n′ (ng + nb)) hn′

------------------------------------------------------------------------
-- Test-first: a concrete `while` loop (drain register 0), compiled and run
-- through the recursive I program, reproduces the WHILE result.

module Examples where
  open Loop (wvar 0) (wasgn 0 (wtl (wvar 0))) lf-asgn

  s1 : V
  s1 = (⟨⟩ ∙ ⟨⟩) ∙ ⟨⟩          -- register 0 = a one-element list

  -- runW 6 (while r0≠nil do r0:=tl r0) s1 ≡ just (⟨⟩ ∙ ⟨⟩); the compiled loop
  -- reproduces it at some fuel.
  ex : Σ ℕ (λ n → run n s1 ≡ just (⟨⟩ ∙ ⟨⟩))
  ex = loop-correct 6 s1 refl

------------------------------------------------------------------------
-- A richer demonstrator: LIST REVERSAL, whose loop body is a SEQUENCE with
-- cons/hd/tl (not a single assignment).  The compiled recursive I program
-- reproduces the WHILE reversal.  This exercises `compileLF`'s seq/cons/hd/tl
-- cases through `loop-correct`, closing the gap the drain-loop example leaves.

module ExamplesRev where
  -- while r0 is non-nil do  r1 := cons (hd r0) r1 ; r0 := tl r0.
  revBody : WCmd
  revBody = wseq (wasgn 1 (wcons (whd (wvar 0)) (wvar 1)))
                 (wasgn 0 (wtl (wvar 0)))

  open Loop (wvar 0) revBody (lf-seq lf-asgn lf-asgn)

  -- input: register 0 = [⟨⟩, ⟨⟩∙⟨⟩], register 1 = ⟨⟩ (empty accumulator).
  sRev : V
  sRev = (⟨⟩ ∙ ((⟨⟩ ∙ ⟨⟩) ∙ ⟨⟩)) ∙ (⟨⟩ ∙ ⟨⟩)

  -- the compiled recursive I program reverses r0 into r1: register 0 = ⟨⟩,
  -- register 1 = [⟨⟩∙⟨⟩, ⟨⟩] (the reversal), matching WhileLang's `ex-rev`.
  ex-rev : Σ ℕ (λ n → run n sRev ≡ just (⟨⟩ ∙ (((⟨⟩ ∙ ⟨⟩) ∙ (⟨⟩ ∙ ⟨⟩)) ∙ ⟨⟩)))
  ex-rev = loop-correct 12 sRev refl
