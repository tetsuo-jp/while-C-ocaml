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

open import Data.Nat using (ℕ; zero; suc; _+_)
open import Data.Nat.Properties using (m≤m+n; m≤n+m)
open import Data.Product using (Σ; _,_)
open import Relation.Binary.PropositionalEquality using (_≡_; refl; sym; trans)
open import UniversalInterp using (Maybe; just)
open import WhileLang
  using (WExp; wvar; wnil; wcons; whd; wtl;
         WCmd; wskip; wasgn; wseq; wif; wwhile; runW; runW-mono-≤)
open import WhileCompile
  using (CallFree; cf-var; cf-knil; cf-kcons; cf-khd; cf-ktl; cf-kifnil;
         compileE; compileLF; substVar; getRE; setRE';
         LoopFree; lf-skip; lf-asgn; lf-seq; lf-if; runLF; runLF-sound)

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
