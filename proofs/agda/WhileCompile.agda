{-# OPTIONS --safe #-}

------------------------------------------------------------------------
-- Robustness brick B3a (docs/ROBUSTNESS-PLAN.md): the LOOP-FREE fragment of the
-- WHILE -> I compiler, and its correctness.
--
-- A loop-free WHILE command maps a store to a store; we compile it to a
-- CALL-FREE `Exp` (the I language) whose call-free evaluator `ev0` computes the
-- same store transformation.  No `call`/recursion is needed for the loop-free
-- fragment -- only the `while` case (B3b/B4) needs it.
--
-- Key pieces (all self-contained over `ev0`, the call-free I evaluator):
--   * `substVar` (+ correctness) -- substitute an Exp for the single variable
--     `var`; realises WHILE sequencing as Exp composition, and register descent.
--   * `getRE` / `setRE'` (+ correctness) -- read/write register `i` as an Exp,
--     matching `OneVarTuring.getR`/`setR`.
--   * `compileE` (WExp -> Exp), `compileLF` (loop-free WCmd -> Exp), and
--     `compile-correct : ev0 (compileLF c) s ≡ runLF c s` against the total
--     loop-free semantics `runLF`.
-- --safe, axiom-free.  (Bridging `runLF` to the fuel-indexed `runW`, and `ev0`
-- to the fuel-indexed I `eval`, are the small follow-on steps toward B5.)
------------------------------------------------------------------------

module WhileCompile where

open import Data.Nat using (ℕ; zero; suc)
open import Relation.Binary.PropositionalEquality
  using (_≡_; refl; trans; cong; cong₂)
open import UniversalInterp
  using (V; ⟨⟩; _∙_; hd; tl; ifnil;
         Exp; var; knil; kcons; khd; ktl; kifnil; call)
open import OneVarTuring using (getR; setR)
open import WhileLang using (WExp; wvar; wnil; wcons; whd; wtl;
                            WCmd; wskip; wasgn; wseq; wif; wwhile; evalE)

-- The call-free I evaluator (= `SelfInterpCorrect.ev0`; inlined here to avoid
-- depending on that module's heavy `simF` proof).  `call` is a dummy: the
-- loop-free fragment compiles to call-free Exp only.
ev0 : Exp → V → V
ev0 var            env = env
ev0 knil           env = ⟨⟩
ev0 (kcons a b)    env = ev0 a env ∙ ev0 b env
ev0 (khd e)        env = hd (ev0 e env)
ev0 (ktl e)        env = tl (ev0 e env)
ev0 (kifnil c t e) env = ifnil (ev0 c env) (ev0 t env) (ev0 e env)
ev0 (call e)       env = ⟨⟩

------------------------------------------------------------------------
-- Substitution for the single variable `var`, and its correctness.

substVar : Exp → Exp → Exp
substVar var            u = u
substVar knil           u = knil
substVar (kcons a b)    u = kcons (substVar a u) (substVar b u)
substVar (khd e)        u = khd (substVar e u)
substVar (ktl e)        u = ktl (substVar e u)
substVar (kifnil c t e) u = kifnil (substVar c u) (substVar t u) (substVar e u)
substVar (call e)       u = call (substVar e u)

substVar-correct : ∀ t u s → ev0 (substVar t u) s ≡ ev0 t (ev0 u s)
substVar-correct var            u s = refl
substVar-correct knil           u s = refl
substVar-correct (kcons a b)    u s = cong₂ _∙_ (substVar-correct a u s) (substVar-correct b u s)
substVar-correct (khd e)        u s = cong hd (substVar-correct e u s)
substVar-correct (ktl e)        u s = cong tl (substVar-correct e u s)
substVar-correct (kifnil c t e) u s
  rewrite substVar-correct c u s | substVar-correct t u s | substVar-correct e u s = refl
substVar-correct (call e)       u s = refl

------------------------------------------------------------------------
-- Register read/write as call-free Exp, matching OneVarTuring.getR/setR.

getRE : ℕ → Exp
getRE zero    = khd var
getRE (suc i) = substVar (getRE i) (ktl var)

getRE-correct : ∀ i s → ev0 (getRE i) s ≡ getR i s
getRE-correct zero    s = refl
getRE-correct (suc i) s =
  trans (substVar-correct (getRE i) (ktl var) s) (getRE-correct i (tl s))

-- `setRE' i ve se`: rebuild the store `se` with register `i` set to `ve`
-- (both Exps evaluated against the SAME input `s`, so `ve` stays anchored).
setRE' : ℕ → Exp → Exp → Exp
setRE' zero    ve se = kcons ve (ktl se)
setRE' (suc i) ve se = kcons (khd se) (setRE' i ve (ktl se))

setRE'-correct : ∀ i ve se s → ev0 (setRE' i ve se) s ≡ setR i (ev0 ve s) (ev0 se s)
setRE'-correct zero    ve se s = refl
setRE'-correct (suc i) ve se s =
  cong (hd (ev0 se s) ∙_) (setRE'-correct i ve (ktl se) s)

setRE : ℕ → Exp → Exp
setRE i ve = setRE' i ve var

------------------------------------------------------------------------
-- Expression compiler.

compileE : WExp → Exp
compileE (wvar i)    = getRE i
compileE wnil        = knil
compileE (wcons a b) = kcons (compileE a) (compileE b)
compileE (whd e)     = khd (compileE e)
compileE (wtl e)     = ktl (compileE e)

compileE-correct : ∀ e s → ev0 (compileE e) s ≡ evalE e s
compileE-correct (wvar i)    s = getRE-correct i s
compileE-correct wnil        s = refl
compileE-correct (wcons a b) s = cong₂ _∙_ (compileE-correct a s) (compileE-correct b s)
compileE-correct (whd e)     s = cong hd (compileE-correct e s)
compileE-correct (wtl e)     s = cong tl (compileE-correct e s)

------------------------------------------------------------------------
-- Loop-free commands: predicate, total denotational semantics, compiler.

data LoopFree : WCmd → Set where
  lf-skip : LoopFree wskip
  lf-asgn : ∀ {i e}   → LoopFree (wasgn i e)
  lf-seq  : ∀ {c d}   → LoopFree c → LoopFree d → LoopFree (wseq c d)
  lf-if   : ∀ {e c d} → LoopFree c → LoopFree d → LoopFree (wif e c d)

-- total store transformer for loop-free commands (while = identity dummy).
runLF : WCmd → V → V
runLF wskip        s = s
runLF (wasgn i e)  s = setR i (evalE e s) s
runLF (wseq c d)   s = runLF d (runLF c s)
runLF (wif e c d)  s = ifnil (evalE e s) (runLF c s) (runLF d s)
runLF (wwhile e c) s = s

compileLF : WCmd → Exp
compileLF wskip        = var
compileLF (wasgn i e)  = setRE i (compileE e)
compileLF (wseq c d)   = substVar (compileLF d) (compileLF c)
compileLF (wif e c d)  = kifnil (compileE e) (compileLF c) (compileLF d)
compileLF (wwhile e c) = var

-- CORRECTNESS: the compiled call-free Exp computes the loop-free semantics.
compile-correct : ∀ c → LoopFree c → ∀ s → ev0 (compileLF c) s ≡ runLF c s
compile-correct wskip       lf-skip       s = refl
compile-correct (wasgn i e) lf-asgn       s =
  trans (setRE'-correct i (compileE e) var s)
        (cong (λ v → setR i v s) (compileE-correct e s))
compile-correct (wseq c d)  (lf-seq lc ld) s =
  trans (substVar-correct (compileLF d) (compileLF c) s)
        (trans (cong (ev0 (compileLF d)) (compile-correct c lc s))
               (compile-correct d ld (runLF c s)))
compile-correct (wif e c d) (lf-if lc ld) s
  rewrite compileE-correct e s with evalE e s
... | ⟨⟩      = compile-correct c lc s
... | (_ ∙ _) = compile-correct d ld s

------------------------------------------------------------------------
-- Test-first: a concrete loop-free program, compiled and run under `ev0`,
-- agrees with the WHILE semantics.

module Examples where
  s0 : V
  s0 = ⟨⟩ ∙ (⟨⟩ ∙ ⟨⟩)

  prog : WCmd
  prog = wseq (wasgn 0 (wcons wnil wnil)) (wasgn 1 (whd (wvar 0)))

  lf : LoopFree prog
  lf = lf-seq lf-asgn lf-asgn

  -- the compiled I program computes the same store as the WHILE program
  ex : ev0 (compileLF prog) s0 ≡ runLF prog s0
  ex = compile-correct prog lf s0

  -- and that store is the concrete expected value
  ex-value : runLF prog s0 ≡ (⟨⟩ ∙ ⟨⟩) ∙ (⟨⟩ ∙ ⟨⟩)
  ex-value = refl
