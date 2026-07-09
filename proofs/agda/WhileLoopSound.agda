{-# OPTIONS --safe #-}

------------------------------------------------------------------------
-- Robustness brick B3b, the REVERSE direction (docs/ROBUSTNESS-PLAN.md).
--
-- `WhileLoop.Loop.loop-correct` proves the compiled recursive I program
-- REPRODUCES every terminating run of a WHILE `while` loop (WHILE ⟹ I).  This
-- module proves the converse, SOUNDNESS: whenever the compiled I program
-- returns a value, the WHILE loop returns the SAME value (I ⟹ WHILE).  Together
-- they give full OPERATIONAL EQUIVALENCE of the single loop and its I image on
-- the defined domain -- exactly the `run_I (comp p) d ≡ run_WHILE p d` equality
-- the robustness theorem targets (for the single-loop case).
--
-- Key device: `cf-inv`, inversion for the I evaluator on CALL-FREE programs --
-- any defined `eval n e x` equals the total call-free value `ev0 e x`.  With it,
-- the guard and (loop-free) body of the compiled loop are pinned to their WHILE
-- meanings, and the proof is an induction on the I fuel mirroring `loop-correct`
-- in reverse.  --safe, axiom-free.  (`with` on the guard/branch value uses the
-- `branch`-helper idiom -- as in `UniversalInterp.Mono.mono` -- so the `ifnil`
-- in the hypothesis reduces alongside the goal.)
------------------------------------------------------------------------

module WhileLoopSound where

open import Data.Nat using (ℕ; zero; suc; _+_; _≤_)
open import Data.Nat.Properties using (m≤m+n; m≤n+m)
open import Data.Product using (Σ; _,_; proj₁; proj₂)
open import Data.Empty using (⊥-elim)
open import Relation.Binary.PropositionalEquality
  using (_≡_; refl; sym; trans; cong; cong₂)
open import UniversalInterp
  using (V; ⟨⟩; _∙_; hd; tl; ifnil; Maybe; just; nothing; nothing≢just; _>>=_;
         Exp; var; knil; kcons; khd; ktl; kifnil; call; module Interp)
open import WhileLang
  using (WExp; wvar; wtl; WCmd; wasgn; wwhile; evalE; runW; runW-mono-≤)
open import WhileCompile
  using (CallFree; cf-var; cf-knil; cf-kcons; cf-khd; cf-ktl; cf-kifnil;
         ev0; compileE; compileLF; LoopFree; lf-asgn; runLF; runLF-sound;
         compileE-correct; compile-correct)
open import WhileLoop
  using (just-inj; compileE-callfree; compileLF-callfree)

------------------------------------------------------------------------
-- Inversion for the I evaluator on CALL-FREE programs: a defined result is the
-- total call-free value `ev0 e x`.  (`call` is unreachable under `CallFree`, so
-- this holds for ANY `prog`.)

module Snd (prog : Exp) where
  open Interp prog using (eval)

  cf-inv : ∀ n e → CallFree e → ∀ x {v} → eval n e x ≡ just v → v ≡ ev0 e x
  cf-inv zero    e            cf               x ()
  cf-inv (suc n) var          cf-var           x h = sym (just-inj h)
  cf-inv (suc n) knil         cf-knil          x h = sym (just-inj h)
  cf-inv (suc n) (kcons a b)  (cf-kcons ca cb) x h with eval n a x in eqa
  ... | nothing = ⊥-elim (nothing≢just h)
  ... | just va with eval n b x in eqb
  ...   | nothing = ⊥-elim (nothing≢just h)
  ...   | just vb = trans (sym (just-inj h))
                          (cong₂ _∙_ (cf-inv n a ca x eqa) (cf-inv n b cb x eqb))
  cf-inv (suc n) (khd e)      (cf-khd ce)      x h with eval n e x in eqe
  ... | nothing = ⊥-elim (nothing≢just h)
  ... | just ve = trans (sym (just-inj h)) (cong hd (cf-inv n e ce x eqe))
  cf-inv (suc n) (ktl e)      (cf-ktl ce)      x h with eval n e x in eqe
  ... | nothing = ⊥-elim (nothing≢just h)
  ... | just ve = trans (sym (just-inj h)) (cong tl (cf-inv n e ce x eqe))
  cf-inv (suc n) (kifnil c t e) (cf-kifnil cc ct ce) x {v} h with eval n c x in eqc
  ... | nothing = ⊥-elim (nothing≢just h)
  ... | just vc = branch vc (cf-inv n c cc x eqc) h
    where
      branch : ∀ w → w ≡ ev0 c x
             → ifnil w (eval n t x) (eval n e x) ≡ just v
             → v ≡ ifnil (ev0 c x) (ev0 t x) (ev0 e x)
      branch ⟨⟩        w≡ hh rewrite sym w≡ = cf-inv n t ct x hh
      branch (va ∙ vb) w≡ hh rewrite sym w≡ = cf-inv n e ce x hh

------------------------------------------------------------------------
-- The single loop's soundness: any defined run of the compiled recursive I
-- program is reproduced by the WHILE loop with the SAME result.

module Loop (we : WExp) (wc : WCmd) (lfc : LoopFree wc) where

  guardE bodyE progLoop : Exp
  guardE   = compileE we
  bodyE    = compileLF wc
  progLoop = kifnil guardE var (call bodyE)

  open Interp progLoop using (eval; run)
  open Snd progLoop using (cf-inv)

  -- the compiled guard's defined value is the WHILE guard's value.
  guard-inv : ∀ n x {vc} → eval n guardE x ≡ just vc → vc ≡ evalE we x
  guard-inv n x h = trans (cf-inv n guardE (compileE-callfree we) x h)
                          (compileE-correct we x)

  -- the compiled body's defined value is the WHILE body's `runLF` value.
  body-inv : ∀ n x {vb} → eval n bodyE x ≡ just vb → vb ≡ runLF wc x
  body-inv n x h = trans (cf-inv n bodyE (compileLF-callfree wc lfc) x h)
                         (compile-correct wc lfc x)

  -- `with` cannot fire on the parent-clause fuel, so the fuel cases live in
  -- module-level helpers that take the fuel as their own argument.  `nonnil` and
  -- `loop-sound` are mutually recursive (both descend on the I fuel).

  loop-sound  : ∀ n x {r} → run n x ≡ just r
              → Σ ℕ (λ m → runW m (wwhile we wc) x ≡ just r)
  eval-var-inv : ∀ k x {r} → eval k var x ≡ just r → r ≡ x
  nonnil : ∀ k x va vb {r} → (va ∙ vb) ≡ evalE we x
         → eval k (call bodyE) x ≡ just r
         → Σ ℕ (λ m → runW m (wwhile we wc) x ≡ just r)

  -- `eval (suc k) var x = just x`, so a defined var-run returns the input.
  eval-var-inv zero    x ()
  eval-var-inv (suc k) x h = sym (just-inj h)

  loop-sound zero    x ()
  loop-sound (suc n) x {r} h with eval n guardE x in eqg
  ... | nothing = ⊥-elim (nothing≢just h)
  ... | just vc = go vc (guard-inv n x eqg) h
    where
      go : ∀ w → w ≡ evalE we x
         → ifnil w (eval n var x) (eval n (call bodyE) x) ≡ just r
         → Σ ℕ (λ m → runW m (wwhile we wc) x ≡ just r)
      -- guard nil: the I program returns `x` (so `r ≡ x`); the loop stops.
      go ⟨⟩ w≡ hh = 1 , pf
        where
          pf : runW 1 (wwhile we wc) x ≡ just r
          pf rewrite sym w≡ = cong just (sym (eval-var-inv n x hh))
      -- guard non-nil: run body then recurse.
      go (va ∙ vb) w≡ hh = nonnil n x va vb w≡ hh

  nonnil zero    x va vb w≡ ()
  nonnil (suc k) x va vb {r} w≡ h with eval k bodyE x in eqb
  ... | nothing = ⊥-elim (nothing≢just h)
  ... | just arg with body-inv k x eqb | loop-sound k arg h
  ...   | arg≡ | m′ , hm′ = suc (nc + m′) , pf
    where
      nc = proj₁ (runLF-sound wc lfc x)
      hc = proj₂ (runLF-sound wc lfc x)
      -- outer fuel `suc (nc + m′)`: guard non-nil ⇒ run body (`runW nc`, lifted
      -- to `nc + m′`) then recurse (`runW m′`, lifted likewise).
      pf : runW (suc (nc + m′)) (wwhile we wc) x ≡ just r
      pf rewrite sym w≡
               | runW-mono-≤ wc x (m≤m+n nc m′) hc
               | sym arg≡
         = runW-mono-≤ (wwhile we wc) arg (m≤n+m m′ nc) hm′

------------------------------------------------------------------------
-- Test-first: a concrete soundness run.  The drain loop `while r0≠nil do
-- r0:=tl r0` on a store whose register 0 is already nil: the compiled I program
-- returns the store unchanged (fuel 3), and `loop-sound` recovers the immediate
-- WHILE run with the same result.

module Example where
  open Loop (wvar 0) (wasgn 0 (wtl (wvar 0))) lf-asgn

  -- register 0 = ⟨⟩ (nil): the loop does not iterate.
  s0 : V
  s0 = ⟨⟩ ∙ ⟨⟩

  ex-sound : Σ ℕ (λ m → runW m (wwhile (wvar 0) (wasgn 0 (wtl (wvar 0)))) s0 ≡ just s0)
  ex-sound = loop-sound 3 s0 refl
