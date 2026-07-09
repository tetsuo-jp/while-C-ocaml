{-# OPTIONS --safe #-}

------------------------------------------------------------------------
-- Robustness brick B3b+ (docs/ROBUSTNESS-PLAN.md): a WHILE `while` loop followed
-- by a LOOP-FREE SUFFIX, `(while g do c) ; S`, compiled to the I language.
--
-- The single global program keeps ONE `call`, but its base case now runs the
-- suffix instead of returning the store:
--     prog = kifnil (compileE g) (compileLF S) (call (compileLF c))
-- guard nil  ⇒ the loop is over, so run S once (`compileLF S`);
-- guard cons ⇒ run the body and `call` (recurse) -- exactly the loop.  So `prog`
-- computes `S (loop g c x)`.  This strictly generalises `WhileLoop`
-- (`loop-correct` is the `S = skip` case) and shows the recursive-program
-- base-case technique that a full dispatcher will reuse.
--
-- `loopThen-correct` is the forward direction (WHILE ⟹ I): every terminating run
-- of `(while g do c) ; S` is reproduced.  --safe, axiom-free.
------------------------------------------------------------------------

module WhileLoopThen where

open import Data.Nat using (ℕ; zero; suc; _+_; _≤_)
open import Data.Nat.Properties using (m≤m+n; m≤n+m; ≤-trans)
open import Data.Product using (Σ; _,_; _×_)
open import Data.Empty using (⊥-elim)
open import Relation.Binary.PropositionalEquality using (_≡_; refl; sym; trans; cong)
open import UniversalInterp
  using (V; ⟨⟩; _∙_; ifnil; Maybe; just; nothing; nothing≢just; _>>=_;
         Exp; var; kifnil; call; module Interp; module Mono)
open import WhileLang
  using (WExp; wvar; wnil; wcons; wtl; WCmd; wasgn; wseq; wwhile;
         evalE; runW; runW-mono-≤)
open import WhileCompile
  using (compileE; compileLF; LoopFree; lf-asgn; runLF; runLF-sound;
         compileE-correct; compile-correct; module CF)
open import WhileLoop
  using (just-inj; compileE-callfree; compileLF-callfree; runW-runLF)
open import WhileLoopSound using (module Snd)
open import Data.Product using (proj₁; proj₂)

------------------------------------------------------------------------
-- `>>=` inversion for `wseq`: a defined `runW (suc m) (wseq A B)` factors through
-- an intermediate store.

seq-inv : ∀ m A B x {r} → runW (suc m) (wseq A B) x ≡ just r
        → Σ V (λ x* → (runW m A x ≡ just x*) × (runW m B x* ≡ just r))
seq-inv m A B x {r} h = help (runW m A x) refl h
  where
    help : ∀ w → runW m A x ≡ w → (w >>= runW m B) ≡ just r
         → Σ V (λ x* → (runW m A x ≡ just x*) × (runW m B x* ≡ just r))
    help nothing   eq h′ = ⊥-elim (nothing≢just h′)
    help (just x*) eq h′ = x* , eq , h′

------------------------------------------------------------------------

module LoopThen (we : WExp) (wc : WCmd) (lfc : LoopFree wc)
                (ws : WCmd) (lfs : LoopFree ws) where

  guardE bodyE sufE prog : Exp
  guardE = compileE we
  bodyE  = compileLF wc
  sufE   = compileLF ws
  prog   = kifnil guardE sufE (call bodyE)

  open Interp prog using (eval; run) public
  open CF prog using (eval-callfree; eval-≤)

  guard-eval : ∀ x → Σ ℕ (λ n → eval n guardE x ≡ just (evalE we x))
  guard-eval x with eval-callfree guardE (compileE-callfree we) x
  ... | n , h = n , trans h (cong just (compileE-correct we x))

  body-eval : ∀ x → Σ ℕ (λ n → eval n bodyE x ≡ just (runLF wc x))
  body-eval x with eval-callfree bodyE (compileLF-callfree wc lfc) x
  ... | n , h = n , trans h (cong just (compile-correct wc lfc x))

  suf-eval : ∀ x → Σ ℕ (λ n → eval n sufE x ≡ just (runLF ws x))
  suf-eval x with eval-callfree sufE (compileLF-callfree ws lfs) x
  ... | n , h = n , trans h (cong just (compile-correct ws lfs x))

  -- if the loop `while g do c` (fuel m) reaches `x*`, then `prog` reproduces the
  -- suffix applied to it: `run n x ≡ just (runLF ws x*)`.
  prog-correct : ∀ m x {x*} → runW m (wwhile we wc) x ≡ just x*
               → Σ ℕ (λ n → run n x ≡ just (runLF ws x*))
  prog-correct zero    x ()
  prog-correct (suc m) x {x*} h with evalE we x in eqg | guard-eval x
  ... | ⟨⟩ | ng , hg with suf-eval x
  ...   | ns , hs = suc (ng + ns) , pf
    where
      -- guard nil: `runW` returns `x`, so `x* = x`; `prog` runs the suffix.
      pf : run (suc (ng + ns)) x ≡ just (runLF ws x*)
      pf rewrite eval-≤ guardE x (m≤m+n ng ns) hg
               | eval-≤ sufE x (m≤n+m ns ng) hs
               | just-inj h
         = refl
  prog-correct (suc m) x {x*} h | (a ∙ b) | ng , hg with runW m wc x in eqc | body-eval x
  ...   | nothing | _ = ⊥-elim (nothing≢just h)
  ...   | just x″ | nb , hb with prog-correct m x″ h
  ...     | n′ , hn′ = suc (suc ((ng + nb) + n′)) , pf
    where
      M = (ng + nb) + n′
      x″≡ : x″ ≡ runLF wc x
      x″≡ = runW-runLF wc lfc m x eqc
      ng≤M : ng ≤ M
      ng≤M = ≤-trans (m≤m+n ng nb) (m≤m+n (ng + nb) n′)
      -- guard non-nil: run body, then `call` recurses; `prog-correct` on `x″`.
      pf : run (suc (suc M)) x ≡ just (runLF ws x*)
      pf rewrite eval-≤ guardE x (≤-trans ng≤M (m≤n+m M 1)) hg
               | eval-≤ bodyE x (≤-trans (m≤n+m nb ng) (m≤m+n (ng + nb) n′)) hb
               | sym x″≡
         = eval-≤ prog x″ (m≤n+m n′ (ng + nb)) hn′

  -- FORWARD: every terminating run of `(while g do c) ; S` is reproduced.
  loopThen-correct : ∀ m x {r} → runW m (wseq (wwhile we wc) ws) x ≡ just r
                   → Σ ℕ (λ n → run n x ≡ just r)
  loopThen-correct zero    x ()
  loopThen-correct (suc m) x {r} h with seq-inv m (wwhile we wc) ws x h
  ... | x* , hloop , hsuf with prog-correct m x hloop | runW-runLF ws lfs m x* hsuf
  ...   | n , hn | r≡ = n , trans hn (cong just (sym r≡))

  ----------------------------------------------------------------------
  -- REVERSE (I ⟹ WHILE): any defined run of `prog` is reproduced by
  -- `(while g do c) ; S` with the same result.  `cf-inv` (from WhileLoopSound)
  -- inverts the call-free guard/suffix/body; the loop witness is assembled
  -- explicitly (body-run then recursion) since `prog`'s base case is the suffix.

  open Snd prog using (cf-inv)

  guard-inv : ∀ n x {vc} → eval n guardE x ≡ just vc → vc ≡ evalE we x
  guard-inv n x h = trans (cf-inv n guardE (compileE-callfree we) x h)
                          (compileE-correct we x)
  suf-inv : ∀ n x {vs} → eval n sufE x ≡ just vs → vs ≡ runLF ws x
  suf-inv n x h = trans (cf-inv n sufE (compileLF-callfree ws lfs) x h)
                        (compile-correct ws lfs x)
  body-inv : ∀ n x {vb} → eval n bodyE x ≡ just vb → vb ≡ runLF wc x
  body-inv n x h = trans (cf-inv n bodyE (compileLF-callfree wc lfc) x h)
                         (compile-correct wc lfc x)

  -- `prog-sound` yields a common fuel `mL` at which the loop reaches `x*` AND the
  -- suffix maps `x*` to the observed result.  Mutually recursive with `ps-nonnil`
  -- (the guard-non-nil step) on the I fuel.
  prog-sound : ∀ n x {r} → run n x ≡ just r
             → Σ ℕ (λ mL → Σ V (λ x* →
                 (runW mL (wwhile we wc) x ≡ just x*) × (runW mL ws x* ≡ just r)))
  ps-nonnil : ∀ k x va vb {r} → (va ∙ vb) ≡ evalE we x
            → eval k (call bodyE) x ≡ just r
            → Σ ℕ (λ mL → Σ V (λ x* →
                 (runW mL (wwhile we wc) x ≡ just x*) × (runW mL ws x* ≡ just r)))

  prog-sound zero    x ()
  prog-sound (suc n) x {r} h with eval n guardE x in eqg
  ... | nothing = ⊥-elim (nothing≢just h)
  ... | just vc = go vc (guard-inv n x eqg) h
    where
      go : ∀ w → w ≡ evalE we x
         → ifnil w (eval n sufE x) (eval n (call bodyE) x) ≡ just r
         → Σ ℕ (λ mL → Σ V (λ x* →
             (runW mL (wwhile we wc) x ≡ just x*) × (runW mL ws x* ≡ just r)))
      -- guard nil: loop does 0 iterations (x* = x); the suffix gives r.
      go ⟨⟩ w≡ hh = suc nc , x , loopnil , sufpf
        where
          r≡ : r ≡ runLF ws x
          r≡ = suf-inv n x hh
          nc = proj₁ (runLF-sound ws lfs x)
          hc = proj₂ (runLF-sound ws lfs x)
          loopnil : runW (suc nc) (wwhile we wc) x ≡ just x
          loopnil rewrite sym w≡ = refl
          sufpf : runW (suc nc) ws x ≡ just r
          sufpf rewrite r≡ = runW-mono-≤ ws x (m≤n+m nc 1) hc
      go (va ∙ vb) w≡ hh = ps-nonnil n x va vb w≡ hh

  ps-nonnil zero    x va vb w≡ ()
  ps-nonnil (suc k) x va vb {r} w≡ h with eval k bodyE x in eqb
  ... | nothing = ⊥-elim (nothing≢just h)
  ... | just arg with body-inv k x eqb | prog-sound k arg h
  ...   | arg≡ | mL′ , x* , hloop′ , hsuf′ = suc (ncc + mL′) , x* , looppf , sufpf
    where
      ncc = proj₁ (runLF-sound wc lfc x)
      hcc = proj₂ (runLF-sound wc lfc x)
      -- loop from x: guard non-nil ⇒ run body (→ runLF wc x = arg) then recurse.
      looppf : runW (suc (ncc + mL′)) (wwhile we wc) x ≡ just x*
      looppf rewrite sym w≡
                   | runW-mono-≤ wc x (m≤m+n ncc mL′) hcc
                   | sym arg≡
         = runW-mono-≤ (wwhile we wc) arg (m≤n+m mL′ ncc) hloop′
      sufpf : runW (suc (ncc + mL′)) ws x* ≡ just r
      sufpf = runW-mono-≤ ws x* (m≤n+m mL′ (suc ncc)) hsuf′

  -- REVERSE: any defined run of `prog` is reproduced by `(while g do c) ; S`.
  loopThen-sound : ∀ n x {r} → run n x ≡ just r
                 → Σ ℕ (λ m → runW m (wseq (wwhile we wc) ws) x ≡ just r)
  loopThen-sound n x {r} h with prog-sound n x h
  ... | mL , x* , hloop , hsuf = suc mL , seqpf
    where
      seqpf : runW (suc mL) (wseq (wwhile we wc) ws) x ≡ just r
      seqpf rewrite hloop = hsuf

------------------------------------------------------------------------
-- Test-first: a drain loop with a loop-free SUFFIX.
--   (while r0≠nil do r0:=tl r0) ; r1 := cons nil nil
-- on a store with register 0 = a one-element list.  The loop drains r0 to nil,
-- then the suffix sets r1; the compiled I program reproduces the whole thing.

module Example where
  open LoopThen (wvar 0) (wasgn 0 (wtl (wvar 0))) lf-asgn
                (wasgn 1 (wcons wnil wnil)) lf-asgn

  s : V
  s = (⟨⟩ ∙ ⟨⟩) ∙ (⟨⟩ ∙ ⟨⟩)          -- r0 = [⟨⟩], r1 = ⟨⟩

  -- after: r0 = ⟨⟩ (drained), r1 = ⟨⟩∙⟨⟩ (cons nil nil).
  ex : Σ ℕ (λ n → run n s ≡ just (⟨⟩ ∙ ((⟨⟩ ∙ ⟨⟩) ∙ ⟨⟩)))
  ex = loopThen-correct 8 s refl
