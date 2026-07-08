{-# OPTIONS --safe #-}

------------------------------------------------------------------------
-- Robustness brick B1 (docs/ROBUSTNESS-PLAN.md): a minimal MULTI-VARIABLE
-- WHILE language, as the source of a to-be-verified `WHILE ⟶ I` compiler.
--
-- The store is a single value `V` viewed as a register file: variable `i` is
-- register `i`, read by `OneVarTuring.getR i` and written by `setR i`.  So the
-- many-variable → one-variable state encoding (B2) is already in place, with the
-- array axioms `get-set-eq`/`get-set-neq` proven there -- exactly what the
-- assignment case of the compiler-correctness proof (B4) will use.
--
-- Expressions: variables, nil, cons, hd, tl.  Commands: skip, assignment,
-- sequencing, `if` (branch on nil), `while` (non-nil guard).  Semantics is
-- FUEL-INDEXED (total in Agda; the object language is genuinely partial), fuel
-- charged on every command node -- the same discipline as `UniversalInterp.eval`.
-- --safe, axiom-free.
------------------------------------------------------------------------

module WhileLang where

open import Data.Nat using (ℕ; zero; suc)
open import Data.Empty using (⊥-elim)
open import Relation.Binary.PropositionalEquality using (_≡_; refl)
open import UniversalInterp
  using (V; ⟨⟩; _∙_; hd; tl; ifnil; Maybe; just; nothing; _>>=_; nothing≢just)
open import OneVarTuring using (getR; setR)

------------------------------------------------------------------------
-- Expressions over the register-file store.

data WExp : Set where
  wvar  : ℕ → WExp                 -- read register i
  wnil  : WExp                     -- the constant ⟨⟩
  wcons : WExp → WExp → WExp        -- cons
  whd   : WExp → WExp               -- head
  wtl   : WExp → WExp               -- tail

evalE : WExp → V → V
evalE (wvar i)    s = getR i s
evalE wnil        s = ⟨⟩
evalE (wcons a b) s = evalE a s ∙ evalE b s
evalE (whd e)     s = hd (evalE e s)
evalE (wtl e)     s = tl (evalE e s)

------------------------------------------------------------------------
-- Commands.

data WCmd : Set where
  wskip  : WCmd
  wasgn  : ℕ → WExp → WCmd          -- register i := e
  wseq   : WCmd → WCmd → WCmd
  wif    : WExp → WCmd → WCmd → WCmd -- if (e is nil) then c₁ else c₂
  wwhile : WExp → WCmd → WCmd        -- while (e is non-nil) do c

------------------------------------------------------------------------
-- Fuel-indexed semantics.  `runW n c s` runs command `c` from store `s`;
-- `nothing` means fuel ran out (or a diverging object computation).

runW : ℕ → WCmd → V → Maybe V
runW zero    _            s = nothing
runW (suc n) wskip        s = just s
runW (suc n) (wasgn i e)  s = just (setR i (evalE e s) s)
runW (suc n) (wseq c d)   s = runW n c s >>= runW n d
runW (suc n) (wif e c d)  s = ifnil (evalE e s) (runW n c s) (runW n d s)
runW (suc n) (wwhile e c) s =
  ifnil (evalE e s) (just s) (runW n c s >>= λ s′ → runW n (wwhile e c) s′)

------------------------------------------------------------------------
-- Fuel monotonicity: more fuel never changes a defined result.  This is the
-- imperative counterpart of `UniversalInterp.Mono.add-fuel`, and is what the
-- while-case of the compiler-correctness proof (B4) needs to align the two
-- languages' fuel.

runW-mono : ∀ n c s {s′} → runW n c s ≡ just s′ → runW (suc n) c s ≡ just s′
runW-mono zero    c            s ()
runW-mono (suc n) wskip        s h = h
runW-mono (suc n) (wasgn i e)  s h = h
runW-mono (suc n) (wseq c d)   s h with runW n c s in eqc
... | nothing = ⊥-elim (nothing≢just h)
... | just sc rewrite runW-mono n c s eqc = runW-mono n d sc h
runW-mono (suc n) (wif e c d)  s h with evalE e s
... | ⟨⟩      = runW-mono n c s h
... | (_ ∙ _) = runW-mono n d s h
runW-mono (suc n) (wwhile e c) s h with evalE e s
... | ⟨⟩      = h
... | (_ ∙ _) with runW n c s in eqc
...   | nothing = ⊥-elim (nothing≢just h)
...   | just sc rewrite runW-mono n c s eqc = runW-mono n (wwhile e c) sc h

------------------------------------------------------------------------
-- Test-first: concrete runs, each closed by `refl`.

module Examples where
  -- a two-register store: register 0 = ⟨⟩, register 1 = ⟨⟩.
  s0 : V
  s0 = ⟨⟩ ∙ (⟨⟩ ∙ ⟨⟩)

  -- assignment: register 0 := cons nil nil.
  ex-asgn : runW 1 (wasgn 0 (wcons wnil wnil)) s0 ≡ just ((⟨⟩ ∙ ⟨⟩) ∙ (⟨⟩ ∙ ⟨⟩))
  ex-asgn = refl

  -- sequencing + reading a register: r0 := cons nil nil ; r1 := hd r0.
  ex-seq : runW 3 (wseq (wasgn 0 (wcons wnil wnil)) (wasgn 1 (whd (wvar 0)))) s0
           ≡ just ((⟨⟩ ∙ ⟨⟩) ∙ (⟨⟩ ∙ ⟨⟩))
  ex-seq = refl

  -- conditional: guard (register 0) is nil, so the then-branch runs (r1 := nil).
  ex-if : runW 2 (wif (wvar 0) (wasgn 1 wnil) (wasgn 1 (wcons wnil wnil))) s0 ≡ just s0
  ex-if = refl

  -- a drain loop: store with register 0 = one-element list; while it is non-nil,
  -- r0 := tl r0.  After the single iteration r0 becomes nil and the loop stops.
  s1 : V
  s1 = (⟨⟩ ∙ ⟨⟩) ∙ ⟨⟩

  ex-while : runW 6 (wwhile (wvar 0) (wasgn 0 (wtl (wvar 0)))) s1 ≡ just (⟨⟩ ∙ ⟨⟩)
  ex-while = refl
