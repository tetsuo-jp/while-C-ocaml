{-# OPTIONS --safe #-}
------------------------------------------------------------------------
-- A UNIVERSAL INTERPRETER IN AGDA (the machinery gap G4 names).
--
-- `FutamuraSmn` made the FIRST Futamura projection's specialization concrete,
-- but its straight-line object language could not express its own specializer,
-- so self-application (fp2/fp3) fell back to a closure.  Closing that wall needs
-- a Turing-complete object language with programs-as-data and a universal
-- interpreter `run ⌜p⌝ d ≡ [p] d` -- the in-Agda counterpart of
-- `universal.while` / `DesugarWhile --data`.
--
-- This module builds that interpreter.  The object language is a minimal
-- Turing-complete first-order recursive function over S-expressions (one global
-- recursive function `prog`, input is one S-expression, pairs encode multiple
-- arguments).  Because such programs may diverge, the interpreter is
-- FUEL-INDEXED (`eval : ℕ → Exp → V → Maybe V`), hence total in Agda yet partial
-- as a semantics -- exactly the partial `run` the full meta-theorem must accept.
--
-- What is machine-checked here:
--   * the interpreter is total (Agda accepts it: lexicographic (fuel, term));
--   * it genuinely COMPUTES -- a real `reverse` program runs by `refl`;
--   * fuel-monotonicity (`mono`, `add-fuel`): more fuel never changes a defined
--     result -- the key reusable lemma for reasoning about the partial `run`.
--
-- Remaining for G4 (scoped): write the specializer `mix` AS an `Exp` program and
-- prove `eval ⌜mix⌝ (⌜t⌝ , s) ≡ just ⌜spec t s⌝`, so folding survives
-- self-application.  This module supplies the universal `run` that step needs.
--
-- Referenced by docs/AGDA_CORRESPONDENCE.md (gap G4).
------------------------------------------------------------------------

module UniversalInterp where

open import Data.Nat using (ℕ; zero; suc; _+_)
open import Data.Empty using (⊥; ⊥-elim)
open import Relation.Binary.PropositionalEquality using (_≡_; refl)

------------------------------------------------------------------------
-- S-expression values and a self-contained Maybe with a reducing bind.
------------------------------------------------------------------------

data V : Set where
  ⟨⟩  : V
  _∙_ : V → V → V          -- cons

data Maybe (A : Set) : Set where
  nothing : Maybe A
  just    : A → Maybe A

_>>=_ : ∀ {A B} → Maybe A → (A → Maybe B) → Maybe B
nothing >>= f = nothing
just x  >>= f = f x

-- distinct constructors: `nothing ≡ just _` is impossible.
nothing≢just : ∀ {A} {v : A} → nothing ≡ just v → ⊥
nothing≢just ()

hd : V → V
hd (a ∙ _) = a
hd ⟨⟩      = ⟨⟩

tl : V → V
tl (_ ∙ b) = b
tl ⟨⟩      = ⟨⟩

-- choose by whether `v` is nil.
ifnil : ∀ {A : Set} → V → A → A → A
ifnil ⟨⟩      t e = t
ifnil (_ ∙ _) t e = e

------------------------------------------------------------------------
-- The object language: expressions over the single input `var`, with a
-- recursive self-call `call`.  A program is one body `Exp` (the body of the
-- one global recursive function f, with f(x) = body).
------------------------------------------------------------------------

data Exp : Set where
  var    : Exp                       -- the function's parameter
  knil   : Exp                       -- constant nil
  kcons  : Exp → Exp → Exp           -- build a cons
  khd    : Exp → Exp                 -- head
  ktl    : Exp → Exp                 -- tail
  kifnil : Exp → Exp → Exp → Exp     -- if (e is nil) then e₁ else e₂
  call   : Exp → Exp                 -- recursive call f(e)

------------------------------------------------------------------------
-- The fuel-indexed universal interpreter, parametrised by the program `prog`.
-- `eval n e x` runs expression `e` (parameter bound to `x`) under `prog`; `n`
-- bounds recursion depth (decremented only on `call`).  Total in Agda by
-- lexicographic descent on (fuel, expression).
------------------------------------------------------------------------

module Interp (prog : Exp) where

  -- One unit of fuel per evaluation step (every node), so the interpreter is
  -- total by plain structural descent on the fuel; `fuel` thus bounds the total
  -- number of steps.  (Charging fuel only on `call` would also be total, by a
  -- lexicographic measure, but that descent is invisible through `with`, which
  -- the monotonicity proof needs.)
  eval : ℕ → Exp → V → Maybe V
  eval zero    _              x = nothing
  eval (suc n) var            x = just x
  eval (suc n) knil           x = just ⟨⟩
  eval (suc n) (kcons a b)    x = eval n a x >>= λ va →
                                  eval n b x >>= λ vb →
                                  just (va ∙ vb)
  eval (suc n) (khd e)        x = eval n e x >>= λ v → just (hd v)
  eval (suc n) (ktl e)        x = eval n e x >>= λ v → just (tl v)
  eval (suc n) (kifnil c t e) x = eval n c x >>= λ vc →
                                  ifnil vc (eval n t x) (eval n e x)
  eval (suc n) (call e)       x = eval n e x >>= λ arg →
                                  eval n prog arg

  -- run the whole program on an input.
  run : ℕ → V → Maybe V
  run n x = eval n prog x

------------------------------------------------------------------------
-- Fuel-monotonicity: more fuel never changes an already-defined result.
-- Proved by lexicographic induction mirroring `eval` (the `with ... in` idiom
-- exposes each sub-evaluation so its monotone image can rewrite the goal).
------------------------------------------------------------------------

module Mono (prog : Exp) where
  open Interp prog

  mono : ∀ n e x {v} → eval n e x ≡ just v → eval (suc n) e x ≡ just v
  mono (suc n) var  x h = h
  mono (suc n) knil x h = h
  mono (suc n) (kcons a b) x h with eval n a x in eqa
  ... | nothing = ⊥-elim (nothing≢just h)
  ... | just va with eval n b x in eqb
  ...   | nothing = ⊥-elim (nothing≢just h)
  ...   | just vb rewrite mono n a x eqa | mono n b x eqb = h
  mono (suc n) (khd e) x h with eval n e x in eqe
  ... | nothing = ⊥-elim (nothing≢just h)
  ... | just ve rewrite mono n e x eqe = h
  mono (suc n) (ktl e) x h with eval n e x in eqe
  ... | nothing = ⊥-elim (nothing≢just h)
  ... | just ve rewrite mono n e x eqe = h
  mono (suc n) (kifnil c t e) x {v} h with eval n c x in eqc
  ... | nothing = ⊥-elim (nothing≢just h)
  ... | just vc rewrite mono n c x eqc = branch vc h
    where
      branch : ∀ w → ifnil w (eval n t x) (eval n e x) ≡ just v →
                     ifnil w (eval (suc n) t x) (eval (suc n) e x) ≡ just v
      branch ⟨⟩      h1 = mono n t x h1
      branch (_ ∙ _) h1 = mono n e x h1
  mono (suc n) (call e) x h with eval n e x in eqe
  ... | nothing = ⊥-elim (nothing≢just h)
  ... | just ve rewrite mono n e x eqe = mono n prog ve h

  -- add any amount of fuel.
  add-fuel : ∀ k n e x {v} → eval n e x ≡ just v → eval (k + n) e x ≡ just v
  add-fuel zero    n e x h = h
  add-fuel (suc k) n e x h = mono (k + n) e x (add-fuel k n e x h)

------------------------------------------------------------------------
-- It computes: a genuine `reverse` (accumulator-passing) runs by `refl`.
-- Input is a pair (list ∙ acc); f returns the reversed list once `list` is nil.
------------------------------------------------------------------------

module Reverse where

  -- list = hd var, acc = tl var.
  --   if (hd var) is nil  then  tl var
  --   else  f( (tl (hd var)) ∙ ((hd (hd var)) ∙ (tl var)) )
  revBody : Exp
  revBody =
    kifnil (khd var)
      (ktl var)
      (call (kcons (ktl (khd var))
                   (kcons (khd (khd var)) (ktl var))))

  open Interp revBody

  -- three distinct opaque elements.
  a b c : V
  a = ⟨⟩
  b = ⟨⟩ ∙ ⟨⟩
  c = (⟨⟩ ∙ ⟨⟩) ∙ ⟨⟩

  list3 : V                -- [a , b , c]
  list3 = a ∙ (b ∙ (c ∙ ⟨⟩))

  rev3 : V                 -- [c , b , a]
  rev3 = c ∙ (b ∙ (a ∙ ⟨⟩))

  -- reverse [a,b,c] = [c,b,a], computed entirely inside Agda by the interpreter.
  _ : run 60 (list3 ∙ ⟨⟩) ≡ just rev3
  _ = refl

  -- reverse of the empty list is the accumulator (here ⟨⟩).
  _ : run 60 (⟨⟩ ∙ ⟨⟩) ≡ just ⟨⟩
  _ = refl

  -- reverse of a singleton.
  _ : run 60 ((b ∙ ⟨⟩) ∙ ⟨⟩) ≡ just (b ∙ ⟨⟩)
  _ = refl

  -- too little fuel ⇒ undefined (the interpreter is honestly partial).
  _ : run 3 (list3 ∙ ⟨⟩) ≡ nothing
  _ = refl
