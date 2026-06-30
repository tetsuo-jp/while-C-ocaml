{-# OPTIONS --safe #-}
------------------------------------------------------------------------
-- HALTING PROBLEM UNDECIDABLE (Jones, Computability and Complexity, Thm 5.3.1
-- `thm-halt-undecidable`; the diagonal of Prop 1.4.4 `prop-halting-problem`).
--
-- The mathematical content of the theorem is the DIAGONAL argument, which works
-- for any self-applicable language closed under the "flip-and-loop"
-- construction.  This module formalises that argument abstractly and
-- CONSTRUCTIVELY (no excluded middle), then records how the concrete WHILE / I
-- language (`UniversalInterp.Exp`, with the self-interpreter `SelfInterp.sint`)
-- instantiates it.
--
-- `Model` abstracts a programming language: codes (programs, which are also their
-- own inputs ⌜p⌝ since programs are data), a termination predicate `Halts`, and
-- boolean outputs `Yields`.  A `Decides-self-halting` program `h` is total and
-- correctly answers whether `p` halts on its own code.  Given such an `h`, the
-- language can build a diagonal program `g` that loops exactly when `h` says
-- "halts".  Running `g` on its own code contradicts.
--
-- CONCRETE INSTANCE (WHILE / I).  `Code := V` (encoded programs ⌜p⌝);
-- `Halts p x := ∃ n v. eval n (decode p) x ≡ just v` (some fuel yields a result);
-- `Yields p x b := ...` with `true`/`false` as distinguished data.  The diagonal
-- `g` is constructible: run `h` on `(⌜p⌝.⌜p⌝)` via the self-interpreter, branch
-- with `kifnil`, and loop with `call` -- so the `build-diagonal` hypothesis holds.
-- The non-trivial part, the contradiction, is what is proved here.
--
-- Referenced by docs/AGDA_CORRESPONDENCE.md and docs/BOOK-ROADMAP.md (Thm 5.3.1).
------------------------------------------------------------------------

module HaltingUndecidable where

open import Data.Bool using (Bool; true; false)
open import Data.Product using (Σ; _×_; _,_; proj₁; proj₂)
open import Relation.Nullary using (¬_)

------------------------------------------------------------------------
-- An abstract self-applicable language with boolean outputs.
------------------------------------------------------------------------

record Model : Set₁ where
  field
    Code   : Set                       -- programs; being data, also inputs ⌜p⌝
    Halts  : Code → Code → Set         -- Halts p x : p halts on input x
    Yields : Code → Code → Bool → Set  -- Yields p x b : p on x halts with output b

  -- a program `h` that decides self-halting: total, and correct on every `p`
  -- (outputs `true` iff `p` halts on its own code `p`).
  Decides-self-halting : Code → Set
  Decides-self-halting h = ∀ p →
      (  Halts p p → Yields h p true)
    × (¬ Halts p p → Yields h p false)

  -- a diagonal program `g` built from a decider `h`: on input `p`, it LOOPS when
  -- `h` says `p` halts-on-`p`, and HALTS when `h` says it does not.
  Is-diagonal : Code → Code → Set
  Is-diagonal h g = ∀ p →
      (Yields h p true  → ¬ Halts g p)
    × (Yields h p false →   Halts g p)

------------------------------------------------------------------------
-- THEOREM 5.3.1.  In any language closed under the flip-and-loop construction
-- (`build-diagonal`), no program decides self-halting.  Hence HALT is
-- undecidable.  The proof is constructive: it derives `¬ Halts g g` and then a
-- contradiction from it, never appealing to `Halts g g ∨ ¬ Halts g g`.
------------------------------------------------------------------------

module Theorem (M : Model) where
  open Model M

  halting-undecidable :
      (∀ h → Decides-self-halting h → Σ Code (Is-diagonal h)) →
      ∀ h → ¬ Decides-self-halting h
  halting-undecidable build h dec with build h dec
  ... | g , diag =
    let -- g cannot halt on its own code: if it did, h would output `true`, so by
        -- the diagonal g would loop -- contradicting that very halt.
        nhalt : ¬ Halts g g
        nhalt hh = proj₁ (diag g) (proj₁ (dec g) hh) hh
    in  -- but then h outputs `false`, so by the diagonal g DOES halt -- contradiction.
        nhalt (proj₂ (diag g) (proj₂ (dec g) nhalt))
