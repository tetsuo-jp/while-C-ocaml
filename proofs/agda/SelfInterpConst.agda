{-# OPTIONS --safe #-}
------------------------------------------------------------------------
-- G4, STEP 4c (first brick): a SELF-INTERPRETER in `Exp`, for the constant
-- fragment of the encoded object language.
--
-- The remaining G4 piece is a metaprogram `mixProg : Exp` that, run by the
-- universal interpreter, interprets an ENCODED program ⌜p⌝ -- a self-interpreter
-- (`universal.while` in Agda).  Its core pattern: dispatch on ⌜e⌝'s constructor
-- TAG, then recursively interpret the sub-expressions via the interpreter's own
-- `call`.  This module realises that pattern for the CONSTANT fragment (the
-- `knil`/`kcons` sub-language, whose meaning is input-independent), establishing
-- the dispatch-and-recurse-over-⌜p⌝ technique the full self-interpreter extends.
--
-- `sintC : Exp` reads the tag of its input ⌜e⌝ (`tag 1` for knil, `tag 2` for
-- kcons; distinguished by the tag's tail) and either returns `⟨⟩` or rebuilds the
-- cons of its two recursively-interpreted arguments (`call` on ⌜a⌝ and ⌜b⌝).
-- Run with itself as the global program (`Interp sintC`), it computes the value
-- of any constant expression from its encoding -- verified on concrete programs.
--
-- Remaining for 4c: extend the dispatch to `var`/`khd`/`ktl`/`kifnil`/`call` (the
-- env-threading and recursion of the full language) and prove a general
-- fuel-bounded correctness theorem; then 4d assembles the `PartialMixModel`.
--
-- Referenced by docs/AGDA_CORRESPONDENCE.md (gap G4).
------------------------------------------------------------------------

module SelfInterpConst where

open import Relation.Binary.PropositionalEquality using (_≡_; refl)
open import UniversalInterp
  using (V; ⟨⟩; _∙_; Exp; var; knil; kcons; khd; ktl; kifnil; call; just; module Interp)
open import ProgEncoding using (⌜_⌝)

------------------------------------------------------------------------
-- The self-interpreter for the constant fragment.
--   input  = ⌜e⌝   (the encoded constant expression)
--   tag    = khd var ;  tag's tail distinguishes knil (nil) from kcons (cons)
--   knil   ⇒ ⟨⟩
--   kcons  ⇒ ( interpret ⌜a⌝ . interpret ⌜b⌝ ), the args via call
------------------------------------------------------------------------

sintC : Exp
sintC =
  kifnil (ktl (khd var))                 -- nil ⇒ knil-code, cons ⇒ kcons-code
    knil                                  -- knil: output ⟨⟩
    (kcons (call (khd (ktl var)))         -- kcons: ( ⟦⌜a⌝⟧ . ⟦⌜b⌝⟧ )
           (call (ktl (ktl var))))

------------------------------------------------------------------------
-- Verified on concrete encoded programs: the self-interpreter computes the
-- value of each constant expression from its encoding.
------------------------------------------------------------------------

module Run where
  open Interp sintC   -- run the self-interpreter with itself as the global program

  -- ⟦ knil ⟧ = ⟨⟩
  _ : run 5 ⌜ knil ⌝ ≡ just ⟨⟩
  _ = refl

  -- ⟦ kcons knil knil ⟧ = (⟨⟩ . ⟨⟩)
  _ : run 12 ⌜ kcons knil knil ⌝ ≡ just (⟨⟩ ∙ ⟨⟩)
  _ = refl

  -- ⟦ kcons (kcons knil knil) knil ⟧ = ((⟨⟩.⟨⟩) . ⟨⟩) -- nested recursion
  _ : run 30 ⌜ kcons (kcons knil knil) knil ⌝ ≡ just ((⟨⟩ ∙ ⟨⟩) ∙ ⟨⟩)
  _ = refl
