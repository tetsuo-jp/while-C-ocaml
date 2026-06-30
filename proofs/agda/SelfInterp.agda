{-# OPTIONS --safe #-}
------------------------------------------------------------------------
-- G4, STEP 4c: a FULL self-interpreter in `Exp` (universal.while in Agda).
--
-- `sint : Exp` interprets an ENCODED program over the WHOLE object language
-- (var/knil/kcons/khd/ktl/kifnil/call), run by the universal interpreter with
-- itself as the global program.  Its input is a triple `(⌜p⌝ . (⌜e⌝ . env))`:
--   * `⌜p⌝`  -- the encoded program being interpreted (needed for `call`);
--   * `⌜e⌝`  -- the encoded sub-expression currently under evaluation;
--   * `env`  -- the value of `var` (the interpreted program's input).
-- It dispatches on `⌜e⌝`'s constructor TAG (a unary numeral, walked by `ktl`),
-- recursively interpreting sub-expressions via the interpreter's own `call`
-- (the `ireq` helper rebuilds the triple for a sub-request).  `call e` is handled
-- by re-interpreting the whole program `⌜p⌝` with `env := ⟦e⟧`.
--
-- `⟦p⟧(inp)` is computed as `run sint F (⌜p⌝ . (⌜p⌝ . inp))` (start by
-- interpreting the whole body with `env = inp`).  Verified on concrete programs
-- exercising every constructor.  This is the metaprogram G4's `mix` is built on:
-- a residual carrying ⌜p⌝ to `sint` runs `p` standalone, so genuine specialization
-- survives self-application (the remaining 4d assembles the `PartialMixModel`).
--
-- Referenced by docs/AGDA_CORRESPONDENCE.md (gap G4).
------------------------------------------------------------------------

module SelfInterp where

open import Relation.Binary.PropositionalEquality using (_≡_; refl)
open import UniversalInterp
  using (V; ⟨⟩; _∙_; Exp; var; knil; kcons; khd; ktl; kifnil; call; just; module Interp)
open import ProgEncoding using (⌜_⌝)

------------------------------------------------------------------------
-- Accessors into the triple `var = (⌜p⌝ . (⌜e⌝ . env))`.
------------------------------------------------------------------------

P E R tagE argsE : Exp
P     = khd var                 -- ⌜p⌝
E     = khd (ktl var)           -- ⌜e⌝
R     = ktl (ktl var)           -- env
tagE  = khd E                   -- ⌜e⌝'s tag (unary numeral)
argsE = ktl E                   -- ⌜e⌝'s argument(s)

-- a sub-request: interpret code `subE` with environment `envE`.
ireq : Exp → Exp → Exp
ireq subE envE = call (kcons P (kcons subE envE))

------------------------------------------------------------------------
-- The self-interpreter: a 7-way dispatch on the tag, walking it with `ktl`.
------------------------------------------------------------------------

sint : Exp
sint =
  kifnil tagE
    R                                                       -- 0  var   ⇒ env
    (kifnil (ktl tagE)
      knil                                                  -- 1  knil  ⇒ ⟨⟩
      (kifnil (ktl (ktl tagE))
        (kcons (ireq (khd argsE) R) (ireq (ktl argsE) R))   -- 2  kcons ⇒ (⟦a⟧ . ⟦b⟧)
        (kifnil (ktl (ktl (ktl tagE)))
          (khd (ireq argsE R))                              -- 3  khd   ⇒ hd ⟦e⟧
          (kifnil (ktl (ktl (ktl (ktl tagE))))
            (ktl (ireq argsE R))                            -- 4  ktl   ⇒ tl ⟦e⟧
            (kifnil (ktl (ktl (ktl (ktl (ktl tagE)))))
              (kifnil (ireq (khd argsE) R)                  -- 5  kifnil ⇒ ifnil ⟦c⟧
                (ireq (khd (ktl argsE)) R)                  --              ⟦t⟧
                (ireq (ktl (ktl argsE)) R))                 --              ⟦e⟧
              (ireq P (ireq argsE R)))))))                  -- 6  call  ⇒ ⟦p⟧ with env ⟦e⟧

------------------------------------------------------------------------
-- Verified on concrete programs (run with itself as the global program).
-- `selfrun p inp = run sint F (⌜p⌝ . (⌜p⌝ . inp))`.
------------------------------------------------------------------------

module Run where
  open Interp sint

  d : V
  d = ⟨⟩ ∙ (⟨⟩ ∙ ⟨⟩)        -- a 2-list [⟨⟩ , ⟨⟩]

  -- identity: ⟦ var ⟧(d) = d
  _ : run 15 (⌜ var ⌝ ∙ (⌜ var ⌝ ∙ d)) ≡ just d
  _ = refl

  -- tail: ⟦ tl(var) ⟧(d) = tl d
  _ : run 20 (⌜ ktl var ⌝ ∙ (⌜ ktl var ⌝ ∙ d)) ≡ just (⟨⟩ ∙ ⟨⟩)
  _ = refl

  -- constant nil: ⟦ knil ⟧(d) = ⟨⟩
  _ : run 15 (⌜ knil ⌝ ∙ (⌜ knil ⌝ ∙ d)) ≡ just ⟨⟩
  _ = refl

  -- head: ⟦ hd(var) ⟧(d) = hd d = ⟨⟩
  _ : run 20 (⌜ khd var ⌝ ∙ (⌜ khd var ⌝ ∙ d)) ≡ just ⟨⟩
  _ = refl

  -- cons: ⟦ cons(var, var) ⟧(d) = (d . d)
  _ : run 25 (⌜ kcons var var ⌝ ∙ (⌜ kcons var var ⌝ ∙ d)) ≡ just (d ∙ d)
  _ = refl

  -- conditional: ⟦ if var then ⟨⟩ else var ⟧(d) = d   (d is a cons ⇒ else branch)
  _ : run 25 (⌜ kifnil var knil var ⌝ ∙ (⌜ kifnil var knil var ⌝ ∙ d)) ≡ just d
  _ = refl

  -- RECURSION via `call`: drain `p = if var then ⟨⟩ else p(tl var)` to nil.
  -- ⟦p⟧(1-list) recurses once then hits the base case ⇒ ⟨⟩.
  drain : Exp
  drain = kifnil var knil (call (ktl var))

  _ : run 80 (⌜ drain ⌝ ∙ (⌜ drain ⌝ ∙ (⟨⟩ ∙ ⟨⟩))) ≡ just ⟨⟩
  _ = refl
