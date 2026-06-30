{-# OPTIONS --safe #-}
------------------------------------------------------------------------
-- G4: the full Jones picture -- ONE universal machine, the self-interpreter as
-- DATA, programs as DATA.
--
-- `G4Run.run` is the universal machine over encoded programs (decode + interpret).
-- `SelfInterp.sint` is the self-interpreter, itself an `Exp`; `⌜ sint ⌝` is it as
-- data.  Running the ENCODED self-interpreter on `(⌜p⌝ . (⌜p⌝ . inp))` computes
-- `⟦p⟧(inp)` -- everything (machine input, interpreter, interpreted program) is
-- data, exactly Jones's `universal.while` setup.
--
-- `self` is `G4Run.run-encode` instantiated at `sint`; the concrete results then
-- follow because the self-interpreter actually computes (verified, incl.
-- recursion).  This wires `UniversalInterp` + `ProgEncoding` + `G4Run` +
-- `SelfInterp` into one pipeline -- the substrate 4d's `PartialMixModel` runs on.
--
-- Referenced by docs/AGDA_CORRESPONDENCE.md (gap G4).
------------------------------------------------------------------------

module SelfInterpRun where

open import Relation.Binary.PropositionalEquality using (_≡_)
open import UniversalInterp
  using (V; ⟨⟩; _∙_; var; knil; ktl; kifnil; call; just; module Interp)
open import ProgEncoding using (⌜_⌝)
open import SelfInterp using (sint)
open import G4Run using (run; run-encode)

-- the universal machine running the ENCODED self-interpreter equals running the
-- self-interpreter directly (the encoding is semantically free, via run-encode).
self : ∀ n inp → run n ⌜ sint ⌝ inp ≡ Interp.run sint n inp
self n inp = run-encode n sint inp

------------------------------------------------------------------------
-- Concrete: the encoded universal machine, given ⌜sint⌝ and an encoded program,
-- computes that program's meaning -- including a recursive program.
------------------------------------------------------------------------

d : V
d = ⟨⟩ ∙ (⟨⟩ ∙ ⟨⟩)

-- run ⌜sint⌝ on (⌜tl(var)⌝ , (⌜tl(var)⌝ , d)) = tl d
_ : run 20 ⌜ sint ⌝ (⌜ ktl var ⌝ ∙ (⌜ ktl var ⌝ ∙ d)) ≡ just (⟨⟩ ∙ ⟨⟩)
_ = run-encode 20 sint (⌜ ktl var ⌝ ∙ (⌜ ktl var ⌝ ∙ d))

-- recursion through the encoded self-interpreter: encoded `drain` ⇒ ⟨⟩.
_ : run 80 ⌜ sint ⌝
      (⌜ kifnil var knil (call (ktl var)) ⌝
        ∙ (⌜ kifnil var knil (call (ktl var)) ⌝ ∙ (⟨⟩ ∙ ⟨⟩)))
    ≡ just ⟨⟩
_ = run-encode 80 sint
      (⌜ kifnil var knil (call (ktl var)) ⌝
        ∙ (⌜ kifnil var knil (call (ktl var)) ⌝ ∙ (⟨⟩ ∙ ⟨⟩)))
