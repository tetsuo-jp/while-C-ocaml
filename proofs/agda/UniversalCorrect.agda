{-# OPTIONS --safe #-}

------------------------------------------------------------------------
-- The universal program is correct for EVERY program (G1 ⇒ oracle agreement).
--
-- Composing G1 (`SelfInterpCorrect.simF`: the self-interpreter matches the direct
-- interpreter for all programs, incl. recursion/`call`) with `G4Run.run-encode`
-- (running an ENCODED program is semantically free) gives: the ONE universal
-- machine `G4Run.run`, fed the encoded self-interpreter `⌜ sint ⌝` and the pair
-- `(⌜ p ⌝ . (⌜ p ⌝ . inp))`, computes exactly `⟦ p ⟧(inp)` for EVERY program `p`
-- and input `inp`, whenever `p` terminates.
--
-- This is Jones's universal program `u` (`universal.while`), machine-checked in
-- full generality: the `rundata`/`u` oracle-agreement (PROOF-OBLIGATIONS §6) and
-- the base fact under the §2 discharges.  `SelfInterpRun` only checked concrete
-- programs by `refl`; this is the ∀-quantified statement.
------------------------------------------------------------------------

module UniversalCorrect where

open import Data.Nat using (ℕ)
open import Data.Product using (Σ; _,_)
open import Relation.Binary.PropositionalEquality using (_≡_; trans)

open import UniversalInterp using (V; _∙_; just; module Interp)
open import ProgEncoding using (⌜_⌝)
open import SelfInterp using (sint)
open import SelfInterpCorrect using (simF)
open import G4Run using (run; run-encode)

-- Running program `p` on `inp` in the direct interpreter (fuel `G`, global = `p`)
-- IS `Interp.run p G inp = Interp.eval p G p inp`.  Whenever that yields `v`, the
-- encoded universal machine reaches the same `v` for some fuel `F`.
universal-correct : ∀ G p inp {v} →
  Interp.eval p G p inp ≡ just v →
  Σ ℕ (λ F → run F ⌜ sint ⌝ (⌜ p ⌝ ∙ (⌜ p ⌝ ∙ inp)) ≡ just v)
universal-correct G p inp h with simF G p h
... | F , ih = F , trans (run-encode F sint (⌜ p ⌝ ∙ (⌜ p ⌝ ∙ inp))) ih
