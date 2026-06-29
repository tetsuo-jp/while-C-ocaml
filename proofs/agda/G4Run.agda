{-# OPTIONS --safe #-}
------------------------------------------------------------------------
-- G4, STEP 3: the universal `run` over ENCODED programs.
--
-- Closing G4 needs a concrete `PartialMixModel` (FutamuraPartial) whose carrier
-- is the value domain `V` and whose `run : V → V → Maybe V` interprets a program
-- given as DATA (⌜p⌝).  This module builds exactly that `run` by composing the
-- two halves already proven:
--   * `ProgEncoding` — `decode : V → Maybe Exp`, faithful (`decode ⌜e⌝ ≡ just e`);
--   * `UniversalInterp` — `Interp.run : Exp → ℕ → V → Maybe V`, the fuel-indexed
--     universal interpreter.
-- `run n code inp` decodes `code` to a program and interprets it on `inp`.
--
-- Proven: `run-encode` — running an ENCODED program equals running the program
-- directly (`run n ⌜p⌝ inp ≡ [p](inp)`), so the data representation costs nothing
-- semantically.  This is the `run` shape `FutamuraPartial.PartialMixModel.run`
-- consumes (Data := V).
--
-- WHAT REMAINS for G4 (the last, hard piece): a `mix : V` — an encoded program
-- that, run by this very `run`, SPECIALIZES, i.e. `run F mix (pair ⌜p⌝ s) ≡ just
-- (spec p s)` — which requires a self-interpreter-grade metaprogram in `Exp`
-- (the Agda counterpart of `universal.while`).  This module supplies the
-- substrate that metaprogram plugs into.
--
-- Referenced by docs/AGDA_CORRESPONDENCE.md (gap G4).
------------------------------------------------------------------------

module G4Run where

open import Data.Nat using (ℕ)
open import Relation.Binary.PropositionalEquality using (_≡_; refl)
open import UniversalInterp
  using (V; ⟨⟩; _∙_; Exp; var; ktl; Maybe; nothing; just; _>>=_; module Interp)
open import ProgEncoding using (⌜_⌝; decode; decode-⌜⌝)

------------------------------------------------------------------------
-- The universal run over programs-as-data: decode, then interpret.
------------------------------------------------------------------------

run : ℕ → V → V → Maybe V
run n code inp = decode code >>= λ p → Interp.run p n inp

------------------------------------------------------------------------
-- Faithfulness: running an ENCODED program = running the program directly.
-- (The `⌜⌝`/`decode` round-trip discharges the bind.)
------------------------------------------------------------------------

run-encode : ∀ n p inp → run n ⌜ p ⌝ inp ≡ Interp.run p n inp
run-encode n p inp rewrite decode-⌜⌝ p = refl

------------------------------------------------------------------------
-- Concrete check: the encoded program `tl(input)`, run on a 2-list, computes
-- the tail — entirely through decode + the universal interpreter.
------------------------------------------------------------------------

_ : run 2 ⌜ ktl var ⌝ ((⟨⟩ ∙ ⟨⟩) ∙ ⟨⟩) ≡ just ⟨⟩
_ = refl

-- a malformed code (not a valid program) decodes to nothing ⇒ run is nothing.
_ : run 2 ⟨⟩ ⟨⟩ ≡ nothing
_ = refl
