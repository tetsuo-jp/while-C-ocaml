{-# OPTIONS --safe #-}

------------------------------------------------------------------------
-- G4, STEP 4b': the spec equation for a STANDALONE residual (recursion resolved).
--
-- `MixSpec.specE-sound` proves the spec equation `[residual](d) = [p]((sv,d))` for
-- a residual `call (kcons (quoteV sv) var)` that DELEGATES to the AMBIENT program
-- `p` (its "honest scope" note flags that a recursive `p` run standalone -- with
-- itself as the global program -- would break, because its own `call`s have no
-- ambient `p` to reach).
--
-- The fix Jones uses: the standalone residual carries the program as DATA and runs
-- it through the self-interpreter.  Here the residual is the universal machine
-- `G4Run.run` fed `⌜ sint ⌝` and carrying `⌜ p ⌝`; its recursion goes through
-- `sint`'s own `call`, so no ambient `p` is needed.  The spec equation then holds
-- for EVERY (possibly recursive) `p` -- it is `UniversalCorrect.universal-correct`
-- specialised to a partially-static input `(sv . d)`.  This discharges the §2
-- obligation "MixSpec standalone residual for recursive p".
------------------------------------------------------------------------

module MixSpecStandalone where

open import Data.Nat using (ℕ)
open import Data.Product using (Σ; _,_)

open import UniversalInterp using (V; _∙_; just; module Interp)
open import ProgEncoding using (⌜_⌝)
open import SelfInterp using (sint)
open import G4Run using (run)
open import UniversalCorrect using (universal-correct)
open import Relation.Binary.PropositionalEquality using (_≡_)

-- THE STANDALONE SPEC EQUATION.  The residual for static value `sv` and program
-- `p` is `run ⌜sint⌝` carrying `⌜p⌝`; run on the dynamic input `d` (as the pair
-- `(sv . d)`) it computes `⟦p⟧(sv . d)`, for every terminating -- incl. recursive
-- -- program `p`.  Unlike `MixSpec.specE-sound`, `p` needs no ambient copy of
-- itself: recursion is carried inside `⌜p⌝` and served by `sint`.
standalone-spec : ∀ G p sv d {v} →
  Interp.eval p G p (sv ∙ d) ≡ just v →
  Σ ℕ (λ F → run F ⌜ sint ⌝ (⌜ p ⌝ ∙ (⌜ p ⌝ ∙ (sv ∙ d))) ≡ just v)
standalone-spec G p sv d h = universal-correct G p (sv ∙ d) h
