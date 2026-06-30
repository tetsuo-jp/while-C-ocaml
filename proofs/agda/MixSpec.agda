{-# OPTIONS --safe #-}
------------------------------------------------------------------------
-- G4, STEP 4b: the spec equation for an Exp residual, via `quoteV`.
--
-- Building on `MixQuote` (4a), this gives a residual EXPRESSED IN `Exp` whose
-- meaning satisfies the spec equation.  The two-input convention: a program's
-- input is a pair `(static . dynamic)`.  Specializing to a static value `sv`, the
-- residual `call (kcons (quoteV sv) var)` reconstructs the pair `(sv . d)` from
-- the dynamic input `d` (folding `sv` in via `quoteV`) and hands it to the
-- ambient program via the interpreter's `call`.
--
-- Proven (`specE-sound`), fuel-relative: under the universal interpreter loaded
-- with program `p` (`Interp p`), the residual run on `d` equals `p` run on the
-- pair `(sv . d)` -- i.e. `[residual](d) = [p]((sv,d))`, the spec equation.
--
-- HONEST SCOPE.  This residual delegates recursion to the AMBIENT program `p`
-- (the interpreter's global).  Under Jones's standalone semantics (G4Run, where a
-- residual is run with ITSELF as the global), a recursive `p`'s own `call`s would
-- have to be handled too -- which is precisely what a self-interpreter does.  So
-- step 4c (`mixProg` carrying ⌜p⌝ to an interpreter) remains the piece that lifts
-- this to standalone residuals; this lemma is the spec equation in the
-- program-loaded model and isolates exactly that gap.
--
-- Referenced by docs/AGDA_CORRESPONDENCE.md (gap G4).
------------------------------------------------------------------------

module MixSpec where

open import Data.Nat using (ℕ; suc)
open import Relation.Binary.PropositionalEquality using (_≡_; refl)
open import UniversalInterp
  using (V; ⟨⟩; _∙_; Exp; var; knil; kcons; ktl; call; just; module Interp)
open import MixQuote using (quoteV; sz; quote-ok)

module _ (p : Exp) where
  open Interp p   -- the program being specialized is the ambient global program

  -- the residual for static value `sv`: call the ambient program with `(sv . d)`.
  specResidual : V → Exp
  specResidual sv = call (kcons (quoteV sv) var)

  -- reconstructing the (static . dynamic) pair from the dynamic input.
  pair-ok : ∀ sv d → eval (suc (suc (sz sv))) (kcons (quoteV sv) var) d ≡ just (sv ∙ d)
  pair-ok sv d rewrite quote-ok p sv d = refl

  -- THE SPEC EQUATION (fuel-relative): the residual on `d` ≡ `p` on `(sv . d)`.
  specE-sound : ∀ sv d →
    eval (suc (suc (suc (sz sv)))) (specResidual sv) d
      ≡ eval (suc (suc (sz sv))) p (sv ∙ d)
  specE-sound sv d rewrite pair-ok sv d = refl

------------------------------------------------------------------------
-- Concrete check: load `p = tl(input)`; the residual for static `⟨⟩`, run on a
-- dynamic value `d`, returns `d` (the tail of the reconstructed pair `(⟨⟩ . d)`).
------------------------------------------------------------------------

module Example where
  open Interp (ktl var)   -- load p = tl(input)

  -- specResidual ⟨⟩ = call (kcons (quoteV ⟨⟩) var) = call (kcons knil var)
  _ : eval 3 (call (kcons knil var)) (⟨⟩ ∙ ⟨⟩) ≡ just (⟨⟩ ∙ ⟨⟩)
  _ = refl
