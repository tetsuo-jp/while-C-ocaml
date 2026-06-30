{-# OPTIONS --safe #-}
------------------------------------------------------------------------
-- G1 (THE KEYSTONE): general self-interpreter correctness -- started.
--
-- Goal (`docs/PROOF-OBLIGATIONS.md` §G1): the self-interpreter `SelfInterp.sint`
-- computes the meaning of EVERY program for ALL inputs, not just the concrete
-- programs `SelfInterp` checks by `refl`.  This module sets up the statement, the
-- target semantics, and the BASE cases; the recursive cases are the continuation.
--
-- Target semantics.  For the CALL-FREE fragment, a program's meaning is the total
-- function `ev0 : Exp → V → V` (var ↦ env, knil ↦ ⟨⟩, kcons ↦ cons, khd/ktl ↦
-- hd/tl, kifnil ↦ ifnil).  G1 for this fragment is:
--
--     ∀ e → CallFree e → ∀ p env → Σ F, run sint F (⌜p⌝ ∙ (⌜e⌝ ∙ env)) ≡ just (ev0 e env)
--
-- Status.  The LEAF cases (`var`, `knil`) hold by `refl` at a constant fuel (the
-- dispatch resolves on the concrete tag of `⌜e⌝`).  The RECURSIVE cases
-- (kcons/khd/ktl/kifnil) need the inductive hypothesis lifted by fuel-monotonicity
-- (`UniversalInterp.Mono.add-fuel`) and an explicit unfolding of `sint`'s dispatch
-- to expose the sub-request `run sint Fₑ (⌜p⌝.(⌜sub⌝.env))` -- a probe showed the
-- inner fuel is NOT a simple constant offset of `Fₑ`, so a one-line `rewrite` does
-- not suffice; this is the remaining G1 work.  The `call` case adds the object
-- recursion (re-interpret `⌜p⌝`) and needs an induction on the OBJECT eval fuel.
--
-- Referenced by docs/AGDA_CORRESPONDENCE.md and docs/PROOF-OBLIGATIONS.md (§G1).
------------------------------------------------------------------------

module SelfInterpCorrect where

open import Data.Nat using (ℕ)
open import Relation.Binary.PropositionalEquality using (_≡_; refl)
open import UniversalInterp
  using (V; ⟨⟩; _∙_; hd; tl; ifnil; Exp; var; knil; kcons; khd; ktl; kifnil; call;
         just; module Interp)
open import ProgEncoding using (⌜_⌝)
open import SelfInterp using (sint)

------------------------------------------------------------------------
-- The target: the meaning of a call-free program (a total function).
------------------------------------------------------------------------

ev0 : Exp → V → V
ev0 var          env = env
ev0 knil         env = ⟨⟩
ev0 (kcons a b)  env = ev0 a env ∙ ev0 b env
ev0 (khd e)      env = hd (ev0 e env)
ev0 (ktl e)      env = tl (ev0 e env)
ev0 (kifnil c t e) env = ifnil (ev0 c env) (ev0 t env) (ev0 e env)
ev0 (call e)     env = ⟨⟩          -- dummy; `call` is excluded by `CallFree`

-- the call-free fragment (where `ev0` is the genuine semantics).
data CallFree : Exp → Set where
  cf-var   : CallFree var
  cf-knil  : CallFree knil
  cf-kcons : ∀ {a b} → CallFree a → CallFree b → CallFree (kcons a b)
  cf-khd   : ∀ {e} → CallFree e → CallFree (khd e)
  cf-ktl   : ∀ {e} → CallFree e → CallFree (ktl e)
  cf-kifnil : ∀ {c t e} → CallFree c → CallFree t → CallFree e → CallFree (kifnil c t e)

------------------------------------------------------------------------
-- Base cases: the self-interpreter computes `var` and `knil` for all programs
-- and environments (the dispatch resolves on the concrete tag).
------------------------------------------------------------------------

open Interp sint

sint-var : ∀ p env → run 8 (p ∙ (⌜ var ⌝ ∙ env)) ≡ just (ev0 var env)
sint-var p env = refl

sint-knil : ∀ p env → run 8 (p ∙ (⌜ knil ⌝ ∙ env)) ≡ just (ev0 knil env)
sint-knil p env = refl
