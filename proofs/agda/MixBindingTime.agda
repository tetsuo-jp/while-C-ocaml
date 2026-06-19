{-# OPTIONS --safe #-}
------------------------------------------------------------------------
-- Binding-time correctness of mix.while's MKAV input builder.
--
-- This is the non-reversible analogue of rwhile's RWhileRevProj2BT.agda,
-- specialised to THIS repository's partial evaluator
-- `examples/desugar/mix.while`.  In mix.while the read variable's initial
-- annotated value is built from a binding-time tag BT:
--
--     if =? BT 'S then
--       InputAV := cons 'C (cons (cons 'S Sval) (cons 'D VarI));   -- partial static
--     else
--       InputAV := cons 'D VarI;                                   -- fully dynamic
--
-- This module models the essence (a single static-vs-dynamic input half) and
-- proves WHY that conditional must branch on BT, rather than committing to
-- 'S unconditionally:
--
--   * fp1-ok            : when the input half is genuinely static, the
--                         BT-aware builder agrees with an unconditional-'S one;
--   * fp2-mistags       : under self-application the input half is DYNAMIC,
--                         and an unconditional-'S builder MIS-TAGS it;
--   * buggy-ignores-input / correct-uses-input :
--                         the consequence — an over-static residual bakes a
--                         constant and ignores its runtime input, whereas the
--                         BT-aware residual uses it.
--
-- So mix.while's `if =? BT 'S` is the necessary BTA/two-level discipline
-- (Futamura/Jones).  As rwhile records, this is necessary but NOT sufficient
-- for the 2nd projection (see MixEmitModel.agda for the next obstacle).
------------------------------------------------------------------------

module MixBindingTime where

open import Relation.Binary.PropositionalEquality using (_≡_; refl)
open import Relation.Nullary using (¬_)

module Spec (V : Set) where

  -- Annotated value: a known static value, or dynamic (runtime-only).
  -- (mix.while's third shape, partially-static ('C ...), is irrelevant to the
  -- binding-time point and omitted here.)
  data AV : Set where
    sta : V → AV          -- ('S . v)
    dyn : AV              -- ('D . <residual code>)

  -- Residual of a unary operation that mix may specialise.
  data Res : Set where
    konst : V → Res          -- baked constant: ignores the runtime input
    resid : (V → V) → Res    -- the operation kept: uses the runtime input

  run : Res → V → V
  run (konst c) _ = c
  run (resid f) d = f d

  -- One specialisation step for op `f` against the input's annotated value.
  -- (mix.while: a static AV folds via avhd/avtl/aveq; a dynamic AV residualises.)
  specOp : (V → V) → AV → Res
  specOp f (sta v) = konst (f v)
  specOp f dyn     = resid f

  ----------------------------------------------------------------------
  -- The two input builders.

  -- BUG: tag the static half unconditionally 'S (mix.while WITHOUT the BT test).
  mkBuggy : V → AV
  mkBuggy v = sta v

  -- FIX: tag by the input half's actual binding time (mix.while's `if =? BT 'S`).
  data BT : Set where st dy : BT
  mkAV : BT → V → AV
  mkAV st v = sta v
  mkAV dy v = dyn

  ----------------------------------------------------------------------
  -- fp1: the input half is genuinely static (BT = st).  Buggy and BT-aware
  -- builders AGREE, so fp1 is correct either way (mix.while's fp1 is green).
  fp1-ok : ∀ f v → specOp f (mkBuggy v) ≡ specOp f (mkAV st v)
  fp1-ok f v = refl

  -- fp2 (self-application): the input half is DYNAMIC (BT = dy).  The
  -- unconditional-'S builder MIS-TAGS it — its residual differs from the
  -- correct one.
  fp2-mistags : ∀ f v → ¬ (specOp f (mkBuggy v) ≡ specOp f (mkAV dy v))
  fp2-mistags f v ()

  ----------------------------------------------------------------------
  -- Consequence of mis-tagging a dynamic input half as static.

  -- the buggy residual ignores its runtime input (it baked a constant):
  buggy-ignores-input : ∀ f v d₁ d₂
                      → run (specOp f (mkBuggy v)) d₁ ≡ run (specOp f (mkBuggy v)) d₂
  buggy-ignores-input f v d₁ d₂ = refl

  -- the BT-aware residual USES its runtime input:
  correct-uses-input : ∀ f v d → run (specOp f (mkAV dy v)) d ≡ f d
  correct-uses-input f v d = refl

  ----------------------------------------------------------------------
  -- fp1 correctness (unary core) for the BT-aware builder, both binding times:
  --   static input  -> residual bakes the compile-time value f v;
  --   dynamic input -> residual computes f on its runtime input d.
  fp1-static-bakes : ∀ f v d → run (specOp f (mkAV st v)) d ≡ f v
  fp1-static-bakes f v d = refl

  fp1-dynamic-residual : ∀ f v d → run (specOp f (mkAV dy v)) d ≡ f d
  fp1-dynamic-residual = correct-uses-input
