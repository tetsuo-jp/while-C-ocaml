{-# OPTIONS --safe #-}
------------------------------------------------------------------------
-- Design spec for gap #2 (PAT-READ-ITER) of mix.while: why MKAV is
-- "necessary but NOT sufficient" — the leak is at the READ of the
-- partially-static input, not its construction.  Referenced by
-- docs/FUTAMURA.md.  Composes with MixBindingTime.agda.
--
-- Setting.  MixBindingTime proved the input BUILDER must be binding-time
-- aware (`if =? BT 'S`).  In the BT='S branch mix builds the partially-static
-- input  ('C . (('S . s) . ('D . (var i)))).  Under self-application (fp2) the
-- value `s` carried in that `'S` cell is itself the DYNAMIC half that the
-- generated compiler receives at run time — call its origin binding time
-- `bo : BT`.
--
-- mix reads the static half with avhd, which returns the cell verbatim:
--     avhd ('C.(('S.s).·)) = ('S . s)                      -- over-static READ
-- i.e. it commits `s` to static REGARDLESS of `bo`.  This module proves that
-- read is the remaining leak:
--   * read-ignores-origin : the naive read gives `sta s` for either origin;
--   * read-mistags        : when the origin is dynamic (fp2), the naive read
--                           differs from the binding-time-correct read;
--   * naive-bakes / correct-uses : the consequence downstream — a conditional
--                           specialised on the naive read bakes a branch and
--                           ignores its runtime input, while one on the
--                           origin-aware read residualises and uses it.
--
-- So the fix (PAT-READ-ITER) is to make the READ of a partial-static input
-- consult the static half's ORIGIN binding time, residualising it when dynamic
-- — exactly rwhile's depth-general read-pattern residualisation.
------------------------------------------------------------------------

module MixPatRead where

open import Relation.Binary.PropositionalEquality using (_≡_; refl)
open import Relation.Nullary using (¬_)

module Spec (V : Set) where

  data AV : Set where
    sta : V → AV
    dyn : AV

  -- origin binding time of the static half `s` (st under fp1, dy under fp2).
  data BT : Set where st dy : BT

  -- The partially-static input cell as built by MKAV's BT='S branch carries the
  -- value `s`; what we model here is how it is READ back.

  -- naive read (mix.while's avhd today): returns the cell as static, always.
  naiveRead : V → AV
  naiveRead s = sta s

  -- origin-aware read (PAT-READ-ITER): residualise when the half's origin is
  -- dynamic.
  patRead : BT → V → AV
  patRead st s = sta s
  patRead dy s = dyn

  ----------------------------------------------------------------------
  -- The naive read ignores the origin binding time.
  read-ignores-origin : ∀ s → naiveRead s ≡ patRead st s
  read-ignores-origin s = refl

  -- fp2 (origin dynamic): the naive read MIS-TAGS — it differs from the
  -- binding-time-correct read.
  read-mistags : ∀ s → ¬ (naiveRead s ≡ patRead dy s)
  read-mistags s ()

  ----------------------------------------------------------------------
  -- Downstream consequence: specialise a guarded operation on the read AV.

  data Res : Set where
    konst : V → Res
    resid : (V → V) → Res

  run : Res → V → V
  run (konst c) _ = c
  run (resid f) d = f d

  specOp : (V → V) → AV → Res
  specOp f (sta v) = konst (f v)
  specOp f dyn     = resid f

  -- specialising on the naive read bakes a constant -> ignores runtime input:
  naive-bakes : ∀ f s d₁ d₂
              → run (specOp f (naiveRead s)) d₁ ≡ run (specOp f (naiveRead s)) d₂
  naive-bakes f s d₁ d₂ = refl

  -- specialising on the origin-aware read (fp2) residualises -> uses the input:
  correct-uses : ∀ f s d → run (specOp f (patRead dy s)) d ≡ f d
  correct-uses f s d = refl
