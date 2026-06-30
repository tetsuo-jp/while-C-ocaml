{-# OPTIONS --safe #-}
------------------------------------------------------------------------
-- SEMI-DECIDABILITY: HALT is semi-decidable but its complement is not
-- (Jones, Thm 5.6.1 `thm-halt-semi` / Cor 5.6.2 `cor-halt-not-semidecidable`,
-- with the underlying Post-style lemma towards Thm 5.7.2
-- `thm-re-characterizations`).
--
-- A set `S` is SEMI-DECIDABLE if some program halts on `x` exactly when `x ∈ S`;
-- DECIDABLE if some total program outputs `true`/`false` matching membership.
-- The pivotal lemma (Post): if `S` AND its complement are semi-decidable then `S`
-- is decidable -- the deciding program DOVETAILS the two semi-deciders (run both
-- in parallel; one must halt).  We take that routine parallel construction as the
-- `Post` hypothesis and prove the consequence:
--
--   COR 5.6.2.  If `S` is semi-decidable but UNDECIDABLE, then its complement is
--   NOT semi-decidable.
--
-- Applied to the self-halting set `SH p = Halts p p`: `SH` is semi-decidable
-- (Thm 5.6.1 -- run `p` on its own code via the self-interpreter `SelfInterp`) and
-- undecidable (Thm 5.3.1 -- `HaltingUndecidable`), so the complement of
-- self-halting is not semi-decidable.  The argument is one constructive step.
--
-- Referenced by docs/AGDA_CORRESPONDENCE.md and docs/BOOK-ROADMAP.md (Thm 5.6.x).
------------------------------------------------------------------------

module SemiDecidable where

open import Data.Bool using (Bool; true; false)
open import Data.Product using (Σ; _×_; _,_)
open import Relation.Nullary using (¬_)

------------------------------------------------------------------------
-- An abstract language with a termination predicate and boolean outputs.
------------------------------------------------------------------------

record Model : Set₁ where
  field
    Code   : Set
    Halts  : Code → Code → Set
    Yields : Code → Code → Bool → Set

  -- complement of a set of codes.
  ∁ : (Code → Set) → (Code → Set)
  ∁ S x = ¬ S x

  -- `e` semi-decides `S`: `e` halts on `x` iff `x ∈ S`.
  SemiDec : (Code → Set) → Set
  SemiDec S = Σ Code (λ e → ∀ x → (S x → Halts e x) × (Halts e x → S x))

  -- `h` decides `S`: total, outputs `true` on members and `false` on non-members.
  Dec : (Code → Set) → Set
  Dec S = Σ Code (λ h → ∀ x → (S x → Yields h x true) × (¬ S x → Yields h x false))

------------------------------------------------------------------------
-- Cor 5.6.2 (general form).  Post's lemma + undecidability ⇒ the complement of a
-- semi-decidable undecidable set is not semi-decidable.
------------------------------------------------------------------------

module Theorem (M : Model) where
  open Model M

  -- Post's lemma (the dovetailing construction, taken as a hypothesis):
  -- `S` and its complement both semi-decidable ⇒ `S` decidable.
  Post : (Code → Set) → Set
  Post S = SemiDec S → SemiDec (∁ S) → Dec S

  complement-not-semidec : ∀ {S} →
      Post S → ¬ Dec S → SemiDec S → ¬ SemiDec (∁ S)
  complement-not-semidec post ¬dec sdS sdcoS = ¬dec (post sdS sdcoS)

  -- Specialised to self-halting `SH p = Halts p p` (Thm 5.6.1 gives `SemiDec SH`,
  -- Thm 5.3.1 gives `¬ Dec SH`): the complement of self-halting is not
  -- semi-decidable.
  SH : Code → Set
  SH p = Halts p p

  co-self-halting-not-semidec :
      Post SH → ¬ Dec SH → SemiDec SH → ¬ SemiDec (∁ SH)
  co-self-halting-not-semidec = complement-not-semidec
