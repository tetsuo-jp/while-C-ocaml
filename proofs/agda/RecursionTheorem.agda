{-# OPTIONS --safe #-}
------------------------------------------------------------------------
-- KLEENE'S SECOND RECURSION THEOREM (Jones, Thm 14.2.1
-- `thm-kleenes-2-recursion`): for every program `p` there is a program `q` with
--
--     ⟦q⟧(d) = ⟦p⟧(⌜q⌝ . d)   for all d,
--
-- i.e. `q` can access its own source.  This is a direct corollary of the s-m-n
-- theorem (Jones Thm 5.2.1, formalised here as `FutamuraProjections.mix-eq` /
-- `MixSpec.specE-sound`): the specializer's self-application gives the fixpoint.
--
-- The construction: with `spec` the s-m-n specializer (`⟦spec p s⟧(d) =
-- ⟦p⟧(s.d)`), let `vʰᵃᵗ = spec v ⌜v⌝` (`v` specialized to its own code, so
-- `⟦vʰᵃᵗ⟧(d) = ⟦v⟧(⌜v⌝.d)`).  Build `e` with `⟦e⟧(⌜v⌝.d) = ⟦p⟧(⌜vʰᵃᵗ⟧.d)`
-- (compose self-specialization with `p` -- a routine closure, the `build`
-- hypothesis).  Then `q := spec e ⌜e⌝` is the fixpoint:
--     ⟦q⟧(d) = ⟦e⟧(⌜e⌝.d) = ⟦p⟧(⌜spec e ⌜e⌝⟧.d) = ⟦p⟧(⌜q⌝.d).
--
-- (The model uses a total `⟦_⟧`; the proof is purely equational — `trans` of two
-- given equations — so it transfers unchanged to a partial/`Maybe` semantics.)
--
-- Referenced by docs/AGDA_CORRESPONDENCE.md and docs/BOOK-ROADMAP.md (Thm 14.2.1).
------------------------------------------------------------------------

module RecursionTheorem where

open import Data.Product using (Σ; _,_)
open import Relation.Binary.PropositionalEquality using (_≡_; trans)

------------------------------------------------------------------------
-- A language with programs-as-data, pairing, and an s-m-n specializer.
------------------------------------------------------------------------

record Model : Set₁ where
  field
    Code : Set
    Data : Set
    ⟦_⟧  : Code → Data → Data           -- semantics
    code : Code → Data                  -- ⌜_⌝ : program as data
    pair : Data → Data → Data
    spec : Code → Data → Code           -- the s-m-n specializer
    -- s-m-n (Jones 5.2.1): specializing `p` to static `s` fixes the first input.
    spec-eq : ∀ p s d → ⟦ spec p s ⟧ d ≡ ⟦ p ⟧ (pair s d)

------------------------------------------------------------------------
-- THEOREM 14.2.1.  Given the routine closure that builds `e` from `p`, every
-- program has a fixpoint `q` reading its own code.
------------------------------------------------------------------------

module Theorem (M : Model) where
  open Model M

  recursion-theorem :
      (∀ p → Σ Code (λ e → ∀ v d →
          ⟦ e ⟧ (pair (code v) d) ≡ ⟦ p ⟧ (pair (code (spec v (code v))) d))) →
      ∀ p → Σ Code (λ q → ∀ d → ⟦ q ⟧ d ≡ ⟦ p ⟧ (pair (code q) d))
  recursion-theorem build p with build p
  ... | e , e-eq = spec e (code e) , λ d → trans (spec-eq e (code e) d) (e-eq e d)
