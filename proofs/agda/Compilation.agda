{-# OPTIONS --safe #-}
------------------------------------------------------------------------
-- COMPILATION PREORDER AND LANGUAGE EQUIVALENCE (Jones, Def 3.1.2
-- `def-simulate` / Def 3.3.1 `def-compile-same-data`, and the robustness
-- equivalences of ch.7-8 `robust-computable.tex`: WHILE ≡ I ≡ GOTO ≡ CM ≡ TM).
--
-- This is the STRUCTURAL backbone of the robustness theme.  A language (over a
-- fixed data domain `D`) is a set of programs with a semantics.  `L ⟶ M` is a
-- semantics-preserving COMPILER (`Prop 8.2.1`/`8.2.2`, `Thm 8.7.2` are concrete
-- instances).  The two facts every robustness proof silently uses:
--
--   * compilation is a PREORDER (`⟶-refl`, `⟶-trans`) -- so `Lem 16.3.2`
--     `lem-simulation-transitive` / `Lem 18.1.3` chains of equivalences compose;
--   * mutual compilation `≃` is an EQUIVALENCE relation (`≃-refl`/`≃-sym`/
--     `≃-trans`) -- so `WHILE ≃ GOTO ≃ I ≃ CM ≃ TM` is one equivalence class.
--
-- The concrete compilers themselves (`--onevar`, GOTO↔WHILE Böhm-Jacopini, the
-- 2-counter Gödel coding) are the remaining work in `docs/PROOF-OBLIGATIONS.md`;
-- this module is the framework they instantiate.  Undecidability transfer along a
-- compiler is `ReductionUndecidable`.
--
-- Referenced by docs/AGDA_CORRESPONDENCE.md and docs/BOOK-ROADMAP.md (ch.8).
------------------------------------------------------------------------

module Compilation where

open import Relation.Binary.PropositionalEquality using (_≡_; refl; trans)

------------------------------------------------------------------------
-- A language over a fixed data domain `D`: programs and their semantics.
------------------------------------------------------------------------

record Lang (D : Set) : Set₁ where
  field
    Prog : Set
    run  : Prog → D → D     -- ⟦p⟧ : D → D

------------------------------------------------------------------------
-- `L ⟶ M`: a semantics-preserving compiler from L to M (Def 3.3.1).
------------------------------------------------------------------------

record _⟶_ {D : Set} (L M : Lang D) : Set where
  open Lang
  field
    comp    : Prog L → Prog M
    correct : ∀ p d → run M (comp p) d ≡ run L p d

-- compilation is a preorder.
⟶-refl : ∀ {D} {L : Lang D} → L ⟶ L
⟶-refl = record { comp = λ p → p ; correct = λ p d → refl }

⟶-trans : ∀ {D} {L M N : Lang D} → L ⟶ M → M ⟶ N → L ⟶ N
⟶-trans LM MN = record
  { comp    = λ p → comp MN (comp LM p)
  ; correct = λ p d → trans (correct MN (comp LM p) d) (correct LM p d)
  }
  where open _⟶_

------------------------------------------------------------------------
-- Language equivalence: mutual compilation (the robustness relation).
------------------------------------------------------------------------

record _≃_ {D : Set} (L M : Lang D) : Set where
  field
    fwd : L ⟶ M
    bwd : M ⟶ L

≃-refl : ∀ {D} {L : Lang D} → L ≃ L
≃-refl = record { fwd = ⟶-refl ; bwd = ⟶-refl }

≃-sym : ∀ {D} {L M : Lang D} → L ≃ M → M ≃ L
≃-sym e = record { fwd = _≃_.bwd e ; bwd = _≃_.fwd e }

≃-trans : ∀ {D} {L M N : Lang D} → L ≃ M → M ≃ N → L ≃ N
≃-trans e f = record
  { fwd = ⟶-trans (_≃_.fwd e) (_≃_.fwd f)
  ; bwd = ⟶-trans (_≃_.bwd f) (_≃_.bwd e)
  }
