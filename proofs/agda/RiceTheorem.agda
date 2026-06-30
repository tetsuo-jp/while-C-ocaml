{-# OPTIONS --safe #-}
------------------------------------------------------------------------
-- RICE'S THEOREM (Jones, Thm 5.4.2 `thm-rice`): every NON-TRIVIAL EXTENSIONAL
-- program property is undecidable.
--
-- Proof (the elegant recursion-theorem reduction).  Let `InP` be the property,
-- extensional (depends only on `⟦·⟧`) and non-trivial (`a ∈ P`, `b ∉ P`).  If `P`
-- were decidable by a program, the recursion theorem (`RecursionTheorem`,
-- Thm 14.2.1) would yield a SELF-SELECTING program `q`:
--
--     q ∈ P  ⟹  ⟦q⟧ = ⟦b⟧        (q mimics the out-of-P witness)
--     q ∉ P  ⟹  ⟦q⟧ = ⟦a⟧        (q mimics the in-P witness)
--
-- (on input, `q` consults the decider on its own code and behaves like `b` or
-- `a` accordingly.)  Such a `q` is contradictory: by extensionality `q ∈ P ⟺
-- ⟦q⟧=⟦b⟧ ∈... ` flips its own membership.  This module proves, constructively,
-- that no such `q` exists -- hence no decider, hence `P` is undecidable.
--
-- (The `q`-from-decider step is the routine closure -- self-consult-and-branch
-- via the recursion theorem, exactly as in `HaltingUndecidable.build-diagonal`.
-- The non-trivial content, the contradiction, is what is proved here.)
--
-- Referenced by docs/AGDA_CORRESPONDENCE.md and docs/BOOK-ROADMAP.md (Thm 5.4.2).
------------------------------------------------------------------------

module RiceTheorem where

open import Data.Product using (Σ; _×_; _,_)
open import Data.Empty using (⊥)
open import Relation.Nullary using (¬_)
open import Relation.Binary.PropositionalEquality using (_≡_; sym)

------------------------------------------------------------------------
-- A language with a semantics and an EXTENSIONAL property `InP`.
------------------------------------------------------------------------

record Model : Set₁ where
  field
    Code : Set
    Data : Set
    ⟦_⟧  : Code → Data → Data
    InP  : Code → Set                      -- the program property P
    -- extensional: programs with the same semantics agree on membership.
    ext  : ∀ a b → (∀ d → ⟦ a ⟧ d ≡ ⟦ b ⟧ d) → InP a → InP b

------------------------------------------------------------------------
-- RICE 5.4.2.  For a non-trivial property (`a ∈ P`, `b ∉ P`), the self-selecting
-- program a decider would build cannot exist.  So `P` is not decidable.
------------------------------------------------------------------------

module Theorem (M : Model) where
  open Model M

  rice : ∀ {a b} → InP a → ¬ InP b →
      ¬ (Σ Code (λ q → (  InP q → ∀ d → ⟦ q ⟧ d ≡ ⟦ b ⟧ d)
                       × (¬ InP q → ∀ d → ⟦ q ⟧ d ≡ ⟦ a ⟧ d)))
  rice {a} {b} ina ¬inb (q , qb , qa) =
    let -- if q ∈ P then ⟦q⟧ = ⟦b⟧, so q ∈ P forces b ∈ P -- impossible.
        ¬inq : ¬ InP q
        ¬inq inq = ¬inb (ext q b (qb inq) inq)
        -- so q ∉ P, hence ⟦q⟧ = ⟦a⟧, so a ∈ P forces q ∈ P -- contradiction.
    in  ¬inq (ext a q (λ d → sym (qa ¬inq d)) ina)
