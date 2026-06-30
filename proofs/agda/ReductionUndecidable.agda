{-# OPTIONS --safe #-}
------------------------------------------------------------------------
-- MANY-ONE REDUCTION transfers undecidability (the engine behind Jones's
-- undecidability corollaries, e.g. Cor 8.7.3 `cor-2CM-universal`, and the lift
-- from self-halting to the general halting problem of Thm 5.3.1).
--
-- A reduction `S ≤ₘ T via f` is a function `f` with `x ∈ S ⟺ f x ∈ T`.  Then
-- decidability pulls back along `f` (`reduce`), so undecidability pushes forward
-- (`reduce-undec`).  Decidability is the pointwise notion from `DecidableClosure`.
--
-- Corollary (in `HaltCorollary`): self-halting `SH p = Halts p p` reduces to the
-- general halting set `HALT (p,d) = Halts p d` via `p ↦ (p,p)`, so the general
-- halting problem is undecidable once self-halting is (which
-- `HaltingUndecidable`, Thm 5.3.1, gives).
--
-- Referenced by docs/AGDA_CORRESPONDENCE.md and docs/BOOK-ROADMAP.md.
------------------------------------------------------------------------

module ReductionUndecidable where

open import Data.Product using (_×_; _,_; proj₁; proj₂)
open import Relation.Nullary using (¬_; Dec; yes; no)
open import DecidableClosure using (Decidable)

------------------------------------------------------------------------
-- Many-one reduction and the transfer lemmas.
------------------------------------------------------------------------

-- `S ≤ₘ T via f`: membership in S is mirrored by membership of `f x` in T.
Reduces : {A B : Set} → (A → Set) → (B → Set) → (A → B) → Set
Reduces S T f = ∀ x → (S x → T (f x)) × (T (f x) → S x)

-- decidability pulls back along a reduction.
reduce : {A B : Set} {S : A → Set} {T : B → Set} {f : A → B} →
         Reduces S T f → Decidable T → Decidable S
reduce {f = f} red dt x with dt (f x)
... | yes tfx = yes (proj₂ (red x) tfx)
... | no ¬tfx = no (λ sx → ¬tfx (proj₁ (red x) sx))

-- hence undecidability pushes forward: if S is undecidable and reduces to T,
-- then T is undecidable.
reduce-undec : {A B : Set} {S : A → Set} {T : B → Set} {f : A → B} →
               Reduces S T f → ¬ Decidable S → ¬ Decidable T
reduce-undec red ¬decS decT = ¬decS (reduce red decT)

------------------------------------------------------------------------
-- Corollary: the GENERAL halting problem is undecidable (lifting Thm 5.3.1 from
-- self-halting via the trivial reduction `p ↦ (p,p)`).
------------------------------------------------------------------------

module HaltCorollary {Code : Set} (Halts : Code → Code → Set) where

  SH : Code → Set                       -- self-halting: p halts on its own code
  SH p = Halts p p

  HALT : (Code × Code) → Set            -- general halting: p halts on d
  HALT (p , d) = Halts p d

  -- self-halting reduces to general halting via `p ↦ (p,p)`.
  SH≤HALT : Reduces SH HALT (λ p → (p , p))
  SH≤HALT p = (λ z → z) , (λ z → z)

  -- so general HALT is undecidable once self-halting is (HaltingUndecidable 5.3.1).
  general-halt-undecidable : ¬ Decidable SH → ¬ Decidable HALT
  general-halt-undecidable = reduce-undec SH≤HALT
