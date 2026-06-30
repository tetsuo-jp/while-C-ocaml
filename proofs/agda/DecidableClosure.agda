{-# OPTIONS --safe #-}
------------------------------------------------------------------------
-- CLOSURE OF DECIDABLE SETS (Jones, Thm 5.5.1 `thm-decidable-basics` /
-- Thm 5.5.2 `thm-decidable-semidecidable-closure-properties`): the decidable
-- subsets of D are closed under intersection, union, and complement.
--
-- A set is modelled as a predicate `S : A → Set`; it is DECIDABLE when membership
-- is pointwise decidable (`∀ x → Dec (S x)`) -- the constructive reading of "a
-- program decides membership".  The closures are then proved directly on `Dec`.
-- (Semi-decidable closure under ∩/∪, and the decidable⇔both-semidecidable link,
-- are in `SemiDecidable`; the undecidable witness is `HaltingUndecidable`.)
--
-- Referenced by docs/AGDA_CORRESPONDENCE.md and docs/BOOK-ROADMAP.md (Thm 5.5.x).
------------------------------------------------------------------------

module DecidableClosure where

open import Relation.Nullary using (¬_; Dec; yes; no)
open import Data.Product using (_×_; _,_)
open import Data.Sum using (_⊎_; inj₁; inj₂)

-- a subset of `A` is decidable when membership is pointwise decidable.
Decidable : {A : Set} → (A → Set) → Set
Decidable {A} S = (x : A) → Dec (S x)

module _ {A : Set} {S T : A → Set} where

  -- closed under intersection.
  dec-∩ : Decidable S → Decidable T → Decidable (λ x → S x × T x)
  dec-∩ ds dt x with ds x | dt x
  ... | yes p | yes q = yes (p , q)
  ... | yes p | no ¬q = no (λ { (_ , q) → ¬q q })
  ... | no ¬p | _     = no (λ { (p , _) → ¬p p })

  -- closed under union.
  dec-∪ : Decidable S → Decidable T → Decidable (λ x → S x ⊎ T x)
  dec-∪ ds dt x with ds x | dt x
  ... | yes p | _     = yes (inj₁ p)
  ... | no ¬p | yes q = yes (inj₂ q)
  ... | no ¬p | no ¬q = no (λ { (inj₁ p) → ¬p p ; (inj₂ q) → ¬q q })

module _ {A : Set} {S : A → Set} where

  -- closed under complement.
  dec-∁ : Decidable S → Decidable (λ x → ¬ S x)
  dec-∁ ds x with ds x
  ... | yes p = no (λ ¬p → ¬p p)
  ... | no ¬p = yes ¬p
