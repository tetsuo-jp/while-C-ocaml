{-# OPTIONS --safe #-}

------------------------------------------------------------------------
-- Programs are countable: an injective Gödel numbering `index : Exp → ℕ`
-- (Levin plan Layer B2, `docs/LEVIN-PLAN.md`).
--
-- Route: `Exp --⌜_⌝--> V --vnat--> ℕ`, both legs injective.
--   * `⌜_⌝` is injective because `ProgEncoding.decode` is its left inverse
--     (`decode-⌜⌝`), so `⌜p⌝ ≡ ⌜q⌝ ⇒ p ≡ q`;
--   * `vnat` numbers the binary tree `V` via `NatPairing.pair` on the two
--     subtrees (only two constructors ⇒ a 4-case injectivity proof).
-- Composing gives `index` with `index-injective`: distinct programs get distinct
-- numbers, so each solver `q` has a unique index `j := index q` -- the constant
-- `LevinOptimal`'s `idx` field ranges over.
--
-- (The total surjective decoder `enum : ℕ → Exp` with `enum (index p) ≡ p`, needed
-- by the operational dovetail in Layer B3, is a well-founded inversion of `vnat`/
-- `pair` and is left to B3.)
------------------------------------------------------------------------

module ProgramEnum where

open import Data.Nat using (ℕ; zero; suc)
open import Data.Nat.Properties using (suc-injective)
open import Data.Product using (_×_; _,_)
open import Relation.Binary.PropositionalEquality
  using (_≡_; refl; sym; trans; cong; cong₂)

open import UniversalInterp
  using (V; ⟨⟩; _∙_; Exp; var; knil; kcons; khd; ktl; kifnil; call; just)
open import ProgEncoding using (⌜_⌝; decode; decode-⌜⌝)
open import NatPairing using (pair; pair-inj)

------------------------------------------------------------------------
-- number the binary tree V.
------------------------------------------------------------------------

vnat : V → ℕ
vnat ⟨⟩      = zero
vnat (a ∙ b) = suc (pair (vnat a) (vnat b))

vnat-inj : ∀ u v → vnat u ≡ vnat v → u ≡ v
vnat-inj ⟨⟩      ⟨⟩      _  = refl
vnat-inj ⟨⟩      (c ∙ d) ()
vnat-inj (a ∙ b) ⟨⟩      ()
vnat-inj (a ∙ b) (c ∙ d) eq
  with pair-inj (vnat a) (vnat b) (vnat c) (vnat d) (suc-injective eq)
... | pa , pb = cong₂ _∙_ (vnat-inj a c pa) (vnat-inj b d pb)

------------------------------------------------------------------------
-- ⌜_⌝ is injective (decode is its left inverse), hence so is index.
------------------------------------------------------------------------

just-inj : ∀ {x y : Exp} → just x ≡ just y → x ≡ y
just-inj refl = refl

⌜⌝-inj : ∀ {p q} → ⌜ p ⌝ ≡ ⌜ q ⌝ → p ≡ q
⌜⌝-inj {p} {q} e =
  just-inj (trans (sym (decode-⌜⌝ p)) (trans (cong decode e) (decode-⌜⌝ q)))

index : Exp → ℕ
index e = vnat ⌜ e ⌝

index-injective : ∀ p q → index p ≡ index q → p ≡ q
index-injective p q eq = ⌜⌝-inj (vnat-inj ⌜ p ⌝ ⌜ q ⌝ eq)

------------------------------------------------------------------------
-- concrete: var is numbered 1 (⌜var⌝ = ⟨⟩ ∙ ⟨⟩ ⇒ suc (pair 0 0) = 1).
------------------------------------------------------------------------

_ : index var ≡ 1
_ = refl
