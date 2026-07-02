{-# OPTIONS --safe #-}

------------------------------------------------------------------------
-- The total surjective program decoder `enum : ℕ → Exp` (Levin plan Layer B2b).
--
-- `ProgramEnum.index : Exp → ℕ` is injective (B2a).  For the operational dovetail
-- (B3) Levin's search needs the OTHER direction: a total `enum : ℕ → Exp` that hits
-- every program, `enum (index p) ≡ p`.  We invert the three legs of
--     index = vnat ∘ ⌜_⌝ : Exp --⌜_⌝--> V --vnat--> ℕ:
--   * `NatPairing.unpair` inverts the Cantor pairing (structural, already proven);
--   * `vunnat` inverts `vnat : V → ℕ` -- a fuel-driven recursion (`unpair n` returns
--     numbers only BOUNDED by `n`, not structurally smaller, so we thread `n` as
--     fuel and prove `vnat v` steps of fuel always suffice, via `pair-fst-≤`/`-snd-≤`);
--   * `ProgEncoding.decode` inverts `⌜_⌝` (already proven, `decode-⌜⌝`).
-- Composing gives `enum` with `enum-index : ∀ p → enum (index p) ≡ p`, so `index` is
-- a section and `enum` a retraction -- programs are effectively enumerable.
------------------------------------------------------------------------

module ProgramDecode where

open import Data.Nat using (ℕ; zero; suc; _≤_; s≤s; z≤n)
open import Data.Nat.Properties using (≤-refl; ≤-trans)
open import Data.Product using (_×_; _,_)
open import Relation.Binary.PropositionalEquality using (_≡_; refl; cong; cong₂)

open import UniversalInterp using (V; ⟨⟩; _∙_; Exp; var; khd; ktl; Maybe; just; nothing)
open import ProgEncoding using (⌜_⌝; decode; decode-⌜⌝)
open import NatPairing using (pair; unpair; unpair-pair; pair-fst-≤; pair-snd-≤)
open import ProgramEnum using (vnat; index)

------------------------------------------------------------------------
-- invert `vnat : V → ℕ`.  Fuel = the number itself; `vnat v` steps always suffice.
------------------------------------------------------------------------

vunnat′ : ℕ → ℕ → V          -- fuel, value
vunnat′ zero    _       = ⟨⟩
vunnat′ (suc f) zero    = ⟨⟩
vunnat′ (suc f) (suc n) with unpair n
... | (na , nb) = vunnat′ f na ∙ vunnat′ f nb

vunnat : ℕ → V
vunnat n = vunnat′ n n

-- correctness: with at least `vnat v` fuel, `vunnat′` reconstructs `v`.
vunnat′-vnat : ∀ f v → vnat v ≤ f → vunnat′ f (vnat v) ≡ v
vunnat′-vnat zero    ⟨⟩      _        = refl
vunnat′-vnat (suc f) ⟨⟩      _        = refl
vunnat′-vnat (suc f) (a ∙ b) (s≤s le)
  rewrite unpair-pair (vnat a) (vnat b) =
    cong₂ _∙_ (vunnat′-vnat f a (≤-trans (pair-fst-≤ (vnat a) (vnat b)) le))
              (vunnat′-vnat f b (≤-trans (pair-snd-≤ (vnat a) (vnat b)) le))

vunnat-vnat : ∀ v → vunnat (vnat v) ≡ v
vunnat-vnat v = vunnat′-vnat (vnat v) v ≤-refl

------------------------------------------------------------------------
-- the total surjective decoder.
------------------------------------------------------------------------

enum : ℕ → Exp
enum n with decode (vunnat n)
... | just e  = e
... | nothing = var          -- default for numbers outside the range of `index`

enum-index : ∀ p → enum (index p) ≡ p
enum-index p rewrite vunnat-vnat ⌜ p ⌝ | decode-⌜⌝ p = refl

------------------------------------------------------------------------
-- computational checks.
------------------------------------------------------------------------

_ : enum (index var) ≡ var
_ = refl

_ : vunnat (vnat (⟨⟩ ∙ ⟨⟩)) ≡ (⟨⟩ ∙ ⟨⟩)
_ = refl

-- a compound program round-trips too.
check : Exp
check = khd (ktl var)

_ : enum (index check) ≡ check
_ = refl
