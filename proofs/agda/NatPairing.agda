{-# OPTIONS --safe #-}

------------------------------------------------------------------------
-- An injective pairing ℕ × ℕ ↪ ℕ (Cantor), for Gödel-numbering programs.
--
-- `pair x y = tri (x + y) + x` with `tri n = 0+1+⋯+n`.  We prove it INJECTIVE
-- (`pair-inj`) -- enough to number a countable inductive type.  The diagonal
-- `x + y` is recovered first (`diag-eq`): a pair value lies in `[tri d, tri (d+1))`,
-- so the strictly-increasing `tri` pins the diagonal `d`; then `+`-cancellation
-- pins `x`, and the diagonal pins `y`.
--
-- Used by `ProgramEnum` to give every `Exp` program a unique natural-number index
-- (Levin plan Layer B2, `docs/LEVIN-PLAN.md`).
------------------------------------------------------------------------

module NatPairing where

open import Data.Nat using (ℕ; zero; suc; _+_; _≤_; _<_; s≤s; z≤n)
open import Data.Nat.Properties
  using (+-comm; +-mono-≤; +-monoʳ-<; +-cancelˡ-≡; <-cmp; <⇒≢; ≤-trans;
         <-≤-trans; m≤m+n)
open import Data.Product using (_×_; _,_)
open import Data.Empty using (⊥-elim)
open import Relation.Binary.Definitions using (tri<; tri≈; tri>)
open import Relation.Binary.PropositionalEquality
  using (_≡_; refl; sym; trans; subst; cong)

-- triangular numbers  tri n = 0 + 1 + ⋯ + n
tri : ℕ → ℕ
tri zero    = zero
tri (suc n) = suc n + tri n

-- Cantor pairing
pair : ℕ → ℕ → ℕ
pair x y = tri (x + y) + x

------------------------------------------------------------------------
-- monotonicity of tri, and the interval bound  tri d + a < tri (suc d)  for a ≤ d.
------------------------------------------------------------------------

tri-mono-≤ : ∀ {m n} → m ≤ n → tri m ≤ tri n
tri-mono-≤ z≤n       = z≤n
tri-mono-≤ (s≤s m≤n) = +-mono-≤ (s≤s m≤n) (tri-mono-≤ m≤n)

tri-bound : ∀ {a d} → a ≤ d → tri d + a < tri (suc d)
tri-bound {a} {d} a≤d =
  subst (λ z → tri d + a < z) (+-comm (tri d) (suc d))
        (+-monoʳ-< (tri d) (s≤s a≤d))

------------------------------------------------------------------------
-- the diagonal is determined by the pair value.
------------------------------------------------------------------------

diag-eq : ∀ {d e a c} → a ≤ d → c ≤ e → tri d + a ≡ tri e + c → d ≡ e
diag-eq {d} {e} {a} {c} a≤d c≤e eq with <-cmp d e
... | tri≈ _ d≡e _ = d≡e
... | tri< d<e _ _ =
      ⊥-elim (<⇒≢ (<-≤-trans (tri-bound a≤d)
                             (≤-trans (tri-mono-≤ d<e) (m≤m+n (tri e) c))) eq)
... | tri> _ _ e<d =
      ⊥-elim (<⇒≢ (<-≤-trans (tri-bound c≤e)
                             (≤-trans (tri-mono-≤ e<d) (m≤m+n (tri d) a)))
                  (sym eq))

------------------------------------------------------------------------
-- injectivity.
------------------------------------------------------------------------

pair-inj : ∀ a b c d → pair a b ≡ pair c d → (a ≡ c) × (b ≡ d)
pair-inj a b c d eq = a≡c , b≡d
  where
    dd≡ee : a + b ≡ c + d
    dd≡ee = diag-eq (m≤m+n a b) (m≤m+n c d) eq

    eq' : tri (a + b) + a ≡ tri (a + b) + c
    eq' = trans eq (cong (λ z → tri z + c) (sym dd≡ee))

    a≡c : a ≡ c
    a≡c = +-cancelˡ-≡ (tri (a + b)) a c eq'

    b≡d : b ≡ d
    b≡d = +-cancelˡ-≡ a b d (trans dd≡ee (cong (_+ d) (sym a≡c)))
