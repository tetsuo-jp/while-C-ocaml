{-# OPTIONS --safe #-}

------------------------------------------------------------------------
-- Reversible Levin Track-2 (brick S2): the GENERIC linear-overhead lemma
-- (abstraction (i) of docs/REV-LEVIN-R3D-BRICKS.md).
--
-- `RevSintCost.int-efficient-rev` proved `usintC n ⌜c⌝ s ≤ 4 · ocost c s` with
-- fixed per-op dispatch charges 1/3/4.  Its arithmetic is EXTRACTED here once:
-- for ANY per-op charges `κ-swp/κ-sq/κ-ifn` bounded by a constant `K`, the
-- `usintC`-shaped cost `gcost` satisfies `gcost n ⌜c⌝ s ≤ K · ocost c s`.  Every
-- concrete overhead model (the meta-dispatch a-rev = 4 here; the store-walk
-- κ ≤ k+2L of Track 2; the program-level count of Track 3) instantiates it.
--
-- Reuse is DEMONSTRATED: `recover` re-derives `int-efficient-rev` from
-- `Generic.gcost-linear` at K=4, κ = 1/3/4 (since `usintC ≡ gcost 1 3 4`).
-- --safe, axiom-free.
------------------------------------------------------------------------

module RevLinearOverhead where

open import Data.Nat using (ℕ; zero; suc; _+_; _*_; _≤_; z≤n; s≤s)
open import Data.Nat.Properties
  using (≤-refl; ≤-trans; +-mono-≤; m≤m+n; m≤n+m)
open import Data.Nat.Solver using (module +-*-Solver)
open import Relation.Binary.PropositionalEquality using (_≡_; refl; sym; subst; cong; cong₂)
open import UniversalInterp using (V; ⟨⟩; _∙_; ifnil)
open import RevSelfInterp using (usint; usint-correct; size)
open import RevSintCost using (ocost; usintC)
import RevInst as R
open R using (⌜_⌝)
open +-*-Solver

------------------------------------------------------------------------
-- linear reshapes (discharged by the ring solver).

K*1 : ∀ K → K * 1 ≡ K
K*1 = solve 1 (λ K → K :* con 1 := K) refl

distrib3 : ∀ K x y → K * (1 + x + y) ≡ (K + K * x) + K * y
distrib3 = solve 3 (λ K x y → K :* (con 1 :+ x :+ y) := (K :+ K :* x) :+ K :* y) refl

distrib2 : ∀ K z → K * (1 + z) ≡ K + K * z
distrib2 = solve 2 (λ K z → K :* (con 1 :+ z) := K :+ K :* z) refl

------------------------------------------------------------------------
-- the three per-node inequality combinators (leaf / branch / binary node).

lin-leaf : ∀ {κ K} → κ ≤ K → κ ≤ K * 1
lin-leaf {κ} {K} h = ≤-trans h (subst (K ≤_) (sym (K*1 K)) ≤-refl)

lin-un : ∀ {κ t z K} → κ ≤ K → t ≤ K * z → κ + t ≤ K * (1 + z)
lin-un {κ} {t} {z} {K} hκ ht =
  subst (κ + t ≤_) (sym (distrib2 K z)) (+-mono-≤ hκ ht)

lin-bin : ∀ {κ a b x y K} → κ ≤ K → a ≤ K * x → b ≤ K * y → κ + a + b ≤ K * (1 + x + y)
lin-bin {κ} {a} {b} {x} {y} {K} hκ ha hb =
  subst (κ + a + b ≤_) (sym (distrib3 K x y)) (+-mono-≤ (+-mono-≤ hκ ha) hb)

------------------------------------------------------------------------
-- the generic linear-overhead theorem: any `usintC`-shaped cost whose per-op
-- charges are bounded by K has overhead ≤ K · ocost.

module Generic (K κ-swp κ-sq κ-ifn : ℕ)
               (hs : κ-swp ≤ K) (hq : κ-sq ≤ K) (hi : κ-ifn ≤ K) where

  gcost : ℕ → V → V → ℕ
  gcost zero    _                             _ = 0
  gcost (suc n) ⟨⟩                            s = κ-swp
  gcost (suc n) ((⟨⟩ ∙ ⟨⟩) ∙ (c ∙ d))        s = κ-sq + gcost n c s + gcost n d (usint n c s)
  gcost (suc n) (((⟨⟩ ∙ ⟨⟩) ∙ ⟨⟩) ∙ (c ∙ d)) s = κ-ifn + ifnil s (gcost n c s) (gcost n d s)
  gcost (suc n) _                             s = 0

  gcost-linear : (c : R.RCmd) (s : V) (n : ℕ) → size c ≤ n → gcost n ⌜ c ⌝ s ≤ K * ocost c s
  gcost-linear R.swp      s zero ()
  gcost-linear (R.sq c d)  s zero ()
  gcost-linear (R.ifn c d) s zero ()
  gcost-linear R.swp s (suc n) _ = lin-leaf hs
  gcost-linear (R.sq c d) s (suc n) (s≤s le)
    rewrite usint-correct c s n (≤-trans (m≤m+n (size c) (size d)) le)
    = lin-bin hq (gcost-linear c s n (≤-trans (m≤m+n (size c) (size d)) le))
                 (gcost-linear d (R.fwd c s) n (≤-trans (m≤n+m (size d) (size c)) le))
  gcost-linear (R.ifn c d) ⟨⟩ (suc n) (s≤s le) =
    lin-un hi (gcost-linear c ⟨⟩ n (≤-trans (m≤m+n (size c) (size d)) le))
  gcost-linear (R.ifn c d) (a ∙ b) (suc n) (s≤s le) =
    lin-un hi (gcost-linear d (a ∙ b) n (≤-trans (m≤n+m (size d) (size c)) le))

------------------------------------------------------------------------
-- REUSE demonstration: recover RevSintCost.int-efficient-rev at K=4, κ=1/3/4.
-- (usintC ≡ gcost 1 3 4 definitionally, so the generic theorem IS int-efficient-rev.)

module Inst = Generic 4 1 3 4 (s≤s z≤n) (s≤s (s≤s (s≤s z≤n))) ≤-refl

-- usintC is literally `gcost 1 3 4` (pointwise, by induction on the source).
usintC≡gcost : (c : R.RCmd) (s : V) (n : ℕ) → usintC n ⌜ c ⌝ s ≡ Inst.gcost n ⌜ c ⌝ s
usintC≡gcost R.swp      s zero    = refl
usintC≡gcost R.swp      s (suc n) = refl
usintC≡gcost (R.sq c d)  s zero    = refl
usintC≡gcost (R.sq c d)  s (suc n) =
  cong₂ (λ p q → 3 + p + q) (usintC≡gcost c s n) (usintC≡gcost d (usint n ⌜ c ⌝ s) n)
usintC≡gcost (R.ifn c d) s zero    = refl
usintC≡gcost (R.ifn c d) s (suc n) =
  cong (4 +_) (cong₂ (ifnil s) (usintC≡gcost c s n) (usintC≡gcost d s n))

recover : (c : R.RCmd) (s : V) (n : ℕ) → size c ≤ n → usintC n ⌜ c ⌝ s ≤ 4 * ocost c s
recover c s n h = subst (_≤ 4 * ocost c s) (sym (usintC≡gcost c s n)) (Inst.gcost-linear c s n h)
