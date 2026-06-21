{-# OPTIONS --safe #-}
------------------------------------------------------------------------
-- Design spec for online-termination control (item 1) of cogen --online:
-- the BINDING-TIME WIDENING is a SOUND over-approximation and is
-- DEPTH-BOUNDED, so the BTA fixpoint always terminates.  Referenced by
-- docs/FUTAMURA.md.
--
-- Context.  The binding-time domain BT = S | D | C BT BT is the
-- partially-static lattice (S = static/bottom, D = dynamic/top, C = a
-- partially-static cons).  It has INFINITE ascending chains: a static-
-- control accumulator `Acc := cons D Acc` grows Acc's type
-- C(D, C(D, ...)) without bound, so the naive least-fixpoint never
-- converges (cogen hung on count.while).
--
-- The fix is `widen` (Cogen.ml): cap the C-nesting at a depth `d`
-- (collapsing deeper structure to D) and normalize C D D to D.  We model
-- the information order ⊑ (S least, D greatest, C pointwise) and prove:
--
--   * widen-sound  : t ⊑ widen d t     -- widening only moves UP the
--                    lattice, so the analysis stays a safe
--                    over-approximation (BTA soundness is preserved);
--   * depth-widen  : depth (widen d t) ≤ d  -- the widened domain is
--                    finite (bounded depth over {S,D} leaves), which is
--                    exactly what makes the ascending chain finite and
--                    the fixpoint terminate.
------------------------------------------------------------------------

module BTAWiden where

open import Data.Nat using (ℕ; zero; suc; _≤_; z≤n; s≤s; _⊔_)
open import Data.Nat.Properties using (⊔-mono-≤; ⊔-idem; ≤-trans; ≤-reflexive)
open import Relation.Binary.PropositionalEquality using (_≡_; refl)

-- The binding-time domain.
data BT : Set where
  S : BT                     -- static  (bottom)
  D : BT                     -- dynamic (top)
  C : BT → BT → BT           -- partially-static cons

-- Information order: S is least, D is greatest, C is pointwise.
data _⊑_ : BT → BT → Set where
  s⊑  : ∀ {t} → S ⊑ t
  ⊑d  : ∀ {t} → t ⊑ D
  c⊑c : ∀ {a b c e} → a ⊑ c → b ⊑ e → C a b ⊑ C c e

⊑-refl : ∀ t → t ⊑ t
⊑-refl S       = s⊑
⊑-refl D       = ⊑d
⊑-refl (C a b) = c⊑c (⊑-refl a) (⊑-refl b)

-- C D D normalizes to D; every other cons stays a C.
norm : BT → BT → BT
norm D D = D
norm x y = C x y

-- Widening: cap the C-nesting depth at d, collapsing deeper C to D, and
-- normalize C D D to D along the way.
widen : ℕ → BT → BT
widen _       S       = S
widen _       D       = D
widen zero    (C _ _) = D
widen (suc d) (C a b) = norm (widen d a) (widen d b)

-- structural depth (over {S,D} leaves)
depth : BT → ℕ
depth S       = 0
depth D       = 0
depth (C a b) = suc (depth a ⊔ depth b)

------------------------------------------------------------------------
-- Soundness: widening only moves up the lattice.
------------------------------------------------------------------------

-- C a b ⊑ norm a' b' whenever a ⊑ a' and b ⊑ b'.
norm-sound : ∀ {a b} (a' b' : BT) → a ⊑ a' → b ⊑ b' → C a b ⊑ norm a' b'
norm-sound D       D       pa pb = ⊑d
norm-sound D       S       pa pb = c⊑c pa pb
norm-sound D       (C _ _) pa pb = c⊑c pa pb
norm-sound S       b'      pa pb = c⊑c pa pb
norm-sound (C _ _) b'      pa pb = c⊑c pa pb

widen-sound : ∀ d t → t ⊑ widen d t
widen-sound d       S       = s⊑
widen-sound d       D       = ⊑d
widen-sound zero    (C a b) = ⊑d
widen-sound (suc d) (C a b) =
  norm-sound (widen d a) (widen d b) (widen-sound d a) (widen-sound d b)

------------------------------------------------------------------------
-- Termination: the widened domain is depth-bounded.
------------------------------------------------------------------------

-- least-upper-bound elimination for _⊔_, from monotonicity + idempotence
m⊔n≤o : ∀ {m n o} → m ≤ o → n ≤ o → m ⊔ n ≤ o
m⊔n≤o {o = o} p q = ≤-trans (⊔-mono-≤ p q) (≤-reflexive (⊔-idem o))

depth-norm : ∀ {d} (x y : BT) → depth x ≤ d → depth y ≤ d → depth (norm x y) ≤ suc d
depth-norm D       D       px py = z≤n
depth-norm D       S       px py = s≤s (m⊔n≤o px py)
depth-norm D       (C _ _) px py = s≤s (m⊔n≤o px py)
depth-norm S       y       px py = s≤s (m⊔n≤o px py)
depth-norm (C _ _) y       px py = s≤s (m⊔n≤o px py)

depth-widen : ∀ d t → depth (widen d t) ≤ d
depth-widen d       S       = z≤n
depth-widen d       D       = z≤n
depth-widen zero    (C a b) = z≤n
depth-widen (suc d) (C a b) =
  depth-norm (widen d a) (widen d b) (depth-widen d a) (depth-widen d b)
