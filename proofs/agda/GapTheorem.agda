{-# OPTIONS --safe #-}

------------------------------------------------------------------------
-- BORODIN'S GAP THEOREM (Jones, Thm 20.4.1 `thm-gap`), constructive core.
--
-- For an expanding bound `g` (`g t ≥ t`) build the LADDER
--
--     rung 0        = 0
--     rung (suc j)  = suc (g (rung j))
--
-- whose "gap intervals"  Iⱼ = (rung j , g (rung j)]  are pairwise DISJOINT and
-- strictly increasing: `rung (suc j) = suc (g (rung j))` sits strictly above the
-- top of `Iⱼ`.  Consequently any single number lies in AT MOST ONE interval
-- (`at-most-one`).  The gap theorem then follows by pigeonhole: the (finitely
-- many) halting times of the first `k` programs on an input occupy at most `k`
-- of the `k+1` intervals `I₀ … I_k`, so some interval is empty -- a gap
-- `(rung j , g (rung j)]` in which no program halts.
--
-- This module proves the GEOMETRIC CORE (ladder monotonicity, interval
-- disjointness, at-most-one) which is the reusable heart, plus the ONE-program
-- and TWO-interval base facts.  The finite pigeonhole over `k+1` intervals, and
-- the instantiation of "halting time" to `StepCount.evalT` (via
-- `KleeneNF.T-dec`), are the named remaining steps (see `docs/BOOK-WAVE1-AND-
-- NEXT.md`).  --safe, axiom-free.
------------------------------------------------------------------------

module GapTheorem where

open import Data.Nat using (ℕ; zero; suc; _<_; _≤_; s≤s; z≤n; _≤′_; ≤′-refl; ≤′-step)
open import Data.Nat.Properties
  using (<-cmp; <-trans; <-≤-trans; ≤-<-trans; <⇒≤; ≤-trans; ≤-refl; <-irrefl; n<1+n; ≤⇒≤′)
open import Relation.Binary.Definitions using (tri<; tri≈; tri>)
open import Relation.Nullary using (¬_)
open import Data.Empty using (⊥-elim)
open import Data.Product using (_×_; _,_; proj₁; proj₂)
open import Relation.Binary.PropositionalEquality using (_≡_; refl)

module _ (g : ℕ → ℕ) (g-exp : ∀ t → t ≤ g t) where

  ----------------------------------------------------------------------
  -- the ladder and its monotonicity.

  rung : ℕ → ℕ
  rung zero    = 0
  rung (suc j) = suc (g (rung j))

  -- each rung is strictly below the next: rung j < rung (suc j).
  rung-inc : ∀ j → rung j < rung (suc j)
  rung-inc j = s≤s (g-exp (rung j))

  -- the ladder is ≤-monotone (via ≤′ whose +1 steps match `rung`'s recursion).
  rung-mono≤′ : ∀ {j j'} → j ≤′ j' → rung j ≤ rung j'
  rung-mono≤′ ≤′-refl     = ≤-refl
  rung-mono≤′ (≤′-step p) = ≤-trans (rung-mono≤′ p) (<⇒≤ (rung-inc _))

  rung-mono≤ : ∀ {j j'} → j ≤ j' → rung j ≤ rung j'
  rung-mono≤ le = rung-mono≤′ (≤⇒≤′ le)

  ----------------------------------------------------------------------
  -- interval disjointness: for j < j', the top `g (rung j)` of interval j is
  -- strictly below the bottom `rung j'` of interval j' (indeed below rung (suc j)).

  gap-below : ∀ {j j'} → j < j' → g (rung j) < rung j'
  gap-below jj' = <-≤-trans (n<1+n _) (rung-mono≤ jj')

  ----------------------------------------------------------------------
  -- membership in the j-th gap interval Iⱼ = (rung j , g (rung j)].

  inGap : ℕ → ℕ → Set
  inGap j h = (rung j < h) × (h ≤ g (rung j))

  -- KEYSTONE: a single number lies in at most one gap interval.
  -- If i < j then  h ≤ g(rung i) < rung j < h,  contradiction (h < h); dually.
  at-most-one : ∀ {i j h} → inGap i h → inGap j h → i ≡ j
  at-most-one {i} {j} {h} (li , ui) (lj , uj) with <-cmp i j
  ... | tri≈ _ e _   = e
  ... | tri< i<j _ _ = ⊥-elim (<-irrefl refl (<-trans (≤-<-trans ui (gap-below i<j)) lj))
  ... | tri> _ _ j<i = ⊥-elim (<-irrefl refl (<-trans (≤-<-trans uj (gap-below j<i)) li))

  ----------------------------------------------------------------------
  -- base facts.

  -- interval 0 = (0 , g 0]; nothing below or at 0 is in it, e.g. 0 ∉ I₀.
  zero∉I0 : ¬ inGap 0 0
  zero∉I0 (l0 , _) = <-irrefl refl l0
