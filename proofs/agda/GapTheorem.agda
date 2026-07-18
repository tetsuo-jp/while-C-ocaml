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
-- disjointness, `at-most-one`) AND the finite GAP THEOREM itself (`gap`): for
-- any list `hs` of halting times, some rung `j ≤ length hs` has an empty gap
-- interval, proved by the standard-library `pigeonhole` over the `length hs + 1`
-- rungs.  The one remaining step to Jones's exact statement is instantiating
-- "halting time" to `StepCount.evalT` (via `KleeneNF.T-dec`): collect the
-- (bounded, decidable) halting times of the first `k` programs on an input into
-- `hs`, then `gap` yields the time bound `t = rung j` with `TIME(t) = TIME(g∘t)`
-- for those programs (see `docs/BOOK-WAVE1-AND-NEXT.md`).  --safe, axiom-free.
------------------------------------------------------------------------

module GapTheorem where

open import Data.Nat using (ℕ; zero; suc; _<_; _≤_; s≤s; z≤n; _≤′_; ≤′-refl; ≤′-step)
open import Data.Nat.Properties
  using (<-cmp; <-trans; <-≤-trans; ≤-<-trans; <⇒≤; ≤-trans; ≤-refl; <-irrefl
        ; n<1+n; ≤⇒≤′; _<?_; _≤?_)
open import Data.Fin using (Fin; toℕ)
open import Data.Fin.Properties using (pigeonhole; ¬∀⇒∃¬)
open import Data.List using (List; length; lookup)
open import Data.List.Relation.Unary.Any using (Any; any?; index)
open import Data.List.Relation.Unary.Any.Properties using (lookup-index)
open import Data.List.Relation.Unary.All using (All)
open import Data.List.Relation.Unary.All.Properties using (¬Any⇒All¬)
open import Relation.Binary.Definitions using (tri<; tri≈; tri>)
open import Relation.Nullary using (¬_; Dec)
open import Relation.Nullary.Decidable.Core using (_×?_)
open import Data.Empty using (⊥-elim)
open import Data.Product using (Σ; ∃; _×_; _,_; proj₁; proj₂)
open import Relation.Binary.PropositionalEquality using (_≡_; refl; sym; subst)

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

  ----------------------------------------------------------------------
  -- THE GAP THEOREM (finite form), by pigeonhole.
  --
  -- Given the halting times `hs` of the first `length hs` programs on an input,
  -- some rung `j ≤ length hs` has an EMPTY gap interval: no halting time in `hs`
  -- lies in `(rung j , g (rung j)]`.  Proof: if every one of the `length hs + 1`
  -- rungs 0 … (length hs) were hit, mapping each rung to the list POSITION of a
  -- halting time in its interval gives `f : Fin (suc n) → Fin n`; by `pigeonhole`
  -- two distinct rungs share a position, i.e. one halting time lies in two
  -- intervals -- impossible by `at-most-one`.  So (decidability + `¬∀⇒∃¬`) some
  -- rung is unhit; `¬Any⇒All¬` turns "unhit" into "avoids every element".

  inGap? : ∀ c h → Dec (inGap c h)
  inGap? c h = (rung c <? h) ×? (h ≤? g (rung c))

  gap : (hs : List ℕ)
      → Σ (Fin (suc (length hs))) (λ j → All (λ h → ¬ inGap (toℕ j) h) hs)
  gap hs with ¬∀⇒∃¬ (suc (length hs))
                    (λ j → Any (λ h → inGap (toℕ j) h) hs)
                    (λ j → any? (λ h → inGap? (toℕ j) h) hs)
                    absurd
    where
      n : ℕ
      n = length hs

      absurd : ¬ ((j : Fin (suc n)) → Any (λ h → inGap (toℕ j) h) hs)
      absurd allhit with pigeonhole (n<1+n n) (λ j → index (allhit j))
      ... | i , j , i<j , fi≡fj = <-irrefl (at-most-one gi gj') i<j
        where
          gi : inGap (toℕ i) (lookup hs (index (allhit i)))
          gi = lookup-index (allhit i)
          gj : inGap (toℕ j) (lookup hs (index (allhit j)))
          gj = lookup-index (allhit j)
          gj' : inGap (toℕ j) (lookup hs (index (allhit i)))
          gj' = subst (λ p → inGap (toℕ j) (lookup hs p)) (sym fi≡fj) gj
  ... | j , ¬hit = j , ¬Any⇒All¬ hs ¬hit
