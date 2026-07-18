{-# OPTIONS --safe #-}

------------------------------------------------------------------------
-- BORODIN'S GAP THEOREM for actual programs (Jones, Thm 20.4.1
-- `thm-gap`), the finite/per-input form fully assembled.
--
-- Fix an ambient program environment `prog`, an expanding, MONOTONE bound `g`
-- (`g t ≥ t`, `a ≤ b ⇒ g a ≤ g b`), a list of programs `ps`, and an input `x`.
-- Then some rung `j ≤ length ps` of the `GapTheorem` ladder is a GAP: NO program
-- in `ps` halts on `x` with a step count in `(rung j , g (rung j)]`
-- (`gap-prog`).
--
-- Assembly of the pieces built earlier:
--   * `GapTheorem.gap` (pigeonhole over the k+1 disjoint gap intervals);
--   * `BoundedTime.TimeLE?` (bounded-time halting is decidable) to `collect`
--     the halting times `≤ B` of `ps` (with `B = g (rung (length ps))`);
--   * `BoundedTime.evalT-det` (halting time is unique) for the membership lemma;
--   * monotonicity of `g` to bound `g (rung j) ≤ B` for `j ≤ length ps`, so a
--     program halting at time `> B` is automatically outside every interval.
-- --safe, axiom-free.  (`g` monotone is the standard Gap-theorem setting and
-- lets `B` be a single value instead of a max over the rungs.)
------------------------------------------------------------------------

module GapPrograms where

open import Data.Nat using (ℕ; zero; suc; _≤_; _<_; s≤s; z≤n)
open import Data.Nat.Properties using (≤-trans; n≤1+n; _≤?_)
open import Data.Fin using (Fin; toℕ)
open import Data.Fin.Properties using (toℕ≤pred[n])
open import Data.List using (List; []; _∷_; length)
open import Data.List.Membership.Propositional using (_∈_)
open import Data.List.Relation.Unary.Any using (here; there)
open import Data.List.Relation.Unary.All using (All) renaming (lookup to all-lookup)
open import Data.Product using (Σ; ∃; _×_; _,_; proj₁; proj₂)
open import Data.Empty using (⊥-elim)
open import Relation.Nullary using (¬_; yes; no)
open import Relation.Binary.PropositionalEquality using (_≡_; refl; sym; cong)
open import UniversalInterp using (V; Exp; just)
import StepCount as SC
import GapTheorem as GT
import BoundedTime as BT

module _ (prog : Exp) (g : ℕ → ℕ) (g-exp : ∀ t → t ≤ g t)
         (g-mono : ∀ {a b} → a ≤ b → g a ≤ g b) where

  open SC.StepCount prog using (evalT)

  private
    rung      = GT.rung g g-exp
    inGap     = GT.inGap g g-exp
    gap       = GT.gap g g-exp
    rung-mono≤ = GT.rung-mono≤ g g-exp
    TimeLE?   = BT.TimeLE? prog
    TimeLE    = BT.TimeLE prog
    evalT-det = BT.evalT-det prog

  ----------------------------------------------------------------------
  -- collect the halting times ≤ B of a program list (decidably, TimeLE?).

  collect : List Exp → V → ℕ → List ℕ
  collect []       x B = []
  collect (e ∷ es) x B with TimeLE? e x B
  ... | yes ((_ , t) , _) = t ∷ collect es x B
  ... | no  _             = collect es x B

  length-collect≤ : ∀ ps x B → length (collect ps x B) ≤ length ps
  length-collect≤ []       x B = z≤n
  length-collect≤ (e ∷ es) x B with TimeLE? e x B
  ... | yes _ = s≤s (length-collect≤ es x B)
  ... | no  _ = ≤-trans (length-collect≤ es x B) (n≤1+n (length es))

  -- membership: a program that halts on x with time t ≤ B has t collected.
  -- (`n` is explicit: `evalT` is not injective in its fuel, so it cannot be
  -- inferred from the run equation.)
  collect-∈ : ∀ ps x B {e v t} (n : ℕ) → e ∈ ps → evalT n e x ≡ just (v , t)
            → t ≤ B → t ∈ collect ps x B
  collect-∈ (e′ ∷ es) x B {e} {v} {t} n (here refl) hn t≤B with TimeLE? e′ x B
  ... | yes ((v₀ , t₀) , (n₀ , hn₀) , _) =
          here (sym (cong proj₂ (evalT-det {n = n₀} {m = n} {e = e′} {x = x} hn₀ hn)))
  ... | no ¬p = ⊥-elim (¬p ((v , t) , (n , hn) , t≤B))
  collect-∈ (e′ ∷ es) x B n (there e∈es) hn t≤B with TimeLE? e′ x B
  ... | yes _ = there (collect-∈ es x B n e∈es hn t≤B)
  ... | no  _ = collect-∈ es x B n e∈es hn t≤B

  ----------------------------------------------------------------------
  -- THE GAP THEOREM for programs.

  -- (`n`, the fuel witnessing the halt, is explicit -- `evalT` is not injective
  -- in its fuel so it is not inferable from the run equation.)
  gap-prog : (ps : List Exp) (x : V)
    → Σ ℕ (λ j → (j ≤ length ps)
        × (∀ {e v t} (n : ℕ) → e ∈ ps → evalT n e x ≡ just (v , t) → ¬ inGap j t))
  gap-prog ps x = toℕ jF , jbound , avoid
    where
      B : ℕ
      B = g (rung (length ps))

      G = gap (collect ps x B)
      jF = proj₁ G

      jbound : toℕ jF ≤ length ps
      jbound = ≤-trans (toℕ≤pred[n] jF) (length-collect≤ ps x B)

      avoid : ∀ {e v t} (n : ℕ) → e ∈ ps → evalT n e x ≡ just (v , t)
            → ¬ inGap (toℕ jF) t
      avoid {e} {v} {t} n e∈ps hn ing with t ≤? B
      ... | yes t≤B = all-lookup (proj₂ G)
                        (collect-∈ ps x B {e = e} {v = v} {t = t} n e∈ps hn t≤B) ing
      ... | no  t≰B = t≰B (≤-trans (proj₂ ing) (g-mono (rung-mono≤ jbound)))
