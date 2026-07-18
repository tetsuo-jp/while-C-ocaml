{-# OPTIONS --safe #-}

------------------------------------------------------------------------
-- BOUNDED-TIME HALTING IS DECIDABLE (the Blum-measure / Gap-theorem handle).
--
-- Using `EvalTFuelTime.fuel-time` (the reported step count is sufficient fuel),
-- the predicate "program `e` halts on input `x` with (true) time `≤ B`" is
-- DECIDABLE, by a single bounded run `evalT B e x`:
--
--   * `evalT B e x = just (v , t')` with `t' ≤ B`  ⇒  yes (time `t' ≤ B`);
--   * `evalT B e x = just (v , t')` with `t' > B`  ⇒  no  (the true time IS `t'`
--       by determinism, and `t' > B`);
--   * `evalT B e x = nothing`                      ⇒  no  (any run of time `≤ B`
--       would already have halted `evalT B`, by `time≤-fuel`).
--
-- Determinism of the reported result (`evalT-det`) comes from fuel-monotonicity
-- (lift both runs to a common fuel).  This `TimeLE?` is exactly the decidable
-- ingredient the constructive Gap theorem needs to collect the halting times
-- `≤ B` of a program list into `GapTheorem.gap`'s input.  --safe, axiom-free.
------------------------------------------------------------------------

module BoundedTime where

open import Data.Nat using (ℕ; suc; _+_; _≤_; _<_)
open import Data.Nat.Properties using (m≤m+n; m≤n+m; _≤?_)
open import Data.Product using (Σ; ∃; _×_; _,_; proj₁; proj₂)
open import Relation.Nullary using (Dec; yes; no; ¬_)
open import Relation.Binary.PropositionalEquality
  using (_≡_; refl; sym; trans; cong; subst)
open import UniversalInterp
  using (V; Exp; Maybe; just; nothing; nothing≢just)
import StepCount as SC
import EvalTFuelTime as FT

module _ (prog : Exp) where

  open SC.StepCount prog using (evalT)

  private
    evalT-le  = FT.evalT-le  prog
    time≤-fuel = FT.time≤-fuel prog

    just-inj : ∀ {r r' : V × ℕ} → just r ≡ just r' → r ≡ r'
    just-inj refl = refl

  ----------------------------------------------------------------------
  -- determinism: the reported (value, step-count) is fuel-independent.

  evalT-det : ∀ {n m e x r r'} → evalT n e x ≡ just r → evalT m e x ≡ just r'
            → r ≡ r'
  evalT-det {n} {m} hn hm =
    just-inj (trans (sym (evalT-le (m≤m+n n m) hn)) (evalT-le (m≤n+m m n) hm))

  ----------------------------------------------------------------------
  -- "e halts on x with (true) time ≤ B", and its decidability.

  Runs : Exp → V → V × ℕ → Set
  Runs e x r = ∃ (λ n → evalT n e x ≡ just r)

  TimeLE : Exp → V → ℕ → Set
  TimeLE e x B = ∃ (λ r → Runs e x r × (proj₂ r ≤ B))

  TimeLE? : ∀ e x B → Dec (TimeLE e x B)
  TimeLE? e x B with evalT B e x in eq
  ... | just (v , t′) with t′ ≤? B
  ...   | yes t′≤B = yes ((v , t′) , (B , eq) , t′≤B)
  ...   | no  t′≰B = no ¬p
    where
      ¬p : ¬ TimeLE e x B
      ¬p ((v″ , t″) , (n , hn) , t″≤B) =
        t′≰B (subst (_≤ B)
              (sym (cong proj₂ (evalT-det {n = B} {m = n} {e = e} {x = x} eq hn)))
              t″≤B)
  TimeLE? e x B | nothing = no ¬p
    where
      ¬p : ¬ TimeLE e x B
      ¬p ((v″ , t″) , (n , hn) , t″≤B) =
        nothing≢just (trans (sym eq) (time≤-fuel {B = B} n e x hn t″≤B))
