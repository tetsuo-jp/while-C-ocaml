{-# OPTIONS --safe #-}
------------------------------------------------------------------------
-- M4 step (2): TERMINATION of the M3c homeomorphic-embedding whistle's generalization
-- (Cogen.ml `m3_specialize`).  Referenced by docs/ITEM3-DESIGN.md / AGDA_CORRESPONDENCE.
--
-- The whistle keeps the configuration set finite by GENERALIZING a growing config:
-- the slots where an ancestor's projection differs are dynamicized to `Dv`.  A
-- dynamicized slot is `'D` (bottom), so it can no longer grow / re-trigger the whistle
-- (`'C ⋬ 'D`).  Hence each slot is generalized AT MOST ONCE, and the number of
-- generalizations is bounded by the slot count -- there is no infinite generalization
-- regress.  (This is the implementation's actual termination argument: a monotone,
-- bounded measure.  It is weaker than -- and avoids -- a full Higman/Kruskal well-quasi-
-- order proof, modelling exactly the strategy `m3_specialize` uses: dynamicise the
-- growing slots, which are then terminal.)
--
-- Model: a config's per-slot generalization status is a `Vec Bool n` (true = already
-- dynamicized).  Dynamicization is MONOTONE (`_⊑_`: false→false/true, true→true, never
-- true→false).  A GENERALIZATION step (`Gen`) is monotone AND dynamicizes ≥1 slot.
-- We prove `trues` (the count of dynamic slots) STRICTLY INCREASES per `Gen` and is
-- BOUNDED by `n`; hence `gen-bound`: from any config you can generalize fewer than `n`
-- more times -- the whistle fires finitely often.
------------------------------------------------------------------------

module WhistleTerminates where

open import Data.Nat using (ℕ; zero; suc; _≤_; z≤n; s≤s; _<_)
open import Data.Nat.Properties using (m≤n⇒m≤1+n; ≤-trans)
open import Data.Bool using (Bool; true; false)
open import Data.Vec using (Vec; []; _∷_)

-- number of already-dynamicized (true) slots
trues : ∀ {n} → Vec Bool n → ℕ
trues []          = 0
trues (true  ∷ v) = suc (trues v)
trues (false ∷ v) = trues v

-- monotone dynamicization: once a slot is dynamic ('D, true) it stays dynamic.
data _⊑_ : ∀ {n} → Vec Bool n → Vec Bool n → Set where
  []  : [] ⊑ []
  ff  : ∀ {n}{v v' : Vec Bool n} → v ⊑ v' → (false ∷ v) ⊑ (false ∷ v')
  ft  : ∀ {n}{v v' : Vec Bool n} → v ⊑ v' → (false ∷ v) ⊑ (true  ∷ v')
  tt  : ∀ {n}{v v' : Vec Bool n} → v ⊑ v' → (true  ∷ v) ⊑ (true  ∷ v')

-- a GENERALIZATION step: monotone AND at least one slot newly dynamicized.
data Gen : ∀ {n} → Vec Bool n → Vec Bool n → Set where
  here  : ∀ {n}{v v' : Vec Bool n} → v ⊑ v' → Gen (false ∷ v) (true  ∷ v')
  skipF : ∀ {n}{v v' : Vec Bool n} → Gen v v' → Gen (false ∷ v) (false ∷ v')
  skipT : ∀ {n}{v v' : Vec Bool n} → Gen v v' → Gen (true  ∷ v) (true  ∷ v')

-- the dynamic-slot count is monotone along ⊑.
mono : ∀ {n}{v v' : Vec Bool n} → v ⊑ v' → trues v ≤ trues v'
mono []       = z≤n
mono (ff p)   = mono p
mono (ft p)   = m≤n⇒m≤1+n (mono p)
mono (tt p)   = s≤s (mono p)

-- a generalization STRICTLY increases the dynamic-slot count.
gen-strict : ∀ {n}{v v' : Vec Bool n} → Gen v v' → trues v < trues v'
gen-strict (here p)  = s≤s (mono p)
gen-strict (skipF g) = gen-strict g
gen-strict (skipT g) = s≤s (gen-strict g)

-- the dynamic-slot count is bounded by the slot count.
trues-bound : ∀ {n} (v : Vec Bool n) → trues v ≤ n
trues-bound []          = z≤n
trues-bound (true  ∷ v) = s≤s (trues-bound v)
trues-bound (false ∷ v) = m≤n⇒m≤1+n (trues-bound v)

-- TERMINATION CERTIFICATE: from a config v, any generalization leaves strictly fewer
-- than n further generalizations possible (trues v < trues v' ≤ n).  So the whistle's
-- generalization cannot regress infinitely -- the configuration set stays finite.
gen-bound : ∀ {n}{v v' : Vec Bool n} → Gen v v' → suc (trues v) ≤ n
gen-bound {v' = v'} g = ≤-trans (gen-strict g) (trues-bound v')
