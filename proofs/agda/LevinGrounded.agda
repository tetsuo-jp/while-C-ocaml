{-# OPTIONS --safe #-}

------------------------------------------------------------------------
-- LEVIN OPTIMALITY, with the interpreter-overhead hypothesis DISCHARGED
-- (Jones Thm 20.1.2, Layer A ∘ Layer B1; the "Phase 4" linking step).
--
-- `LevinOptimal.levin-optimal` proved the optimality inequality
--        gsum k  <  a_q · (T_q + T_r),   a_q = 4 · a · 2^j,
-- leaving the linear self-interpreter overhead `a` (`LevinSearch.overhead`) as an
-- abstract field.  `IntEfficient.int-efficient` (Thm 19.1.3) proved that the I
-- self-interpreter `sint` runs one simulated step in ≤ `a = 73` real steps.  This
-- module *feeds that proven constant into Layer A*: every `LevinSearch` built by
-- `search` carries `overhead = IntEfficient.a = 73`, and `overhead-grounded`
-- witnesses that this exact constant is the machine-checked interpreter bound (not
-- an assumption).  So `levin-grounded` is Levin optimality with `a` a theorem.
--
-- Honest remaining gap (Layer B2b/B3): `idx j` still ranges over q's enumeration
-- index, and the concrete `opt` program with `time_opt = gsum k` is not yet built.
-- See docs/LEVIN-PLAN.md.
------------------------------------------------------------------------

module LevinGrounded where

open import Data.Nat using (ℕ; suc; _+_; _*_; _≤_; _<_; s≤s; z≤n)
open import Data.Product using (Σ; _×_; _,_)
open import Relation.Binary.PropositionalEquality using (_≡_; refl)

open import UniversalInterp using (V; _∙_; Exp)
open import ProgEncoding using (⌜_⌝)
open import SelfInterp using (sint)
open import CostRelation using (module Rel)
open import LevinBudget using (gsum)
open import LevinOptimal using (LevinSearch; levin-optimal; aq; c; S)
open import ProgramEnum using (index)
import IntEfficient as IE

------------------------------------------------------------------------
-- The linear-overhead property, as a proposition on the constant `ov`:
-- interpreting any object derivation `p ⊢ e,env ⇓ v # c` costs `sint` at most
-- `ov · c` steps.  `IntEfficient.int-efficient` is exactly an inhabitant at `ov = 73`.
------------------------------------------------------------------------

LinearOverhead : ℕ → Set
LinearOverhead ov = ∀ (p : Exp) {e env v c} →
  Rel._,_⇓_#_ p e env v c →
  Σ ℕ (λ c' → Rel._,_⇓_#_ sint sint (⌜ p ⌝ ∙ (⌜ e ⌝ ∙ env)) v c' × (c' ≤ ov * c))

-- The overhead constant Layer A ranges over is PROVEN, at value 73.
overhead-grounded : LinearOverhead IE.a
overhead-grounded = IE.int-efficient

------------------------------------------------------------------------
-- Feed the proven constant into Layer A's abstract search record.
------------------------------------------------------------------------

search : ℕ → ℕ → ℕ → LevinSearch
search j Tq Tr = record { overhead = IE.a ; idx = j ; time-q = Tq ; time-r = Tr }

-- The overhead field is literally the machine-checked interpreter bound a = 73.
overhead-is-a : ∀ j Tq Tr → LevinSearch.overhead (search j Tq Tr) ≡ IE.a
overhead-is-a _ _ _ = refl

_ : LevinSearch.overhead (search 0 0 0) ≡ 73
_ = refl

-- a_q = 4 · 73 · 2^j; at j = 0 that is 292 (whereas the abstract `ex` had a_q = 4).
_ : aq (search 0 3 1) ≡ 292
_ = refl

------------------------------------------------------------------------
-- LEVIN OPTIMALITY with `a` discharged.  For every solver of index `j` and times
-- `T_q, T_r` (positive target), the dovetail finishes in a phase of cumulative cost
-- < a_q·(T_q+T_r), a_q = 4·73·2^j -- the interpreter overhead is now a theorem.
------------------------------------------------------------------------

levin-grounded : ∀ j Tq Tr → 1 ≤ c (search j Tq Tr) * S (search j Tq Tr) →
  Σ ℕ (λ k → gsum k < aq (search j Tq Tr) * S (search j Tq Tr))
levin-grounded j Tq Tr = levin-optimal (search j Tq Tr)

-- Concrete: index 0, T_q = 3, T_r = 1  ⇒  c = 73, S = 4, target = 292, a_q·S = 1168.
_ : Σ ℕ (λ k → gsum k < 1168)
_ = levin-grounded 0 3 1 (s≤s z≤n)

------------------------------------------------------------------------
-- BOTH operational constants grounded.  Phase 4 grounded `overhead := 73` in
-- `int-efficient` (B1).  For a concrete solver `q`, its enumeration index is
-- `ProgramEnum.index q` (B2a, injective), so `search-solver` grounds BOTH `overhead`
-- and `idx` in proven constructs -- only `T_q, T_r` (q/r's actual running times) and
-- the operational `opt` (B3) remain to make the whole search concrete.
------------------------------------------------------------------------

search-solver : Exp → ℕ → ℕ → LevinSearch
search-solver q Tq Tr = search (index q) Tq Tr

_ : ∀ q Tq Tr → LevinSearch.overhead (search-solver q Tq Tr) ≡ 73
_ = λ q Tq Tr → refl

_ : ∀ q Tq Tr → LevinSearch.idx (search-solver q Tq Tr) ≡ index q
_ = λ q Tq Tr → refl

-- Levin optimality for a concrete solver program, both constants grounded.
levin-grounded-solver : ∀ q Tq Tr → 1 ≤ c (search-solver q Tq Tr) * S (search-solver q Tq Tr) →
  Σ ℕ (λ k → gsum k < aq (search-solver q Tq Tr) * S (search-solver q Tq Tr))
levin-grounded-solver q Tq Tr = levin-optimal (search-solver q Tq Tr)
