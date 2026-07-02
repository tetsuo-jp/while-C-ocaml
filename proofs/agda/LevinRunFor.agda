{-# OPTIONS --safe #-}

------------------------------------------------------------------------
-- B3b DISCHARGED: the concrete self-interpreter runner `runFor`, and the operational
-- Levin search over it (Levin plan Layer B3b, final step).
--
-- `runFor b p x` runs program `p` on input `x` via the self-interpreter `sint` (on the
-- triple `⌜p⌝ ∙ (⌜p⌝ ∙ x)`) with depth budget `b`.  This instantiates the abstract
-- `run` of `LevinDovetailRun`, and we prove the one remaining hypothesis `run-hits`:
--   if the solver `q` halts on `x` in `Tq` object-steps with output `yq`, then for every
--   budget `b ≥ 73·Tq`, `runFor b q x ≡ just yq`.
-- Proof: `IntEfficient.int-efficient-time` gives a `sint`-run of `≤ 73·Tq` steps; replay
-- it at fuel = its step count (`CostRelation.evalT-count`) and lift to `b` (`evalT-mono-≤`).
--
-- Composing with `LevinDovetailRun.finishing` and `LevinComplete.levin-operational` yields
-- `levin-optimal-run`: the operational dovetailing search halts at a stage `kop` whose
-- real (phase-charged) running time `gsum kop` is `< a_q·(Tq+Tr)` -- Levin optimality for
-- a concrete search, `a = 73` a theorem, no hypotheses left except the solver's own
-- halting fact.  (Modelling scope: cost model (A) charges `pow2 i` per stage; the checker
-- is the semantic `chk`; `opt` is this Agda driver -- see docs/LEVIN-B3-DESIGN.md.)
------------------------------------------------------------------------

module LevinRunFor where

open import Data.Nat using (ℕ; suc; _+_; _*_; _≤_; _<_)
open import Data.Nat.Properties using (≤-trans)
open import Data.Product using (Σ; _×_; _,_; proj₁; proj₂)
open import Data.Bool using (Bool; true)
open import Relation.Binary.PropositionalEquality using (_≡_; refl; sym; cong; subst)

open import UniversalInterp using (V; Exp; _∙_; Maybe; just; nothing)
open import ProgEncoding using (⌜_⌝)
open import SelfInterp using (sint)
open import StepCount using (module StepCount)
open import CostRelation using (module Rel)
import IntEfficient as IE
open import ProgramEnum using (index)
open import ProgramDecode using (enum; enum-index)
open import LevinBudget using (gsum)
open import LevinOptimal using (aq; S)
open import LevinGrounded using (search)
open import LevinDovetail using (module Search)
open import LevinDovetailRun using (module Oracle)
import LevinComplete
import Data.Maybe as DM

private
  module SS = StepCount.StepCount sint
  module RS = Rel sint

  mapVal : Maybe (V × ℕ) → Maybe V
  mapVal (just r) = just (proj₁ r)
  mapVal nothing  = nothing

-- run program p on x via sint with depth budget b; the output if it halts in budget.
runFor : ℕ → Exp → V → Maybe V
runFor b p x = mapVal (SS.evalT b sint (⌜ p ⌝ ∙ (⌜ p ⌝ ∙ x)))

------------------------------------------------------------------------
-- The solver `q` halts on `x` (`hq`), verified by `chk`; discharge everything.
------------------------------------------------------------------------

module Solver (q : Exp) (x : V) (yq : V) (Tq G Tr : ℕ)
              (hq     : StepCount.StepCount.evalT q G q x ≡ just (yq , Tq))
              (chk    : V → V → Bool) (chk-yq : chk x yq ≡ true)
              (1≤Tq   : 1 ≤ Tq) where

  -- THE run-hits obligation, discharged: budget ≥ 73·Tq reproduces q's output.
  run-hits : ∀ b → 73 * Tq ≤ b → runFor b q x ≡ just yq
  run-hits b 73Tq≤b with IE.int-efficient-time q G hq
  ... | F , c' , eF , c'≤73Tq =
        cong mapVal (RS.evalT-mono-≤ (≤-trans c'≤73Tq 73Tq≤b) sint _
                       (RS.evalT-count (RS.complete F sint _ eF)))

  -- same, phrased for `enum (index q) = q` (what the dovetail addresses).
  run-hits-enum : ∀ b → 73 * Tq ≤ b → runFor b (enum (index q)) x ≡ just yq
  run-hits-enum b h = subst (λ p → runFor b p x ≡ just yq) (sym (enum-index q)) (run-hits b h)

  open Oracle runFor chk x

  -- the concrete oracle finishes at some stage with the Levin bound (no hypotheses left).
  levin-finishes : Σ ℕ (λ k → (done k ≡ true)
                    × (gsum k < aq (search (index q) Tq Tr) * S (search (index q) Tq Tr)))
  levin-finishes = finishing (index q) Tq Tr yq chk-yq run-hits-enum 1≤Tq

  -- FULL end-to-end: the operational dovetail search halts at `kop`, its real
  -- (phase-charged) running time `gsum kop` is Levin-optimal.
  levin-optimal-run : Σ ℕ (λ kop → (Σ ℕ (λ f → Search.find done f 0 ≡ DM.just kop))
                       × (Search.time-opt done kop ≡ gsum kop)
                       × (gsum kop < aq (search (index q) Tq Tr) * S (search (index q) Tq Tr)))
  levin-optimal-run =
    LevinComplete.levin-operational done (index q) Tq Tr
      {proj₁ levin-finishes} (proj₂ (proj₂ levin-finishes)) (proj₁ (proj₂ levin-finishes))
