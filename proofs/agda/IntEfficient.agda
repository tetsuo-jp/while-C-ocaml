{-# OPTIONS --safe #-}

------------------------------------------------------------------------
-- The self-interpreter has LINEAR overhead (Jones Thm 19.1.3, Levin B1.2 Phase 3).
--
-- By induction on the OBJECT derivation `p ⊢ e , env ⇓ v # c` (in `CostRelation.Rel
-- p`), the self-interpreter reproduces the same value with at most `a = 73` times
-- the steps: `sint , (⌜p⌝ . (⌜e⌝ . env)) ⇓ v # c'` with `c' ≤ 73 · c`.  Each case
-- feeds the IH sub-results to the matching Phase-2 per-node lemma (`SintCost`) and
-- discharges `K + Σ a·cᵢ ≤ a·(1 + Σ cᵢ)` via `K ≤ a = max K`.  The `call` case uses
-- the IH on the recursive sub-derivation `p ⊢ p , arg ⇓ …` -- interpreting the whole
-- program on the argument -- exactly what `sint-call`'s second premise needs.
--
-- No sub-run is ever reduced under a symbolic argument (the derivation-based route,
-- see docs/LEVIN-PLAN.md B1.2): this is the keystone `a` that Layer A's
-- `LevinOptimal.overhead` hypothesis ranges over.
------------------------------------------------------------------------

module IntEfficient where

open import Data.Nat using (ℕ; zero; suc; _+_; _*_; _≤_; _≤?_)
open import Data.Nat.Properties
  using (*-suc; *-distribˡ-+; +-mono-≤; +-monoʳ-≤; +-monoˡ-≤; ≤-trans)
open import Data.Product using (Σ; _×_; _,_)
open import Relation.Binary.PropositionalEquality using (_≡_; sym; cong; trans; subst)
open import Relation.Nullary.Decidable using (True; toWitness)
open import Data.Unit using (tt)

open import UniversalInterp using (V; ⟨⟩; _∙_; Exp; just)
open import ProgEncoding using (⌜_⌝)
open import SelfInterp using (sint)
open import CostRelation using (module Rel)
open import StepCount using (module StepCount)
open import SintCost

open Rel sint   -- the interpreter-side relation (matches the SintCost lemmas' types)
module SS = StepCount.StepCount sint    -- the interpreter-side step semantics

a : ℕ
a = 73          -- the linear-overhead constant (= max per-node dispatch cost)

-- `K ≤ 73` (and `≤ 73 * 1`) discharged by the decision procedure.
private
  le : ∀ {m n} → True (m ≤? n) → m ≤ n
  le = toWitness

-- node inequalities:  K + Σ subcost' ≤ a · (1 + Σ subcost),  given each subcost' ≤ a·subcost.
bound1 : ∀ K c {c'} → K ≤ a → c' ≤ a * c → K + c' ≤ a * suc c
bound1 K c {c'} K≤ c'≤ =
  subst (λ z → K + c' ≤ z) (sym (*-suc a c))
    (≤-trans (+-monoʳ-≤ K c'≤) (+-monoˡ-≤ (a * c) K≤))

bound2 : ∀ K ca cb {ca' cb'} → K ≤ a → ca' ≤ a * ca → cb' ≤ a * cb →
         K + (ca' + cb') ≤ a * suc (ca + cb)
bound2 K ca cb {ca'} {cb'} K≤ a≤ b≤ =
  subst (λ z → K + (ca' + cb') ≤ z)
    (sym (trans (*-suc a (ca + cb)) (cong (a +_) (*-distribˡ-+ a ca cb))))
    (≤-trans (+-monoʳ-≤ K (+-mono-≤ a≤ b≤)) (+-monoˡ-≤ (a * ca + a * cb) K≤))

------------------------------------------------------------------------
-- THE LINEAR-OVERHEAD THEOREM (relational form).
------------------------------------------------------------------------

module _ (p : Exp) where
  private module O  = Rel p                     -- the object-program relation
  private module SP = StepCount.StepCount p     -- the object-program step semantics

  int-efficient : ∀ {e env v c} → O._,_⇓_#_ e env v c →
    Σ ℕ (λ c' → (sint , (⌜ p ⌝ ∙ (⌜ e ⌝ ∙ env)) ⇓ v # c') × (c' ≤ a * c))
  int-efficient O.⇓var  = 8  , sint-var  ⌜ p ⌝ _ , le tt
  int-efficient O.⇓knil = 12 , sint-knil ⌜ p ⌝ _ , le tt
  int-efficient (O.⇓khd d) with int-efficient d
  ... | c' , sd , c'≤ = 39 + c' , sint-khd ⌜ p ⌝ _ _ sd , bound1 39 _ (le tt) c'≤
  int-efficient (O.⇓ktl d) with int-efficient d
  ... | c' , sd , c'≤ = 48 + c' , sint-ktl ⌜ p ⌝ _ _ sd , bound1 48 _ (le tt) c'≤
  int-efficient (O.⇓kcons {ca = ca} {cb = cb} da db) with int-efficient da | int-efficient db
  ... | ca' , sda , a≤ | cb' , sdb , b≤ =
        45 + (ca' + cb') , sint-kcons ⌜ p ⌝ _ _ _ sda sdb , bound2 45 ca cb (le tt) a≤ b≤
  int-efficient (O.⇓then {cg = cg} {ct = ct} dg dt) with int-efficient dg | int-efficient dt
  ... | cg' , sdg , g≤ | ct' , sdt , t≤ =
        73 + (cg' + ct') , sint-kifnil-then ⌜ p ⌝ _ _ _ _ sdg sdt , bound2 73 cg ct (le tt) g≤ t≤
  int-efficient (O.⇓else {cg = cg} {ce = ce} dg de) with int-efficient dg | int-efficient de
  ... | cg' , sdg , g≤ | ce' , sde , e≤ =
        73 + (cg' + ce') , sint-kifnil-else ⌜ p ⌝ _ _ _ _ sdg sde , bound2 73 cg ce (le tt) g≤ e≤
  int-efficient (O.⇓call {ca = ce} {cv = cp} de dp) with int-efficient de | int-efficient dp
  ... | ce' , sde , e≤ | cp' , sdp , p≤ =
        64 + (ce' + cp') , sint-call ⌜ p ⌝ _ _ sde sdp , bound2 64 ce cp (le tt) e≤ p≤

  -- Executable corollary (Thm 19.1.3): if `p` on `d` halts in `c` steps
  -- (`time_p(d) = c`), then the self-interpreter fed `⌜p⌝` halts on
  -- `(⌜p⌝ . (⌜p⌝ . d))` in `c' ≤ 73·c` steps -- linear overhead in the step model.
  int-efficient-time : ∀ {d v c} G → SP.evalT G p d ≡ just (v , c) →
    Σ ℕ (λ F → Σ ℕ (λ c' →
      (SS.evalT F sint (⌜ p ⌝ ∙ (⌜ p ⌝ ∙ d)) ≡ just (v , c')) × (c' ≤ a * c)))
  int-efficient-time {d = d} G h with int-efficient (O.complete G p d h)
  ... | c' , sd , c'≤ with sound sd
  ...   | F , eh = F , c' , eh , c'≤
