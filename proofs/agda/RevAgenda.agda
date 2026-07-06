{-# OPTIONS --safe #-}

------------------------------------------------------------------------
-- Reversible Levin Track-3 (brick P2): REVERSIBLE DEFUNCTIONALIZATION of the
-- self-interpreter -- the AGENDA (stack) machine, and its correctness
-- `agenda ≡ usint` (docs/REV-LEVIN-R3D-BRICKS.md).
--
-- `RevSelfInterp.usint` gets its recursion from fuel; to write it as an actual
-- LOOP program `rsint` (P3) that program preserves, the recursion must become an
-- explicit AGENDA: a stack `todo` of codes still to process.  Each step pops the
-- top code and either applies it (`swp`), expands it (`sq` pushes its two
-- sub-codes -- the stack discipline runs the left fully before the right, exactly
-- `usint d (usint c s)`), or picks a branch by the store (`ifn`).  This module
-- proves the agenda computes `usint`; the processed-code "done" stack (= the
-- Bennett garbage, R0b made concrete) is added for the reversible reification P3.
--
-- Correctness is FUEL-FREE: a big-step relation `Agenda todo s s'`, with the key
-- lemma `prepend` (pushing `⌜c⌝` = applying `fwd c`) by induction on the SOURCE
-- `c`.  --safe, axiom-free.
------------------------------------------------------------------------

module RevAgenda where

open import Data.Nat using (ℕ; zero; suc)
open import Data.List using (List; []; _∷_)
open import Data.Product using (Σ; _,_)
open import Relation.Binary.PropositionalEquality using (_≡_; refl; subst; sym)
open import UniversalInterp using (V; ⟨⟩; _∙_; ifnil)
open import RevSelfInterp using (usint; usint-exact; size)
import RevInst as R
open R using (⌜_⌝)

------------------------------------------------------------------------
-- The agenda machine as a big-step relation.  Tags match RevInst's encoding:
-- swp = ⟨⟩; sq = (⟨⟩∙⟨⟩)∙(c∙d); ifn = ((⟨⟩∙⟨⟩)∙⟨⟩)∙(c∙d).

data Agenda : List V → V → V → Set where
  a-done : ∀ {s} → Agenda [] s s
  a-swp  : ∀ {todo s s'} → Agenda todo (R.swapTop s) s'
         → Agenda (⟨⟩ ∷ todo) s s'
  a-sq   : ∀ {c d todo s s'} → Agenda (c ∷ d ∷ todo) s s'
         → Agenda (((⟨⟩ ∙ ⟨⟩) ∙ (c ∙ d)) ∷ todo) s s'
  a-ifn-nil  : ∀ {c d todo s'} → Agenda (c ∷ todo) ⟨⟩ s'
             → Agenda ((((⟨⟩ ∙ ⟨⟩) ∙ ⟨⟩) ∙ (c ∙ d)) ∷ todo) ⟨⟩ s'
  a-ifn-cons : ∀ {c d todo a b s'} → Agenda (d ∷ todo) (a ∙ b) s'
             → Agenda ((((⟨⟩ ∙ ⟨⟩) ∙ ⟨⟩) ∙ (c ∙ d)) ∷ todo) (a ∙ b) s'

------------------------------------------------------------------------
-- KEY LEMMA: pushing `⌜c⌝` onto the agenda applies `fwd c` to the store
-- (defunctionalization step correctness), by induction on the source `c`.

prepend : (c : R.RCmd) (todo : List V) (s s' : V)
        → Agenda todo (R.fwd c s) s' → Agenda (⌜ c ⌝ ∷ todo) s s'
prepend R.swp       todo s       s' h = a-swp h
prepend (R.sq c d)  todo s       s' h =
  a-sq (prepend c (⌜ d ⌝ ∷ todo) s s' (prepend d todo (R.fwd c s) s' h))
prepend (R.ifn c d) todo ⟨⟩      s' h = a-ifn-nil  (prepend c todo ⟨⟩ s' h)
prepend (R.ifn c d) todo (a ∙ b) s' h = a-ifn-cons (prepend d todo (a ∙ b) s' h)

------------------------------------------------------------------------
-- P2 THEOREM: the agenda on a single encoded program computes `usint` (= `fwd`).

agenda-correct : (c : R.RCmd) (s : V) → Agenda (⌜ c ⌝ ∷ []) s (R.fwd c s)
agenda-correct c s = prepend c [] s (R.fwd c s) a-done

agenda-usint : (c : R.RCmd) (s : V) → Agenda (⌜ c ⌝ ∷ []) s (usint (size c) ⌜ c ⌝ s)
agenda-usint c s = subst (Agenda (⌜ c ⌝ ∷ []) s) (sym (usint-exact c s)) (agenda-correct c s)

------------------------------------------------------------------------
-- the agenda machine is DETERMINISTIC (a function of todo and the store).

agenda-det : ∀ {todo s s1 s2} → Agenda todo s s1 → Agenda todo s s2 → s1 ≡ s2
agenda-det a-done         a-done         = refl
agenda-det (a-swp p)      (a-swp q)      = agenda-det p q
agenda-det (a-sq p)       (a-sq q)       = agenda-det p q
agenda-det (a-ifn-nil p)  (a-ifn-nil q)  = agenda-det p q
agenda-det (a-ifn-cons p) (a-ifn-cons q) = agenda-det p q

------------------------------------------------------------------------
-- The EXECUTABLE agenda runner: a single tail-recursive LOOP (fuel-indexed) --
-- the loop shape `rsint` (P3) reifies into the object language.  It coincides
-- with the relation (hence with `usint`), for enough fuel.

agendaRun : ℕ → List V → V → V
agendaRun zero    _                                       s = s
agendaRun (suc n) []                                      s = s
agendaRun (suc n) (⟨⟩ ∷ todo)                             s = agendaRun n todo (R.swapTop s)
agendaRun (suc n) (((⟨⟩ ∙ ⟨⟩) ∙ (c ∙ d)) ∷ todo)         s = agendaRun n (c ∷ d ∷ todo) s
agendaRun (suc n) ((((⟨⟩ ∙ ⟨⟩) ∙ ⟨⟩) ∙ (c ∙ d)) ∷ todo)  s =
  agendaRun n (ifnil s (c ∷ todo) (d ∷ todo)) s
agendaRun (suc n) (_ ∷ todo)                              s = agendaRun n todo s

-- soundness: the relation is realised by the runner at SOME fuel (∃ fuel,
-- accumulated up the derivation -- no explicit bound needed).
agenda-sound : ∀ {todo s s'} → Agenda todo s s' → Σ ℕ (λ n → agendaRun n todo s ≡ s')
agenda-sound a-done                     = suc zero , refl
agenda-sound (a-swp p)      with agenda-sound p
... | (n , eq) = suc n , eq
agenda-sound (a-sq p)       with agenda-sound p
... | (n , eq) = suc n , eq
agenda-sound (a-ifn-nil p)  with agenda-sound p
... | (n , eq) = suc n , eq
agenda-sound (a-ifn-cons p) with agenda-sound p
... | (n , eq) = suc n , eq

-- the loop-form self-interpreter computes `fwd c` (= usint) on a single program.
agendaRun-usint : (c : R.RCmd) (s : V) → Σ ℕ (λ n → agendaRun n (⌜ c ⌝ ∷ []) s ≡ R.fwd c s)
agendaRun-usint c s = agenda-sound (agenda-correct c s)

------------------------------------------------------------------------
-- Test-first: a concrete program's agenda run coincides with fwd/usint.

module Examples where
  prog : R.RCmd
  prog = R.sq R.swp (R.ifn R.swp R.swp)

  s0 : V
  s0 = ⟨⟩ ∙ (⟨⟩ ∙ ⟨⟩)

  ex : Agenda (⌜ prog ⌝ ∷ []) s0 (R.fwd prog s0)
  ex = agenda-correct prog s0

  ex-usint : Agenda (⌜ prog ⌝ ∷ []) s0 (usint (size prog) ⌜ prog ⌝ s0)
  ex-usint = agenda-usint prog s0
