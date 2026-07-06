{-# OPTIONS --safe #-}

------------------------------------------------------------------------
-- Reversible Levin Track-3 (brick P3c): `rsint` -- the reversible self-interpreter
-- as an actual REVERSIBLE MACHINE (docs/REV-LEVIN-R3D-BRICKS.md).
--
-- The loop-form self-interpreter `RevAgenda.agendaRun` is not reversible on its own
-- (it discards each processed code as `todo` shrinks).  Bennett's method makes it
-- reversible: keep a `done` stack of the processed codes.  The state is
-- `(todo , done , store)`; each FORWARD step moves the top code from `todo` to
-- `done` (applying it), which is INJECTIVE -- the `done` stack records exactly what
-- is needed to undo the step.  `done` is thus the Bennett GARBAGE (R0b made
-- concrete -- exactly why `ri.rwhile` outputs `(⌜p⌝ . result)`).
--
--   astep-invol    : astepB (astepF (⌜c⌝ ∷ todo , done , store)) ≡ (⌜c⌝ ∷ todo , …)
--                    -- every forward step is undone by the backward step: REVERSIBLE.
--   rsint-computes : proj-store (rsintRun n (⌜c⌝ ∷ [] , [] , s)) ≡ fwd c s
--                    -- the forward machine COMPUTES the self-interpreter (= usint),
--                    -- reusing RevAgenda's agenda relation.
--
-- So `rsint` is a reversible program computing the reversible self-interpreter, with
-- its garbage the processed-code stack.  --safe, axiom-free.
------------------------------------------------------------------------

module RevRsint where

open import Data.Nat using (ℕ; zero; suc)
open import Data.List using (List; []; _∷_)
open import Data.Product using (_×_; _,_; Σ)
open import Relation.Binary.PropositionalEquality using (_≡_; refl; cong; trans)
open import UniversalInterp using (V; ⟨⟩; _∙_; ifnil)
import RevInst as R
open R using (⌜_⌝)
open import RevAgenda
  using (Agenda; a-done; a-swp; a-sq; a-ifn-nil; a-ifn-cons; agenda-correct)

------------------------------------------------------------------------
-- the reversible machine state and its store projection.

RState : Set
RState = List V × List V × V           -- (todo , done , store)

proj-store : RState → V
proj-store (_ , _ , s) = s

------------------------------------------------------------------------
-- forward step: pop the top code, apply/expand/branch, PUSH it to `done`.
-- (Tags match RevInst's encoding, as in RevAgenda.)

astepF : RState → RState
astepF ([]                                       , done , store) = ([] , done , store)
astepF (⟨⟩ ∷ todo                                , done , store) = (todo , ⟨⟩ ∷ done , R.swapTop store)
astepF (((⟨⟩ ∙ ⟨⟩) ∙ (c ∙ d)) ∷ todo            , done , store) =
  (c ∷ d ∷ todo , ((⟨⟩ ∙ ⟨⟩) ∙ (c ∙ d)) ∷ done , store)
astepF ((((⟨⟩ ∙ ⟨⟩) ∙ ⟨⟩) ∙ (c ∙ d)) ∷ todo     , done , store) =
  (ifnil store (c ∷ todo) (d ∷ todo) , (((⟨⟩ ∙ ⟨⟩) ∙ ⟨⟩) ∙ (c ∙ d)) ∷ done , store)
astepF (code ∷ todo                              , done , store) = (todo , code ∷ done , store)

------------------------------------------------------------------------
-- backward step: pop the top code from `done`, un-apply, push back to `todo`.

-- (ifn undo -- pop 1 -- BEFORE sq undo -- pop 2 -- so the sq clause's 2-cons todo
-- pattern never blocks reduction of an ifn-tagged step.)
astepB : RState → RState
astepB (todo , []                                    , store) = (todo , [] , store)
astepB (todo , ⟨⟩ ∷ done                             , store) = (⟨⟩ ∷ todo , done , R.swapTop store)
astepB (b ∷ todo , (((⟨⟩ ∙ ⟨⟩) ∙ ⟨⟩) ∙ (c' ∙ d')) ∷ done , store) =
  ((((⟨⟩ ∙ ⟨⟩) ∙ ⟨⟩) ∙ (c' ∙ d')) ∷ todo , done , store)
astepB (c ∷ d ∷ todo , ((⟨⟩ ∙ ⟨⟩) ∙ (c' ∙ d')) ∷ done , store) =
  (((⟨⟩ ∙ ⟨⟩) ∙ (c' ∙ d')) ∷ todo , done , store)
astepB (todo , code ∷ done                           , store) = (code ∷ todo , done , store)

------------------------------------------------------------------------
-- REVERSIBILITY: every forward step (on an ENCODED code) is undone by the
-- backward step -- the machine is reversible, `done` recording just enough.

astep-invol : (c : R.RCmd) (todo done : List V) (store : V)
  → astepB (astepF (⌜ c ⌝ ∷ todo , done , store)) ≡ (⌜ c ⌝ ∷ todo , done , store)
astep-invol R.swp       todo done store =
  cong (λ z → ⟨⟩ ∷ todo , done , z) (R.swapTop-invol store)
astep-invol (R.sq c d)  todo done store       = refl
astep-invol (R.ifn c d) todo done ⟨⟩          = refl
astep-invol (R.ifn c d) todo done (a ∙ b)     = refl

------------------------------------------------------------------------
-- the forward machine (fuel-indexed loop) and its correctness: it computes the
-- self-interpreter's result in the store, reusing RevAgenda's agenda relation
-- (which ranges only over valid tags -- no junk to case on).

rsintRun : ℕ → RState → RState
rsintRun zero    st = st
rsintRun (suc n) st = rsintRun n (astepF st)

rsint-tracks : ∀ {todo s s'} → Agenda todo s s' → (done : List V)
             → Σ ℕ (λ n → proj-store (rsintRun n (todo , done , s)) ≡ s')
rsint-tracks a-done             done = zero , refl
rsint-tracks (a-swp p)          done with rsint-tracks p (⟨⟩ ∷ done)
... | (n , eq) = suc n , eq
rsint-tracks (a-sq {c} {d} p)   done with rsint-tracks p (((⟨⟩ ∙ ⟨⟩) ∙ (c ∙ d)) ∷ done)
... | (n , eq) = suc n , eq
rsint-tracks (a-ifn-nil {c} {d} p)  done with rsint-tracks p ((((⟨⟩ ∙ ⟨⟩) ∙ ⟨⟩) ∙ (c ∙ d)) ∷ done)
... | (n , eq) = suc n , eq
rsint-tracks (a-ifn-cons {c} {d} p) done with rsint-tracks p ((((⟨⟩ ∙ ⟨⟩) ∙ ⟨⟩) ∙ (c ∙ d)) ∷ done)
... | (n , eq) = suc n , eq

-- rsint COMPUTES the reversible self-interpreter (= usint = fwd c) in its store.
rsint-computes : (c : R.RCmd) (s : V)
  → Σ ℕ (λ n → proj-store (rsintRun n (⌜ c ⌝ ∷ [] , [] , s)) ≡ R.fwd c s)
rsint-computes c s = rsint-tracks (agenda-correct c s) []

------------------------------------------------------------------------
-- Test-first: a concrete program run + a concrete reversible step.

module Examples where
  prog : R.RCmd
  prog = R.sq R.swp (R.ifn R.swp R.swp)

  s0 : V
  s0 = ⟨⟩ ∙ (⟨⟩ ∙ ⟨⟩)

  ex-step-rev : astepB (astepF (⌜ prog ⌝ ∷ [] , [] , s0)) ≡ (⌜ prog ⌝ ∷ [] , [] , s0)
  ex-step-rev = astep-invol prog [] [] s0
