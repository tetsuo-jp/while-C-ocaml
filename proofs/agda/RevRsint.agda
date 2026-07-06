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

open import Data.Nat using (ℕ; zero; suc; _+_; _*_)
open import Data.Nat.Properties using (suc-injective; +-comm)
open import Data.List using (List; []; _∷_)
open import Data.Product using (_×_; _,_; Σ)
open import Relation.Binary.PropositionalEquality using (_≡_; refl; cong; trans; sym)
open import UniversalInterp using (V; ⟨⟩; _∙_; ifnil)
import RevInst as R
open R using (⌜_⌝)
open import RevAgenda
  using (Agenda; a-done; a-swp; a-sq; a-ifn-nil; a-ifn-cons; agenda-correct; asteps; asteps-agenda)
open import RevSintCost using (ocost)

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
-- FULL-RUN REVERSIBILITY: running forward `dsteps` steps then backward `dsteps`
-- steps recovers the EXACT initial state (Bennett: the `done` stack is consumed
-- back to empty).  The step count is the Agenda derivation's depth (each processed
-- code = one step); reversibility is per-step `astep-invol` lifted by induction.

dsteps : ∀ {todo s s'} → Agenda todo s s' → ℕ
dsteps a-done         = 0
dsteps (a-swp p)      = suc (dsteps p)
dsteps (a-sq p)       = suc (dsteps p)
dsteps (a-ifn-nil p)  = suc (dsteps p)
dsteps (a-ifn-cons p) = suc (dsteps p)

rsintRunB : ℕ → RState → RState
rsintRunB zero    st = st
rsintRunB (suc n) st = astepB (rsintRunB n st)

run-invol : ∀ {todo s s'} (d : Agenda todo s s') (done : List V)
  → rsintRunB (dsteps d) (rsintRun (dsteps d) (todo , done , s)) ≡ (todo , done , s)
run-invol a-done                done = refl
run-invol (a-swp {todo} {s} p)  done rewrite run-invol p (⟨⟩ ∷ done) =
  cong (λ z → ⟨⟩ ∷ todo , done , z) (R.swapTop-invol s)
run-invol (a-sq {c} {d} p)      done rewrite run-invol p (((⟨⟩ ∙ ⟨⟩) ∙ (c ∙ d)) ∷ done) = refl
run-invol (a-ifn-nil {c} {d} p) done rewrite run-invol p ((((⟨⟩ ∙ ⟨⟩) ∙ ⟨⟩) ∙ (c ∙ d)) ∷ done) = refl
run-invol (a-ifn-cons {c} {d} p) done rewrite run-invol p ((((⟨⟩ ∙ ⟨⟩) ∙ ⟨⟩) ∙ (c ∙ d)) ∷ done) = refl

-- rsint is REVERSIBLE as a whole program: forward then backward = identity, on
-- the actual self-interpretation of any `c` (starting/ending with empty `done`).
rsint-reversible : (c : R.RCmd) (s : V) →
  rsintRunB (dsteps (agenda-correct c s)) (rsintRun (dsteps (agenda-correct c s)) (⌜ c ⌝ ∷ [] , [] , s))
    ≡ (⌜ c ⌝ ∷ [] , [] , s)
rsint-reversible c s = run-invol (agenda-correct c s) []

------------------------------------------------------------------------
-- PROGRAM-LEVEL a-rev (Track-3 P4/P5): the rsint MACHINE runs in EXACTLY `ocost c s`
-- steps -- linear in the interpreted program's own runtime, and with the SAME count
-- `ocost` the fuel-level bound uses (RevSintCost).  Each executed object node is one
-- reversible machine step: the program-level overhead is a CONSTANT `a-revP` per
-- step (= 1 here; this minimal language has no variable-list walk, so unlike the
-- measured 364× there is no L-dependence).  This reconciles the cost models (P5):
--   fuel level  usintC n ⌜c⌝ s ≤ 4 · ocost c s   (RevSintCost.int-efficient-rev)
--   program level  #(rsint steps) = ocost c s     (below).

-- the machine step count is the Agenda-derivation depth = the executed-node count.
asteps≡suc-dsteps : ∀ {todo s s'} (d : Agenda todo s s') → asteps d ≡ suc (dsteps d)
asteps≡suc-dsteps a-done         = refl
asteps≡suc-dsteps (a-swp p)      = cong suc (asteps≡suc-dsteps p)
asteps≡suc-dsteps (a-sq p)       = cong suc (asteps≡suc-dsteps p)
asteps≡suc-dsteps (a-ifn-nil p)  = cong suc (asteps≡suc-dsteps p)
asteps≡suc-dsteps (a-ifn-cons p) = cong suc (asteps≡suc-dsteps p)

-- rsint's forward run takes EXACTLY `ocost c s` reversible machine steps.
rsint-steps : (c : R.RCmd) (s : V) → dsteps (agenda-correct c s) ≡ ocost c s
rsint-steps c s = suc-injective
  (trans (sym (asteps≡suc-dsteps (agenda-correct c s)))
         (trans (asteps-agenda c s) (+-comm (ocost c s) 1)))

-- charging each step a bounded per-step constant `a-revP`, the total is a-revP·ocost
-- -- program-level LINEAR overhead (the object-level twin of int-efficient-rev).
a-revP : ℕ
a-revP = 1

rsint-linear : (c : R.RCmd) (s : V)
  → a-revP * dsteps (agenda-correct c s) ≡ a-revP * ocost c s
rsint-linear c s = cong (a-revP *_) (rsint-steps c s)

------------------------------------------------------------------------
-- Test-first: a concrete program run + a concrete reversible step.

module Examples where
  prog : R.RCmd
  prog = R.sq R.swp (R.ifn R.swp R.swp)

  s0 : V
  s0 = ⟨⟩ ∙ (⟨⟩ ∙ ⟨⟩)

  ex-step-rev : astepB (astepF (⌜ prog ⌝ ∷ [] , [] , s0)) ≡ (⌜ prog ⌝ ∷ [] , [] , s0)
  ex-step-rev = astep-invol prog [] [] s0
