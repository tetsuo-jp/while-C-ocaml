{-# OPTIONS --safe #-}
------------------------------------------------------------------------
-- Design spec for item-3 M2: soundness of the configuration-memoizing FOLD
-- (Cogen.ml `drive`/`drive_dyn_if`, gated COGEN_MEMO).  Referenced by
-- docs/ITEM3-DESIGN.md, docs/ITEM3-M3-FINDINGS.md, docs/PAPER-SKELETON.md (C5).
--
-- Context.  The M2 driver specialises a subject loop by UNFOLDING the
-- self-interpreter's main loop, and when a configuration recurs (a back-edge =
-- the subject loop iterating) it FOLDS: it emits `while g do <body>` where
-- <body> is the specialisation of ONE subject-loop iteration, with the
-- interpreter's code pointer kept static so the dispatch inside it unrolls.
--
-- The fold is sound exactly when that residual body is SEMANTICS-PRESERVING per
-- iteration -- extensionally equal to the source step the loop would take.  Then
-- the residual loop computes the same function as the source loop.  We model a
-- loop as a guard g and a step over an abstract state, with a fuel-bounded run
-- (loops may not terminate), and prove:
--
--   * while-cong / fold-sound : if the residual body equals the source step on
--       every state, the folded `while` computes the same partial function as
--       the source loop (every fuel, every start state) -- M2 fold soundness;
--   * fold-unsound-witness : the contrast -- a concrete loop where the residual
--       body differs from the source step gives a DIFFERENT run, so the
--       per-iteration obligation is necessary.
--
-- This pins the M2 correctness kernel at the loop level; the per-iteration
-- "residual body == source step" obligation is the iteration-level congruence
-- supplied by spec/spec_exp (differential-tested; full G1 = the whole-spec
-- congruence, rwhile RWhileAVSound template).
------------------------------------------------------------------------

module MemoFold where

open import Data.Bool using (Bool; true; false)
open import Data.Nat using (ℕ; zero; suc)
open import Data.Maybe using (Maybe; just; nothing)
open import Data.Unit using (⊤; tt)
open import Relation.Binary.PropositionalEquality using (_≡_; refl)
open import Relation.Nullary using (¬_)

module Loop (S : Set) (g : S → Bool) where

  -- fuel-bounded execution of `while g do step` from a state.
  -- just s' = exited (g s' = false) with final state s'; nothing = out of fuel.
  run : (S → S) → ℕ → S → Maybe S
  run step zero    s = nothing
  run step (suc n) s with g s
  ... | false = just s
  ... | true  = run step n (step s)

  -- M2 FOLD SOUNDNESS.  `src` is the step the source subject loop takes; `body`
  -- is the residual one-iteration body the fold emits.  If they agree on every
  -- state, the folded `while g do body` computes the same function as the source
  -- `while g do src`.
  while-cong : (src body : S → S) → (∀ s → src s ≡ body s)
             → ∀ n s → run src n s ≡ run body n s
  while-cong src body eq zero    s = refl
  while-cong src body eq (suc n) s with g s
  ... | false = refl
  ... | true  rewrite eq s = while-cong src body eq n (body s)

  -- the same statement under M2's name.
  fold-sound : (src body : S → S) → (∀ s → src s ≡ body s)
             → ∀ n s → run src n s ≡ run body n s
  fold-sound = while-cong

  ----------------------------------------------------------------------
  -- LOOP-INVARIANT PRESERVATION (the engine for the command-level `'while`
  -- congruence, SpecCmdStore stage 3b).  If a step preserves an invariant `Inv`,
  -- a fuel-bounded run preserves it: the exit state (when there is one) satisfies
  -- Inv.  Instantiating S with the (source-store, residual-store) pair and Inv
  -- with store consistency gives: a loop whose body keeps the two stores
  -- consistent leaves them consistent on exit (guard agreement -- if-guard-agree
  -- -- justifies running both in lockstep under the shared guard g).
  MaybeInv : (S → Set) → Maybe S → Set
  MaybeInv Inv nothing  = ⊤
  MaybeInv Inv (just s) = Inv s

  run-inv : (Inv : S → Set) (step : S → S)
          → (∀ s → Inv s → Inv (step s))
          → ∀ n s → Inv s → MaybeInv Inv (run step n s)
  run-inv Inv step pres zero    s inv = tt
  run-inv Inv step pres (suc n) s inv with g s
  ... | false = inv
  ... | true  = run-inv Inv step pres n (step s) (pres s inv)

------------------------------------------------------------------------
-- Contrast: the per-iteration obligation is NECESSARY.  Concrete loop over Bool
-- with guard = identity (true loops, false exits).  Source step exits (→false);
-- a wrong residual body loops (→true).  With fuel 2 the runs already differ, so
-- a body that disagrees with the source step yields a different function.

guardId : Bool → Bool
guardId b = b

open Loop Bool guardId

srcStep : Bool → Bool
srcStep _ = false        -- source: one step then exit

badBody : Bool → Bool
badBody _ = true         -- wrong residual body: keeps looping

-- they disagree at the (reached) state `true`
bodies-differ : ¬ (srcStep true ≡ badBody true)
bodies-differ ()

-- and the 2-fuel runs differ: source exits with `just false`, bad body runs out.
fold-unsound-witness : ¬ (run srcStep 2 true ≡ run badBody 2 true)
fold-unsound-witness ()
