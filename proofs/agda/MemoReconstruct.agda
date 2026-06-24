{-# OPTIONS --safe #-}
------------------------------------------------------------------------
-- M4 (G1 for the MEMOIZED driver): soundness of the multi-block M3b structured
-- reconstruction (Cogen.ml `m3_reconstruct_general`).  Referenced by
-- docs/AGDA_CORRESPONDENCE.md and docs/ITEM3-DESIGN.md (M4).
--
-- The M3 global-memo driver produces a flowchart of labelled blocks; M3b folds a
-- single REDUCIBLE NATURAL LOOP -- header H, one back-edge from a latch, one exit:
--     H : sH ; if g then goto B else goto X
--     B : sB ; goto H                              (the back-edge)
--     X : sX ; halt
-- into the structured WHILE that `m3_reconstruct_general` emits:
--     sH ; while g do { sB ; sH } ; sX
-- (the head stmts sH re-run at the end of each iteration -- the `@ stmts` in the
-- emit; the while re-tests g, exactly as the flowchart re-enters H).
--
-- We model the flowchart's fuel-bounded run `fc` (one fuel = one pass through H)
-- and the structured form (`sH`, then the while `wh` whose body is sB-then-sH,
-- then `sX` on exit) over an abstract state S, guard g and block effects
-- sH/sB/sX, and prove they compute the SAME partial function:
--     sound : fc n s ≡ struct n s
-- So the emitted `while` faithfully implements the flowchart loop -- the loop-free
-- reconstruction is semantics-preserving.  Companion to `MemoFold` (the loop-FOLD
-- soundness: folded `while` ≡ source loop); together they cover the M3 driver's
-- two soundness obligations (fold a recurring config; reconstruct it to a while).
------------------------------------------------------------------------

module MemoReconstruct where

open import Data.Bool using (Bool; true; false; if_then_else_)
open import Data.Nat using (ℕ; zero; suc)
open import Data.Maybe using (Maybe; just; nothing)
open import Relation.Binary.PropositionalEquality using (_≡_; refl)

module Recon (S : Set) (g : S → Bool) (sH sB sX : S → S) where

  -- our own Maybe map (avoid stdlib version drift; keeps --safe self-contained)
  mapM : (S → S) → Maybe S → Maybe S
  mapM f (just s) = just (f s)
  mapM f nothing  = nothing

  -- FLOWCHART run from the loop header H, fuel-bounded (nothing = out of fuel).
  -- One fuel = one pass through H: run sH, test g; if true take the body (sB) and
  -- loop, else exit via X (sX).
  fc : ℕ → S → Maybe S
  fc zero    s = nothing
  fc (suc n) s = if g (sH s) then fc n (sB (sH s)) else just (sX (sH s))

  -- STRUCTURED reconstruction.  `wh` = `while g do (sH ∘ sB)` -- the body runs sB
  -- then re-runs the head sH; `struct` = run sH once (loop entry), then `wh`, then
  -- sX on exit.  This is exactly `sH ; while g { sB ; sH } ; sX`.
  wh : ℕ → S → Maybe S
  wh zero    s = nothing
  wh (suc n) s = if g s then wh n (sH (sB s)) else just s

  struct : ℕ → S → Maybe S
  struct n s = mapM sX (wh n (sH s))

  -- SOUNDNESS: the flowchart loop and the structured reconstruction compute the
  -- same partial function, for every fuel and start state.
  sound : ∀ n s → fc n s ≡ struct n s
  sound zero    s = refl
  sound (suc n) s with g (sH s)
  ... | true  = sound n (sB (sH s))
  ... | false = refl
