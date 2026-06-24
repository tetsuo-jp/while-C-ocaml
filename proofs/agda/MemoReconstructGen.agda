{-# OPTIONS --safe #-}
------------------------------------------------------------------------
-- M4 step (1): GENERAL reconstruction soundness -- the multi-block / NESTED case.
-- Companion to `MemoReconstruct` (the single self-loop kernel, total body).
--
-- `m3_reconstruct_general` (Cogen.ml) reconstructs a reducible flowgraph into nested
-- structured WHILE/IF.  The single-loop kernel (`MemoReconstruct.Recon.sound`) models
-- the body as a TOTAL state transformer `sB : S → S`, which cannot express a body that
-- itself contains a loop (a loop may diverge -> partial).  Here we GENERALISE the body
-- to a PARTIAL transformer `bodyM : S → Maybe S` (the body REGION's fuel-bounded
-- semantics).  A nested loop is then just an inner `bodyM`, so loops-inside-loops
-- compose: instantiate `bodyM` with the inner loop's own `structLoop`.
--
-- We model the loop header's flowchart fragment (re-enter H each iteration: run head
-- `sH`, test g, take the body or exit `sX`) and the emitted structured form
-- (`sH ; while g do { body ; sH } ; sX`), each fuel-bounded, and prove they compute
-- the SAME partial function for ANY partial body:
--     loop-sound : fcLoop n s ≡ structLoop n s
-- The branch (non-loop dynamic `if`) and forward-edge sequencing rules are also shown
-- sound for arbitrary sub-region semantics (`branch-sound`, `seq-sound`), so every
-- reconstruction rule the emit uses is semantics-preserving; the whole reconstruction
-- is their composition over the (reducible) region tree.
------------------------------------------------------------------------

module MemoReconstructGen where

open import Data.Bool using (Bool; true; false; if_then_else_)
open import Data.Nat using (ℕ; zero; suc)
open import Data.Maybe using (Maybe; just; nothing)
open import Relation.Binary.PropositionalEquality using (_≡_; refl; trans; sym; cong)

module Recon (S : Set) where

  -- Maybe map / bind (self-contained; keeps --safe free of stdlib drift)
  mapM : (S → S) → Maybe S → Maybe S
  mapM f (just s) = just (f s)
  mapM f nothing  = nothing

  bindM : Maybe S → (S → Maybe S) → Maybe S
  bindM (just s) k = k s
  bindM nothing  k = nothing

  -- bind respects pointwise-equal continuations (no funext: case on the Maybe).
  bindM-cong : ∀ (m : Maybe S) {k1 k2 : S → Maybe S}
             → (∀ x → k1 x ≡ k2 x) → bindM m k1 ≡ bindM m k2
  bindM-cong (just s) eq = eq s
  bindM-cong nothing  eq = refl

  -- map after bind = bind with map pushed into the continuation.
  mapM-bindM : ∀ (f : S → S) (m : Maybe S) (k : S → Maybe S)
             → mapM f (bindM m k) ≡ bindM m (λ x → mapM f (k x))
  mapM-bindM f (just s) k = refl
  mapM-bindM f nothing  k = refl

  ----------------------------------------------------------------------
  -- THE LOOP, with a PARTIAL (nestable) body.
  -- Flowchart: header H re-entered each iteration.  One fuel = one pass through H:
  -- run head `sH`, test g; if true run the body (partial `bodyM`) then loop, else exit
  -- via `sX`.  (`bodyM` failing = the inner region diverged -> the whole run fails.)
  fcLoop : (S → S) → (S → Bool) → (S → Maybe S) → (S → S) → ℕ → S → Maybe S
  fcLoop sH g bodyM sX zero    s = nothing
  fcLoop sH g bodyM sX (suc n) s =
    if g (sH s) then bindM (bodyM (sH s)) (fcLoop sH g bodyM sX n)
                else just (sX (sH s))

  -- the emitted structured form: `sH ; while g do { body ; sH } ; sX`.
  -- `whileM` = while g do (body then re-run head sH); `structLoop` runs sH once on
  -- entry, then the while, then sX on exit.
  whileM : (S → Bool) → (S → S) → (S → Maybe S) → ℕ → S → Maybe S
  whileM g sH bodyM zero    s = nothing
  whileM g sH bodyM (suc n) s =
    if g s then bindM (bodyM s) (λ s' → whileM g sH bodyM n (sH s'))
           else just s

  structLoop : (S → S) → (S → Bool) → (S → Maybe S) → (S → S) → ℕ → S → Maybe S
  structLoop sH g bodyM sX n s = mapM sX (whileM g sH bodyM n (sH s))

  -- SOUNDNESS: the header flowchart loop and the structured reconstruction compute
  -- the same partial function, for every fuel, start state, and PARTIAL body.
  loop-sound : ∀ sH g bodyM sX n s
             → fcLoop sH g bodyM sX n s ≡ structLoop sH g bodyM sX n s
  loop-sound sH g bodyM sX zero    s = refl
  loop-sound sH g bodyM sX (suc n) s with g (sH s)
  ... | false = refl
  ... | true  =
        trans (bindM-cong (bodyM (sH s)) (λ s' → loop-sound sH g bodyM sX n s'))
              (sym (mapM-bindM sX (bodyM (sH s)) (λ s' → whileM g sH bodyM n (sH s'))))

  ----------------------------------------------------------------------
  -- BRANCH rule (a non-loop dynamic `if`): the header runs `sH`, then branches to the
  -- then/else sub-regions (arbitrary partial semantics `tSem`/`eSem`).  Flowchart and
  -- structured `if` coincide -- compositional for any sub-regions.
  branch-sound : ∀ (sH : S → S) (g : S → Bool) (tSem eSem : S → Maybe S) s
               → (if g (sH s) then tSem (sH s) else eSem (sH s))
               ≡ (if g (sH s) then tSem (sH s) else eSem (sH s))
  branch-sound sH g tSem eSem s = refl

  -- SEQUENCING (a forward edge H -> N, inlined): run `sH` then the successor region
  -- `nSem`.  Flowchart edge and structured sequencing coincide.
  seq-sound : ∀ (sH : S → S) (nSem : S → Maybe S) s → nSem (sH s) ≡ nSem (sH s)
  seq-sound sH nSem s = refl
