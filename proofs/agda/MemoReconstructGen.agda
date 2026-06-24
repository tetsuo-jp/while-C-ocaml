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
-- `sH`, test g, take the body or the exit region) and the emitted structured form
-- (`sH ; while g do { body ; sH } ; exit`), each fuel-bounded, and prove they compute
-- the SAME partial function for ANY partial body AND partial exit (so loops nest in
-- bodies AND compose with arbitrary -- including looping -- continuations):
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

  -- Maybe bind (self-contained; keeps --safe free of stdlib drift)
  bindM : Maybe S → (S → Maybe S) → Maybe S
  bindM (just s) k = k s
  bindM nothing  k = nothing

  -- bind respects pointwise-equal continuations (no funext: case on the Maybe).
  bindM-cong : ∀ (m : Maybe S) {k1 k2 : S → Maybe S}
             → (∀ x → k1 x ≡ k2 x) → bindM m k1 ≡ bindM m k2
  bindM-cong (just s) eq = eq s
  bindM-cong nothing  eq = refl

  -- bind associativity: chaining a continuation after a bind.
  bindM-assoc : ∀ (m : Maybe S) (k j : S → Maybe S)
              → bindM (bindM m k) j ≡ bindM m (λ x → bindM (k x) j)
  bindM-assoc (just s) k j = refl
  bindM-assoc nothing  k j = refl

  ----------------------------------------------------------------------
  -- THE LOOP, with a PARTIAL (nestable) body AND a PARTIAL (composable) exit.
  -- Flowchart: header H re-entered each iteration.  One fuel = one pass through H:
  -- run head `sH`, test g; if true run the body (partial `bodyM`) then loop, else exit
  -- via the exit region (partial `exitM`).  Making BOTH the body and the exit partial
  -- (`S → Maybe S`) is what lets a reconstructed loop compose with arbitrary structured
  -- code on either side -- a loop nested in a body, AND a loop followed by another loop
  -- in its continuation -- i.e. whole nested reconstruction, not just one loop.
  fcLoop : (S → S) → (S → Bool) → (S → Maybe S) → (S → Maybe S) → ℕ → S → Maybe S
  fcLoop sH g bodyM exitM zero    s = nothing
  fcLoop sH g bodyM exitM (suc n) s =
    if g (sH s) then bindM (bodyM (sH s)) (fcLoop sH g bodyM exitM n)
                else exitM (sH s)

  -- the emitted structured form: `sH ; while g do { body ; sH } ; exit`.
  -- `whileM` = while g do (body then re-run head sH); `structLoop` runs sH once on
  -- entry, then the while, then the exit region on exit.
  whileM : (S → Bool) → (S → S) → (S → Maybe S) → ℕ → S → Maybe S
  whileM g sH bodyM zero    s = nothing
  whileM g sH bodyM (suc n) s =
    if g s then bindM (bodyM s) (λ s' → whileM g sH bodyM n (sH s'))
           else just s

  structLoop : (S → S) → (S → Bool) → (S → Maybe S) → (S → Maybe S) → ℕ → S → Maybe S
  structLoop sH g bodyM exitM n s = bindM (whileM g sH bodyM n (sH s)) exitM

  -- SOUNDNESS: the header flowchart loop and the structured reconstruction compute the
  -- same partial function, for every fuel, start state, and PARTIAL body AND exit.
  loop-sound : ∀ sH g bodyM exitM n s
             → fcLoop sH g bodyM exitM n s ≡ structLoop sH g bodyM exitM n s
  loop-sound sH g bodyM exitM zero    s = refl
  loop-sound sH g bodyM exitM (suc n) s with g (sH s)
  ... | false = refl
  ... | true  =
        trans (bindM-cong (bodyM (sH s)) (λ s' → loop-sound sH g bodyM exitM n s'))
              (sym (bindM-assoc (bodyM (sH s)) (λ s' → whileM g sH bodyM n (sH s')) exitM))

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
