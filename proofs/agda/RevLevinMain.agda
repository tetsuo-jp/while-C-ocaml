{-# OPTIONS --safe #-}

------------------------------------------------------------------------
-- CAPSTONE: the headline machine-checked results of the REVERSIBLE LEVIN R-track,
-- collected (and re-checked together) in one place for citation.  Importing this
-- module type-checks all the marquee theorems at once.  See
-- docs/REV-LEVIN-SKELETON.md and docs/REV-LEVIN-R3D-BRICKS.md for the full
-- module ↔ result map and the remaining (Track 2/3) fidelity upgrades.
--
--   rev-levin-thm        THE reversible Levin theorem (driver level): time-optimal at
--                        a PROVEN constant AND garbage |fibre| ≤ g ≤ |input|
--   rev-levin-grounded   time-optimality with the overhead constant a-rev a THEOREM
--   int-efficient-rev    a-rev linear-overhead of the reversible self-interpreter
--   usintC-sym           reversible time-symmetry of the self-interpreter (a-rev two-sided)
--   gen-linear-overhead  the GENERIC linear-overhead lemma (any per-op κ ≤ K)
--   sint-correct         the reversible self-interpreter reproduces `fwd` (usint-correct)
--   sint-reversible      self-interpreting the inverse undoes self-interpreting forward
--   rsint-computes       the PROGRAM-level reversible self-interpreter (the `rsint` machine,
--                        todo/done/store stacks) computes `fwd c` in its store (Track 3 P3c)
--   rsint-reversible     `rsint` reverses as a WHOLE program (forward then backward = id);
--                        the done-stack IS the Bennett garbage
--   op-inv-sound         RevInst reversible-op inversion soundness (bwd ∘ fwd = id)
--   op-cost-sym          RevInst reversible time-symmetry (backward run = forward cost)
--   lang-inv-sound       RCmdL (encodable loop language) inversion soundness
--   loop-inv-sound       RevLoop general data-dependent reversible loop inversion
--   op-decode / lang-decode / lang-decodeG   program⇄data round trips (encodable)
--   store-lookupC-bound / store-updateC-bound / store-update-length
--                        the 364× store-walk cost content (walk ≤ L; L invariant)
--
-- `--safe`, no postulates/holes.
------------------------------------------------------------------------

module RevLevinMain where

-- the reversible-op layer (R3a/R3b)
open import RevInst public
  using ()
  renaming (inv-sound to op-inv-sound; cost-sym to op-cost-sym; decode-⌜⌝ to op-decode)

-- the general reversible LOOP (R1a'-guard)
import RevLoop as RL
open RL.Core public using () renaming (inv-sound to loop-inv-sound)

-- the semantic self-interpreter (R3c step1)
open import RevSelfInterp public
  using ()
  renaming (usint-correct to sint-correct; usint-reversible to sint-reversible)

-- the encodable loop language + its encoding (R3c step2 bricks 1+2)
open import RCmdL public
  using ()
  renaming (inv-sound to lang-inv-sound; inv-complete to lang-inv-complete)
open import RCmdLEnc public
  using ()
  renaming (decode-⌜⌝ to lang-decode; decodeG-⌜⌝g to lang-decodeG)

-- the a-rev linear-overhead theorem + reversible time-symmetry (R3d)
open import RevSintCost public
  using (int-efficient-rev; usintC-sym; a-rev)

-- the generic linear-overhead lemma (Track 2, S2)
open import RevLinearOverhead public
  using (lin-leaf; lin-un; lin-bin)
  renaming (recover to gen-linear-overhead)

-- the store's walk-length cost (Track 2, S1 -- the 364× content)
open import RevStore public
  using ()
  renaming (lookupC-bound to store-lookupC-bound; updateC-bound to store-updateC-bound;
            update-length to store-update-length)

-- overhead grounded into Layer A (R3e, E1)
open import RevLevinGrounded public
  using (rev-levin-grounded; overhead-grounded)

-- THE reversible Levin theorem, assembled (R3e, E2)
open import RevLevin public
  using (ReversibleLevin)
  renaming (reversible-levin to rev-levin-thm)

-- the PROGRAM-level reversible self-interpreter as an actual reversible machine (R3c, P3)
open import RevRsint public
  using (rsint-computes; rsint-reversible)
