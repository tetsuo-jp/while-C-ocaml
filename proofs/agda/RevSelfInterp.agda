{-# OPTIONS --safe #-}

------------------------------------------------------------------------
-- Reversible Levin R3c (step 1): the reversible SELF-INTERPRETER, at the
-- semantic level -- the `SelfInterp`/`SelfInterpCorrect` twin.
--
-- `RevInst` (R3a/R3b) is a concrete, ENCODABLE reversible language `RCmd`
-- (swp/sq/ifn) with `⌜_⌝ : RCmd → V`.  But `RCmd` is a TERMINATING tree-rewriter
-- with no recursion primitive, so it cannot host its own self-interpreter
-- DIRECTLY: a self-interpreter must recurse over the unbounded structure of the
-- encoded program `⌜c⌝`.  (Classically, `SelfInterp.sint` gets that recursion from
-- the object language's `call`; the reversible language's own recursive control is
-- R1a'-guard / the eventual `rsint : RCmd`, still ahead.)
--
-- STEP 1 here: the self-interpretation ALGORITHM as a FUEL-INDEXED interpreter
-- `usint : ℕ → V → V → V` that dispatches on the encoded op TAG of `⌜c⌝` and
-- recurses on the sub-codes, together with the correctness theorem
--
--     usint-correct : size c ≤ n → usint n ⌜ c ⌝ s ≡ fwd c s
--
-- i.e. running the self-interpreter on the ENCODING of `c` reproduces `c`'s own
-- reversible semantics `RevInst.fwd`.  Because `fwd c` is reversible
-- (`RevInst.inv-sound`), self-interpretation is reversible FOR FREE
-- (`usint-reversible` below): interpreting `⌜ inv c ⌝` undoes interpreting `⌜ c ⌝`.
--
-- This is the correct-dispatch core that the eventual reversible self-interpreter
-- PROGRAM `rsint : RCmd` (R3c step 2) must realize, and the object whose steps R3d
-- counts to turn the MEASURED `a-rev ≈ 364×` into a THEOREM.
------------------------------------------------------------------------

module RevSelfInterp where

open import Data.Nat using (ℕ; zero; suc; _+_; _≤_; z≤n; s≤s)
open import Data.Nat.Properties using (≤-refl; ≤-trans; m≤m+n; m≤n+m)
open import Relation.Binary.PropositionalEquality using (_≡_; refl; trans; cong; cong₂)
open import UniversalInterp using (V; ⟨⟩; _∙_; ifnil)
open import RevInst
  using (RCmd; swp; sq; ifn; fwd; inv; bwd; swapTop; ⌜_⌝; inv-sound)

------------------------------------------------------------------------
-- code size: the structural fuel needed to self-interpret a program.
------------------------------------------------------------------------

size : RCmd → ℕ
size swp      = 1
size (sq c d)  = suc (size c + size d)
size (ifn c d) = suc (size c + size d)

------------------------------------------------------------------------
-- the fuel-indexed reversible self-interpreter.  It dispatches on the SAME op
-- tags `RevInst.decode` recognises (swp = ⟨⟩; sq = (⟨⟩∙⟨⟩)∙(·∙·);
-- ifn = ((⟨⟩∙⟨⟩)∙⟨⟩)∙(·∙·)) and recurses on the encoded sub-programs, mirroring
-- `RevInst.fwd` clause-for-clause.  Out of fuel / unrecognised code = identity.
------------------------------------------------------------------------

usint : ℕ → V → V → V
usint zero    code                          s = s
usint (suc n) ⟨⟩                            s = swapTop s
usint (suc n) ((⟨⟩ ∙ ⟨⟩) ∙ (c ∙ d))        s = usint n d (usint n c s)
usint (suc n) (((⟨⟩ ∙ ⟨⟩) ∙ ⟨⟩) ∙ (c ∙ d)) s = ifnil s (usint n c s) (usint n d s)
usint (suc n) _                             s = s

------------------------------------------------------------------------
-- R3c-step1 KEYSTONE: the self-interpreter reproduces `fwd` given enough fuel.
-- (Enough = the program's own size; more fuel is fine, monotone by the bound.)
------------------------------------------------------------------------

usint-correct : (c : RCmd) (s : V) (n : ℕ) → size c ≤ n → usint n ⌜ c ⌝ s ≡ fwd c s
usint-correct swp      s (suc n) _          = refl
usint-correct (sq c d) s (suc n) (s≤s le) =
  trans (cong (usint n ⌜ d ⌝)
              (usint-correct c s n (≤-trans (m≤m+n (size c) (size d)) le)))
        (usint-correct d (fwd c s) n (≤-trans (m≤n+m (size d) (size c)) le))
usint-correct (ifn c d) s (suc n) (s≤s le) =
  cong₂ (ifnil s)
        (usint-correct c s n (≤-trans (m≤m+n (size c) (size d)) le))
        (usint-correct d s n (≤-trans (m≤n+m (size d) (size c)) le))

-- the exact-fuel corollary (the headline self-interpretation identity).
usint-exact : (c : RCmd) (s : V) → usint (size c) ⌜ c ⌝ s ≡ fwd c s
usint-exact c s = usint-correct c s (size c) ≤-refl

------------------------------------------------------------------------
-- Reversibility of self-interpretation, FOR FREE: interpreting `⌜ inv c ⌝` on the
-- self-interpreted forward result recovers the store.  (usint ∘ ⌜ inv c ⌝ ∘
-- usint ∘ ⌜ c ⌝ = id, from usint-exact twice + `RevInst.inv-sound`.)
------------------------------------------------------------------------

usint-reversible : (c : RCmd) (s : V) →
  usint (size (inv c)) ⌜ inv c ⌝ (usint (size c) ⌜ c ⌝ s) ≡ s
usint-reversible c s =
  trans (cong (usint (size (inv c)) ⌜ inv c ⌝) (usint-exact c s))
        (trans (usint-exact (inv c) (fwd c s)) (inv-sound c s))

------------------------------------------------------------------------
-- Test-first: concrete machine-checked instances (all `refl`).
------------------------------------------------------------------------

module Examples where
  -- a store to run on:  s0 = ⟨⟩ ∙ (⟨⟩ ∙ ⟨⟩)   (non-nil, so `ifn` takes else)
  s0 : V
  s0 = ⟨⟩ ∙ (⟨⟩ ∙ ⟨⟩)

  -- swp: self-interpret the swap; the store's two subtrees are exchanged.
  ex-swp : usint (size swp) ⌜ swp ⌝ s0 ≡ (⟨⟩ ∙ ⟨⟩) ∙ ⟨⟩
  ex-swp = refl

  -- sq swp swp: swapping twice is the identity (self-interpreted).
  ex-sq-id : usint (size (sq swp swp)) ⌜ sq swp swp ⌝ s0 ≡ s0
  ex-sq-id = refl

  -- ifn swp swp on a non-nil store takes the else arm = one swap.
  ex-ifn-else : usint (size (ifn swp swp)) ⌜ ifn swp swp ⌝ s0 ≡ (⟨⟩ ∙ ⟨⟩) ∙ ⟨⟩
  ex-ifn-else = refl

  -- ifn swp sq… on a NIL store takes the then arm.
  ex-ifn-then : usint (size (ifn swp (sq swp swp))) ⌜ ifn swp (sq swp swp) ⌝ ⟨⟩ ≡ ⟨⟩
  ex-ifn-then = refl

  -- the self-interpretation identity holds on this instance…
  ex-correct : usint (size (sq swp swp)) ⌜ sq swp swp ⌝ s0 ≡ fwd (sq swp swp) s0
  ex-correct = refl

  -- …and reversibility: interpret ⌜ inv (sq swp swp) ⌝ to undo it.
  ex-reversible : usint (size (inv (sq swp swp))) ⌜ inv (sq swp swp) ⌝
                    (usint (size (sq swp swp)) ⌜ sq swp swp ⌝ s0) ≡ s0
  ex-reversible = refl
