{-# OPTIONS --safe #-}

------------------------------------------------------------------------
-- Reversible Levin Track-2 (brick S4, interpreter side; traversal-charge
-- plan task 3): the STORE-CARRYING self-interpreter -- the
-- `RevSelfInterp.usint` twin over the multi-variable language `RCmdV`.
--
-- `usintV : ℕ → V → Store → Store` is the fuel-indexed interpreter that
-- dispatches on the encoded op tag of `⌜ c ⌝V` (RCmdVEnc), reads variable
-- operands with the lenient unary walker `unℕ` (the interpreter-side
-- reader of the index-decode convention -- see RCmdVEnc's header), and
-- performs the ACTUAL store access of each op (`mapAt`/`lookup` on the
-- association-list store of RevStore).  Out of fuel / unrecognised code =
-- identity, as in `usint`.  Correctness:
--
--     usintV-correct : sizeV c ≤ n → usintV n ⌜ c ⌝V s ≡ fwdV c s
--
-- self-interpretation reproduces `RCmdV.fwdV`, so it is reversible FOR
-- FREE (`usintV-reversible`, via `inv-soundV`) and NEVER changes the store
-- length on encoded programs (`usintV-length`, via
-- `RCmdVWalkCost.fwdV-length` -- "the" L of a probe is well-defined even
-- under self-interpretation).  This is the object whose OWN step count
-- plan task 4 (`RevStoreSintCost.usintVC`) charges dispatch + index-decode
-- + walk, targeting `usintVC n ⌜c⌝V s ≤ (base + 2L) · ocostV c s` -- the
-- interpreter-level a-rev(L) with the walk charged, discharging the
-- assumption-based envelope of `RevStoreOverhead.AffineInL`.
-- --safe, axiom-free.
------------------------------------------------------------------------

module RevStoreInterp where

open import Data.Nat using (ℕ; zero; suc; _+_; _≤_; z≤n; s≤s)
open import Data.Nat.Properties using (≤-refl; ≤-trans; m≤m+n; m≤n+m)
open import Data.List using (length; []; _∷_)
open import Data.Product using (_,_)
open import Relation.Binary.PropositionalEquality
  using (_≡_; refl; trans; cong; cong₂)
open import UniversalInterp using (V; ⟨⟩; _∙_; ifnil)
open import RevStore using (Store; lookup; mapAt)
open import RCmdV using (RCmdV; swpV; sqV; ifnV; fwdV; invV; inv-soundV)
open import RCmdVWalkCost using (fwdV-length)
open import RCmdVEnc using (⌜_⌝V; ⌜_⌝ℕ; unℕ; unℕ-⌜⌝ℕ)
import RCmdV
import RevInst as R

------------------------------------------------------------------------
-- code size: the structural fuel needed to self-interpret a program
-- (the `RevSelfInterp.size` twin; operand numerals cost no fuel -- their
-- walk is a COST, charged in task 4, not a recursion depth).

sizeV : RCmdV → ℕ
sizeV (swpV k)     = 1
sizeV (sqV c d)    = suc (sizeV c + sizeV d)
sizeV (ifnV k c d) = suc (sizeV c + sizeV d)

------------------------------------------------------------------------
-- the fuel-indexed store-carrying self-interpreter.  Tags as in
-- `RCmdVEnc.decodeV` (swpV = ⟨⟩ ∙ k; sqV = (⟨⟩∙⟨⟩) ∙ (c ∙ d);
-- ifnV = ((⟨⟩∙⟨⟩)∙⟨⟩) ∙ (k ∙ (c ∙ d))), mirroring `RCmdV.fwdV`
-- clause-for-clause on the store.

usintV : ℕ → V → Store → Store
usintV zero    code                                 s = s
usintV (suc n) (⟨⟩ ∙ k)                             s = mapAt R.swapTop (unℕ k) s
usintV (suc n) ((⟨⟩ ∙ ⟨⟩) ∙ (c ∙ d))               s = usintV n d (usintV n c s)
usintV (suc n) (((⟨⟩ ∙ ⟨⟩) ∙ ⟨⟩) ∙ (k ∙ (c ∙ d))) s =
  ifnil (lookup (unℕ k) s) (usintV n c s) (usintV n d s)
usintV (suc n) _                                    s = s

------------------------------------------------------------------------
-- KEYSTONE: the self-interpreter reproduces `fwdV` given enough fuel.
-- (Enough = the program's own size; the operand rewrite `unℕ ⌜k⌝ℕ ≡ k`
-- is where the index-decode convention discharges.)

usintV-correct : (c : RCmdV) (s : Store) (n : ℕ)
  → sizeV c ≤ n → usintV n ⌜ c ⌝V s ≡ fwdV c s
usintV-correct (swpV k) s (suc n) _ =
  cong (λ j → mapAt R.swapTop j s) (unℕ-⌜⌝ℕ k)
usintV-correct (sqV c d) s (suc n) (s≤s le) =
  trans (cong (usintV n ⌜ d ⌝V)
              (usintV-correct c s n (≤-trans (m≤m+n (sizeV c) (sizeV d)) le)))
        (usintV-correct d (fwdV c s) n (≤-trans (m≤n+m (sizeV d) (sizeV c)) le))
usintV-correct (ifnV k c d) s (suc n) (s≤s le)
  rewrite unℕ-⌜⌝ℕ k =
  cong₂ (ifnil (lookup k s))
        (usintV-correct c s n (≤-trans (m≤m+n (sizeV c) (sizeV d)) le))
        (usintV-correct d s n (≤-trans (m≤n+m (sizeV d) (sizeV c)) le))

-- the exact-fuel corollary (the headline self-interpretation identity).
usintV-exact : (c : RCmdV) (s : Store) → usintV (sizeV c) ⌜ c ⌝V s ≡ fwdV c s
usintV-exact c s = usintV-correct c s (sizeV c) ≤-refl

------------------------------------------------------------------------
-- Reversibility of self-interpretation, FOR FREE: interpreting the
-- encoding of `invV c` undoes interpreting `⌜ c ⌝V` (usintV-exact twice +
-- `RCmdV.inv-soundV`).

usintV-reversible : (c : RCmdV) (s : Store) →
  usintV (sizeV (invV c)) ⌜ invV c ⌝V (usintV (sizeV c) ⌜ c ⌝V s) ≡ s
usintV-reversible c s =
  trans (cong (usintV (sizeV (invV c)) ⌜ invV c ⌝V) (usintV-exact c s))
        (trans (usintV-exact (invV c) (fwdV c s)) (inv-soundV c s))

------------------------------------------------------------------------
-- Self-interpretation preserves the store length L on encoded programs
-- (so a-rev(L) stays well-defined across an interpreted run -- the task-4
-- bound is against a STABLE L).

usintV-length : (c : RCmdV) (s : Store) (n : ℕ) → sizeV c ≤ n
  → length (usintV n ⌜ c ⌝V s) ≡ length s
usintV-length c s n le =
  trans (cong length (usintV-correct c s n le)) (fwdV-length c s)

------------------------------------------------------------------------
-- Test-first: the concrete two-variable run of RCmdV.Examples,
-- self-interpreted (all refl where computation decides).

module Examples where
  open RCmdV.Examples using (s0; prog)   -- L = 2; prog = identity on s0

  -- self-interpretation reproduces the direct semantics, by COMPUTATION.
  ex-correct : usintV (sizeV prog) ⌜ prog ⌝V s0 ≡ fwdV prog s0
  ex-correct = refl

  -- ... and prog is the identity on s0 (swap var 0 twice), interpreted.
  ex-run : usintV (sizeV prog) ⌜ prog ⌝V s0 ≡ s0
  ex-run = refl

  -- one swpV alone, interpreted: var 0's subtrees are exchanged.
  ex-swp : usintV 1 ⌜ swpV 0 ⌝V s0 ≡ (0 , ((⟨⟩ ∙ ⟨⟩) ∙ ⟨⟩)) ∷ (1 , ⟨⟩) ∷ []
  ex-swp = refl

  -- reversibility on the instance: interpret ⌜ invV prog ⌝V to undo it.
  ex-reversible : usintV (sizeV (invV prog)) ⌜ invV prog ⌝V
                    (usintV (sizeV prog) ⌜ prog ⌝V s0) ≡ s0
  ex-reversible = usintV-reversible prog s0

  -- out of fuel = identity (the rollback-safe default).
  ex-nofuel : usintV 0 ⌜ prog ⌝V s0 ≡ s0
  ex-nofuel = refl

  -- unrecognised code = identity.
  ex-garbage : usintV 5 ((⟨⟩ ∙ ⟨⟩) ∙ ⟨⟩) s0 ≡ s0
  ex-garbage = refl

  -- the store length is preserved under self-interpretation (L stable).
  ex-length : length (usintV (sizeV prog) ⌜ prog ⌝V s0) ≡ length s0
  ex-length = usintV-length prog s0 (sizeV prog) ≤-refl
