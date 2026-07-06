{-# OPTIONS --safe #-}

------------------------------------------------------------------------
-- Reversible Levin Track-2 (brick S3): a MULTI-VARIABLE reversible language
-- `RCmdV` over the association-list store (RevStore) -- the object language the
-- store-carrying interpreter (S4) will self-interpret, so its per-op cost carries
-- the length-`L` lookup/update walk of the measured 364×.
--
-- Atoms are variable-indexed: `swpV k` swaps the root subtrees of variable k's
-- value (self-inverse via `RevStore.mapAt-invol`); `sqV` sequences; `ifnV k`
-- branches on variable k being nil.  Every atom PRESERVES every variable's
-- nil-ness (`preservesNilV`), so the `ifnV` guard is recovered on inversion and
-- `inv-sound` holds (mirroring RevInst, lifted to the multi-slot store).
-- `ocostV` = 1 per executed node (the fair unit cost the interpreter charges
-- against).  --safe, axiom-free.
------------------------------------------------------------------------

module RCmdV where

open import Data.Nat using (ℕ; suc; _+_)
open import Data.Bool using (Bool; true; false)
open import Data.List using ([]; _∷_)
open import Data.Product using (_,_)
open import Relation.Binary.PropositionalEquality using (_≡_; refl; trans; cong; cong₂)
open import UniversalInterp using (V; ⟨⟩; _∙_; ifnil)
open import RevStore using (Store; lookup; mapAt; mapAt-invol; eqℕ)
import RevInst as R

------------------------------------------------------------------------
-- `swpV`'s atom preserves every variable's nil-ness.

swapTop-nil : (v : V) → R.isNil (R.swapTop v) ≡ R.isNil v
swapTop-nil ⟨⟩      = refl
swapTop-nil (a ∙ b) = refl

lookup-mapAt-nil : (j k : ℕ) (s : Store)
  → R.isNil (lookup j (mapAt R.swapTop k s)) ≡ R.isNil (lookup j s)
lookup-mapAt-nil j k []             = refl
lookup-mapAt-nil j k ((k' , v) ∷ s) with eqℕ k k'
... | true  with eqℕ j k'
...   | true  = swapTop-nil v
...   | false = refl
lookup-mapAt-nil j k ((k' , v) ∷ s) | false with eqℕ j k'
...   | true  = refl
...   | false = lookup-mapAt-nil j k s

------------------------------------------------------------------------
-- syntax, semantics, inversion.

data RCmdV : Set where
  swpV : ℕ → RCmdV
  sqV  : RCmdV → RCmdV → RCmdV
  ifnV : ℕ → RCmdV → RCmdV → RCmdV

fwdV : RCmdV → Store → Store
fwdV (swpV k)     s = mapAt R.swapTop k s
fwdV (sqV c d)    s = fwdV d (fwdV c s)
fwdV (ifnV k c d) s = ifnil (lookup k s) (fwdV c s) (fwdV d s)

invV : RCmdV → RCmdV
invV (swpV k)     = swpV k
invV (sqV c d)    = sqV (invV d) (invV c)
invV (ifnV k c d) = ifnV k (invV c) (invV d)

invV-invol : (c : RCmdV) → invV (invV c) ≡ c
invV-invol (swpV k)     = refl
invV-invol (sqV c d)    = cong₂ sqV (invV-invol c) (invV-invol d)
invV-invol (ifnV k c d) = cong₂ (ifnV k) (invV-invol c) (invV-invol d)

------------------------------------------------------------------------
-- every command preserves every variable's nil-ness (⇒ ifnV guard survives).

preservesNilV : (c : RCmdV) (j : ℕ) (s : Store)
  → R.isNil (lookup j (fwdV c s)) ≡ R.isNil (lookup j s)
preservesNilV (swpV k)     j s = lookup-mapAt-nil j k s
preservesNilV (sqV c d)    j s = trans (preservesNilV d j (fwdV c s)) (preservesNilV c j s)
preservesNilV (ifnV k c d) j s with lookup k s
... | ⟨⟩    = preservesNilV c j s
... | _ ∙ _ = preservesNilV d j s

------------------------------------------------------------------------
-- KEYSTONE: inversion is sound.  swpV = mapAt-invol; sqV mirrors RevInst; ifnV
-- recovers the guard via preservesNilV (ifnil-then/else on the transformed store).

inv-soundV : (c : RCmdV) (s : Store) → fwdV (invV c) (fwdV c s) ≡ s
inv-soundV (swpV k)  s = mapAt-invol R.swapTop R.swapTop-invol k s
inv-soundV (sqV c d) s =
  trans (cong (fwdV (invV c)) (inv-soundV d (fwdV c s))) (inv-soundV c s)
inv-soundV (ifnV k c d) s with lookup k s in eq
... | ⟨⟩ rewrite eq =
  trans (R.ifnil-then (lookup k (fwdV c s)) (fwdV (invV c) (fwdV c s)) (fwdV (invV d) (fwdV c s))
                      (trans (preservesNilV c k s) (cong R.isNil eq)))
        (inv-soundV c s)
... | (a ∙ b) rewrite eq =
  trans (R.ifnil-else (lookup k (fwdV d s)) (fwdV (invV c) (fwdV d s)) (fwdV (invV d) (fwdV d s))
                      (trans (preservesNilV d k s) (cong R.isNil eq)))
        (inv-soundV d s)

------------------------------------------------------------------------
-- fair unit cost: 1 per executed node (ifnV runs only the taken branch).

ocostV : RCmdV → Store → ℕ
ocostV (swpV k)     s = 1
ocostV (sqV c d)    s = 1 + ocostV c s + ocostV d (fwdV c s)
ocostV (ifnV k c d) s = 1 + ifnil (lookup k s) (ocostV c s) (ocostV d s)

------------------------------------------------------------------------
-- Test-first: a concrete two-variable reversible program run + reversed.

module Examples where
  s0 : Store
  s0 = (0 , (⟨⟩ ∙ (⟨⟩ ∙ ⟨⟩))) ∷ (1 , ⟨⟩) ∷ []

  -- swap var 0's subtrees, then (guard var 1 nil ⇒) swap var 0 again = identity.
  prog : RCmdV
  prog = sqV (swpV 0) (ifnV 1 (swpV 0) (swpV 0))

  ex-rev : fwdV (invV prog) (fwdV prog s0) ≡ s0
  ex-rev = inv-soundV prog s0

  ex-invol : invV (invV prog) ≡ prog
  ex-invol = refl
