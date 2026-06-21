{-# OPTIONS --safe #-}
------------------------------------------------------------------------
-- Design spec for SPINE-PRESERVING EMIT (contribution C5) of cogen --online.
-- Referenced by docs/FUTAMURA.md and docs/PAPER-SKELETON.md.
--
-- Context.  When the online specializer emits an assignment whose value has a
-- static cons SPINE but DYNAMIC leaves (e.g. a self-interpreter's variable store
-- after `T := cons (hd Vl) T`), it must keep the spine known so loops that walk it
-- still unroll.  The old code collapsed the slot to a single dynamic reference
-- `Dv (var k)`, losing the spine, so the store-walk residualised.  The fix stores
-- the slot as a Cv mirroring the spine, every DYNAMIC leaf replaced by a
-- SELF-reference (an hd/tl path into the slot's own variable k).
--
-- This module models that representation and proves it correct:
--
--   * readback  : evaluating the spine-of-self-refs against k's actual value
--                 RECONSTRUCTS that value -- spine preservation is faithful;
--   * stable    : the slot references only k, so re-materialising ANY OTHER
--                 variable leaves it unchanged (the emit-model soundness of
--                 MixEmitModel, here for the partially-static SPINE case);
--   * cross-unsound : the contrast -- a slot that instead referenced another
--                 variable j would change when j is re-materialised (why we use
--                 self-references, not cross-references).
------------------------------------------------------------------------

module SpinePreservingEmit where

open import Relation.Binary.PropositionalEquality
  using (_≡_; refl; cong; cong₂; sym)
open import Relation.Nullary using (¬_)
open import Data.Nat using (ℕ; zero; suc)

-- Values: leaves (abstract tags) and cons pairs.
data Val : Set where
  leaf : ℕ → Val
  pair : Val → Val → Val

hdV : Val → Val
hdV (pair x _) = x
hdV (leaf n)   = leaf n      -- as in WHILE: hd of an atom is itself/nil-like

tlV : Val → Val
tlV (pair _ y) = y
tlV (leaf n)   = leaf n

-- A path = an hd/tl access chain (built OUTSIDE-IN, mirroring selfref in Cogen.ml:
-- the child of a pair at path p is reached by `hd p` / `tl p`).
data Path : Set where
  root : Path
  hdP  : Path → Path
  tlP  : Path → Path

nav : Path → Val → Val
nav root    v = v
nav (hdP p) v = hdV (nav p v)
nav (tlP p) v = tlV (nav p v)

-- A slot: the spine, with leaves carrying a self-reference path into k.
data Slot : Set where
  sref  : Path → Slot          -- self-reference: nav this path into (var k)
  scons : Slot → Slot → Slot

-- selfref / spine-preserving emit: build the slot for value w, leaves -> paths.
buildSlot : Path → Val → Slot
buildSlot p (pair x y) = scons (buildSlot (hdP p) x) (buildSlot (tlP p) y)
buildSlot p (leaf _)   = sref p

-- evaluate a slot against vk = the value currently stored at variable k.
evalSlot : Slot → Val → Val
evalSlot (sref p)    vk = nav p vk
evalSlot (scons a b) vk = pair (evalSlot a vk) (evalSlot b vk)

------------------------------------------------------------------------
-- readback: the spine-of-self-refs faithfully reconstructs the value.
------------------------------------------------------------------------

-- general form: if the value AT path p in v is w, the slot built for w at p
-- evaluates (against v) back to w.
readback-gen : ∀ (w : Val) (p : Path) (v : Val) → nav p v ≡ w
             → evalSlot (buildSlot p w) v ≡ w
readback-gen (leaf n)   p v eq = eq
readback-gen (pair x y) p v eq =
  cong₂ pair
    (readback-gen x (hdP p) v (cong hdV eq))
    (readback-gen y (tlP p) v (cong tlV eq))

-- the slot built for k's own value v reconstructs v (p = root).
readback : ∀ (v : Val) → evalSlot (buildSlot root v) v ≡ v
readback v = readback-gen v root v refl

------------------------------------------------------------------------
-- stability: a self-referential slot reads ONLY k, so updates elsewhere
-- (re-materialisation of other variables) cannot change it.
------------------------------------------------------------------------

-- A two-variable store; the slot self-refers into vK.
data Var : Set where vK vJ : Var

Store : Set
Store = Var → Val

upd : Var → Val → Store → Store
upd vK w st vK = w
upd vK w st vJ = st vJ
upd vJ w st vK = st vK
upd vJ w st vJ = w

-- the slot's value depends only on st vK
evalIn : Store → Slot → Val
evalIn st s = evalSlot s (st vK)

-- re-materialising vJ (any value w) does not change the self-ref slot's value
stable : ∀ (st : Store) (w : Val) (s : Slot) → evalIn (upd vJ w st) s ≡ evalIn st s
stable st w s = refl

------------------------------------------------------------------------
-- contrast: a CROSS-reference slot (reading vJ) is NOT stable -- exactly why
-- the emit uses self-references.  (Mirrors MixEmitModel.fold-unsound for the
-- partially-static spine case.)
------------------------------------------------------------------------

-- a cross-ref "slot" is just: read variable vJ.
evalCross : Store → Val
evalCross st = st vJ

-- after re-materialising vJ to a DIFFERENT value, the cross-ref reads the new one
cross-changes : ∀ (st : Store) (w : Val) → evalCross (upd vJ w st) ≡ w
cross-changes st w = refl

-- so if the intended (old) value differs from w, the cross-ref is now wrong:
-- evalCross (upd vJ w st) reduces to w, so `≡ old` is `w ≡ old`, refuted by neq.
cross-unsound : ∀ (st : Store) (old w : Val) → ¬ (old ≡ w)
              → ¬ (evalCross (upd vJ w st) ≡ old)
cross-unsound st old w neq ev = neq (sym ev)
