{-# OPTIONS --safe #-}

------------------------------------------------------------------------
-- Reversible Levin R3c step 2 (brick 1): an ENCODABLE, FIRST-ORDER reversible
-- language WITH a loop -- the object language the reversible self-interpreter
-- `rsint` will be written in.
--
-- `RevInst.RCmd` (R3a/R3b) is encodable but has NO recursive control; `RevLoop`
-- (R1a'-guard) has the general reversible loop but with atoms = arbitrary
-- RELATIONS (functions-as-data, NOT encodable).  This module combines the two
-- WITHOUT reintroducing functions-as-data: the atoms are the FIRST-ORDER
-- `RevInst.RCmd` programs (run by the TOTAL function `RevInst.fwd`) and the loop
-- guards are a small FIRST-ORDER predicate language `GCode` (nil-tests along
-- hd/tl paths -- exactly what a tag dispatcher needs).  Everything is data, hence
-- encodable.
--
-- Because atoms run by a total function, the atom case of `inv-sound` is closed
-- DEFINITIONALLY by `RevInst.inv-sound` (no funext, no relation equality); the
-- seq/cond/loop cases reuse the `rev-rest` accumulator chain-reversal from
-- `RevLoop`.  Result: `P` is a reversible language with loops (`inv-sound`,
-- `inv-inv`, `inv-complete`) whose programs are first-order data -- ready for the
-- self-interpreter term `rsint` (R3c step 2, brick 2) and its encoding.
------------------------------------------------------------------------

module RCmdL where

open import Data.Bool using (Bool; true; false)
open import Data.Product using (Σ-syntax; _×_; _,_)
open import Relation.Binary.PropositionalEquality
  using (_≡_; refl; cong₂; subst)
open import UniversalInterp using (V; ⟨⟩; _∙_; hd; tl)
import RevInst as R

------------------------------------------------------------------------
-- First-order loop guards: nil-tests along an hd/tl path (encodable predicates).

data GCode : Set where
  gNil : GCode           -- λ v → isNil v
  gHd  : GCode → GCode   -- λ v → g (hd v)
  gTl  : GCode → GCode   -- λ v → g (tl v)

⟦_⟧g : GCode → V → Bool
⟦ gNil  ⟧g v = R.isNil v
⟦ gHd g ⟧g v = ⟦ g ⟧g (hd v)
⟦ gTl g ⟧g v = ⟦ g ⟧g (tl v)

------------------------------------------------------------------------
-- The first-order reversible language with a loop.  Atoms are RevInst.RCmd.

data P : Set where
  pOp   : R.RCmd → P
  pSeq  : P → P → P
  pCond : GCode → P → P → GCode → P
  pLoop : GCode → P → P → GCode → P   -- from e do D loop L until f

invP : P → P
invP (pOp c)         = pOp (R.inv c)
invP (pSeq a b)      = pSeq (invP b) (invP a)
invP (pCond e a b f) = pCond f (invP a) (invP b) e
invP (pLoop e D L f) = pLoop f (invP D) (invP L) e

------------------------------------------------------------------------
-- Big-step semantics.  Atoms run by the TOTAL function RevInst.fwd; the loop
-- uses the auxiliary Rest relation, exactly as in RevLoop.

infix 3 _⊢_⇒_

data _⊢_⇒_ : P → V → V → Set
data Rest (e : GCode) (D L : P) (f : GCode) : V → V → Set

data _⊢_⇒_ where
  e-op   : ∀ {c s} → pOp c ⊢ s ⇒ R.fwd c s
  e-seq  : ∀ {a b s t u} → a ⊢ s ⇒ t → b ⊢ t ⇒ u → pSeq a b ⊢ s ⇒ u
  e-then : ∀ {e a b f s t}
         → ⟦ e ⟧g s ≡ true  → a ⊢ s ⇒ t → ⟦ f ⟧g t ≡ true  → pCond e a b f ⊢ s ⇒ t
  e-else : ∀ {e a b f s t}
         → ⟦ e ⟧g s ≡ false → b ⊢ s ⇒ t → ⟦ f ⟧g t ≡ false → pCond e a b f ⊢ s ⇒ t
  e-loop : ∀ {e D L f s t u}
         → ⟦ e ⟧g s ≡ true → D ⊢ s ⇒ t → Rest e D L f t u → pLoop e D L f ⊢ s ⇒ u

data Rest e D L f where
  r-exit : ∀ {w} → ⟦ f ⟧g w ≡ true → Rest e D L f w w
  r-iter : ∀ {w u v x}
         → ⟦ f ⟧g w ≡ false → L ⊢ w ⇒ u → ⟦ e ⟧g u ≡ false → D ⊢ u ⇒ v
         → Rest e D L f v x → Rest e D L f w x

------------------------------------------------------------------------
-- MAIN THEOREM: inversion is sound.  Atom case = RevInst.inv-sound (definitional,
-- since RevInst.bwd c = RevInst.fwd (RevInst.inv c)); the rest mirrors RevLoop.

inv-sound : ∀ {p s t} → p ⊢ s ⇒ t → invP p ⊢ t ⇒ s

rev-rest : ∀ {e D L f s t u}
         → Rest e D L f t u
         → ∀ {q} → invP D ⊢ t ⇒ q → Rest f (invP D) (invP L) e q s
         → Σ[ m ∈ V ] (⟦ f ⟧g u ≡ true) × (invP D ⊢ u ⇒ m) × Rest f (invP D) (invP L) e m s

inv-sound (e-op {c} {s}) =
  subst (λ z → pOp (R.inv c) ⊢ R.fwd c s ⇒ z) (R.inv-sound c s) e-op
inv-sound (e-seq as bs)     = e-seq (inv-sound bs) (inv-sound as)
inv-sound (e-then es as ft) = e-then ft (inv-sound as) es
inv-sound (e-else es bs ft) = e-else ft (inv-sound bs) es
inv-sound (e-loop es Dst rest)
  with rev-rest rest (inv-sound Dst) (r-exit es)
... | (m , fu , invDum , revrest) = e-loop fu invDum revrest

rev-rest (r-exit fw) {q} invDtq acc = (q , fw , invDtq , acc)
rev-rest (r-iter fw Lwu eu Duv rest1) invDtq acc =
  rev-rest rest1 (inv-sound Duv)
    (r-iter eu (inv-sound Lwu) fw invDtq acc)

------------------------------------------------------------------------
-- inv is a syntactic involution ⇒ the reverse direction too.

open import Relation.Binary.PropositionalEquality using (cong)

inv-inv : ∀ p → invP (invP p) ≡ p
inv-inv (pOp c)         = cong pOp (R.inv-inv c)
inv-inv (pSeq a b)      = cong₂ pSeq (inv-inv a) (inv-inv b)
inv-inv (pCond e a b f) = cong₂ (λ x y → pCond e x y f) (inv-inv a) (inv-inv b)
inv-inv (pLoop e D L f) = cong₂ (λ x y → pLoop e x y f) (inv-inv D) (inv-inv L)

inv-complete : ∀ {p s t} → invP p ⊢ t ⇒ s → p ⊢ s ⇒ t
inv-complete {p} {s} {t} d = subst (λ x → x ⊢ s ⇒ t) (inv-inv p) (inv-sound d)

------------------------------------------------------------------------
-- Test-first: a concrete loop over V whose body is the ENCODABLE op `swp`.
-- On ⟨⟩∙(⟨⟩∙⟨⟩) the entry guard (hd is nil) holds, one swap gives (⟨⟩∙⟨⟩)∙⟨⟩,
-- and the exit guard (tl is nil) then holds.  inv-sound runs it back.

module Examples where
  s0 t0 : V
  s0 = ⟨⟩ ∙ (⟨⟩ ∙ ⟨⟩)
  t0 = (⟨⟩ ∙ ⟨⟩) ∙ ⟨⟩

  swpLoop : P
  swpLoop = pLoop (gHd gNil) (pOp R.swp) (pOp R.swp) (gTl gNil)

  ex-run : swpLoop ⊢ s0 ⇒ t0
  ex-run = e-loop refl e-op (r-exit refl)

  ex-inv-inv : invP (invP swpLoop) ≡ swpLoop
  ex-inv-inv = refl

  ex-rev : invP swpLoop ⊢ t0 ⇒ s0
  ex-rev = inv-sound ex-run

  -- a plain sequenced op program, reversed.
  seqProg : P
  seqProg = pSeq (pOp R.swp) (pOp R.swp)

  ex-seq : seqProg ⊢ s0 ⇒ s0
  ex-seq = e-seq e-op e-op        -- swp ∘ swp = id on s0 (two subtree swaps)

  ex-seq-rev : invP seqProg ⊢ s0 ⇒ s0
  ex-seq-rev = inv-sound ex-seq
