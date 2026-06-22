{-# OPTIONS --safe #-}
------------------------------------------------------------------------
-- G1 ASSEMBLY (straight-line fragment): the whole-spec congruence for skip /
-- assignment / sequencing.  Models `spec : Cmd → av-store → (residual, av-store)`
-- and the source semantics `cexec`, and proves by structural induction that
-- specialising a command and running its residual reproduces the source run --
-- `[cogen(p,s)](d) = [p]((s,d))` for the loop-free, conditional-free fragment.
-- Each case is discharged by the proven command-level lemmas (SpecCmdStore):
-- assignment by asgn-fold-sound / asgn-emit-sound, sequencing by threading the IH.
-- Referenced by docs/AGDA_CORRESPONDENCE.md (gap G1).
--
-- `'if`/`'while` need the per-branch store MERGE (MixMerge) and the loop
-- generalisation (σloop) lifted to a store transformer; this module is the
-- straight-line capstone, those are the remaining assembly step.
------------------------------------------------------------------------

module SpecAssembly where

open import Data.Nat using (ℕ)
open import Data.Nat.Properties using (_≟_)
open import Data.Bool using (Bool; true; false; _∧_)
open import Data.Product using (_×_; _,_; proj₁; proj₂)
open import Relation.Nullary using (yes; no)
open import Relation.Binary.PropositionalEquality using (_≡_; refl; cong₂)

open import AVExprSound using (Val; vcons; Expr; var; eval; AV; Sv; Dv; Cv; concretize; lift)
open import SpecExprStore using (sxp; Consistent)
open import SpecCmdStore
  using (upd; asgn-fold-sound; asgn-emit-sound; SlotDepK; SelfRef; selfref-preserved)

-- straight-line commands.
data Cmd : Set where
  skip : Cmd
  asgn : ℕ → Expr → Cmd
  semi : Cmd → Cmd → Cmd

-- source semantics: a command transforms a Val-store.
cexec : Cmd → (ℕ → Val) → (ℕ → Val)
cexec skip         ss = ss
cexec (asgn k e)   ss = upd ss k (eval e ss)
cexec (semi c1 c2) ss = cexec c2 (cexec c1 ss)

-- is an annotated value fully static (no dynamic leaf)?
av-static? : AV → Bool
av-static? (Sv _)   = true
av-static? (Dv _)   = false
av-static? (Cv a b) = av-static? a ∧ av-static? b

-- the specializer: command + av-store ↦ (residual command, new av-store).
-- assignment folds when its value is fully static (no residual code), else emits.
spec : Cmd → (ℕ → AV) → (Cmd × (ℕ → AV))
spec skip         σ = skip , σ
spec (asgn k e)   σ with av-static? (sxp e σ)
... | true  = skip                       , upd σ k (sxp e σ)
... | false = asgn k (lift (sxp e σ))     , upd σ k (Dv (var k))
spec (semi c1 c2) σ = let r1 = spec c1 σ
                          r2 = spec c2 (proj₂ r1)
                      in semi (proj₁ r1) (proj₁ r2) , proj₂ r2

----------------------------------------------------------------------
-- a fully static slot depends on no part of the residual store (SlotDepK k).
data IsStatic : AV → Set where
  s-sv : ∀ v → IsStatic (Sv v)
  s-cv : ∀ {a b} → IsStatic a → IsStatic b → IsStatic (Cv a b)

∧≡true : ∀ a b → (a ∧ b) ≡ true → (a ≡ true) × (b ≡ true)
∧≡true true  true  _ = refl , refl
∧≡true true  false ()
∧≡true false _     ()

reflect : ∀ av → av-static? av ≡ true → IsStatic av
reflect (Sv v)   _  = s-sv v
reflect (Dv e)   ()
reflect (Cv a b) eq = let (ea , eb) = ∧≡true (av-static? a) (av-static? b) eq
                      in s-cv (reflect a ea) (reflect b eb)

static-dep : ∀ {av} k → IsStatic av → SlotDepK k av
static-dep k (s-sv v)     rs rs′ _ = refl
static-dep k (s-cv ia ib) rs rs′ r =
  cong₂ vcons (static-dep k ia rs rs′ r) (static-dep k ib rs rs′ r)

-- a folded (fully static) slot keeps the store self-referential.
selfref-fold : ∀ σ k e → SelfRef σ → av-static? (sxp e σ) ≡ true
             → SelfRef (upd σ k (sxp e σ))
selfref-fold σ k e self stat j with k ≟ j
... | yes refl = static-dep k (reflect (sxp e σ) stat)
... | no  _    = self j

----------------------------------------------------------------------
-- WHOLE-SPEC CONGRUENCE (straight-line fragment): specialising c and running
-- its residual reproduces the source run, preserving store consistency and the
-- self-reference invariant.
spec-sound : ∀ c σ ss rs → Consistent σ ss rs → SelfRef σ
           → Consistent (proj₂ (spec c σ)) (cexec c ss) (cexec (proj₁ (spec c σ)) rs)
           × SelfRef (proj₂ (spec c σ))
spec-sound skip σ ss rs cons-σ self = cons-σ , self
spec-sound (asgn k e) σ ss rs cons-σ self with av-static? (sxp e σ) in stat
... | true  = asgn-fold-sound σ ss rs k e cons-σ
            , selfref-fold σ k e self stat
... | false = asgn-emit-sound σ ss rs k e cons-σ self
            , selfref-preserved σ k self
spec-sound (semi c1 c2) σ ss rs cons-σ self =
  spec-sound c2 (proj₂ (spec c1 σ)) (cexec c1 ss) (cexec (proj₁ (spec c1 σ)) rs)
            (proj₁ IH1) (proj₂ IH1)
  where IH1 = spec-sound c1 σ ss rs cons-σ self
