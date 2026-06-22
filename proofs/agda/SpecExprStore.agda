{-# OPTIONS --safe #-}
------------------------------------------------------------------------
-- G1 building block: soundness of `spec_exp` OVER A STORE.  Extends
-- `AVExprSound` (single annotated value) to expressions evaluated against an
-- av-store, which is exactly what `Cogen.ml`'s `spec_exp`/`sget` do.  This is the
-- precise lemma the command-level congruence needs (the middle of G1, between
-- `AVExprSound` at the expression level and `MemoFold` at the loop level).
-- Referenced by docs/AGDA_CORRESPONDENCE.md (gap G1).
--
-- Setup.  An av-store `σ : ℕ → AV` is the specializer's store; a source store
-- `ss : ℕ → Val` is the runtime store; `denv : ℕ → Val` binds the residual's
-- dynamic variables.  σ is CONSISTENT with ss (under denv) when every slot
-- concretises to the source value: `∀ k → ss k ≡ concretize (σ k) denv`.
-- `sxp e σ` is `spec_exp e` (var ↦ store slot, cst static, hd/tl/cons via the av
-- operations).  We prove:
--
--   * spec-exp-sound : under consistency, specialising an expression and
--       concretising the result equals evaluating the source expression on the
--       source store -- `concretize (sxp e σ) denv ≡ eval e ss`.
--
-- i.e. `spec_exp` commutes with concretisation, pointwise over the store -- no
-- function extensionality needed (consistency is stated pointwise).
------------------------------------------------------------------------

module SpecExprStore where

open import Data.Nat using (ℕ)
open import Relation.Binary.PropositionalEquality using (_≡_; refl; sym; cong; cong₂; trans)

open import AVExprSound
  using ( Val; vnil; vatom; vcons; vhd; vtl
        ; Expr; var; cst; hd; tl; cons; eval
        ; AV; Sv; Dv; Cv; concretize; av_hd; av_tl
        ; hd-sound; tl-sound )

-- `spec_exp` over an av-store (Cogen.ml: var ↦ sget, cst static, hd/tl/cons via
-- the av operations av_hd/av_tl/Cv).
sxp : Expr → (ℕ → AV) → AV
sxp (var k)    σ = σ k
sxp (cst v)    σ = Sv v
sxp (hd e)     σ = av_hd (sxp e σ)
sxp (tl e)     σ = av_tl (sxp e σ)
sxp (cons a b) σ = Cv (sxp a σ) (sxp b σ)

-- consistency of an av-store with a source store under a dynamic environment
Consistent : (ℕ → AV) → (ℕ → Val) → (ℕ → Val) → Set
Consistent σ ss denv = ∀ k → ss k ≡ concretize (σ k) denv

-- spec_exp commutes with concretisation, over a consistent store.
spec-exp-sound : ∀ σ ss denv → Consistent σ ss denv
               → ∀ e → concretize (sxp e σ) denv ≡ eval e ss
spec-exp-sound σ ss denv cons-σ (var k)    = sym (cons-σ k)
spec-exp-sound σ ss denv cons-σ (cst v)    = refl
spec-exp-sound σ ss denv cons-σ (hd e)     =
  trans (hd-sound (sxp e σ) denv) (cong vhd (spec-exp-sound σ ss denv cons-σ e))
spec-exp-sound σ ss denv cons-σ (tl e)     =
  trans (tl-sound (sxp e σ) denv) (cong vtl (spec-exp-sound σ ss denv cons-σ e))
spec-exp-sound σ ss denv cons-σ (cons a b) =
  cong₂ vcons (spec-exp-sound σ ss denv cons-σ a) (spec-exp-sound σ ss denv cons-σ b)
