{-# OPTIONS --safe #-}
------------------------------------------------------------------------
-- G1 building block (command level): assignment (both the FOLD and the EMIT
-- case) and sequencing preserve store consistency.  Connects `SpecExprStore`
-- (expressions over a store) toward `MemoFold` (the loop level).
-- Referenced by docs/AGDA_CORRESPONDENCE.md (gap G1).
--
-- A command transforms a store.  Store consistency `∀k. ss k ≡ concretize (σ k)
-- rs` (from SpecExprStore) relates the av-store σ, the source store ss, and the
-- residual runtime store rs.
--
-- STAGE 1 (fold + sequencing):
--   * asgn-fold-sound : a FOLDED `x := e` (av-store updated, no residual code, rs
--       untouched) preserves consistency -- the new slot `spec_exp e σ`
--       concretises to the source value `eval e ss` (via spec-exp-sound);
--   * seq-cong : consistency-preserving steps compose (the `'semi` case).
--
-- STAGE 2 (emit).  A DYNAMIC `x := e` EMITS `x := lift(spec_exp e σ)` (so rs is
-- updated at x) and sets the slot to the SELF-reference `Dv (var x)`.  The other
-- slots must be unaffected by the rs-update at x -- the no-cross-reference
-- condition of `SpinePreservingEmit`, here as `SlotDepK`/`SelfRef`:
--   * SlotDepK k av : concretising av depends only on rs at index k;
--   * selfref-dep / selfref-preserved : a self-ref slot satisfies it, and emit
--       preserves `SelfRef` of the whole store;
--   * asgn-emit-sound : an EMITTED assignment preserves consistency, given the
--       store is self-referential (so updating rs at x leaves other slots intact).
--
-- STAGE 3a (conditional).  A dynamic `if` specialises both branches and merges
-- the stores (per-slot merge = `MixMerge`):
--   * if-guard-agree : the residual guard dispatches like the source guard;
--   * cond-cong : with guard agreement and both branches reaching the common
--       merged store, the conditional preserves consistency.
-- Remaining for full G1: `'while` (via `MemoFold`) and the whole-spec assembly.
------------------------------------------------------------------------

module SpecCmdStore where

open import Data.Nat using (ℕ)
open import Data.Nat.Properties using (_≟_)
open import Data.Bool using (Bool; true; false)
open import Relation.Nullary using (yes; no; ¬_)
open import Relation.Binary.PropositionalEquality using (_≡_; refl; sym; trans; cong)

open import AVExprSound using (Val; vnil; vatom; vcons; Expr; var; eval; AV; Dv; concretize; lift; lift-sound)
open import SpecExprStore using (sxp; Consistent; spec-exp-sound)

-- pointwise store update over any index→value function.
upd : {A : Set} → (ℕ → A) → ℕ → A → (ℕ → A)
upd f k v j with k ≟ j
... | yes _ = v
... | no  _ = f j

upd-same : {A : Set} (f : ℕ → A) (k : ℕ) (v : A) → upd f k v k ≡ v
upd-same f k v with k ≟ k
... | yes _   = refl
... | no  k≢k with k≢k refl
...              | ()

upd-other : {A : Set} (f : ℕ → A) (k : ℕ) (v : A) (j : ℕ)
          → ¬ (k ≡ j) → upd f k v j ≡ f j
upd-other f k v j k≢j with k ≟ j
... | yes p with k≢j p
...            | ()
upd-other f k v j k≢j | no _ = refl

----------------------------------------------------------------------
-- the FOLD case of assignment preserves store consistency.
asgn-fold-sound : ∀ σ ss rs k e → Consistent σ ss rs
                → Consistent (upd σ k (sxp e σ)) (upd ss k (eval e ss)) rs
asgn-fold-sound σ ss rs k e cons-σ j with k ≟ j
... | yes refl = sym (spec-exp-sound σ ss rs cons-σ e)   -- both upds reduce; goal: eval e ss ≡ concretize (sxp e σ) rs
... | no  _    = cons-σ j                                 -- both upds reduce to the unchanged slot

----------------------------------------------------------------------
-- sequencing: two consistency-preserving steps (same residual store rs) compose.
-- The `'semi` case for the fold fragment (each fold leaves rs untouched).
seq-cong : {σ₁ σ₂ σ₃ : ℕ → AV} {s₁ s₂ s₃ : ℕ → Val} {rs : ℕ → Val}
         → (Consistent σ₁ s₁ rs → Consistent σ₂ s₂ rs)
         → (Consistent σ₂ s₂ rs → Consistent σ₃ s₃ rs)
         → Consistent σ₁ s₁ rs → Consistent σ₃ s₃ rs
seq-cong step1 step2 c = step2 (step1 c)

----------------------------------------------------------------------
-- STAGE 2: the EMIT case.
-- A slot `depends only on index k` if concretising it is insensitive to the
-- residual store everywhere except k (the no-cross-reference condition).
SlotDepK : ℕ → AV → Set
SlotDepK k av = ∀ rs rs′ → rs k ≡ rs′ k → concretize av rs ≡ concretize av rs′

-- a self-referential store: every slot depends only on its own index.
SelfRef : (ℕ → AV) → Set
SelfRef σ = ∀ j → SlotDepK j (σ j)

-- the emitted self-reference slot `Dv (var k)` depends only on k.
selfref-dep : ∀ k → SlotDepK k (Dv (var k))
selfref-dep k rs rs′ eq = eq

-- emit preserves self-referentiality: σ' = σ[k ↦ Dv (var k)].
selfref-preserved : ∀ σ k → SelfRef σ → SelfRef (upd σ k (Dv (var k)))
selfref-preserved σ k self j with k ≟ j
... | yes refl = selfref-dep k
... | no  _    = self j

-- an EMITTED assignment `x := e` preserves store consistency, given SelfRef σ.
-- residual: `x := lift (spec_exp e σ)` (rs updated at k); slot ↦ Dv (var k).
asgn-emit-sound : ∀ σ ss rs k e → Consistent σ ss rs → SelfRef σ
                → Consistent (upd σ k (Dv (var k)))
                             (upd ss k (eval e ss))
                             (upd rs k (eval (lift (sxp e σ)) rs))
asgn-emit-sound σ ss rs k e cons-σ self j with k ≟ j
... | yes refl =
      sym (trans (upd-same rs k (eval (lift (sxp e σ)) rs))
                 (trans (lift-sound (sxp e σ) rs) (spec-exp-sound σ ss rs cons-σ e)))
... | no k≢j =
      trans (cons-σ j)
            (self j rs (upd rs k (eval (lift (sxp e σ)) rs))
                  (sym (upd-other rs k (eval (lift (sxp e σ)) rs) j k≢j)))

----------------------------------------------------------------------
-- STAGE 3a: the `'if` case.  A dynamic `if g c1 c2` specialises to
-- `if (lift (spec_exp g σ)) r1 r2` and MERGES the two branch stores into one σ'
-- (the per-slot merge is `MixMerge`).  Two facts:
--
--   * if-guard-agree : the residual guard evaluates (on rs) to the SAME value as
--       the source guard (on ss), so both dispatch identically;
--   * cond-cong : if both branches reach a COMMON consistent store σ' (the merge,
--       justified by MixMerge) and the guards agree, the conditional preserves
--       consistency -- whichever branch runs, the result is consistent with σ'.

-- WHILE truthiness: nil is false, any cons/atom is true (as in EvalWhile/Cogen).
istrue : Val → Bool
istrue vnil        = false
istrue (vatom _)   = true
istrue (vcons _ _) = true

dispatch : {A : Set} → Bool → A → A → A
dispatch true  t e = t
dispatch false t e = e

-- the residual `if` guard dispatches exactly like the source guard.
if-guard-agree : ∀ σ ss rs g → Consistent σ ss rs
               → istrue (eval (lift (sxp g σ)) rs) ≡ istrue (eval g ss)
if-guard-agree σ ss rs g cons-σ =
  cong istrue (trans (lift-sound (sxp g σ) rs) (spec-exp-sound σ ss rs cons-σ g))

-- given guard agreement and that each branch reaches the common merged store σ',
-- the conditional preserves consistency (the taken branch's result is consistent).
cond-cong : {σ′ : ℕ → AV} {s₁ s₂ r₁ r₂ : ℕ → Val} (b br : Bool)
          → br ≡ b
          → Consistent σ′ s₁ r₁ → Consistent σ′ s₂ r₂
          → Consistent σ′ (dispatch b s₁ s₂) (dispatch br r₁ r₂)
cond-cong b br eq h₁ h₂ rewrite eq with b
... | true  = h₁
... | false = h₂
