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
--
-- STAGE 3b (loop).  A dynamic `while` reuses a fixed loop av-store σloop; modelling
-- the loop state as the (source-store, residual-store) pair under the shared guard
-- (lockstep justified by if-guard-agree), `WhileCase.while-store-cong` instantiates
-- MemoFold.run-inv: if the body keeps the two stores consistent, the loop leaves
-- them consistent on exit.
-- Remaining for full G1: the whole-spec assembly (structural induction over `spec`
-- tying asgn/semi/if/while together).
------------------------------------------------------------------------

module SpecCmdStore where

open import Data.Nat using (ℕ)
open import Data.Nat.Properties using (_≟_)
open import Data.Bool using (Bool; true; false)
open import Data.Product using (_×_; _,_; proj₁; proj₂)
open import Data.List using (List; []; _∷_)
open import Relation.Nullary using (yes; no; ¬_)
open import Relation.Binary.PropositionalEquality using (_≡_; refl; sym; trans; cong)
import MemoFold

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

-- GENERALISATION step (shared by the `'if` merge and the `'while` loop-entry):
-- dynamicise ONE var by emitting the residual-only fixup `var k := lift (σ k)`
-- (the SOURCE store is unchanged; rs gets k ↦ the slot's current value; the slot
-- becomes the self-reference Dv (var k)).  Preserves consistency, given SelfRef σ.
dyn1-sound : ∀ σ ss rs k → Consistent σ ss rs → SelfRef σ
           → Consistent (upd σ k (Dv (var k))) ss (upd rs k (eval (lift (σ k)) rs))
dyn1-sound σ ss rs k cons-σ self j with k ≟ j
... | yes refl =
      trans (cons-σ k)
            (sym (trans (upd-same rs k (eval (lift (σ k)) rs)) (lift-sound (σ k) rs)))
... | no k≢j =
      trans (cons-σ j)
            (self j rs (upd rs k (eval (lift (σ k)) rs))
                  (sym (upd-other rs k (eval (lift (σ k)) rs) j k≢j)))

-- generalise a LIST of vars (the escaping set of an `'if`, or the loop-carried
-- set of a `'while`): emit a residual fixup for each, threading the stores.
dynList : List ℕ → (ℕ → AV) → (ℕ → AV)
dynList []       σ = σ
dynList (k ∷ ks) σ = dynList ks (upd σ k (Dv (var k)))

dynListRS : List ℕ → (ℕ → AV) → (ℕ → Val) → (ℕ → Val)
dynListRS []       σ rs = rs
dynListRS (k ∷ ks) σ rs = dynListRS ks (upd σ k (Dv (var k))) (upd rs k (eval (lift (σ k)) rs))

-- generalising a whole list preserves consistency and self-referentiality
-- (a fold of dyn1-sound + selfref-preserved).
dynlist-sound : ∀ ks σ ss rs → Consistent σ ss rs → SelfRef σ
              → Consistent (dynList ks σ) ss (dynListRS ks σ rs) × SelfRef (dynList ks σ)
dynlist-sound []       σ ss rs c s = c , s
dynlist-sound (k ∷ ks) σ ss rs c s =
  dynlist-sound ks (upd σ k (Dv (var k))) ss (upd rs k (eval (lift (σ k)) rs))
                (dyn1-sound σ ss rs k c s) (selfref-preserved σ k s)

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

----------------------------------------------------------------------
-- STAGE 3b: the `'while` case, via MemoFold's loop-invariant preservation.
-- The residual `while` reuses a fixed loop av-store σloop; the loop body maps
-- consistent (source, residual) stores to consistent ones (the command-IH), and
-- guard agreement (if-guard-agree) lets both loops run in LOCKSTEP under the
-- shared guard.  Modelling the loop state as the (source-store, residual-store)
-- pair with the source guard, MemoFold.run-inv gives: the loop preserves store
-- consistency -- if it exits, the final stores are consistent.
module WhileCase (gexpr : Expr) (σloop : ℕ → AV)
                 (bodyS bodyR : (ℕ → Val) → (ℕ → Val)) where

  PS : Set
  PS = (ℕ → Val) × (ℕ → Val)

  pguard : PS → Bool
  pguard p = istrue (eval gexpr (proj₁ p))

  open MemoFold.Loop PS pguard

  pstep : PS → PS
  pstep (ss , rs) = (bodyS ss , bodyR rs)

  Inv : PS → Set
  Inv (ss , rs) = Consistent σloop ss rs

  -- the loop preserves consistency, given the body does (the command-IH).
  while-store-cong : (∀ ss rs → Consistent σloop ss rs
                              → Consistent σloop (bodyS ss) (bodyR rs))
                   → ∀ n ss rs → Consistent σloop ss rs
                   → MaybeInv Inv (run pstep n (ss , rs))
  while-store-cong pres n ss rs c =
    run-inv Inv pstep (λ { (s , r) i → pres s r i }) n (ss , rs) c
