{-# OPTIONS --safe #-}
------------------------------------------------------------------------
-- G1 building block (command level, STAGE 1): the FOLD case of assignment and
-- sequencing preserve store consistency.  Connects `SpecExprStore` (expressions
-- over a store) toward `MemoFold` (the loop level); the remaining emit case
-- (needs the self-reference / no-cross-ref condition) is stage 2.
-- Referenced by docs/AGDA_CORRESPONDENCE.md (gap G1).
--
-- A command transforms a store.  The specializer FOLDS a static assignment by
-- updating its av-store (no residual code), so the residual store `rs` is
-- unchanged.  Store consistency `∀k. ss k ≡ concretize (σ k) rs`
-- (from SpecExprStore) must be preserved.  Here:
--
--   * asgn-fold-sound : a folded `x := e` preserves consistency -- the av-store's
--       new slot `spec_exp e σ` concretises to the source value `eval e ss`
--       (via spec-exp-sound), and rs is untouched;
--   * seq-cong : if two steps each preserve consistency (same rs), so does their
--       sequence -- the `'semi` case for the fold fragment.
------------------------------------------------------------------------

module SpecCmdStore where

open import Data.Nat using (ℕ)
open import Data.Nat.Properties using (_≟_)
open import Relation.Nullary using (yes; no; ¬_)
open import Relation.Binary.PropositionalEquality using (_≡_; refl; sym; trans; cong)

open import AVExprSound using (Val; Expr; eval; AV; concretize)
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
