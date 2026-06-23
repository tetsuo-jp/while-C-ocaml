{-# OPTIONS --safe #-}
------------------------------------------------------------------------
-- G1 ASSEMBLY, the `'if` case (standalone, on straight-line branches).
-- A dynamic `if g c1 c2` specialises each branch, then MERGES: dynamicise the
-- escaping set `esc = assigned c1 ++ assigned c2` (the only slots the branches
-- can touch, by `frame`), emitting a residual fixup for each (dynlist-sound).
-- Both branches then reach the SAME merged store `dynList esc σ`, so by guard
-- agreement (if-guard-agree) + matching-branch dispatch (cond-cong) the
-- conditional preserves store consistency.  Kept standalone so `SpecAssembly`'s
-- total functions stay untouched.  Referenced by docs/AGDA_CORRESPONDENCE.md.
------------------------------------------------------------------------

module SpecAssemblyIf where

open import Data.Nat using (ℕ)
open import Data.Nat.Properties using (_≟_)
open import Data.List using (List; []; _∷_; _++_)
open import Data.Product using (_×_; _,_; proj₁; proj₂)
open import Data.Sum using (inj₁; inj₂)
open import Relation.Nullary using (yes; no; ¬_)
open import Relation.Binary.PropositionalEquality using (_≡_; refl; trans; cong)

open import AVExprSound using (Val; Expr; eval; AV; Dv; var; concretize; lift)
open import SpecExprStore using (sxp; Consistent)
open import SpecCmdStore
  using (upd; SelfRef; dynList; dynListRS; dynlist-sound
        ; istrue; dispatch; if-guard-agree; cond-cong)
open import SpecAssembly using (Cmd; cexec; spec; spec-sound; assigned; _∈L_; frame)

-- list membership respects append (left / right).
∈L-++ˡ : ∀ {j} xs ys → j ∈L xs → j ∈L (xs ++ ys)
∈L-++ˡ (x ∷ xs) ys (inj₁ e) = inj₁ e
∈L-++ˡ (x ∷ xs) ys (inj₂ m) = inj₂ (∈L-++ˡ xs ys m)

∈L-++ʳ : ∀ {j} xs ys → j ∈L ys → j ∈L (xs ++ ys)
∈L-++ʳ []       ys m = m
∈L-++ʳ (x ∷ xs) ys m = inj₂ (∈L-++ʳ xs ys m)

-- pointwise rewrite of consistency along an equal av-store.
consistent-cong : ∀ {ss rs} (σ σ′ : ℕ → AV) → (∀ k → σ k ≡ σ′ k)
                → Consistent σ ss rs → Consistent σ′ ss rs
consistent-cong {rs = rs} σ σ′ eq cons k = trans (cons k) (cong (λ z → concretize z rs) (eq k))

-- dynList agrees pointwise when the two base stores agree off the list.
dynList-cong : ∀ ks σ σ′ → (∀ j → ¬ (j ∈L ks) → σ j ≡ σ′ j)
             → ∀ k → dynList ks σ k ≡ dynList ks σ′ k
dynList-cong []       σ σ′ h k = h k (λ ())
dynList-cong (x ∷ ks) σ σ′ h k =
  dynList-cong ks (upd σ x (Dv (var x))) (upd σ′ x (Dv (var x))) h′ k
  where h′ : ∀ j → ¬ (j ∈L ks) → upd σ x (Dv (var x)) j ≡ upd σ′ x (Dv (var x)) j
        h′ j j∉ks with x ≟ j
        ... | yes refl = refl
        ... | no  x≢j  = h j (λ { (inj₁ e) → x≢j e ; (inj₂ m) → j∉ks m })

----------------------------------------------------------------------
-- the `'if` case: the conditional's result is consistent with `dynList esc σ`.
spec-if-sound :
  ∀ g c1 c2 σ ss rs → Consistent σ ss rs → SelfRef σ →
  Consistent (dynList (assigned c1 ++ assigned c2) σ)
    (dispatch (istrue (eval g ss)) (cexec c1 ss) (cexec c2 ss))
    (dispatch (istrue (eval (lift (sxp g σ)) rs))
              (dynListRS (assigned c1 ++ assigned c2) (proj₂ (spec c1 σ)) (cexec (proj₁ (spec c1 σ)) rs))
              (dynListRS (assigned c1 ++ assigned c2) (proj₂ (spec c2 σ)) (cexec (proj₁ (spec c2 σ)) rs)))
spec-if-sound g c1 c2 σ ss rs cons-σ self =
  cond-cong {σ′ = dynList (assigned c1 ++ assigned c2) σ}
            (istrue (eval g ss)) (istrue (eval (lift (sxp g σ)) rs))
            (if-guard-agree σ ss rs g cons-σ)
            (mk c1 (frame c1 σ) (∈L-++ˡ (assigned c1) (assigned c2)))
            (mk c2 (frame c2 σ) (∈L-++ʳ (assigned c1) (assigned c2)))
  where
    esc = assigned c1 ++ assigned c2
    mk : ∀ c → (∀ j → ¬ (j ∈L assigned c) → proj₂ (spec c σ) j ≡ σ j)
       → (∀ {j} → j ∈L assigned c → j ∈L esc)
       → Consistent (dynList esc σ) (cexec c ss)
                    (dynListRS esc (proj₂ (spec c σ)) (cexec (proj₁ (spec c σ)) rs))
    mk c framec incl =
      consistent-cong (dynList esc (proj₂ (spec c σ))) (dynList esc σ)
        (dynList-cong esc (proj₂ (spec c σ)) σ
           (λ j j∉esc → framec j (λ m → j∉esc (incl m))))
        (proj₁ (dynlist-sound esc (proj₂ (spec c σ)) (cexec c ss)
                  (cexec (proj₁ (spec c σ)) rs) (proj₁ IH) (proj₂ IH)))
      where IH = spec-sound c σ ss rs cons-σ self
