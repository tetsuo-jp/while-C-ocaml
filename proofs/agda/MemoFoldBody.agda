{-# OPTIONS --safe #-}
------------------------------------------------------------------------
-- M2 PER-ITERATION OBLIGATION (gap row 21): the residual body ≡ one source step.
--
-- `MemoFold` proved the loop-LEVEL fold: the folded `while` is sound GIVEN that
-- the specialized body, run once, equals one source iteration.  That
-- per-iteration "body ≡ source step" obligation itself was differential.  This
-- module discharges it and shows it suffices for the whole loop.
--
--   * `body-step`: `rev (specB b sv) dv ≡ aev b sv dv` -- the residual body, run
--     once on the dynamic store, reproduces ONE source iteration (static input
--     `sv` invariant, the M2 memo-fold assumption);
--   * `fold-sound`: `resLoop n (specB b sv) dv ≡ srcLoop n b sv dv` -- the folded
--     `while` (iterating the residual body) equals the source loop, by induction
--     using `body-step` at each iteration.
--
-- Together with `MemoFold` (the general loop-level fold) this closes row 21: the
-- per-iteration obligation is now proven, not merely differential.
--
-- Referenced by docs/AGDA_CORRESPONDENCE.md (row 21).
------------------------------------------------------------------------

module MemoFoldBody where

open import Data.Nat using (ℕ; zero; suc)
open import Relation.Binary.PropositionalEquality using (_≡_; refl)

------------------------------------------------------------------------
-- Values, total head/tail, and the loop-guard test (cons ⇒ continue).
------------------------------------------------------------------------

data Val : Set where
  ⟨⟩  : Val
  _∙_ : Val → Val → Val

hd : Val → Val
hd (a ∙ _) = a
hd ⟨⟩      = ⟨⟩

tl : Val → Val
tl (_ ∙ b) = b
tl ⟨⟩      = ⟨⟩

ifc : ∀ {A : Set} → Val → A → A → A
ifc ⟨⟩      t e = e
ifc (_ ∙ _) t e = t

------------------------------------------------------------------------
-- Residual body language (one dynamic input) and source body (two inputs).
------------------------------------------------------------------------

data RTm : Set where
  din   : RTm
  rcons : RTm → RTm → RTm
  rhd   : RTm → RTm
  rtl   : RTm → RTm
  rlit  : Val → RTm

rev : RTm → Val → Val
rev din         dv = dv
rev (rcons a b) dv = rev a dv ∙ rev b dv
rev (rhd r)     dv = hd (rev r dv)
rev (rtl r)     dv = tl (rev r dv)
rev (rlit v)    dv = v

data ATm : Set where
  sin   : ATm
  din'  : ATm
  anil  : ATm
  acons : ATm → ATm → ATm
  ahd   : ATm → ATm
  atl   : ATm → ATm

aev : ATm → Val → Val → Val
aev sin         sv dv = sv
aev din'        sv dv = dv
aev anil        sv dv = ⟨⟩
aev (acons a b) sv dv = aev a sv dv ∙ aev b sv dv
aev (ahd a)     sv dv = hd (aev a sv dv)
aev (atl a)     sv dv = tl (aev a sv dv)

------------------------------------------------------------------------
-- The body specializer and the PER-ITERATION OBLIGATION.
------------------------------------------------------------------------

specB : ATm → Val → RTm
specB sin         sv = rlit sv
specB din'        sv = din
specB anil        sv = rlit ⟨⟩
specB (acons a b) sv = rcons (specB a sv) (specB b sv)
specB (ahd a)     sv = rhd (specB a sv)
specB (atl a)     sv = rtl (specB a sv)

-- the residual body, run once, equals one source iteration.
body-step : ∀ b sv dv → rev (specB b sv) dv ≡ aev b sv dv
body-step sin         sv dv = refl
body-step din'        sv dv = refl
body-step anil        sv dv = refl
body-step (acons a b) sv dv rewrite body-step a sv dv | body-step b sv dv = refl
body-step (ahd a)     sv dv rewrite body-step a sv dv = refl
body-step (atl a)     sv dv rewrite body-step a sv dv = refl

------------------------------------------------------------------------
-- The loop, both ways: iterate while the dynamic store is a cons (non-nil).
-- `srcLoop` runs the source body; `resLoop` runs the residual (folded) body.
-- Fuel-bounded so both are total (the loop itself may not terminate).
------------------------------------------------------------------------

srcLoop : ℕ → ATm → Val → Val → Val
srcLoop zero    b sv dv = dv
srcLoop (suc n) b sv dv = ifc dv (srcLoop n b sv (aev b sv dv)) dv

resLoop : ℕ → RTm → Val → Val
resLoop zero    rb dv = dv
resLoop (suc n) rb dv = ifc dv (resLoop n rb (rev rb dv)) dv

-- the folded `while` equals the source loop, using the per-iteration obligation.
fold-sound : ∀ n b sv dv → resLoop n (specB b sv) dv ≡ srcLoop n b sv dv
fold-sound zero    b sv dv = refl
fold-sound (suc n) b sv dv with dv
... | ⟨⟩      = refl
... | (x ∙ y) rewrite body-step b sv (x ∙ y) = fold-sound n b sv (aev b sv (x ∙ y))

------------------------------------------------------------------------
-- Concrete check: the body `dv := tl dv` drains a list to nil; the folded loop
-- (residual body `rtl din`) computes the same as the source loop.
------------------------------------------------------------------------

list2 : Val
list2 = ⟨⟩ ∙ (⟨⟩ ∙ ⟨⟩)        -- [⟨⟩ , ⟨⟩]

_ : srcLoop 5 (atl din') ⟨⟩ list2 ≡ ⟨⟩
_ = refl

_ : resLoop 5 (specB (atl din') ⟨⟩) list2 ≡ ⟨⟩
_ = refl
