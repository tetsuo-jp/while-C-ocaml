{-# OPTIONS --safe #-}
------------------------------------------------------------------------
-- GENERATING-EXTENSION SOUNDNESS (gap G3, the `--gen` / offline cogen output).
--
-- `Cogen.ml --gen` produces a generating extension: a program `gen` such that
-- `[gen](s)` EMITS a residual/target program with `[ [gen](s) ](d) = [p]((s,d))`.
-- Its correctness was differential-only (`test-genext`, `test-cogen`).  This
-- module pins it as a `--safe` design spec.
--
-- The model has the generating-extension SHAPE — distinct from `FutamuraSmn`,
-- whose residual stayed in the source language: here the residual lives in a
-- SEPARATE target language `RTm` (one dynamic input), and `genext : ATm → Val →
-- RTm` maps an annotated two-input source + a static value to emitted target
-- code.  Static structure is computed away at genext time (the OFFLINE
-- optimisation): the static input folds to a literal, and constant subexpressions
-- collapse via the smart constructors `rcons'`/`rhd'`/`rtl'`.  Soundness shows the
-- emitted target, run on the dynamic input, reproduces the source on `(sv , dv)`.
--
-- Referenced by docs/AGDA_CORRESPONDENCE.md (gap G3).
------------------------------------------------------------------------

module GenextSound where

open import Relation.Binary.PropositionalEquality using (_≡_; refl)

------------------------------------------------------------------------
-- Values, with total head/tail.
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

------------------------------------------------------------------------
-- Target / residual language: one DYNAMIC input `din`, plus literals `rlit`
-- (how the genext bakes computed static values into emitted code).
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

-- smart constructors: fold when the operands are already literals (constant
-- folding = the offline optimisation that keeps the residual small).
rcons' : RTm → RTm → RTm
rcons' (rlit a) (rlit b) = rlit (a ∙ b)
rcons' a        b        = rcons a b

rhd' : RTm → RTm
rhd' (rlit v) = rlit (hd v)
rhd' r        = rhd r

rtl' : RTm → RTm
rtl' (rlit v) = rlit (tl v)
rtl' r        = rtl r

-- the smart constructors preserve meaning.
rcons'-sound : ∀ a b dv → rev (rcons' a b) dv ≡ (rev a dv ∙ rev b dv)
rcons'-sound (rlit a) (rlit b)    dv = refl
rcons'-sound (rlit a) din         dv = refl
rcons'-sound (rlit a) (rcons x y) dv = refl
rcons'-sound (rlit a) (rhd r)     dv = refl
rcons'-sound (rlit a) (rtl r)     dv = refl
rcons'-sound din      b           dv = refl
rcons'-sound (rcons x y) b        dv = refl
rcons'-sound (rhd r)  b           dv = refl
rcons'-sound (rtl r)  b           dv = refl

rhd'-sound : ∀ r dv → rev (rhd' r) dv ≡ hd (rev r dv)
rhd'-sound din         dv = refl
rhd'-sound (rcons a b) dv = refl
rhd'-sound (rhd r)     dv = refl
rhd'-sound (rtl r)     dv = refl
rhd'-sound (rlit v)    dv = refl

rtl'-sound : ∀ r dv → rev (rtl' r) dv ≡ tl (rev r dv)
rtl'-sound din         dv = refl
rtl'-sound (rcons a b) dv = refl
rtl'-sound (rhd r)     dv = refl
rtl'-sound (rtl r)     dv = refl
rtl'-sound (rlit v)    dv = refl

------------------------------------------------------------------------
-- Annotated source: two inputs, the STATIC `sin` and the DYNAMIC `din'`.
-- (The annotation is which input each leaf reads; the BTA division.)
------------------------------------------------------------------------

data ATm : Set where
  sin   : ATm                  -- static input
  din'  : ATm                  -- dynamic input
  anil  : ATm
  acons : ATm → ATm → ATm
  ahd   : ATm → ATm
  atl   : ATm → ATm

-- two-input source semantics.
aev : ATm → Val → Val → Val
aev sin         sv dv = sv
aev din'        sv dv = dv
aev anil        sv dv = ⟨⟩
aev (acons a b) sv dv = aev a sv dv ∙ aev b sv dv
aev (ahd a)     sv dv = hd (aev a sv dv)
aev (atl a)     sv dv = tl (aev a sv dv)

------------------------------------------------------------------------
-- The generating extension: emit target code for a static input value.
------------------------------------------------------------------------

genext : ATm → Val → RTm
genext sin         sv = rlit sv          -- fold the static input to a literal
genext din'        sv = din              -- the dynamic input stays as code
genext anil        sv = rlit ⟨⟩
genext (acons a b) sv = rcons' (genext a sv) (genext b sv)
genext (ahd a)     sv = rhd'  (genext a sv)
genext (atl a)     sv = rtl'  (genext a sv)

-- SOUNDNESS: the emitted target, run on the dynamic input, reproduces the source
-- run on the combined input (sv , dv).  `[ [gen](sv) ](dv) = [p]((sv,dv))`.
genext-sound : ∀ at sv dv → rev (genext at sv) dv ≡ aev at sv dv
genext-sound sin         sv dv = refl
genext-sound din'        sv dv = refl
genext-sound anil        sv dv = refl
genext-sound (acons a b) sv dv
  rewrite rcons'-sound (genext a sv) (genext b sv) dv
        | genext-sound a sv dv | genext-sound b sv dv = refl
genext-sound (ahd a)     sv dv
  rewrite rhd'-sound (genext a sv) dv | genext-sound a sv dv = refl
genext-sound (atl a)     sv dv
  rewrite rtl'-sound (genext a sv) dv | genext-sound a sv dv = refl

------------------------------------------------------------------------
-- Concrete checks (the "unit tests").
------------------------------------------------------------------------

c : Val
c = ⟨⟩ ∙ ⟨⟩

-- a FULLY-static computation `hd (cons sin nil)` is folded to a literal at genext
-- time: the residual is just `rlit sv` (no operations left) — the offline payoff.
_ : genext (ahd (acons sin anil)) c ≡ rlit c
_ = refl

-- a mixed computation keeps the dynamic part as code: `cons din' sin` ↦
-- `rcons din (rlit sv)`, which on dynamic d computes (d , sv).
_ : rev (genext (acons din' sin) c) (⟨⟩ ∙ ⟨⟩) ≡ ((⟨⟩ ∙ ⟨⟩) ∙ c)
_ = refl
