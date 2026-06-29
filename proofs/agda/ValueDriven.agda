{-# OPTIONS --safe #-}
------------------------------------------------------------------------
-- VALUE-DRIVEN CONTROL (gap row 20): static guards resolve at spec time.
--
-- `Cogen.ml --online` is value-driven: when an `if`/`while` guard is fully
-- STATIC (an `Sv`/`Cv` annotated value), the specializer evaluates it at
-- specialization time and resolves the control flow — selecting the branch
-- (unrolling the loop) and emitting NO residual conditional.  Only a DYNAMIC
-- guard is residualized.  This was differential (`test-poly`/`test-jones`/
-- `test-usi`/`test-bench`); here it is pinned as a `--safe` design spec.
--
-- Model: a source with two conditional forms — `aifS` (guard is a STATIC
-- expression `STm`, over the static input only) and `aifD` (guard is a dynamic
-- `ATm`).  The specializer `spec`:
--   * `aifS` — evaluates the guard now (`sev g sv`) and returns the CHOSEN
--     branch's residual, with no `rif` node (value-driven resolution);
--   * `aifD` — emits a residual conditional `rif`.
-- Proven: `spec-correct` (the residual computes the source meaning in both
-- cases) and `static-resolves` (a static-guard residual is literally one
-- branch's residual — never a conditional, i.e. control was resolved at spec
-- time, residualizing nothing).
--
-- Referenced by docs/AGDA_CORRESPONDENCE.md (row 20).
------------------------------------------------------------------------

module ValueDriven where

open import Data.Sum using (_⊎_; inj₁; inj₂)
open import Relation.Binary.PropositionalEquality using (_≡_; refl)

------------------------------------------------------------------------
-- Values, total head/tail, and the guard test (nil ⇒ else, cons ⇒ then).
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
ifc ⟨⟩      t e = e        -- nil  ⇒ else
ifc (_ ∙ _) t e = t        -- cons ⇒ then

------------------------------------------------------------------------
-- Static expressions (the guard of a static `if`): only the static input.
------------------------------------------------------------------------

data STm : Set where
  s-in  : STm
  s-nil : STm
  s-cons : STm → STm → STm
  s-hd  : STm → STm
  s-tl  : STm → STm

sev : STm → Val → Val
sev s-in        sv = sv
sev s-nil       sv = ⟨⟩
sev (s-cons a b) sv = sev a sv ∙ sev b sv
sev (s-hd a)    sv = hd (sev a sv)
sev (s-tl a)    sv = tl (sev a sv)

------------------------------------------------------------------------
-- Residual / target language, with a residual conditional `rif`.
------------------------------------------------------------------------

data RTm : Set where
  din   : RTm
  rcons : RTm → RTm → RTm
  rhd   : RTm → RTm
  rtl   : RTm → RTm
  rlit  : Val → RTm
  rif   : RTm → RTm → RTm → RTm

rev : RTm → Val → Val
rev din         dv = dv
rev (rcons a b) dv = rev a dv ∙ rev b dv
rev (rhd r)     dv = hd (rev r dv)
rev (rtl r)     dv = tl (rev r dv)
rev (rlit v)    dv = v
rev (rif g t e) dv = ifc (rev g dv) (rev t dv) (rev e dv)

------------------------------------------------------------------------
-- Annotated source with static (`aifS`) and dynamic (`aifD`) conditionals.
------------------------------------------------------------------------

data ATm : Set where
  sin   : ATm
  din'  : ATm
  anil  : ATm
  acons : ATm → ATm → ATm
  ahd   : ATm → ATm
  atl   : ATm → ATm
  aifS  : STm → ATm → ATm → ATm    -- static guard
  aifD  : ATm → ATm → ATm → ATm    -- dynamic guard

aev : ATm → Val → Val → Val
aev sin          sv dv = sv
aev din'         sv dv = dv
aev anil         sv dv = ⟨⟩
aev (acons a b)  sv dv = aev a sv dv ∙ aev b sv dv
aev (ahd a)      sv dv = hd (aev a sv dv)
aev (atl a)      sv dv = tl (aev a sv dv)
aev (aifS g b1 b2) sv dv = ifc (sev g sv)      (aev b1 sv dv) (aev b2 sv dv)
aev (aifD g b1 b2) sv dv = ifc (aev g sv dv)   (aev b1 sv dv) (aev b2 sv dv)

------------------------------------------------------------------------
-- The value-driven specializer.
------------------------------------------------------------------------

spec : ATm → Val → RTm
spec sin          sv = rlit sv
spec din'         sv = din
spec anil         sv = rlit ⟨⟩
spec (acons a b)  sv = rcons (spec a sv) (spec b sv)
spec (ahd a)      sv = rhd (spec a sv)
spec (atl a)      sv = rtl (spec a sv)
spec (aifS g b1 b2) sv = ifc (sev g sv) (spec b1 sv) (spec b2 sv)  -- resolve NOW; no rif
spec (aifD g b1 b2) sv = rif (spec g sv) (spec b1 sv) (spec b2 sv) -- residualize

------------------------------------------------------------------------
-- Soundness: the residual computes the source meaning (both guard kinds).
------------------------------------------------------------------------

spec-correct : ∀ t sv dv → rev (spec t sv) dv ≡ aev t sv dv
spec-correct sin         sv dv = refl
spec-correct din'        sv dv = refl
spec-correct anil        sv dv = refl
spec-correct (acons a b) sv dv
  rewrite spec-correct a sv dv | spec-correct b sv dv = refl
spec-correct (ahd a)     sv dv rewrite spec-correct a sv dv = refl
spec-correct (atl a)     sv dv rewrite spec-correct a sv dv = refl
spec-correct (aifS g b1 b2) sv dv with sev g sv
... | ⟨⟩      = spec-correct b2 sv dv     -- guard nil ⇒ else branch chosen at spec time
... | (_ ∙ _) = spec-correct b1 sv dv     -- guard cons ⇒ then branch chosen at spec time
spec-correct (aifD g b1 b2) sv dv
  rewrite spec-correct g sv dv | spec-correct b1 sv dv | spec-correct b2 sv dv = refl

------------------------------------------------------------------------
-- The value-driven property: a STATIC guard resolves at spec time — the
-- residual is exactly ONE branch's residual, never a residual conditional.
------------------------------------------------------------------------

static-resolves : ∀ g b1 b2 sv →
  (spec (aifS g b1 b2) sv ≡ spec b1 sv) ⊎ (spec (aifS g b1 b2) sv ≡ spec b2 sv)
static-resolves g b1 b2 sv with sev g sv
... | ⟨⟩      = inj₂ refl
... | (_ ∙ _) = inj₁ refl

------------------------------------------------------------------------
-- Concrete checks.
------------------------------------------------------------------------

-- a static-guard `if` collapses to a branch residual (no `rif` emitted):
-- guard `s-in` at sv = (⟨⟩∙⟨⟩) is cons ⇒ then branch `acons din' din'`.
_ : spec (aifS s-in (acons din' din') anil) (⟨⟩ ∙ ⟨⟩) ≡ rcons din din
_ = refl

-- a dynamic-guard `if` IS residualized as a conditional.
_ : spec (aifD din' anil din') ⟨⟩ ≡ rif din (rlit ⟨⟩) din
_ = refl
