{-# OPTIONS --safe #-}
------------------------------------------------------------------------
-- THE FUTAMURA META-THEOREM FOR A PARTIAL `run`.
--
-- `FutamuraProjections` assumed a TOTAL `run : Data → Data → Data`.  A
-- Turing-complete object language (the setting `UniversalInterp` builds) has a
-- PARTIAL interpreter `run : Data → Data → Maybe Data` (programs may diverge),
-- so the meta-theorem must be restated up to that partiality.  This module does
-- that, closing the "Maybe-relative `mix-eq`" item of gap G4.
--
-- The specializer `mix` is assumed to TERMINATE on a program/static pair
-- (`mix-runs`: it returns `just (spec p s)`), which is exactly the well-known
-- requirement that a partial evaluator be total even though the programs it
-- specializes need not be.  The spec equation `mix-eq` then equates the
-- residual's (partial) behaviour with the source's (partial) behaviour as
-- `Maybe`-values — both diverge, or both converge to the same result.
--
-- From these two the three projections follow in their partial form:
--   fp1 — the residual computes the source meaning (Maybe-equality);
--   fp2 — the compiler RUNS (`≡ just …`) to the fp1 target;
--   fp3 — cogen RUNS to the fp2 compiler;
-- plus `fp2-correct`/`fp3-correct`: whatever the compiler / cogen actually
-- return is a correct target / compiler (via just-injectivity).
--
-- Referenced by docs/AGDA_CORRESPONDENCE.md (gap G4).
------------------------------------------------------------------------

module FutamuraPartial where

open import Relation.Binary.PropositionalEquality
  using (_≡_; refl; cong; trans; sym)

------------------------------------------------------------------------
-- A self-contained `Maybe` (partiality of the interpreter).
------------------------------------------------------------------------

data Maybe (A : Set) : Set where
  nothing : Maybe A
  just    : A → Maybe A

just-inj : ∀ {A} {x y : A} → just x ≡ just y → x ≡ y
just-inj refl = refl

------------------------------------------------------------------------
-- The partial mix model.
------------------------------------------------------------------------

record PartialMixModel : Set₁ where
  field
    Data : Set
    -- `run pgm inp` = [pgm](inp), now PARTIAL: it may be `nothing` (diverge).
    run  : Data → Data → Maybe Data
    pair : Data → Data → Data
    mix  : Data
    -- the residual `mix` produces (mix is assumed total: it always returns one).
    spec : Data → Data → Data
    -- mix TERMINATES on a (program, static) pair, yielding that residual.
    mix-runs : ∀ p s → run mix (pair p s) ≡ just (spec p s)
    -- the residual is correct, as Maybe-values (the partial spec equation).
    mix-eq   : ∀ p s d → run (spec p s) d ≡ run p (pair s d)

------------------------------------------------------------------------
-- The three projections, in their partial form.
------------------------------------------------------------------------

module Projections (M : PartialMixModel) where
  open PartialMixModel M

  -- First projection: `spec int source` is a target — running it on d computes
  -- (partially) what `int` computes on `(source , d)`.
  target : Data → Data → Data
  target int source = spec int source

  fp1 : ∀ int source d → run (target int source) d ≡ run int (pair source d)
  fp1 = mix-eq

  -- Second projection: `spec mix int` is a compiler — it RUNS (terminates) on a
  -- source to the fp1 target.
  compiler : Data → Data
  compiler int = spec mix int

  fp2 : ∀ int source → run (compiler int) source ≡ just (target int source)
  fp2 int source = trans (mix-eq mix int source) (mix-runs int source)

  -- whatever the compiler returns on a source is a CORRECT target.
  fp2-correct : ∀ int source d {t} →
                run (compiler int) source ≡ just t →
                run t d ≡ run int (pair source d)
  fp2-correct int source d {t} h
    rewrite just-inj (trans (sym h) (fp2 int source)) = fp1 int source d

  -- Third projection: `spec mix mix` is a cogen — it RUNS on an interpreter to
  -- the fp2 compiler.
  cogen : Data
  cogen = spec mix mix

  fp3 : ∀ int → run cogen int ≡ just (compiler int)
  fp3 int = trans (mix-eq mix mix int) (mix-runs mix int)

  -- whatever cogen returns is a CORRECT compiler (producing correct targets).
  fp3-correct : ∀ int {c} → run cogen int ≡ just c →
                ∀ source d {t} → run c source ≡ just t →
                run t d ≡ run int (pair source d)
  fp3-correct int {c} hc source d {t} ht
    rewrite just-inj (trans (sym hc) (fp3 int)) = fp2-correct int source d ht

------------------------------------------------------------------------
-- A genuinely-partial witness: running a non-program returns `nothing`, so the
-- partiality is real (not a `just`-wrapped total model).  `mix` builds a closure
-- capturing the static input; `mix-runs`/`mix-eq` hold by `refl`.
------------------------------------------------------------------------

module Witness where

  data D : Set where
    ⟨⟩  : D
    cns : D → D → D
    MIX : D
    clo : D → D → D

  run : D → D → Maybe D
  run MIX       (cns p s) = just (clo p s)     -- mix on a pair: freeze s
  run MIX       _         = nothing            -- mix on a non-pair: undefined
  run (clo p s) d         = run p (cns s d)     -- resume the closure on (s , d)
  run ⟨⟩        _         = nothing            -- data-as-program: undefined (real partiality)
  run (cns _ _) _         = nothing

  model : PartialMixModel
  model = record
    { Data     = D
    ; run      = run
    ; pair     = cns
    ; mix      = MIX
    ; spec     = clo
    ; mix-runs = λ p s → refl
    ; mix-eq   = λ p s d → refl
    }

  open Projections model

  -- fp2: the compiler runs to the fp1 target.
  _ : run (compiler MIX) MIX ≡ just (target MIX MIX)
  _ = refl

  -- fp3: cogen runs to the fp2 compiler.
  _ : run cogen MIX ≡ just (compiler MIX)
  _ = refl

  -- genuine partiality: a data value used as a program diverges (`nothing`).
  _ : run ⟨⟩ ⟨⟩ ≡ nothing
  _ = refl
