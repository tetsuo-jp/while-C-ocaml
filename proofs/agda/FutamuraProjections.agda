{-# OPTIONS --safe #-}
------------------------------------------------------------------------
-- THE FUTAMURA PROJECTIONS AS A META-THEOREM.
--
-- The three Futamura projections are not three separate facts: they are
-- THREE INSTANCES of a single specification equation for `mix` (the
-- "mix equation" / Jones's spec equation, Computability and Complexity
-- p.105-):
--
--     [ [mix](p , s) ](d)  ≡  [p](s , d)        -- for all p, s, d
--
-- Writing `run pgm inp` for "[pgm](inp)" (a program text run on data) and
-- `spec p s = run mix (pair p s)` for "[mix](p,s)", the projections fall out
-- by choosing the two arguments of `mix`:
--
--   fp1 (compile)      : spec int    source   -- args (int , source)
--   fp2 (compiler)     : spec mix    int      -- args (mix , int)
--   fp3 (cogen)        : spec mix    mix       -- args (mix , mix)
--
-- This module makes that derivation a machine-checked theorem.  It is
-- parametrised over an ABSTRACT `MixModel` carrying only `run`, `pair`, `mix`
-- and the one equation `mix-eq`; from `mix-eq` alone it derives fp1/fp2/fp3
-- AND the composed correctness chain (cogen produces correct compilers that
-- produce correct targets).  The hypothesis `mix-eq` is exactly the semantic
-- content established, at the operational level, by the whole-spec congruence
-- `[cogen(p,s)](d) = [p]((s,d))` in SpecAssembly / SpecAssemblyIf /
-- SpecAssemblyWhile (gap G1).
--
-- Non-vacuity: `Witness.model` is a small but FAITHFUL self-applicable
-- specializer (mix captures the static input in a closure), discharging
-- `mix-eq` by `refl`, so the abstraction is inhabited by a non-degenerate
-- model and the derived theorems are not vacuous.
--
-- Referenced by docs/AGDA_CORRESPONDENCE.md.
------------------------------------------------------------------------

module FutamuraProjections where

open import Relation.Binary.PropositionalEquality
  using (_≡_; refl; cong; trans)

------------------------------------------------------------------------
-- The abstract mix model: one carrier, a universal `run`, a pairing, a
-- distinguished `mix` program text, and the spec equation.
------------------------------------------------------------------------

record MixModel : Set₁ where
  field
    Data : Set
    -- `run pgm inp` = [pgm](inp): run the program *text* `pgm` on input `inp`.
    run  : Data → Data → Data
    -- encode a pair of inputs into the single data domain.
    pair : Data → Data → Data
    -- the specializer's own program text.
    mix  : Data
    -- the mix equation: specializing p to static s, then running on d,
    -- equals running p on the combined input (s , d).
    mix-eq : ∀ (p s d : Data) →
      run (run mix (pair p s)) d ≡ run p (pair s d)

------------------------------------------------------------------------
-- The three projections, derived from `mix-eq` alone.
------------------------------------------------------------------------

module Projections (M : MixModel) where
  open MixModel M

  -- [mix](p , s): the residual program text produced by specialization.
  spec : Data → Data → Data
  spec p s = run mix (pair p s)

  ----------------------------------------------------------------------
  -- First projection (compilation): for an interpreter `int` and a source
  -- program `source`, `spec int source` is a TARGET program — running it on
  -- d computes what `int` computes on `(source , d)`.
  ----------------------------------------------------------------------
  target : Data → Data → Data
  target int source = spec int source

  fp1 : ∀ int source d →
        run (target int source) d ≡ run int (pair source d)
  fp1 int source d = mix-eq int source d

  ----------------------------------------------------------------------
  -- Second projection (compiler generation): `spec mix int` is a COMPILER —
  -- applied to a source it yields exactly the fp1 target, hence a correct
  -- compiled program.
  ----------------------------------------------------------------------
  compiler : Data → Data
  compiler int = spec mix int

  -- the compiler maps each source to its fp1 target.
  fp2 : ∀ int source →
        run (compiler int) source ≡ target int source
  fp2 int source = mix-eq mix int source

  -- composed: the compiled program is correct.
  fp2-correct : ∀ int source d →
        run (run (compiler int) source) d ≡ run int (pair source d)
  fp2-correct int source d =
    trans (cong (λ t → run t d) (fp2 int source)) (fp1 int source d)

  ----------------------------------------------------------------------
  -- Third projection (compiler-generator generation): `spec mix mix` is a
  -- COGEN — applied to an interpreter it yields exactly the fp2 compiler.
  ----------------------------------------------------------------------
  cogen : Data
  cogen = spec mix mix

  -- cogen turns each interpreter into its fp2 compiler.
  fp3 : ∀ int →
        run cogen int ≡ compiler int
  fp3 int = mix-eq mix mix int

  -- composed: cogen produces correct compilers that produce correct targets.
  fp3-correct : ∀ int source d →
        run (run (run cogen int) source) d ≡ run int (pair source d)
  fp3-correct int source d =
    trans (cong (λ c → run (run c source) d) (fp3 int))
          (fp2-correct int source d)

------------------------------------------------------------------------
-- A faithful, non-degenerate witness: a tiny self-applicable specializer.
--
-- `mix` applied to a pair ⟪p,s⟫ builds a CLOSURE `clo p s` that has frozen
-- the static input s; running that closure on d runs p on ⟪s,d⟫.  This is a
-- real (if minimal) instance of the spec equation, and it is self-applicable
-- (mix is just another datum), so cogen/compiler genuinely compute below.
------------------------------------------------------------------------

module Witness where

  data D : Set where
    nil   : D
    ⟪_,_⟫ : D → D → D
    MIX   : D
    clo   : D → D → D

  run : D → D → D
  run MIX       ⟪ p , s ⟫ = clo p s      -- mix on a pair: freeze s into a closure
  run MIX       x         = x            -- mix on a non-pair: identity (unused)
  run (clo p s) d         = run p ⟪ s , d ⟫
  run nil       d         = nil
  run ⟪ a , b ⟫ d         = ⟪ a , b ⟫    -- a pair-as-program is inert

  model : MixModel
  model = record
    { Data   = D
    ; run    = run
    ; pair   = ⟪_,_⟫
    ; mix    = MIX
    ; mix-eq = λ p s d → refl            -- the spec equation holds definitionally
    }

  open Projections model

  ----------------------------------------------------------------------
  -- Concrete reduction checks (the "unit tests": each holds by refl, so the
  -- type-checker validates that the derived projections actually compute).
  ----------------------------------------------------------------------

  -- pick a couple of opaque stand-ins for int / source / data.
  int₀ source₀ d₀ : D
  int₀    = ⟪ nil , nil ⟫        -- some interpreter text
  source₀ = nil                  -- some source text
  d₀      = ⟪ nil , ⟪ nil , nil ⟫ ⟫

  -- fp1 target is a closure that, run on d, reruns int on (source , d).
  _ : run (target int₀ source₀) d₀ ≡ run int₀ ⟪ source₀ , d₀ ⟫
  _ = refl

  -- fp2: the compiler applied to a source gives the fp1 target.
  _ : run (compiler int₀) source₀ ≡ target int₀ source₀
  _ = refl

  -- fp3: cogen is the closure clo MIX MIX, and applied to int gives the compiler.
  _ : cogen ≡ clo MIX MIX
  _ = refl

  _ : run cogen int₀ ≡ compiler int₀
  _ = refl

  -- the deep composition: cogen ▸ compiler ▸ target, all the way down to int.
  _ : run (run (run cogen int₀) source₀) d₀ ≡ run int₀ ⟪ source₀ , d₀ ⟫
  _ = refl
