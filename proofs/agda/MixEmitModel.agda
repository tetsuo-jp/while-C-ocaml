{-# OPTIONS --safe #-}
------------------------------------------------------------------------
-- Design spec for gap #1 (dynamic-while residualisation) of mix.while:
-- the FOLD model is unsound under variable re-materialisation, the EMIT
-- model is sound.  Referenced by docs/FUTAMURA.md.
--
-- Context.  mix.while currently FOLDS a dynamic value as a residual
-- expression that references OTHER source variables, e.g. specialising
-- `X := In` records X's slot as the code `(var In)`.  For straight-line,
-- assemble-at-the-end fp1 this is fine.  But residualising a dynamic loop
-- needs DYNAMICIZE-ALL: it materialises the store by emitting `In := …`,
-- i.e. it REASSIGNS In at run time.  Then X's slot code `(var In)` reads the
-- NEW In — a STALE cross-reference — so the residual is wrong.
--
-- The fix (the "emit-on-dynamic-assignment" model): on a dynamic assignment,
-- EMIT `(var k) := <code>` into the residual AND set the slot to the
-- SELF-reference `(var k)`.  Then no slot references another variable, so a
-- later re-materialisation of any other variable cannot corrupt it.
--
-- We model variables, a runtime store, and residual code that references
-- variables, and prove:
--   * emit-sound : a self-referential slot is STABLE under updates to other
--                  variables — it still reads the value materialised into it;
--   * fold-tracks-new + fold-unsound :
--                  a cross-referential slot tracks the REASSIGNED variable, so
--                  after In := w it reads w, not the intended old value of In.
------------------------------------------------------------------------

module MixEmitModel where

open import Relation.Binary.PropositionalEquality using (_≡_; refl; sym; trans; cong)
open import Relation.Nullary using (Dec; yes; no; ¬_)
open import Data.Empty using (⊥-elim)

-- Two distinct source variables suffice to exhibit the cross-reference bug.
data Var : Set where
  vIn vX : Var

_≟_ : (x y : Var) → Dec (x ≡ y)
vIn ≟ vIn = yes refl
vIn ≟ vX  = no (λ ())
vX  ≟ vIn = no (λ ())
vX  ≟ vX  = yes refl

module Model (V : Set) where

  -- Runtime store and residual-expression code (mix's ('D . code) carries this).
  Store : Set
  Store = Var → V

  data Code : Set where
    cvar : Var → Code            -- (var k)

  eval : Code → Store → V
  eval (cvar x) σ = σ x

  update : Store → Var → V → Store
  update σ x v y with x ≟ y
  ... | yes _ = v
  ... | no  _ = σ y

  update-eq : ∀ σ x v → update σ x v x ≡ v
  update-eq σ x v with x ≟ x
  ... | yes _ = refl
  ... | no ¬p = ⊥-elim (¬p refl)

  update-neq : ∀ σ x y v → ¬ (x ≡ y) → update σ x v y ≡ σ y
  update-neq σ x y v x≢y with x ≟ y
  ... | yes p = ⊥-elim (x≢y p)
  ... | no  _ = refl

  ----------------------------------------------------------------------
  -- Scenario: specialise `X := In`, then later re-materialise In := w
  -- (DYNAMICIZE-ALL).  σ is the runtime store when X is specialised; the
  -- INTENDED value of X is the source semantics of `X := In`, i.e. σ vIn.

  intended : Store → V
  intended σ = σ vIn

  ------------------------------- FOLD model -------------------------------
  -- X's slot is the cross-reference code (var In).  After In := w the slot
  -- is read against the updated store.
  foldRead : Store → V → V
  foldRead σ w = eval (cvar vIn) (update σ vIn w)

  -- It tracks the REASSIGNED In, not the intended old value.
  fold-tracks-new : ∀ σ w → foldRead σ w ≡ w
  fold-tracks-new σ w = update-eq σ vIn w

  -- Hence whenever the materialised value w differs from In's old value,
  -- the fold residual is WRONG.
  fold-unsound : ∀ σ w → ¬ (w ≡ intended σ) → ¬ (foldRead σ w ≡ intended σ)
  fold-unsound σ w w≢ eq = w≢ (trans (sym (fold-tracks-new σ w)) eq)

  ------------------------------- EMIT model -------------------------------
  -- Emitting `X := In` materialises σ vIn into X, and sets X's slot to the
  -- SELF-reference (var X).  Later In := w updates a DIFFERENT variable.
  emitStore : Store → Store
  emitStore σ = update σ vX (σ vIn)               -- materialise X := In

  emitRead : Store → V → V
  emitRead σ w = eval (cvar vX) (update (emitStore σ) vIn w)   -- read X after In := w

  -- The self-reference is STABLE under the later In update and reads exactly
  -- the intended (old) value of In — sound.
  emit-sound : ∀ σ w → emitRead σ w ≡ intended σ
  emit-sound σ w =
    trans (update-neq (emitStore σ) vIn vX w (λ ()))   -- In := w doesn't touch vX
          (update-eq σ vX (σ vIn))                     -- vX holds the materialised σ vIn
