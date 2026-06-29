{-# OPTIONS --safe #-}
------------------------------------------------------------------------
-- G4, STEP 4a: `quoteV` -- lifting a static value into closed object code.
--
-- The remaining G4 construction is `mix` as an `Exp` program (steps 4b-4d).  A
-- specializer must FOLD a known static value into the residual program; but
-- `UniversalInterp.Exp` has no literal constructor.  So the first brick is a
-- `quote` that turns a value `v : V` into a CLOSED expression `quoteV v : Exp`
-- evaluating to `v` regardless of input -- the `Exp` counterpart of `FutamuraSmn`'s
-- `lit`.  Because `eval` is fuel-indexed, correctness is stated with sufficient
-- fuel (`suc (sz v)` nodes), discharged via fuel-monotonicity (`Mono.add-fuel`).
--
-- Roadmap (the rest of step 4, future rounds):
--   4b  `specE : Exp → V → Exp` -- specialize a program w.r.t. a static value,
--       residual reads the dynamic input, static input folded via `quoteV`;
--   4c  `mixProg : Exp` -- the metaprogram that, run on ⌜p⌝, emits ⌜specE p s⌝;
--   4d  assemble a concrete `FutamuraPartial.PartialMixModel` over `V`.
--
-- Referenced by docs/AGDA_CORRESPONDENCE.md (gap G4).
------------------------------------------------------------------------

module MixQuote where

open import Data.Nat using (ℕ; zero; suc; _+_)
open import Data.Nat.Properties using (+-comm; +-suc)
open import Relation.Binary.PropositionalEquality
  using (_≡_; refl; cong; trans; subst)
open import UniversalInterp
  using (V; ⟨⟩; _∙_; Exp; knil; kcons; just; module Interp; module Mono)

------------------------------------------------------------------------
-- quote a value into closed code, and a node-count fuel bound.
------------------------------------------------------------------------

quoteV : V → Exp
quoteV ⟨⟩      = knil
quoteV (a ∙ b) = kcons (quoteV a) (quoteV b)

sz : V → ℕ
sz ⟨⟩      = zero
sz (a ∙ b) = suc (sz a + sz b)

------------------------------------------------------------------------
-- Correctness: `quoteV v` evaluates to `v` (under any program, since it is
-- call-free), given `suc (sz v)` fuel.
------------------------------------------------------------------------

module _ (prog : Exp) where
  open Interp prog
  open Mono prog

  -- (mutually with the two fuel-raising helpers below)
  quote-ok : ∀ v x → eval (suc (sz v)) (quoteV v) x ≡ just v
  -- raise a child's fuel from `suc (sz child)` to `suc (sz a + sz b)`.
  raise-a : ∀ a b x → eval (suc (sz a + sz b)) (quoteV a) x ≡ just a
  raise-b : ∀ a b x → eval (suc (sz a + sz b)) (quoteV b) x ≡ just b

  quote-ok ⟨⟩      x = refl
  quote-ok (a ∙ b) x rewrite raise-a a b x | raise-b a b x = refl

  raise-a a b x = subst (λ f → eval f (quoteV a) x ≡ just a)
                    (trans (+-suc (sz b) (sz a)) (cong suc (+-comm (sz b) (sz a))))
                    (add-fuel (sz b) (suc (sz a)) (quoteV a) x (quote-ok a x))
  raise-b a b x = subst (λ f → eval f (quoteV b) x ≡ just b)
                    (+-suc (sz a) (sz b))
                    (add-fuel (sz a) (suc (sz b)) (quoteV b) x (quote-ok b x))

------------------------------------------------------------------------
-- Concrete check (with a fixed dummy program, since quoteV is call-free).
------------------------------------------------------------------------

module _ where
  open Interp knil
  _ : eval 2 (quoteV (⟨⟩ ∙ ⟨⟩)) ⟨⟩ ≡ just (⟨⟩ ∙ ⟨⟩)
  _ = refl
