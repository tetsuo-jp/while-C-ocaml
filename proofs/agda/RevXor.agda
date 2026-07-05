{-# OPTIONS --safe #-}

------------------------------------------------------------------------
-- REVERSIBLE LEVIN, R1c (tree outputs): the PARTIAL reversible copy.
--
-- R1c's total-GF(2) Bennett (`RevCore.Bennett`) copies into a `Bool`/`Vec Bool n`
-- slot via a self-inverse xor.  Extending it to TREE-valued outputs (so that
-- `f := ⟦p⟧` for a classical `Exp p` can be Bennett-simulated) hits a real wall:
-- there is NO total involutive xor on binary trees `V` (from `⟨⟩ ⊕ v = v` and the
-- involution `(o ⊕ v) ⊕ v = o` one derives `v ⊕ v = ⟨⟩`, impossible for a `cons v`
-- without quotienting `⟨⟩ ∙ ⟨⟩ ∼ ⟨⟩`).
--
-- R-WHILE resolves this exactly as here: its `X ^= E` is a PARTIAL involution --
-- SET a blank (`⟨⟩`) slot to `v`, CLEAR a `v`-slot to `⟨⟩`, and ABORT otherwise
-- (`rwhile-C-ocaml/RWhileValStore.RAss-sym`, a partial involution).  On the domain
-- Bennett actually uses (blank before compute / `f x` before uncompute) it is
-- total and reversible, and needs no tree-xor.  So this is the tree-output twin of
-- `RevCore.Bennett`, faithful to R-WHILE's real reversible assignment.
------------------------------------------------------------------------

module RevXor where

open import Data.Bool using (Bool; true; false; _∧_; if_then_else_)
open import Data.Maybe using (Maybe; just; nothing; map)
open import Data.Product using (_×_; _,_; proj₂)
open import Relation.Binary.PropositionalEquality using (_≡_; refl; sym; cong; cong₂)

open import UniversalInterp using (V; ⟨⟩; _∙_)

------------------------------------------------------------------------
-- structural equality on trees
------------------------------------------------------------------------

eqV : V → V → Bool
eqV ⟨⟩      ⟨⟩      = true
eqV ⟨⟩      (_ ∙ _) = false
eqV (_ ∙ _) ⟨⟩      = false
eqV (a ∙ b) (c ∙ d) = eqV a c ∧ eqV b d

eqV-refl : (v : V) → eqV v v ≡ true
eqV-refl ⟨⟩      = refl
eqV-refl (a ∙ b) rewrite eqV-refl a | eqV-refl b = refl

∧-elim : {x y : Bool} → x ∧ y ≡ true → (x ≡ true) × (y ≡ true)
∧-elim {true} {true} _ = refl , refl
∧-elim {true} {false} ()
∧-elim {false} ()

eqV-sound : (a b : V) → eqV a b ≡ true → a ≡ b
eqV-sound ⟨⟩      ⟨⟩      _ = refl
eqV-sound ⟨⟩      (_ ∙ _) ()
eqV-sound (_ ∙ _) ⟨⟩      ()
eqV-sound (a ∙ b) (c ∙ d) e with ∧-elim e
... | (ea , eb) = cong₂ _∙_ (eqV-sound a c ea) (eqV-sound b d eb)

just-inj : {A : Set} {x y : A} → (just x ≡ just y) → x ≡ y
just-inj refl = refl

------------------------------------------------------------------------
-- the R-WHILE reversible slot update  out ^= w  (partial involution)
------------------------------------------------------------------------

xorSlot : V → V → Maybe V      -- xorSlot w o : xor value w into slot o
xorSlot ⟨⟩      o       = just o                              -- w = ⟨⟩ : identity
xorSlot (a ∙ b) ⟨⟩      = just (a ∙ b)                        -- blank slot: SET to w
xorSlot (a ∙ b) (c ∙ d) = if eqV (c ∙ d) (a ∙ b)
                            then just ⟨⟩                      -- slot = w : CLEAR
                            else nothing                      -- else: ABORT

-- xoring w into a blank slot always sets it to w
xorSlot-blank : (w : V) → xorSlot w ⟨⟩ ≡ just w
xorSlot-blank ⟨⟩      = refl
xorSlot-blank (a ∙ b) = refl

-- xoring w into a w-slot always clears it
xorSlot-same : (w : V) → xorSlot w w ≡ just ⟨⟩
xorSlot-same ⟨⟩      = refl
xorSlot-same (a ∙ b) rewrite eqV-refl (a ∙ b) = refl

-- PARTIAL INVOLUTION: whatever xorSlot does, doing it again undoes it.
xorSlot-invol : (w o o' : V) → xorSlot w o ≡ just o' → xorSlot w o' ≡ just o
xorSlot-invol ⟨⟩ o o' eq rewrite just-inj eq = refl
xorSlot-invol (a ∙ b) ⟨⟩ o' eq rewrite sym (just-inj eq) = xorSlot-same (a ∙ b)
xorSlot-invol (a ∙ b) (c ∙ d) o' eq with eqV (c ∙ d) (a ∙ b) in eqcd
... | true  rewrite sym (just-inj eq) = cong just (sym (eqV-sound (c ∙ d) (a ∙ b) eqcd))
... | false with () ← eq

------------------------------------------------------------------------
-- BENNETT for a TREE-valued function f : the store is (input , output slot);
-- `bcopy f` xors f(input) into the slot.  From a blank slot it computes
-- (x , f x) keeping x; on (x , f x) it uncomputes back to (x , ⟨⟩).
------------------------------------------------------------------------

bcopy : (V → V) → (V × V) → Maybe (V × V)
bcopy f (i , o) = map (λ o' → (i , o')) (xorSlot (f i) o)

-- COMPUTE: from a blank output, produce (x , f x), input preserved (garbage = x)
bennett-tree : (f : V → V) (x : V) → bcopy f (x , ⟨⟩) ≡ just (x , f x)
bennett-tree f x rewrite xorSlot-blank (f x) = refl

-- UNCOMPUTE: on (x , f x), clear the output back to blank
bcopy-uncompute : (f : V → V) (x : V) → bcopy f (x , f x) ≡ just (x , ⟨⟩)
bcopy-uncompute f x rewrite xorSlot-same (f x) = refl

-- extract the slot equation from the paired one
bcopy-slot : (f : V → V) (i o o' : V)
  → bcopy f (i , o) ≡ just (i , o') → xorSlot (f i) o ≡ just o'
bcopy-slot f i o o' e with xorSlot (f i) o
... | just x = cong just (cong proj₂ (just-inj e))
... | nothing with () ← e

-- REVERSIBILITY: bcopy is a partial involution (from xorSlot-invol)
bcopy-invol : (f : V → V) (i o o' : V)
  → bcopy f (i , o) ≡ just (i , o') → bcopy f (i , o') ≡ just (i , o)
bcopy-invol f i o o' eq rewrite xorSlot-invol (f i) o o' (bcopy-slot f i o o' eq) = refl

------------------------------------------------------------------------
-- Concrete: Bennett-copy the RESULT of a real classical `Exp` program.
-- With `f x = ⟦p⟧(x)` (at enough fuel), bcopy reversibly logs p's output while
-- keeping p's input -- Bennett applied to an actual object-language computation.
------------------------------------------------------------------------

open import UniversalInterp using (Exp; var; khd; module Interp)
import UniversalInterp as U   -- its own Maybe (distinct from Data.Maybe)

-- ⟦p⟧ as a total function, defaulting to ⟨⟩ off its domain (fuel 6)
sem : Exp → (V → V)
sem p x with Interp.eval p 6 p x
... | U.just v  = v
... | U.nothing = ⟨⟩

-- p = khd var  (head).  On a 2-list, bcopy logs its head, keeping the list.
_ : bcopy (sem (khd var)) ((⟨⟩ ∙ ⟨⟩) ∙ ((⟨⟩ ∙ ⟨⟩) ∙ ⟨⟩) , ⟨⟩)
      ≡ just ((⟨⟩ ∙ ⟨⟩) ∙ ((⟨⟩ ∙ ⟨⟩) ∙ ⟨⟩) , ⟨⟩ ∙ ⟨⟩)
_ = refl
-- and it uncomputes
_ : bcopy (sem (khd var)) ((⟨⟩ ∙ ⟨⟩) ∙ ((⟨⟩ ∙ ⟨⟩) ∙ ⟨⟩) , ⟨⟩ ∙ ⟨⟩)
      ≡ just ((⟨⟩ ∙ ⟨⟩) ∙ ((⟨⟩ ∙ ⟨⟩) ∙ ⟨⟩) , ⟨⟩)
_ = refl
