{-# OPTIONS --safe #-}

------------------------------------------------------------------------
-- Reversible Levin Track-3 (brick P3a): reversible TREE-NAVIGATION atoms for the
-- self-interpreter program `rsint`.
--
-- The agenda machine (P2 `RevAgenda`) manipulates a stack state
-- `(todo , store)` -- but `RevInst.RCmd`'s only atom, root-subtree `swap`, cannot
-- navigate into `todo` to pop/expose its head.  The missing primitive is
-- reversible ROTATION (tree re-association).  Rotations are PARTIAL bijections
-- (defined only on the matching shape), so -- exactly as `RevXor` handles the
-- reversible `^=` -- they are modelled as a PARTIAL INVOLUTION pair `rotR`/`rotL`
-- (`Maybe`, `nothing` = abort on shape mismatch), plus the total self-inverse
-- `swapV`.  These are the reversible atoms `rsint` (P3c) is assembled from.
--
-- KEY navigation fact (`pop-exposes`): on a stack state `((code , rest) , store)`,
-- `rotR` brings `code` to the top-left -- exactly the agenda's "pop the head" step,
-- reversibly.  --safe, axiom-free.
------------------------------------------------------------------------

module RevRot where

open import Data.Maybe using (Maybe; just; nothing)
open import Relation.Binary.PropositionalEquality using (_≡_; refl)
open import UniversalInterp using (V; ⟨⟩; _∙_)

------------------------------------------------------------------------
-- swapV : the total, self-inverse root swap (⟨a,b⟩ ↦ ⟨b,a⟩).

swapV : V → V
swapV ⟨⟩      = ⟨⟩
swapV (a ∙ b) = b ∙ a

swapV-invol : (v : V) → swapV (swapV v) ≡ v
swapV-invol ⟨⟩      = refl
swapV-invol (a ∙ b) = refl

------------------------------------------------------------------------
-- rotR / rotL : the reversible re-association rotations, a PARTIAL involution
-- pair (nothing = abort on shape mismatch).

rotR : V → Maybe V              -- ⟨⟨a,b⟩,c⟩ ↦ ⟨a,⟨b,c⟩⟩
rotR ((a ∙ b) ∙ c) = just (a ∙ (b ∙ c))
rotR _             = nothing

rotL : V → Maybe V              -- ⟨a,⟨b,c⟩⟩ ↦ ⟨⟨a,b⟩,c⟩
rotL (a ∙ (b ∙ c)) = just ((a ∙ b) ∙ c)
rotL _             = nothing

-- round trips (partial involution): each rotation, where defined, is undone by
-- the other.
rotR-rotL : (v : V) {w : V} → rotR v ≡ just w → rotL w ≡ just v
rotR-rotL ((a ∙ b) ∙ c) refl = refl
rotR-rotL ⟨⟩            ()
rotR-rotL (⟨⟩ ∙ c)      ()

rotL-rotR : (v : V) {w : V} → rotL v ≡ just w → rotR w ≡ just v
rotL-rotR (a ∙ (b ∙ c)) refl = refl
rotL-rotR ⟨⟩            ()
rotL-rotR (a ∙ ⟨⟩)      ()

------------------------------------------------------------------------
-- NAVIGATION: `rotR` implements the agenda's "pop" -- on a stack state
-- ⟨⟨code,rest⟩,store⟩ it exposes `code` at the top-left (reversibly, via rotL).

pop-exposes : (code rest store : V)
  → rotR ((code ∙ rest) ∙ store) ≡ just (code ∙ (rest ∙ store))
pop-exposes code rest store = refl

pop-restore : (code rest store : V)
  → rotL (code ∙ (rest ∙ store)) ≡ just ((code ∙ rest) ∙ store)
pop-restore code rest store = refl

------------------------------------------------------------------------
-- Test-first: concrete rotations and their round trips.

module Examples where
  v0 : V
  v0 = (⟨⟩ ∙ (⟨⟩ ∙ ⟨⟩)) ∙ ⟨⟩          -- ⟨⟨a,b⟩,c⟩ shape

  ex-rotR : rotR v0 ≡ just (⟨⟩ ∙ ((⟨⟩ ∙ ⟨⟩) ∙ ⟨⟩))
  ex-rotR = refl

  ex-roundtrip : rotL (⟨⟩ ∙ ((⟨⟩ ∙ ⟨⟩) ∙ ⟨⟩)) ≡ just v0
  ex-roundtrip = rotR-rotL v0 refl

  ex-swap-invol : swapV (swapV v0) ≡ v0
  ex-swap-invol = refl
