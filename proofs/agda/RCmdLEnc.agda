{-# OPTIONS --safe #-}

------------------------------------------------------------------------
-- Reversible Levin R3c step 2 (brick 2): the ENCODING of the loop language
-- `RCmdL.P` as data -- the `ProgEncoding` / `RevInst.⌜_⌝` twin for the language
-- `rsint` is written in.
--
-- `⌜_⌝ : P → V` (and `⌜_⌝g : GCode → V`) tag each constructor and recurse; atoms
-- reuse `RevInst.⌜_⌝`.  `decode` is a left inverse: `decode ⌜ p ⌝ ≡ just p`
-- (`decode-⌜⌝`), so `P` is genuinely encodable.  This is what makes the reversible
-- self-interpreter SELF-APPLICABLE: `rsint : P` will consume `⌜ p ⌝` for `p : P`
-- its own language, exactly as Levin/Futamura self-application needs.
------------------------------------------------------------------------

module RCmdLEnc where

open import Data.Maybe using (Maybe; just; nothing)
open import Relation.Binary.PropositionalEquality using (_≡_; refl)
open import UniversalInterp using (V; ⟨⟩; _∙_)
import RevInst as R
open import RCmdL using (P; pOp; pSeq; pCond; pLoop; GCode; gNil; gHd; gTl)

------------------------------------------------------------------------
-- Guard encoding (tags 0/1/2, like RevInst's swp/sq/ifn).

⌜_⌝g : GCode → V
⌜ gNil  ⌝g = ⟨⟩
⌜ gHd g ⌝g = (⟨⟩ ∙ ⟨⟩) ∙ ⌜ g ⌝g
⌜ gTl g ⌝g = ((⟨⟩ ∙ ⟨⟩) ∙ ⟨⟩) ∙ ⌜ g ⌝g

decodeG : V → Maybe GCode
decodeG ⟨⟩ = just gNil
decodeG ((⟨⟩ ∙ ⟨⟩) ∙ g) with decodeG g
... | just g' = just (gHd g')
... | nothing = nothing
decodeG (((⟨⟩ ∙ ⟨⟩) ∙ ⟨⟩) ∙ g) with decodeG g
... | just g' = just (gTl g')
... | nothing = nothing
decodeG _ = nothing

decodeG-⌜⌝g : (g : GCode) → decodeG ⌜ g ⌝g ≡ just g
decodeG-⌜⌝g gNil     = refl
decodeG-⌜⌝g (gHd g) rewrite decodeG-⌜⌝g g = refl
decodeG-⌜⌝g (gTl g) rewrite decodeG-⌜⌝g g = refl

------------------------------------------------------------------------
-- Program encoding (tags 0/1/2/3 for pOp/pSeq/pCond/pLoop); atoms reuse
-- RevInst.⌜_⌝, guards reuse ⌜_⌝g.

⌜_⌝ : P → V
⌜ pOp c ⌝         = ⟨⟩ ∙ R.⌜ c ⌝
⌜ pSeq a b ⌝      = (⟨⟩ ∙ ⟨⟩) ∙ (⌜ a ⌝ ∙ ⌜ b ⌝)
⌜ pCond e a b f ⌝ = ((⟨⟩ ∙ ⟨⟩) ∙ ⟨⟩) ∙ (⌜ e ⌝g ∙ (⌜ a ⌝ ∙ (⌜ b ⌝ ∙ ⌜ f ⌝g)))
⌜ pLoop e D L f ⌝ = ((⟨⟩ ∙ ⟨⟩) ∙ (⟨⟩ ∙ ⟨⟩)) ∙ (⌜ e ⌝g ∙ (⌜ D ⌝ ∙ (⌜ L ⌝ ∙ ⌜ f ⌝g)))

decode : V → Maybe P
decode (⟨⟩ ∙ v) with R.decode v
... | just c  = just (pOp c)
... | nothing = nothing
decode ((⟨⟩ ∙ ⟨⟩) ∙ (a ∙ b)) with decode a | decode b
... | just a' | just b' = just (pSeq a' b')
... | _       | _       = nothing
decode (((⟨⟩ ∙ ⟨⟩) ∙ ⟨⟩) ∙ (e ∙ (a ∙ (b ∙ f)))) with decodeG e | decode a | decode b | decodeG f
... | just e' | just a' | just b' | just f' = just (pCond e' a' b' f')
... | _       | _       | _       | _       = nothing
decode (((⟨⟩ ∙ ⟨⟩) ∙ (⟨⟩ ∙ ⟨⟩)) ∙ (e ∙ (d ∙ (l ∙ f)))) with decodeG e | decode d | decode l | decodeG f
... | just e' | just d' | just l' | just f' = just (pLoop e' d' l' f')
... | _       | _       | _       | _       = nothing
decode _ = nothing

decode-⌜⌝ : (p : P) → decode ⌜ p ⌝ ≡ just p
decode-⌜⌝ (pOp c)         rewrite R.decode-⌜⌝ c = refl
decode-⌜⌝ (pSeq a b)      rewrite decode-⌜⌝ a | decode-⌜⌝ b = refl
decode-⌜⌝ (pCond e a b f) rewrite decodeG-⌜⌝g e | decode-⌜⌝ a | decode-⌜⌝ b | decodeG-⌜⌝g f = refl
decode-⌜⌝ (pLoop e d l f) rewrite decodeG-⌜⌝g e | decode-⌜⌝ d | decode-⌜⌝ l | decodeG-⌜⌝g f = refl

------------------------------------------------------------------------
-- Test-first: a concrete program round-trips (all refl).

module Examples where
  prog : P
  prog = pLoop (gHd gNil) (pOp R.swp) (pSeq (pOp R.swp) (pOp R.swp)) (gTl gNil)

  ex-roundtrip : decode ⌜ prog ⌝ ≡ just prog
  ex-roundtrip = decode-⌜⌝ prog

  ex-guard : decodeG ⌜ gTl (gHd gNil) ⌝g ≡ just (gTl (gHd gNil))
  ex-guard = refl
