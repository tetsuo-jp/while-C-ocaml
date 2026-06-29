{-# OPTIONS --safe #-}
------------------------------------------------------------------------
-- PROGRAMS-AS-DATA for the universal interpreter (a step toward G4).
--
-- The remaining G4 construction (write the specializer `mix` as an object
-- program and prove `run ⌜mix⌝ (⌜p⌝ , s) ≡ just ⌜spec p s⌝`) needs the object
-- language's programs to be DATA -- an encoding `⌜_⌝ : Exp → V` together with a
-- decoder, the in-Agda counterpart of `DesugarWhile --data`'s ⌜p⌝.  A
-- self-interpreter / specializer written in `Exp` consumes such a ⌜p⌝.
--
-- This module supplies that encoding and proves it FAITHFUL: `decode` is a left
-- inverse of `⌜_⌝` (`decode ⌜e⌝ ≡ just e`), so no information is lost encoding a
-- program as data.  Tags are unary numerals built from `⟨⟩`/`_∙_`; a code is
-- `(tag . arguments)`.
--
-- Referenced by docs/AGDA_CORRESPONDENCE.md (gap G4).
------------------------------------------------------------------------

module ProgEncoding where

open import Data.Nat using (ℕ; zero; suc)
open import Relation.Binary.PropositionalEquality using (_≡_; refl)
open import UniversalInterp
  using ( V; ⟨⟩; _∙_
        ; Exp; var; knil; kcons; khd; ktl; kifnil; call
        ; Maybe; nothing; just; _>>=_ )

------------------------------------------------------------------------
-- Encoding: ⌜ e ⌝ = (tag . arguments), tags are unary numerals.
------------------------------------------------------------------------

tag : ℕ → V
tag zero    = ⟨⟩
tag (suc n) = ⟨⟩ ∙ tag n

⌜_⌝ : Exp → V
⌜ var ⌝          = tag 0 ∙ ⟨⟩
⌜ knil ⌝         = tag 1 ∙ ⟨⟩
⌜ kcons a b ⌝    = tag 2 ∙ (⌜ a ⌝ ∙ ⌜ b ⌝)
⌜ khd e ⌝        = tag 3 ∙ ⌜ e ⌝
⌜ ktl e ⌝        = tag 4 ∙ ⌜ e ⌝
⌜ kifnil c t e ⌝ = tag 5 ∙ (⌜ c ⌝ ∙ (⌜ t ⌝ ∙ ⌜ e ⌝))
⌜ call e ⌝       = tag 6 ∙ ⌜ e ⌝

------------------------------------------------------------------------
-- Decoding: structural on the (tag . arguments) shape; invalid codes ⇒ nothing.
-- Recurses on the strictly-smaller argument sub-values, so it is total.
------------------------------------------------------------------------

decode : V → Maybe Exp
-- var: tag 0 = ⟨⟩
decode (⟨⟩ ∙ ⟨⟩)                                                          = just var
-- knil: tag 1
decode ((⟨⟩ ∙ ⟨⟩) ∙ ⟨⟩)                                                  = just knil
-- kcons: tag 2, args = (a ∙ b)
decode ((⟨⟩ ∙ (⟨⟩ ∙ ⟨⟩)) ∙ (va ∙ vb))                                     =
  decode va >>= λ a → decode vb >>= λ b → just (kcons a b)
-- khd: tag 3
decode ((⟨⟩ ∙ (⟨⟩ ∙ (⟨⟩ ∙ ⟨⟩))) ∙ ve)                                     =
  decode ve >>= λ e → just (khd e)
-- ktl: tag 4
decode ((⟨⟩ ∙ (⟨⟩ ∙ (⟨⟩ ∙ (⟨⟩ ∙ ⟨⟩)))) ∙ ve)                              =
  decode ve >>= λ e → just (ktl e)
-- kifnil: tag 5, args = (c ∙ (t ∙ e))
decode ((⟨⟩ ∙ (⟨⟩ ∙ (⟨⟩ ∙ (⟨⟩ ∙ (⟨⟩ ∙ ⟨⟩))))) ∙ (vc ∙ (vt ∙ ve)))         =
  decode vc >>= λ c → decode vt >>= λ t → decode ve >>= λ e → just (kifnil c t e)
-- call: tag 6
decode ((⟨⟩ ∙ (⟨⟩ ∙ (⟨⟩ ∙ (⟨⟩ ∙ (⟨⟩ ∙ (⟨⟩ ∙ ⟨⟩)))))) ∙ ve)                =
  decode ve >>= λ e → just (call e)
-- anything else is not a valid program code.
decode _                                                                  = nothing

------------------------------------------------------------------------
-- Faithfulness: decode is a left inverse of the encoding.
------------------------------------------------------------------------

decode-⌜⌝ : ∀ e → decode ⌜ e ⌝ ≡ just e
decode-⌜⌝ var = refl
decode-⌜⌝ knil = refl
decode-⌜⌝ (kcons a b) rewrite decode-⌜⌝ a | decode-⌜⌝ b = refl
decode-⌜⌝ (khd e)     rewrite decode-⌜⌝ e = refl
decode-⌜⌝ (ktl e)     rewrite decode-⌜⌝ e = refl
decode-⌜⌝ (kifnil c t e) rewrite decode-⌜⌝ c | decode-⌜⌝ t | decode-⌜⌝ e = refl
decode-⌜⌝ (call e)    rewrite decode-⌜⌝ e = refl

------------------------------------------------------------------------
-- A couple of concrete encode/decode round-trips (the "unit tests").
------------------------------------------------------------------------

_ : decode ⌜ kcons (khd var) (ktl var) ⌝ ≡ just (kcons (khd var) (ktl var))
_ = refl

_ : decode ⌜ kifnil var knil (call var) ⌝ ≡ just (kifnil var knil (call var))
_ = refl
