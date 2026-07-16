{-# OPTIONS --safe #-}

------------------------------------------------------------------------
-- Reversible Levin Track-2 (brick S4, encoding side; traversal-charge plan
-- task 2): the ENCODING of the multi-variable language `RCmdV` as data --
-- the `RevInst.⌜_⌝` / `RCmdLEnc` twin for the language whose per-op cost
-- carries the length-L store walk (`RCmdVWalkCost`).
--
-- `⌜_⌝V : RCmdV → V` tags each constructor (0/1/2 = swpV/sqV/ifnV, the
-- RevInst tag scheme) and recurses; `decodeV` is a left inverse
-- (`decode-⌜⌝V`), so `RCmdV` is genuinely encodable -- the substrate the
-- store-carrying self-interpreter (`RevStoreInterp.usintV`, plan task 3)
-- consumes, and whose step count plan task 4 charges.
--
-- INDEX-DECODE CONVENTION (the risk flagged in the scoping note): variable
-- indices are UNARY NIL-LISTS -- `⌜ k ⌝ℕ` is the length-k list of ⟨⟩, so
-- READING an operand index k costs Θ(k).  This is legitimate *as* the
-- traversal charge only under k < L (walking the unary numeral for
-- variable k IS the walk to position k of the length-L variable list `Vl`;
-- main.tex §cost-model supports this reading: variables live on `Vl`).
-- Task 4 (`RevStoreSintCost`) must therefore either state the side
-- condition `vars c < length s` or fold the numeral walk into the store
-- walk; this module fixes the numeral representation those costs charge.
-- Two readers are provided: the strict `decodeℕ : V → Maybe ℕ` (rejects
-- non-numerals; used by `decodeV`) and the lenient total spine-counter
-- `unℕ : V → ℕ` (what a dispatching interpreter uses; agrees with
-- `decodeℕ` on well-formed numerals, `unℕ-⌜⌝ℕ`).  --safe, axiom-free.
------------------------------------------------------------------------

module RCmdVEnc where

open import Data.Nat using (ℕ; zero; suc)
open import Data.Maybe using (Maybe; just; nothing)
open import Relation.Binary.PropositionalEquality using (_≡_; refl; cong)
open import UniversalInterp using (V; ⟨⟩; _∙_)
open import RCmdV using (RCmdV; swpV; sqV; ifnV)
import RCmdV

------------------------------------------------------------------------
-- unary numerals: `⌜ k ⌝ℕ` = nil-list of length k (the Vl-position walk).

⌜_⌝ℕ : ℕ → V
⌜ zero ⌝ℕ  = ⟨⟩
⌜ suc n ⌝ℕ = ⟨⟩ ∙ ⌜ n ⌝ℕ

-- strict reader: rejects anything but a nil-list.
decodeℕ : V → Maybe ℕ
decodeℕ ⟨⟩ = just zero
decodeℕ (⟨⟩ ∙ n) with decodeℕ n
... | just n' = just (suc n')
... | nothing = nothing
decodeℕ _ = nothing

decodeℕ-⌜⌝ℕ : (n : ℕ) → decodeℕ ⌜ n ⌝ℕ ≡ just n
decodeℕ-⌜⌝ℕ zero              = refl
decodeℕ-⌜⌝ℕ (suc n) rewrite decodeℕ-⌜⌝ℕ n = refl

-- lenient TOTAL reader (interpreter side): counts the spine length,
-- ignoring heads -- the O(k) walk a dispatcher actually performs.
unℕ : V → ℕ
unℕ ⟨⟩      = zero
unℕ (_ ∙ n) = suc (unℕ n)

unℕ-⌜⌝ℕ : (n : ℕ) → unℕ ⌜ n ⌝ℕ ≡ n
unℕ-⌜⌝ℕ zero    = refl
unℕ-⌜⌝ℕ (suc n) = cong suc (unℕ-⌜⌝ℕ n)

------------------------------------------------------------------------
-- program encoding (tags 0/1/2 for swpV/sqV/ifnV; indices as unary
-- numerals, mirroring RCmdLEnc's constructor-tag pattern).

⌜_⌝V : RCmdV → V
⌜ swpV k ⌝V     = ⟨⟩ ∙ ⌜ k ⌝ℕ                                   -- tag 0 . k
⌜ sqV c d ⌝V    = (⟨⟩ ∙ ⟨⟩) ∙ (⌜ c ⌝V ∙ ⌜ d ⌝V)                -- tag 1 . (c . d)
⌜ ifnV k c d ⌝V = ((⟨⟩ ∙ ⟨⟩) ∙ ⟨⟩) ∙ (⌜ k ⌝ℕ ∙ (⌜ c ⌝V ∙ ⌜ d ⌝V))  -- tag 2 . (k . (c . d))

decodeV : V → Maybe RCmdV
decodeV (⟨⟩ ∙ k) with decodeℕ k
... | just k' = just (swpV k')
... | nothing = nothing
decodeV ((⟨⟩ ∙ ⟨⟩) ∙ (c ∙ d)) with decodeV c | decodeV d
... | just c' | just d' = just (sqV c' d')
... | _       | _       = nothing
decodeV (((⟨⟩ ∙ ⟨⟩) ∙ ⟨⟩) ∙ (k ∙ (c ∙ d))) with decodeℕ k | decodeV c | decodeV d
... | just k' | just c' | just d' = just (ifnV k' c' d')
... | _       | _       | _       = nothing
decodeV _ = nothing

decode-⌜⌝V : (c : RCmdV) → decodeV ⌜ c ⌝V ≡ just c
decode-⌜⌝V (swpV k)     rewrite decodeℕ-⌜⌝ℕ k = refl
decode-⌜⌝V (sqV c d)    rewrite decode-⌜⌝V c | decode-⌜⌝V d = refl
decode-⌜⌝V (ifnV k c d) rewrite decodeℕ-⌜⌝ℕ k | decode-⌜⌝V c | decode-⌜⌝V d = refl

------------------------------------------------------------------------
-- Test-first: the two-variable program of RCmdV.Examples round-trips, the
-- numeral readers agree on well-formed numerals, garbage is rejected.

module Examples where
  open RCmdV.Examples using (prog)   -- sqV (swpV 0) (ifnV 1 (swpV 0) (swpV 0))

  ex-roundtrip : decodeV ⌜ prog ⌝V ≡ just prog
  ex-roundtrip = decode-⌜⌝V prog

  -- ... and it holds by COMPUTATION on this instance.
  ex-roundtrip-refl : decodeV ⌜ prog ⌝V ≡ just prog
  ex-roundtrip-refl = refl

  ex-num : decodeℕ ⌜ 3 ⌝ℕ ≡ just 3
  ex-num = refl

  ex-un : unℕ ⌜ 3 ⌝ℕ ≡ 3
  ex-un = refl

  -- the strict reader rejects a non-numeral (non-nil head on the spine)...
  ex-strict : decodeℕ ((⟨⟩ ∙ ⟨⟩) ∙ ⟨⟩) ≡ nothing
  ex-strict = refl

  -- ...where the lenient walker still counts the spine.
  ex-lenient : unℕ ((⟨⟩ ∙ ⟨⟩) ∙ ⟨⟩) ≡ 1
  ex-lenient = refl

  -- garbage code is rejected.
  ex-garbage : decodeV ((⟨⟩ ∙ ⟨⟩) ∙ ⟨⟩) ≡ nothing
  ex-garbage = refl
