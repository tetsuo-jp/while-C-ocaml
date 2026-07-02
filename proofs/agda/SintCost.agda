{-# OPTIONS --safe #-}

------------------------------------------------------------------------
-- Per-node dispatch cost of the self-interpreter (Levin plan B1.2, Phase 2).
--
-- Working in the big-step cost relation `CostRelation.Rel` at `prog = sint`, we
-- show that simulating ONE object node costs `sint` a BOUNDED number of its own
-- steps plus the cost of the sub-interpretations.  Each lemma is a derivation TREE
-- built over `sint`'s dispatch code (no reduction of a symbolic sub-run -- the
-- sub-derivation enters as an opaque premise), so the count constant `K` is exactly
-- the node-count of that tree.
--
-- This module: the `khd` case (`sint-khd`, `K = 39`).  `sint` reaches the `khd`
-- handler `khd (ireq argsE R)` by walking the tag of `⌜ khd e ⌝` (= `tag 3`) down
-- three `kifnil`-else branches to the fourth (then) branch; the handler runs `sint`
-- on the sub-request `(⌜p⌝ . (⌜e⌝ . env))` (the premise) and applies `hd`.
------------------------------------------------------------------------

module SintCost where

open import Data.Nat using (ℕ; suc; _+_)
open import UniversalInterp
  using (V; ⟨⟩; _∙_; hd; tl; Exp; var; knil; kcons; khd; ktl; kifnil; call)
open import ProgEncoding using (⌜_⌝)
open import SelfInterp using (sint)
open import CostRelation using (module Rel)

open Rel sint   -- the cost relation with the ambient program fixed to `sint`

-- Interpreting `khd e` costs `sint` 39 steps on top of interpreting `e`.
-- (The guard walks tag 3 by 3 `ktl`s: else,else,else,then; the handler is
-- `khd (call (kcons P (kcons argsE R)))` with the sub-run as the `⇓call` premise.)
sint-khd : ∀ p e env {v cₑ} →
  sint , (p ∙ (⌜ e ⌝ ∙ env)) ⇓ v # cₑ →
  sint , (p ∙ (⌜ khd e ⌝ ∙ env)) ⇓ hd v # (39 + cₑ)
sint-khd p e env d =
  ⇓else (⇓khd (⇓khd (⇓ktl ⇓var)))
    (⇓else (⇓ktl (⇓khd (⇓khd (⇓ktl ⇓var))))
      (⇓else (⇓ktl (⇓ktl (⇓khd (⇓khd (⇓ktl ⇓var)))))
        (⇓then (⇓ktl (⇓ktl (⇓ktl (⇓khd (⇓khd (⇓ktl ⇓var))))))
          (⇓khd (⇓call
                   (⇓kcons (⇓khd ⇓var)
                           (⇓kcons (⇓ktl (⇓khd (⇓ktl ⇓var)))
                                   (⇓ktl (⇓ktl ⇓var))))
                   d)))))
