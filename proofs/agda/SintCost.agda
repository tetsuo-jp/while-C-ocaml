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
open import Data.Nat.Solver using (module +-*-Solver)
open import Relation.Binary.PropositionalEquality using (_≡_; refl; subst; cong)
open import UniversalInterp
  using (V; ⟨⟩; _∙_; hd; tl; Exp; var; knil; kcons; khd; ktl; kifnil; call)
open import ProgEncoding using (⌜_⌝)
open import SelfInterp using (sint)
open import CostRelation using (module Rel)

open Rel sint            -- the cost relation with the ambient program fixed to `sint`
open +-*-Solver          -- linear-arithmetic solver for the count reshapes

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

-- Interpreting `ktl e` costs 48 (tag 4 ⇒ one extra `kifnil`-else vs `khd`).
sint-ktl : ∀ p e env {v cₑ} →
  sint , (p ∙ (⌜ e ⌝ ∙ env)) ⇓ v # cₑ →
  sint , (p ∙ (⌜ ktl e ⌝ ∙ env)) ⇓ tl v # (48 + cₑ)
sint-ktl p e env d =
  ⇓else (⇓khd (⇓khd (⇓ktl ⇓var)))
    (⇓else (⇓ktl (⇓khd (⇓khd (⇓ktl ⇓var))))
      (⇓else (⇓ktl (⇓ktl (⇓khd (⇓khd (⇓ktl ⇓var)))))
        (⇓else (⇓ktl (⇓ktl (⇓ktl (⇓khd (⇓khd (⇓ktl ⇓var))))))
          (⇓then (⇓ktl (⇓ktl (⇓ktl (⇓ktl (⇓khd (⇓khd (⇓ktl ⇓var)))))))
            (⇓ktl (⇓call
                     (⇓kcons (⇓khd ⇓var)
                             (⇓kcons (⇓ktl (⇓khd (⇓ktl ⇓var)))
                                     (⇓ktl (⇓ktl ⇓var))))
                     d))))))

-- Base case `var` (tag 0): the outer `kifnil` guard is nil ⇒ return `env` (`R`).
sint-var : ∀ p env → sint , (p ∙ (⌜ var ⌝ ∙ env)) ⇓ env # 8
sint-var p env =
  ⇓then (⇓khd (⇓khd (⇓ktl ⇓var))) (⇓ktl (⇓ktl ⇓var))

-- Base case `knil` (tag 1): one else, then the `knil` branch ⇒ ⟨⟩.
sint-knil : ∀ p env → sint , (p ∙ (⌜ knil ⌝ ∙ env)) ⇓ ⟨⟩ # 12
sint-knil p env =
  ⇓else (⇓khd (⇓khd (⇓ktl ⇓var)))
    (⇓then (⇓ktl (⇓khd (⇓khd (⇓ktl ⇓var)))) ⇓knil)

------------------------------------------------------------------------
-- Multi-sub constructors: the count carries two sub-costs inside a `+`, so it is
-- reshaped to the clean `K + Σ subcost` form by the linear solver + `subst`.
------------------------------------------------------------------------

private
  kcons-eq : ∀ ca cb →
    suc (4 + suc (5 + suc (6 + suc ((13 + ca) + (13 + cb))))) ≡ 45 + (ca + cb)
  kcons-eq = solve 2
    (λ ca cb → con 1 :+ (con 4 :+ (con 1 :+ (con 5 :+ (con 1 :+ (con 6 :+
                 (con 1 :+ ((con 13 :+ ca) :+ (con 13 :+ cb))))))))
             := con 45 :+ (ca :+ cb))
    refl

-- Interpreting `kcons a b` costs 45 on top of interpreting `a` and `b`.
sint-kcons : ∀ p a b env {va vb ca cb} →
  sint , (p ∙ (⌜ a ⌝ ∙ env)) ⇓ va # ca →
  sint , (p ∙ (⌜ b ⌝ ∙ env)) ⇓ vb # cb →
  sint , (p ∙ (⌜ kcons a b ⌝ ∙ env)) ⇓ (va ∙ vb) # (45 + (ca + cb))
sint-kcons p a b env {va = va} {vb = vb} {ca = ca} {cb = cb} da db =
  subst (λ n → sint , (p ∙ (⌜ kcons a b ⌝ ∙ env)) ⇓ (va ∙ vb) # n) (kcons-eq ca cb)
    (⇓else (⇓khd (⇓khd (⇓ktl ⇓var)))
      (⇓else (⇓ktl (⇓khd (⇓khd (⇓ktl ⇓var))))
        (⇓then (⇓ktl (⇓ktl (⇓khd (⇓khd (⇓ktl ⇓var)))))
          (⇓kcons
            (⇓call (⇓kcons (⇓khd ⇓var)
                           (⇓kcons (⇓khd (⇓ktl (⇓khd (⇓ktl ⇓var))))
                                   (⇓ktl (⇓ktl ⇓var))))
                   da)
            (⇓call (⇓kcons (⇓khd ⇓var)
                           (⇓kcons (⇓ktl (⇓ktl (⇓khd (⇓ktl ⇓var))))
                                   (⇓ktl (⇓ktl ⇓var))))
                   db)))))

private
  -- small solver "cores": reshape only the handler count; the outer literal
  -- `kifnil`-walk (`45 + _`) reduces definitionally, and `cong (45 +_)` lifts it.
  kifnil-core : ∀ cg cb → suc (suc (12 + cg) + suc (13 + cb)) ≡ 28 + (cg + cb)
  kifnil-core = solve 2
    (λ cg cb → con 1 :+ ((con 1 :+ (con 12 :+ cg)) :+ (con 1 :+ (con 13 :+ cb)))
             := con 28 :+ (cg :+ cb)) refl

  call-core : ∀ cₑ cᵣ → suc ((18 + cₑ) + cᵣ) ≡ 19 + (cₑ + cᵣ)
  call-core = solve 2
    (λ cₑ cᵣ → con 1 :+ ((con 18 :+ cₑ) :+ cᵣ) := con 19 :+ (cₑ :+ cᵣ)) refl

-- Interpreting `kifnil g t e`, guard true (⟨⟩): interpret g then the THEN branch t.
sint-kifnil-then : ∀ p g t e env {vt cg ct} →
  sint , (p ∙ (⌜ g ⌝ ∙ env)) ⇓ ⟨⟩ # cg →
  sint , (p ∙ (⌜ t ⌝ ∙ env)) ⇓ vt # ct →
  sint , (p ∙ (⌜ kifnil g t e ⌝ ∙ env)) ⇓ vt # (73 + (cg + ct))
sint-kifnil-then p g t e env {vt = vt} {cg = cg} {ct = ct} dg dt =
  subst (λ n → sint , (p ∙ (⌜ kifnil g t e ⌝ ∙ env)) ⇓ vt # n) (cong (45 +_) (kifnil-core cg ct))
    (⇓else (⇓khd (⇓khd (⇓ktl ⇓var)))
      (⇓else (⇓ktl (⇓khd (⇓khd (⇓ktl ⇓var))))
        (⇓else (⇓ktl (⇓ktl (⇓khd (⇓khd (⇓ktl ⇓var)))))
          (⇓else (⇓ktl (⇓ktl (⇓ktl (⇓khd (⇓khd (⇓ktl ⇓var))))))
            (⇓else (⇓ktl (⇓ktl (⇓ktl (⇓ktl (⇓khd (⇓khd (⇓ktl ⇓var)))))))
              (⇓then (⇓ktl (⇓ktl (⇓ktl (⇓ktl (⇓ktl (⇓khd (⇓khd (⇓ktl ⇓var))))))))
                (⇓then
                  (⇓call (⇓kcons (⇓khd ⇓var)
                                 (⇓kcons (⇓khd (⇓ktl (⇓khd (⇓ktl ⇓var))))
                                         (⇓ktl (⇓ktl ⇓var)))) dg)
                  (⇓call (⇓kcons (⇓khd ⇓var)
                                 (⇓kcons (⇓khd (⇓ktl (⇓ktl (⇓khd (⇓ktl ⇓var)))))
                                         (⇓ktl (⇓ktl ⇓var)))) dt))))))))

-- Interpreting `kifnil g t e`, guard false (cons): interpret g then the ELSE branch e.
sint-kifnil-else : ∀ p g t e env {vh vl ve cg ce} →
  sint , (p ∙ (⌜ g ⌝ ∙ env)) ⇓ (vh ∙ vl) # cg →
  sint , (p ∙ (⌜ e ⌝ ∙ env)) ⇓ ve # ce →
  sint , (p ∙ (⌜ kifnil g t e ⌝ ∙ env)) ⇓ ve # (73 + (cg + ce))
sint-kifnil-else p g t e env {ve = ve} {cg = cg} {ce = ce} dg de =
  subst (λ n → sint , (p ∙ (⌜ kifnil g t e ⌝ ∙ env)) ⇓ ve # n) (cong (45 +_) (kifnil-core cg ce))
    (⇓else (⇓khd (⇓khd (⇓ktl ⇓var)))
      (⇓else (⇓ktl (⇓khd (⇓khd (⇓ktl ⇓var))))
        (⇓else (⇓ktl (⇓ktl (⇓khd (⇓khd (⇓ktl ⇓var)))))
          (⇓else (⇓ktl (⇓ktl (⇓ktl (⇓khd (⇓khd (⇓ktl ⇓var))))))
            (⇓else (⇓ktl (⇓ktl (⇓ktl (⇓ktl (⇓khd (⇓khd (⇓ktl ⇓var)))))))
              (⇓then (⇓ktl (⇓ktl (⇓ktl (⇓ktl (⇓ktl (⇓khd (⇓khd (⇓ktl ⇓var))))))))
                (⇓else
                  (⇓call (⇓kcons (⇓khd ⇓var)
                                 (⇓kcons (⇓khd (⇓ktl (⇓khd (⇓ktl ⇓var))))
                                         (⇓ktl (⇓ktl ⇓var)))) dg)
                  (⇓call (⇓kcons (⇓khd ⇓var)
                                 (⇓kcons (⇓ktl (⇓ktl (⇓ktl (⇓khd (⇓ktl ⇓var)))))
                                         (⇓ktl (⇓ktl ⇓var)))) de))))))))

-- Interpreting `call e`: interpret e ⇒ arg, then re-interpret the program on arg.
sint-call : ∀ p e env {arg v cₑ cᵣ} →
  sint , (p ∙ (⌜ e ⌝ ∙ env)) ⇓ arg # cₑ →
  sint , (p ∙ (p ∙ arg)) ⇓ v # cᵣ →
  sint , (p ∙ (⌜ call e ⌝ ∙ env)) ⇓ v # (64 + (cₑ + cᵣ))
sint-call p e env {v = v} {cₑ = cₑ} {cᵣ = cᵣ} dₑ dᵣ =
  subst (λ n → sint , (p ∙ (⌜ call e ⌝ ∙ env)) ⇓ v # n) (cong (45 +_) (call-core cₑ cᵣ))
    (⇓else (⇓khd (⇓khd (⇓ktl ⇓var)))
      (⇓else (⇓ktl (⇓khd (⇓khd (⇓ktl ⇓var))))
        (⇓else (⇓ktl (⇓ktl (⇓khd (⇓khd (⇓ktl ⇓var)))))
          (⇓else (⇓ktl (⇓ktl (⇓ktl (⇓khd (⇓khd (⇓ktl ⇓var))))))
            (⇓else (⇓ktl (⇓ktl (⇓ktl (⇓ktl (⇓khd (⇓khd (⇓ktl ⇓var)))))))
              (⇓else (⇓ktl (⇓ktl (⇓ktl (⇓ktl (⇓ktl (⇓khd (⇓khd (⇓ktl ⇓var))))))))
                (⇓call
                  (⇓kcons (⇓khd ⇓var)
                          (⇓kcons (⇓khd ⇓var)
                                  (⇓call (⇓kcons (⇓khd ⇓var)
                                                 (⇓kcons (⇓ktl (⇓khd (⇓ktl ⇓var)))
                                                         (⇓ktl (⇓ktl ⇓var)))) dₑ)))
                  dᵣ)))))))
