{-# OPTIONS --safe #-}
------------------------------------------------------------------------
-- M4 step (3): soundness of the GLOBAL-MEMO loop (Cogen.ml `m3_specialize` /
-- `drive2`).  Referenced by docs/ITEM3-DESIGN.md / AGDA_CORRESPONDENCE.
--
-- The M3 driver shares work via a global memo `m3_seen : config ⇀ name`: a config is
-- specialized ONCE, and a re-reached config FOLDS to its existing block (back-edge).
-- The driver's correctness factors into three obligations, the first proven HERE and
-- the other two already machine-checked:
--   (a) MEMOIZATION is TRANSPARENT -- sharing identical configs computes the same as
--       re-specializing each (this module): a consistent memo yields exactly `spec c`
--       on every config, and the worklist keeps the memo consistent (it only ever
--       records `spec c`).  So the residual equals the NON-memoized per-config result.
--   (b) each per-config specialization is sound -- the whole-spec congruence G1
--       (`SpecAssembly{,If,While}`, `SpecCmdStore`, `SpecExprStore`, `AVExprSound`).
--   (c) folding a recurring config into a loop, and reconstructing the block graph to
--       structured WHILE, are sound (`MemoFold.fold-sound`, `MemoReconstructGen`).
-- Termination of the configuration set is `WhistleTerminates`.
--
-- This module pins (a): the memo is a partial map `Config ⇀ Block`; it is CONSISTENT
-- when every entry equals the deterministic specializer `spec`.  `drive-transparent`:
-- under a consistent memo, looking up (hit) or specializing (miss) both yield `spec c`.
-- `store-preserves`: recording `(c, spec c)` keeps the memo consistent.  Hence the
-- memo built by the worklist is consistent (from `empty`, by repeated `store`), so the
-- driver is transparent -- memoization adds sharing, not a semantic change.
------------------------------------------------------------------------

module MemoDriverSound where

open import Data.Bool using (Bool; true; false; if_then_else_)
open import Data.Maybe using (Maybe; just; nothing)
open import Relation.Binary.PropositionalEquality using (_≡_; refl; sym; trans; cong)

module Driver (Config Block : Set)
              (spec : Config → Block)                       -- deterministic per-config specializer
              (eq : Config → Config → Bool)                 -- config equality test
              (eq-sound : ∀ {c c'} → eq c c' ≡ true → c ≡ c') where

  just-inj : ∀ {b b' : Block} → just b ≡ just b' → b ≡ b'
  just-inj refl = refl

  Memo : Set
  Memo = Config → Maybe Block

  -- a memo is CONSISTENT when every recorded block is the specialization of its config.
  Consistent : Memo → Set
  Consistent m = ∀ c b → m c ≡ just b → b ≡ spec c

  empty : Memo
  empty _ = nothing

  empty-consistent : Consistent empty
  empty-consistent c b ()

  -- record (c, b) in the memo (overwriting c's entry).
  store : Memo → Config → Block → Memo
  store m c b c' = if eq c' c then just b else m c'

  -- drive one config: memo HIT -> the stored block (the fold); MISS -> specialize it.
  useMemo : Memo → Config → Block
  useMemo m c with m c
  ... | just b  = b
  ... | nothing = spec c

  -- TRANSPARENCY: under a consistent memo, driving a config = specializing it, whether
  -- it hits or misses.  So the global memo only SHARES work; it never changes the
  -- computed residual (vs the non-memoized per-config specialization).
  drive-transparent : ∀ m c → Consistent m → useMemo m c ≡ spec c
  drive-transparent m c cons with m c in p
  ... | just b  = cons c b p
  ... | nothing = refl

  -- the worklist keeps the memo consistent: it only ever records `spec c`.
  store-preserves : ∀ m c → Consistent m → Consistent (store m c (spec c))
  store-preserves m c cons c' b eqp with eq c' c in p
  ... | true  = trans (sym (just-inj eqp)) (cong spec (sym (eq-sound p)))
  ... | false = cons c' b eqp
