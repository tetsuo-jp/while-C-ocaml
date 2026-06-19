module Desugar(desugar, desugarIf, extractMain) where

import AbsWhile
import TransNumberWhile(transNumber)
import TransPCallWhile(doInline, expandIf)
import TransAndWhile(transAnd)
import TransCaseWhile(transCase)
import TransConspWhile(transConsp)
import TransBlkWhile(transBlk)
import TransList(transList)

-- 名前無しプロシージャを取り出す
extractMain :: Program -> Program
extractMain (Prog procs) = Prog (filter f procs)
  where f (AProc pNameOp _ _ _) = case pNameOp of
          Name ident -> False
          NoName -> True

-- 拡張 WHILE をコア WHILE に脱糖する(右から順に適用)
desugar :: Program -> Program
desugar = transNumber . transList . transBlk . expandIf . extractMain
        . doInline . transConsp . transAnd . transCase

-- expandIf を省いた版: `if` を `while` に展開せず CIf のまま残す。
-- 第一級の `if` を保つ programs-as-data 表現 (--data-if) 用。
-- これにより、自己適用時に動的化する条件 (例: mix 自身の `if =? BT 'S`) が
-- ループではなく再収束する分岐として特殊化器に渡る (FUTAMURA.md 参照)。
desugarIf :: Program -> Program
desugarIf = transNumber . transList . transBlk . extractMain
          . doInline . transConsp . transAnd . transCase
