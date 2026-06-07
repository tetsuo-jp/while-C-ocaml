module Desugar(desugar, extractMain) where

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
