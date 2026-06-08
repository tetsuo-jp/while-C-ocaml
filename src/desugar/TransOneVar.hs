module TransOneVar(transOneVar) where

import AbsWhile
import PrintWhile(printTree)
import Data.List(nub)
import Data.Generics.Schemes    -- everything
import Data.Generics.Aliases    -- mkQ

-- WHILE -> I (one-variable WHILE) 変換  (Jones, Prop. many-one-var, ch.19)
--
-- 全変数 X1..Xn を 1 変数 A にリスト (X1 ... Xn) として詰め込む。
--   ⌜Xi⌝          = hd (tl^{i-1} A)
--   ⌜Xi := E⌝     = A := cons T1 (... (cons Tn nil))  (Ti=⌜E⌝, Tj=⌜Xj⌝)
--   ⌜while E do C⌝ = while ⌜E⌝ do ⌜C⌝
--   ⌜read X1; C; write X2⌝
--                 = read A; A := cons A nil; ⌜C⌝; A := hd(tl^{iout-1} A); write A
--
-- 入力変数を番号 1、出力変数を 2 (入力と異なれば) として割り当てる。
-- desugar 済みのコア WHILE (CAsn/CLoop と cons/hd/tl/=?/var/val) のみ受け付ける。

transOneVar :: Program -> Program
transOneVar (Prog [AProc nameop xin coms xout]) =
  let vars = nub (xin : xout : allIdents coms)   -- xin=1, xout=2 (異なれば)
      n = length vars
      a = freshName vars

      -- vars は nub (xin:xout:allIdents coms) なので全変数を必ず含む (全域)
      idx v = 1 + length (takeWhile (/= v) vars)
      iout = idx xout

      -- tl^k E
      tlPow 0 e = e
      tlPow k e = tlPow (k - 1 :: Int) (ETl e)
      -- ⌜Xi⌝ = hd (tl^{i-1} A)
      access i = EHd (tlPow (i - 1) (EVar a))

      tExp e = case e of
        EVar v      -> access (idx v)
        EVal d      -> EVal d
        ECons e1 e2 -> ECons (tExp e1) (tExp e2)
        EHd e1      -> EHd (tExp e1)
        ETl e1      -> ETl (tExp e1)
        EEq e1 e2   -> EEq (tExp e1) (tExp e2)
        _ -> error ("transOneVar: non-core expression: " ++ printTree e)

      -- スロット i に val、他は現在の Xj を入れて A を作り直す
      packWith i val =
        foldr ECons (EVal VNil)
          [ if j == i then val else access j | j <- [1 .. n] ]

      tCom c = case c of
        CAsn v e   -> CAsn a (packWith (idx v) (tExp e))
        CLoop e cs -> CLoop (tExp e) (map tCom cs)
        _ -> error ("transOneVar: non-core command: " ++ printTree c)

      initA  = CAsn a (ECons (EVar a) (EVal VNil))  -- A := cons A nil
      finalA = CAsn a (access iout)                 -- A := hd(tl^{iout-1} A)
  in Prog [AProc nameop a (initA : map tCom coms ++ [finalA]) a]
transOneVar _ =
  error "transOneVar: expected a single procedure (run desugar first)"

-- コマンド中の全変数を出現順に集める
allIdents :: [Com] -> [Ident]
allIdents = everything (++) (mkQ [] (\i -> [i :: Ident]))

-- vars に現れない新しい変数名を作る ("A", "A_0", "A_1", ...)
freshName :: [Ident] -> Ident
freshName used = pick (0 :: Int)
  where pick i = let v = if i == 0 then Ident "A" else Ident ("A_" ++ show (i - 1))
                 in if v `notElem` used then v else pick (i + 1)
