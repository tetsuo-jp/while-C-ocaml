module ProgToData(progToData) where

import AbsWhile
import PrintWhile(printTree)
import Data.List(nub, delete, elemIndex)
import Data.Maybe(fromJust)
import TransPCallWhile(varsComs)

-- programs-as-data 表現 ⌜p⌝ p.49 (Section 3.2 Representing WHILE programs in D)
--
--   ⌜read Vi; C; write Vj⌝ = ((var i) ⌜C⌝ (var j))
--   ⌜C; D⌝                 = (semi ⌜C⌝ ⌜D⌝)
--   ⌜while E do C⌝         = (while ⌜E⌝ ⌜C⌝)
--   ⌜Vi := E⌝              = (asgn (var i) ⌜E⌝)
--   ⌜Vi⌝ = (var i)   ⌜d⌝ = (quote d)   ⌜cons E F⌝ = (cons ⌜E⌝ ⌜F⌝)   ...
--
-- 教科書のアトム :=, ;, =? は字句上アトムにできないため
-- 'asgn, 'semi, 'eq で代用する。数 i は nil^i (i 要素のリスト)。
-- 変数番号は出現順に 1 から振り、出力変数の番号が最大になるよう
-- 最後に置く (p.49 脚注の規約)。
--
-- コア WHILE のみ受け付ける。拡張構文は先に desugar しておくこと。

progToData :: Program -> Val
progToData (Prog [AProc NoName x@(Ident xs) coms (Ident ys)]) =
  let env = delete ys (nub (xs : varsComs coms)) ++ [ys]
      var (Ident s) = list [atom "var", num (fromJust (elemIndex s env) + 1)]

      -- 空のコマンド列 (skip 等) は no-op 代入 V1 := V1 に符号化する
      -- (教科書のコマンドは常に非空のため表現がない)
      noop = list [atom "asgn", var1, var1]
        where var1 = list [atom "var", num 1]

      encComs cs = case cs of
        []        -> noop
        [c]       -> encCom c
        (c : cs') -> list [atom "semi", encCom c, encComs cs']

      encCom c = case c of
        CAsn v e   -> list [atom "asgn", var v, encExp e]
        CLoop e cs -> list [atom "while", encExp e, encComs cs]
        _          -> error ("progToData: not a core command: " ++ printTree c)

      encExp e = case e of
        EVar v    -> var v
        EVal d
          | coreVal d -> list [atom "quote", d]
          | otherwise -> error ("progToData: not a core value: " ++ printTree d)
        ECons a b -> list [atom "cons", encExp a, encExp b]
        EHd a     -> list [atom "hd", encExp a]
        ETl a     -> list [atom "tl", encExp a]
        EEq a b   -> list [atom "eq", encExp a, encExp b]
        _         -> error ("progToData: not a core expression: " ++ printTree e)

  in list [var x, encComs coms, var (Ident ys)]
progToData _ =
  error "progToData: expected a single unnamed procedure (run desugar first)"

-- コアの値 (nil, アトム, 対) か
coreVal :: Val -> Bool
coreVal v = case v of
  VNil      -> True
  VAtom _   -> True
  VCons a b -> coreVal a && coreVal b
  _         -> False

-- リスト記法 (x1 ... xn) = cons x1 (... (cons xn nil))
list :: [Val] -> Val
list = foldr VCons VNil

-- 数記法 i = nil^i
num :: Int -> Val
num n = list (replicate n VNil)

atom :: String -> Val
atom s = VAtom (Atom ('\'' : s))
