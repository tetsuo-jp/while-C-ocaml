-- DesugarSpec.hs --- 脱糖パスのユニットテスト (hspec)
--
-- 実行:   make unittest
-- C0 計測: ghc -fhpc でビルドし、hpc report で確認する (make coverage)

module Main where

import Test.Hspec
import Control.Exception (evaluate)
import Data.List (isInfixOf, nub)

import AbsWhile
import ParWhile (pProgram, myLexer)
import PrintWhile (Print, printTree)
import LayoutWhile (resolveLayout)
import ErrM

import Lib (true, false, skip)
import Desugar (desugar, extractMain)
import ProgToData (progToData)
import TransOneVar (transOneVar)
import Data.Generics.Schemes (everything)
import Data.Generics.Aliases (mkQ)
import TransAndWhile (transAnd)
import TransCaseWhile (transCase)
import TransConspWhile (transConsp)
import TransBlkWhile (transBlk)
import TransList (transList)
import TransNumberWhile (transNumber)
import TransPCallWhile (doInline, expandIf, varsComs, mkPenv)

-- ソース文字列をパースする (失敗は error)
parseProg :: String -> Program
parseProg s = case pProgram (resolveLayout True (myLexer s)) of
  Ok t -> t
  Bad e -> error ("parse error: " ++ e ++ "\nin:\n" ++ s)

-- 空白を正規化して比較する
norm :: String -> String
norm = unwords . words

-- パスを適用した印字結果が期待文字列と一致する
(==>) :: (Program -> Program) -> (String, String) -> Expectation
pass ==> (src, expected) =
  norm (printTree (pass (parseProg src))) `shouldBe` norm expected

-- 期待側もパース可能な場合 (生成変数 _X, _Z, _W を含まない場合)
(===>) :: (Program -> Program) -> (String, String) -> Expectation
pass ===> (src, expected) =
  norm (printTree (pass (parseProg src)))
    `shouldBe` norm (printTree (parseProg expected))

-- パスを適用しても変化しない
unchangedBy :: (Program -> Program) -> String -> Expectation
unchangedBy pass src = pass ===> (src, src)

-- パス適用が指定メッセージの error を投げる
throwsMsg :: Print a => (Program -> a) -> String -> String -> Expectation
throwsMsg pass src msg =
  evaluate (length (printTree (pass (parseProg src)))) `shouldThrow` errorCall msg

main :: IO ()
main = hspec $ do

  describe "Lib" $ do
    it "true は cons nil nil" $
      norm (printTree true) `shouldBe` "cons nil nil"
    it "false は nil" $
      norm (printTree false) `shouldBe` "nil"
    it "skip は while nil do {} (no-op)" $
      norm (printTree skip) `shouldBe` "while nil do { }"

  describe "TransAndWhile" $ do
    it "and A B を =? (cons (=? A nil) (=? B nil)) (cons nil nil) に置き換える" $
      transAnd ===> ("read X; Y := and X Z; write Y",
                     "read X; Y := =? (cons (=? X nil) (=? Z nil)) (cons nil nil); write Y")
    it "and を含まないプログラムは変化しない" $
      unchangedBy transAnd "read X; Y := cons X X; write Y"

  describe "TransNumberWhile" $ do
    it "数値 n を nil^n に置き換える" $
      transNumber ==> ("read X; A := 2; write X",
                       "read X; A := (nil . (nil . nil)); write X")
    it "0 は nil になる" $
      transNumber ==> ("read X; A := 0; write X",
                       "read X; A := nil; write X")
    it "true は (nil . nil) になる" $
      transNumber ==> ("read X; A := true; write X",
                       "read X; A := (nil . nil); write X")
    it "false は nil になる" $
      transNumber ==> ("read X; A := false; write X",
                       "read X; A := nil; write X")
    it "アトムは変化しない" $
      unchangedBy transNumber "read X; A := 'a; write X"

  describe "TransList" $ do
    it "リスト表記 (a b) を cons の入れ子にする" $
      transList ===> ("read X; A := ('a 'b); write X",
                      "read X; A := cons 'a (cons 'b nil); write X")
    it "テール付きリスト表記 (a b . c) を展開する" $
      transList ===> ("read X; A := ('a 'b . 'c); write X",
                      "read X; A := cons 'a (cons 'b 'c); write X")
    it "cons* を右結合の cons にする" $
      transList ===> ("read X; A := cons* X Y Z; write X",
                      "read X; A := cons X (cons Y Z); write X")
    it "list を nil 終端の cons にする" $
      transList ===> ("read X; A := list X Y; write X",
                      "read X; A := cons X (cons Y nil); write X")
    it "リスト表記を含まないプログラムは変化しない" $
      unchangedBy transList "read X; A := cons X X; write X"

  describe "TransConspWhile" $ do
    it "cons? E を =? E (cons (hd E) (tl E)) にする" $
      transConsp ===> ("read X; A := cons? Y; write X",
                       "read X; A := =? Y (cons (hd Y) (tl Y)); write X")
    it "atom? E を =? nil (cons? E) の展開形にする" $
      transConsp ===> ("read X; A := atom? Y; write X",
                       "read X; A := =? nil (=? Y (cons (hd Y) (tl Y))); write X")
    it "cons?/atom? を含まないプログラムは変化しない" $
      unchangedBy transConsp "read X; A := cons X X; write X"

  describe "TransBlkWhile" $ do
    it "begin/end ブロックを外側のコマンド列に展開する (入れ子も)" $
      transBlk ===> ("read X; begin { X := nil; begin { Y := nil; } end Z := nil; } end write X",
                     "read X; X := nil; Y := nil; Z := nil; write X")
    it "while 本体の中のブロックも展開する" $
      transBlk ===> ("read X; while X do { begin { Y := nil; } end } write Y",
                     "read X; while X do { Y := nil; } write Y")
    it "ブロックを含まないプログラムは変化しない" $
      unchangedBy transBlk "read X; X := nil; write X"

  describe "TransCaseWhile" $ do
    it "変数パターン: 束縛して本体を実行する if になる" $
      transCase ==> ("read X; case X of { Z -> Y := Z; } write Y",
                     "read X; begin { _X := X; if cons nil nil then { Z := _X; Y := Z; } } end write Y")
    it "値パターン: =? による比較と skip になる" $
      transCase ==> ("read X; case X of { nil -> Y := nil; } write Y",
                     "read X; begin { _X := X; if =? _X nil then { while nil do { } Y := nil; } } end write Y")
    it "cons パターン: cons? の検査と hd/tl の束縛になる" $
      transCase ==> ("read X; case X of { (H . T) -> Y := H; } write Y",
                     "read X; begin { _X := X; \
                     \ if and (cons? _X) (and (cons nil nil) (cons nil nil)) then \
                     \ { begin { H := hd _X; T := tl _X; } end Y := H; } } end write Y")
    it "複数アーム: else で連鎖する" $ do
      let out = norm (printTree (transCase (parseProg
                  "read X; case X of { nil -> Y := nil; Z -> Y := Z; } write Y")))
      out `shouldSatisfy` ("else" `isInfixOf`)
      out `shouldSatisfy` (not . ("case" `isInfixOf`))
    it "複数 scrutinee: cons で束ねて要素ごとに照合する" $ do
      let out = norm (printTree (transCase (parseProg
                  "read X; case X, Y of { (A . B), C -> Z := A; } write Z")))
      out `shouldSatisfy` ("_X := cons X Y;" `isInfixOf`)
      out `shouldSatisfy` (not . ("case" `isInfixOf`))
    it "値どうしの cons パターン: hd/tl の双方を検査する" $ do
      let out = norm (printTree (transCase (parseProg
                  "read X; case X of { (nil . nil) -> Y := nil; } write Y")))
      out `shouldSatisfy` ("=? (hd _X) nil" `isInfixOf`)
      out `shouldSatisfy` ("=? (tl _X) nil" `isInfixOf`)
    it "アリティ不一致は error になる" $
      throwsMsg transCase "read X; case X, Y of { nil -> Z := nil; } write Z"
        "case: arity mismatch: 2 scrutinee(s) but 1 pattern(s) in an arm"
    it "アームなしの case は error になる" $
      throwsMsg transCase "read X; case X of { } write X"
        "impossible happened"
    it "case を含まないプログラムは変化しない" $
      unchangedBy transCase "read X; X := nil; write X"

  describe "TransPCallWhile.expandIf" $ do
    it "else なし if を while にエンコードする" $
      expandIf ==> ("read X; if X then { Y := nil; } write Y",
                    "read X; begin { _Z := X; while _Z do { _Z := nil; Y := nil; } } end write Y")
    it "else あり if を _Z/_W の 2 つの while にエンコードする" $
      expandIf ==> ("read X; if X then { Y := nil; } else { Y := X; } write Y",
                    "read X; begin { _Z := X; _W := cons nil nil; \
                    \ while _Z do { _Z := nil; Y := nil; _W := nil; } \
                    \ while _W do { _W := nil; Y := X; } } end write Y")
    it "if を含まないプログラムは変化しない" $
      unchangedBy expandIf "read X; while X do { X := tl X; } write X"

  describe "TransPCallWhile.doInline" $ do
    it "呼び出しを本体で置き換え read/write 変数を呼び出し側に配線する" $
      doInline ==> ("procedure p read A; B := cons nil A; write B \
                    \ read X; Y := p X; write Y",
                    "procedure p read A; B := cons nil A; write B \
                    \ read X; begin { Y := cons nil X; } end write Y")
    it "ローカル変数を呼び出しごとに fresh な名前にする" $ do
      let out = norm (printTree (doInline (parseProg
                  "procedure p read A; L := A; B := L; write B \
                  \ read X; Y := p X; Z := p Y; write Z")))
      out `shouldSatisfy` ("L'0 := X;" `isInfixOf`)
      out `shouldSatisfy` ("L'1 := Y;" `isInfixOf`)
    it "ネストした呼び出しも展開する" $ do
      let out = norm (printTree (doInline (parseProg
                  "procedure r read C; D := cons C C; write D \
                  \ procedure s read E; F := r E; write F \
                  \ read X; Y := s X; write Y")))
      out `shouldSatisfy` ("Y := cons X X;" `isInfixOf`)
    it "本体の while/if/case/begin/show の中もリネームする" $ do
      let out = norm (printTree (doInline (parseProg
                  "procedure q read A; \
                  \ while A do { L := A; } \
                  \ if A then { L := nil; } \
                  \ if A then { L := nil; } else { L := A; } \
                  \ case A of { (H . T) -> L := H; } \
                  \ begin { L := nil; } end \
                  \ show A; \
                  \ B := L; write B \
                  \ read X; Y := q X; write Y")))
      out `shouldSatisfy` ("while X do" `isInfixOf`)
      out `shouldSatisfy` ("case X of" `isInfixOf`)
      out `shouldSatisfy` ("show X;" `isInfixOf`)
      out `shouldSatisfy` ("L'0" `isInfixOf`)
    it "未定義プロシージャの呼び出しは error になる" $
      throwsMsg doInline "read X; Y := nosuch X; write Y"
        "No procedure has a procedure name."

  describe "TransPCallWhile.varsComs" $ do
    it "コマンド列中の識別子を重複なく集める" $ do
      let Prog [AProc _ _ coms _] =
            parseProg "read X; B := cons A A; write B"
      varsComs coms `shouldBe` ["B", "A"]

  describe "TransPCallWhile.mkPenv" $ do
    it "名前付きプロシージャを登録する" $ do
      let prog = parseProg "procedure p read A; B := A; write B \
                           \ read X; Y := p X; write Y"
      case lookup "p" (mkPenv prog) of
        Just (Ident a, _, Ident b, vs) -> do
          a `shouldBe` "A"
          b `shouldBe` "B"
          vs `shouldBe` ["B", "A"]
        Nothing -> expectationFailure "procedure p not found"

  describe "Desugar.extractMain" $ do
    it "名前付きプロシージャを取り除き main だけ残す" $
      extractMain ===> ("procedure p read A; B := A; write B \
                        \ read X; Y := X; write Y",
                        "read X; Y := X; write Y")

  describe "ProgToData (programs-as-data 表現 p.49)" $ do
    -- 期待値構築用ヘルパ (テスト側で教科書の図から独立に書き下す)
    let vList = foldr VCons VNil
        vNum n = vList (replicate n VNil)
        vAtom s = VAtom (Atom ('\'' : s))
        vVar i = vList [vAtom "var", vNum i]
        enc src = progToData (parseProg src)

    it "代入と変数: read 変数が 1、write 変数が最後の番号になる" $
      enc "read X; Y := X; write Y" `shouldBe`
        vList [vVar 1, vList [vAtom "asgn", vVar 2, vVar 1], vVar 2]

    it "教科書 p.49 の reverse の例と一致する" $
      enc "read X; Y := nil; while X do { Y := cons (hd X) Y; X := tl X; } write Y"
        `shouldBe`
        vList
          [ vVar 1
          , vList [ vAtom "semi"
                  , vList [vAtom "asgn", vVar 2, vList [vAtom "quote", VNil]]
                  , vList [ vAtom "while", vVar 1
                          , vList [ vAtom "semi"
                                  , vList [ vAtom "asgn", vVar 2
                                          , vList [ vAtom "cons"
                                                  , vList [vAtom "hd", vVar 1]
                                                  , vVar 2]]
                                  , vList [ vAtom "asgn", vVar 1
                                          , vList [vAtom "tl", vVar 1]]]]]
          , vVar 2 ]

    it "=? とアトムの quote を符号化する" $
      enc "read X; Y := =? X 'a; write Y" `shouldBe`
        vList [ vVar 1
              , vList [vAtom "asgn", vVar 2,
                       vList [vAtom "eq", vVar 1, vList [vAtom "quote", vAtom "a"]]]
              , vVar 2 ]

    it "出力変数は途中に現れても最大番号になる" $
      case enc "read X; Y := X; Z := Y; write Y" of
        VCons _ (VCons _ (VCons outVar VNil)) -> outVar `shouldBe` vVar 3
        v -> expectationFailure ("unexpected shape: " ++ show v)

    it "desugar 経由で数値も (quote nil^n) になる" $
      progToData (desugar (parseProg "read X; Y := 2; write Y")) `shouldBe`
        vList [ vVar 1
              , vList [vAtom "asgn", vVar 2,
                       vList [vAtom "quote", VCons VNil (VCons VNil VNil)]]
              , vVar 2 ]

    it "空のコマンド列は no-op 代入 V1 := V1 に符号化される" $
      enc "read X; write X" `shouldBe`
        vList [vVar 1, vList [vAtom "asgn", vVar 1, vVar 1], vVar 1]

    it "コア以外のコマンドは error になる" $
      throwsMsg progToData "read X; show X; write X"
        ("progToData: not a core command: " ++ printTree (CShow (EVar (Ident "X"))))

    it "コア以外の式は error になる" $
      throwsMsg progToData "read X; Y := cons? X; write Y"
        ("progToData: not a core expression: " ++ printTree (EConsp (EVar (Ident "X"))))

    it "コア以外の値は error になる" $
      throwsMsg progToData "read X; Y := 3; write Y"
        ("progToData: not a core value: " ++ printTree (VInt 3))

    it "プロシージャが残っていれば error になる" $
      throwsMsg progToData
        "procedure p read A; B := A; write B read X; Y := X; write Y"
        "progToData: expected a single unnamed procedure (run desugar first)"

  describe "TransOneVar (WHILE -> 1 変数 I, many-one-var)" $ do
    let oneVar = transOneVar . desugar . parseProg
        varsOf prog = nub (everything (++) (mkQ [] (\i -> [i :: Ident])) prog)

    it "結果はちょうど 1 変数を使う (reverse)" $
      varsOf (oneVar
        "read X; Y := nil; while X do { Y := cons (hd X) Y; X := tl X; } write Y")
        `shouldBe` [Ident "A"]
    it "結果はちょうど 1 変数を使う (3 変数プログラム)" $
      varsOf (oneVar
        "read XY; X := hd XY; Y := tl XY; while X do { Y := cons (hd X) Y; X := tl X; } write Y")
        `shouldBe` [Ident "A"]
    it "入力変数と出力変数が同じでも 1 変数になる" $
      varsOf (oneVar "read X; X := tl X; write X") `shouldBe` [Ident "A"]
    it "既存の変数名 A と衝突しない (fresh な名前を選ぶ)" $
      varsOf (oneVar "read A; B := tl A; write B")
        `shouldSatisfy` (\vs -> length vs == 1)
    it "=? も 1 変数アクセスに変換する" $
      norm (printTree (oneVar "read X; Y := =? X X; write Y"))
        `shouldBe` norm
          "read A; A := cons A nil; \
          \ A := cons (hd A) (cons (=? (hd A) (hd A)) nil); \
          \ A := hd (tl A); write A"
    it "教科書の構成 (reverse, X=1 Y=2) と一致する" $
      norm (printTree (oneVar
        "read X; Y := nil; while X do { Y := cons (hd X) Y; X := tl X; } write Y"))
        `shouldBe` norm
          "read A; \
          \ A := cons A nil; \
          \ A := cons (hd A) (cons nil nil); \
          \ while hd A do { \
          \   A := cons (hd A) (cons (cons (hd (hd A)) (hd (tl A))) nil); \
          \   A := cons (tl (hd A)) (cons (hd (tl A)) nil); \
          \ } \
          \ A := hd (tl A); \
          \ write A"
    -- transOneVar はコア WHILE のみ受け付ける (desugar 前は error)
    it "コア以外の式は error になる" $
      throwsMsg transOneVar "read X; Y := cons? X; write Y"
        ("transOneVar: non-core expression: " ++ printTree (EConsp (EVar (Ident "X"))))
    it "コア以外のコマンドは error になる" $
      throwsMsg transOneVar "read X; show X; write X"
        ("transOneVar: non-core command: " ++ printTree (CShow (EVar (Ident "X"))))
    it "プロシージャが複数あれば error になる" $
      throwsMsg transOneVar
        "procedure p read A; B := A; write B read X; Y := X; write Y"
        "transOneVar: expected a single procedure (run desugar first)"

  describe "Desugar.desugar (パイプライン全体)" $ do
    it "プロシージャ呼び出しを含むプログラムをコア WHILE にする" $
      desugar ===> ("procedure succ read X; Y := cons nil X; write Y \
                    \ read A; B := succ A; write B",
                    "read A; B := cons nil A; write B")
    it "拡張構文がコア言語に残らない" $ do
      let out = norm (printTree (desugar (parseProg
                  "read X; Y := nil; GO := true; \
                  \ while GO do { case X of { nil -> GO := false; (H . X) -> Y := cons H Y; } } \
                  \ write Y")))
      out `shouldSatisfy` (not . ("case" `isInfixOf`))
      out `shouldSatisfy` (not . ("if" `isInfixOf`))
      out `shouldSatisfy` (not . ("begin" `isInfixOf`))
      out `shouldSatisfy` (not . ("cons?" `isInfixOf`))
      out `shouldSatisfy` (not . ("and " `isInfixOf`))
      out `shouldSatisfy` (not . ("true" `isInfixOf`))
      out `shouldSatisfy` (not . ("false" `isInfixOf`))
