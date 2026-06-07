-- DesugarSpec.hs --- 脱糖パスのユニットテスト (hspec)
--
-- 実行:   make unittest
-- C0 計測: ghc -fhpc でビルドし、hpc report で確認する (make coverage)

module Main where

import Test.Hspec
import Control.Exception (evaluate)
import Data.List (isInfixOf)

import AbsWhile
import ParWhile (pProgram, myLexer)
import PrintWhile (printTree)
import LayoutWhile (resolveLayout)
import ErrM

import Lib (true, false, skip)
import Desugar (desugar, extractMain)
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
throwsMsg :: (Program -> Program) -> String -> String -> Expectation
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
