(* EvalWhileTest.ml --- コア・インタプリタのユニットテスト (OUnit2) *)
(* 実行: make unittest *)

open OUnit2
open AbsWhile
open EvalWhile

(* ---- ヘルパ ---- *)

let parse_proc (s : string) : proc =
  ParWhile.pProc LexWhile.token (Lexing.from_string s)

let parse_val (s : string) : valT =
  ParWhile.pValT LexWhile.token (Lexing.from_string s)

let show_val (v : valT) : string =
  PrintWhile.printTree PrintWhile.prtValT v

let assert_val (expected : valT) (actual : valT) =
  assert_equal ~printer:show_val expected actual

(* 略記 *)
let x = Id "X"
let y = Id "Y"
let z = Id "Z"
let atom_a = VAtom (Atom "'a")
let atom_b = VAtom (Atom "'b")
let tt = VCons (VNil, VNil)          (* true の表現 *)
let list_ab = VCons (atom_a, VCons (atom_b, VNil))

(* ---- update ---- *)

let test_update =
  "update" >::: [
    "先頭の変数を更新する" >:: (fun _ ->
      assert_equal [(x, atom_a); (y, VNil)]
        (update (x, atom_a) [(x, VNil); (y, VNil)]));
    "後方の変数を更新する (他の束縛は保存)" >:: (fun _ ->
      assert_equal [(x, VNil); (y, atom_b)]
        (update (y, atom_b) [(x, VNil); (y, VNil)]));
    "存在しない変数は failwith" >:: (fun _ ->
      assert_raises (Failure "Variable Z is not found") (fun () ->
        update (z, atom_a) [(x, VNil)]));
  ]

(* ---- varExp / varCom / varProc ---- *)

let test_vars =
  "varExp/varCom/varProc" >::: [
    "cons の両辺から変数を集める" >:: (fun _ ->
      assert_equal [x; y] (varExp (ECons (EVar x, EVar y))));
    "hd/tl の中の変数を集める" >:: (fun _ ->
      assert_equal [x; y] (varExp (ECons (EHd (EVar x), ETl (EVar y)))));
    "=? の両辺から変数を集める" >:: (fun _ ->
      assert_equal [x; y] (varExp (EEq (EVar x, EVar y))));
    "値リテラルに変数はない" >:: (fun _ ->
      assert_equal [] (varExp (EVal atom_a)));
    "代入は左辺と右辺の変数を集める" >:: (fun _ ->
      assert_equal [y; x] (varCom (CAsn (y, EVar x))));
    "while はガードと本体から集める" >:: (fun _ ->
      assert_equal [x; y; x] (varCom (CLoop (EVar x, [CAsn (y, EVar x)]))));
    "varProc は重複を除きソートする" >:: (fun _ ->
      let p = parse_proc "read X; Y := cons X X; X := Y; write Y" in
      assert_equal [x; y] (varProc p));
  ]

(* ---- evalExp ---- *)

let test_evalExp =
  "evalExp" >::: [
    "cons は対を作る" >:: (fun _ ->
      assert_val (VCons (atom_a, VNil)) (evalExp [] (ECons (EVal atom_a, EVal VNil))));
    "hd は対の左を返す" >:: (fun _ ->
      assert_val atom_a (evalExp [] (EHd (EVal list_ab))));
    "hd nil は nil (全域性)" >:: (fun _ ->
      assert_val VNil (evalExp [] (EHd (EVal VNil))));
    "hd 'a は nil (全域性)" >:: (fun _ ->
      assert_val VNil (evalExp [] (EHd (EVal atom_a))));
    "tl は対の右を返す" >:: (fun _ ->
      assert_val (VCons (atom_b, VNil)) (evalExp [] (ETl (EVal list_ab))));
    "tl nil は nil (全域性)" >:: (fun _ ->
      assert_val VNil (evalExp [] (ETl (EVal VNil))));
    "tl 'a は nil (全域性)" >:: (fun _ ->
      assert_val VNil (evalExp [] (ETl (EVal atom_a))));
    "=? は等しければ (nil.nil)" >:: (fun _ ->
      assert_val tt (evalExp [] (EEq (EVal atom_a, EVal atom_a))));
    "=? は等しくなければ nil" >:: (fun _ ->
      assert_val VNil (evalExp [] (EEq (EVal atom_a, EVal VNil))));
    "変数はストアから引く" >:: (fun _ ->
      assert_val atom_a (evalExp [(x, atom_a)] (EVar x)));
    "値はそのまま" >:: (fun _ ->
      assert_val atom_b (evalExp [] (EVal atom_b)));
  ]

(* ---- evalCom / evalComs ---- *)

let test_evalCom =
  "evalCom/evalComs" >::: [
    "代入はストアを更新する" >:: (fun _ ->
      assert_equal [(x, atom_a)]
        (evalCom [(x, VNil)] (CAsn (x, EVal atom_a))));
    "ガードが nil なら while は実行されない" >:: (fun _ ->
      assert_equal [(x, VNil)]
        (evalCom [(x, VNil)] (CLoop (EVar x, [CAsn (x, EVal atom_a)]))));
    "while はガードが nil になるまで繰り返す" >:: (fun _ ->
      assert_equal [(x, VNil)]
        (evalCom [(x, list_ab)] (CLoop (EVar x, [CAsn (x, ETl (EVar x))]))));
    "コマンド列は左から順に実行する" >:: (fun _ ->
      assert_equal [(x, atom_a); (y, atom_a)]
        (evalComs [(x, VNil); (y, VNil)]
           [CAsn (x, EVal atom_a); CAsn (y, EVar x)]));
    "空のコマンド列はストアを変えない" >:: (fun _ ->
      assert_equal [(x, VNil)] (evalComs [(x, VNil)] []));
  ]

(* ---- evalProc (プログラム全体の意味) ---- *)

let test_evalProc =
  "evalProc" >::: [
    "恒等プログラム" >:: (fun _ ->
      assert_val atom_a (evalProc (parse_proc "read X; write X") atom_a));
    "未初期化変数は nil" >:: (fun _ ->
      assert_val VNil (evalProc (parse_proc "read X; Y := Z; write Y") atom_a));
    "リスト反転 (p.30 Example 2.1.4)" >:: (fun _ ->
      let p = parse_proc
        "read X; Y := nil; while X do { Y := cons (hd X) Y; X := tl X; } write Y" in
      assert_val (parse_val "('3 . ('2 . ('1 . nil)))")
        (evalProc p (parse_val "('1 . ('2 . ('3 . nil)))")));
    "入力は read 変数に束縛される" >:: (fun _ ->
      assert_val (VCons (atom_a, atom_a))
        (evalProc (parse_proc "read X; Y := cons X X; write Y") atom_a));
  ]

(* ---- timeExp / timeCom / timeProc (実行時間, ch.16-19 のコストモデル) ---- *)

let test_time =
  "timeExp/timeCom/timeProc" >::: [
    "変数と値は 1 ステップ" >:: (fun _ ->
      assert_equal 1 (timeExp (EVar x));
      assert_equal 1 (timeExp (EVal atom_a)));
    "cons/hd/tl は 1 + 部分式" >:: (fun _ ->
      assert_equal 5 (timeExp (ECons (EHd (EVar x), ETl (EVar y)))));
    "=? は 1 + 両辺" >:: (fun _ ->
      assert_equal 3 (timeExp (EEq (EVar x, EVal VNil))));
    "空の本体は 0 ステップ" >:: (fun _ ->
      let (v, t) = timeProc (parse_proc "read X; write X") atom_a in
      assert_val atom_a v;
      assert_equal 0 t);
    "代入は T[E] + 1" >:: (fun _ ->
      let (_, t) = timeProc (parse_proc "read X; Y := X; write Y") atom_a in
      assert_equal 2 t);
    "ガードが nil の while は T[E] + 1" >:: (fun _ ->
      let (_, t) = timeProc (parse_proc "read X; while Z do { Z := X; } write X") atom_a in
      assert_equal 2 t);
    "ループは反復ごとに T[E] + 1 + 本体" >:: (fun _ ->
      (* 本体 3, 反復あたり 1+1+3 = 5 が 2 回, 最後の nil 判定 2 → 12 *)
      let (_, t) = timeProc (parse_proc "read X; while X do { X := tl X; } write X")
                     list_ab in
      assert_equal 12 t);
    "reverse の総時間 (手計算: 反復 10×3 + 末尾 2 + 初期化 2 = 34)" >:: (fun _ ->
      let p = parse_proc
        "read X; Y := nil; while X do { Y := cons (hd X) Y; X := tl X; } write Y" in
      let (v, t) = timeProc p (parse_val "('1 . ('2 . ('3 . nil)))") in
      assert_val (parse_val "('3 . ('2 . ('1 . nil)))") v;
      assert_equal 34 t);
    "timeProc の結果は evalProc と一致する" >:: (fun _ ->
      let p = parse_proc "read X; Y := cons X X; write Y" in
      let (v, _) = timeProc p atom_b in
      assert_val (evalProc p atom_b) v);
  ]

(* ---- パーサ/プリンタの往復 ---- *)

let test_parse_print =
  "parse/print" >::: [
    "値の往復: ネストした対" >:: (fun _ ->
      assert_equal "('a . ('b . nil))"
        (show_val (parse_val "('a . ('b . nil))")));
    "値の往復: アトムと nil" >:: (fun _ ->
      assert_val atom_a (parse_val "'a"));
  ]

let () =
  run_test_tt_main
    ("EvalWhile" >::: [
        test_update;
        test_vars;
        test_evalExp;
        test_evalCom;
        test_evalProc;
        test_time;
        test_parse_print;
      ])
