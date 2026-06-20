OCAMLC=ocamlc -g
# OCAMLC=ocamlfind ocamlopt -warn-error A

all: while

TestWhile: BNFC_Util.ml AbsWhile.ml SkelWhile.ml ShowWhile.ml PrintWhile.ml ParWhile.mli ParWhile.ml LexWhile.ml TestWhile.ml
	ocamlc -o TestWhile $^

while: BNFC_Util.ml AbsWhile.ml ShowWhile.ml PrintWhile.ml ParWhile.mli ParWhile.ml LexWhile.ml EvalWhile.ml Main.ml
	$(OCAMLC) -o $@ $^

# ネイティブコンパイル版 (自己解釈などの重い実行向け)
while.opt: BNFC_Util.ml AbsWhile.ml ShowWhile.ml PrintWhile.ml ParWhile.mli ParWhile.ml LexWhile.ml EvalWhile.ml Main.ml
	ocamlopt -O2 -o $@ $^

# programs-as-data ⌜p⌝ をネイティブ直接実行する高速インタプリタ
# (mix.while の大きな残余 comp2/comp3 の検証用; universal.while より高速)
rundata: BNFC_Util.ml AbsWhile.ml ShowWhile.ml PrintWhile.ml ParWhile.mli ParWhile.ml LexWhile.ml RunData.ml
	ocamlopt -O2 -o $@ $^

LexWhile.ml: LexWhile.mll
	ocamllex $^

ParWhile.mli: ParWhile.mly
	ocamlyacc ParWhile.mly

BNFC_Util.ml: While.cf
	bnfc --ocaml --force $^
	touch BNFC_Util.ml

clean:
	-rm -f *.cmi *.cmo *.o *.mli *.cmx while

distclean: clean
	-rm -f LexWhile.* ParWhile.* LayoutWhile.* SkelWhile.* PrintWhile.* ShowWhile.* TestWhile.* AbsWhile.* TestWhile BNFC_Util.ml

install: while
	cp -f ./while ../../web/
	cp -f ../../examples/*.while ../../web/examples/
	cp -f ../../examples/*.val ../../web/examples/
	chmod 777 ../../web/data
	chmod 777 ../../web/programs

uninstall:
	rm -f ../../web/while
	rm -f ../../web/examples/*.while
	rm -f ../../web/examples/*.val

test: while
	./while ../../examples/reverse.while ../../examples/list123.val

# ユニットテスト (OUnit2)。要: opam install ounit2
unittest: BNFC_Util.ml AbsWhile.ml ShowWhile.ml PrintWhile.ml ParWhile.mli ParWhile.ml LexWhile.ml EvalWhile.ml EvalWhileTest.ml
	ocamlfind ocamlc -package ounit2 -linkpkg -o EvalWhileTest $^
	./EvalWhileTest

# 残余プログラム ⌜p⌝ の簡約 (no-op 自己代入除去・semi 平坦化)
simp: BNFC_Util.ml AbsWhile.ml ShowWhile.ml PrintWhile.ml ParWhile.mli ParWhile.ml LexWhile.ml Simp.ml
	ocamlopt -O2 -o $@ $^

# 束縛時刻解析 (offline PE の第一構成要素)
bta: BNFC_Util.ml AbsWhile.ml ShowWhile.ml PrintWhile.ml ParWhile.mli ParWhile.ml LexWhile.ml Bta.ml
	ocamlopt -O2 -o $@ $^

# 汎用 cogen (注釈追従の offline 特殊化器)
cogen: BNFC_Util.ml AbsWhile.ml ShowWhile.ml PrintWhile.ml ParWhile.mli ParWhile.ml LexWhile.ml Cogen.ml
	ocamlopt -O2 -o $@ $^
