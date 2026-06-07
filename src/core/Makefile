OCAMLC=ocamlc -g
# OCAMLC=ocamlfind ocamlopt -warn-error A

all: while

TestWhile: BNFC_Util.ml AbsWhile.ml SkelWhile.ml ShowWhile.ml PrintWhile.ml ParWhile.mli ParWhile.ml LexWhile.ml TestWhile.ml
	ocamlc -o TestWhile $^

while: BNFC_Util.ml AbsWhile.ml ShowWhile.ml PrintWhile.ml ParWhile.mli ParWhile.ml LexWhile.ml EvalWhile.ml Main.ml
	$(OCAMLC) -o $@ $^

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
