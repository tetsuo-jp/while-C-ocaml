#OCAMLC=ocamlfind ocamlc -g -package extlib -linkpkg
OCAMLC=ocamlfind ocamlopt -warn-error A -package extlib -linkpkg

all: ri

ri: BNFC_Util.ml AbsRwhile.ml ShowRwhile.ml PrintRwhile.ml ParRwhile.mli ParRwhile.ml LexRwhile.ml InvRwhile.ml MacroRwhile.ml EvalRwhile.ml Program2DataRwhile.ml Main.ml
	$(OCAMLC) -o $@ $^

LexRwhile.ml: LexRwhile.mll
	ocamllex LexRwhile.mll

ParRwhile.mli: ParRwhile.mly
	ocamlyacc ParRwhile.mly

BNFC_Util.ml: Rwhile.cf
	bnfc -ocaml Rwhile.cf
	touch BNFC_Util.ml

clean:
	-rm -f *.cmi *.cmo *.o *.mli *.cmx ri

distclean: clean
	-rm -f LexRwhile.* ParRwhile.* LayoutRwhile.* SkelRwhile.* PrintRwhile.* ShowRwhile.* TestRwhile.* AbsRwhile.* TestRwhile BNFC_Util.ml\
	 	ri

install: ri
	cp -f ./ri ../web/

test: ri
	./ri ../examples/rep.rwhile ../examples/list123.val
	./ri -inverse ../examples/ri.rwhile
	./ri -exp ../examples/ri.rwhile
	./ri -p2d ../examples/reverse.rwhile
	./ri ../examples/ri.rwhile ../examples/id_and_nil.p_val
	./ri ../examples/ri.rwhile ../examples/reverse_and_list123.p_val
	./ri ../examples/ri.rwhile ../examples/piorder.p_val
	# ./ri ../examples/ri.rwhile ../examples/ri_reverse_list123.p_val    # 6.6s かかった
	# ./ri ../examples/ri.rwhile ../examples/ri_piorder_atree.p_val      # 1分以上かかるかも
	# ./ri ../examples/ri.rwhile ../examples/ri_ri_reverse_list123.p_val # 1:31:39.23もかかった
