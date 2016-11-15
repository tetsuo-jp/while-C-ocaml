all: DesugarWhile

ParWhile.hs: ParWhile.y
	happy -gca $^

LexWhile.hs: LexWhile.x
	alex -g $^

ParWhile.y LexWhile.x: While.cf
	bnfc $<
	touch $@

DesugarWhile: DesugarWhile.hs ParWhile.hs LexWhile.hs TransNumberWhile.hs TransPCallWhile.hs Lib.hs TransCaseWhile.hs TransAndWhile.hs
	ghc --make $< -o $@

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi

distclean: clean
	-rm -f DocWhile.* LexWhile.* ParWhile.* LayoutWhile.* SkelWhile.* PrintWhile.* TestWhile.* AbsWhile.* TestWhile ErrM.* SharedString.* ComposOp.* While.dtd XMLWhile.* DesugarWhile

test: DesugarWhile
	./DesugarWhile ../../examples/desugar/reverse-indent.while
	./DesugarWhile ../../examples/desugar/bool.while
	./DesugarWhile ../../examples/desugar/list.while
	./DesugarWhile ../../examples/desugar/and.while
