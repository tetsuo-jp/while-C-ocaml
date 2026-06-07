SED := $(shell command -v gsed || echo sed)

all: DesugarWhile

ParWhile.hs: ParWhile.y
	happy -gca $^

LexWhile.hs: LexWhile.x
	alex -g $^

ParWhile.y LexWhile.x: While.cf
	bnfc $<
	$(SED) -i -e '1i {-# LANGUAGE DeriveDataTypeable #-}' -e '1i {-# LANGUAGE DeriveGeneric #-}' AbsWhile.hs
	$(SED) -i -e '/^module AbsWhile where/a import Data.Data (Data,Typeable)\nimport GHC.Generics (Generic)' AbsWhile.hs
	$(SED) -i -e 's/deriving (C\.Eq, C\.Ord, C\.Show, C\.Read/deriving (Data, Typeable, Generic, C.Eq, C.Ord, C.Show, C.Read/' AbsWhile.hs
	$(SED) -i -e 's/deriving (Eq, Ord, Show, Read/deriving (Data, Typeable, Generic, Eq, Ord, Show, Read/' AbsWhile.hs

	touch $@

DesugarWhile: DesugarWhile.hs ParWhile.hs LexWhile.hs TransNumberWhile.hs TransPCallWhile.hs Lib.hs TransCaseWhile.hs TransAndWhile.hs TransConspWhile.hs TransBlkWhile.hs TransList.hs
	ghc --make $< -o $@

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi DesugarWhile

distclean: clean
	-rm -f DocWhile.* LexWhile.* ParWhile.* LayoutWhile.* SkelWhile.* PrintWhile.* TestWhile.* AbsWhile.* TestWhile ErrM.* SharedString.* ComposOp.* While.dtd XMLWhile.* DesugarWhile

test: DesugarWhile
	./DesugarWhile ../../examples/desugar/reverse-indent.while
	./DesugarWhile ../../examples/desugar/bool.while
	./DesugarWhile ../../examples/desugar/list.while
	./DesugarWhile ../../examples/desugar/and.while
	./DesugarWhile ../../examples/desugar/numbers.while
	./DesugarWhile ../../examples/desugar/if.while
	./DesugarWhile ../../examples/desugar/inline1.while
	./DesugarWhile ../../examples/desugar/inline2.while
	./DesugarWhile ../../examples/desugar/inline3.while
	./DesugarWhile ../../examples/desugar/inline4.while
	./DesugarWhile ../../examples/desugar/inline5.while
	./DesugarWhile ../../examples/desugar/reverse-case.while
