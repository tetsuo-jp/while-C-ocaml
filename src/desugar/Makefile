all: DesugarWhile

ParWhile.hs: ParWhile.y
	happy -gca $^

LexWhile.hs: LexWhile.x
	alex -g $^

ParWhile.y LexWhile.x: While.cf
	bnfc $<
	gsed -i -e 's/(Eq, Ord, Show, Read)/(Eq, Ord, Show, Read, Data, Typeable, Generic)/' AbsWhile.hs
	gsed -i -e '/-- Haskell module generated by the BNF converter/d' AbsWhile.hs
	gsed -i -e '1i {-# LANGUAGE DeriveDataTypeable #-}' AbsWhile.hs
	gsed -i -e '2i {-# LANGUAGE DeriveGeneric #-}' AbsWhile.hs
	gsed -i -e '8i import Data.Data (Data,Typeable)' AbsWhile.hs
	gsed -i -e '9i import GHC.Generics (Generic)' AbsWhile.hs

	touch $@

DesugarWhile: DesugarWhile.hs ParWhile.hs LexWhile.hs TransNumberWhile.hs TransPCallWhile.hs Lib.hs TransCaseWhile.hs TransAndWhile.hs
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
