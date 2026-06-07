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

TRANS = Desugar.hs TransNumberWhile.hs TransPCallWhile.hs Lib.hs TransCaseWhile.hs TransAndWhile.hs TransConspWhile.hs TransBlkWhile.hs TransList.hs

DesugarWhile: DesugarWhile.hs ParWhile.hs LexWhile.hs $(TRANS)
	ghc --make $< -o $@

# ユニットテスト (hspec)。HPC 用に -fhpc でビルドし別ディレクトリに出力する
DesugarSpec: DesugarSpec.hs ParWhile.hs LexWhile.hs $(TRANS)
	ghc --make -fhpc $< -odir .hpcbuild -hidir .hpcbuild -o $@

unittest: DesugarSpec
	rm -f DesugarSpec.tix
	./DesugarSpec

# C0 (文) カバレッジ: 変換モジュールのみを対象に報告する
HPC_MODULES = --include=Lib --include=Desugar --include=TransAndWhile \
              --include=TransBlkWhile --include=TransCaseWhile \
              --include=TransConspWhile --include=TransList \
              --include=TransNumberWhile --include=TransPCallWhile

coverage: unittest
	hpc report DesugarSpec.tix $(HPC_MODULES)

coverage-html: unittest
	hpc markup DesugarSpec.tix $(HPC_MODULES) --destdir=coverage-html

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi DesugarWhile DesugarSpec DesugarSpec.tix
	-rm -rf .hpcbuild .hpc coverage-html

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
