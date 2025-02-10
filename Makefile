GHC=ghc
BNFC=bnfc
CCOMPILER=gcc

CFLAGS=-O3 -Wall -Wextra

SRC= src/Optimalizations.hs src/LCSE_GCSE.hs src/SSA.hs src/RemoveSSA.hs src/AsmGenerator.hs src/Intermediate.hs src/IntermediateTypes.hs src/ConstantPropagation.hs src/UnusedCode.hs src/DeadCode.hs src/UnusedBlocks.hs src/TypeChecker src/TypeCheckerTypes.hs src/Common.hs src/Grammar/Abs.hs src/Grammar/Lex.hs src/Grammar/Par.hs src/Grammar/Print.hs

.PHONY: all clean lib grammar distclean clean_lib clean_grammar

all: lib grammar
	$(GHC) $(CFLAGS) src/Main.hs $(SRC) -o latc_x86_64

lib:
	$(CCOMPILER) $(CFLAGS) -c src/runtime.c -o lib/runtime.o

grammar:
	$(BNFC) --functor --haskell -d -m src/Grammar.cf -o src/
	cd src; make

distclean: clean clean_lib clean_grammar

clean:
	rm -f latc_x86_64 src/*.o src/*.hi

clean_lib:
	-rm -f lib/runtime.o
	-rmdir -p lib/

clean_grammar:
	-cd src; make distclean
