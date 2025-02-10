GHC=ghc
BNFC=bnfc
CCOMPILER=gcc

CFLAGS=-O3 -Wall -Wextra

SRC_DIR=src
OUTPUT_DIR=build
LIB_DIR=lib
GRAMMAR_DIR=$(OUTPUT_DIR)/grammar

SRC= $(SRC_DIR)/Optimalizations.hs $(SRC_DIR)/LCSE_GCSE.hs $(SRC_DIR)/SSA.hs $(SRC_DIR)/RemoveSSA.hs $(SRC_DIR)/AsmGenerator.hs $(SRC_DIR)/Intermediate.hs $(SRC_DIR)/IntermediateTypes.hs $(SRC_DIR)/ConstantPropagation.hs $(SRC_DIR)/UnusedCode.hs $(SRC_DIR)/DeadCode.hs $(SRC_DIR)/UnusedBlocks.hs $(SRC_DIR)/TypeChecker $(SRC_DIR)/TypeCheckerTypes.hs $(SRC_DIR)/Common.hs $(GRAMMAR_DIR)/Grammar/Abs.hs $(GRAMMAR_DIR)/Grammar/Lex.hs $(GRAMMAR_DIR)/Grammar/Par.hs $(GRAMMAR_DIR)/Grammar/Print.hs

.PHONY: all clean lib grammar distclean clean_lib clean_grammar

all: lib grammar
	mkdir -p $(OUTPUT_DIR)
	$(GHC) $(CFLAGS) $(SRC_DIR)/Main.hs $(SRC) -o $(OUTPUT_DIR)/latc_x86_64 -odir $(OUTPUT_DIR) -hidir $(OUTPUT_DIR)

lib:
	mkdir -p $(LIB_DIR)
	$(CCOMPILER) $(CFLAGS) -c $(SRC_DIR)/runtime.c -o $(LIB_DIR)/runtime.o

grammar:
	mkdir -p $(GRAMMAR_DIR)
	$(BNFC) --functor --haskell -d -m $(SRC_DIR)/Grammar.cf -o $(GRAMMAR_DIR)
	cd $(GRAMMAR_DIR); make

distclean: clean clean_lib clean_grammar
	-rmdir -p $(OUTPUT_DIR)

clean:
	rm -f $(OUTPUT_DIR)/latc_x86_64 $(OUTPUT_DIR)/*.o $(OUTPUT_DIR)/*.hi $(OUTPUT_DIR)/Grammar/*.o $(OUTPUT_DIR)/Grammar/*.hi

clean_lib:
	-rm -f $(LIB_DIR)/runtime.o
	-rmdir -p $(LIB_DIR)

clean_grammar:
	-cd $(GRAMMAR_DIR); make distclean
	-rmdir -p $(GRAMMAR_DIR)
