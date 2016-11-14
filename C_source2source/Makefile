# Copyright (c) 2013-2016, The IMDEA Software Institute and
# Copyright (c) 2013-2016, Universidad Politécnica de Madrid

# See LICENSE.txt and AUTHORS.txt for licensing and authorship


NAME = polca_s2s
NAME_EXT = polca_s2s_ext
TESTER = rules_tester
AST = ast
EXPANDER = expand
PRAGMA_READER = pragma_reader
TRANS = polca_trans
INIT = polca_init
APPLY = polca_apply
ROLLUP = polca_rollup
PRETTY = polca_pretty
RAN_ORACLE = random_oracle
FEAT_EXTR = polca_features

FILES = Makefile Main.hs Rul2Has.hs RulesLib.hs RulesEmpty.hs README README.md LICENSE.txt AUTHORS.txt rules.c vec_mult.c dps.c Translator.hs PragmaPolcaLib.hs Compilable.hs CompilableExt.hs CompileRules.hs RandomOracle.hs FeaturesExtractor.hs TestRules.hs TransExe.hs InitTransformation.hs AppTransformation.hs IsRollable.hs PragmaPolcaReader.hs PrettyPrinter.hs Ast.hs Expander.hs polcas2s.cabal Setup.hs opencl_main_funs
DEMOFILES = demo/rgb_filter_input.c demo/rgb_filter_input_maxjready.c demo/ppmImage.c demo/ppmImage.h demo/ppmImage.c demo/ppmImage.c demo/lodepng.h demo/lodepng.c 
CETUSMODIFIED = ../cetus/src/cetus/analysis/CallGraph.java ../cetus/src/cetus/analysis/DDGraph.java ../cetus/src/cetus/analysis/LoopTools.java ../cetus/src/cetus/analysis/IPPointsToAnalysis.java ../cetus/src/cetus/exec/Driver.java ../cetus/src/cetus/hir/STMLAnnotation.java ../cetus/src/cetus/hir/TranslationUnit.java 
CETUSFILES = ../cetus.tar.gz cetus/cetus.sh cetus/cetus_stml.sh cetus/options.cetus ../cetus/polca_changes.txt 

GHC_FLAGS = -fno-warn-tabs -XFlexibleContexts

# make executable (Default option)
exe: 
	ghc -main-is Compilable Compilable.hs -o $(NAME) $(GHC_FLAGS)

exe_ext: 
	ghc -main-is CompilableExt CompilableExt.hs -o $(NAME_EXT) $(GHC_FLAGS)		

all:
	make exe
	make exe_ext
	make tester
	make ast
	make expand_ann
	make translator
	make interface
	make random_oracle

# compile rules using a given file
# Usage: make rules FILE=rules_source_file.c
rules: 
	ghc -main-is CompileRules CompileRules.hs -o stml2has $(GHC_FLAGS)
	./stml2has $(FILE)

# compile rules using defaul rules set, i.e. "rules.c"
rules_def: 
	ghc -main-is CompileRules CompileRules.hs -o stml2has $(GHC_FLAGS)
	./stml2has "rules"

# make executable optimized
exe_op: 
	ghc -O2 -main-is Compilable Compilable.hs -o $(NAME) $(GHC_FLAGS)

# make executable with profiling
exe_pro: 
	ghc -prof -fprof-auto -rtsopts -main-is Compilable Compilable.hs -o $(NAME) $(GHC_FLAGS)

# make executable optimized and with profiling
exe_op_pro: 
	ghc -O2 -prof -fprof-auto -rtsopts -main-is Compilable Compilable.hs -o $(NAME) $(GHC_FLAGS)

# run profiling of a file
# Usage: make pro FILE=source_file.c
# Result store in $(NAME).prof
pro: 
	./$(NAME) $(FILE) +RTS -p $(GHC_FLAGS)

# Build a rules tester executable
tester:
	ghc -main-is TestRules TestRules.hs -o $(TESTER) $(GHC_FLAGS)

# Build a executable that builds a file with the AST of a c file
ast:
	ghc -main-is Ast Ast.hs -o $(AST) $(GHC_FLAGS)

# Build a executable that expands POLCA annotations
expand_ann:
	ghc -main-is Expander Expander.hs -o $(EXPANDER) $(GHC_FLAGS)

# Build the translator direct access application
translator:
	ghc -main-is TransExe TransExe.hs -o $(TRANS) $(GHC_FLAGS)

# A random oracle to be used as a testing oracle
random_oracle:
	ghc -main-is RandomOracle RandomOracle.hs -o $(RAN_ORACLE) $(GHC_FLAGS)

# Build the interface for external tools
interface:
	# Build an executable that returns a JSON with the pragmas in a program
	ghc -main-is PragmaPolcaReader PragmaPolcaReader.hs -o $(PRAGMA_READER) $(GHC_FLAGS)
	# Build an executable that returns 1/0 depending on whether the statements are rollable or not
	ghc -main-is IsRollable IsRollable.hs -o $(ROLLUP) $(GHC_FLAGS)
	# Executables to transform the program through external tools 
	ghc -main-is InitTransformation InitTransformation.hs -o $(INIT) $(GHC_FLAGS)
	ghc -main-is AppTransformation AppTransformation.hs -o $(APPLY) $(GHC_FLAGS)
	# Reformating of the input code
	ghc -main-is PrettyPrinter PrettyPrinter.hs -o $(PRETTY) $(GHC_FLAGS)
	# Features extractor to be used by the machine learning component
	ghc -main-is FeaturesExtractor FeaturesExtractor.hs -o $(FEAT_EXTR) $(GHC_FLAGS)

# Do all the previous configuration needed before to build the executables
configure:
	make cetus_install
	cabal install
	rm -rf dist

# Build Cetus system libraries to be used by the tool
cetus_install:
	tar -xvzf cetus/cetus.tar.gz -C cetus
	cp cetus/mod/CallGraph.java cetus/cetus-1.4.3/src/cetus/analysis/CallGraph.java
	cp cetus/mod/DDGraph.java cetus/cetus-1.4.3/src/cetus/analysis/DDGraph.java
	cp cetus/mod/LoopTools.java cetus/cetus-1.4.3/src/cetus/analysis/LoopTools.java
	cp cetus/mod/IPPointsToAnalysis.java cetus/cetus-1.4.3/src/cetus/analysis/IPPointsToAnalysis.java
	cp cetus/mod/Driver.java cetus/cetus-1.4.3/src/cetus/exec/Driver.java
	cp cetus/mod/STMLAnnotation.java cetus/cetus-1.4.3/src/cetus/hir/STMLAnnotation.java 
	cp cetus/mod/TranslationUnit.java cetus/cetus-1.4.3/src/cetus/hir/TranslationUnit.java
	cd cetus/cetus-1.4.3/; ./build.sh bin 
	cp cetus/cetus-1.4.3/lib/cetus.jar cetus/
	cp cetus/cetus-1.4.3/lib/antlr.jar cetus/
	cp cetus/cetus-1.4.3/lib/checkstyle.jar cetus/
	cp cetus/cetus-1.4.3/lib/rsyntaxtextarea.jar cetus/
	rm -rf cetus/cetus-1.4.3

# make tar file with all neede files
tar:
	rm -rf $(NAME)
	mkdir $(NAME)
	cp $(FILES) $(NAME)
	mkdir $(NAME)/cetus
	cp $(CETUSFILES) $(NAME)/cetus/
	mkdir $(NAME)/cetus/mod
	echo "" > $(NAME)/cetus/output.txt
	cp $(CETUSMODIFIED) $(NAME)/cetus/mod/
	mv $(NAME)/RulesEmpty.hs $(NAME)/Rules.hs
	mkdir $(NAME)/demo
	cp $(DEMOFILES) $(NAME)/demo/
	tar cfz $(NAME).tgz $(NAME)
	rm -rf $(NAME)

clean: 
	@rm -rf $(NAME)
	@rm -rf $(NAME_EXT)
	@rm -rf $(TESTER)
	@rm -rf $(AST)
	@rm -rf $(EXPANDER)
	@rm -rf $(PRAGMA_READER)
	@rm -rf $(TRANS)
	@rm -rf $(INIT)
	@rm -rf $(APPLY)
	@rm -rf $(ROLLUP)
	@rm -rf $(PRETTY)
	@rm -rf $(RAN_ORACLE)
	@rm -rf *.hi
	@rm -rf *.dyn_o
	@rm -rf *.dyn_hi


