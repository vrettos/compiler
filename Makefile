.PHONY: clean distclean pack count

# OS type: Linux/Win DJGPP
ifdef OS
   EXE=.exe
else
   EXE=
endif

EXEFILE=llamac$(EXE)
MLFILES=Hashcons.ml Identifier.ml Error.ml Types.ml Ast.ml Parser.ml Lexer.ml \
  Structs.ml Symbol.ml ErrorMsgs.ml LibraryFunctions.ml Constraints.ml Typeinfer.ml \
  Semantics.ml Quads.ml GenerateQuads.ml ControlFlowGraph.ml IterativeEquationsSolver.ml \
  ReachingDefinitions.ml ConstructDuUdChains.ml DeadCodeElimination.ml Propagation.ml \
  ConstantFolding.ml UnreachableCodeElimination.ml BranchSimplification.ml RemoveUnusedTemps.ml \
  RevCopyPropagation.ml Optimizations.ml Asm.ml GenerateAsm.ml Print.ml Main.ml
MLIFILES=Hashcons.mli Identifier.mli Error.mli Types.mli Ast.mli Parser.mli Lexer.mli \
  Structs.mli Symbol.mli ErrorMsgs.mli LibraryFunctions.mli Constraints.mli Typeinfer.mli \
  Semantics.mli Quads.mli GenerateQuads.mli ControlFlowGraph.mli IterativeEquationsSolver.mli \
  ReachingDefinitions.mli ConstructDuUdChains.mli DeadCodeElimination.mli Propagation.mli \
  ConstantFolding.mli UnreachableCodeElimination.mli BranchSimplification.mli RemoveUnusedTemps.mli \
  RevCopyPropagation.mli Optimizations.mli Asm.mli GenerateAsm.mli Print.mli
LIBS=str.cma
CMOFILES=$(patsubst %.ml,%.cmo,$(MLFILES))
CMIFILES=$(patsubst %.ml,%.cmi,$(MLFILES))
CMXFILES=$(patsubst %.ml,%.cmx,$(MLFILES))
OBJFILES=$(patsubst %.ml,%.o,$(MLFILES))
PARSERFILES=Parser.ml Parser.mli Parser.output Lexer.ml
SRCFILES=Makefile extend.ml Lexer.mll Parser.mly \
  $(filter-out Parser.% Lexer.%,$(MLFILES)) \
  $(filter-out Parser.%,$(MLIFILES))

CAMLP5_FLAGS=-pp "camlp5o ./extend.cmo"
OCAMLC_FLAGS=-g
OCAMLOPT_FLAGS=
OCAMLC=ocamlc $(OCAMLC_FLAGS)
OCAMLOPT=ocamlopt $(OCAMLOPT_FLAGS)
OCAMLDEP=ocamldep
INCLUDES=

all: $(EXEFILE)

extend.cmo: extend.ml
	$(OCAMLC) -pp "camlp5o pa_extend.cmo q_MLast.cmo" -I +camlp5 -c $<

%.cmo: %.ml %.mli extend.cmo
	$(OCAMLC) $(CAMLP5_FLAGS) -c $<

%.cmx: %.ml extend.cmo
	$(OCAMLOPT) $(CAMLP5_FLAGS) -c $<

%.cmi: %.mli extend.cmo
	$(OCAMLC) $(CAMLP5_FLAGS) -c $<

%.cmo %.cmi: %.ml extend.cmo
	$(OCAMLC) $(CAMLP5_FLAGS) -c $<

.PHONY: all clean count depend

$(EXEFILE): Parser.mli Lexer.ml $(CMOFILES)
	$(OCAMLC) -o $@ $(LIBS) $(CMOFILES)

Parser.ml Parser.mli: Parser.mly
	ocamlyacc -v Parser.mly

Lexer.ml: Lexer.mll
	ocamllex Lexer.mll

-include .depend

depend: $(MLFILES) $(MLIFILES) extend.cmo
	$(OCAMLDEP) $(CAMLP5_FLAGS) $(INCLUDES) \
          $(filter-out extend.cmo,$^) > .depend

clean:
	$(RM) $(CMXFILES) $(CMOFILES) $(CMIFILES) $(OBJFILES) $(EXEFILES) \
           extend.cmi extend.cmo \
           $(patsubst %,%.cm?,$(EXEFILES)) $(PARSERFILES) pplib.cma *~

distclean: clean
	$(RM) $(EXEFILE) llamac$(EXE) .depend

pack: clean
	tar cvfz compiler.tar.gz $(SRCFILES)

count:
	wc -l $(SRCFILES)
