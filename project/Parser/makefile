# makefile

SOURCES = \
symbol_Table.mli \
symbol_Table.ml \
variables.mli \
variables.ml \
abstract_Syntax.ml \
concrete_To_Abstract_Syntax.mli \
concrete_To_Abstract_Syntax.ml \
labels.mli \
labels.ml \
parser.mli \
parser.ml \
lexer.ml \
program_To_Abstract_Syntax.mli \
program_To_Abstract_Syntax.ml \
pretty_Print.mli \
pretty_Print.ml \
main.ml

.PHONY : help
help :
	@echo ""
	@echo "make help        : this help"
	@echo "make compile     : compile"
	@echo "./a.out filename : execute"
	@echo "make examples    : run examples"
	@echo "make clean       : remove auxiliary files"
	@echo ""

.PHONY : compile
compile:
	ocamlyacc parser.mly
	ocamllex lexer.mll
	ocamlc $(SOURCES)

.PHONY : examples
examples :
	./a.out ../Examples/example01.sil

.PHONY : clean
clean :
	/bin/rm -f *.cmi *.cmo *~ a.out lexer.ml parser.ml

.PHONY : delete
delete : clean	
	/bin/rm -f parser.mli
