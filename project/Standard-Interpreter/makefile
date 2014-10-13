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
values.mli \
values.ml \
env.mli \
env.ml \
aexp.mli \
aexp.ml \
bexp.mli \
bexp.ml \
smallstep.mli \
smallstep.ml \
bigstep.mli \
bigstep.ml \
main.ml

.PHONY : help
help :
	@echo ""
	@echo "make help        : this help"
	@echo "make compile     : compile"
	@echo "./a.out filename : execute"
	@echo "make examples    : execute all examples"
	@echo "make clean       : remove auxilairy files"
	@echo ""

.PHONY : compile
compile:
	ocamlyacc parser.mly
	ocamllex lexer.mll
	ocamlc $(SOURCES)

.PHONY : examples
examples :
	./a.out ../Examples/example00.sil
	./a.out ../Examples/example01.sil
	./a.out ../Examples/example02.sil
	./a.out ../Examples/example03.sil
	./a.out ../Examples/example04.sil
	./a.out ../Examples/example05.sil
	./a.out ../Examples/example06.sil
	./a.out ../Examples/example07.sil
	./a.out ../Examples/example08.sil
	./a.out ../Examples/example09.sil
	./a.out ../Examples/example10.sil
	./a.out ../Examples/example11.sil
	
.PHONY : clean
clean :
	/bin/rm -f *.cmi *.cmo *~ a.out lexer.ml parser.ml

.PHONY : delete
delete : clean	
	/bin/rm -f parser.mli
