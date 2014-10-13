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
avalues.mli \
avalues.ml \
aenv.mli \
aenv.ml \
fixpoint.mli \
fixpoint.ml \
aaexp.mli \
aaexp.ml \
baexp.mli \
baexp.ml \
abexp.mli \
abexp.ml \
acom.mli \
acom.ml \
main.ml

.PHONY : help
help :
	@echo ""
	@echo "make help        : this help"
	@echo "make trace-fp    : trace fixpoint iterates"
	@echo "make untrace-fp  : don't trace fixpoint iterates"
	@echo "make trace       : trace all"
	@echo "make untrace     : don't trace at all"
	@echo "make compile     : compile"
	@echo "./a.out filename : execute"
	@echo "make examples    : execute the examples"
	@echo "make errors      : execute the examples with runtime errors"
	@echo "make clean       : remove auxiliary files"
	@echo ""

.PHONY : trace-fp preparetrace-fp
trace-fp: preparetrace-fp compile
	@echo "tracing mode"
preparetrace-fp:
	@/bin/rm -f fixpoint.ml
	@ln -s fixpoint_printing_iterates.ml fixpoint.ml

.PHONY : untrace-fp prepareuntrace-fp
untrace-fp: prepareuntrace-fp compile
	@echo "no tracing, recompile!"
prepareuntrace-fp:
	@/bin/rm -f fixpoint.ml
	@ln -s fixpoint_no_printing.ml fixpoint.ml


.PHONY : trace preparetrace
trace: preparetrace-fp preparetrace compile
	@echo "tracing mode"
preparetrace:
	@/bin/rm -f aaexp.ml
	@ln -s aaexp-trace.ml aaexp.ml
	@/bin/rm -f baexp.ml
	@ln -s baexp-trace.ml baexp.ml
	@/bin/rm -f abexp.ml
	@ln -s abexp-trace.ml abexp.ml

.PHONY : untrace prepareuntrace
untrace: prepareuntrace-fp prepareuntrace compile
	@echo "no tracing, recompile!"
prepareuntrace:
	@/bin/rm -f aaexp.ml
	@ln -s aaexp-untrace.ml aaexp.ml
	@/bin/rm -f baexp.ml
	@ln -s baexp-untrace.ml baexp.ml
	@/bin/rm -f abexp.ml
	@ln -s abexp-untrace.ml abexp.ml

.PHONY : compile
compile:
	ocamlyacc parser.mly
	ocamllex lexer.mll
#	ocamlc -i $(SOURCES) # to print types
	ocamlc $(SOURCES)

.PHONY : examples
examples :
	./a.out ../Examples/example00.sil
	./a.out ../Examples/example01.sil
	./a.out ../Examples/example02.sil
	./a.out ../Examples/example03.sil
	./a.out ../Examples/example04.sil
	./a.out ../Examples/example05.sil
	./a.out ../Examples/example07.sil
	./a.out ../Examples/example13.sil
	./a.out ../Examples/example14.sil
	
.PHONY : errors
errors :
	./a.out ../Examples/example06.sil
	./a.out ../Examples/example08.sil
	./a.out ../Examples/example09.sil
	./a.out ../Examples/example10.sil
	./a.out ../Examples/example11.sil
	./a.out ../Examples/example16.sil

.PHONY :
clean :
	/bin/rm -f *.cmi *.cmo *~ a.out lexer.ml parser.mli parser.ml
