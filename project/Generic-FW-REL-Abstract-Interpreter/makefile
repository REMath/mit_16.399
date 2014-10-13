
EXAMPLES = ../Examples

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
linear_Syntax.mli \
linear_Syntax.ml \
abstract_To_Linear_Syntax.mli \
abstract_To_Linear_Syntax.ml \
lpretty_Print.mli \
lpretty_Print.ml \
aenv.mli \
aenv.ml \
abexp.mli \
abexp.ml \
fixpoint.mli \
fixpoint.ml \
acom.mli \
acom.ml \
main.ml

.PHONY : help
help :
	@echo ""
	@echo "Forward relational static analysis:"
	@echo "make [help]      : this help"
	@echo "make pol         : polyhedral analysis"
	@echo "./a.out file.sil : analyze file.sil"
	@echo "make examples    : analyze all examples"
	@echo "make clean       : remove auxiliary files"
	@echo ""

.PHONY : pol
pol:
	@/bin/rm -f aenv.ml
	@ln -s ../Relational-FW/Polyhedra/aenv.ml aenv.ml
	@echo "Polyhedral analysis"
	ocamlyacc parser.mly
	ocamllex lexer.mll
	ocamlc -custom -I /usr/local/lib -I /usr/local/lib/ocaml \
	  -cclib "-L/usr/local/lib -L/usr/local/lib/ocaml -lpolkag_caml \
	  -lpolkag -lgmp -lcamlidl" \
	  polka.cma ${SOURCES}

include ${EXAMPLES}/makefile
	
.PHONY : clean
clean :
	/bin/rm -f *.cmi *.cmo *~ a.out lexer.ml parser.mli parser.ml 