# makefile

SHELL = tcsh

EXAMPLES = ../Examples

SOURCES_SINGLE_DOMAIN = \
trace.mli \
trace.ml \
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
aaexp.mli \
aaexp.ml \
baexp.mli \
baexp.ml \
fixpoint.mli \
fixpoint.ml \
abexp.mli \
abexp.ml \
acom.mli \
acom.ml \
main.ml

SOURCES_BINARY_REDUCED_PRODUCT = \
trace.mli \
trace.ml \
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
avalues1.mli \
avalues1.ml \
avalues2.mli \
avalues2.ml \
red12.mli \
red12.ml \
avalues.mli \
avalues.ml \
aenv.mli \
aenv.ml \
aaexp.mli \
aaexp.ml \
baexp.mli \
baexp.ml \
fixpoint.mli \
fixpoint.ml \
abexp.mli \
abexp.ml \
acom.mli \
acom.ml \
main.ml

SOURCES_TERNARY_REDUCED_PRODUCT = \
trace.mli \
trace.ml \
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
avalues1.mli \
avalues1.ml \
avalues2.mli \
avalues2.ml \
avalues3.mli \
avalues3.ml \
red12.mli \
red12.ml \
red23.mli \
red23.ml \
red13.mli \
red13.ml \
red123.mli \
red123.ml \
avalues.mli \
avalues.ml \
aenv.mli \
aenv.ml \
aaexp.mli \
aaexp.ml \
baexp.mli \
baexp.ml \
fixpoint.mli \
fixpoint.ml \
abexp.mli \
abexp.ml \
acom.mli \
acom.ml \
main.ml

.PHONY : help
help :
	@echo ""
	@echo "Forward non-relational static analysis:"
	@echo "make help        : this help"
	@echo "1) reset:"
	@echo "make reset       : erase all mode choices"
	@echo "2) choose tracing mode:"
	@echo "make trace       : tracing all"
	@echo "make traceaexp   : tracing arithmetic expressions"
	@echo "make tracebexp   : tracing boolean expressions"
	@echo "make tracecom    : tracing commands"
	@echo "make tracered    : tracing ternary reductions"
	@echo "make notrace     : no tracing"
	@echo "3) choose abstract interpreter mode:"
	@echo "3a) relational/non-relational analysis:"
	@echo "make r           : relational abstract interpretor"
	@echo "make nr          : non-relational abstract interpretor"
	@echo "3b) boolean expressions:"
	@echo "make fbool       : forward analysis"
	@echo "make fbbool      : forward/backward analysis"
	@echo "make fbrbool     : forward/backward reductive analysis"
	@echo "3c) arithmetic expressions:"
	@echo "make fassign     : forward analysis"
	@echo "make fbassign    : forward/backward analysis"
	@echo "4) choose static analysis and compile analyzer:"
	@echo "make err         : error analysis"
	@echo "make iss         : initialization and simple sign analysis"
	@echo "make int         : interval analysis"
	@echo "make par         : parity analysis"
	@echo "make err-int     : error x interval analysis"
	@echo "make iss-int     : initialization and simple sign x interval analysis"
	@echo "make par-int     : parity x interval analysis"
	@echo "make par-iss     : parity x initialization and simple sign analysis"
	@echo "make par-iss-int : parity x initialization and simple sign analysis x interval"
	@echo "5) analyze:"
	@echo "./a.out          : analyze (the standard input)"
	@echo './a.out file.sil : analyze (the file "file.sil")'
	@echo "make examples    : analyze all examples"
	@echo "6) clean:"
	@echo "make clean       : remove auxiliary files"
	@echo ""

.PHONY : trace
trace : traceaexp tracebexp tracecom tracered

.PHONY : traceaexp
traceaexp :
	-@/bin/rm -f trace_aexp  || true
	@echo "" > trace_aexp
	@echo "Tracing arithmetic expressions"

.PHONY : tracebexp
tracebexp :
	-@/bin/rm -f trace_bexp  || true
	@echo "" > trace_bexp
	@echo "Tracing boolean expressions"

.PHONY : tracecom
tracecom :
	-@/bin/rm -f trace_com  || true
	@echo "" > trace_com
	@echo "Tracing commands"

.PHONY : tracered
tracered :
	-@/bin/rm -f trace_red  || true
	@echo "" > trace_red
	@echo "Tracing ternary reductions"


.PHONY : notrace
notrace :
	-@/bin/rm -f trace_* || true
	@echo "Tracing mode off"

.PHONY : fbool
fbool :
	@/bin/rm -f abexp.ml || true
	@ln -s abexp_f.ml abexp.ml
	@echo "Forward analysis of boolean expressions"

.PHONY : fbbool
fbbool :
	@/bin/rm -f abexp.ml || true
	@ln -s abexp_fb.ml abexp.ml
	@echo "Forward/backward analysis of boolean expressions"

.PHONY : fbrbool
fbrbool :
	@/bin/rm -f abexp.ml || true
	@ln -s abexp_fbr.ml abexp.ml
	@echo "Forward/backward analysis of boolean expressions with reduction"

.PHONY : fassign
fassign :
	@/bin/rm -f acom.ml || true
	@ln -s acom_fa.ml acom.ml
	@echo "Forward analysis of assignments"

.PHONY : fbassign
fbassign :
	@/bin/rm -f acom.ml || true
	@ln -s acom_fba.ml acom.ml
	@echo "Forward/backward analysis of assignments"

.PHONY : r
r : nr
	@echo '"Relational" static analysis not implemented'

.PHONY : nr
nr :
	@/bin/rm -f aenv.ml || true
	@ln -s ../Non-Relational/aenv.ml aenv.ml
	@/bin/rm -f avalues.mli || true
	@ln -s ../Non-Relational/avalues.mli avalues.mli
	@echo '"Non-relational" static analysis'

.PHONY : err
err :
	ocamlyacc parser.mly
	ocamllex lexer.mll
	@/bin/rm -f avalues.ml || true
	@ln -s ../Non-Relational/02-Errors/avalues.ml avalues.ml
#	ocamlc -i ${SOURCES_SINGLE_DOMAIN} # to print types
	ocamlc ${SOURCES_SINGLE_DOMAIN}
	@echo '"Error" static analysis'

.PHONY : iss
iss :
	ocamlyacc parser.mly
	ocamllex lexer.mll
	@/bin/rm -f avalues.ml || true
	@ln -s ../Non-Relational/01-Initialization-Simple-Sign/avalues.ml avalues.ml
#	ocamlc -i ${SOURCES_SINGLE_DOMAIN} # to print types
	ocamlc ${SOURCES_SINGLE_DOMAIN}
	@echo '"Initialization and simple sign" static analysis'

.PHONY : int
int :
	ocamlyacc parser.mly
	ocamllex lexer.mll
	@/bin/rm -f avalues.ml || true
	@ln -s ../Non-Relational/03-Intervals/avalues.ml avalues.ml
#	ocamlc -i ${SOURCES_SINGLE_DOMAIN} # to print types
	ocamlc ${SOURCES_SINGLE_DOMAIN}
	@echo '"Interval" static analysis'

.PHONY : par
par :
	ocamlyacc parser.mly
	ocamllex lexer.mll || true
	@/bin/rm -f avalues.ml
	@ln -s ../Non-Relational/04-Parity/avalues.ml avalues.ml
#	ocamlc -i ${SOURCES_SINGLE_DOMAIN} # to print types
	ocamlc ${SOURCES_SINGLE_DOMAIN}
	@echo '"Parity" static analysis'


.PHONY :  err-int
err-int :
	ocamlyacc parser.mly
	ocamllex lexer.mll
	@/bin/rm -f avalues1.mli || true
	@ln -s avalues.mli avalues1.mli
	@/bin/rm -f avalues1.ml || true
	@ln -s ../Non-Relational/02-Errors/avalues.ml avalues1.ml
	@/bin/rm -f avalues2.mli || true
	@ln -s avalues.mli avalues2.mli
	@/bin/rm -f avalues2.ml || true
	@ln -s ../Non-Relational/03-Intervals/avalues.ml avalues2.ml
	@/bin/rm -f red12.ml || true
	@ln -s ../Non-Relational/05-Prod-Red/red-Errors-Intervals12.ml red12.ml
	@/bin/rm -f avalues.ml || true
	@ln -s ../Non-Relational/05-Prod-Red/avalues12.ml avalues.ml
#	ocamlc -i ${SOURCES_BINARY_REDUCED_PRODUCT} # to print types
	ocamlc ${SOURCES_BINARY_REDUCED_PRODUCT}
	@echo 'Reduced "error" and "interval" static analysis'

.PHONY :  iss-int
iss-int :
	ocamlyacc parser.mly
	ocamllex lexer.mll
	@/bin/rm -f avalues1.mli || true
	@ln -s avalues.mli avalues1.mli
	@/bin/rm -f avalues1.ml || true
	@ln -s ../Non-Relational/01-Initialization-Simple-Sign/avalues.ml avalues1.ml
	@/bin/rm -f avalues2.mli || true
	@ln -s ../Non-Relational/03-Intervals/avalues.mli avalues2.mli
	@/bin/rm -f avalues2.ml || true
	@ln -s ../Non-Relational/03-Intervals/avalues.ml avalues2.ml
	@/bin/rm -f red12.ml || true
	@ln -s ../Non-Relational/05-Prod-Red/red-ISS-Intervals12.ml red12.ml
	@/bin/rm -f avalues.ml || true
	@ln -s ../Non-Relational/05-Prod-Red/avalues12.ml avalues.ml
#	ocamlc -i ${SOURCES_BINARY_REDUCED_PRODUCT} # to print types
	ocamlc ${SOURCES_BINARY_REDUCED_PRODUCT}
	@echo 'Reduced "initialization and simple sign" and "interval" static analysis'

.PHONY :  par-int
par-int :
	ocamlyacc parser.mly
	ocamllex lexer.mll
	@/bin/rm -f avalues1.mli || true
	@ln -s avalues.mli avalues1.mli
	@/bin/rm -f avalues1.ml || true
	@ln -s ../Non-Relational/04-Parity/avalues.ml avalues1.ml
	@/bin/rm -f avalues2.mli || true
	@ln -s ../Non-Relational/03-Intervals/avalues.mli avalues2.mli
	@/bin/rm -f avalues2.ml || true
	@ln -s ../Non-Relational/03-Intervals/avalues.ml avalues2.ml
	@/bin/rm -f red12.ml || true
	@ln -s ../Non-Relational/05-Prod-Red/red-Parity-Intervals12.ml red12.ml
	@/bin/rm -f avalues.ml || true
	@ln -s ../Non-Relational/05-Prod-Red/avalues12.ml avalues.ml
#	ocamlc -i ${SOURCES_BINARY_REDUCED_PRODUCT} # to print types
	ocamlc ${SOURCES_BINARY_REDUCED_PRODUCT}
	@echo 'Reduced "parity" and "interval" static analysis'

.PHONY :  par-iss
par-iss :
	ocamlyacc parser.mly
	ocamllex lexer.mll
	@/bin/rm -f avalues1.mli || true
	@ln -s avalues.mli avalues1.mli
	@/bin/rm -f avalues1.ml || true
	@ln -s ../Non-Relational/04-Parity/avalues.ml avalues1.ml
	@/bin/rm -f avalues2.mli || true
	@ln -s avalues.mli avalues2.mli
	@/bin/rm -f avalues2.ml || true
	@ln -s ../Non-Relational/01-Initialization-Simple-Sign/avalues.ml avalues2.ml
	@/bin/rm -f red12.ml || true
	@ln -s ../Non-Relational/05-Prod-Red/red-Parity-ISS12.ml red12.ml
	@/bin/rm -f avalues.ml || true
	@ln -s ../Non-Relational/05-Prod-Red/avalues12.ml avalues.ml
#	ocamlc -i ${SOURCES_BINARY_REDUCED_PRODUCT} # to print types
	ocamlc ${SOURCES_BINARY_REDUCED_PRODUCT}
	@echo 'Reduced "parity" and "Initialization and simple sign" static analysis'

.PHONY :  par-iss-int
par-iss-int :
	ocamlyacc parser.mly
	ocamllex lexer.mll
	@/bin/rm -f avalues1.mli || true
	@ln -s avalues.mli avalues1.mli
	@/bin/rm -f avalues1.ml || true
	@ln -s ../Non-Relational/04-Parity/avalues.ml avalues1.ml
	@/bin/rm -f avalues2.mli || true
	@ln -s avalues.mli avalues2.mli
	@/bin/rm -f avalues2.ml || true
	@ln -s ../Non-Relational/01-Initialization-Simple-Sign/avalues.ml avalues2.ml
	@/bin/rm -f avalues3.mli || true
	@ln -s ../Non-Relational/03-Intervals/avalues.mli avalues3.mli
	@/bin/rm -f avalues3.ml || true
	@ln -s ../Non-Relational/03-Intervals/avalues.ml avalues3.ml
	@/bin/rm -f red12.ml || true
	@ln -s ../Non-Relational/05-Prod-Red/red-Parity-ISS12.ml red12.ml
	@/bin/rm -f red23.ml || true
	@ln -s ../Non-Relational/05-Prod-Red/red-ISS-Intervals23.ml red23.ml
	@/bin/rm -f red13.ml || true
	@ln -s ../Non-Relational/05-Prod-Red/red-Parity-Intervals13.ml red13.ml
	@/bin/rm -f avalues.ml || true
	@ln -s ../Non-Relational/05-Prod-Red/avalues123.ml avalues.ml
#	ocamlc -i ${SOURCES_TERNARY_REDUCED_PRODUCT} # to print types
	ocamlc ${SOURCES_TERNARY_REDUCED_PRODUCT}
	@echo 'Reduced "parity", "initialization and simple sign" and "interval" static analysis'

include ${EXAMPLES}/makefile
	
.PHONY : clean
clean :
	-@/bin/rm -f *.cmi *.cmo *~ a.out lexer.ml parser.mli parser.ml || true
	-@/bin/rm -f examples/*~ ../Non-Relational/*~ trace_* || true
	-@/bin/rm -f ../Non-Relational/*/*~ || true
	@echo "Remove auxiliary files"

.PHONY : reset
reset :
	-@/bin/rm -f abexp.ml acom.ml aenv.ml avalues.ml avalues.mli avalues1.ml avalues1.mli avalues2.ml avalues2.mli avalues3.ml avalues3.mli red12.ml red13.ml red23.ml || true
	@echo "Remove instanciated files"

