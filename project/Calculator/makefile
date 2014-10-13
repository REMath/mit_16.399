# make delete
# script calculator.typescript
# make
# ^D

.PHONY : all
all : 
	ls 
	@echo '# Lexer specification:'
	cat calculatorLEX.mll
	@echo '# Lexer creation:'
	ocamllex calculatorLEX.mll
	ls
	@echo '# Parser specification:'
	cat calculatorYACC.mly
	@echo '# Parser creation:'
	ocamlyacc calculatorYACC.mly
	ls
	@echo '# Types of lexem returned values:'
	cat calculatorYACC.mli
	@echo '# Compilation of the lexer and parser:'
	ocamlc -c calculatorYACC.mli
	ocamlc -c calculatorLEX.ml
	ocamlc -c calculatorYACC.ml
	@echo '# Specification of the calculator'
	cat calculator.ml 
	ocamlc -c calculator.ml
	@echo '# Linking and code generation for the lexer, '
	@echo '# parser and calculator:'
	ocamlc -o calculator calculatorLEX.cmo calculatorYACC.cmo \
	calculator.cmo
	ls
	@echo '# Using the calculator:'
	echo "X:=1; Y:=2; Z:= 3; (X+Y)*Z+4" | ./calculator
	echo "X:=1; Y:=2; *X; X" | ./calculator
	
.PHONY : clean
clean :
	@-/bin/rm -f *.cmo *.cmi calculatorLEX.ml calculatorYACC.mli \
	calculatorYACC.ml || true

.PHONY : delete
delete : clean
	@-/bin/rm -f calculator typescript 
