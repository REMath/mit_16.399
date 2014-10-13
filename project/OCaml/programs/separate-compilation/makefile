############################ Documentation ######################
#
# To use this Makefile you must create a file makefile.depend, 
# using "touch makefile.depend"
#
#
#################################################################

SHELL = /bin/tcsh

.PHONY :all
all : clean original generic compilesep script compile listing interactive


.PHONY : interactive
interactive :
	@echo "*** interactive mode, type:"
	@echo "  ocaml"
	@echo "and then:"
	@echo '  #use "scriptfile.ml";;'
	@echo "  #quit;;"
	@echo 'to use "ocaml" interactively with "scriptfile.ml"'

.PHONY : original
original : 
	@echo "*** execution of the original program p.ml:"
	ocaml p.ml

SOURCES = a.mli b.ml b.mli c.ml c.mli d.ml e.ml
OBJS = $(SOURCES:.ml=.cmo)

.PHONY : generic
generic : message $(OBJS)
	ocamlc $(OBJS)
	@echo "*** execution of the compiled code:"
	./a.out

.PHONY : message
message :
	@echo "*** generic separate compilation of the modules:"

.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi

.ml.cmo:
	ocamlc -c $<

.mli.cmi:
	ocamlc -c $<

makefile.depend: $(SOURCES2)
	$(CAMLDEP) *.mli *.ml > makefile.depend

depend: $(SOURCES2)
	$(CAMLDEP) *.mli *.ml > makefile.depend

include makefile.depend

.PHONY : compilesep
compilesep :
	@echo "*** customed separate compilation of the modules:"
	ocamlc -c a.mli
	ocamlc -c b.mli
	ocamlc -c b.ml
	ocamlc -c c.mli
	ocamlc -c c.ml
	ocamlc -c d.ml
	ocamlc -c e.ml
	ocamlc b.cmo c.cmo d.cmo e.cmo
	@echo "*** execution of the compiled code:"
	./a.out

.PHONY : script
script :
	@echo "*** creation of a script file:"
	@echo "" > scriptfile.ml
	@echo "module A = struct" >> scriptfile.ml
	@cat a.mli >> scriptfile.ml
	@echo "end;;" >> scriptfile.ml
	@echo "module B : sig" >> scriptfile.ml
	@cat b.mli >> scriptfile.ml
	@echo "end = struct" >> scriptfile.ml
	@cat b.ml >> scriptfile.ml
	@echo "end;;" >> scriptfile.ml
	@echo "module C : sig" >> scriptfile.ml
	@cat c.mli >> scriptfile.ml
	@echo "end = struct" >> scriptfile.ml
	@cat c.ml >> scriptfile.ml
	@echo "end;;" >> scriptfile.ml
	@echo "module D = struct" >> scriptfile.ml
	@cat d.ml >> scriptfile.ml
	@echo "end;;" >> scriptfile.ml
	@cat e.ml >> scriptfile.ml
	@echo "*** ocaml in script mode:"
	ocaml scriptfile.ml

.PHONY : compile
compile :
	@echo "*** compilation of the scriptfile:"
	ocamlc scriptfile.ml
	@echo "*** execution of the compiled code:"
	./a.out

.PHONY : listing
listing :
	@echo "*** original program p.ml:"
	@echo ""
	@cat p.ml
	@echo ""
	@echo "*** modules:"
	@echo ""
	@echo "** a.mli"
	@echo ""
	@cat a.mli
	@echo ""
	@echo "** b.mli"
	@echo ""
	@cat b.mli
	@echo ""
	@echo "** b.ml"
	@echo ""
	@cat b.ml
	@echo ""
	@echo "** c.mli"
	@echo ""
	@cat c.mli
	@echo ""
	@echo "** c.ml"
	@echo ""
	@cat c.ml
	@echo ""
	@echo "** d.ml"
	@echo ""
	@cat d.ml
	@echo ""
	@echo "** e.ml"
	@echo ""
	@cat e.ml
	@echo ""
	@echo ""
	@echo "*** script file:"
	@echo ""
	@cat scriptfile.ml
	@echo ""

.PHONY : clean
clean :
	-@rm -f *~ *.cm[iox]
	-@rm -f a.out