OCAMLC		= ocamlc
OCAMLOPT	= ocamlopt
OCAMLDEP	= ocamldep
OCAMLDOC	= ocamldoc
DESTDIR = /usr/local/lib/ocaml/`ocamlc -version`
OCAMLFLAGS 	=
OCAMLOPTFLAGS	= -inline 2

SOURCEDIR	= gnuplot
WEBDIR		= ../webpublishing
DOCDIR		= gnuplot

SOURCES = gnuplot.ml gnuplotBig.ml
INTERFACE =  gnuplot.mli gnuplotBig.mli
DEMOS = ex1.ml ex2.ml ex3.ml ex4.ml ex5.ml ex6.ml

.PHONY: all byte opt install install-byte install-opt doc dist
all: byte opt
byte: $(SOURCES:.ml=.cma)
opt: $(SOURCES:.ml=.cmxa)

install: install-byte install-opt
install-byte: byte
	[ -d $(DESTDIR) ] || mkdir -p $(DESTDIR)
	cp $(SOURCES:.ml=.cma) $(INTERFACE) $(INTERFACE:.mli=.cmi) $(DESTDIR)
install-opt: opt
	[ -d $(DESTDIR) ] || mkdir -p $(DESTDIR)
	cp $(SOURCES:.ml=.cmxa) $(SOURCES:.ml=.a) \
	  $(INTERFACE) $(INTERFACE:.mli=.cmi) $(DESTDIR)

doc: gnuplot.html
dist:
	$(MAKE) gnuplot.html
	[ -d $(WEBDIR)/$(DOCDIR)/ ] || mkdir $(WEBDIR)/$(DOCDIR)/
	cp -rf $(DOCDIR)/ $(WEBDIR)/
	$(MAKE) clean
	cd ..; \
	  tar --preserve --exclude=CVS \
	  -zcf $(SOURCEDIR)/$(WEBDIR)/$(SOURCEDIR).tar.gz $(SOURCEDIR)

gnuplot.html: gnuplot.mli gnuplot.cmi
	[ -d $(DOCDIR)/ ] || mkdir $(DOCDIR)
	$(OCAMLDOC) -d $(DOCDIR) -stars -html -colorize-code $<


# Examples
.PHONY: demos
demos: $(DEMOS:.ml=.exe)
ex%.exe: ex%.ml gnuplot.cma
	$(OCAMLC) -o $@ bigarray.cma unix.cma gnuplot.cma $<
ex%.com: ex%.ml gnuplot.cmxa
	$(OCAMLOPT) -o $@ bigarray.cmxa unix.cmxa gnuplot.cmxa $<


# Caml general dependencies
.SUFFIXES: .ml .mli .cmo .cmi .cmx
.mli.cmi:
	$(OCAMLC) $(OCAMLFLAGS) -c $<
.ml.cmo:
	$(OCAMLC) $(OCAMLFLAGS) -c $<
%.cma: %.cmo %.cmi
	$(OCAMLC) $(OCAMLFLAGS) -a -o $@ $<
.ml.cmx:
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<
%.cmxa: %.cmx %.cmi
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -a -o $@ $<

.PHONY: depend
depend: .depend
.depend:
	$(OCAMLDEP) *.mli *.ml > .depend

include .depend

########################################################################

.PHONY: clean dist-clean
clean:
	rm -f *~ .*~ *.{o,a} *.cm[aiox] *.cmxa *.html *.css *.exe *.com
	find . -type f -perm +u=x -exec rm -f {} \;
	[ -d ./$(DOCDIR)/ ] && rm -rf $(DOCDIR)

dist-clean: clean
	rm .depend