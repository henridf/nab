#*************************************************************#
# Choose whether to use the optimizing, profiling, or regular
# compiler.
#

ifdef OPT
	include mk/ocamlopt.mk
else
	include  mk/ocaml.mk	
endif


MWS_SUBDIR = mws
MWS_SCRIPT_SUBDIR = scripts
MISC_SUBDIR = misc
TEST_SUBDIR = test
BIN_SUBDIR = bin
LIB_SUBDIR = /usr/lib/ocaml
CAMLIMAGES_SUBDIR = $(LIB_SUBDIR)/camlimages

INCLUDE = -I $(MISC_SUBDIR) -I $(TEST_SUBDIR) -I $(MWS_SUBDIR)
INCLUDE_CAMLIMAGES = -I $(CAMLIMAGES_SUBDIR) 

LINKFLAGS_CAMLIMAGES = -cclib "-L/usr/lib/ocaml/camlimages"  

SUBDIRS = \
	$(MISC_SUBDIR) \
	$(TEST_SUBDIR) \
	$(MWS_SUBDIR) \
	$(MWS_SCRIPT_SUBDIR)


DEPEND = .depend

DEPENDS = \
	$(MWS_SUBDIR)/*.ml \
	$(MWS_SUBDIR)/*.mli \
	$(MWS_SCRIPT_SUBDIR)/*.ml \
	$(MWS_SCRIPT_SUBDIR)/*.mli \
	$(TEST_SUBDIR)/*.mli \
	$(MISC_SUBDIR)/*.ml \
	$(MISC_SUBDIR)/*.mli


############
# Libraries

GFX_LIB = $(LIB_SUBDIR)/graphics$(CMA)
UNIX_LIB = $(LIB_SUBDIR)/unix$(CMA)
STR_LIB = $(LIB_SUBDIR)/str$(CMA)

CAMLIMAGES_LIBS = ci_core$(CMA) \
		$(GFX_LIB) \
		ci_graphics$(CMA) \
		ci_png$(CMA)

##########################################
# Files corresponding to different subdirs


MWS_OBJ_FILES = $(GFX_LIB) \
		$(MISC_OBJ_FILES) \
		$(MISC_SUBDIR)/param$(CMO) \
		$(MISC_SUBDIR)/linkedlist$(CMO) \
		$(MISC_SUBDIR)/data$(CMO) \
		$(MWS_SUBDIR)/common$(CMO) \
		$(MWS_SUBDIR)/packet$(CMO) \
		$(MWS_SUBDIR)/trace$(CMO) \
		$(MWS_SUBDIR)/log$(CMO) \
		$(MWS_SUBDIR)/sched$(CMO) \
		$(MWS_SUBDIR)/gsched$(CMO) \
		$(MWS_SUBDIR)/gworld$(CMO) \
		$(MWS_SUBDIR)/nodes$(CMO) \
		$(MWS_SUBDIR)/route$(CMO) \
		$(MISC_SUBDIR)/ler_graphics$(CMO) \
		$(MWS_SUBDIR)/params$(CMO) \
		$(MWS_SUBDIR)/nodeDB$(CMO) \
		$(MWS_SUBDIR)/bler_agent$(CMO) \
		$(MWS_SUBDIR)/simplenode$(CMO) \
		$(MWS_SUBDIR)/persistency$(CMO) \
		$(MWS_SUBDIR)/crworld$(CMO) \
		$(MWS_SUBDIR)/script_utils$(CMO) 

MWS_SCRIPT = $(MWS_SCRIPT_SUBDIR)/current$(CMO)


MODULE_OBJ_FILES = \
	$(MISC_SUBDIR)/graph$(CMO) \
	$(MISC_SUBDIR)/larray$(CMO) \
	$(MISC_SUBDIR)/circbuf$(CMO) \
	$(MISC_SUBDIR)/itin$(CMO)


MISC_OBJ_FILES = $(MISC_SUBDIR)/misc$(CMO) \
		 $(MISC_SUBDIR)/coord$(CMO)

TEST_OBJ_FILES = \
	$(TEST_SUBDIR)/testUtils$(CMO) \
	$(MISC_SUBDIR)/coord$(CMO) \
	$(TEST_SUBDIR)/graph-test$(CMO) \
	$(TEST_SUBDIR)/itin-test$(CMO) \
	$(TEST_SUBDIR)/circbuf-test$(CMO)


%.cmo: %.ml
	$(MLCOMP) $(MLFLAGS) $(INCLUDE) -c $<

%.cmi: %.mli
	$(MLCOMP) $(MLFLAGS) $(INCLUDE) -c $<

%.cmx: %.ml
	$(MLCOMP) $(MLFLAGS) $(INCLUDE) -c $<

misc/im2png$(CMO): misc/im2png.ml
	$(MLCOMP) $(MLFLAGS) $(INCLUDE) $(INCLUDE_CAMLIMAGES) -c $<
im2png: misc/im2png$(CMO)
	$(MLCOMP) $(MLFLAGS) $(LINKFLAGS_CAMLIMAGES) $(INCLUDE) $(INCLUDE_CAMLIMAGES) $(CAMLIMAGES_LIBS) misc/im2png$(CMO) -o bin/$@

mws: bin/mws
bin/mws: $(MWS_OBJ_FILES) $(MWS_SCRIPT)
	$(MLCOMP) $(MLFLAGS)  $(INCLUDE)  $(MWS_OBJ_FILES) $(MWS_SCRIPT) -o $@ 

mwstop: bin/mwstop
bin/mwstop: $(MWS_OBJ_FILES)
	$(MLTOP) $(INCLUDE) $(MWS_OBJ_FILES) -o $@

testmodules:  $(MODULE_OBJ_FILES) $(MISC_OBJ_FILES) $(TEST_OBJ_FILES)
	$(MLCOMP) $(MLFLAGS) $(INCLUDE) $(MISC_OBJ_FILES) $(MODULE_OBJ_FILES) $(TEST_OBJ_FILES)  -o $(BIN_SUBDIR)/$@ 

ocamlgfx: 
	ocamlmktop -custom -o bin/ocamlgfx $(GFX_LIB) -cclib -L/usr/X11/lib -cclib -lX11


CLEANALL = for d in $(SUBDIRS); do (cd $$d; rm -f *.o *.cmx *.cmi *.cmo *.out); done
EMACSCLEANALL = for d in $(SUBDIRS); do (cd $$d; rm -f *~; rm -f .*~); done

clean:
	$(CLEANALL)
	rm -f *.o *.cmx *.cmi *.cmo a.out 
eclean:
	$(EMACSCLEANALL)
	rm -f *~
	rm -f .*~

depend:
	ocamldep $(INCLUDE) $(DEPENDS) > $(DEPEND)

include $(DEPEND)
