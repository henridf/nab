#*************************************************************#
# Choose whether to use the optimizing, profiling, or regular
# compiler.
#
ifdef OPT
	include mk/ocamlopt.mk
else
	include  mk/ocaml.mk	
endif



LER_SUBDIR = ler
LLER_SUBDIR = ller
MISC_SUBDIR = misc
TEST_SUBDIR = test
BIN_SUBDIR = bin
LIB_SUBDIR = /usr/lib/ocaml
CAMLIMAGES_SUBDIR = $(LIB_SUBDIR)/camlimages


INCLUDE = -I $(MISC_SUBDIR) -I $(TEST_SUBDIR) -I $(LER_SUBDIR) -I $(LLER_SUBDIR) -I $(CAMLIMAGES_SUBDIR) 


LINKFLAGS_CAMLIMAGES = -cclib "-L/usr/lib/ocaml/camlimages"  

SUBDIRS = \
	$(MISC_SUBDIR) \
	$(TEST_SUBDIR) \
	$(LER_SUBDIR) \
	$(LLER_SUBDIR) 


DEPEND = .depend

DEPENDS = \
	$(LER_SUBDIR)/*.ml \
	$(LER_SUBDIR)/*.mli \
	$(TEST_SUBDIR)/*.mli \
	$(LLER_SUBDIR)/*.ml \
	$(LLER_SUBDIR)/*.mli \
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



LER_OBJ_FILES = $(GFX_LIB) \
		$(MISC_OBJ_FILES) \
		$(MISC_SUBDIR)/graph$(CMO) \
		$(LER_SUBDIR)/ler$(CMO) \
		$(MISC_SUBDIR)/ler_graphics$(CMO) \
		$(LER_SUBDIR)/movements$(CMO) \
		$(LLER_SUBDIR)/larray$(CMO) \
		$(LLER_SUBDIR)/circbuf$(CMO) \
		$(LLER_SUBDIR)/itin$(CMO) \
		$(LER_SUBDIR)/ease$(CMO) \
		$(LER_SUBDIR)/abf$(CMO) \
		$(LER_SUBDIR)/ler_main$(CMO)

LLER_OBJ_FILES = $(MISC_OBJ_FILES) \
		 $(MISC_SUBDIR)/data$(CMO) \
		 $(MISC_SUBDIR)/graph$(CMO) \
		 $(LLER_SUBDIR)/larray$(CMO) \
		 $(LLER_SUBDIR)/circbuf$(CMO) \
		 $(LLER_SUBDIR)/itin$(CMO) \
	  	 $(LLER_SUBDIR)/ller$(CMO) \
		 $(LLER_SUBDIR)/ller_main$(CMO)

MODULE_OBJ_FILES = \
	$(MISC_SUBDIR)/graph$(CMO) \
	$(LLER_SUBDIR)/larray$(CMO) \
	$(LLER_SUBDIR)/circbuf$(CMO) \
	$(LLER_SUBDIR)/itin$(CMO)


PROOF_OBJ_FILES = $(LER_SUBDIR)/proof$(CMO)

DOPLOTS_OBJ_FILES = \
		$(GFX_LIB) \
		$(UNIX_LIB) \
		$(STR_LIB) \
		$(MISC_OBJ_FILES) \
		$(MISC_SUBDIR)/doplots$(CMO) 	


MISC_OBJ_FILES = $(MISC_SUBDIR)/misc$(CMO) \
		 $(MISC_SUBDIR)/coord$(CMO)

GRAPH_OBJ_FILES = \
		$(MISC_OBJ_FILES) \
		$(MISC_SUBDIR)/graph$(CMO) \
		$(MISC_SUBDIR)/graph_main$(CMO)

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

im2png: misc/im2png$(CMO)
	$(MLCOMP) $(MLFLAGS) $(LINKFLAGS_CAMLIMAGES) $(INCLUDE) $(CAMLIMAGES_LIBS) misc/im2png$(CMO)

exp: bin/exp
bin/exp:  $(LER_OBJ_FILES)
	$(MLCOMP) $(MLFLAGS)  $(INCLUDE)  $(LER_OBJ_FILES) -o $@ 
#	$(MLCOMP) $(MLFLAGS) $(LINKFLAGS_CAMLIMAGES) $(INCLUDE) $(CAMLIMAGES_LIBS) $(LER_OBJ_FILES) -o $@ 

doplots: $(DOPLOTS_OBJ_FILES)
	$(MLCOMP) $(MLFLAGS) $(INCLUDE) $(DOPLOTS_OBJ_FILES) $(DOPLOTS_MAIN_FILE) -o $(BIN_SUBDIR)/$@ 

testmodules:  $(MODULE_OBJ_FILES) $(MISC_OBJ_FILES) $(TEST_OBJ_FILES)
	$(MLCOMP) $(MLFLAGS) $(INCLUDE) $(MISC_OBJ_FILES) $(MODULE_OBJ_FILES) $(TEST_OBJ_FILES)  -o $(BIN_SUBDIR)/$@ 

ller: bin/ller
bin/ller: $(LLER_OBJ_FILES)
	$(MLCOMP) $(MLFLAGS) $(INCLUDE) $(LLER_OBJ_FILES) -o $@

ocamlgfx: 
	ocamlmktop -custom -o bin/ocamlgfx $(GFX_LIB) -cclib -L/usr/X11/lib -cclib -lX11

all: exp doplots graph-test

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
