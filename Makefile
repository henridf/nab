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
MISC_SUBDIR = misc
TEST_SUBDIR = test
UBASE_SUBDIR = ubase
BIN_SUBDIR = bin
LIB_SUBDIR = /usr/lib/ocaml
CAMLIMAGES_SUBDIR = $(LIB_SUBDIR)/camlimages

INCLUDE = -I $(MISC_SUBDIR) -I $(TEST_SUBDIR) -I $(LER_SUBDIR) -I $(UBASE_SUBDIR)
INCLUDE_CAMLIMAGES = -I $(CAMLIMAGES_SUBDIR) 

LINKFLAGS_CAMLIMAGES = -cclib "-L/usr/lib/ocaml/camlimages"  

SUBDIRS = \
	$(MISC_SUBDIR) \
	$(TEST_SUBDIR) \
	$(LER_SUBDIR) \
	$(UBASE_SUBDIR) 


DEPEND = .depend

DEPENDS = \
	$(LER_SUBDIR)/*.ml \
	$(LER_SUBDIR)/*.mli \
	$(TEST_SUBDIR)/*.mli \
	$(MISC_SUBDIR)/*.ml \
	$(MISC_SUBDIR)/*.mli \
	$(UBASE_SUBDIR)/*.ml \
	$(UBASE_SUBDIR)/*.mli


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


UBASE_OBJ_FILES = $(UNIX_LIB) \
		  $(UBASE_SUBDIR)/safelist$(CMO) \
		  $(UBASE_SUBDIR)/uprintf$(CMO) \
		  $(UBASE_SUBDIR)/util$(CMO) \
		  $(UBASE_SUBDIR)/uarg$(CMO) \
		  $(UBASE_SUBDIR)/prefs$(CMO) \
		  $(UBASE_SUBDIR)/trace$(CMO) \
		  $(UBASE_SUBDIR)/rx$(CMO)

LER_OBJ_FILES = $(GFX_LIB) \
		$(MISC_OBJ_FILES) \
		$(UBASE_OBJ_FILES) \
		$(MISC_SUBDIR)/graph$(CMO) \
		$(MISC_SUBDIR)/param$(CMO) \
		$(MISC_SUBDIR)/data$(CMO) \
		$(LER_SUBDIR)/common$(CMO) \
		$(LER_SUBDIR)/route$(CMO) \
		$(MISC_SUBDIR)/ler_graphics$(CMO) \
		$(LER_SUBDIR)/params$(CMO) \
		$(LER_SUBDIR)/runsim$(CMO) \
		$(LER_SUBDIR)/ler$(CMO) \
		$(LER_SUBDIR)/lat2d_top$(CMO) \
		$(LER_SUBDIR)/cont2d_top$(CMO) \
		$(LER_SUBDIR)/abf$(CMO) \
		$(LER_SUBDIR)/main$(CMO)

DATAGEN_OBJ_FILES = $(MISC_OBJ_FILES) \
		    $(GFX_LIB) \
		    $(MISC_SUBDIR)/graph$(CMO) \
		    $(LER_SUBDIR)/ler$(CMO) \
		    $(MISC_SUBDIR)/ler_graphics$(CMO) \
		    $(LER_SUBDIR)/movements$(CMO) \
		    $(LER_SUBDIR)/datagen$(CMO)

MODULE_OBJ_FILES = \
	$(MISC_SUBDIR)/graph$(CMO) \
	$(MISC_SUBDIR)/larray$(CMO) \
	$(MISC_SUBDIR)/circbuf$(CMO) \
	$(MISC_SUBDIR)/itin$(CMO)


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

misc/im2png$(CMO): misc/im2png.ml
	$(MLCOMP) $(MLFLAGS) $(INCLUDE) $(INCLUDE_CAMLIMAGES) -c $<
im2png: misc/im2png$(CMO)
	$(MLCOMP) $(MLFLAGS) $(LINKFLAGS_CAMLIMAGES) $(INCLUDE) $(INCLUDE_CAMLIMAGES) $(CAMLIMAGES_LIBS) misc/im2png$(CMO) -o bin/$@

ler: bin/ler
bin/ler: $(LER_OBJ_FILES)
	$(MLCOMP) $(MLFLAGS)  $(INCLUDE)  $(LER_OBJ_FILES) -o $@ 


datagen: bin/datagen
bin/datagen: $(DATAGEN_OBJ_FILES)
	$(MLCOMP) $(MLFLAGS)  $(INCLUDE)  $(DATAGEN_OBJ_FILES) -o $@ 

doplots: $(DOPLOTS_OBJ_FILES)
	$(MLCOMP) $(MLFLAGS) $(INCLUDE) $(DOPLOTS_OBJ_FILES) $(DOPLOTS_MAIN_FILE) -o $(BIN_SUBDIR)/$@ 

testmodules:  $(MODULE_OBJ_FILES) $(MISC_OBJ_FILES) $(TEST_OBJ_FILES)
	$(MLCOMP) $(MLFLAGS) $(INCLUDE) $(MISC_OBJ_FILES) $(MODULE_OBJ_FILES) $(TEST_OBJ_FILES)  -o $(BIN_SUBDIR)/$@ 

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
