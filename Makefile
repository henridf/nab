#*************************************************************#
# Choose whether to use the optimizing, profiling, or regular
# compiler.
#
ifdef NOPT
	include  mk/ocaml.mk	
else
	include mk/ocamlopt.mk
endif


LER_SUBDIR = ler
BLER_SUBDIR = bler
MISC_SUBDIR = misc
TEST_SUBDIR = test
LIB_SUBDIR = /usr/lib/ocaml

INCLUDE = -I $(MISC_SUBDIR) -I $(TEST_SUBDIR) -I $(LER_SUBDIR) -I $(BLER_SUBDIR) -I $(LIB_SUBDIR)


SUBDIRS = \
	$(MISC_SUBDIR) \
	$(TEST_SUBDIR) \
	$(LER_SUBDIR) \
	$(BLER_SUBDIR)

DEPEND = .depend

DEPENDS = \
	$(LER_SUBDIR)/*.ml	$(LER_SUBDIR)/*.mli	\
	$(BLER_SUBDIR)/*.ml	$(BLER_SUBDIR)/*.mli	\
	$(MISC_SUBDIR)/*.ml	$(MISC_SUBDIR)/*.mli	\
	$(TEST_SUBDIR)test/*.ml	$(TEST_SUBDIR)test/*.mli


##########################################
# Files corresponding to different subdirs


LER_OBJ_FILES = \
	$(LER_SUBDIR)/ler$(CMO) \
	$(LER_SUBDIR)/ler_experiments$(CMO) \
	$(LER_SUBDIR)/ler_graphics$(CMO)

BLER_OBJ_FILES = \
	$(LER_SUBDIR)/ler_graphics$(CMO) \
	$(BLER_SUBDIR)/bler$(CMO)

PROOF_OBJ_FILES = \
	$(LER_SUBDIR)/proof$(CMO)

MISC_OBJ_FILES = \
	$(MISC_SUBDIR)/misc$(CMO)


TEST_OBJ_FILES = \
	$(TEST_SUBDIR)/testUtils$(CMO)



############
# Libraries

GFX_LIB = $(LIB_SUBDIR)/graphics$(CMA)
UNIX_LIB = $(LIB_SUBDIR)/unix$(CMA)
STR_LIB = $(LIB_SUBDIR)/str$(CMA)

%.cmo: %.ml
	$(MLCOMP) $(MLFLAGS) $(INCLUDE) -c $<

%.cmi: %.mli
	$(MLCOMP) $(MLFLAGS) $(INCLUDE) -c $<

%.cmx: %.ml
	$(MLCOMP) $(MLFLAGS) $(INCLUDE) -c $<

test-ler: $(LER_OBJ_FILES) $(LER_SUBDIR)/test-ler$(CMO)
	$(MLCOMP) $(MLFLAGS) $(INCLUDE) $(MISC_OBJ_FILES) $(TEST_OBJ_FILES) $(GFX_LIB) $(LER_OBJ_FILES) $(LER_SUBDIR)/test-ler$(CMO) -o $@ 

bin/exp: $(LER_OBJ_FILES) 
	$(MLCOMP) $(MLFLAGS) $(INCLUDE) $(MISC_OBJ_FILES) $(GFX_LIB) $(LER_OBJ_FILES) -o $(BIN_SUBDIR)$@ 

bin/bler: $(BLER_OBJ_FILES)
	$(MLCOMP) $(MLFLAGS) $(INCLUDE) $(MISC_OBJ_FILES) $(GFX_LIB) $(BLER_OBJ_FILES) -o $(BIN_SUBDIR)$@ 

bin/doplots: $(LER_OBJ_FILES) $(LER_SUBDIR)/ler_experiments$(CMO) $(MISC_SUBDIR)/doplots$(CMO)
	$(MLCOMP) $(MLFLAGS) $(INCLUDE) $(MISC_OBJ_FILES) $(UNIX_LIB) $(STR_LIB) $(GFX_LIB) $(MISC_SUBDIR)/doplots$(CMO) -o $(BIN_SUBDIR)$@ 

bin/proof: $(LER_OBJ_FILES) $(PROOF_OBJ_FILES)
	$(MLCOMP) $(MLFLAGS) $(INCLUDE) $(MISC_OBJ_FILES) $(PROOF_OBJ_FILES) -o $(BIN_SUBDIR)$@ 

CLEANALL = for d in $(SUBDIRS); do (cd $$d; rm -f *.o *.cmx *.cmi *.cmo a.out); done

clean:
	$(CLEANALL)
	rm -f *.o *.cmx *.cmi *.cmo a.out 

depend:
	ocamldep $(INCLUDE) $(DEPENDS) > $(DEPEND)

include $(DEPEND)
