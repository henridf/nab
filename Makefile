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
BIN_SUBDIR = bin
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


############
# Libraries

GFX_LIB = $(LIB_SUBDIR)/graphics$(CMA)
UNIX_LIB = $(LIB_SUBDIR)/unix$(CMA)
STR_LIB = $(LIB_SUBDIR)/str$(CMA)

##########################################
# Files corresponding to different subdirs



LER_OBJ_FILES = $(LER_SUBDIR)/ler$(CMO) \
	$(MISC_OBJ_FILES)
LER_MAIN_FILE = $(LER_SUBDIR)/ler_main$(CMO)


BLER_OBJ_FILES = $(BLER_SUBDIR)/bler$(CMO) \
	$(BLER_SUBDIR)/ler_graphics$(CMO) \
	$(MISC_OBJ_FILES)
BLER_MAIN_FILE = $(BLER_SUBDIR)/bler_main$(CMO) 


PROOF_MAIN_FILE = $(LER_SUBDIR)/proof$(CMO)

DOPLOTS_OBJ_FILES = \
	$(GFX_LIB) \
	$(UNIX_LIB) \
	$(STR_LIB) \
	$(MISC_OBJ_FILES)
DOPLOTS_MAIN_FILE = $(MISC_SUBDIR)/doplots$(CMO) 


MISC_OBJ_FILES = $(MISC_SUBDIR)/misc$(CMO)


TEST_OBJ_FILES = \
	$(TEST_SUBDIR)/testUtils$(CMO)




%.cmo: %.ml
	$(MLCOMP) $(MLFLAGS) $(INCLUDE) -c $<

%.cmi: %.mli
	$(MLCOMP) $(MLFLAGS) $(INCLUDE) -c $<

%.cmx: %.ml
	$(MLCOMP) $(MLFLAGS) $(INCLUDE) -c $<

exp:  $(LER_MAIN_FILE)
	$(MLCOMP) $(MLFLAGS) $(INCLUDE) $(LER_OBJ_FILES) $(LER_MAIN_FILE) -o $(BIN_SUBDIR)/$@ 

bler: $(BLER_MAIN_FILE)
	$(MLCOMP) $(MLFLAGS) $(INCLUDE)  $(BLER_OBJ_FILES) $(BLER_MAIN_FILE) -o $(BIN_SUBDIR)/$@ 

doplots:  $(MISC_SUBDIR)/doplots$(CMO)
	$(MLCOMP) $(MLFLAGS) $(INCLUDE) $(DOPLOTS_OBJ_FILES) $(DOPLOTS_MAIN_FILE) -o $(BIN_SUBDIR)/$@ 

proof: $(LER_OBJ_FILES) $(PROOF_OBJ_FILES)
	$(MLCOMP) $(MLFLAGS) $(INCLUDE) $(MISC_OBJ_FILES) $(PROOF_MAIN_FILE) -o $(BIN_SUBDIR)/$@ 

CLEANALL = for d in $(SUBDIRS); do (cd $$d; rm -f *.o *.cmx *.cmi *.cmo a.out); done

clean:
	$(CLEANALL)
	rm -f *.o *.cmx *.cmi *.cmo a.out 

depend:
	ocamldep $(INCLUDE) $(DEPENDS) > $(DEPEND)

include $(DEPEND)
