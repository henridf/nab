#*************************************************************#
# Choose whether to use the optimizing, profiling, or regular
# compiler.
#

ifdef OPT
	include mk/ocamlopt.mk
else
	include  mk/ocaml.mk	
endif

MWS_DIR = mws
MWS_SCRIPT_DIR = scripts
NAML_DIR = naml
MISC_DIR = misc
MK_DIR = mk
TEST_DIR = test
BIN_DIR = bin
CAML_DIR = /usr/lib/ocaml
GTK_DIR = $(CAML_DIR)/lablgtk
CAMLIMAGES_DIR = $(CAML_DIR)/camlimages

INCLUDE = -I $(MISC_DIR) -I $(TEST_DIR) -I $(MWS_DIR) -I $(GTK_DIR) -I $(NAML_DIR)


INCLUDE_CAMLIMAGES = -I $(CAMLIMAGES_DIR) 

LINKFLAGS_CAMLIMAGES = -cclib "-L/usr/lib/ocaml/camlimages"  
THFLAGS = -thread

DIRS = \
	$(MISC_DIR) \
	$(MK_DIR) \
	$(TEST_DIR) \
	$(MWS_DIR) \
	$(MWS_SCRIPT_DIR) \
	$(NAML_DIR)

DEPEND = .depend

DEPENDS = \
	$(MWS_DIR)/*.ml \
	$(MWS_DIR)/*.mli \
	$(NAML_DIR)/*.ml \
	$(NAML_DIR)/*.mli \
	$(MWS_SCRIPT_DIR)/*.ml \
	$(MWS_SCRIPT_DIR)/*.mli \
	$(TEST_DIR)/*.mli \
	$(MISC_DIR)/*.ml \
	$(MISC_DIR)/*.mli


############
# Libraries

GFX_LIB = $(CAML_DIR)/graphics$(CMA)
UNIX_LIB = $(CAML_DIR)/unix$(CMA)
STR_LIB = $(CAML_DIR)/str$(CMA)
THREADS_LIB = $(CAML_DIR)/threads/threads$(CMA)


GTK_LIBS = $(GTK_DIR)/lablgtk$(CMA) $(UNIX_LIB) $(STR_LIB)
GTK_TH_LIBS = $(UNIX_LIB) $(THREADS_LIB) $(STR_LIB)  $(GTK_DIR)/lablgtk$(CMA)
GTK_TH_OBJS =  $(GTK_DIR)/gtkThread$(CMO)  $(GTK_DIR)/gtkInit$(CMO) $(GTK_DIR)/gtkThInit$(CMO)
GTK_INIT_OBJS = $(GTK_DIR)/gtkInit$(CMO)

GTK_TH_STUFF = $(GTK_TH_LIBS)  $(GTK_TH_OBJS)
GTK_STUFF =  $(GTK_LIBS) $(GTK_INIT_OBJS)

CAMLIMAGES_LIBS = ci_core$(CMA) \
		$(GFX_LIB) \
		ci_graphics$(CMA) \
		ci_png$(CMA)

##########################################
# Files corresponding to different subdirs


MWS_OBJ_FILES = $(GFX_LIB) \
		$(MISC_OBJ_FILES) \
		$(MWS_DIR)/mws_utils$(CMO) \
		$(MISC_DIR)/param$(CMO) \
		$(MISC_DIR)/linkedlist$(CMO) \
		$(MISC_DIR)/data$(CMO) \
		$(MWS_DIR)/common$(CMO) \
		$(MWS_DIR)/packet$(CMO) \
		$(NAML_DIR)/naml_msg$(CMO) \
		$(MWS_DIR)/params$(CMO) \
		$(MWS_DIR)/trace$(CMO) \
		$(MWS_DIR)/log$(CMO) \
		$(MWS_DIR)/sched$(CMO) \
		$(MWS_DIR)/gsched$(CMO) \
		$(MWS_DIR)/gworld$(CMO) \
		$(MWS_DIR)/nodes$(CMO) \
		$(MWS_DIR)/route$(CMO) \
		$(MISC_DIR)/ler_graphics$(CMO) \
		$(MWS_DIR)/nodeDB$(CMO) \
		$(MWS_DIR)/rtab$(CMO) \
		$(MWS_DIR)/simplenode$(CMO) \
		$(MWS_DIR)/grep_hooks$(CMO) \
		$(MWS_DIR)/aodv_agent$(CMO) \
		$(MWS_DIR)/grep_agent$(CMO) \
		$(MWS_DIR)/mob$(CMO) \
		$(MWS_DIR)/persistency$(CMO) \
		$(MWS_DIR)/crsearch$(CMO) \
		$(MWS_DIR)/crworld$(CMO) \
		$(MWS_DIR)/script_utils$(CMO) \
		$(MWS_SCRIPT)


NAML_OBJ_FILES = $(MISC_OBJ_FILES) \
		$(MWS_DIR)/common$(CMO) \
		$(NAML_DIR)/naml_msg$(CMO) \
		$(NAML_DIR)/naml_gtk$(CMO) \
		$(NAML_DIR)/naml_action$(CMO) \
		$(NAML_DIR)/naml_io$(CMO) \
		$(NAML_DIR)/naml_main$(CMO) \


MODULE_OBJ_FILES = \
	$(MISC_DIR)/graph$(CMO) \
	$(MISC_DIR)/larray$(CMO) \
	$(MISC_DIR)/circbuf$(CMO) \
	$(MISC_DIR)/itin$(CMO)


MISC_OBJ_FILES = $(MISC_DIR)/misc$(CMO) \
		 $(MISC_DIR)/coord$(CMO)

TEST_OBJ_FILES = \
	$(TEST_DIR)/testUtils$(CMO) \
	$(MISC_DIR)/coord$(CMO) \
	$(TEST_DIR)/graph-test$(CMO) \
	$(TEST_DIR)/itin-test$(CMO) \
	$(TEST_DIR)/circbuf-test$(CMO)


%.cmo: %.ml
	$(MLCOMP) $(MLFLAGS) $(INCLUDE) -c $<

%.cmi: %.mli
	$(MLCOMP) $(MLFLAGS) $(INCLUDE) -c $<

%.cmx: %.ml
	$(MLCOMP) $(MLFLAGS) $(INCLUDE) -c $<

misc/im2png$(CMO): misc/im2png.ml
	$(MLCOMP) $(MLFLAGS) $(INCLUDE) $(INCLUDE_CAMLIMAGES) -c $<
im2png: misc/im2png$(CMO)
	$(MLCOMP) $(MLFLAGS) $(LINKFLAGS_CAMLIMAGES) $(INCLUDE) $(INCLUDE_CAMLIMAGES) \
	$(CAMLIMAGES_LIBS) misc/im2png$(CMO) -o bin/$@

mws: bin/mws
bin/mws: $(MWS_OBJ_FILES) 
	$(MLCOMP) $(MLFLAGS)  $(INCLUDE)  $(MWS_OBJ_FILES)  -o $@ 

mwstop: bin/mwstop
bin/mwstop: $(MWS_OBJ_FILES)
	$(MLTOP) $(INCLUDE)  $(MWS_OBJ_FILES) -o $@

naml: bin/naml
bin/naml: $(NAML_OBJ_FILES) 
	$(MLCOMP) $(MLFLAGS) $(INCLUDE) $(GTK_TH_STUFF) $(NAML_OBJ_FILES) -o $@ 


camlgtk-th: bin/camlgtk-th
bin/camlgtk-th: 
	$(MLTOP) $(THFLAGS) $(INCLUDE) -o $@  $(GTK_TH_STUFF)

camlgtk: bin/camlgtk
bin/camlgtk: 
	$(MLTOP) $(INCLUDE) -o $@  $(GTK_STUFF)

testmodules:  $(MODULE_OBJ_FILES) $(MISC_OBJ_FILES) $(TEST_OBJ_FILES)
	$(MLCOMP) $(MLFLAGS) $(INCLUDE) $(MISC_OBJ_FILES) $(MODULE_OBJ_FILES) $(TEST_OBJ_FILES)  -o $(BIN_DIR)/$@ 

ocamlgfx: 
	ocamlmktop -custom -o bin/ocamlgfx $(GFX_LIB) -cclib -L/usr/X11/lib -cclib -lX11


CLEANALL = for d in $(DIRS); do (cd $$d; rm -f *.o *.cmx *.cmi *.cmo *.out); done
EMACSCLEANALL = for d in $(DIRS); do (cd $$d; rm -f *~; rm -f .*~); done

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
