#*************************************************************#
# Choose whether to use the optimizing, profiling, or regular
# compiler.
#

ifdef OPT
	include mk/ocamlopt.mk
else
	include  mk/ocaml.mk	
endif

CAML_BIN_DIR	= $(dir $(shell which ocamlc))

OCAMLDOC = $(CAML_BIN_DIR)/ocamldoc.opt
OCAMLDEP = $(CAML_BIN_DIR)/ocamldep.opt

LIB_DIR = $(shell $(CAML_BIN_DIR)/ocamlc -where)


MWS_DIR = mws
PKT_DIR = pkt
MWS_SCRIPT_DIR = scripts
GUI_DIR = gui
GUI_DATA_DIR = $(GUI_DIR)/data
MISC_DIR = misc
MK_DIR = mk
TEST_DIR = test
BIN_DIR = bin

GTK_DIR = $(LIB_DIR)/lablgtk
GNUPLOT_DIR = contrib/gnuplot
CAMLIMAGES_DIR = $(LIB_DIR)/camlimages

INCLUDE_LIBS = -I $(GTK_DIR) -I $(GNUPLOT_DIR)
INCLUDE_SRC = -I $(MISC_DIR) -I $(PKT_DIR) -I $(TEST_DIR) -I $(MWS_DIR) -I $(GUI_DIR) 
INCLUDE_SCRIPTS = -I $(MWS_SCRIPT_DIR)
INCLUDE = $(INCLUDE_LIBS) $(INCLUDE_SRC) $(INCLUDE_SCRIPTS) -I $(GUI_DATA_DIR)
INCLUDE_DOCS = $(INCLUDE_LIBS) $(INCLUDE_SRC) 

INCLUDE_CAMLIMAGES = -I $(CAMLIMAGES_DIR)

LINKFLAGS_CAMLIMAGES = -cclib "-L/usr/lib/ocaml/camlimages"  
THFLAGS = -thread

DIRS = \
	$(MISC_DIR) \
	$(MK_DIR) \
	$(GUI_DIR) \
	$(TEST_DIR) \
	$(MWS_DIR) \
	$(PKT_DIR) \
	$(MWS_SCRIPT_DIR) \
	$(GUI_DIR)

DEPEND = .depend

DEPENDS = \
	$(MWS_DIR)/*.ml \
	$(MWS_DIR)/*.mli \
	$(PKT_DIR)/*.ml \
	$(PKT_DIR)/*.mli \
	$(GUI_DIR)/*.ml \
	$(GUI_DIR)/*.mli \
	$(GUI_DATA_DIR)/*.ml \
	$(GUI_DATA_DIR)/*.mli \
	$(MWS_SCRIPT_DIR)/*.ml \
	$(MWS_SCRIPT_DIR)/*.mli \
	$(TEST_DIR)/*.mli \
	$(MISC_DIR)/*.ml \
	$(MISC_DIR)/*.mli


ifdef SCRIPT
	MWS_SCRIPT = $(MWS_SCRIPT_DIR)/$(SCRIPT)
endif

############
# Libraries

GFX_LIB = $(LIB_DIR)/graphics$(CMA)
GNUPLOT_LIB = $(GNUPLOT_DIR)/gnuplot$(CMA)
UNIX_LIB = $(LIB_DIR)/unix$(CMA)
STR_LIB = $(LIB_DIR)/str$(CMA)
THREADS_LIB = $(LIB_DIR)/threads/threads$(CMA)


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
		$(STR_LIB) \
		$(GNUPLOT_LIB) \
		$(MISC_OBJ_FILES) \
		$(MISC_DIR)/graph$(CMO) \
		$(GUI_DIR)/epflcoords$(CMO) \
		$(GUI_DIR)/read_coords$(CMO) \
		$(MISC_DIR)/param$(CMO) \
		$(MISC_DIR)/linkedlist$(CMO) \
		$(MISC_DIR)/data$(CMO) \
		$(MWS_DIR)/common$(CMO) \
		$(PKT_DIR)/pkt_common$(CMO) \
		$(PKT_DIR)/l4pkt$(CMO) \
		$(PKT_DIR)/ease_pkt$(CMO) \
		$(PKT_DIR)/grep_pkt$(CMO) \
		$(PKT_DIR)/aodv_pkt$(CMO) \
		$(PKT_DIR)/diff_pkt$(CMO) \
		$(PKT_DIR)/l3pkt$(CMO) \
		$(PKT_DIR)/l2pkt$(CMO) \
		$(MWS_DIR)/log$(CMO) \
		$(MWS_DIR)/mac$(CMO) \
		$(MWS_DIR)/params$(CMO) \
		$(MISC_DIR)/heap$(CMO) \
		$(MWS_DIR)/rt_agent$(CMO) \
		$(MWS_DIR)/sched$(CMO) \
		$(MWS_DIR)/worldt$(CMO) \
		$(MWS_DIR)/world$(CMO) \
		$(MWS_DIR)/nodes$(CMO) \
		$(MWS_DIR)/route$(CMO) \
		$(MWS_DIR)/flood$(CMO) \
		$(MISC_DIR)/ler_graphics$(CMO) \
		$(MWS_DIR)/le_tab$(CMO) \
		$(MWS_DIR)/rtab$(CMO) \
		$(MWS_DIR)/ether$(CMO) \
		$(MWS_DIR)/tsource$(CMO) \
		$(MWS_DIR)/simplenode$(CMO) \
		$(MWS_DIR)/mac_base$(CMO) \
		$(MWS_DIR)/mac_null$(CMO) \
		$(MWS_DIR)/mac_contention$(CMO) \
		$(MWS_DIR)/gpsnode$(CMO) \
		$(MWS_DIR)/grep_hooks$(CMO) \
		$(MWS_DIR)/aodv_grep_common$(CMO) \
		$(MWS_DIR)/rt_agent_base$(CMO) \
		$(MWS_DIR)/aodv_agent$(CMO) \
		$(MWS_DIR)/grep_agent$(CMO) \
		$(MWS_DIR)/diff_agent$(CMO) \
		$(MWS_DIR)/ease_agent$(CMO) \
		$(MWS_DIR)/gui_hooks$(CMO) \
		$(MWS_DIR)/hello_agents$(CMO) \
		$(MWS_DIR)/mob$(CMO) \
		$(MWS_DIR)/mob_ctl$(CMO) \
		$(MWS_DIR)/crsearch$(CMO) \
		$(MWS_DIR)/crworld$(CMO) \
		$(MWS_DIR)/script_utils$(CMO) \
		$(MWS_DIR)/persistency$(CMO)
# script_utils should be as near to end as possible
# because only scripts should need to use it
# persistency gets an exception because it performs 
# setup-type functions, similar to those done in a script

GUI_OBJ_FILES = $(MWS_OBJ_FILES) \
		$(GUI_DATA_DIR)/epfl$(CMO) \
		$(GUI_DATA_DIR)/blank$(CMO) \
		$(GUI_DIR)/params_gui$(CMO) \
		$(GUI_DIR)/mwsconv$(CMO) \
		$(GUI_DIR)/gui_gtk$(CMO) \
		$(GUI_DIR)/gui_ops$(CMO) \
		$(GUI_DIR)/gui_ctl$(CMO) \
		$(GUI_DIR)/gui_grep$(CMO) 

MODULE_OBJ_FILES = \
	$(MISC_DIR)/graph$(CMO) \
	$(MISC_DIR)/larray$(CMO) \
	$(MISC_DIR)/circbuf$(CMO) \
	$(MISC_DIR)/itin$(CMO)


MISC_OBJ_FILES = $(MISC_DIR)/misc$(CMO) \
		 $(MISC_DIR)/mods$(CMO) \
		 $(MISC_DIR)/opt$(CMO) \
		 $(MISC_DIR)/randoms$(CMO) \
		 $(MISC_DIR)/coord$(CMO) \
		 $(MISC_DIR)/naryTree$(CMO)

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

alltargets: mws mwsgrep grepviz mwstop gui mwsvor guitop


mws: bin/mws
bin/mws: $(MWS_OBJ_FILES) $(MWS_SCRIPT)
	$(MLCOMP) $(MLFLAGS) $(INCLUDE) $(UNIX_LIB) $(MWS_OBJ_FILES) $(MWS_SCRIPT) -o $@ 

mwsgrep: bin/mwsgrep
bin/mwsgrep: $(MWS_OBJ_FILES) scripts/grep_common$(CMO) scripts/grep$(CMO)
	$(MLCOMP) $(MLFLAGS) $(INCLUDE)  $(UNIX_LIB) $(MWS_OBJ_FILES) scripts/grep_common$(CMO) scripts/grep$(CMO) -o $@ 

grepviz: bin/grepviz
bin/grepviz: $(GUI_OBJ_FILES) scripts/grep_common$(CMO) scripts/grepviz$(CMO)
	$(MLCOMP) $(MLFLAGS) $(INCLUDE) $(GTK_STUFF) $(GUI_OBJ_FILES) scripts/grep_common$(CMO) scripts/grepviz$(CMO) -o $@ 

mwsvor: bin/mwsvor
bin/mwsvor:  $(MWS_OBJ_FILES) scripts/voronoi_common$(CMO)
	$(MLCOMP) $(MLFLAGS) $(INCLUDE) $(UNIX_LIB) $(STR_LIB) $(MWS_OBJ_FILES) scripts/voronoi_common$(CMO) $(MWS_SCRIPT) -o $@ 

mwstop: bin/mwstop
bin/mwstop: $(MWS_OBJ_FILES)  $(MWS_SCRIPT)
	$(MLTOP) $(INCLUDE) $(UNIX_LIB) $(MWS_OBJ_FILES)  $(MWS_SCRIPT) -o $@

gui: bin/gui
bin/gui: $(GUI_OBJ_FILES) $(MWS_SCRIPT)
	$(MLCOMP) $(MLFLAGS) $(INCLUDE) $(GTK_STUFF) \
	$(GUI_OBJ_FILES) $(MWS_SCRIPT) -o $@ 

guitop: bin/guitop
bin/guitop: $(GUI_OBJ_FILES) $(MWS_SCRIPT)
	$(MLTOP) $(INCLUDE) $(GTK_STUFF) $(GUI_OBJ_FILES) $(MWS_SCRIPT) -o $@ 


DOC_FILES = \
	$(MWS_DIR)/*.ml \
	$(MWS_DIR)/*.mli \
	$(PKT_DIR)/*.ml \
	$(PKT_DIR)/*.mli \
	$(GUI_DATA_DIR)/*.ml \
	$(GUI_DATA_DIR)/*.mli \
	$(GUI_DIR)/*.ml \
	$(GUI_DIR)/*.mli \
	$(MISC_DIR)/*.ml \
	$(MISC_DIR)/*.mli

GUI_OBJ_ONLY_CMOS = $(filter %.cmo, $(GUI_OBJ_FILES))
GUI_ML_FILES = $(GUI_OBJ_ONLY_CMOS:.cmo=.ml)
DOC_DIR = doc

htmldoc: $(GUI_OBJ_FILES)
	$(OCAMLDOC) -html -sort -d $(DOC_DIR)  $(INCLUDE)  $(DOC_FILES)

dotdoc:
	$(OCAMLDOC) -dot -d $(DOC_DIR)  $(INCLUDE_DOCS)  $(DOC_FILES); dot -Tgif ocamldoc.out -o graph.gif

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

ocamlstr: 
	ocamlmktop -o bin/ocamlstr $(STR_LIB) 

OPTCLEANALL = for d in $(DIRS); do (cd $$d; rm -f *.cmx); done
BYTECLEANALL = for d in $(DIRS); do (cd $$d; rm -f *.cmi *.cmo); done
MISCCLEANALL = for d in $(DIRS); do (cd $$d; rm -f *.o *.out *.annot); done
EMACSCLEANALL = for d in $(DIRS); do (cd $$d; rm -f *~; rm -f .*~); done

bclean:
	$(BYTECLEANALL)
	rm -f  *.cmi *.cmo 

oclean:
	$(OPTCLEANALL)
	rm -f  *.cmx
clean:  bclean oclean
	$(MISCCLEANALL)
	rm -f *.o *.cmx *.cmi *.cmo a.out 
eclean:
	$(EMACSCLEANALL)
	rm -f *~
	rm -f .*~



.depend:
	$(OCAMLDEP) $(INCLUDE_SRC) $(DEPENDS) > $(DEPEND)

depend: .depend

include $(DEPEND)
