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
MWS_BASE_DIR = $(MWS_DIR)/base
MWS_INTF_DIR = $(MWS_DIR)/interfaces
MWS_MAC_DIR = $(MWS_DIR)/mac
MWS_MOB_DIR = $(MWS_DIR)/mob
MWS_PKT_DIR = $(MWS_DIR)/pkt
MWS_DIRS = $(MWS_BASE_DIR) $(MWS_INTF_DIR) $(MWS_MAC_DIR) $(MWS_MOB_DIR) $(MWS_PKT_DIR)


PROTO_DIR = proto
PROTO_LER_DIR = $(PROTO_DIR)/ler
PROTO_GREP_DIR = $(PROTO_DIR)/grep
PROTO_AODV_DIR = $(PROTO_DIR)/aodv
PROTO_DIFF_DIR = $(PROTO_DIR)/diff
PROTO_MISC_DIR = $(PROTO_DIR)/misc
PROTO_DIRS = $(PROTO_GREP_DIR) $(PROTO_AODV_DIR) $(PROTO_DIFF_DIR) \
	$(PROTO_MISC_DIR) $(PROTO_LER_DIR) 

MWS_LIB_DIR = lib
MWS_LIB_CONTRIB_DIR = $(MWS_LIB_DIR)/contrib
MWS_LIB_DIRS = $(MWS_LIB_DIR) $(MWS_LIB_CONTRIB_DIR)


MWS_SCRIPT_DIR = scripts
GUI_DIR = gui
GUI_DATA_DIR = $(GUI_DIR)/data
MK_DIR = mk
TEST_DIR = test

# target dirs
DOC_TARGET_DIR = doc
BIN_DIR = bin

GTK_DIR = $(LIB_DIR)/lablgtk
CAMLIMAGES_DIR = $(LIB_DIR)/camlimages

INCLUDE_SRC = $(foreach dir,$(DIRS), -I $(dir))
INCLUDE_LIBS = -I $(GTK_DIR)
INCLUDE = $(INCLUDE_LIBS) $(INCLUDE_SRC) -I $(MWS_SCRIPT_DIR) -I $(GUI_DATA_DIR)

INCLUDE_CAMLIMAGES = -I $(CAMLIMAGES_DIR)

LINKFLAGS_CAMLIMAGES = -cclib "-L/usr/lib/ocaml/camlimages"  
THFLAGS = -thread


DIRS = 	$(MWS_LIB_DIRS) \
	$(GUI_DIR) \
	$(TEST_DIR) \
	$(MWS_DIRS) \
	$(PROTO_DIRS) 


# For clean_* targets, we don't rm gui/data/ files (they are slow to compile and 
# never change)
CLEAN_DIRS = $(DIRS) $(MWS_SCRIPT_DIR) $(MK_DIR)

DEPEND_DIRS = $(DIRS) $(MWS_SCRIPT_DIR)	$(GUI_DATA_DIR)

DOC_DIRS = $(DIRS) $(GUI_DATA_DIR)
DOC_DIR = doc
DOC_GEN_DIR = doc/gen

DEPEND_FILES := $(foreach dir,$(DEPEND_DIRS),$(wildcard $(dir)/*mli)) \
	$(foreach dir,$(DEPEND_DIRS),$(wildcard $(dir)/*ml))



ifdef SCRIPT
	MWS_SCRIPT = $(MWS_SCRIPT_DIR)/$(SCRIPT)
endif

############
# Libraries

GFX_LIB = $(LIB_DIR)/graphics$(CMA)
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



# Files to generate doc for (all except scripts and gui/data/ files)
DOC_FILES := $(foreach dir,$(DOC_DIRS),$(wildcard $(dir)/*mli)) \
	$(foreach dir,$(DOC_DIRS),$(wildcard $(dir)/*ml)) \
	$(MWS_SCRIPT_DIR)/script_utils.mli \
	$(MWS_SCRIPT_DIR)/script_utils.ml


##########################################
# Files corresponding to different subdirs


MWS_OBJS = $(GFX_LIB) \
		$(STR_LIB) \
		$(MWS_LIB_OBJS) \
		$(GUI_DIR)/epflcoords$(CMO) \
		$(GUI_DIR)/read_coords$(CMO) \
		$(MWS_LIB_DIR)/param$(CMO) \
		$(MWS_LIB_DIR)/linkedlist$(CMO) \
		$(MWS_INTF_DIR)/rt_agent$(CMO) \
		$(MWS_INTF_DIR)/scheduler$(CMO) \
		$(MWS_INTF_DIR)/worldt$(CMO) \
		$(MWS_INTF_DIR)/mac$(CMO) \
		$(MWS_INTF_DIR)/trafficgen$(CMO) \
		$(MWS_BASE_DIR)/time$(CMO) \
		$(MWS_BASE_DIR)/common$(CMO) \
		$(MWS_PKT_DIR)/pkt_common$(CMO) \
		$(MWS_PKT_DIR)/l4pkt$(CMO) \
		$(PROTO_LER_DIR)/ease_pkt$(CMO) \
		$(PROTO_GREP_DIR)/grep_pkt$(CMO) \
		$(PROTO_AODV_DIR)/aodv_pkt$(CMO) \
		$(PROTO_DIFF_DIR)/diff_pkt$(CMO) \
		$(PROTO_MISC_DIR)/simple_pkt$(CMO) \
		$(MWS_PKT_DIR)/l3pkt$(CMO) \
		$(MWS_PKT_DIR)/l2pkt$(CMO) \
		$(MWS_LIB_DIR)/log$(CMO) \
		$(MWS_BASE_DIR)/world$(CMO) \
		$(MWS_BASE_DIR)/params$(CMO) \
		$(MWS_LIB_CONTRIB_DIR)/heap$(CMO) \
		$(MWS_BASE_DIR)/sched$(CMO) \
		$(MWS_BASE_DIR)/nodes$(CMO) \
		$(MWS_BASE_DIR)/route$(CMO) \
		$(MWS_BASE_DIR)/flood$(CMO) \
		$(PROTO_LER_DIR)/le_tab$(CMO) \
		$(MWS_BASE_DIR)/rtab$(CMO) \
		$(MWS_BASE_DIR)/ether$(CMO) \
		$(MWS_BASE_DIR)/tsource$(CMO) \
		$(MWS_BASE_DIR)/simplenode$(CMO) \
		$(MWS_MAC_DIR)/mac_base$(CMO) \
		$(MWS_MAC_DIR)/mac_null$(CMO) \
		$(MWS_MAC_DIR)/mac_contention$(CMO) \
		$(MWS_MAC_DIR)/mac_cheat$(CMO) \
		$(MWS_BASE_DIR)/gpsnode$(CMO) \
		$(PROTO_GREP_DIR)/grep_hooks$(CMO) \
		$(PROTO_GREP_DIR)/aodv_grep_common$(CMO) \
		$(MWS_BASE_DIR)/rt_agent_base$(CMO) \
		$(PROTO_AODV_DIR)/aodv_agent$(CMO) \
		$(PROTO_GREP_DIR)/grep_agent$(CMO) \
		$(PROTO_DIFF_DIR)/diff_agent$(CMO) \
		$(PROTO_LER_DIR)/ease_agent$(CMO) \
		$(PROTO_MISC_DIR)/hello_agents$(CMO) \
		$(PROTO_MISC_DIR)/flood_agent$(CMO) \
		$(MWS_BASE_DIR)/gui_hooks$(CMO) \
		$(MWS_MOB_DIR)/mob_base$(CMO) \
		$(MWS_MOB_DIR)/mobs$(CMO) \
		$(MWS_BASE_DIR)/mob_ctl$(CMO) \
		$(MWS_BASE_DIR)/crsearch$(CMO) \
		$(MWS_BASE_DIR)/crworld$(CMO) \
		$(MWS_SCRIPT_DIR)/script_utils$(CMO) \
		$(MWS_BASE_DIR)/persistency$(CMO)
# script_utils should be as near to end as possible
# because only scripts should need to use it
# persistency gets an exception because it performs 
# setup-type functions, similar to those done in a script

GUI_OBJS = $(MWS_OBJS) \
		$(GUI_DATA_DIR)/epfl$(CMO) \
		$(GUI_DATA_DIR)/blank$(CMO) \
		$(GUI_DIR)/params_gui$(CMO) \
		$(GUI_DIR)/mwsconv$(CMO) \
		$(GUI_DIR)/gui_gtk$(CMO) \
		$(GUI_DIR)/gui_ops$(CMO) \
		$(GUI_DIR)/gui_ctl$(CMO) \
		$(GUI_DIR)/gui_grep$(CMO) 

MODULE_OBJS = \
	$(MWS_LIB_DIR)/larray$(CMO) \
	$(MWS_LIB_DIR)/circbuf$(CMO) \
	$(MWS_LIB_DIR)/itin$(CMO)


MWS_LIB_OBJS = $(MWS_LIB_DIR)/misc$(CMO) \
		 $(MWS_LIB_DIR)/mods$(CMO) \
		 $(MWS_LIB_CONTRIB_DIR)/opt$(CMO) \
		 $(MWS_LIB_DIR)/randoms$(CMO) \
		 $(MWS_LIB_DIR)/coord$(CMO) \
		 $(MWS_LIB_DIR)/naryTree$(CMO) \
		$(MWS_LIB_DIR)/graph$(CMO)

TEST_OBJS = \
	$(TEST_DIR)/testUtils$(CMO) \
	$(MWS_LIB_DIR)/coord$(CMO) \
	$(TEST_DIR)/graph-test$(CMO) \
	$(TEST_DIR)/itin-test$(CMO) \
	$(TEST_DIR)/circbuf-test$(CMO)


%.cmo: %.ml
	$(MLCOMP) $(MLFLAGS) $(INCLUDE) -c $<

%.cmi: %.mli
	$(MLCOMP) $(MLFLAGS) $(INCLUDE) -c $<

%.cmx: %.ml
	$(MLCOMP) $(MLFLAGS) $(INCLUDE) -c $<

alltargets: mws mwsgrep grepviz mws-top mwsviz mwsvor mwsviz-top


mws: bin/mws
bin/mws: $(MWS_OBJS) $(MWS_SCRIPT)
	$(MLCOMP) $(MLFLAGS) $(INCLUDE) $(UNIX_LIB) $(MWS_OBJS) $(MWS_SCRIPT) -o $@ 

mwsgrep: bin/mwsgrep
bin/mwsgrep: $(MWS_OBJS) scripts/grep_common$(CMO) scripts/grep$(CMO)
	$(MLCOMP) $(MLFLAGS) $(INCLUDE)  $(UNIX_LIB) $(MWS_OBJS) scripts/grep_common$(CMO) scripts/grep$(CMO) -o $@ 

grepviz: bin/grepviz
bin/grepviz: $(GUI_OBJS) scripts/grep_common$(CMO) scripts/grepviz$(CMO)
	$(MLCOMP) $(MLFLAGS) $(INCLUDE) $(GTK_STUFF) $(GUI_OBJS) scripts/grep_common$(CMO) scripts/grepviz$(CMO) -o $@ 

mwsvor: bin/mwsvor
bin/mwsvor:  $(MWS_OBJS) scripts/voronoi_common$(CMO)
	$(MLCOMP) $(MLFLAGS) $(INCLUDE) $(UNIX_LIB) $(STR_LIB) $(MWS_OBJS) scripts/voronoi_common$(CMO) $(MWS_SCRIPT) -o $@ 

mws-top: bin/mws-top
bin/mws-top: $(MWS_OBJS)  $(MWS_SCRIPT)
	$(MLTOP) $(INCLUDE) $(UNIX_LIB) $(MWS_OBJS)  $(MWS_SCRIPT) -o $@

mwsviz: bin/mwsviz
bin/mwsviz: $(GUI_OBJS) $(MWS_SCRIPT)
	$(MLCOMP) $(MLFLAGS) $(INCLUDE) $(GTK_STUFF) \
	$(GUI_OBJS) $(MWS_SCRIPT) -o $@ 

mwsviz-top: bin/mwsviz-top
bin/mwsviz-top: $(GUI_OBJS) $(MWS_SCRIPT)
	$(MLTOP) $(INCLUDE) $(GTK_STUFF) $(GUI_OBJS) $(MWS_SCRIPT) -o $@ 



htmldoc: $(GUI_OBJS)
	$(OCAMLDOC) -html -sort -d $(DOC_GEN_DIR)  $(INCLUDE)  $(DOC_FILES)

dotdoc:
	$(OCAMLDOC) -dot -d $(DOC_GEN_DIR)  $(INCLUDE)  $(DOC_FILES); dot -Tgif ocamldoc.out -o graph.gif

camlgtk-th: bin/camlgtk-th
bin/camlgtk-th: 
	$(MLTOP) $(THFLAGS) $(INCLUDE) -o $@  $(GTK_TH_STUFF)

camlgtk: bin/camlgtk
bin/camlgtk: 
	$(MLTOP) $(INCLUDE) -o $@  $(GTK_STUFF)

testmodules:  $(MODULE_OBJS) $(MWS_LIB_OBJS) $(TEST_OBJS)
	$(MLCOMP) $(MLFLAGS) $(INCLUDE) $(MWS_LIB_OBJS) $(MODULE_OBJS) $(TEST_OBJS)  -o $(BIN_DIR)/$@ 

ocamlgfx: 
	ocamlmktop -custom -o bin/ocamlgfx $(GFX_LIB) -cclib -L/usr/X11/lib -cclib -lX11

ocamlstr: 
	ocamlmktop -o bin/ocamlstr $(STR_LIB) 

OPTCLEANALL = for d in $(CLEAN_DIRS); do (cd $$d; rm -f *.cmx); done
BYTECLEANALL = for d in $(CLEAN_DIRS); do (cd $$d; rm -f *.cmi *.cmo); done
CLEANALL = for d in $(CLEAN_DIRS); do (cd $$d; rm -f *.o *.out *.annot *.cm*); done
EMACSCLEANALL = for d in $(CLEAN_DIRS); do (cd $$d; rm -f *~; rm -f .*~); done
DOCCLEAN = for d in $(DOC_GEN_DIR); do (cd $$d; rm -f *html; rm -f .*css); done

bclean:
	$(BYTECLEANALL)
	rm -f  *.cmi *.cmo 

oclean:
	$(OPTCLEANALL)

	rm -f  *.cmx
clean:  
	$(CLEANALL) 
	rm -f *.o *.cmx *.cmi *.cmo a.out 
eclean:
	$(EMACSCLEANALL)
	rm -f *~
	rm -f .*~
docclean:
	$(DOCCLEAN)


DEPEND = .depend

.depend:
	$(OCAMLDEP) $(INCLUDE_SRC) $(DEPEND_FILES) > $(DEPEND)

depend: .depend

include $(DEPEND)
