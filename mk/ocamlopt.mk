# -*- Mode: makefile -*- 
#*************************************************************#
#
# OCAMLOPT: definitions for the native code compiler
#
# Author: Mark Hayden, 2/96
#
#*************************************************************#
MLCOMP		= ocamlopt.opt
MLTOP		= ocamlmktop
MLLINK		= $(MLCOMP)
MLLIBR		= $(MLCOMP) -a
CMI		= .cmi
CMOS		= .cmx
CMAS		= .cmxa
CMO		= $(CMOS)
CMA		= $(CMAS)
#*************************************************************#
COMPTYPE	= opt
MLWARN		=
MLFAST		= -unsafe -inline 10 -noassert
ifdef PROF
	PROFILE	= 	 -p
endif
DEBUGGER	=
MLFLAGS		= $(DEBUGGER) $(MLFAST) $(PROFILE)
MLLINKFLAGS	= $(MLFAST) $(PROFILE)
DEPFLAGS	= -opt
ENSCOMPFLAGS	= -opt $(MLFLAGS)
ENSCOMP		= $(MLCOMP) $(MLFLAGS)
MLRUNTIME	= $(OCAML_LIB)/libasmrun$(ARCS)
#*************************************************************#
CUSTOM		=# no -custom option for ocamlopt
#*************************************************************#
