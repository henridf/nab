# -*- Mode: makefile -*- 
#*************************************************************#
#
# OCAML: definitions for the bytecode compiler
#
# Author: Mark Hayden, 2/96
#
#*************************************************************#
MLCOMP		= ocamlc.opt
MLTOP		= ocamlmktop
MLLINK		= $(MLCOMP)
MLLIBR		= $(MLCOMP) -a $(DEBUGGER)
CMI		= .cmi
CMOS		= .cmo
CMAS		= .cma
CMO		= $(CMOS)
CMA		= $(CMAS)
#*************************************************************#
MLWARN		=
MLFAST		=# -unsafe
DEBUGGER        =-g #-ccopt -lefence
COMP_THR	= #-thread
MLFLAGS		= $(DEBUGGER) $(MLFAST) $(COMP_THR) 
MLLINKFLAGS	= $(DEBUGGER) $(MLWARN) $(MLFAST) $(MLTHREAD)
DEPFLAGS	= -noopt
ENSCOMP		= $(MLCOMP) $(MLFLAGS)
MLRUNTIME	= $(OCAML_LIB)/libcamlrun$(ARCS)
#*************************************************************#
CUSTOM		= -custom
#*************************************************************#
