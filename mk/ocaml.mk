# -*- Mode: makefile -*- 
#*************************************************************#
#
# OCAML: definitions for the bytecode compiler
#
# Author: Mark Hayden, 2/96
#
#*************************************************************#
MLCOMP		= $(CAML_BIN_DIR)/ocamlc.opt
MLTOP		= $(CAML_BIN_DIR)/ocamlmktop
MLLINK		= $(MLCOMP)
MLLIBR		= $(MLCOMP) -a $(DEBUGGER)
CMI		= .cmi
CMOS		= .cmo
CMAS		= .cma
CMO		= $(CMOS)
CMA		= $(CMAS)
#*************************************************************#
MLWARN		= -w A
MLFAST		=# -unsafe
DEBUGGER        =-g #-ccopt -lefence
DTYPES	 	=#-dtypes
COMP_THR	= #-thread
MLFLAGS		=  $(DTYPES) $(DEBUGGER) $(MLWARN) $(MLFAST) $(COMP_THR) 
MLLINKFLAGS	= $(DEBUGGER) $(MLWARN) $(MLFAST) $(MLTHREAD)
DEPFLAGS	= -noopt
ENSCOMP		= $(MLCOMP) $(MLFLAGS)
MLRUNTIME	= $(OCAML_LIB)/libcamlrun$(ARCS)
#*************************************************************#
CUSTOM		= -custom
#*************************************************************#
