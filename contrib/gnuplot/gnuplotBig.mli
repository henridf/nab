open Gnuplot
open Bigarray

module GnuplotBigarrayC : 
  (M with type vec = (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t      
     and type mat = (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array2.t)

module GnuplotBigarrayFortran : 
  (M with type vec = (float, Bigarray.float64_elt, Bigarray.fortran_layout) Bigarray.Array1.t      
     and type mat = (float, Bigarray.float64_elt, Bigarray.fortran_layout) Bigarray.Array2.t)
