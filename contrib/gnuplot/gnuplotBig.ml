open Gnuplot
open Bigarray

module BigarrayModuleC : 
  (GnuPlotArrayType 
  with type vec = (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t
  and type mat = (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array2.t) = 
struct
  type  vec = (float, Bigarray.float64_elt, c_layout) Bigarray.Array1.t
  type  mat = (float, Bigarray.float64_elt, c_layout) Bigarray.Array2.t
  let dim = Bigarray.Array1.dim 
  let get1 vec i = vec.{i}
  let get2 mat i j = mat.{i,j}
end
  
module BigarrayModuleFortran : 
  (GnuPlotArrayType 
  with type vec = (float, Bigarray.float64_elt, Bigarray.fortran_layout) Bigarray.Array1.t
  and type mat = (float, Bigarray.float64_elt, Bigarray.fortran_layout) Bigarray.Array2.t) = 
struct
  type  vec = (float, Bigarray.float64_elt, fortran_layout) Bigarray.Array1.t
  type  mat = (float, Bigarray.float64_elt, fortran_layout) Bigarray.Array2.t
  let dim = Bigarray.Array1.dim 
  let get1 vec i = vec.{i}
  let get2 mat i j = mat.{i,j}
end


module GnuplotBigarrayC = Make (BigarrayModuleC)
module GnuplotBigarrayFortran = Make (BigarrayModuleFortran)
