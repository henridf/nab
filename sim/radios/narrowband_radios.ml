(*
 *
 *  NAB - Network in a Box
 *  Henri Dubois-Ferriere, LCA/LCAV, EPFL
 * 
 *  Copyright (C) 2004 Laboratory of Audiovisual Communications (LCAV), and
 *  Laboratory for Computer Communications and Applications (LCA), 
 *  Ecole Polytechnique Federale de Lausanne (EPFL),
 *  CH-1015 Lausanne, Switzerland
 *
 *  This file is part of NAB. NAB is free software; you can redistribute it 
 *  and/or modify it under the terms of the GNU General Public License as 
 *  published by the Free Software Foundation; either version 2 of the License,
 *  or (at your option) any later version. 
 *
 *  NAB is distributed in the hope that it will be useful, but WITHOUT ANY
 *  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 *  FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 *  details (enclosed in the file GPL). 
 *
 *)

(* $Id$ *)

open Radiochips

let xemics1205 = {
    description = "Xemix1205";
    pt_min = 0.;
    pt_max = 15.;
    p_n = -99.;
    modulation = FSK_coherent;
    encoding = NRZ;
    rate = 78000.;
    b_n = 200000.
  }


let cc1000 = {
    description = "CC1000";
    pt_min = -20.;
    pt_max = 5.;
    p_n = -105.;
    modulation = FSK_noncoherent;
    encoding = Manchester;
    rate = 19200.;
    b_n = 30000.
  }
