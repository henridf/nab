(*

  Copyright (C) 2004 Swiss Federal Institute of Technology Lausanne (EPFL),
  Laboratory of Audiovisual Communications (LCAV) and 
  Laboratory for Computer Communications and Applications (LCA), 
  CH-1015 Lausanne, Switzerland

  Author: Henri Dubois-Ferriere 

  This file is part of mws (multihop wireless simulator).

  Mws is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.
  
  Mws is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.
  
  You should have received a copy of the GNU General Public License
  along with mws; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA


*)






open Printf

let sources = ref 5
let pkts_originated = ref [||]
let pkts_received = ref [||]

(* any new counter/value must be reset in set_sources *)
let total_pkts_sent = ref 0
let data_pkts_sent = ref 0
let data_pkts_drop = ref 0
let data_pkts_drop_rerr = ref 0
let data_pkts_recv = ref 0
let pkts_to_send = ref 0
let data_pkts_orig = ref 0

let rreq_pkts_sent = ref 0
let rrer_pkts_sent = ref 0

let rrep_rerr_pkts_sent = ref 0

let inv_viol = ref 0

let set_sources n = (
  sources := n;
  pkts_originated := (Array.make n 0);
  pkts_received :=  (Array.make n 0);
  data_pkts_sent := 0;
  data_pkts_orig := 0;
  data_pkts_drop := 0;
  data_pkts_drop_rerr := 0;
  data_pkts_recv := 0;
  pkts_to_send := 0;
  rreq_pkts_sent := 0;
  rrer_pkts_sent := 0;
  rrep_rerr_pkts_sent := 0;
  total_pkts_sent := 0;
  inv_viol := 0;
)


let reset() = (
  set_sources !sources;
)

let sent_data() = (
  incr total_pkts_sent;
  incr data_pkts_sent
)

let orig_data() = (
  incr total_pkts_sent;
  incr data_pkts_orig;
  if !data_pkts_orig = !pkts_to_send then
      (Sched.s())#stop_in 0.1;
)

let set_stop_thresh thepkts_to_send = (
  pkts_to_send := thepkts_to_send
)

let recv_data() = (
  incr data_pkts_recv;
  flush stdout;
(*~t:(Sched.ASAP);*)
)

let drop_data() = (
  incr data_pkts_drop
)

let drop_data_rerr() = (
  incr data_pkts_drop_rerr
)

let sent_rreq() = (
  incr total_pkts_sent;
  incr rreq_pkts_sent
)

let sent_rrep_rerr() = (
  incr total_pkts_sent;
  incr rrep_rerr_pkts_sent
)

let inv_violation() = (
  incr inv_viol
)
