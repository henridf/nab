(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)
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
let pkts_to_recv = ref 0
let data_pkts_orig = ref 0

let rreq_pkts_sent = ref 0
let rrer_pkts_sent = ref 0

let rrep_rerr_pkts_sent = ref 0

let set_sources n = (
  sources := n;
  pkts_originated := (Array.make n 0);
  pkts_received :=  (Array.make n 0);
  data_pkts_sent := 0;
  data_pkts_orig := 0;
  data_pkts_drop := 0;
  data_pkts_drop_rerr := 0;
  data_pkts_recv := 0;
  pkts_to_recv := 0;
  rreq_pkts_sent := 0;
  rrer_pkts_sent := 0;
  rrep_rerr_pkts_sent := 0;
  total_pkts_sent := 0;
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
  incr data_pkts_orig
)

let set_stop_thresh thepkts_to_recv = (
  pkts_to_recv := thepkts_to_recv
)

let recv_data() = (
  incr data_pkts_recv;
  Printf.printf "recv : %d\n" !data_pkts_recv;
  flush stdout;
  if !data_pkts_recv = !pkts_to_recv then
    (Gsched.sched())#stop_in 0.1;
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
