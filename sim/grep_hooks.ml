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
let rreq_pkts_sent = ref 0
let rrer_pkts_sent = ref 0
let rrep_pkts_sent = ref 0

let set_sources n = (
  sources := n;
  pkts_originated := (Array.make n 0);
  pkts_received :=  (Array.make n 0);
  data_pkts_sent := 0;
  rreq_pkts_sent := 0;
  rrer_pkts_sent := 0;
  rrep_pkts_sent := 0;
  total_pkts_sent := 0;
)


let reset() = (
  set_sources !sources;
)

let sent_data() = (
  incr total_pkts_sent;
  incr data_pkts_sent
)

let sent_rreq() = (
  incr total_pkts_sent;
  incr data_pkts_sent
)

let sent_rrep() = (
  incr total_pkts_sent;
  incr data_pkts_sent
)
