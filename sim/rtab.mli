(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

type rtab_t


type aodv_flags = {
  mutable valid:bool;
}

type spec = [ `GREP | `AODV of aodv_flags ]

type rtab_entry_t = {
  mutable seqno: int option;
  mutable nexthop: Common.nodeid_t option;
  mutable hopcount: int option;
  mutable repairing: bool;
  other: spec
}

val create_aodv : size:int -> rtab_t
val create_grep : size:int -> rtab_t

val seqno : rt:rtab_t -> dst:Common.nodeid_t -> int option
val nexthop : rt:rtab_t -> dst:Common.nodeid_t -> Common.nodeid_t option
val hopcount : rt:rtab_t -> dst:Common.nodeid_t -> int option

val invalidate : rt:rtab_t -> dst:Common.nodeid_t -> unit
val invalid : rt:rtab_t -> dst:Common.nodeid_t -> bool
val repairing  : rt:rtab_t -> dst:Common.nodeid_t -> bool
val repair_start  : rt:rtab_t -> dst:Common.nodeid_t -> unit
val repair_done  : rt:rtab_t -> dst:Common.nodeid_t -> unit

val newadv : 
  rt:rtab_t -> 
  dst:Common.nodeid_t -> 
  sn:int -> hc:int -> nh:int ->
  bool
  (* looks at rtent (proposed new routing entry), updates routing table 
     if fresher seqno or same seqno and shorter hopcount *)

val newadv_ignorehops : 
  rt:rtab_t -> 
  dst:Common.nodeid_t -> 
  sn:int -> hc:int -> nh:int ->
  bool
  (* same as newadv except based on seqnos only: a fresher or equal seqno is
  accepted, older seqno not *)


val clear_entry : rt:rtab_t -> dst:Common.nodeid_t -> unit
  (** Set entry for dst back to 'empty' state (ie, state when a routing table
    is initially created *)

val clear_all_entries : rt:rtab_t -> unit
  (** Set all entries back to 'empty' state (ie, state when a routing table
    is initially created *)

