type rtab_t

type rtab_entry_t = {
  mutable seqno: int option;
  mutable nexthop: Common.nodeid_t option;
  mutable hopcount: int option
}

val create : size:int -> rtab_t

val seqno : rt:rtab_t -> dst:Common.nodeid_t -> int option
val nexthop : rt:rtab_t -> dst:Common.nodeid_t -> Common.nodeid_t option
val hopcount : rt:rtab_t -> dst:Common.nodeid_t -> int option
val invalidate : rt:rtab_t -> dst:Common.nodeid_t -> unit
val invalid : rt:rtab_t -> dst:Common.nodeid_t -> bool

val newadv : 
  rt:rtab_t -> 
  dst:Common.nodeid_t -> 
  rtent:rtab_entry_t ->
  bool
  (* looks at rtent (proposed new routing entry), updates routing table 
     if fresher seqno or same seqno and shorter hopcount *)

val newadv_ignorehops : 
  rt:rtab_t -> 
  dst:Common.nodeid_t -> 
  rtent:rtab_entry_t 
  -> bool
  (* same as newadv except based on seqnos only: a fresher or equal seqno is
  accepted, older seqno not *)
