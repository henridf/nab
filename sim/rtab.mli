type rtab_t

type rtab_entry_t = {
  mutable seqno: int option;
  mutable nexthop: Common.nodeid_t option;
  mutable hops: int option
}

val create : size:int -> rtab_t

val seqno : rtab:rtab_t -> dst:Common.nodeid_t -> int option
val nexthop : rtab:rtab_t -> dst:Common.nodeid_t -> Common.nodeid_t option
val hops : rtab:rtab_t -> dst:Common.nodeid_t -> int option


val newadv : rtab:rtab_t -> dst:Common.nodeid_t -> rtent:rtab_entry_t 
  -> bool
(* looks at rtent (proposed new routing entry), updates routing table as
   if fresher seqno or same seqno and shorter hops *)


