type 'a node 
type 'a llist = 'a node option ref

val create : unit -> 'a llist

val insert : ll:('a llist) -> v:'a -> compare:('a -> 'a -> bool) -> unit
  (* compare v1 v2 should return true if v1 should go before v2 *)

val pophead : 
  ll:('a llist) -> 
  'a option

val iter : 
  ('a -> unit) -> 
  ('a llist) -> 
  unit
