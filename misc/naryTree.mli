(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

(** Functional n-ary trees. *)

type 'a t = Empty | Node of 'a * 'a t list

val map : f:('a -> 'b) -> 'a t -> 'b  t

val belongs : 'a -> 'a t -> bool
val height :  'a t -> int

val root : 'a t -> 'a
  (** Returns the node at the root of the tree. Raise [Failure "root"] if tree
    is empty.*)

val iter : f:('a -> unit) -> 'a t -> unit
  (** Iterate over tree, presenting each node once to the provided function. 
    Order is not specified. *)

val iter2 : f:(parent:'a -> child:'a -> unit) -> 'a t -> unit
  (** Iterate over tree, presenting each node along with its parent to the
    provided function. Order is not specified *)


val addnode : parent:'a -> node:'a -> 'a t -> 'a t 
  (** [NaryTree.addnode ~parent ~node tree] returns a new tree which is the
    result of adding [node] under [parent] in [tree]. Raise [Failure "nth"] if
    [parent] is not in [tree].*)


