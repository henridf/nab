(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

(** Data structures for representing globally routes and floods (for
  monitoring or gui purposes). 
  @author Henri Dubois-Ferriere.
*)

open Common

(** Simple list-based representation of a LER route.*)
   
(** A route between src and dest should start at the src and finish at the dest.
   Therefore  its length is 1 more than the # hops in the route.
   The anchor changes at the hop which does a new anchor search (or in the
   case of GREASE, at the hop which has itself a new anchor, in which case the 
   searchcost will be 0.)
   The anchor_age represents the age of the current anchor, and also changes 
   when the anchor changes.
   The searchcost can therefore only be non-zero at hops where the anchor is
   different than the previous hop.
*)



type ('a, 'b) hop_t = {hop:'a; mutable info:'b option}
    (** ['a] is typically a {!Common.nodeid_t} or a {!Coord.coordf_t} *)
    (* note: info's get copied around in some functions of this module.
       therefore, caution if creating a new info type with  mutable field(s). *)


type 'a ease_info_t = {
  anchor: 'a; 
  anchor_age: Common.time_t; 
  searchcost: float
}

type grep_info_t = Flood.t


type ('a, 'b) t = ('a, 'b) hop_t list
    (* the type is exposed so that list operations can be easily used on
       routes *)

type 'a ease_route_t = ('a, 'a ease_info_t) t
type 'a grep_route_t = ('a, grep_info_t) t
type 'a hops_only_route_t = ('a, unit) t



val create : unit -> ('a, 'b) t

val add_hop : ('a, 'b) t -> ('a, 'b) hop_t -> ('a, 'b) t 
  (* Returns a new route with hop at end *)

(*val append_hops : front:'a t -> back:'a t -> 'a t*)
  (* append a route to the end of another *)

val nth_hop : ('a, 'b) t -> int -> ('a, 'b) hop_t
val last_hop : ('a, 'b) t -> ('a, 'b) hop_t

val length : ('a, 'b) t -> int


  (*
    changes to make info an option made this a pain to compile...
val ease_route_valid : 'a ease_route_t -> src:'a -> dst:'a -> bool
  (* generic checks:
     - length >= 1
     - searchcost >= 0
     - searchcost can only be non-zero when anchor changes
     - anchor_age must be monotonically decreasing
     - anchor_age can only change when anchor changes
     - starts at src, ends at dst
     - is loop-free (no hop is repeated twice)
  *)

val anchor_cost : 'a ease_route_t -> float 
  (* Sum of squares of all search radii *)

  *)
val eucl_length : dist_f:(Coord.coordf_t -> Coord.coordf_t -> float) ->
  (Coord.coordf_t, 'b) t -> float
  (* Compute Euclidean length of route given a distance function *)


val i2c : (Common.nodeid_t, 'b) t -> (Coord.coordf_t, 'b) t

val sprint : (Coord.coordf_t, 'b) t -> string  

