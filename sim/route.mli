(* *** ********* *)
(* LER Simulator *)
(* *** ********* *)

(* Simple list-based representation of a LER route. *)
   
(* A route between src and dest should start at the src and finish at the dest.
   Therefore  its length is 1 more than the # hops in the route.
   The anchor changes at the hop which does a new anchor search (or in the
   case of GREASE, at the hop which has itself a new anchor, in which case the 
   searchcost will be 0.)
   The anchor_age represents the age of the current anchor, and also changes 
   when the anchor changes.
   The searchcost can therefore only be non-zero at hops where the anchor is
   different than the previous hop.
*)

open Common

type 'a hop = {hop:'a; anchor:'a; anchor_age: Common.time_t ; mutable searchcost:float}
    (* this is not enforced, but 'a should normally be an int or a coordf_t *)

type 'a t = 'a hop list
    (* the type is not abstract so that list operations can be easily used on
       routes *)

val create : unit -> 'a t

val add_hop : 'a t -> 'a hop -> 'a t 
  (* returns a new route with hop at end *)

val append_hops : front:'a t -> back:'a t -> 'a t
  (* append a route to the end of another *)

val nth_hop : 'a t -> int -> 'a hop
val last_hop : 'a t -> 'a hop

val length : 'a t -> int

(*
val i2c : int t -> Coord.coordf_t t
  (* convert an int route to a coordf_t route using the coordinates in
     ler_data_t *)
*)
  
val route_valid : 'a t -> src:'a -> dst:'a -> bool
  (* generic checks:
     - length >= 1
     - searchcost >= 0
     - searchcost can only be non-zero when anchor changes
     - anchor_age must be monotonically decreasing
     - anchor_age can only change when anchor changes
     - starts at src, ends at dst
  *)

val eucl_length : dist_f:(Coord.coordf_t -> Coord.coordf_t -> float) -> Coord.coordf_t t -> float
  (* Compute Euclidean length of route given a distance function *)

val anchor_cost : 'a t -> float 
  (* Sum of squares of all search radii *)

val i2c : int hop list -> Coord.coordf_t hop list

val sprint : Coord.coordf_t t -> string  

