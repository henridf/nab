(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

(** {2 Application Layer (L4) Packet Types}
  
  @author Henri Dubois-Ferriere.
*)

type hello_payload_t = Coord.coordf_t




type t =
    [ `NONE
    | `APP_PKT
    | `HELLO_PKT of hello_payload_t
    ]


(** {2 Application Layer (L4) Packet Constructors} *)

val clone_l4pkt : l4pkt:'a -> 'a


(** {2 Application Layer (L4) Packet Manipulators} *)

val l4pkt_size : l4pkt:t -> int
