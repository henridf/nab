(** A simple L3 packet extension type.
  @author Henri Dubois-Ferriere.
*)



type t = {
  seqno : int;
}

val hdr_size : t -> int

val clone : t -> t

val seqno : t -> int

val make_simple_hdr : seqno:int -> t

