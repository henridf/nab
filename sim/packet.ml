(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

let pkt_uid = ref 0



(* ports are hidden (for now?) because packet types are distinguished 
   by the payload type *)
type l3hdr_t = {
  mutable src: Common.nodeid_t;
  (* mutable src_port: Common.port_t;*)
  mutable dst: Common.nodeid_t;
  (* mutable dst_port: Common.port_t;*)
  mutable ttl : int
}

type l2_dst_t = L2_BCAST | L2_DST of Common.nodeid_t

type l2hdr_t = {
  mutable l2src: Common.nodeid_t;
  mutable l2dst: l2_dst_t
}

type bler_payload_t = 
  | ANCH_REQ of (Common.nodeid_t * Common.time_t)  (* dst, current encounter age *)
  | ANCH_RPLY of (Common.nodeid_t * Common.time_t) (* anchorid, encounter age *)

type app_payload_t = 
    APP_PLD



type 'a pm_packet_t = {
  l3hdr : l3hdr_t;
  mutable pld : 'a 
}

type app_packet_t = app_payload_t pm_packet_t
type bler_packet_t = bler_payload_t pm_packet_t

(* originally the intent was to have packet_t be the polymorphic type 
   which is defined above as pm_packet_t, but this was way too painful 
   with polymorphic objects, so we wrap the polymorphic type with a sum 
   type *)
type packet_t = 
  | APP_PKT of app_packet_t 
  | BLER_PKT of bler_packet_t
  | FLOOD_PKT of app_packet_t

type l2packet_t = {
  l2hdr : l2hdr_t;
  l3pkt : packet_t
}
    

let get_l3hdr (pkt:packet_t) = 
  match pkt with
    | APP_PKT p -> p.l3hdr
    | BLER_PKT p -> p.l3hdr

let get_src (pkt:packet_t) = (get_l3hdr pkt).src
let get_dst (pkt:packet_t) = (get_l3hdr pkt).dst


let make_l3hdr ~srcid ~dstid ?(ttl=255) () = {
  src=srcid; 
  dst=dstid;
  ttl=ttl
}

let make_app_pkt ~l3hdr = {
  l3hdr=l3hdr;
  pld=APP_PLD
}


let make_bler_pkt ~srcid ~dstid ~l3hdr ~blerpld = {

  l3hdr=l3hdr;
  pld=(blerpld:bler_payload_t)
}


let make_l2pkt ~srcid ~l2_dst ~l3pkt = 
  let l2hdr = {l2src=srcid; l2dst=l2_dst} 
  in {l2hdr=l2hdr; l3pkt=l3pkt}
