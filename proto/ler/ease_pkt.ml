



type t = {
  mutable enc_age : Time.time_t;
  mutable anchor_pos : Coord.coordf_t;
  mutable search_dist : float;
}

let make_ease_hdr
  ~enc_age
  ~anchor_pos
  =  {
    enc_age=enc_age;
    anchor_pos=anchor_pos;
    search_dist=0.0
  }
	
let clone ease_pkt = {ease_pkt with enc_age=ease_pkt.enc_age}
let anchor ease_hdr = ease_hdr.anchor_pos
let enc_age ease_hdr = ease_hdr.enc_age
let search_dist ease_hdr = ease_hdr.search_dist

let set_search_dist ease_hdr d = 
  ease_hdr.search_dist <- d

let set_anchor_pos ease_hdr anch = 
  ease_hdr.anchor_pos <- anch

let set_enc_age ease_hdr age = 
  ease_hdr.enc_age <- age
