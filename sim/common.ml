open Printf


type nodeid_t = int


type time_t = float


type enc_t = {
  mutable t: time_t; 
  mutable p: Coord.coordf_t
}
    
let time_ = ref 0.0

let set_time t = time_ := t

let get_time () = !time_
let time = get_time


let enc ~time ~place = {t = time; p = place}
let enc_age  m = (get_time ()) -. m.t






