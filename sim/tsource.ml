(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

open Misc

type traffic_generator_t = (unit -> float option)

let rndgen = Random.State.copy (Random.get_state())

let make_finite_trafficsource f num_pkts = 
    let ct_ = ref 0 in 
    let f_ = fun()  -> (
      incr ct_;   if !ct_ < num_pkts then Some (f())  else None)
    in
    f_


let make_cbr ?num_pkts ~pkts_per_sec () = 
  let time_to_next_pkt() = 1.0 /. pkts_per_sec in
  match num_pkts with 
    | None -> fun () -> Some (time_to_next_pkt())
    | Some n ->  make_finite_trafficsource time_to_next_pkt n
  
    
    
let make_poisson ?num_pkts ~lambda () =
  let time_to_next_pkt() = 
    let rand = Random.State.float rndgen 1.0 in
    Misc.expo ~rand ~lambda
  in
  match num_pkts with 
    | None -> fun () -> Some (time_to_next_pkt())
    | Some n -> make_finite_trafficsource time_to_next_pkt n
    
