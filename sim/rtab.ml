(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

open Misc 

type aodv_flags = {
  mutable valid:bool;
}

type spec = [ `GREP | `AODV of aodv_flags ]

type rtab_entry_t = {
  mutable seqno: int option;
  mutable nexthop: Common.nodeid_t option;
  mutable hopcount: int option;
  mutable repairing: bool;
  other: spec
}

type rtab_t = rtab_entry_t array

let create_aodv ~size = Array.init size 
  (fun n -> {
    seqno=None; 
    nexthop=None;
    hopcount=None; 
    repairing=false; 
    other= `AODV {valid=false}
})

let create_grep ~size = Array.init size 
  (fun n -> {
    seqno=None; 
    nexthop=None;
    hopcount=None; 
    repairing=false; 
    other= `GREP
})

let seqno ~rt ~dst = rt.(dst).seqno
let nexthop ~rt ~dst = rt.(dst).nexthop
let hopcount ~rt ~dst = rt.(dst).hopcount

let invalidate ~rt ~dst = (
(*
  rt.(dst).hopcount <- Some max_int;
  rt.(dst).nexthop <- None;
*)
  match rt.(dst).other with 
    | `GREP -> ()
    | `AODV flags -> flags.valid <- false
)

let validate ~rt ~dst = 
  match rt.(dst).other with 
    | `GREP -> ()
    | `AODV flags -> flags.valid <- true


let invalid ~rt ~dst = 
(*rt.(dst).hopcount = Some max_int*)
  match rt.(dst).other with 
    | `GREP -> false
    | `AODV flags -> not flags.valid

(*  *)

let repairing ~rt ~dst = 
  rt.(dst).repairing 

let repair_start ~rt ~dst = 
  rt.(dst).repairing <- true

let repair_done ~rt ~dst = 
  rt.(dst).repairing <- false

let newadv ~rt ~dst ~sn ~hc ~nh = 
  assert (hc >= 0);
  let rtab_changed = 
    
    match rt.(dst).seqno with 
    | None -> (* first time we get entry for this target *)
	rt.(dst).seqno <- Some sn;
	rt.(dst).hopcount <- Some hc;
	rt.(dst).nexthop <- Some nh;
	validate ~rt ~dst;
	true;
    | Some oldseqno when (sn > oldseqno)  -> (* advertisement has fresher seqno *)
	rt.(dst).seqno <- Some sn;
	rt.(dst).hopcount <- Some hc;
	rt.(dst).nexthop <- Some nh;
	validate ~rt ~dst;
	true;
    | Some oldseqno when (sn = oldseqno)  -> (* same seqno, only keep if shorter route *)
	if hc < o2v rt.(dst).hopcount then (
	  rt.(dst).seqno <- Some sn;
	  rt.(dst).hopcount <- Some hc;
	  rt.(dst).nexthop <- Some nh;
	validate ~rt ~dst;
	  true;
	) else false;
    | Some oldseqno when (sn < oldseqno)  -> false; (* advertisement has stale seqno, discard *)
    | _ -> raise (Misc.Impossible_Case "rtab.ml") 
  in
  rtab_changed


let newadv_ignorehops ~rt ~dst ~sn ~hc ~nh = 
  
  let rtab_changed = 
    
    match rt.(dst).seqno with 

    | None -> (* first time we get entry for this target *)
	rt.(dst).seqno <- Some sn;
	rt.(dst).hopcount <- Some hc;
	rt.(dst).nexthop <- Some nh;
	validate ~rt ~dst;
	true;
    | Some oldseqno when (sn >= oldseqno)  -> 
	rt.(dst).seqno <- Some sn;
	rt.(dst).hopcount <- Some hc;
	rt.(dst).nexthop <- Some nh;
	validate ~rt ~dst;
	true;
    | Some oldseqno when (sn < oldseqno)  -> false; 
    | _ -> raise (Misc.Impossible_Case "rtab.ml") 
  in
  rtab_changed
