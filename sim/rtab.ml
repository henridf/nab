(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

open Misc 

type rtab_entry_t = {
  mutable seqno: int option;
  mutable nexthop: Common.nodeid_t option;
  mutable hopcount: int option
}


type rtab_t = rtab_entry_t array

let create ~size = Array.init size (fun n -> {seqno=None; nexthop=None; hopcount=None})
let seqno ~rt ~dst = rt.(dst).seqno
let nexthop ~rt ~dst = rt.(dst).nexthop
let hopcount ~rt ~dst = rt.(dst).hopcount

let invalidate ~rt ~dst = (
  rt.(dst).hopcount <- Some max_int;
  rt.(dst).nexthop <- None
)

let invalid ~rt ~dst = (
  rt.(dst).hopcount = Some max_int;
)

let newadv ~rt ~dst ~rtent = 
  let newseqno = o2v rtent.seqno in
  let newhopcount = o2v rtent.hopcount in

  assert (newhopcount >= 0);
  let rtab_changed = 
    
    match rt.(dst).seqno with 

    | None -> (* first time we get entry for this target *)
	rt.(dst).seqno <- rtent.seqno;
	rt.(dst).hopcount <- rtent.hopcount;
	rt.(dst).nexthop <- rtent.nexthop;
	true;
    | Some oldseqno when (newseqno > oldseqno)  -> (* advertisement has fresher seqno *)
	rt.(dst).seqno <- rtent.seqno;
	rt.(dst).hopcount <- rtent.hopcount;
	rt.(dst).nexthop <- rtent.nexthop;
	true;
    | Some oldseqno when (newseqno = oldseqno)  -> (* same seqno, only keep if shorter route *)
	if newhopcount < o2v rt.(dst).hopcount then (
	  rt.(dst).seqno <- rtent.seqno;
	  rt.(dst).hopcount <- rtent.hopcount;
	  rt.(dst).nexthop <- rtent.nexthop;
	  true;
	) else false;
    | Some oldseqno when (newseqno < oldseqno)  -> false; (* advertisement has stale seqno, discard *)
    | _ -> raise (Misc.Impossible_Case "rtab.ml") 
  in
  rtab_changed


let newadv_ignorehops ~rt ~dst ~rtent = 
  let newseqno = o2v rtent.seqno in
  let newhopcount = o2v rtent.hopcount in

  let rtab_changed = 
    
    match rt.(dst).seqno with 

    | None -> (* first time we get entry for this target *)
	rt.(dst).seqno <- rtent.seqno;
	rt.(dst).hopcount <- rtent.hopcount;
	rt.(dst).nexthop <- rtent.nexthop;
	true;
    | Some oldseqno when (newseqno >= oldseqno)  -> 
	rt.(dst).seqno <- rtent.seqno;
	rt.(dst).hopcount <- rtent.hopcount;
	rt.(dst).nexthop <- rtent.nexthop;
	true;
    | Some oldseqno when (newseqno < oldseqno)  -> false; 
    | _ -> raise (Misc.Impossible_Case "rtab.ml") 
  in
  rtab_changed
