(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

open Misc 

type rtab_entry_t = {
  mutable seqno: int option;
  mutable nexthop: Common.nodeid_t option;
  mutable hopcount: int option
}


type rtab_t = rtab_entry_t  array

let create ~size = Array.create size {seqno=None; nexthop=None; hopcount=None}
let seqno ~rtab ~dst = rtab.(dst).seqno
let nexthop ~rtab ~dst = rtab.(dst).nexthop
let hopcount ~rtab ~dst = rtab.(dst).hopcount

let newadv ~rtab ~dst ~rtent = 
  let newseqno = o2v rtent.seqno in
  let newhopcount = o2v rtent.hopcount in

  let rtab_changed = 
    
  match rtab.(dst).seqno with 

    | None -> (* first time we get entry for this target *)
	rtab.(dst).seqno <- rtent.seqno;
	rtab.(dst).hopcount <- rtent.hopcount;
	rtab.(dst).nexthop <- rtent.nexthop;
	true;
    | Some oldseqno when (newseqno > oldseqno)  -> (* advertisement has fresher seqno *)
	rtab.(dst).seqno <- rtent.seqno;
	rtab.(dst).hopcount <- rtent.hopcount;
	rtab.(dst).nexthop <- rtent.nexthop;
	true;
    | Some oldseqno when (newseqno = oldseqno)  -> (* same seqno, only keep if shorter route *)
	if newhopcount < o2v rtab.(dst).hopcount then (
	  rtab.(dst).seqno <- rtent.seqno;
	  rtab.(dst).hopcount <- rtent.hopcount;
	  rtab.(dst).nexthop <- rtent.nexthop;
	  true;
	) else false;
    | Some oldseqno when (newseqno < oldseqno)  -> false; (* advertisement has stale seqno, discard *)
    | _ -> raise (Misc.Impossible_Case "rtab.ml") 
  in
  rtab_changed
