(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

open Misc 

type rtab_entry_t = {
  mutable seqno: int option;
  mutable nexthop: Common.nodeid_t option;
  mutable hops: int option
}


type rtab_t = rtab_entry_t  array

let create ~size = Array.create size {seqno=None; nexthop=None; hops=None}
let seqno ~rtab ~dst = rtab.(dst).seqno
let nexthop ~rtab ~dst = rtab.(dst).nexthop
let hops ~rtab ~dst = rtab.(dst).hops

let newadv ~rtab ~dst ~rtent = 
  let newseqno = o2v rtent.seqno in
  let newhops = o2v rtent.hops in

  let rtab_changed = 
    
  match rtab.(dst).seqno with 

    | None -> (* first time we get entry for this target *)
	rtab.(dst).seqno <- rtent.seqno;
	rtab.(dst).hops <- rtent.hops;
	rtab.(dst).nexthop <- rtent.nexthop;
	true;
    | Some oldseqno when (newseqno > oldseqno)  -> (* advertisement has fresher seqno *)
	rtab.(dst).seqno <- rtent.seqno;
	rtab.(dst).hops <- rtent.hops;
	rtab.(dst).nexthop <- rtent.nexthop;
	true;
    | Some oldseqno when (newseqno = oldseqno)  -> (* same seqno, only keep if shorter route *)
	if newhops < o2v rtab.(dst).hops then (
	  rtab.(dst).seqno <- rtent.seqno;
	  rtab.(dst).hops <- rtent.hops;
	  rtab.(dst).nexthop <- rtent.nexthop;
	  true;
	) else false;
    | Some oldseqno when (newseqno < oldseqno)  -> false; (* advertisement has stale seqno, discard *)
    | _ -> raise (Misc.Impossible_Case "rtab.ml") 
  in
  rtab_changed
