(*
 * 
 * NAB - Network in a Box Henri Dubois-Ferriere, LCA/LCAV, EPFL
 * 
 * Copyright (C) 2004 Laboratory of Audiovisual Communications (LCAV), and
 * Laboratory for Computer Communications and Applications (LCA), Ecole
 * Polytechnique Federale de Lausanne (EPFL), CH-1015 Lausanne, Switzerland
 * 
 * This file is part of NAB. NAB is free software; you can redistribute it
 * and/or modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of the License,
 * or (at your option) any later version.
 * 
 * NAB is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details (enclosed in the file GPL).
 * 
 *)

(* $Id$ *)

type trafficmatrix = HOTSPOT  | BIDIR | UNIDIR

let sp = Printf.sprintf

let hotspot_dst() = 0
  (* this could currently be exposed as a val and not a function, but we make
     it a function to force people to use it as such in case for some reason
     it becomes stg that can vary based on the configuration. *)

module TParams = 
struct

  let string_of_tmat tmat = 
    match tmat with 
      | HOTSPOT -> "hotspot"
      | BIDIR  -> "bidirectional"
      | UNIDIR -> "unidirectional"
(*      | HOTSPOT_BIDIR -> "hotspot_bidir"*)

  let tmat_of_string tmat = 
    match (String.lowercase tmat) with 
      | "hotspot" | "hot" -> HOTSPOT
      | "bidirectional" | "bi" | "bidir" -> BIDIR  
      | "unidirectional" | "uni" | "unidir" -> UNIDIR  
(*      | "hotspot_bi" | "hotspot_bidir" -> HOTSPOT_BIDIR*)

      | _ -> raise (Failure "Invalid format for traffic type")

  let tmat = 
    Param.create  ~name:"tmat" ~default:HOTSPOT
      ~cmdline:true
      ~doc:"Traffic Type" 
      ~reader:tmat_of_string
      ~printer:string_of_tmat
      ()

  let sources = 
    Param.intcreate  ~name:"sources" ~default:50
      ~cmdline:true
      ~doc:"Number of sources"  ()
      
  let rate = 
    Param.floatcreate ~name:"rate" ~default:0.03
      ~cmdline:true
      ~doc:"Orig rate [pkt/s]"  ()

  let pkts_orig = 
    Param.intcreate ~name:"pkts_orig" 
      ~cmdline:true
      ~default:50
      ~doc:"Packets originated"  ()

    
end

let is_source nid = 
  let n_nodes = (Param.get Params.nodes) in
  if nid < 0 || nid >= (Param.get Params.nodes) then 
    failwith (sp "Invalid node id: %d" nid);
  nid >= n_nodes - (Param.get TParams.sources) 


let dests_of_source src = 
  (* 2nd test is in case we have n nodes, n sources, then the n/2'th
     node would have itself as destination*)
  match Param.get TParams.tmat with
    | HOTSPOT -> 
	if is_source src && src <> hotspot_dst() 
	then  Some [hotspot_dst()] else  None
    | UNIDIR -> 
	let dst = ((Param.get Params.nodes)  - 1 - src) in
	if is_source src && src <> dst 
	then Some [dst]	else None
    | BIDIR -> 
	let dst = 
	  2 * (Param.get Params.nodes) - src - (Param.get TParams.sources) - 1
	in
	if is_source src && src <> dst 
	then Some [dst] else None

let src_dst_pairs() = 
  let n_nodes = Param.get Params.nodes in
  let rec fold n pairs = 
    if n = n_nodes then pairs else
      let newpairs = 
	let dsts = (dests_of_source n) in
	if dsts <> None then
	  (n, Opt.get dsts)::pairs
	else pairs in
      fold (n+1) newpairs
  in
  fold 0 []


let all_sources()  = 
  let n_nodes = Param.get Params.nodes in
  let rec fold n sources = 
    if n = n_nodes then sources else
      if is_source n then fold (n+1) sources
      else fold (n+1) sources
  in fold 0 []

let all_destinations() = 
  let pairs = src_dst_pairs() in
  let all_dests = 
    List.fold_left (fun dsts (src, dstlist) -> dsts@dstlist) [] pairs
  in
  Misc.list_unique_elements all_dests  

let sprint_matrix() = 
  let b = Buffer.create 64 in  
  Buffer.add_string b 
    (sp "Traffic Matrix: %s\n" (Param.as_string TParams.tmat));
  Buffer.add_string b (sp "%d sources\n" (Param.get TParams.sources));
  let pairs = src_dst_pairs() in
  List.iter (fun (src,dsts) -> 
    List.iter (fun dst ->
      Buffer.add_string b (Misc.padto (string_of_int src) 5);
      Buffer.add_string b " -> ";
      Buffer.add_string b ((string_of_int dst)^"\n");
    ) dsts
  ) pairs;
  
  Buffer.contents b
      
let install_tsources () = 

  let sources = (Param.get TParams.sources) in
  let rndstream = Random.State.copy (Random.get_state()) in
  
  let num_pkts = (Param.get TParams.pkts_orig) / sources in

  if num_pkts < 1 then 
    failwith "Too few packets to originate!!!";

  let tsource () = 
    (Tsource.make_cbr ~num_pkts ~rate:(Param.get TParams.rate) ()) in

  let pairs = src_dst_pairs() in
  List.iter (fun (src,dsts) -> 
    List.iter (fun dst ->
      let pkt_reception() = 	      
	(Nodes.node src)#set_trafficsource ~gen:(tsource()) ~dst in
      (* start sessions jittered out over an interval equivalent to the
	 inter-packet interval *)
      let t = Random.State.float rndstream  (1. /. (Param.get TParams.rate)) in
      (Sched.s())#sched_in ~f:pkt_reception ~t;
    ) dsts
  ) pairs 


let clear_tsources () = 
  Nodes.iter (fun n -> n#clear_trafficsources())
