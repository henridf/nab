open Printf
open Misc
open Coord

let init_sched() = Gsched.set_sched (new Sched.schedList)
let init_world() = Gworld.set_world (new Crworld.crworld (Param.get Params.nodes))

let make_bler_nodes ?ntargets () = (
  let nt = 
    match ntargets with
      | None -> (Param.get Params.nodes)
      | Some n -> n
  in
  Nodes.set_nodes [||]; (* in case this is being called a second time in the same script *)  
  Nodes.set_nodes
  (Array.init (Param.get Params.nodes)
    (fun i -> 
      (new Simplenode.simplenode 
	~pos_init:((Gworld.world())#random_pos) 
	~id:i
	~ntargets:nt)));

  (* create bler_agents, who will register with their owners *)
  Nodes.iter (fun n -> ignore (new Magic_bler_agent.magic_bler_agent n));

  (* set up initial node position in internal structures of world object *)
  Nodes.iter (fun n -> (Gworld.world())#update_pos ~node:n ~oldpos_opt:None);
  assert ((Gworld.world())#neighbors_consistent);
)

let cleanup, add_cleanup_hook = 
  let cleanup_functions = ref [] in
  let cleanup_ () = List.iter (fun c -> c()) !cleanup_functions in
  let add_cleanup_hook_ f = cleanup_functions := !cleanup_functions @ [f]
  in (cleanup_, add_cleanup_hook_)

let set_tracefile f = (
  Trace.set_trace_chan (open_out_bin f);
  add_cleanup_hook (fun () -> Trace.close_trace_chan())
)

let draw_nodes () = 
  Ler_graphics.draw_nodes (Nodes.map (fun n -> (Gworld.world())#project_2d
    n#pos))
    
let draw_node ~nid = 
  Ler_graphics.draw_nodes [|((Gworld.world())#project_2d (Nodes.node(nid))#pos)|]
    
let label_node ~node = (
  Ler_graphics.label_node 
  ((Gworld.world())#project_2d node#pos)
  (Printf.sprintf "%d" node#id)
)

let label_nodes() = Nodes.iter (fun node -> label_node node)

let redraw_and_label_nodes() = (
  Ler_graphics.clear_gfx();
  draw_nodes();
  label_nodes()
)

let proportion_met_nodes ~targets  = 
  let total_encounters = 
    Nodes.fold (fun n encs -> (n#db#num_encounters) + encs) 0
  in
  (i2f total_encounters) /. (i2f ((Param.get Params.nodes) * targets))

  
let avg_neighbors_per_node() = 
  let total_neighbors = 
    Nodes.fold (fun n total -> (List.length n#neighbors) + total) 0 
  in
  (i2f total_neighbors) /. (i2f (Param.get Params.nodes))

let wait_for_any_keypress() = (
  Printf.printf "Press any key to continue...\n" ; 
  flush stdout;
  ignore (Graphics.read_key())
)

let make_app_packet ~srcid ~dstid = 
  Packet.make_app_pkt ~l3hdr:(Packet.make_l3hdr ~srcid:srcid ~dstid:dstid  ())

let make_bler_packet ~srcid ~dstid = 
  Packet.make_bler_pkt 
    ~l3hdr:(Packet.make_l3hdr ~srcid:srcid ~dstid:dstid  ())
    ~blerpld:(Packet.ANCH_REQ (dstid, max_float)) (*dst, current enc. age*)
  
let do_one_route ~src ~dst = (
(*
  let l3pkt = Packet.BLER_PKT (make_bler_packet ~srcid:src ~dstid:dst) in
  let l2pkt = Packet.make_l2pkt ~srcid:src ~l2_dst:(Packet.L2_DST dst)
    ~l3pkt:l3pkt in
  
  let routeref = ref (Route.create()) in
  let bler_mhook = Magic_bler_agent.magic_bler_route_mhook routeref in
  Nodes.iter (fun n -> n#add_mhook bler_mhook);
  
  let pkt_reception() = (Nodes.node(src))#mac_recv_pkt l2pkt in
  (Gsched.sched())#sched_at ~handler:pkt_reception ~t:(Sched.ASAP);
  (Gsched.sched())#run();
  *)

  let routeref = ref (Route.create()) in
  let bler_mhook = Magic_bler_agent.magic_bler_route_mhook routeref in
  Nodes.iter (fun n -> n#add_mhook bler_mhook);
  let pkt_reception() = (Nodes.node(src))#originate_app_pkt (Nodes.node(dst)) in
  (Gsched.sched())#sched_at ~handler:pkt_reception ~t:(Sched.ASAP);
  (Gsched.sched())#run();
  
  assert (Route.route_valid !routeref ~src:src ~dst:dst);
  Route.i2c !routeref
)

let hop_col_color ~hop ~routelength = (
  [| Graphics.black; Graphics.red;
  Graphics.blue |].(hop mod 3)
)

let gui_one_route() = (

  draw_nodes();

  let src = Ler_graphics.mouse_choose_node (Gworld.world())#get_node_at "choose a source" 
  and dst =  Ler_graphics.mouse_choose_node (Gworld.world())#get_node_at "choose a dest" 
  in
  Printf.printf "src is %d, enc_age %f\n" src (((Nodes.node(src))#db)#encounter_age dst);
  Printf.printf "dst is %d\n" dst;
  
  let route_coords = do_one_route   ~src:src ~dst:dst in

  Printf.printf "Route has length %f, anchor cost %f\n"
    (Route.eucl_length (Gworld.world())#dist_coords route_coords) (Route.anchor_cost route_coords);
  
  printf "%s\n" (Route.sprint route_coords);
  
  let route_normalized = 
    (List.map 
      (fun x -> {
	Route.hop = (Gworld.world())#project_2d x.Route.hop;
	Route.anchor = (Gworld.world())#project_2d x.Route.anchor;
	Route.anchor_age = x.Route.anchor_age;
	Route.searchcost = (
	  Gworld.world())#scale_unit ((x.Route.searchcost) ** 0.5)}
      )
      route_coords)
  in
  Ler_graphics.draw_route ~color:hop_col_color ~route:route_normalized;
  route_coords;
)

let gui_route_color_ages ~route ~targetnid = (
  
  let rec whichhop route i age = 
    match route with
      | h :: r -> if age > h.Route.anchor_age then i else whichhop r (i+1) age
      | [] -> raise (Impossible_Case "since last hop has age 0, should have stopped there")
  in
	  
  Nodes.iter 
    (fun n -> 
      let hop = whichhop route 0 ((n#db)#encounter_age ~nid:targetnid) in
      Printf.printf "hop is %d\n" hop;
      Graphics.set_color (Ler_graphics.hop_col_color ~hop:hop ~routelength:(List.length route));
      draw_node ~nid:(n#id)
    )
)    



  
let gui_color_ages ~targetnid = (
  let max_age = Common.get_time() in
  let step = Common.get_time() /. 2.0 in
  let color_of_age age = 
    Printf.printf "Age is %f, age/step is %f\n" age (age/.step);
    [|Graphics.red; Graphics.green; Graphics.blue;
    |].(f2i (age /. step))
  in

  Nodes.iter 
    (fun n -> 
      match (n#db)#encounter_age targetnid with
	| age when (age = max_float) -> ()
	| age -> (
	    Graphics.set_color (color_of_age age);
	    draw_node ~nid:(n#id)
	  )
    )
)



let move_nodes ~f ~percent ~targets= (
  let iterations = ((Param.get Params.nodes) / 10) in 
  while ((proportion_met_nodes targets) < percent) do 
    repeat iterations (fun x -> 
      Nodes.iter (fun n -> f ~node:n); 
      Common.set_time (Common.get_time() +. 1.0);
    );
    Log.log#log_notice 
      (Printf.sprintf "prop_met_nodes %f\n"
	(proportion_met_nodes ~targets:targets));
  done;
)
  













let print_header () = (
  Printf.printf "\n";
  Printf.printf "--------------------------------------- \n";
  Printf.printf "    mws  multihop wireless simulator    \n";
  Printf.printf "--------------------------------------- \n\n";
  Printf.printf "Simulation Parameters:\n";
  Printf.printf "\tNumber of nodes:\t\t %d\n" (Param.get Params.nodes);
  flush stdout;
)


let finish () = (
  Printf.printf ".. done\n";
  flush stdout;
  exit 0;
)


