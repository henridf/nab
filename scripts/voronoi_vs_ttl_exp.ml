(*
 *
 *  Fake - a network simulator
 *  Henri Dubois-Ferriere, LCA/LCAV, EPFL
 * 
 *  Copyright (C) 2004 Laboratory of Audiovisual Communications (LCAV), and
 *  Laboratory for Computer Communications and Applications (LCA), 
 *  Ecole Polytechnique Federale de Lausanne (EPFL),
 *  CH-1015 Lausanne, Switzerland
 *
 *  This file is part of fake. Fake is free software; you can redistribute it 
 *  and/or modify it under the terms of the GNU General Public License as 
 *  published by the Free Software Foundation; either version 2 of the License,
 *  or (at your option) any later version. 
 *
 *  Fake is distributed in the hope that it will be useful, but WITHOUT ANY
 *  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 *  FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 *  details (enclosed in the file GPL). 
 *
 *)

(* $Id$ *)



(* 
  1. build executable as 
cd ~/work/caml/
make OPT=y  SCRIPT=voronoi_vs_ttl.ml bin/mwsvor

   2. run test as 
ocaml scripts/voronoi_vs_ttl_exp.ml 

   3.  cp ~/tmp/out.txt /tmp/out.txt
   cat /tmp/out.txt | dbstripextraheaders > out.txt
   [henri@ffnet: ~/work/tex/voronoi] cat out.txt | dbrow '_difftype =~ /vor/'
   | dbrow '_nodes =~ /500/' | dbmultistats nsinks interest | sort -n >
   vor-ttl-500.txt
   [henri@ffnet: ~/work/tex/voronoi] cat out.txt | dbrow '_difftype =~ /opp/'
   | dbrow '_nodes =~ /500/' | dbmultistats nsinks interest | sort -n >
   opp-ttl-500.txt
   [henri@ffnet: ~/work/tex/voronoi] cat out.txt | dbrow '_difftype =~ /opp/'
   | dbrow '_nodes =~ /250/' | dbmultistats nsinks interest | sort -n >
   opp-ttl-250.txt
   [henri@ffnet: ~/work/tex/voronoi] cat out.txt | dbrow '_difftype =~ /vor/'
   | dbrow '_nodes =~ /250/' | dbmultistats nsinks interest | sort -n >
   vor-ttl-250.txt

   4. use  ~/censwork/exp/results/makeplots-vor-ttl.gp to plot
*)

let nodes = [250; 500]
let max_sinks = 25
let nodespersink = 20 
let max_nodes = 40
let nruns = 60
let tmpfile = "/home/henridf/tmp/out.txt"
let resfile = "/home/henridf/out.txt"
  
let do_one_run ~nodes ~sinks = 
  let cmd = Printf.sprintf
    "\n\t/home/henridf/work/caml/bin/mwsvor -nodes %d -nsinks %d -nruns %d >> %s" 
    nodes sinks  nruns tmpfile in
  print_endline cmd; flush stdout;
  ignore (Sys.command cmd)


let _ = 
  for i = 0 to List.length nodes - 1 do
    
    for j = 1 to max_sinks do 
      do_one_run  ~nodes:(List.nth nodes i) ~sinks:j
    done
  done;

(*
  begin try Sys.remove tmpfile with _ -> () end;

  let nodes = ref 0 in
  let sinks = ref 0 in
  while !nodes < max_nodes do
    incr sinks;
    nodes := !nodes + nodespersink;
    do_one_run  ~nodes:!nodes ~sinks:!sinks
  done
*)



  ignore (Sys.command (Printf.sprintf "cat %s | dbstripextraheaders > %s" tmpfile resfile));





  let chop = Filename.chop_extension in
  
  (*
    cat out.txt | dbcoladd data interest total > out.tmp.txt
  *)

  ignore (Sys.command (Printf.sprintf 
    "cat %s | dbrow '_difftype =~ /vor/' | dbmultistats nsinks interest | grep -v \"# \" | sort -n  > vor-int.txt"
    resfile));
  
  ignore (Sys.command (Printf.sprintf 
    "cat %s | dbrow '_difftype =~ /opp/' | dbmultistats nsinks interest | grep -v \"# \" | sort -n  > ttl-int.txt"
    resfile));
  





