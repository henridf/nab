let nodes = [500]
let max_sinks = 25
let nodespersink = 20 
let max_nodes = 40
let nruns = 10
let tmpfile = "/tmp/out.txt"
let resfile = "./out.txt"
  
let do_one_run ~nodes ~sinks = 
  let cmd = Printf.sprintf
    "\n\t/home/henri/work/caml/bin/mwsvor -nodes %d -nsinks %d -nruns %d >> %s" 
    nodes sinks  nruns tmpfile in
  print_endline cmd; flush stdout;
  ignore (Sys.command cmd)


let _ = 
  for i = 0 to List.length nodes - 1 do
    
    for j = 1 to max_sinks do 
      do_one_run  ~nodes:(List.nth nodes i) ~sinks:j
    done
done

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


(*
  ignore (Sys.command (Printf.sprintf "cat %s | dbstripextraheaders > %s" tmpfile resfile));



  let chop = Filename.chop_extension in
  
  (*
    cat out.txt | dbcoladd data interest total > out.tmp.txt
  *)

  ignore (Sys.command (Printf.sprintf 
    "cat %s | dbrow '_difftype =~ /voronoi/' | dbmultistats nsinks interest | grep -v \"# \" | sort -n  > vor-int.txt"
    resfile));
  ignore (Sys.command (Printf.sprintf 
    "cat %s | dbrow '_difftype =~ /voronoi/' | dbmultistats nsinks data | grep -v \"# \" | sort -n  > vor-dat.txt"
    resfile));
  ignore (Sys.command (Printf.sprintf 
    "cat %s | dbrow '_difftype =~ /voronoi/' | dbmultistats nsinks total | grep -v \"# \" | sort -n  > vor-tot.txt"
    resfile));
  
  ignore (Sys.command (Printf.sprintf 
    "cat %s | dbrow '_difftype =~ /opp/' | dbmultistats nsinks interest | grep -v \"# \" | sort -n  > opp-int.txt"
    resfile));
  ignore (Sys.command (Printf.sprintf 
    "cat %s | dbrow '_difftype =~ /opp/' | dbmultistats nsinks data | grep -v \"# \" | sort -n  > opp-dat.txt"
    resfile));
  ignore (Sys.command (Printf.sprintf 
    "cat %s | dbrow '_difftype =~ /opp/' | dbmultistats nsinks total | grep -v \"# \" | sort -n  > opp-tot.txt"
    resfile));
  
  
  ignore (Sys.command (Printf.sprintf 
    "cat %s | dbrow '_difftype =~ /ess/' | dbmultistats nsinks interest | grep -v \"# \" | sort -n  > ess-int.txt"
    resfile));
  ignore (Sys.command (Printf.sprintf 
    "cat %s | dbrow '_difftype =~ /ess/' | dbmultistats nsinks data | grep -v \"# \" | sort -n  > ess-dat.txt"
    resfile));
  ignore (Sys.command (Printf.sprintf 
    "cat %s | dbrow '_difftype =~ /ess/' | dbmultistats nsinks total | grep -v \"# \" | sort -n  > ess-tot.txt"
    resfile));
  




*)
