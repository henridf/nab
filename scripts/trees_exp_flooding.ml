let durations = [1200]
let sinks = [8]
let runs = [1; 2; 3; 4; 5]
let difftypes = ["voronoi"; "opp"; "ess"]
let mactypes = ["nullmac"]
let floodints = [30.; 60.; 90.; 120.; 180.; 240.]
let tmpfile = "/tmp/out-flooding.txt"
let resfile = "./out-flooding.txt"
  
let do_one_run ~duration ~sinks ~difftype ~mactype ~nth_top ~floodint = 
  let cmd = Printf.sprintf
    "\n\t/home/henri/work/caml/bin/mws -nsinks %d -duration %d -difftype %s -mactype %s -nth_top %d  -floodint %f>> %s" sinks
    duration  difftype mactype nth_top floodint tmpfile in
  print_endline cmd; flush stdout;
  ignore (Sys.command cmd)


let _ = 



(*
  begin try Sys.remove tmpfile with _ -> () end;

  List.iter 
    (fun duration -> 
      List.iter 
      (fun sinks -> 
	List.iter 
	(fun difftype ->
	  List.iter 
	  (fun run ->
	    List.iter 
	  (fun floodint ->
	    List.iter 
	    (fun mactype ->
	      print_endline "Doing one run";
	      do_one_run ~duration ~sinks ~difftype ~mactype ~nth_top:run ~floodint
	    ) mactypes
	    ) floodints
	  ) runs
	) difftypes
      ) sinks
    ) durations;



  ignore (Sys.command (Printf.sprintf "cat %s | dbstripextraheaders > %s" tmpfile resfile));
*)

(*
cat out.txt | dbcoladd data interest total > out.tmp.txt
*)


  let chop = Filename.chop_extension in

  ignore (Sys.command (Printf.sprintf 
    "cat %s | dbrow '_difftype =~ /voronoi/' | dbmultistats floodint interest | grep -v \"# \" | sort -n  > vor-flood-int.txt"
    resfile));
  ignore (Sys.command (Printf.sprintf 
    "cat %s | dbrow '_difftype =~ /voronoi/' | dbmultistats floodint data | grep -v \"# \" | sort -n  > vor-flood-dat.txt"
    resfile));
  ignore (Sys.command (Printf.sprintf 
    "cat %s | dbrow '_difftype =~ /voronoi/' | dbmultistats floodint total | grep -v \"# \" | sort -n  > vor-flood-tot.txt"
    resfile));
  
  ignore (Sys.command (Printf.sprintf 
    "cat %s | dbrow '_difftype =~ /opp/' | dbmultistats floodint interest | grep -v \"# \" | sort -n  > opp-flood-int.txt"
    resfile));
  ignore (Sys.command (Printf.sprintf 
    "cat %s | dbrow '_difftype =~ /opp/' | dbmultistats floodint data | grep -v \"# \" | sort -n  > opp-flood-dat.txt"
    resfile));
  ignore (Sys.command (Printf.sprintf 
    "cat %s | dbrow '_difftype =~ /opp/' | dbmultistats floodint total | grep -v \"# \" | sort -n  > opp-flood-tot.txt"
    resfile));
  
  
  ignore (Sys.command (Printf.sprintf 
    "cat %s | dbrow '_difftype =~ /ess/' | dbmultistats floodint interest | grep -v \"# \" | sort -n  > ess-flood-int.txt"
    resfile));
  ignore (Sys.command (Printf.sprintf 
    "cat %s | dbrow '_difftype =~ /ess/' | dbmultistats floodint data | grep -v \"# \" | sort -n  > ess-flood-dat.txt"
    resfile));
  ignore (Sys.command (Printf.sprintf 
    "cat %s | dbrow '_difftype =~ /ess/' | dbmultistats floodint total | grep -v \"# \" | sort -n  > ess-flood-tot.txt"
    resfile));
  





