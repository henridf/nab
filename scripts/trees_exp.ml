let durations = [1200]
let sinks = [1;2;3;4;5;6;7;8;9;10]
let runs = [1; 2; 3; 4; 5]
let difftypes = ["voronoi"; "opp"; "ess"]
let mactypes = ["nullmac", "contmac"]
let tmpfile = "/tmp/out.txt"
let resfile = "./out.txt"
  
let do_one_run ~duration ~sinks ~difftype ~mactype ~nth_top = 
  let cmd = Printf.sprintf
    "\n\t/home/henri/work/caml/bin/mws -nsinks %d -duration %d -difftype %s -mactype %s -nth_top %d >> %s" sinks
    duration  difftype mactype nth_top tmpfile in
  print_endline cmd; flush stdout;
  ignore (Sys.command cmd)


let _ = 


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
	    (fun mactype ->
	      print_endline "Doing one run";
	      do_one_run ~duration ~sinks ~difftype ~mactype ~nth_top:run
	    ) mactypes
	  ) runs
	) difftypes
      ) sinks
    ) durations;



  ignore (Sys.command (Printf.sprintf "cat %s | dbstripextraheaders > %s" tmpfile resfile));


(*
  let chop = Filename.chop_extension in

  ignore (Sys.command (Printf.sprintf 
    "cat %s | dbrow '_difftype =~ /voronoi/' | dbmultistats nsinks interest | sort -n  > vor-int.txt"
    resfile));
  ignore (Sys.command (Printf.sprintf 
    "cat %s | dbrow '_difftype =~ /voronoi/' | dbmultistats nsinks data | sort -n  > vor-dat.txt"
    resfile));
  
  ignore (Sys.command (Printf.sprintf 
    "cat %s | dbrow '_difftype =~ /opp/' | dbmultistats nsinks interest | sort -n  > opp-int.txt"
    resfile));
  ignore (Sys.command (Printf.sprintf 
    "cat %s | dbrow '_difftype =~ /opp/' | dbmultistats nsinks data | sort -n  > opp-dat.txt"
    resfile));
  
  
  ignore (Sys.command (Printf.sprintf 
    "cat %s | dbrow '_difftype =~ /ess/' | dbmultistats nsinks interest | sort -n  > ess-int.txt"
    resfile));
  ignore (Sys.command (Printf.sprintf 
    "cat %s | dbrow '_difftype =~ /ess/' | dbmultistats nsinks data | sort -n  > ess-dat.txt"
    resfile));
  





*)
