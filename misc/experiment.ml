(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)


(* 
   - Maybe html too..
*)





(* 
   What we will need:
   Way for describing parameters and config of series. This includes a
   way to a) name output files b) actually modify params c) represent the info
   in text (for graphs etc) d) specify defaults
   
   - Describing data produced by series
   - Specifying graphs to be produced
*)   
   
type runconfig_t = string

type seriesconfig_t = 
    {runconfig:runconfig_t; repeats:int}

type experimentconfig_t = 
    {series:seriesconfig_t; variable_param:string; param_values:string array}

type 'a seriesresults_t = {variable_par:string; parval:string; seriesres:'a array}
type 'a experimentresults_t = 
    {globalconfig: runconfig_t; expres: 'a seriesresults_t array}

type plotline_t = {linename:string; linedata:(float*float) array}
type plot_t = {plotname:string; plotlines:plotline_t list}


let doseries f repeats = 
  Array.init repeats (fun i -> f())
  

let doexperiment expconfig ~f_do_one_run  = (

  let s = Param.make_argspeclist () in
  let commonargs = Array.of_list (Str.split (Str.regexp "[ \t]+")
    expconfig.series.runconfig) in

  Myarg.parse_argv commonargs s (fun s -> ()) "You messed up!";
  
  let res = 
    Array.map (fun v -> 
      Myarg.parse_argv [|expconfig.variable_param; v|]  s (fun s -> ()) "You messed up!";
      {
	variable_par=expconfig.variable_param;
	parval=v; 
	seriesres= doseries f_do_one_run expconfig.series.repeats;
      }
    ) expconfig.param_values
  in
  {globalconfig=expconfig.series.runconfig; expres=res}
)

let print_exp expres f_printer = (
  Printf.printf "Common Configuration: %s\n" expres.globalconfig;
 Printf.printf "Varying Parameter: %s\n" expres.expres.(0).variable_par;
  Array.iter (fun sres -> 
    Printf.printf "%s :\n" sres.parval;
    Array.iter (fun r ->
      Printf.printf "\t%s\n" (f_printer r)
    ) sres.seriesres
  ) expres.expres;
  flush stdout
)
 
let print_plotline pline = (
  Printf.printf "Plotline name: %s\n" pline.linename;
  Printf.printf "x   y\n";
  Array.iter (fun (x, y) -> Printf.printf "%.2f %.2f\n" x y) pline.linedata;
  flush stdout
)

let get_plotline name expresults f = 
  let pldata = Array.map (fun sres ->
    let vector = Array.map (fun vals -> f vals) sres.seriesres in
    let mean = Data.avg_f vector in
    (float_of_string sres.parval, mean)
  ) expresults.expres
  in
  {linename=name; linedata=pldata}

let make_plot plotlinelist name = 
  {plotname=name; plotlines=plotlinelist}


let dump_plotline plotline = 
  let (ofname, ochan) = Filename.open_temp_file "mws" "" in
  print_endline ("Plotline dumped to"^ofname);
  Array.iter (fun (x, y) -> Printf.fprintf ochan "%.2f %.2f" x y) plotline.linedata;
  close_out ochan;
  ofname
    
let make_gnuplot_cmd outfile plotlinefiles = 
  "set terminal postscript eps 18\n"^
  (Printf.sprintf "set output \"%s\" \n" outfile)^
  (List.fold_left (fun  accum infile -> 
    (Printf.sprintf "plot \"%s\" using 1:2" infile)^accum)
    ""
    plotlinefiles)

let draw_plot plot filename = 
  let plotlinefiles = List.map 
    (fun plotline -> dump_plotline plotline) plot.plotlines in
  let gnuplot_cmd = make_gnuplot_cmd filename plotlinefiles in
  let (inchan, outchan) = Unix.open_process "gnuplot -" in
  output_string outchan gnuplot_cmd;
  flush outchan;
  close_out outchan;
  close_in inchan;


