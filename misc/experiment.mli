(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

(* open:
   - maybe the param_value (the one which we vary btw runs) can be directly
   represented as a float or int rather than string?
   - if we want to represent stdev bars, we should probably associate them
   within the same plotline as the means, rather than having separate
   plotlines.
   - this would mean that the function passed to plotline should be allowed to
   return a 'stat' rather than a scalar. look at data.ml's represenation of
   statistic, if not too ugly...

*)

(** 
  Support for running experiments, representing results of multiple runs,
  their respective configs, etc.

  The basic assumption here is that we make a number of simulations with one
  parameter at a time varying (say node speed). This basic building block can
  then of course be reused to effectively vary more than one parameter (like
  do a number of simulations at varying speed with AODV, then with GREP).

  Vocabulary / Concepts:


  A 'plotvals' represents (one or more) statistics (e.g., avg, stdev) for an
  expressions (e.g., on the results of the series total_pkts or
  total_pkts/data_received). Maybe instead of having 'get_plotline' we would
  have a 'get_plotval' 
  

  A 'page' is an html page containing one or more plots.

*)


type runconfig_t = string
    (** A 'run' is one simulation run, with a certain 'runconfig'
      configuration.  A 'runconfig' is a string such as 
      "-nodes 200  -sources 10  -tmat Uni -rate 4 -speed 12 -pktssend 10" *)
    

type seriesconfig_t = {runconfig:runconfig_t; repeats:int}
    (** A 'seriesconfig' is a runconfig plus an int specifying the # of
      repeats.*)
    
type 'a seriesresults_t = {variable_par:string; parval:string; seriesres:'a array}
    (** A 'series' is a run repeated n times (e.g. for averaging out), which
      produces a vector (array or list) of result. *)


type experimentconfig_t = 
    {series:seriesconfig_t; variable_param:string; param_values:string array}
    (** A 'experimentconfig' is an seriesconfig plus the parameter to vary.*)

type 'a experimentresults_t = 
    {globalconfig: runconfig_t; expres: 'a seriesresults_t array}
    (** A 'experiment' is a number of series performed over one varying
      parameter. A experiment therefore produces a matrix, one index indicating the
      value of the varying parameter and the other index being the different
      results.*)

val doexperiment : experimentconfig_t -> f_do_one_run:(unit -> 'a) -> 'a experimentresults_t
  (** should have created all Param objects *before* calling this *)


val print_exp : 'a experimentresults_t -> ('a -> string) -> unit
  

type plotline_t = {linename:string; linedata:(float*float) array}
    (**  A 'plotline' is an array of (x, y) pairs where x is the varying
      parameter of the experiment and y is a result. *)

val print_plotline : plotline_t -> unit

val get_plotline : string -> 'a experimentresults_t -> ('a -> float) -> plotline_t

type plot_t = {plotname:string; plotlines:plotline_t list}
  (** A 'plot' is one or more plotlines. For now all plotlines in a plot must be
  over the same parameter and have the same Y axis.*)

val make_plot : plotline_t list -> string -> plot_t

val draw_plot : plot_t -> string -> unit



