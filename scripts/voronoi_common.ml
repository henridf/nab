module Pms = Params
module Pm = Param

module Config = 
struct
  let nruns = 
    Pm.intcreate
      ~name:"nruns" 
       ~default:1
      ~cmdline:true
      ~doc:"The number of runs" ()

  let nsinks = 
    Pm.intcreate
      ~name:"nsinks"
      ~default:1
      ~cmdline:true
      ~doc:"Number of sinks" ()
      ~checker:(fun n -> Diff_agent.nsinks_ := Some n)

  let floodint = 
    Pm.floatcreate
      ~name:"floodint"
      ~default:60.
      ~cmdline:true
      ~doc:"Mean interval between floods" ()
      ~checker:(fun n -> Diff_agent.mean_interest_interval := n)

  let duration = 
    Pm.floatcreate
      ~name:"duration"
      ~default:60.
      ~cmdline:true
      ~doc:"Simulation duration" ()
      
  let difftype = 
    Pm.stringcreate ~name:"difftype" ~default:"Voronoi" ~cmdline:true
      ~doc:"Diffusion algorithm" 
      ~checker:(fun s -> Diff_agent.strset_difftype s) ()

end
