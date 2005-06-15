let detach = Param.boolcreate 
  ~name:"detach" 
  ~doc:"Detach from terminal"
  ~cmdline:true
  ~default:false
  ~notpersist:true
  ()

let dumpfile = Param.stringcreate ~name:"dumpfile" 
  ~cmdline:true
  ~notpersist:true
  ~doc:"File to dump warmup state"
  ()

