



val startmws : 
  mws_tick:float -> 
  rt_tick_ms:int ->
  display_cb:(unit -> unit) ->
  unit 
  (** Start running according to the following loop:

    - run the mws event loop for [mws_tick] simulator seconds, 
    - call [display_cb] (user-provided func to e.g. update display)

    The loop is invoked every [rt_tick_ms] millisecs. If [real_tick] is
    less than the time it takes to run mws, then there will be backlog.
    Idempotent.
  *)
    
val stop :  unit -> unit
  (** Stop running. Idempotent. *)
