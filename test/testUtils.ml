(** bits and pieces of a rudimentary test infrastructure **)

exception Test_Failure of string;;
type debug_level_t = DEBUG_LOG_DEBUG 
		     | DEBUG_LOG_INFO 
		     | DEBUG_LOG_NOTICE 
		     | DEBUG_LOG_WARNING 
		     | DEBUG_LOG_ERR 
		     | DEBUG_LOG_ALWAYS;;

let debug_mappings = [| DEBUG_LOG_DEBUG; DEBUG_LOG_INFO; DEBUG_LOG_NOTICE; DEBUG_LOG_WARNING; DEBUG_LOG_ERR; DEBUG_LOG_ALWAYS |];;

let debug_level = ref DEBUG_LOG_NOTICE;;
let set_debug_level level = debug_level := level;;

(** Logging **)
let dprintf level fmt = 
  if debug_mappings.(level) >= !debug_level then Printf.fprintf stdout fmt;;



let run_test name test = 
  try 
    test () ; 
    (Printf.printf "Testcase %s : completed ok\n" name);
  with 
      Test_Failure errmsg -> Printf.printf "Testcase %s : failed\n %s" name errmsg;;


  
