(** bits and pieces of a rudimentary test infrastructure **)

let run_test name test = 
  try 
    test () ; 
    (Printf.printf "Testcase %s : completed ok\n" name);
  with 
      Test_Failure errmsg -> Printf.printf "Testcase %s : failed\n %s" name errmsg;;


