(*
 *
 *  NAB - Network in a Box
 *  Henri Dubois-Ferriere, LCA/LCAV, EPFL
 * 
 *  Copyright (C) 2004 Laboratory of Audiovisual Communications (LCAV), and
 *  Laboratory for Computer Communications and Applications (LCA), 
 *  Ecole Polytechnique Federale de Lausanne (EPFL),
 *  CH-1015 Lausanne, Switzerland
 *
 *  This file is part of NAB. NAB is free software; you can redistribute it 
 *  and/or modify it under the terms of the GNU General Public License as 
 *  published by the Free Software Foundation; either version 2 of the License,
 *  or (at your option) any later version. 
 *
 *  NAB is distributed in the hope that it will be useful, but WITHOUT ANY
 *  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 *  FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 *  details (enclosed in the file GPL). 
 *
 *)



let testno = ref 1
let failures = ref 0
  
let testsuite msg = 

  Printf.printf "\n\n";
  Printf.printf
    "--------------------------------------------------------------------\n";
  Printf.printf "Testsuite: %s\n\n" msg;
  flush stdout


let test msg pass = 
  Printf.printf "Testcase %d: %s... " !testno msg;
  flush stdout;
  incr testno;
  if pass then Printf.printf "Succeeded!\n"
  else 
    (Printf.printf "*** FAILED.\n"; incr failures);
  flush stdout
    

let msg m = print_endline ("[ "^m^" ]"); flush stdout


let testsuite_finished() = if !failures > 0 then begin
  print_endline "\n Eek: Some testcases failed.\n";
  exit 2;
end
