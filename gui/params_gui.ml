(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)


let background_of_string = function
  | "epfl" | "Epfl" | "EPFL" -> Epfl.epfl_xpm
  | "blank" | "Blank" -> Blank.blank_xpm
  | _ -> raise (Failure "Invalid format for background graphic.")

let xpm_bg = Param.create
  ~name:"background"
  ~default:Blank.blank_xpm
  ~doc:"Background graphic"
  ~reader:background_of_string
  ()

