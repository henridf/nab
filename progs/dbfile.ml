open Misc
open Mods

module type DBFileReader_t = 
sig
  type handle

  (** Functions for reading/writing/creating jdbs *)
    
  val openfile : ?infile:string -> unit -> handle
    (** Reads in a jdb file from infile (or from stdin if infile not given), 
      and returns the handle to the loaded jdb structure *)

  val writefile : ?outfile:string -> handle ->  unit

  val newdb : unit -> handle
    (** Returns a handle to a fresh, empty jdb structure *)

  (** Functions for accessing jdb data *)

  val width : handle -> int
    (** The number of fields, as given by the first data line. *)

  val header : handle -> string list
    (** Returns the column names, without the #h marker, or [] if no header *)

  val data_column : handle -> string -> string array
    (** data_column h col returns column with header 'col' as a list *)

  val data_row : handle -> int -> string list

  val comment_lines : handle -> string list 
    (** The list of comment lines, ie all lines starting with '#' except for
      the first line starting with '#h'
      ["line 1"; "line 2"; "line 2"; ..] *)

  (** Functions for modifying jdb data *)
    
  val new_header : handle -> string list -> unit
    (** new_neader h newheader replaces the h's header with newheader *)

  val new_column : handle -> string -> string array -> unit
    (** data_column h colname col 
      adds column column col with header 'colname'
   *)


  val add_comments : handle -> string list -> unit
  val add_line : handle -> string list -> unit

end

module DBFileReader : DBFileReader_t =
struct
  type handle = {
    mutable data_lines: string list array;
    mutable comment_lines:string list;
    mutable header: string list;
  }

  let openfile ?(infile="/dev/stdin") () = 
    (* break up lines on ' ' *)
    let re = Str.regexp "[ \t]+" in

    let chan = open_in infile in
    let all_strings = lines_of_file infile in
    close_in chan;
    let comment_lines =   
      List.filter 
	(fun s ->   try 
	  s.[0] = '#' &&
	  (String.sub s 0 2) <> "#h" with _ -> false) 
      all_strings
    in 
    let header = ref [] in
    List.iter 
      (fun s -> try 
	if (String.sub s 0 2) = "#h" then 
	  (* drop head of list which is "#h" *)
	  header := List.tl (Str.split re s) with _ -> ())
      all_strings;
    
    let data_strings =   List.filter 
      (* catch exception if empty line *)
      (fun s ->   try s.[0] <> '#' with _ -> false) 
      all_strings
    in
    let data_lines = Array.of_list (List.map (fun s -> Str.split re s) data_strings)
      
    in 
    {data_lines=data_lines; comment_lines=comment_lines; header = !header}

    
  let newdb () = {data_lines = [||]; comment_lines = []; header = []}

  let width h = List.length (h.data_lines.(0))

  let comment_lines h = h.comment_lines

  let header h = h.header

  let colindex_ h colname = 
    let rec r hdr i = 
      match hdr with
	| hd::tl when (hd = colname) -> i
      | hd::tl -> r tl (i + 1)
      | [] -> raise (Failure ("No column named "^colname))
    in r h.header 0

  let colname_ h index = 
    try Some (List.nth h.header index) with 
	Failure "nth" -> raise (Failure (Printf.sprintf "No column number %d" index))
	  
  let data_column h colname = 
    let colindex = colindex_ h colname in
    Array.map (fun l -> List.nth l colindex) h.data_lines


  let data_row h n = h.data_lines.(n)

  let new_header handle h =  handle.header <- h
    
  let new_column h colname col = 
    
    if (List.mem colname h.header) then failwith 
      ("add_column: a column named "^colname^" already exists");

    if ((Array.length col) <> (Array.length h.data_lines)) then 
      failwith 	(Printf.sprintf 
	"new_column: new column has %d elements, existing columns have %d"
	(Array.length col)  (Array.length h.data_lines));
    (* Add new header *)
    new_header h (h.header @ [colname]);
    (* Add column of data *)
    for i = 0 to Array.length h.data_lines - 1 do
      h.data_lines.(i) <- h.data_lines.(i) @ [col.(i)]
    done

  let sprint_data_line_ dl = chop (List.fold_left (fun s data -> s^data^" ") "" dl)

  let writefile ?(outfile="/dev/stdout") h = 
    let outchan = open_out outfile in

    output_string outchan "#h ";
    List.iter (fun s -> output_string outchan (s^" ")) h.header;
    output_string outchan "\n";

    List.iter (fun s -> output_string outchan (s^"\n")) h.comment_lines;
    Array.iter (fun s -> output_string outchan ((sprint_data_line_ s)^"\n")) h.data_lines ;

    close_out outchan


let add_comments handle c = 
  handle.comment_lines <- (handle.comment_lines @ c)

let add_line handle l = handle.data_lines <- (Array.append handle.data_lines  [|l|])



end
