(* File: gnuplot.ml

   Copyright (C) 2001-2002

   Christophe Troestler
   email: Christophe.Troestler@umh.ac.be
   WWW: http://www.umh.ac.be/math/an/software/

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details; it is available at
   <http://www.fsf.org/copyleft/gpl.html>, or by writing to the Free
   Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
   02111-1307, USA.
*)
(* 	$Id$	 *)
open Printf


(*
 * Data structures
 **********************************************************************)

module type M = 
sig
  
  exception Error of Unix.process_status
    (** Exception raised if gnuplot terminates with a status different
      from [Unix.WEXITED(0)], the argument of [Error] being its exit
      status. *)

  type handle
    (** Gnuplot handle *)

  type vec
  type mat


  (** {6 Initializing, closing and (sub)pages} *)

  type device =
    | X
    | PS of string   (* Postscript *)
    | EPS of string  (* Encapsulated PostScript *)
    | PSLaTeX of string * string (* picture environment including a PS file *)
    | FIG of string  (* Xfig format *)
    | PNG of string  (* Portable Network Graphics *)
    | MP of string   (* Metapost *)
    | MF of string   (* Metafont *)

  val device_of_filename : string -> device
    (** [device_of_filename f] guesses the device corresponding to the
      extension of [f].
      @raise Failure if none is found. *)

  val init :
    ?persist:bool -> ?color:bool -> ?nxsub:int -> ?nysub:int ->
    ?xsize:float -> ?ysize:float -> ?aspect:float ->
    device -> handle
    (** [init ?persist ?color ?nsubx ?nsuby ?sizex ?sizey ?aspect dev]
      returns a gnuplot handle to a new gnuplot session with device [dev].

      @param persist if [true], the X window will stay open after the
      gnuplot handle has been closed (default = [true]).  This has no effect
      on Win32 platforms.

      @param color if [true], a color output (as opposed to a
      monochrome one) will be produced (default = [true])

      @param nxsub number of horizontal subpages (default = 1)
      @param nysub number of vertical subpages (default = 1)

      @param xsize the horizontal size of the output
      @param ysize the vertical size of the output
      @param aspect the aspect ration of the output (default = 1.)

      For [Gnuplot.X] and [Gnuplot.PNG] devices, the size is expressed
      in pixels and is by default 550. by 550., for the other drivers,
      it is expressed in milimeters with a default of 80. by 80.  One
      has to specify both [sizex] and [sizey], in which case [aspect] is
      ignored, or one of them and [aspect]. *)

  val close : handle -> unit
    (** [close g] closes the gnuplot session [g].

      @raise Error if the gnupot process does not exit with a
      [Unix.process_status] different from [Unix.WEXITED 0].  *)


  val adv : ?sub : int -> handle -> unit
    (** [adv g] advances to the next subpage or, if necessary, opens a
      new page.  [adv ~sub:i g] goes to the subpage [i] or, if [i] is
      too big or [i <= 0], it starts a new page.  Subpages are counted
      row by row, the top left one being numbered 1.  Beware that some
      output devices (e.g., PNG) do not support multipage output.  *)

  val clear : handle -> unit
    (** [clear g] clears the current subpage. *)



  (** {6 Pens and colors} *)

  val pen : int -> unit
    (** [pen i] selects the [i]th pen type -- [i] can at least take the
      values from 0 to 6 but some devices may allow a bigger range. *)
  val pen_width : float -> unit
    (** [pen_width w] sets the pen thickness to [w] (in multiples of the
      default pen size). *)
  val point : int -> unit
    (** [point i] selects the [i]th point type. *)
  val point_width : float -> unit
    (** [point_width w] sets the point thickness to [w] (in multiples of
      the default point size). *)


  (** {6 Text} *)

  val title : handle -> string -> unit
    (** [title g t] sets the title for the current subpage of the
      gnuplot session [g] to [t]. *)
  val xlabel : handle -> string -> unit
    (** [xlabel g t] sets the label for x axis of the current subpage of
      the gnuplot session [g] to [t]. *)
  val ylabel : handle -> string -> unit
    (** [ylabel g t] sets the label for y axis of the current subpage of
      the gnuplot session [g] to [t]. *)
  val label : handle -> float -> float -> string -> unit


  (** {6 2D world coordinates, axes,...} *)

  val win : handle -> float -> float -> float -> float -> unit

  type axis_opt
  type loc = Fst | Snd | Both

  val axis : ?tics : bool -> ?mtics : bool -> ?step : float -> unit -> axis_opt
  val border : ?which:loc -> unit -> axis_opt
  val tics : ?which:loc -> ?outward:bool -> ?minor:bool ->
    ?grid:bool -> ?log:bool -> ?step:float -> unit
    -> axis_opt
  val labels : ?which:loc -> ?prec:int -> unit -> axis_opt

  val box : ?x:axis_opt list -> ?y:axis_opt list -> handle -> unit

  val env : handle ->
    ?xgrid:bool -> ?xlog:bool -> float -> float ->
    ?ygrid:bool -> ?ylog:bool -> float -> float -> unit
    (** [env g ?xgrid ?xlog xmin xmax ?ygrid ?ylog ymin ymax] sets x and
      y ranges. *)


  (** {6 2D Plots } *)

  type style = Lines | Linespoints | Points | Dots | Impulses

  val x : handle -> ?style:style -> ?label:string -> ?n0:int -> vec -> unit
    (** [x g ?style ?label ?n0 xvec] draws the points ([n0 + i],
      [xvec.{i}]) for [0 <= i < Array1.dim xvec] according to the style
      chosen.  Infinite and NaN values will be shown as discontinuities
      (i.e., no line will join the points before and after such values).

      @param style style of the graph (default = [Lines])
      @param label label for this graph (default: none)
      @param n0 x-coordinate of [xvec.{0}] (default = [0]) *)

  val xy : handle -> ?style:style -> ?label:string -> vec -> vec -> unit
    (** [xy g ?style ?label xvec yvec] draws the points ([xvec.{i}],
      [yvec.{i}]) for [0 <= i < min(Array1.dim xvec)(Array1.dim yvec)]
      according to the style chosen.

      @param style style of the graph (default = [Lines])
      @param label label for this graph (default: none) *)

  val fx : handle -> ?style:style -> ?label:string -> ?nsamples:int ->
    (float -> float) -> float -> float -> unit
    (** [fx g ?style ?label ?nsamples f a b] draws the graph of [f] over
      the interval going from [a] to [b] (inclusive).

      @param style style of the graph (default = [Lines])
      @param label label for this graph (default: none)
      @param nsamples number of point at which [f] is evaluated (default = 100)
    *)

  val xy_param : handle -> ?style:style -> ?label:string -> ?nsamples:int ->
    (float -> float * float) -> float -> float -> unit
    (** [xy_param g ?style ?label ?nsamples f a b] draws the image of the
      function [f] (i.e., the cuve parametrized by [f]) over the
      interval going from [a] to [b] (inclusive).

      @param style style of the graph (default = [Lines])
      @param label label for this graph (default: none)
      @param nsamples number of point at which [f] is evaluated (default = 100)
    *)

  val xy_file : handle -> ?style:style -> ?label:string -> string -> unit
    (** [xy_file g ?style ?label fname] draws the graph corresponding to
      the data in the file [fname].  Each line of [fname] must contain
      the x and y coordinates (possibly in scientific notation) of the
      points.  These coordinates must be separated by spaces or tabs.
      The function [xy_file] does not check that the file [fname] has
      the correct format.

      @raise Sys_error if the file does not exist. *)

  val bin : handle -> ?label:string -> ?center:bool -> vec -> vec -> unit

  val vector : handle -> ?label:string ->
    vec -> vec -> vec -> vec -> unit

  val err : handle -> ?xerr:vec -> vec -> ?yerr:vec -> vec -> unit


  (** {6 3D world coordinates, axes,...} *)

  val box3 : ?x : axis_opt list -> ?y : axis_opt list -> ?z : axis_opt list ->
    handle -> unit

  (** {6 3D Plots} *)

  val xyz : handle -> ?style:style -> ?label:string ->
    vec -> vec -> mat -> unit
    (** [xyz g ?style ?label x y z] *)

  val fxy : handle -> ?style:style -> ?label:string -> ?hidden:bool ->
    ?xnsamples:int -> ?ynsamples:int ->
    (float -> float -> float) -> float -> float -> float -> float
    -> unit
    (** [fxy g ?style ?label ?hidden ?xnsamples ?ynsamples f xmin xmax ymin ymax]
    *)

  val fxy_param : handle -> ?style:style -> ?label:string -> ?hidden:bool ->
    ?xnsamples:int -> ?ynsamples:int ->
    (float -> float -> float * float * float) ->
    float -> float -> float -> float -> unit
    (** [fxy_param g ?style ?label ?hidden ?xnsamples ?ynsamples f xmin xmax
      ymin ymax] *)

  (* val contour : handle ->  *)


  (** {6 Miscellaneous} *)

  val cmd : handle -> string -> unit
    (** [cmd g s] send the command [s] to the gnuplot referenced by the
      handle [g].  You are supposed to know what you are doing as no
      error will be returned if the command is wrong for gnuplot.
      Moreover, it can interfere with the expected behavior of the above
      functions.  This should only be useful to extend this library. *)

end

module type GnuPlotArrayType = 
sig
  type vec 
  type mat
  val dim : (vec -> int)
  val get1 : vec -> int -> float
  val get2 : mat -> int -> int -> float
end

  
module ArrayModule : 
  (GnuPlotArrayType
  with type vec = float array
  and type mat = float array array) = 
struct
  type  vec = float array
  type  mat = vec array
  let dim  = Array.length 
  let get1 vec i = vec.(i)
  let get2 mat i j = mat.(i).(j)
end


module Make(Arr: GnuPlotArrayType) : 
  (M with type vec = Arr.vec and type mat = Arr.mat)= 
struct

  exception Error of Unix.process_status

  type vec = Arr.vec
  type mat = Arr.mat
  let get1 = Arr.get1
  let get2 = Arr.get2
  let dim = Arr.dim

  (* Subpages parameters *)
  type page_param = {
    mutable env_set : bool; (* whether x,y ranges have been set *)
    set_viewport : string;  (* gnuplot cmd to set the position of the subpage *)
    cmd_hist : Buffer.t; (* history of commands except plotting; these
                            commands need only to be reissued when the
                            subpage is changed. *)
    mutable plot3d : bool;  (* says if the current plot is 3D *)
    plot_hist : Buffer.t; (* history of plotting commands; This buffer
                             will hold a command of the type "plot..."
                             or "splot..." and NOT be ended by a newline
                             to be able to add more plots. *)
  }

  (* Gnuplot handle *)
  type handle = {
    to_gplot : out_channel; (* channel to send commands to gnuplot *)
    interactive : bool;
    persist : bool; (* keep interactive window open after pipe is closed *)
    newpage : string; (* gnuplot cmd to be issued to start a new page*)
    mutable not_closed : bool;

    mutable tempf : string list;
    (* temporary files -- one list for all subpages because one cannot
       know when gnuplot has finished to use them, so one cannot erase
       them before gnuplot exit. *)
    nxsub : int;  nysub : int;   (* number of subpages in x and y directions *)
    subw : float; subh : float;  (* width and height of subpages *)
    mutable sub : int; (* current subpage number: 0,..., nxsub*nysub - 1 *)
    subpage : page_param array;  (* of length nxsub*nysub *)
  }


  (*
   * Output devices
   **********************************************************************)

  type device =
    | X
    | PS of string   (* Postscript *)
    | EPS of string  (* Encapsulated PostScript *)
    | PSLaTeX of string * string (* picture environment including a PS file *)
    | FIG of string  (* Xfig format *)
    | PNG of string  (* Portable Network Graphics *)
    | MP of string   (* Metapost *)
    | MF of string   (* Metafont *)

  let device_of_filename file =
    if file = "X" then X else
      let suffix_is = Filename.check_suffix file in
      if suffix_is ".ps"  then PS(file)
      else if suffix_is ".eps" then EPS(file)
      else if suffix_is ".tex" then
	PSLaTeX(file, (Filename.chop_suffix file ".tex") ^ ".ps")
      else if suffix_is ".fig" then FIG(file)
      else if suffix_is ".png" then PNG(file)
      else if suffix_is ".mp"  then MP(file)
      else if suffix_is ".mf"  then MF(file)
      else failwith "Gnuplot.device_of_filename: extension not recognized"
	


  (*
   * How to start a pipe to gnuplot,  specify the dimensions of the
   * graphical window and create temporary files.
   **********************************************************************
   * Configure if needed *)

  module UniX = struct
    let open_gnuplot_out dev persist xsize ysize =
      let pgm = match dev with
	| X -> sprintf "gnuplot -noraise -geometry %.0fx%.0f %s" xsize ysize
            (if persist then "-persist" else "")
	| _ -> "gnuplot" in
      Unix.open_process_out pgm

    let set_terminal_X g _ = fprintf g "set terminal x11\n"
    let open_temp_file() = Filename.open_temp_file "gp" ".dat"
  end

  module Win32 = struct
    let win32_gnuplot_ini = "C:/WINDOWS/WGNUPLOT.INI"

    (* Create a wgnuplot.ini file and launch gnuplot *)
    let open_gnuplot_with_ini xsize ysize =
      let fh = open_out win32_gnuplot_ini in
      output_string fh
	"[WGNUPLOT]\nTextOrigin=600 300\nTextSize=500 400\nGraphToTop=0\n";
      fprintf fh "GraphOrigin=30 30\nGraphSize=%.0f %.0f\n" xsize ysize;
      flush fh;
      close_out fh;
      Unix.open_process_out "pgnuplot.exe"

    let open_gnuplot_out dev persist xsize ysize =
      if Sys.file_exists win32_gnuplot_ini then begin
	(* Replace wgnuplot.ini by a special one *)
	let saved_ini = Filename.temp_file "gp" ".ini" in
	Sys.rename win32_gnuplot_ini saved_ini;
	let g = open_gnuplot_with_ini xsize ysize in
	(* Now gnuplot is open, one can restore wgnuplot.ini *)
	Sys.rename saved_ini win32_gnuplot_ini;
	g
      end else begin
	let g = open_gnuplot_with_ini xsize ysize in
	Sys.remove win32_gnuplot_ini;
	g
      end

    let set_terminal_X g = fprintf g "set terminal windows %s\n"
    let open_temp_file() = Filename.open_temp_file "gp" ".dat"
  end

  module Cygwin = struct
    (* To use Win32 gnuplot under Cygwin *)
    let open_gnuplot_out = Win32.open_gnuplot_out
    let set_terminal_X = Win32.set_terminal_X

    let cygroot = ref ""

    let init() =
      let p = Unix.open_process_in "cygpath -w /" in
      let root = String.escaped(input_line p) in
      match Unix.close_process_in p with
	| Unix.WEXITED 0 -> cygroot := root
	| _ -> failwith "Gnuplot: \"cygpath\" returned an error code!"

    let open_temp_file() =
      (* We will use pgunplot.exe with cygwin, so we have to prefix
	 cygwin /tmp dir with the cygwin root path. *)
      let (fname, fh) = Filename.open_temp_file "gp" ".dat" in
      (!cygroot ^ fname, fh)
  end

  module MacOS = struct
    let open_gnuplot_out dev persist xsize ysize =
      Unix.open_process_out "gnuplot" (* FIXME: how to specify geometry ? *)

    let set_terminal_X g _ = fprintf g "set terminal pict\n"
    let open_temp_file() = Filename.open_temp_file "gp" ".dat"
  end


  let (open_gnuplot_out, set_terminal_X, open_temp_file) =
    match Sys.os_type with
      | "Unix" -> UniX.open_gnuplot_out, UniX.set_terminal_X, UniX.open_temp_file
      | "Win32" ->
	  Win32.open_gnuplot_out, Win32.set_terminal_X, Win32.open_temp_file
      | "Cygwin" ->
	  Cygwin.init();
	  Cygwin.open_gnuplot_out, Cygwin.set_terminal_X, Cygwin.open_temp_file
      | "MacOS" ->
	  MacOS.open_gnuplot_out, MacOS.set_terminal_X, MacOS.open_temp_file
      | _ -> failwith "Gnuplot: unknown platform"



  (*
   * Gnuplot initializing, closing and (sub)pages
   **********************************************************************)


  let inches_per_mm = 1. /. 25.4

  let reset_subpage_parameters =
    "set title \"\"\n" ^
    "set xlabel \"\"\n set ylabel \"\"\n set zlabel \"\"\n" ^
    "set nologscale\n set nogrid\n" ^
    "set noborder\nset tics in\n" ^
    "set noxtics\nset noytics\nset noztics\n" ^
    "set nox2tics\nset noy2tics\nset nozeroaxis\n"

  (* Define the subpage "viewport" and reset its parameters -- but does
     NOT clear it; this is the task of the plotting command. *)
  let init_subpage gplot p =
    output_string gplot p.set_viewport;
    output_string gplot reset_subpage_parameters;
    Buffer.output_buffer gplot p.cmd_hist


  let init ?(persist=true) ?(color=true) ?(nxsub=1) ?(nysub=1)
    ?xsize ?ysize ?(aspect=1.) dev =
    let (xsize, ysize) = match xsize, ysize with
      | Some(x), Some(y) ->
          if x <= 0. then invalid_arg "Gnuplot.init: xsize <= 0"
          else if y <= 0. then invalid_arg "Gnuplot.init: ysize <= 0"
          else (x,y)
      | Some(x), None ->
          if x <= 0. then invalid_arg "Gnuplot.init: xsize <= 0"
          else if aspect <= 0. then invalid_arg "Gnuplot.init: aspect <= 0"
          else (x, aspect *. x)
      | None, Some(y) ->
          if aspect <= 0. then invalid_arg "Gnuplot.init: aspect <= 0"
          else if y <= 0. then invalid_arg "Gnuplot.init: ysize <= 0"
          else (y /. aspect, y)
      | None, None ->
          begin match dev with
            | X | PNG(_) -> (550., 550.) (* pixels *)
            | PS(_) | EPS(_) | PSLaTeX(_,_) | MP(_) | MF(_) | FIG(_) ->
		(180., 180.)  (* milimeters *)
          end in
    (* Open a pipe to gnuplot, then set device, output and size *)
    let g = open_gnuplot_out dev persist xsize ysize in
    let color = if color then "color" else "monochrome"
    and set_output s = fprintf g "set output \"%s\"\n" (String.escaped s) in
    let w, h = match dev with
      | X -> set_terminal_X g color; (* no need to set output *)
          (1., 1.)
      | PS(s) ->
          fprintf g "set terminal postscript landscape enhanced %s\n" color;
          set_output s; (* default size 10x7 inches *)
          (xsize /. 10. *. inches_per_mm, ysize /. 7. *. inches_per_mm)
      | EPS(s) ->
          fprintf g "set terminal postscript eps enhanced %s\n" color;
          set_output s;
          (xsize /. 10. *. inches_per_mm, ysize /. 7. *. inches_per_mm)
      | PSLaTeX (tex, ps) ->
          fprintf g "set terminal pslatex %s dashed rotate \"%s\"\n" color
          (String.escaped ps);
          set_output tex;
          (xsize /. 10. *. inches_per_mm, ysize /. 7. *. inches_per_mm)
      | FIG(s) ->
          fprintf g "set terminal fig %s metric size %.15g %.15g depth 50\n"
          color (xsize /. 10.) (ysize /. 10.); (* fig: size in centimeters *)
          set_output s;
          (1., 1.)
      | PNG(s) ->
          fprintf g "set terminal png small %s\n" color;
          set_output s; (* default size 640x480 *)
          (xsize /. 640., ysize /. 480.)
      | MP(s) ->
          fprintf g "set terminal mp %s dashed\n" color;
          set_output s; (* default size 5x3 inches *)
          (xsize /. 5. *. inches_per_mm, ysize /. 3. *. inches_per_mm)
      | MF(s) ->
          fprintf g "set terminal mf\n";
          set_output s; (* default size 5x3 inches *)
          (xsize /. 5. *. inches_per_mm, ysize /. 3. *. inches_per_mm) in
    fprintf g "set size %.15g,%.15g\n\n" w h;
    (* Several plots per page ? *)
    let nxsub = max nxsub 1
    and nysub = max nysub 1 in
    let nsub = nxsub * nysub
    and subw = w /. float nxsub
    and subh = h /. float nysub in
    let begin_page =
      "set multiplot\n" ^
      (match dev with
	| MP _ | MF _ ->
            (* Get the size right even if all the subpages are not used. *)
            sprintf "set size %.15g,%.15g\n clear\n" w h
	| _ -> "") ^
      (sprintf "set size %.15g,%.15g\n" subw subh) in
    output_string g begin_page;
    fprintf g "set origin 0,%.15g\n" (float(nysub - 1) *. subh);
    output_string g reset_subpage_parameters;
    flush g;
    (* Return handle *)
    { to_gplot = g;
    interactive = (dev = X);
    persist = persist;
    not_closed = true;
    newpage = "set nomultiplot\n\n" ^ begin_page;
    tempf = [];
    (* subpages *)
    nxsub = nxsub; nysub = nysub; subw = subw; subh = subh;
    sub = 0;
    subpage = Array.init nsub
  (fun i -> {
    env_set = false;
    plot3d = false;
    set_viewport = (
      (* The subpages are counted horizontally, the top
         left one having number [i=0]. *)
      let x = i mod nxsub
      and y = nysub - 1 - i / nxsub in
      sprintf "set origin %.15g,%.15g\n"
        (float x *. subw) (float y *. subh)
    );
    cmd_hist = Buffer.create 100;
    plot_hist = Buffer.create 100;
  });
    }



  let close g =
    if g.not_closed then begin
      g.not_closed <- false;
      if not g.interactive then
	(* Issue the plotting commands for the last page now *)
	Array.iter (fun p ->
          init_subpage g.to_gplot p;
          Buffer.output_buffer g.to_gplot p.plot_hist;
          output_string g.to_gplot "\n"; (* cf. page_param *)
        ) g.subpage;
      output_string g.to_gplot "set nomultiplot\n";
      flush g.to_gplot;
      (* Wait for the gnuplot process to finish and raise [Error] if
	 termination is not [Unix.WEXITED 0]. *)
      begin match Unix.close_process_out g.to_gplot with
	| Unix.WEXITED 0 -> List.iter Sys.remove g.tempf (* Remove temp files *)
	| s -> raise (Error s)
      end
    end


  let adv ?sub g =
    let nsub = g.nxsub * g.nysub in
    let sub = match sub with
      | None -> g.sub + 1
      | Some i -> if i < 1 then nsub else i - 1 in
    if sub >= nsub then begin
      (* Start a fresh new page at the first subpage and clear all history *)
      if not g.interactive then
	(* Issue the plotting commands for the current page now *)
	Array.iter (fun p ->
          init_subpage g.to_gplot p;
          Buffer.output_buffer g.to_gplot p.plot_hist;
          output_string g.to_gplot "\n"; (* cf. page_param *)
        ) g.subpage;
      output_string g.to_gplot g.newpage;
      Array.iter (fun p ->
        p.env_set <- false;
        Buffer.reset p.cmd_hist;
        Buffer.reset p.plot_hist;
      ) g.subpage;
      g.sub <- 0;
    end else
      g.sub <- sub;
    if g.interactive then
      (* Go to the subpage [sub] and re-set its parameters. *)
      init_subpage g.to_gplot g.subpage.(g.sub)


  let clear g =
    if g.interactive then begin
      output_string g.to_gplot "clear\n";
      flush g.to_gplot
    end;
    (* Clear history of the current subpage *)
    let p = g.subpage.(g.sub) in
    p.env_set <- false;
    Buffer.reset p.cmd_hist;
    Buffer.reset p.plot_hist



  (*
   * Pens and colors
   **********************************************************************)

  (* Pen defaults *)
  let penf = ref 1
  let penw = ref 1.
  let pointf = ref 1
  let pointw = ref 1.

  let pen i = penf := i
  let pen_width w = penw := w
  let point i = pointf := i
  let point_width w = pointw := w


  (*
   * Viewport
   **********************************************************************)

  let cmd_issue_and_hist g p s =
    Buffer.add_string p.cmd_hist s;
    if g.interactive then output_string g.to_gplot s

  type coord = Screen | Subpage | World

  let viewport ?(coord=Subpage) ?aspect xmin xmax ymin ymax =
    ()



  (*
   * Text
   **********************************************************************)

  let title g s =
    cmd_issue_and_hist g g.subpage.(g.sub)
      (sprintf "set title \"%s\"\n" (String.escaped s))
  let xlabel g s =
    cmd_issue_and_hist g g.subpage.(g.sub)
      (sprintf "set xlabel \"%s\"\n" (String.escaped s))
  let ylabel g s =
    cmd_issue_and_hist g g.subpage.(g.sub)
      (sprintf "set ylabel \"%s\"\n" (String.escaped s))

  let label g x y s =
    let x0 = float(g.sub mod g.nxsub)
    and y0 = float(g.nysub - 1 - g.sub / g.nxsub) in
    cmd_issue_and_hist g g.subpage.(g.sub)
      (sprintf "set label \"%s\" at screen %g,%g\n" (String.escaped s)
	((x0 +. x) *. g.subw) ((y0 +. y) *. g.subh))


  (*
   * 2D: world coordinates, axes,...
   **********************************************************************)

  let win g xmin xmax ymin ymax =
    let p = g.subpage.(g.sub) in
    cmd_issue_and_hist g p
      (sprintf "set xrange [%g:%g]\n set yrange [%g:%g]\n" xmin xmax ymin ymax);
    p.env_set <- true


  type axis_opt_record = {
    (* zero axis *)
    mutable axis : bool;
    mutable axis_tics : bool;
    mutable axis_step : float;
    mutable axis_mtics : bool;
    (* box *)
    mutable border : bool;
    mutable border2 : bool;
    (* Tics, bottom or left side *)
    mutable tics : bool;
    mutable tics_log : bool;
    mutable tics_step : float;
    mutable mtics : bool;
    mutable outward : bool;
    mutable grid : bool;
    (* Tics, top or right side *)
    mutable tics2 : bool;
    mutable tics2_log : bool;
    mutable tics2_step : float;
    mutable mtics2 : bool;
    mutable outward2 : bool;
    mutable grid2 : bool;
    (* labels *)
    mutable labels : bool;
    mutable labels2 : bool;
  }

  type axis_opt = axis_opt_record -> unit
  type loc = Fst | Snd | Both

  let axis ?(tics=true) ?(mtics=true) ?step () o =
    o.axis <- true;
    o.axis_tics <- tics;
    o.axis_mtics <- mtics;
    (match step with
      | Some s -> o.axis_step <- s
      | None -> ())

  let border ?(which=Both) () o =
    match which with
      | Fst -> o.border <- true
      | Snd -> o.border2 <- true
      | Both -> o.border <- true; o.border2 <- true

  let tics ?(which=Both) ?(outward=false) ?(minor=false) ?(grid=false)
    ?(log=false) ?step () o =
    let set1() =
      o.tics <- true; o.tics_log <- log; o.mtics <- minor;
      o.grid <- grid; o.outward <- outward;
      (match step with
	| Some s -> o.tics_step <- s
	| None -> ())
    and set2() =
      o.tics2 <- true; o.tics2_log <- log; o.mtics2 <- minor;
      o.grid2 <- grid; o.outward2 <- outward;
      (match step with
	| Some s -> o.tics2_step <- s
	| None -> ()) in
    match which with
      | Fst -> set1()
      | Snd -> set2()
      | Both -> set1(); set2()

  let labels ?(which=Fst) ?prec () o =
    match which with
      | Fst -> o.labels <- true
      | Snd -> o.labels2 <- true
      | Both -> o.labels <- true; o.labels2 <- true

  let parse_opt opt =
    let o = {
      axis = false; axis_tics = false; axis_mtics = false; axis_step = 0.;
      border = false; border2 = false;
      tics = false; tics_log = false; mtics = false; tics_step = 0.;
      outward = false; grid = false;
      tics2 = false; tics2_log = false; mtics2 = false; tics2_step = 0.;
      outward2 = false; grid2 = false;
      labels = false; labels2 = false;
    } in
    List.iter (fun f -> f o) opt;
    o


  let set_axis_opt g p axis o =
    if o.axis then begin
      cmd_issue_and_hist g p (sprintf "set %szeroaxis\n" axis);
      if o.axis_tics then begin
	cmd_issue_and_hist g p (sprintf "set %stics axis" axis);
	cmd_issue_and_hist g p (if o.axis_step <= 0. then "\n" else
          (sprintf " %f\n" o.axis_step))
      end
    end;
    (* Tics, minor tics, grid *)
    if o.tics then begin
      if o.tics_log then
	cmd_issue_and_hist g p (sprintf "set logscale %s\n" axis);
      cmd_issue_and_hist g p (sprintf "set %stics nomirror" axis);
      cmd_issue_and_hist g p (if o.tics_step <= 0. then "\n" else
        (sprintf " %f\n" o.tics_step));
      if o.mtics then
	cmd_issue_and_hist g p (sprintf "set m%stics\n" axis);
      if o.outward then cmd_issue_and_hist g p "set tics out\n";(*FIXME global *)
      if o.grid then
	cmd_issue_and_hist g p (sprintf "set grid %stics\n" axis);
    end;
    (* Numeric labels *)
    cmd_issue_and_hist g p (sprintf "set format %s \"%s\"\n" axis
      (if o.labels then "%g" else ""))

  let set_axis_opt2 g p axis o =
    if o.tics2 then begin
      if o.tics2_log then
	cmd_issue_and_hist g p (sprintf "set logscale %s2\n" axis);
      cmd_issue_and_hist g p (sprintf "set %s2tics nomirror" axis);
      cmd_issue_and_hist g p (if o.tics2_step <= 0. then "\n" else
        (sprintf " %f\n" o.tics2_step));
      if o.mtics2 then
	cmd_issue_and_hist g p (sprintf "set m%s2tics\n" axis);
      if o.outward then cmd_issue_and_hist g p "set tics out\n";(*FIXME global *)
      if o.grid2 then
	cmd_issue_and_hist g p (sprintf "set grid %s2tics\n" axis);
    end;
    (* Numeric labels *)
    cmd_issue_and_hist g p (sprintf "set format %s2 \"%s\"\n" axis
      (if o.labels2 then "%g" else ""))


  let default_opt = parse_opt [border(); tics(); labels()]

  let box ?x ?y g =
    let ox = match x with
      | None -> default_opt
      | Some o -> parse_opt o in
    let oy = match y with
      | None -> default_opt
      | Some o -> parse_opt o in
    let p = g.subpage.(g.sub) in
    set_axis_opt g p "x" ox;
    set_axis_opt g p "y" oy;
    set_axis_opt2 g p "x" ox;
    set_axis_opt2 g p "y" oy;
    (* Options that need X and Y axis to be treated together *)
    let n = if ox.border then 1 else 0 in
    let n = if ox.border2 then n+4 else n in
    let n = if oy.border then n+2 else n in
    let n = if oy.border2 then n+8 else n in
    cmd_issue_and_hist g p (sprintf "set border %i\n" n)



  let env g ?(xgrid=false) ?(xlog=false) xmin xmax
    ?(ygrid=false) ?(ylog=false) ymin ymax =
    win g xmin xmax ymin ymax;
    box g ~x:[border(); tics ~grid:xgrid ~log:xlog (); labels()]
      ~y:[border(); tics ~grid:ygrid ~log:ylog (); labels()]



  (*
   * Generic plotting functions
   **********************************************************************)

  type style = Lines | Linespoints | Points | Dots | Impulses

  let with_style = function
    | Lines -> sprintf "with lines lt %i lw %g" !penf !penw
    | Linespoints ->
	sprintf "with linespoints lt %i lw %g pt %i ps %g"
	!penf !penw !pointf !pointw
    | Points ->
	sprintf "with points lt %i lw %g pt %i ps %g"
	!penf !penw !pointf !pointw
    | Dots -> sprintf "with dots lt %i lw %g" !penf !penw
    | Impulses -> sprintf "with impulses lt %i lw %g" !penf !penw


  let plot_file g plot with_style label fname =
    let p = g.subpage.(g.sub) in
    let plot_args = sprintf " \"%s\" title \"%s\" %s"
      fname (String.escaped label) with_style in
    if g.interactive && p.env_set then begin
      fprintf g.to_gplot "%s %s\n" plot plot_args;
      flush g.to_gplot
    end else begin
      (* If x,y,z ranges do not autoscale, we do not need to erase the
	 previous plot because there is no problem with superimposing
	 possible different axes. *)
      Buffer.add_string p.plot_hist
	(if Buffer.length p.plot_hist = 0 then plot else ",\\\n ");
      Buffer.add_string p.plot_hist plot_args;
      if g.interactive then begin
	if Array.length g.subpage = 1 then
          output_string g.to_gplot "set nomultiplot\nset multiplot\n"
	else output_string g.to_gplot "clear\n";
	Buffer.output_buffer g.to_gplot p.plot_hist;
	output_string g.to_gplot "\n";
	flush g.to_gplot
      end
    end


  let is_finite x =
    let cx = classify_float x in
    cx <> FP_infinite && cx <> FP_nan

  (*
   * 2D Plots
   **********************************************************************)


  let x g ?(style=Lines) ?(label="") ?(n0=0) xvec =
    let atleast1pt = ref false
    and prev_float = ref false in
    let (fname, fh) = open_temp_file()
    and p = g.subpage.(g.sub) in
    g.tempf <- fname :: g.tempf;
    for i = 0 to dim xvec - 1 do
      let xi = get1 xvec i in
      if is_finite xi then begin
	fprintf fh "%i %.15g\n" (n0+i) xi;
	atleast1pt := true;
	prev_float := true;
      end else
	if !prev_float then begin fprintf fh "\n"; prev_float := false end;
    done;
    close_out fh;
    if !atleast1pt then plot_file g "plot" (with_style style) label fname


  let xy g ?(style=Lines) ?(label="") xvec yvec =
    let dim = min (dim xvec) (dim yvec) in
    let atleast1pt = ref false
    and prev_float = ref false in
    let (fname, fh) = open_temp_file()
    and p = g.subpage.(g.sub) in
    g.tempf <- fname :: g.tempf;
    for i = 0 to dim - 1 do
      let xi = get1 xvec i
      and yi = get1 yvec i  in
      if is_finite xi && is_finite yi then begin
        fprintf fh "%.15g %.15g\n" xi yi;
        atleast1pt := true;
        prev_float := true;
      end else
        if !prev_float then begin fprintf fh "\n"; prev_float := false end;
    done;
    close_out fh;
    if !atleast1pt then plot_file g "plot" (with_style style) label fname


  let fx g ?(style=Lines) ?(label="") ?(nsamples=100) f a b =
    let atleast1pt = ref false
    and prev_float = ref false in
    let (fname, fh) = open_temp_file()
    and p = g.subpage.(g.sub) in
    let nsamples = nsamples - 1 in
    let h = (b -. a) /. float nsamples in
    g.tempf <- fname :: g.tempf;
    for i = 0 to nsamples do
      let x = a +. float i *. h in
      let y = f x in
      if is_finite y then begin
	fprintf fh "%.15g %.15g\n" x y;
	atleast1pt := true;
	prev_float := true;
      end else
	if !prev_float then begin fprintf fh "\n"; prev_float := false end;
    done;
    close_out fh;
    if !atleast1pt then plot_file g "plot" (with_style style) label fname


  let xy_param g ?(style=Lines) ?(label="") ?(nsamples=100) f a b =
    let atleast1pt = ref false
    and prev_float = ref false in
    let (fname, fh) = open_temp_file()
    and p = g.subpage.(g.sub) in
    g.tempf <- fname :: g.tempf;
    let nsamples = nsamples - 1 in
    let h = (b -. a) /. float nsamples in
    for i = 0 to nsamples do
      let x = a +. float i *. h in
      let (y1, y2) = f x in
      if is_finite y1 && is_finite y2 then begin
        fprintf fh "%.15g %.15g\n" y1 y2;
        atleast1pt := true;
        prev_float := true;
      end else
        if !prev_float then begin fprintf fh "\n"; prev_float := false end;
    done;
    close_out fh;
    if !atleast1pt then plot_file g "plot" (with_style style) label fname


  let bin g ?(label="") ?(center=true) xvec yvec =
    let dim = dim yvec in
    let atleast1pt = ref false
    and prev_float = ref false in
    let (fname, fh) = open_temp_file()
    and p = g.subpage.(g.sub) in
    let base =
      if center then
	(fun i ->
          (if i = 0 then (3. *. (get1 xvec 0) -. (get1 xvec 1)) /. 2.
          else (get1 xvec (i-1) +. (get1 xvec i)) /. 2.),
          (if i = dim - 1 then (3. *. (get1 xvec i) -. (get1 xvec (i-1))) /. 2.
          else ((get1 xvec i) +. (get1 xvec (i+1))) /. 2.) )
      else
	(fun i -> ((get1 xvec i), (get1 xvec (i+1)))) in
    g.tempf <- fname :: g.tempf;
    for i = 0 to dim - 1 do
      let xl, xu = base i in
      let yi = get1 yvec i in
      fprintf fh "%.15g 0.\n%.15g %.15g\n%.15g %.15g\n%.15g 0.\n"
	xl xl yi xu yi xu;
      atleast1pt := true;
      prev_float := true;
    done;
    close_out fh;
    if !atleast1pt then plot_file g "plot" (with_style Lines) label fname


  let vector g ?(label="") xvec yvec x1vec y1vec =
    let dim = min (dim xvec) (dim yvec) in
    let atleast1pt = ref false
    and prev_float = ref false in
    let (fname, fh) = open_temp_file()
    and p = g.subpage.(g.sub) in
    g.tempf <- fname :: g.tempf;
    for i = 0 to dim - 1 do
      let xi = get1 xvec i
      and yi = get1 yvec i
      and xi1 = get1 x1vec i
      and yi1 = get1 y1vec i  in
      if is_finite xi && is_finite yi
	&& is_finite xi1 && is_finite yi1 then begin
          fprintf fh "%.15g %.15g %.15g %.15g\n" xi yi xi1 yi1;
          atleast1pt := true;
          prev_float := true;
	end else
          if !prev_float then begin fprintf fh "\n"; prev_float := false end;
    done;
    close_out fh;
    if !atleast1pt then
      let with_vector = sprintf "with vector lt %i lw %g" !penf !penw in
      plot_file g "plot" with_vector label fname


  let err g ?xerr xvec ?yerr yvec =
    ()


  let xy_file g ?(style=Lines) ?(label="") fname =
    if Sys.file_exists fname then
      plot_file g "plot" (with_style style) label fname
    else raise (Sys_error (fname ^ ": No such file or directory"))



  (*
   * 3D: world coordinates, axes,...
   **********************************************************************)


  let box3 ?x ?y ?z g =
    let ox = match x with
      | None -> default_opt
      | Some o -> parse_opt o in
    let oy = match y with
      | None -> default_opt
      | Some o -> parse_opt o in
    let oz = match z with
      | None -> default_opt
      | Some o -> parse_opt o in
    let p = g.subpage.(g.sub) in
    set_axis_opt g p "x" ox;
    set_axis_opt g p "y" oy;
    set_axis_opt g p "z" oy;
    set_axis_opt2 g p "x" ox;
    set_axis_opt2 g p "y" oy;
    (* Options that need X and Y axis to be treated together *)
    let n = if ox.border then 1 else 0 in
    let n = if ox.border2 then n+4 else n in
    let n = if oy.border then n+2 else n in
    let n = if oy.border2 then n+8 else n in
    let n = n + 16 + 32 + 64 + 256 + 512 in
    cmd_issue_and_hist g p (sprintf "set border %i\n" n)



  (*
   * 3D Plots
   **********************************************************************)

  let xyz g ?(style=Lines) ?(label="") xvec yvec zmat =
    let atleast1pt = ref false in
    let (fname, fh) = open_temp_file()
    and p = g.subpage.(g.sub) in
    g.tempf <- fname :: g.tempf;
    for i = 0 to dim xvec - 1 do
      let xi = get1 xvec i in
      if is_finite xi then begin
	for j = 0 to dim yvec - 1 do
          let yi = get1 yvec j  in
          let zij = get2 zmat i j in
          if is_finite yi && is_finite zij then begin
            fprintf fh "%.15g %.15g %.15g\n" xi yi zij;
            atleast1pt := true
          end
	done
      end
    done;
    close_out fh;
    if !atleast1pt then plot_file g "splot" (with_style style) label fname


  let fxy g ?(style=Lines) ?(label="") ?(hidden=true)
    ?(xnsamples=30) ?(ynsamples=30) f xmin xmax ymin ymax =
    let atleast1pt = ref false in
    let (fname, fh) = open_temp_file()
    and p = g.subpage.(g.sub) in
    g.tempf <- fname :: g.tempf;
    let xnsamples = xnsamples - 1
    and ynsamples = ynsamples - 1 in
    let hx = (xmax -. xmin) /. float xnsamples
    and hy = (ymax -. ymin) /. float ynsamples in
    for i = 0 to xnsamples do
      let x = xmin +. float i *. hx in
      for j = 0 to ynsamples do
	let y = ymin +. float j *. hy in
	let z = f x y in
	if is_finite z then begin
          fprintf fh "%.15g %.15g %.15g\n" x y z;
          atleast1pt := true
	end
      done;
      output_string fh "\n";
    done;
    close_out fh;
    if !atleast1pt then begin
      cmd_issue_and_hist g p (* FIXME: not sync for replay *)
	(if hidden then "set hidden3d\n" else "set nohidden3d");
      plot_file g "splot" (with_style style) label fname
    end

  let fxy_param g ?(style=Lines) ?(label="") ?(hidden=true)
    ?(xnsamples=30) ?(ynsamples=30) f xmin xmax ymin ymax =
    let atleast1pt = ref false in
    let (fname, fh) = open_temp_file()
    and p = g.subpage.(g.sub) in
    g.tempf <- fname :: g.tempf;
    let xnsamples = xnsamples - 1
    and ynsamples = ynsamples - 1 in
    let hx = (xmax -. xmin) /. float xnsamples
    and hy = (ymax -. ymin) /. float ynsamples in
    for i = 0 to xnsamples do
      let x = xmin +. float i *. hx in
      for j = 0 to ynsamples do
	let y = ymin +. float j *. hy in
	let (z1, z2, z3) = f x y in
	if is_finite z1 && is_finite z2 && is_finite z3 then begin
          fprintf fh "%.15g %.15g %.15g\n" z1 z2 z3;
          atleast1pt := true
	end
      done;
      output_string fh "\n";
    done;
    close_out fh;
    if !atleast1pt then begin
      cmd_issue_and_hist g p (* FIXME: not sync for replay *)
	(if hidden then "set hidden3d\n" else "set nohidden3d");
      plot_file g "splot" (with_style style) label fname
    end



  (*
   * Miscellaneous
   **********************************************************************)

  let cmd g msg =
    fprintf g.to_gplot "%s\n" msg;
    flush g.to_gplot


end


module GnuplotArray = Make (ArrayModule)
