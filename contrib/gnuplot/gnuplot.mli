(* File: gnuplot.mli

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

(**
  Library for scientific plotting using gnuplot.

  This library implements a simple interface to the gnuplot program.
  All functions talk to gnuplot through a pipe, so crude animations
  are possible.

  @version 0.3
  @author Christophe Troestler
*)


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

module Make (Arr : GnuPlotArrayType) : M with type vec = Arr.vec and type mat = Arr.mat


module GnuplotArray : 
  (M with type vec = float array and type mat = float array array)
