open Coord

class virtual mobility (abbrevname:string) = 
object(s)
  val abbrev = abbrevname
  method virtual initialize : unit -> unit
  method virtual move : node:Node.node_t -> unit
  method abbrevname = abbrev (* for making filenames etc *)
end


class waypoint = 
object 
  inherit mobility "wp"
  val mutable waypoint_targets_ = [||]

  method initialize () = 
    waypoint_targets_ <- (
      let w = ref (Array.make (Param.get Params.nodes) ((0.0, 0.0):Coord.coordf_t)) in
      Array.iteri (
	fun i nothing  -> 
	  !w.(i) <- (Gworld.world())#random_pos
      ) !w;
      !w
    )

  method move ~node = (
  
    let target = waypoint_targets_.(node#id) in
    let pos = node#pos in
    if ((Gworld.world())#dist_coords target pos) <= 1.0 then 
      (* arrived at target *)
      begin
	waypoint_targets_.(node#id) <- (Gworld.world())#random_pos;
	node#move target
      end
    else 
      begin
	let direction = Coord.normalize (target ---. pos) in
	node#move (pos +++. direction)
      end
  )

end


class randomjump = 
object 
  inherit mobility "rj"

  method initialize () = ()
  method move ~node  = 
    node#move (Gworld.world())#random_pos
end

class randomWalk = 
object 

  inherit mobility "rw"

  method initialize () = ()
  method move ~node = 

    let newpos = 
      (Gworld.world())#boundarize
      (* amplitude of 3 gives us a variance of 3/4 along either axis *)
      (node#pos +++. ((Random.float 3.0, Random.float 3.0) ---. (1.5, 1.5)))
    in 
    node#move newpos

end

