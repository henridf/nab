open Coord

class virtual mobility (abbrevname:string) = 
object(s)
  val abbrev = abbrevname
  method virtual initialize : unit -> unit
  method virtual getnewpos : node:Node.node_t -> Coord.coordf_t
  method move ~(node:Node.node_t) = (
    let newpos = s#getnewpos ~node:node in 
    node#move newpos
  )
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

  method getnewpos ~node = (
  
    let target = waypoint_targets_.(node#id) in
    let pos = node#pos in
    assert (((Gworld.world())#boundarize pos) = pos);
    if ((Gworld.world())#dist_coords target pos) <= Gworld.one_meter then (
      (* arrived at target *)
      waypoint_targets_.(node#id) <- (Gworld.world())#random_pos;
      target
    ) else (
      let direction =  (Coord.normalize (target ---. pos))  ***. Gworld.one_meter in
      (pos +++. direction)
    )
  )


end


class randomJump = 
object 
  inherit mobility "rj"

  method initialize () = ()
  method getnewpos ~node = 
    (Gworld.world())#random_pos
  method move ~node  = 
    node#move (Gworld.world())#random_pos
end

class randomWalk = 
object 

  inherit mobility "rw"

  method initialize () = ()
  method getnewpos ~node = 
    (Gworld.world())#boundarize
    (* amplitude of 3 gives us a variance of 3/4 along either axis *)
    (node#pos +++. ((Random.float 3.0, Random.float 3.0) ---. (1.5, 1.5)))


end

