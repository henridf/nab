type ack = {cost : int}
    (* cost: the cost of the acking node *)
type pkt = {
  min : int;
  max : int;
  num: int
}

type t = ACK of ack | PKT of pkt

let make_ack ~cost = {cost=cost}

let make_pkt ~min ~max ~num = {min=min; max=max; num=num}

let clone p = p

let string_of_tdack_pkt = function
  | ACK {cost=c} -> Printf.sprintf "ACK: cost %d" c
  | PKT p -> Printf.sprintf "PKT: min: %d max: %d num: %d" p.min p.max p.num

let size p = 
  match p with
    | ACK _ -> 1
    | PKT _ -> 3

let tdack_cost p =
  match p with
    | ACK {cost=c} -> c
    | PKT _ -> -1
