
class type t = 
object

  method myid : Common.nodeid_t

  method mac_recv_l3pkt : L3pkt.t -> unit

  method mac_recv_l2pkt : L2pkt.t -> unit

  method app_recv_l4pkt : L4pkt.t -> Common.nodeid_t -> unit
end

