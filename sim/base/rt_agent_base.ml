
class virtual base ?(stack=0) owner =
object(s : #Rt_agent.t)
  val myid = owner#id
  val owner:#Simplenode.simplenode = owner

  method myid = myid

  method private mac_bcast_pkt l3pkt = owner#mac_bcast_pkt ~stack l3pkt

  method private mac_send_pkt l3pkt ~dstid = 
    owner#mac_send_pkt ~stack ~dst:dstid l3pkt

  method private cheat_send_pkt l3pkt dstid = 
    owner#cheat_send_pkt ~stack ~dst:dstid l3pkt 

  method virtual mac_recv_l3pkt : L3pkt.t -> unit
  method virtual mac_recv_l2pkt : L2pkt.t -> unit
  method virtual app_recv_l4pkt : L4pkt.t -> Common.nodeid_t -> unit

  method private bps = (owner#mac ~stack ())#bps

end
