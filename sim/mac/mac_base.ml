open Misc

class virtual base ?(stack=0) ~bps owner = 
object(s)

  val myid = owner#id
  val owner:#Simplenode.simplenode = owner

  method private xmitdelay ~bytes = (i2f (bytes * 8)) /. bps

  method private send_up ~l2pkt = 
    owner#mac_recv_pkt ~stack l2pkt

  method virtual recv : ?snr:float -> l2pkt:L2pkt.t -> unit -> unit
  method virtual xmit : l2pkt:L2pkt.t -> unit
end
