open Misc

class base ?(stack=0) ~bps owner = 
object(s)

  val ownerid = owner#id
  val owner:#Simplenode.simplenode = owner

  method private xmitdelay ~bytes = (i2f (bytes * 8)) /. bps

  method private send_up ~l2pkt = 
    owner#mac_recv_pkt ~stack l2pkt
end
