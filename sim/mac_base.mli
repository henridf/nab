class base : ?stack:int -> bps:float -> #Simplenode.simplenode ->
object
  method private xmitdelay : bytes:int -> float
  method private send_up : l2pkt:L2pkt.t -> unit
end
