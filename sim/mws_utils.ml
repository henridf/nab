open Misc

let speed_of_light = 1e8
let bits_per_sec = 1e6
let propdelay p1 p2 = (sqrt (Coord.dist_sq p1 p2)) /. speed_of_light
let xmitdelay ~bytes = (i2f (bytes * 8)) /. bits_per_sec
  
  
