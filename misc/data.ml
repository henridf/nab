open Printf
open Misc

(** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **)
(**                                                                                     **)
(** Simple stats                                                                        **)
(**                                                                                     **)
(** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **)

let avg_f (a: float array) = (Array.fold_left (+.) 0.0 a) /. (i2f (Array.length a))
let avg_i (a: int array) = (i2f (Array.fold_left (+) 0 a)) /. (i2f (Array.length a))

let avg_list_f (a: float list) = (List.fold_left (+.) 0.0 a) /. (i2f (List.length a))
let avg_list_i (a: int list) = (i2f (List.fold_left (+) 0 a)) /. (i2f (List.length a))

let var_f (a: float array) = 
  let avg = avg_f a in
    (Array.fold_left (fun s x -> s +. ((x -. avg) ** 2.0)) 0.0 a) /. (i2f (Array.length a))

let var_i (a: int array) = 
  let ints = Array.map (fun i -> i2f i) a in
    var_f ints



(** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **)
(**                                                                                     **)
(** Data Handling                                                                       **)
(**                                                                                     **)
(** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **)

type datapoint_f = {x : float ; data : float array}
type stat_f = {avg : float; var : float}
type bin_f = {center: float; count: int; stats : stat_f array}
    (* there is one  stat for each datapoint in the datapoint_f.data *)

(* val binnify_f : datapoint_f array -> int -> bin_f array = <fun> *)
let binnify_f (datas: datapoint_f array) nbins = ( 
    Array.sort (fun (l1: datapoint_f) (l2: datapoint_f) -> 
		  if (l1.x < l2.x) then (-1) else if (l1.x = l2.x) then 0 else 1
	       ) datas;
    
  let lowx = ref datas.(0).x  in
  let highx = ref datas.(Array.length datas - 1).x  in
  let nmeasures = (Array.length datas.(0).data) in
  let binwidth = (!highx -. !lowx) /. (i2f nbins) in
  let whichbin num =   
    if isint (num /. binwidth) then 
      max 0 ((f2i (num /. binwidth) ) - 1) else 
	f2i (floor ((num /. binwidth))) in
  let nthbin n = (* datapoint_f array *)
    let count = array_count_filt (fun el -> if whichbin el.x = n then true else false) datas in
    let bin = Array.make count {x = 0.0; data = [||]} in
    let ctr = ref 0 in
    (* xxx/canoptimize since a is sorted, no need to iterate over whole array *)
    Array.iter (fun el -> 
      if (whichbin el.x  = n)  then begin bin.(!ctr) <- el; incr ctr end;
    ) datas;
    bin 
  in
  Array.init nbins (fun bin -> 
    let stats = Array.make nmeasures {avg=0.0; var=0.0} in 
    for i = 0 to nmeasures - 1 do
      stats.(i) <- {
	avg = avg_f (Array.map (fun l -> l.data.(i)) (nthbin bin));
	var = var_f (Array.map (fun l -> l.data.(i)) (nthbin bin)) ;
      }
    done;
    {center =  (binwidth /. 2.0) +.  (binwidth *. (i2f bin)); 
    count = (Array.length (nthbin bin));
    stats = stats}
  )
)

let adaptive_binnify_f (datas: datapoint_f array) nbins = ( 
  Array.sort (fun (l1: datapoint_f) (l2: datapoint_f) -> 
    if (l1.x < l2.x) then (-1) else if (l1.x = l2.x) then 0 else 1
  ) datas;
    
  let nmeasures = (Array.length datas.(0).data) in
  let binwidth = (Array.length datas) /  nbins in
  let nthbin n = (* datapoint_f array *)
    Array.sub datas 
      (n * binwidth) binwidth
  in
  Array.init nbins (fun bin -> 
    let stats = Array.make nmeasures {avg=0.0; var=0.0} in 
    let center = avg_f (Array.map (fun l -> l.x) (nthbin bin)) in
    for i = 0 to nmeasures - 1 do
      stats.(i) <- {
	avg = avg_f (Array.map (fun l -> l.data.(i)) (nthbin bin));
	var = var_f (Array.map (fun l -> l.data.(i)) (nthbin bin)) ;
      }
    done;
    {center = center;
    count = (Array.length (nthbin bin));
    stats = stats}
  )
)
  
(* val write_bins : bin_f array -> out_channel -> unit *)
let write_bins bins out = (
  Array.iter (fun {center=center; count = count; stats=stats} -> (
    if count > 0 then (
      fprintf out  "%.2f " center;
      Array.iter (fun {avg=avg; var=var} -> (
	fprintf out "%.2f " avg;
	fprintf out "%.2f " var
      );
      ) stats;
      fprintf out  "%d  " count;
      fprintf out "\n" ;
    )
  )
  ) bins
)
  
  
(* val write_datapoints : datapoint_f array -> out_channel -> unit*)
let write_datapoints points out = (
  Array.iter 
  (fun {x=x; data=data} -> (
    fprintf out "%.2f " x;
    Array.iter (fun stat -> fprintf out "%.2f " stat;) data;
    fprintf out "\n";
  )
  ) 
  points
)
