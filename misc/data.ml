open Printf
open Misc

(** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **)
(**                                                                                     **)
(** Simple stats                                                                        **)
(**                                                                                     **)
(** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **)

let avg_f (a: float array) = (Array.fold_left (+.) 0.0 a) /. (i2f (Array.length a))
let avg_i (a: int array) = (i2f (Array.fold_left (+) 0 a)) /. (i2f (Array.length a))

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
type stat_f = {avg : float; var : float; count : int}
type bin_f = {center: float; stats : stat_f array}

(* val binnify_f : datapoint_f array -> int -> bin_f array = <fun> *)
let binnify_f (a: datapoint_f array) nbins = ( 
    Array.sort (fun (l1: datapoint_f) (l2:datapoint_f) -> 
		  if (l1.x < l2.x) then (-1) else if (l1.x = l2.x) then 0 else 1
	       ) a;
    
    let lowx = ref a.(0).x  in
    let highx = ref a.(Array.length a - 1).x  in
    let nmeasures = (Array.length a.(0).data) in
    let binwidth = (!highx -. !lowx) /. (i2f nbins) in
    let whichbin num =   
      if isint (num /. binwidth) then 
	max 0 ((f2i (num /. binwidth) ) - 1) else 
	  f2i (floor ((num /. binwidth))) in
    let nthbin n = (* datapoint_f array *)
      let count = array_count_filt (fun el -> if whichbin el.x = n then true else false) a in
      let bin = Array.make count {x = 0.0; data = [||]} in
      let ctr = ref 0 in
	(* xxx/canoptimize since a is sorted, no need to iterate over whole array *)
	Array.iter (fun el -> 
		      if (whichbin el.x  = n)  then begin bin.(!ctr) <- el; incr ctr end;
		   ) a;
	bin 
    in
      Array.init nbins (fun bin -> 
			   let stats = Array.make nmeasures {avg=0.0; var=0.0; count=0} in 
			     for i = 0 to nmeasures - 1 do
			       stats.(i) <- {
				 avg = avg_f (Array.map (fun l -> l.data.(i)) (nthbin bin));
				 var = var_f (Array.map (fun l -> l.data.(i)) (nthbin bin)) ;
				 count = (Array.length (nthbin bin))
			       }
			     done;
			  {center =  (binwidth /. 2.0) +.  (binwidth *. (i2f bin)); 
			   stats = stats}
		       )
)

(* val write_bins : bin_f array -> out_channel -> unit *)
let write_bins bins out = (
  Array.iter (fun {center=center; stats=stats} -> (
		fprintf out  "%.2f " center;
		Array.iter (fun {avg=avg; var=var; count=count} -> (
			      fprintf out "%.2f " avg;
			      fprintf out "%.2f " var;
			      fprintf out  "%d  " count;
			    )
			   ) stats;
		fprintf out "\n" ;
	      )
	     ) bins
)
