(** Augmented versions of the standard library modules *)

 module Array = struct

   include Array

   let filter test a =
     let result = (Array.fold_left
       (fun accu elt ->
	 if test elt then elt :: accu else accu)
       [] a) in
     Array.of_list (List.rev result)

   let iter a b = Array.iter b a
   let iteri a b = Array.iteri b a
   let map a b = Array.map b a
  end



 module List = struct

   include ListLabels

  end
