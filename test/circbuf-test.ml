open Misc
open Circbuf

let test_ () = ( 
  (* 0-length cbufs *)
  let cb = CircBuf.make_ 0 in
    assert (CircBuf.maxlength_ cb = 0);
    assert (CircBuf.length_ cb = 0);
    assert (CircBuf.toarray_ cb = [||]);
    assert (CircBuf.equal_ cb (CircBuf.fromarray_ [||]));
    assert (CircBuf.equal_ cb (CircBuf.sub_ cb 0));


    let cb = CircBuf.make_ 5 in
    let cb2 = CircBuf.make_ 6 in
    let ctr = ref 0 in 
      assert (CircBuf.equal_ cb cb);
      assert (CircBuf.length_ cb = 0);
      assert (CircBuf.maxlength_ cb = 5);
      assert (CircBuf.toarray_ cb = [||]);
      
      CircBuf.push_ cb (i2f 0);
      CircBuf.push_ cb2 (i2f 0);
      assert (CircBuf.equal_ cb (CircBuf.sub_ cb 1));

      assert (CircBuf.equal_ cb cb);
      assert (CircBuf.equal_ cb cb2);
      assert (CircBuf.equal_ cb2 cb);
      CircBuf.push_ cb2 (i2f 0);
      assert (CircBuf.equal_ cb (CircBuf.sub_ cb2 1));

      assert (not (CircBuf.equal_ cb cb2));
      assert (not (CircBuf.equal_ cb2 cb));
      assert ((CircBuf.get_ cb 0) = (i2f 0));
      assert (CircBuf.length_ cb = 1);
      
      assert (CircBuf.toarray_ cb = [|0.0|]);
      ctr := 0;
      CircBuf.iter_ (fun x -> incr ctr) cb;
      assert (!ctr = 1);
      
      for i = 1 to 4 do 
	CircBuf.push_ cb (i2f i);
	assert ((CircBuf.get_ cb 0) = (i2f i));
      done;
      CircBuf.push_ cb2 2.0;      
      CircBuf.push_ cb2 3.0;
      CircBuf.push_ cb2 4.0;
      assert (not (CircBuf.equal_ cb2 cb));

      assert (CircBuf.length_ cb = 5);
      assert (CircBuf.toarray_ cb = [|4.0; 3.0; 2.0; 1.0; 0.0|]);
      assert (cb = CircBuf.fromarray_ [|4.0; 3.0; 2.0; 1.0; 0.0|]);
      assert (CircBuf.toarray_ (CircBuf.sub_ cb 2) = [|4.0; 3.0|]);
      assert (CircBuf.toarray_ (CircBuf.sub_ cb 5) = [|4.0; 3.0; 2.0; 1.0; 0.0|]);
      
      for i = 0 to 4 do 
	assert ((CircBuf.get_ cb i) = i2f (4 - i))
      done;
      
      for i = 0 to 2 do 
	CircBuf.push_ cb (i2f (i + 5));
      done;
      assert (CircBuf.toarray_ cb = [| 7.0; 6.0; 5.0; 4.0; 3.0 |]);
      (* internal representation = [| 4.0; 3.0; 7.0; 6.0; 5.0 |];*)
      (*  head                                  ^^^              *)
      assert ((CircBuf.get_ cb 0) = 7.0);
      assert ((CircBuf.get_ cb 1) = 6.0);
      assert ((CircBuf.get_ cb 2) = 5.0);
      assert ((CircBuf.get_ cb 3) = 4.0);
      assert ((CircBuf.get_ cb 4) = 3.0);

      let relcheck = ref 0 and valcheck = ref 7 in
      let iterichecker relindex value = (
	assert (relindex =  !relcheck);
	assert (f2i value =  !valcheck);
	incr relcheck;
	decr valcheck;
      ) in
	CircBuf.iteri_ iterichecker cb;
	Printf.printf "CircBuf.test_ : passed \n";

)

let _  = test_ ()
