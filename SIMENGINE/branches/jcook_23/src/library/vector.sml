structure VectorLib =
struct

val TypeMismatch = DynException.TypeMismatch
and IncorrectNumberOfArguments = DynException.IncorrectNumberOfArguments

fun error msg =
    Logger.log_usererror [PosLog.NOPOS] (Printer.$ msg)

fun std_index _ args =
    (case args of
	 [KEC.VECTOR {array, front_index, ...}, KEC.LITERAL (KEC.CONSTREAL r)] => 
	 (* TODO: propogate type information for vector cells *)
	 (KEC.CELL (KEC.DONTCARE, KEC.GETSET (fn() => Array.sub(!array, !front_index + (Real.round r) - 1),
					      fn(exp) => Array.update(!array, !front_index + (Real.round r) - 1, exp)))
	  handle _ =>
		 (error "Vector index out of bounds"; KEC.CELL (KEC.DONTCARE, KEC.REFERENCE(ref KEC.UNIT)))) (*TODO: error handling system*)
       | [KEC.TUPLE exps, KEC.LITERAL (KEC.CONSTREAL r)] => 
	 ((List.nth (exps, (Real.round r) - 1))
	  handle _ =>
		 (error "Tuple index out of bounds"; KEC.UNIT)) (*TODO: error handling system*)
       | [a,b] 
	 => raise TypeMismatch ("expected a sequence and a number but received " ^ (PrettyPrint.kecexp2nickname a) ^ " and " ^ (PrettyPrint.kecexp2nickname b))
       | _ => raise IncorrectNumberOfArguments {expected=2, actual=(length args)})



fun std_tabulate _ args =
    (case args of
	 [KEC.LITERAL (KEC.CONSTREAL r1), KEC.LITERAL (KEC.CONSTREAL r2)] => 
	 (let
	      val array = Array.tabulate (Real.floor(r2-r1+1.0), fn(x) => KEC.LITERAL(KEC.CONSTREAL(Real.fromInt(x)+r1)))
	  in
	      KEC.VECTOR {array = ref (array),
			  front_index =ref 0,
			  front_pad_size= ref (Real.floor(Real.fromInt(Array.length array) / 2.0)),
			  back_index=ref (Array.length array),
			  back_pad_size= ref(Real.ceil(Real.fromInt(Array.length array) / 2.0))}
	  end	 
	  handle _ =>
		 (error "Vector index out of bounds"; KEC.CELL (KEC.DONTCARE, KEC.REFERENCE(ref KEC.UNIT)))) (*TODO: error handling system*)
       | [a,b] 
	 => raise TypeMismatch ("expected 2 numbers but received " ^ (PrettyPrint.kecexp2nickname a) ^ " and " ^ (PrettyPrint.kecexp2nickname b))
       | _ => raise IncorrectNumberOfArguments {expected=2, actual=(length args)})


fun std_push_front _ args =
    (case args of
	 [KEC.VECTOR (arg as {array, front_index, front_pad_size, back_index, back_pad_size}), exp] => 
	 (if !front_index = 0 then (* need to resize *)
	      let
		  val array' = Array.array (!front_pad_size * 2 + !back_pad_size, KEC.UNDEFINED)
		  val front_index' = !front_pad_size - 1
		  val _ = Array.copy{src= !array, dst=array', di= 0}
		  val back_index' = front_index' + Array.length(!array)
				    
		  val _ = array := array'
		  val _ = front_index := front_index'
		  val _ = back_index := back_index'
		  val _ = front_pad_size := !front_pad_size * 2
			  
		  val _ = Array.update(!array, !front_index-1, exp)
		  val _ = front_index := !front_index - 1
	      in
		  KEC.VECTOR arg
	      end
	  else
	      let
		  val _ = Array.update(!array, !front_index-1, exp)
		  val _ = front_index := !front_index - 1
	      in
		  KEC.VECTOR arg
	      end)
	 
       | [a,b]
	 => raise TypeMismatch ("expected a vector and a value but received " ^ (PrettyPrint.kecexp2nickname a) ^ " and " ^ (PrettyPrint.kecexp2nickname b))
       | _ => raise IncorrectNumberOfArguments {expected=2, actual=(length args)})


fun std_push_back _ args =
    (case args of
	 [KEC.VECTOR (arg as {array, front_index, front_pad_size, back_index, back_pad_size}), exp] => 
	 (if !back_index = (Array.length(!array) - 1) then (* need to resize *)
	     let
		 val array' = Array.array (!front_pad_size + !back_pad_size * 2 , KEC.UNDEFINED)
		 val _ = Array.copy{src= !array, dst=array', di= 0}
			 
		 val _ = array := array'
		 val _ = back_pad_size := !back_pad_size * 2

		  val _ = Array.update(!array, !back_index, exp)
		  val _ = back_index := !back_index + 1
	     in
		 KEC.VECTOR arg
	     end
	  else
	      let
		  val _ = Array.update(!array, !back_index, exp)
		  val _ = back_index := !back_index + 1
	      in
		  KEC.VECTOR arg
	      end)
	     
       | [a,b]
	 => raise TypeMismatch ("expected a vector and a value but received " ^ (PrettyPrint.kecexp2nickname a) ^ " and " ^ (PrettyPrint.kecexp2nickname b))
       | _ => raise IncorrectNumberOfArguments {expected=2, actual=(length args)})

fun std_map exec args =
    (case args of
	 [KEC.VECTOR exps, func] => 
	 let
	     val list = GeneralUtil.listSlice (GeneralUtil.array2list (! (#array exps))) (!(#front_index exps), !(#back_index exps))
	     val list' = map (fn(e) => exec (KEC.APPLY{func=func, args=KEC.TUPLE [e]})) list
	     val newarray = Array.fromList(list')
	     val finalarray = Array.array(!(#front_pad_size exps) + !(#back_pad_size exps), KEC.UNDEFINED)
	     val _ = Array.copy {src=newarray, dst=finalarray, di = ! (#front_index exps)}
	 in
	     KEC.VECTOR {array=ref finalarray, 
			 front_index= ref (!(#front_index exps)),
			 back_index= ref (!(#back_index exps)),
			 front_pad_size = ref(!(#front_pad_size exps)),
			 back_pad_size = ref(!(#back_pad_size exps))}
	 end
       | [KEC.TUPLE exps, func] =>
	 KEC.TUPLE (map (fn (e) => exec (KEC.APPLY {func=func, args=KEC.TUPLE [e]})) exps)
       | [a,b]
	 => raise TypeMismatch ("expected a vector and a function but received " ^ (PrettyPrint.kecexp2nickname a) ^ " and " ^ (PrettyPrint.kecexp2nickname b))
       | _ => raise IncorrectNumberOfArguments {expected=2, actual=(length args)})


fun std_app exec args =
    (case args of
	 [KEC.VECTOR exps, func] 
	 => 
	 let
	     val list = GeneralUtil.listSlice (GeneralUtil.array2list (! (#array exps))) (!(#front_index exps), !(#back_index exps))
	     val list' = map (fn(e) => exec (KEC.APPLY{func=func, args=KEC.TUPLE [e]})) list
	 in
	     KEC.UNIT
	 end
	    
       | [KEC.TUPLE exps, func] 
	 => KEC.UNIT before
	    (app (ignore o (fn (e) => (exec (KEC.APPLY {func=func, args=KEC.TUPLE [e]})))) exps)
       | [a,b]
	 => raise TypeMismatch ("expected a vector and a function but received " ^ (PrettyPrint.kecexp2nickname a) ^ " and " ^ (PrettyPrint.kecexp2nickname b))
       | _ => raise IncorrectNumberOfArguments {expected=2, actual=(length args)})

fun std_slice exec args =
    (case args of
	 [KEC.VECTOR exps, KEC.LITERAL (KEC.CONSTREAL first), KEC.LITERAL (KEC.CONSTREAL last)] 
	 => 
	 let (*TODO: evaluate improve the efficiency of this by copying directly from one array to the other.  WOuld this help or hurt? *)
	     val (first, last) = if first > last then (last, first) else (first, last)
	     val list = GeneralUtil.listSlice (GeneralUtil.array2list (! (#array exps))) (!(#front_index exps) + (Real.floor first) - 1, 
											  !(#front_index exps) + (Real.floor last))

	     val newarray = Array.fromList(list)
	     val finalarray = Array.array(!(#front_pad_size exps) + !(#back_pad_size exps), KEC.UNDEFINED)
	     val _ = Array.copy {src=newarray, dst=finalarray, di = ! (#front_index exps)}
	 in
	     (* TODO: when slicing HUGE arrays, should we reduce the padding size more? *)
	     KEC.VECTOR {array=ref finalarray, 
			 front_index= ref (!(#front_index exps)),
			 back_index= ref (!(#front_index exps) + (Real.floor last) - (Real.floor first) + 1),
			 front_pad_size = ref(!(#front_pad_size exps)),
			 back_pad_size = ref(!(#back_pad_size exps))}
	 end
	    
       | [a,b,c]
	 => raise TypeMismatch ("expected a vector and two numbers but received " ^ (PrettyPrint.kecexp2nickname a) ^ ", " ^ (PrettyPrint.kecexp2nickname b) ^ ", and " ^ (PrettyPrint.kecexp2nickname c))
       | _ => raise IncorrectNumberOfArguments {expected=3, actual=(length args)})

    
fun std_vec_concat exec args =
    (case args of
	 [KEC.VECTOR exps1, KEC.VECTOR exps2] =>
	 ((if ((!(#back_index exps1)) - (! (#front_index exps1)) + (!(#back_index exps2)) - (!(#front_index exps2))) < (!(#front_pad_size exps1) + !(#back_pad_size exps1)) then
	     let
		 val _ = print ("case 1\n")

		  val array' = Array.array (!(#front_pad_size exps1) + !(#back_pad_size exps1), KEC.UNDEFINED)
(*		  val _ = Array.copy{src= !(#array exps1), dst=array', di= !(#front_index exps1)}
		      handle Subscript => ()*)
					  
		  fun copyArray dest source (low, high) count offset =
		      if count >= high then
			  ()
		      else
			  (Array.update(dest, 
					offset+count-low+(!(#front_index exps1)), 
					Array.sub(source, count));
			   copyArray dest source (low, high) (count + 1) offset)

		  val _ = copyArray array' (!(#array exps1)) (!(#front_index exps1), !(#back_index exps1)) (!(#front_index exps1)) 0
		  val _ = copyArray array' (!(#array exps2)) (!(#front_index exps2), !(#back_index exps2)) (!(#front_index exps2)) ((!(#back_index exps1)) - (! (#front_index exps1)))

		  val back_index' = !(#back_index exps1) + (!(#back_index exps2) - !(#front_index exps2))
				    
		  val arg = 
		      {array=ref array',
		       front_index = ref (!(#front_index exps1)),
		       back_index = ref back_index',
		       front_pad_size = ref(!(#front_pad_size exps1)),
		       back_pad_size = ref(!(#back_pad_size exps1))}
	      in
		  KEC.VECTOR arg
	      end

	 else if ((!(#back_index exps1)) - (!(#front_index exps1)) + (!(#back_index exps2)) - (!(#front_index exps2))) < (!(#front_pad_size exps2) + !(#back_pad_size exps2)) then	     
	     let
		 val _ = print ("case 2\n")
		  val array' = Array.array (!(#front_pad_size exps2) + !(#back_pad_size exps2), KEC.UNDEFINED)

					  
		  fun copyArray dest source (low, high) count offset =
		      if count >= high then
			  ()
		      else
			  (Array.update(dest, 
					offset+count-low+(!(#front_index exps1)), 
					Array.sub(source, count));
			   copyArray dest source (low, high) (count + 1) offset)

		  val _ = copyArray array' (!(#array exps1)) (!(#front_index exps1), !(#back_index exps1)) (!(#front_index exps2)) 0
		  val _ = copyArray array' (!(#array exps2)) (!(#front_index exps2), !(#back_index exps2)) (!(#front_index exps2)) ((!(#back_index exps1)) - (! (#front_index exps1)))

		  val back_index' = !(#back_index exps2) - (!(#front_index exps1)) + (!(#back_index exps2) - !(#front_index exps2))
				    
		  val arg = 
		      {array=ref array',
		       front_index = ref (!(#front_index exps2)),
		       back_index = ref back_index',
		       front_pad_size = ref(!(#front_pad_size exps2)),
		       back_pad_size = ref(!(#back_pad_size exps2))}
	      in
		  KEC.VECTOR arg
	      end
	 else
	     let
		 val _ = print ("case 3\n")

		 val front_pad_size = !(#front_pad_size exps1) + !(#front_pad_size exps2)
		 val back_pad_size = !(#back_pad_size exps1) + !(#back_pad_size exps2)
		 val front_index = !(#front_index exps1)
		 val back_index = front_index + (!(#back_index exps1) - !(#front_index exps1)) + (!(#back_index exps2) - !(#front_index exps2))
		 val array' = Array.array (front_pad_size + back_pad_size, KEC.UNDEFINED)

		 fun copyArray dest source (low, high) count offset =
		     if count >= high then
			 ()
		     else
			 (Array.update(dest, 
				       offset+count-low+(front_index), 
				       Array.sub(source, count));
			  copyArray dest source (low, high) (count + 1) offset)
			 
		 val _ = copyArray array' (!(#array exps1)) (!(#front_index exps1), !(#back_index exps1)) (!(#front_index exps1)) 0
		 val _ = copyArray array' (!(#array exps2)) (!(#front_index exps2), !(#back_index exps2)) (!(#front_index exps2)) ((!(#back_index exps1)) - (! (#front_index exps1)))

		 val arg = {array=ref array',
			    front_index = ref front_index,
			    back_index = ref back_index,
			    front_pad_size = ref front_pad_size,
			    back_pad_size = ref back_pad_size}
	     in
		 KEC.VECTOR arg
	     end)
	 handle e => (error "Exception caught in vector concat library call"; print ("stackTrace: " ^ (String.concatWith "\n  " (MLton.Exn.history e))); KEC.UNIT))

       | [a,b]
	 => raise TypeMismatch ("expected 2 vectors but received " ^ (PrettyPrint.kecexp2nickname a) ^ " and " ^ (PrettyPrint.kecexp2nickname b))
       | _ => raise IncorrectNumberOfArguments {expected=2, actual=(length args)})


val library = [{name="index", operation=std_index},
	       {name="push_front", operation=std_push_front},
	       {name="push_back", operation=std_push_back},
	       {name="map", operation=std_map},
	       {name="app", operation=std_app},
	       {name="vecconcat", operation=std_vec_concat},
	       {name="slice", operation=std_slice},
	       {name="tabulate", operation=std_tabulate}]

end
