(* Copyright (C) 2010 by Simatra Modeling Technologies, L.L.C. *)

structure JSONLib =
struct

val TypeMismatch = DynException.TypeMismatch
and IncorrectNumberOfArguments = DynException.IncorrectNumberOfArguments
and ValueError = DynException.ValueError

fun vectorToList {array as ref items, front_index as ref front, back_index as ref back, front_pad_size, back_pad_size} =
    let val slice = ArraySlice.slice (items, front, SOME (back - front))
    in
	Vector.foldr (op ::) [] (ArraySlice.vector slice)
    end

fun vectorToPair (KEC.VECTOR vec) =
    (case vectorToList vec
      of [a, b] => (a, b)
       | list => raise ValueError ("expected a Vector of length 2 but received a Vector of length " ^ (Int.toString (List.length list))))
  | vectorToPair object =
    raise TypeMismatch ("expected a Vector but received " ^ (PrettyPrint.kecexp2nickname object))

val expToString =
 fn KEC.LITERAL (KEC.CONSTSTR s) => s
  | object => raise TypeMismatch ("expected a String but received " ^ (PrettyPrint.kecexp2nickname object))


fun encode exec =
    let
	(* This will not be a perfectly symmetrical encoding, 
	 * since both UNIT and UNDEFINED are encoded as null,
	 * and both TUPLE and VECTOR are encoded as array. *)
	fun encoded object =
	    case object 
	     of KEC.LITERAL (KEC.CONSTREAL r) => JSON.real r
	      | KEC.LITERAL (KEC.CONSTBOOL b) => JSON.bool b
	      | KEC.LITERAL (KEC.CONSTSTR s) => JSON.string s
	      | KEC.UNIT => JSON.null
	      | KEC.UNDEFINED => JSON.null
	      | KEC.TUPLE exps => JSON.array (map encoded exps)
	      | KEC.VECTOR vec => JSON.array (map encoded (vectorToList vec))
	      | KEC.OBJECT _ =>
		if not (TypeLib.check exec (KEC.TYPE (Symbol.symbol "Table"), object))
		then raise TypeMismatch ("unable to encode " ^ (PrettyPrint.kecexp2nickname object))
		else
		    let val contents = exec (KEC.SEND {message = Symbol.symbol "contents", object = object})
		    in
			JSON.object (encodedTableContents contents)
		    end
	      | object =>
		raise TypeMismatch ("unable to encode " ^ (PrettyPrint.kecexp2nickname object))

	and encodedTableContents contents =
	    case contents
	     of KEC.VECTOR vec =>
		let 
		    val (keys, values) = ListPair.unzip (map vectorToPair (vectorToList vec))
		in
		    ListPair.zip (map expToString keys, map encoded values)
		end
	      | _ => 
		raise TypeMismatch ("expected Table contents to be a Vector but received " ^ (PrettyPrint.kecexp2nickname contents))
	    
    in
     fn [object] => KEC.LITERAL (KEC.CONSTSTR (PrintJSON.toString (encoded object)))
      | args => raise IncorrectNumberOfArguments {expected = 1, actual = length args}
    end

fun decode exec =
 fn (* [KEC.LITERAL (KEC.CONSTSTR json)] => decoded (ParseJSON.parseString json) *)
  (* |  *)[a] => raise TypeMismatch ("expected a String but received " ^ (PrettyPrint.kecexp2nickname a))
  | args => raise IncorrectNumberOfArguments {expected = 1, actual = length args}


val library = [{name="jsonEncode", operation=encode},
	       {name="jsonDecode", operation=decode}]

end
