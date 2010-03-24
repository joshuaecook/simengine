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

fun toVector object =
    KEC.APPLY {func = KEC.SEND {message = Symbol.symbol "tovector",
				object = object},
	       args = KEC.UNIT}


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
		then raise TypeMismatch ("Unable to encode objects of type other than Table.")
		else
		    let val contents = exec (KEC.SEND {message = Symbol.symbol "contents", object = object})
		    in
			JSON.object (encodedTableContents contents)
		    end
	      | object =>
		raise TypeMismatch ("Unable to encode " ^ (PrettyPrint.kecexp2nickname object))

	and encodedTableContents contents =
	    case contents
	     of KEC.VECTOR vec =>
		let val (keys, values) = ListPair.unzip (map vectorToPair (vectorToList vec))
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
    let
	fun decoded object =
	    case JSON.jsType object
	     of JSON.JS_NULL => KEC.UNIT
	      | JSON.JS_TRUE => KEC.LITERAL (KEC.CONSTBOOL true)
	      | JSON.JS_FALSE => KEC.LITERAL (KEC.CONSTBOOL false)
	      | JSON.JS_STRING => KEC.LITERAL (KEC.CONSTSTR (JSON.stringVal object))
	      | JSON.JS_NUMBER =>
		if isSome (JSON.toInt object) 
		then KEC.LITERAL (KEC.CONSTREAL (Real.fromInt (IntInf.toInt (JSON.intVal object))))
		else if isSome (JSON.toReal object) 
		then KEC.LITERAL (KEC.CONSTREAL (JSON.realVal object))
		else raise TypeMismatch ("Unknown type of JSON value")
	      | JSON.JS_ARRAY => 
		let val elements = (map decoded (valOf (JSON.elements object)))
		in
		    toVector (KEC.TUPLE elements)
		end
	      | JSON.JS_OBJECT => 
		KEC.APPLY {func = KEC.SEND {message = Symbol.symbol "new",
					    object = KEC.SYMBOL (Symbol.symbol "Table")},
			   args = KEC.TUPLE [decodedObjectMembers (valOf (JSON.members object))]}

	and decodedObjectMembers members =
	    let val entries = map (fn (key, value) => KEC.TUPLE [KEC.LITERAL (KEC.CONSTSTR key), decoded value]) members
	    in
		toVector (KEC.TUPLE entries)
	    end
    in
     fn [KEC.LITERAL (KEC.CONSTSTR json)] => exec (decoded (ParseJSON.parseString json))
      | [a] => raise TypeMismatch ("expected a String but received " ^ (PrettyPrint.kecexp2nickname a))
      | args => raise IncorrectNumberOfArguments {expected = 1, actual = length args}
    end

val library = [{name="jsonEncode", operation=encode},
	       {name="jsonDecode", operation=decode}]

end
