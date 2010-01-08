functor TestParseJSON(P: PARSE_JSON) = struct

fun bug why = raise Fail why

val value = P.parseFile "build-options.json"

val _ = if JSON.isObject value then ()
	else bug ("Expected value to be a JSON object")

val _ = case JSON.member (value, "buildRevision")
	 of SOME rev => 
	    (case JSON.toInt rev
	      of SOME 769 => ()
	       | SOME z => bug ("Expected buildRevision to be 769 but got " ^ (IntInf.toString z))
	       | NONE => bug ("Expected buildRevision to be a string"))
	  | NONE => bug ("Expected build options to have a buildRevision")

val _ = case JSON.member (value, "version")
	 of SOME ver =>
	    (case JSON.toString ver
	      of SOME "0.92" => ()
	       | SOME s => bug ("Expected version to be 0.92 but got " ^ s)
	       | NONE => bug ("Expected version to be a string"))
	  | NONE => bug ("Expected build options to have a version")

val _ = case JSON.member (value, "backends")
	 of SOME bes =>
	    (case JSON.elements bes
	      of SOME nil => bug ("Expected backends to be nonempty")
	       | SOME elems =>
		 (case (List.length elems, JSON.toString (List.hd elems))
		   of (3, SOME "software") => ()
		    | _ => bug ("Unexpected value in backends"))
	       | NONE => bug ("Expected backends to be an array"))
	  | NONE => bug ("Expected build options to have a list of backends")


end

structure TestParseJSON = TestParseJSON(ParseJSON)
