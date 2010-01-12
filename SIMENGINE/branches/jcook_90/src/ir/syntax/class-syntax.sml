structure ClassSyntax: sig

val toJSON: DOF.class -> JSON.json

end = struct

open JSON
val int = int o IntInf.fromInt

fun symbol s = JSON.object [("$symbol", JSON.string (Symbol.name s))]

fun toJSON (class as {name, properties, inputs, outputs, iterators, exps}) =
    object [("name", symbol name),
	    ("properties", propertiesToJSON properties),
	    ("inputs", array (map inputToJSON (! inputs))),
	    ("outputs", array (map outputToJSON (! outputs))),
	    ("iterators", array (map iteratorToJSON iterators)),
	    ("expressions", array (map ExpSyntax.toJSON (! exps)))]

and inputToJSON {name, default} =
    object [("name", ExpSyntax.termToJSON name),
	    ("default", JSONOption (ExpSyntax.toJSON, default))]

and outputToJSON {name, contents, condition} =
    object [("name", ExpSyntax.termToJSON name),
	    ("contents", array (map ExpSyntax.toJSON contents)),
	    ("condition", ExpSyntax.toJSON condition)]

and iteratorToJSON {name, low, step, high} =
    object [("name", symbol name),
	    ("low", real low),
	    ("step", real step),
	    ("high", real step)]

and propertiesToJSON {basename, classform, classtype, sourcepos} =
    object [("baseName", symbol basename),
	    ("classForm", classFormToJSON classform),
	    ("classType", classTypeToJSON classtype),
	    ("sourcePosition", PosLog.toJSON sourcepos)]

and classFormToJSON DOF.FUNCTIONAL = JSONType ("DOF.FUNCTIONAL")
  | classFormToJSON (DOF.INSTANTIATION {readstates, writestates}) =
    JSONTypedObject ("DOF.INSTANTIATION",
		     object [("readStates", array (map symbol readstates)),
			     ("writeStates", array (map symbol writestates))])

and classTypeToJSON (DOF.MASTER name) =
    JSONTypedObject ("DOF.MASTER", symbol name)
  | classTypeToJSON (DOF.SLAVE name) = 
    JSONTypedObject ("DOF.SLAVE", symbol name)

end
