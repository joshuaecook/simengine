(* Copyright (C) 2010 by Simatra Modeling Technologies, L.L.C. *)

structure ClassSyntax: sig
(* Serialization and deserialization for DOF class data. *)

val toJSON: DOF.class -> JSON.json
(* TODO implement fromJSON *)

end = struct

open JSON
open JSONExtensions

val int = int o IntInf.fromInt

fun symbol s = JSON.object [("$symbol", JSON.string (Symbol.name s))]

fun toJSON (class as {name, properties, inputs, outputs, iterators, exps}) =
    object [("name", symbol name),
	    ("properties", propertiesToJSON properties),
	    ("inputs", array (map inputToJSON (! inputs))),
	    ("outputs", array (map outputToJSON (! outputs))),
	    ("iterators", array (map iteratorToJSON iterators)),
	    ("expressions", array (map ExpSyntax.toJSON (! exps)))]

and inputToJSON input =
    object [("name", ExpSyntax.termToJSON (DOF.Input.name input)),
	    ("default", JSONOption (ExpSyntax.toJSON, (DOF.Input.default input)))]

and outputToJSON output =
    object [("name", ExpSyntax.termToJSON (DOF.Output.name output)),
	    ("contents", array (map ExpSyntax.toJSON (DOF.Output.contents output))),
	    ("condition", ExpSyntax.toJSON (DOF.Output.condition output))]

and iteratorToJSON {name, low, step, high} =
    object [("name", symbol name),
	    ("low", real low),
	    ("step", real step),
	    ("high", real step)]

and propertiesToJSON {basename, preshardname, classform, classtype, sourcepos} =
    object [("baseName", symbol basename),
	    ("classForm", classFormToJSON classform),
	    ("preShardName", symbol preshardname),
	    ("classType", classTypeToJSON classtype),
	    ("sourcePosition", PosLog.toJSON sourcepos)]

and classFormToJSON DOF.FUNCTIONAL = JSONType ("DOF.FUNCTIONAL")
  | classFormToJSON (DOF.INSTANTIATION {readstates, writestates}) =
    JSONTypedObject ("DOF.INSTANTIATION",
		     object [("readStates", array (map symbol readstates)),
			     ("writeStates", array (map symbol writestates))])

and classTypeToJSON (DOF.MASTER (*name*)) =
    JSONTypedObject ("DOF.MASTER", symbol (*name*)(Symbol.symbol "-"))
  | classTypeToJSON (DOF.SLAVE name) = 
    JSONTypedObject ("DOF.SLAVE", symbol name)

end
