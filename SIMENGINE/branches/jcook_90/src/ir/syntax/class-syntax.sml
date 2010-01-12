structure ClassSyntax: sig

val toJSON: DOF.class -> JSON.json

end = struct

open JSON
val int = int o IntInf.fromInt


fun toJSON (class as {name, properties, inputs, outputs, iterators, exps}) =
    object [(* ("name", JSONSymbol name), *)
	    ("properties", string "FIXME"),
	    ("inputs", array (map inputToJSON (! inputs))),
	    ("outputs", array (map outputToJSON (! outputs))),
	    (* ("iterators", array (map iteratorToJSON iterators)), *)
	    ("expressions", array (map ExpSyntax.toJSON (! exps)))]

and inputToJSON {name, default} =
    object [("name", ExpSyntax.termToJSON name),
	    ("default", JSONOption (ExpSyntax.toJSON, default))]

and outputToJSON {name, contents, condition} =
    object [("name", ExpSyntax.termToJSON name),
	    ("contents", array (map ExpSyntax.toJSON contents)),
	    ("condition", ExpSyntax.toJSON condition)]

and iteratorToJSON {name, low, step, high} =
    object [(* ("name", JSONSymbol name), *)
	    ("low", real low),
	    ("step", real step),
	    ("high", real step)]

end
