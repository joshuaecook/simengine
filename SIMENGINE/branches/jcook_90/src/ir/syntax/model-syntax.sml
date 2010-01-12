structure ModelSyntax: sig

val toJSON: DOF.model -> JSON.json

end = struct

open JSON
val int = int o IntInf.fromInt

fun toJSON (classes, instance as {name, classname}, properties) =
    object [("classes", array (map ClassSyntax.toJSON classes)),
	    ("instance", object [(* ("name", JSONOption (JSONSymbol, name)), *)
				 (* ("classname", JSONSymbol classname) *)]),
	    ("properties", string "FIXME")]

end
