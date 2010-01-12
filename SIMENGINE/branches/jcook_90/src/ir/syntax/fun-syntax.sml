structure FunSyntax: sig

val toJSON: Fun.funtype -> JSON.json

end = struct

open JSON

fun toJSON (Fun.BUILTIN operation) =
    JSONTypedObject ("FUN.BUILTIN",
		     string (#name (FunProps.op2props operation)))
  | toJSON (Fun.INST {classname, instname, props}) =
    JSONTypedObject ("FUN.BUILTIN",
		     object [("classname", JSONSymbol classname),
			     ("instname", JSONSymbol instname),
			     ("properties", string "FIXME")])

end
