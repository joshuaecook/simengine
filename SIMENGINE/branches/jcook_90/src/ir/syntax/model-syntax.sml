(* Copyright (C) 2010 by Simatra Modeling Technologies, L.L.C. *)

structure ModelSyntax: sig
(* Serialization and deserialization for DOF model data. *)

val toJSON: DOF.model -> JSON.json
val iteratorToJSON: DOF.systemiterator -> JSON.json
(* TODO implement fromJSON *)

end = struct

open JSON
val int = int o IntInf.fromInt
fun symbol s = JSON.object [("$symbol", JSON.string (Symbol.name s))]

fun toJSON (classes, instance as {name, classname}, properties) =
    object [("classes", array (map ClassSyntax.toJSON classes)),
	    ("instance", object [("name", JSONOption (symbol, name)),
				 ("classname", symbol classname)]),
	    ("properties", propertiesToJSON properties)]

and propertiesToJSON {debug, iterators, num_models, precision, profile, target} =
    object [("debug", bool debug),
	    ("iterators", array (map iteratorToJSON iterators)),
	    ("numModels", int num_models),
	    ("precision", JSONType (case precision of DOF.SINGLE => "DOF.SINGLE" | DOF.DOUBLE => "DOF.DOUBLE")),
	    ("profile", bool profile),
	    ("target", targetToJSON target)]

and iteratorToJSON (name, domain) =
    object [("name", symbol name),
	    ("domain", (case domain
			 of DOF.CONTINUOUS solver => 
			    JSONTypedObject ("DOF.CONTINUOUS", 
					     string (Solver.solver2name solver))
			  | DOF.DISCRETE {sample_period} => 
			    JSONTypedObject ("DOF.DISCRETE",
					     object [("samplePeriod", real sample_period)])
			  | DOF.UPDATE parent => 
			    JSONTypedObject ("DOF.UPDATE", symbol parent)
			  | DOF.POSTPROCESS parent => 
			    JSONTypedObject ("DOF.POSTPROCESS", symbol parent)
			  | DOF.IMMEDIATE => 
			    JSONType ("DOF.IMMEDIATE")))]

and targetToJSON (Target.CPU) = JSONType ("Target.CPU")
  | targetToJSON (Target.OPENMP) = JSONType ("Target.OPENMP")
  | targetToJSON (Target.CUDA) = JSONType ("Target.CUDA")

end
