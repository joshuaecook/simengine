structure PopulatedEnv =
struct

datatype def = DIRECTDEF of KEC.stm
	     | CODEDEF of string

fun defglobal name definition =
    DIRECTDEF (KEC.DEFINITION (KEC.DEFGLOBAL (KEC.REPLACE, Symbol.symbol name, KEC.DONTCARE, definition), 
			       PosLog.NOPOS))
        
fun code code =
    CODEDEF ((String.concatWith "\n" code) ^ "\n")

fun import filename =
    DIRECTDEF (KEC.ACTION (KEC.IMPORT filename, PosLog.NOPOS))

fun new (parse) =
    let
	val initial_defs =
	    defglobal "Object" (Objects.classdef "Object" NONE
						 [(KEC.REPLACE, 
						   Objects.method "tostring" nil
								  (Objects.send "name" (KEC.SYMBOL (Symbol.symbol "class")))),
						  (KEC.REPLACE,
						   Objects.method "()" [(Symbol.symbol "table", KEC.TYPE (Symbol.symbol "Table"))]
								  (KEC.APPLY {func=KEC.SYMBOL(Symbol.symbol "initObjWithTable"),
									      args=KEC.TUPLE [KEC.SYMBOL(Symbol.symbol "self"),
											      KEC.SYMBOL(Symbol.symbol "table")]}))]) :: 
	    (map import ["stdlib.dsl", "simulation.dsl", "rules.dsl", "dependency.dsl", "devices.dsl", "startup.dsl"]) @
	    (map (fn(stm) => DIRECTDEF stm) (ShellOptions.getOptionsInit()))

	fun process_def (DIRECTDEF stm, env) =
	    let
		val _ = Globals.eof_encountered := false
		val (env, _) =
		    Exec.run parse env [stm]
	    in
		env
	    end
	  | process_def (CODEDEF code, env) =
	    let 
		val _ = Globals.eof_encountered := false
		val (_, env) = parse (TextIO.openString code) env
	    in
		env
	    end
	    
	val env = foldl process_def (ref (Env.new()), (Env.new(), Env.new()), PosLog.new()) initial_defs
    in
	env
	before Globals.base_env := env
	before Globals.eof_encountered := false
	before Globals.core_init := false
    end

end
