structure Env =
struct

fun new () = SymbolTable.empty

val union  = SymbolTable.priorityunion

fun system_union ((global, localenv, poslog), env) =
    let
(*	val _ = print "____ Entering system union\n"
	val _ = print ("Closure contains " ^ (String.concatWith ", " (SymbolTable.listKeys env)) ^ "\n")
	val _ = print ("localenv contains " ^ (String.concatWith " || " (map (fn(le) => String.concatWith ", " (SymbolTable.listKeys (le))) localenv)) ^ "\n")
*)
	val (first, restUnion, _) = localenv
    in
    (global, 
     (union(env, first), restUnion, ref NONE),
     poslog)
    end

fun closure_union ((global, localenv, poslog), closure) =
    let
(*	val _ = print "____ Entering closure union\n"
	val _ = print ("Closure contains " ^ (String.concatWith ", " (map Symbol.name (SymbolTable.listKeys closure))) ^ "\n")
	val _ = case localenv of
		    SOME localenv=> print ("localenv contains " ^ (String.concatWith ", " (map (Symbol.name) (SymbolTable.listKeys (localenv)))) ^ "\n")
		  | NONE => print "empty localenv\n"
*)
(*	val _ = print ("length localenv = " ^ (Int.toString(length localenv)) ^ "\n") *)
	val (first, restUnion, cacheUnion) = localenv

	val localenvs = case !cacheUnion of
			    SOME env => env
			  | NONE => let val e = union(first, restUnion) 
				    in e before cacheUnion := SOME e 
				    end
	val closure' = union(closure, localenvs)      

(*	val _ = print ("length of closure = " ^ (Int.toString(length (SymbolTable.listKeys closure'))) ^ "\n")*)
    in
	closure'
    end

fun add ((sym,value),env) = 
    SymbolTable.enter (env, sym, value)


fun global_add ((sym,value),(globalenv, localenv, poslog)) = 
    let
	val globalenv' = SymbolTable.enter (!globalenv, sym, value)
	val _ = globalenv := globalenv'
    in
	(globalenv, localenv, poslog)
    end	

fun local_add printer ((sym,value), env as (globalenv, localenv, poslog)) =
    let
(*	val _ = print ("     ---  in local add adding " ^ (sym) ^ " with value "^(printer value)^"\n")
	val _ = print ("          value of it was " ^ (case SymbolTable.look (hd localenv, sym) of
							   SOME x => (printer x)
							 | NONE => "NONEXISTENT") ^ "\n")*)
(*	val _ = print ("         depth = " ^ (Int.toString (length localenv)) ^ "\n")*)
	val (first, restUnion, _) = localenv
    in
	(globalenv, (SymbolTable.enter (first, sym, value), restUnion, ref NONE), poslog)
    end


fun create_local ((global, localenv, poslog)) =
    (global, 
     localenv, 
     poslog)

fun push_local ((global, localenv, poslog)) =
    let
	val (first, restUnion, _) = localenv
	val restUnion' = union(first, restUnion)
    in
	(global, 
	 (new(), restUnion', ref (SOME restUnion')), 
	 poslog)(* before print "pushed local\n"*)
    end

fun system2local (_, localenv, _) = localenv
fun system2global (globalenv, _, _) = globalenv

fun replace_local ((global, _, poslog), localenv) = (global, (localenv, nil, new()), poslog)

fun lookup env sym = 
    SymbolTable.look(env, sym)
(*
fun local_lookup printer nil sym = NONE
  | local_lookup printer (localenv::rest) sym = 
    case lookup localenv sym of
	SOME x => SOME x (*before print ("     ===  in local_lookup and found " ^ sym ^ " with value "^(printer x)^"\n")*)
      | NONE => (*print "     ~~~~ looping down a level\n";*) local_lookup printer rest sym
*)
fun system_lookup printer (globalenv, localenv as (first, restUnion, _), poslog) sym =
    case lookup first sym of
	SOME x => SOME x
      | NONE => (case lookup restUnion sym of
		     SOME x => SOME x
		   | NONE => lookup (!globalenv) sym)

fun top_local_lookup printer (globalenv, localenv as (first, rest, _), poslog) sym =
    lookup first sym

type 'a env = 'a SymbolTable.table


fun top_local_keys env =
    let
	val (first, _, _) = system2local env
    in
	SymbolTable.listKeys (first)
    end

(* DEBUG only functions *)
fun keys env =
    SymbolTable.listKeys env

fun show_env (globalenv, localenv as (first, restUnion, _), _) =
    (print ("Global env: " ^ (String.concatWith ", " (map Symbol.name (SymbolTable.listKeys (!globalenv)))) ^ "\n");
     print ("Local env: " ^ (String.concatWith ", " (map Symbol.name (SymbolTable.listKeys first))) ^ "\n");
     print ("           " ^ (String.concatWith ", " (map Symbol.name (SymbolTable.listKeys restUnion))) ^ "\n"))

end
