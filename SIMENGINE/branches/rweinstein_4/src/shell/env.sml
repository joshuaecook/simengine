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
	val (first, restUnion) = localenv
    in
    (global, 
     (union(env, first), restUnion),
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
	val (first, restUnion) = localenv
    in
	union(closure, union(first, restUnion))      
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
	val (first, restUnion) = localenv
    in
	(globalenv, (SymbolTable.enter (first, sym, value), restUnion), poslog)
    end


fun create_local ((global, localenv, poslog)) =
    (global, 
     localenv, 
     poslog)

fun push_local ((global, localenv, poslog)) =
    let
	val (first, restUnion) = localenv
    in
	(global, 
	 (new(), union(first, restUnion)), 
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
fun system_lookup printer (globalenv, localenv as (first, restUnion), poslog) sym =
    case lookup first sym of
	SOME x => SOME x
      | NONE => (case lookup restUnion sym of
		     SOME x => SOME x
		   | NONE => lookup (!globalenv) sym)

type 'a env = 'a SymbolTable.table


fun top_local_keys env =
    let
	val (first, _) = system2local env
    in
	SymbolTable.listKeys (first)
    end

(* DEBUG only functions *)
fun keys env =
    SymbolTable.listKeys env

fun show_env (globalenv, localenv, _) =
    (print ("Global env: " ^ (String.concatWith ", " (map Symbol.name (SymbolTable.listKeys (!globalenv)))) ^ "\n");
     print ("Local env: " ^ (String.concatWith ", " (map Symbol.name (SymbolTable.listKeys localenv))) ^ "\n"))

end
