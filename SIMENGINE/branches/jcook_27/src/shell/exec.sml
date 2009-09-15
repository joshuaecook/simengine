(* Copyright Simatra - 2006 - 2008
 * exec.sml - Object Oriented Lambda Calculus execution engine
 *
 *)

structure Exec =
struct

(* TODO: replace with usage of global settable by user *)
(*       replace exception with reasonable error report *)
val max_depth = 10000
exception MaxStackDepthExceeded

exception UnexpectedMethodType

open Printer

fun log_error (env as (_, _, poslog)) text =
    (Logger.log_usererror poslog text;
     DynException.setErrored())

fun error (env as (_, _, poslog)) text = 
    (log_error env text;
     raise DynException.RestartRepl)

fun decell (KEC.CELL (_,KEC.REFERENCE c)) = decell (!c)
  | decell (KEC.CELL (_,KEC.GETSET (g,_))) = decell(g())
  | decell exp = exp

(* Returns a SYMBOL expression with the given name. *)
fun sym s = KEC.SYMBOL (Symbol.symbol s)
	    
(* Returns an object instance method or attribute. *)
fun send name object =    
    KEC.SEND {message=Symbol.symbol name, object=object}

(* Returns a function application. *)
fun apply (f, a) = KEC.APPLY {func=f, args=KEC.TUPLE a}

(* Returns a library function invocation. *)
fun libfun name args =
    KEC.LIBFUN (Symbol.symbol name, KEC.TUPLE args)

(* Returns an application of an object instance method to the given arguments. *)
fun applySend message object args = 
    case args of
	SOME args' => apply (send message object, args')
      | NONE => apply (send message object, [])


fun merge_runnables env (original_exp, new_exp) =
    case (original_exp, new_exp) of
	(KEC.RUNNABLE (functions1), KEC.RUNNABLE (functions2))
	=> KEC.RUNNABLE ((functions2 @ functions1))
      | (_, KEC.RUNNABLE _) 
	=> (error env ($("Cannot overload " ^ (PrettyPrint.kecexp2nickname original_exp)));
	    original_exp)
      | _
	=> (error env ($("Overload attempted with non-function"));
	    original_exp)

(* Executes a single expression in a given environment.
   Returns an expression.
   Literals, type expressions, object instances, cells, unit, and undefined are returned as-is.
   Symbols are looked up in the given environment and executed recursively. (It is an error to attempt to execute a symbol that has not been defined.)

   TODO: fill in doc for other types of expressions.
 *)
fun exec_exp parse (depth_count, isLHS) (env(*: (KEC.exp Env.env ref * KEC.exp Env.env list * PosLog.pos list)*), exp) =
    if depth_count > max_depth then
	raise MaxStackDepthExceeded
    else
	let 
	    fun exec exp = exec_exp parse (1 + depth_count, isLHS) (env, exp)
	    val pretty = PrettyPrint.kecexp2prettystr (decell o exec)
 	in
	    case exp of
		 KEC.LITERAL _ => exp
	       | KEC.OBJECT _ => exp
	       | KEC.UNIT => exp
	       | KEC.UNDEFINED => exp

	       | KEC.CELL _ => exp
	       | KEC.DEREF (KEC.CELL (_,KEC.REFERENCE c)) => (! c)
	       | KEC.DEREF (KEC.CELL (_,KEC.GETSET (g,s))) => g()
	       | KEC.DEREF exp
		 => error env ($("Attempt to dereference non-cell " ^ (pretty exp)))

	       | KEC.TYPEEXP _ => exp

	       | KEC.SYMBOL sym
		 => (execSymbol exec env sym
		     handle DynException.NameError _
			    => error env ($("Unknown identifier encountered: " ^ (Symbol.name sym))))

	       | KEC.TUPLE exps 
		 => KEC.TUPLE (map (decell o exec) exps)

	       | KEC.VECTOR exps 
		 => KEC.VECTOR (exps)
		    before Array.appi (fn (i,x) => Array.update(!(#array exps), i, decell (exec (x)))) 
				      (!(#array exps))
	       (*TODO: IS THIS NEEDED?*)

	       | KEC.MAKEREF (tp, e)
		 => KEC.CELL (tp, KEC.REFERENCE(ref (exec e)))

	       | KEC.LIBFUN (name, arg)
		 => (execLibraryFunction (decell o exec) env name (decell (exec arg))
		     handle DynException.IncorrectNumberOfArguments {expected, actual}
			    => error env ($("Incorrect number of arguments for " ^ (Symbol.name name) ^ "; received " ^ (Int.toString actual) ^ " and expected " ^ (Int.toString expected)))
			  | DynException.TypeMismatch reason
			    => error env ($("Type mismatch in " ^ (Symbol.name name) ^ ": " ^ reason)))
			       
	       | KEC.LAMBDA {args, body, closure, undefined_args_allowed}
		 => KEC.LAMBDA {args=args, body=body, closure = Env.closure_union (env, closure), undefined_args_allowed=undefined_args_allowed}

		     

	       | KEC.PROPERTYEXP {name, read, write, expansionAllowed}
		 => 
		 let
		     fun inject {name, args, return, stms, closure} =
			 {closure=Env.closure_union (env, closure), name=name, args=args, return=return, stms=stms}
		 in		     
		     if isLHS orelse not expansionAllowed then
			 KEC.PROPERTYEXP {name=name,
					  expansionAllowed=false,
					  read=GeneralUtil.mapOpt inject read,
					  write=GeneralUtil.mapOpt inject write}
		     else
			 case read of
			     SOME reader =>
			     exec (KEC.APPLY {func=KEC.RUNNABLE(map inject reader), args=KEC.UNIT})
			   | NONE => error env ($("Attempt to read non-readable property " ^ (Symbol.name name)))
		 end


	       | KEC.RUNNABLE (functions)
		 => KEC.RUNNABLE ((map (fn{name, args, return, stms, closure} => 
					  {name=name, 
					   args=args,
					   return=return,
					   stms=stms,
					   closure=Env.closure_union (env, closure)}) functions))

	       | KEC.APPLY {func, args}
		 => (execApply false parse (depth_count,isLHS) env func args
		     handle DynException.IncorrectNumberOfArguments {expected, actual}
			    => error env ($("Incorrect number of arguments for " ^ (pretty func) ^ "; received " ^ (Int.toString actual) ^ " and expected " ^ (Int.toString expected)))
			  | DynException.ValueError reason
			   => error env ($("ValueError: " ^ reason))
			  | DynException.TypeMismatch reason
			    => error env ($("Type mismatch: " ^ reason)))

	       | KEC.IFEXP {cond, ift, iff}
		 => (execConditional (decell o exec) env cond ift iff
		     handle DynException.TypeMismatch reason
			    => error env ($("Type mismatch: " ^ reason)))

	       | KEC.SEND {message, object}
		 => (Send.send isLHS exec env message object
		     handle DynException.TypeMismatch reason
			    => error env ($("Type mismatch: " ^ reason))
			  | DynException.ValueError reason
			   => error env ($("ValueError: " ^ reason))
			  | DynException.NameError name
			    => error env ($("Member " ^ name ^ " not found in object " ^ (pretty object))))

	       | KEC.ERROR message
		 => (case message of 
			 KEC.LITERAL (KEC.CONSTSTR s) => error env ($s)
		       | exp => 
			 let
			     val message' = applySend "tostring" message NONE
			 in
			     case exec message' of
				 KEC.LITERAL (KEC.CONSTSTR s) => error env ($s)
			       | exp => error env ($("Error expects a string, instead received " ^ (pretty exp)))
			 end)

	       | KEC.POS (exp, pos) 
		 => exec_exp parse (depth_count+1, isLHS) (PosLog.addSystemPos (env, pos), exp)

	       | KEC.STMS stms
		 => 	    
		 let
		     fun process_stm (stm, (_,env))=
			 exec_stm parse (stm, env)

		     val (retval, env) = foldl process_stm (KEC.UNIT, env) stms
		 in
		     retval
		 end

	       | KEC.NAMESPACEDEF {name, stms}
		 => (execNamespaceDef parse (depth_count, isLHS) env name stms
		     handle DynException.NameError n
			    => error env ($("Unknown identifier encountered: " ^ n))
			  | DynException.TypeMismatch reason
			    => error env ($("Type mismatch: " ^ reason)))

	       | KEC.CLASSDEF {name, members, parent}
		 => (execClassDef exec env name members parent
		     handle DynException.TypeMismatch reason
			    => error env ($("Type mismatch: " ^ reason)))

	       | KEC.SATISFIES {class, interface}
		 => KEC.UNIT
	end

(* Executes a single statement in a given environment. *)
and exec_stm parse (stm, env as (globalenv, localenv, poslog)) =
    case stm of
	KEC.DEFINITION(KEC.DEFGLOBAL (replacement, name, typepattern, exp), pos)
	=> (execGlobalDefinition parse env (replacement, name, typepattern, exp) pos
	    handle DynException.TypeMismatch reason
		   => error (PosLog.addSystemPos (env, pos)) ($("Type mismatch: " ^ reason))
		 | DynException.NameError name
		   => error (PosLog.addSystemPos (env, pos)) ($("Unknown identifier encountered: " ^ name)))

      | KEC.DEFINITION(KEC.DEFLOCAL (replacement, name, typepattern, exp), pos)
	=> (execLocalDefinition parse env (replacement, name, typepattern, exp) pos
	    handle DynException.TypeMismatch reason
		   => error (PosLog.addSystemPos (env, pos)) ($("Type mismatch: " ^ reason))
		 | DynException.NameError name
		   => error (PosLog.addSystemPos (env, pos)) ($("Unknown identifier encountered: " ^ name)))

      | KEC.DEFINITION(KEC.DEFCONST (replacement, name, typepattern, exp), pos)
	=> (execConstantDefinition parse env (replacement, name, typepattern, exp) pos
	    handle DynException.TypeMismatch reason
		   => error (PosLog.addSystemPos (env, pos)) ($("Type mismatch: " ^ reason))
		 | DynException.NameError name
		   => error (PosLog.addSystemPos (env, pos)) ($("Constant " ^ name ^ " is already defined.")))

      | KEC.ACTION(KEC.OPEN {obj, excludes, include_privates}, pos)
	=> (execOpenStatement parse env (obj, excludes, include_privates) pos
	    handle DynException.TypeMismatch reason
		   => error (PosLog.addSystemPos (env, pos)) ($("Type mismatch: " ^ reason)))
			      
      | KEC.ACTION(KEC.IMPORT file, pos)
	=> (execImportStatement parse env file pos
	    handle DynException.ImportError (_, paths)
		   => (error (PosLog.addSystemPos (env, pos)) 
			     ($("Cannot import file " ^ file ^ " from paths (" ^ (String.concatWith "," paths) ^ ")")))
		 | DynException.TypeMismatch reason
		   => error (PosLog.addSystemPos (env, pos)) ($("Type mismatch: " ^ reason))
		 | DynException.ValueError reason
		   => error (PosLog.addSystemPos (env, pos)) ($("ValueError: " ^ reason))
		 | DynException.NameError name
		   => error (PosLog.addSystemPos (env, pos)) ($("Unknown identifier encountered: " ^ name)))

	   
      | KEC.ACTION(KEC.ASSIGN (dest, source), pos)
	=> (execAssignStatment parse env (dest, source) pos
	    handle DynException.TypeMismatch reason
		   => error (PosLog.addSystemPos (env, pos)) ($("Type mismatch: " ^ reason)))			       

      | KEC.ACTION(KEC.EXP exp, pos)
	=> 
	let
	    fun exec exp = exec_exp parse (0, false) (PosLog.addSystemPos (env, pos), exp)
	in
	    (exec exp, env)
	end

(* Executes a symbolic identifier.
   Returns the value associated with the symbol in the current environment. *)
and execSymbol exec env s =
    case Env.system_lookup (PrettyPrint.kecexp2prettystr (decell o exec)) env s of
	SOME exp => exec exp
      | NONE => 
	(case Symbol.name s of
	     (* Special-case "isdefined" so that it can handle an undefined argument. *)
	     "isdefined"
	     => KEC.LAMBDA{args=[Symbol.symbol "exp"], 
			   body=(libfun "isdefined" [sym "exp"]),
			   undefined_args_allowed=true, closure=Env.new()}
	   | _ => raise DynException.NameError (Symbol.name s))
			
(* Executes a conditional expression.
   Returns the value of the conditional branch. *)
and execConditional exec env cond ift iff =
    let 
	val pretty = PrettyPrint.kecexp2prettystr exec
    in
	case (exec cond) of
	    KEC.LITERAL (KEC.CONSTBOOL b) => exec (if b then ift else iff)
	  | object as KEC.OBJECT _ =>
	    if TypeLib.check exec (KEC.TYPE (Symbol.symbol "ModelOperation"), object) orelse
	       TypeLib.check exec (KEC.TYPE (Symbol.symbol "SimQuantity"), object)
	    then
		exec(apply(sym "branch", [object,
					  KEC.LAMBDA {args=[], body=exec ift, undefined_args_allowed=false, closure=Env.new()},
					  KEC.LAMBDA {args=[], body=exec iff, undefined_args_allowed=false, closure=Env.new()}]))
	    else raise DynException.TypeMismatch ("Condition " ^ (pretty cond) ^ " evaluated to a non-boolean value " ^ (pretty object))
	  | exp => raise DynException.TypeMismatch ("Condition " ^ (pretty cond) ^ " evaluated to a non-boolean value " ^ (pretty exp))
    end

(* Executes the application of a callable object. 
   Returns the result of application. *)
and execApply recurring parse (depth_count, isLHS) env func args =
    let
	val exec = exec_exp parse ((depth_count + 1), isLHS)
	val pretty = PrettyPrint.kecexp2prettystr (fn (exp) => decell (exec (env, exp)))

	fun execAppable lambda =
	    let
		val args' = exec (env, args)
	    in
		if Apply.isValidFunArgs args' then
		    Apply.apply exec env lambda args'
		else
		    error env ($("Function called with invalid arguments: received " ^ (pretty args'))) (* TODO: fill in better info *)
	    end
    in
	if recurring orelse Apply.isAppable func then
	    execAppable func
	else
	    (* func is an expression that must be evaluated before application. *)
	    execApply true parse (depth_count, isLHS) env (decell (exec (env, func))) args
    end

(* Executes a namespace definition. 
   Returns a namespace object instance. *)
and execNamespaceDef parse (depth_count, isLHS) env name stms = 
    let
	fun exec exp = decell (exec_exp parse ((depth_count+1), isLHS) (env, exp))

	fun key2member env vis key =
	    case Env.system_lookup (PrettyPrint.kecexp2prettystr exec) env key of
		NONE => raise DynException.NameError (Symbol.name key)
	      | SOME exp =>
		(case exec exp of
		     KEC.RUNNABLE runnable => 
		     KEC.METHOD (key, vis, runnable)
		   | KEC.CELL (tp, KEC.REFERENCE c) => 
		     KEC.VAR {name=key, value=c, typepattern=tp, visibility=vis}
		   | KEC.CELL (tp, KEC.GETSET (g,s)) =>
		     KEC.VAR {name=key, value=ref (g()), typepattern=tp, visibility=vis}
		   | _ => KEC.CONSTANT (key, vis, exp)) 

	fun process_stm ((vis, stm), (env, obj)) = 
	    let
		val env = Env.push_local env

		val (_, env) = exec_stm parse (stm, env)

		val obj = foldl (fn(key, obj) => SymbolTable.enter (obj, key, (key2member env vis key))) obj (Env.top_local_keys env)
	    in
		(env, obj)
	    end

	(* run the stms in the namespace *)
	val (_, obj) = foldl process_stm (env, SymbolTable.empty) stms


		
	val tostring = Objects.method "tostring" nil
				      (KEC.LITERAL (KEC.CONSTSTR ("Namespace '" ^ (Symbol.name name) ^ "' containing: " ^ (String.concatWith ", " ("tostring" :: map Symbol.name (SymbolTable.listKeys obj))))))
    in
	Objects.instance false ((SymbolTable.listItems obj) @ [tostring])
    end

(* Executes a class definition.
   Returns the class object instance. *)
and execClassDef exec env name members parent =
    Objects.classinstance env name members (decell (exec parent))


(* Executes a library function. 
   Returns the result of the function. *)
and execLibraryFunction exec env name arg =
    case arg of
	KEC.TUPLE args
	=> Library.exec exec name (map decell args)
      | KEC.UNIT
	=> Library.exec exec name []
      | arg 
	=> Library.exec exec name [arg]
	   
(* Executes an import statement. 
   Returns (unit,env). *)
and execImportStatement parse env file pos =
    let
	val includepaths = (!ParserSettings.filepath) :: map StdFun.expand_env_variables (DynamoOptions.getStringVectorSetting("sourcepath"))
			   
	val fullpath = case FilePath.find file includepaths of
			   SOME path => path
			 | NONE => raise DynException.ImportError (file, includepaths)

	val instream = TextIO.openIn fullpath

	val (name, path) = GeneralUtil.filepath_split fullpath
	val oldsettings = ParserSettings.getSettings ()

	val _ = ParserSettings.setSettings(false, name, path)
	val _ = Logger.log_notice ($("Reading source file '" ^ (fullpath)^ "'"))
	val eof_encountered = !Globals.eof_encountered

	val _ = OOLCParse.push_buffer()
    in
	parse instream env
	before (TextIO.closeIn instream;
		OOLCParse.pop_buffer();
		Globals.eof_encountered := eof_encountered;
		ParserSettings.restoreSettings oldsettings)
	handle _ => 
	       (TextIO.closeIn instream;
		Globals.eof_encountered := eof_encountered;
		ParserSettings.restoreSettings oldsettings;
		(KEC.UNIT, env))
    end

(* Executes a global binding definition.
   Returns (unit,env) *)
and execGlobalDefinition parse env (replace, name, typepattern, exp) pos =
    let
	fun exec exp = exec_exp parse (0, false) (PosLog.addSystemPos (env, pos), exp)
	val pretty = PrettyPrint.kecexp2prettystr (decell o exec)

	val exp' = exec exp

	val env' = 
	    case replace of
		KEC.REPLACE => Env.global_add ((name, exp'), env)
	      | KEC.OVERLOAD =>
		let
		    val old =
			case Env.lookup (!(Env.system2global env)) name of
			    SOME old => exec old
			  | NONE => raise DynException.NameError (Symbol.name name)
		    val new = merge_runnables env (old, exp')
		in
		    Env.global_add ((name, new), env)
		end
    in
	if TypeLib.check (decell o exec) (typepattern, exp') then (KEC.UNIT, env')
	else raise DynException.TypeMismatch ("Cannot define " ^ (Symbol.name name) ^ " globally, expected type: " ^ (PrettyPrint.typepattern2str typepattern) ^ " and received expression: " ^ (pretty exp'))
    end
	
(* Executes a local binding definition.
   Returns (unit,env) *)
and execLocalDefinition parse env (replace, name, typepattern, exp) pos =
    let
	fun exec exp = exec_exp parse (0, false) (PosLog.addSystemPos (env, pos), exp)
	val pretty = PrettyPrint.kecexp2prettystr (decell o exec)

	val exp' = exec exp
		   
	val env' = 
	    case replace of
		KEC.REPLACE => Env.local_add pretty ((name, exp'), env)
	      | KEC.OVERLOAD =>
		let
		    val old =
			case Env.system_lookup pretty env name of
			    SOME old => exec old
			  | NONE => raise DynException.NameError (Symbol.name name)
		    val new = merge_runnables env (old, exp')
		in
		    Env.local_add pretty ((name, new), env)
		end
    in
	if TypeLib.check (decell o exec) (typepattern, decell exp') then (KEC.UNIT, env')
	else raise DynException.TypeMismatch ("Cannot define " ^ (Symbol.name name) ^ " locally, expected type: " ^ (PrettyPrint.typepattern2str typepattern) ^ " and received expression: " ^ (pretty (decell exp')))
    end

(* Executes a local constant definition.
   Returns (unit,env) *)
and execConstantDefinition parse env (replace, name, typepattern, exp) pos =
    let
	fun exec exp = exec_exp parse (0, false) (PosLog.addSystemPos (env, pos), exp)
	val pretty = PrettyPrint.kecexp2prettystr (decell o exec)

	val exp' = exec exp

	val env' =
	    case Env.top_local_lookup pretty env name
	     of NONE => Env.local_add pretty ((name, exp'), env)
	      (* It is an error to redefine a constant within the same scope. *)
	      | _ => raise DynException.NameError (Symbol.name name)
    in
	if TypeLib.check (decell o exec) (typepattern, decell exp') then (KEC.UNIT, env')
	else raise DynException.TypeMismatch ("Cannot define " ^ (Symbol.name name) ^ " locally, expected type: " ^ (PrettyPrint.typepattern2str typepattern) ^ " and received expression: " ^ (pretty (decell exp')))
    end
	
(* Executes an open statement.
   Returns (unit,env). *)
and execOpenStatement parse env (object, excludes, include_privates) pos =
    let
	fun exec exp = exec_exp parse (0, false) (PosLog.addSystemPos (env, pos), exp)
	val pretty = PrettyPrint.kecexp2prettystr (decell o exec)

	fun inject_member (member, env) =
	    let
		val name = Objects.memberName member
		val allow = 
		    (not (List.exists (fn(n) => n = name) excludes))
		    andalso (Objects.memberIsPublic member orelse include_privates)
	    in
		if allow then Env.local_add pretty ((name, KEC.SEND {message=name, object=object}), env)
		else env
	    end
		 
	fun update_env exp members = foldl inject_member env members

	val env' = 
	    case decell (exec object) of
		exp as KEC.OBJECT {members=ref members, ...} => update_env exp members
	      | exp => raise DynException.TypeMismatch ("Expected an object instance but received " ^ (pretty exp))
				       
    in
	(KEC.UNIT, env')
    end

(* Executes a variable assignment statement.
   Returns (unit,env). *)
and execAssignStatment parse env (dest, source) pos =
    let
	fun execLHS exp = exec_exp parse (0, true) (PosLog.addSystemPos (env, pos), exp)
	fun execRHS exp = exec_exp parse (0, false) (PosLog.addSystemPos (env, pos), exp)

	val pretty = PrettyPrint.kecexp2prettystr (decell o execRHS)
	val lhs = execLHS dest
    in
	(KEC.UNIT, env) before 
	(case lhs of
		KEC.CELL (tp, c) => 
		let
		    val rhs = execRHS source
		in
		    if TypeLib.check (decell o execRHS) (tp, rhs) 
		    then 
			case c of
			    KEC.REFERENCE r => 
			    r := rhs
			  | KEC.GETSET(g,s) =>
			    s(rhs)
		    (* TODO: better printing of type patterns *)
		    else raise DynException.TypeMismatch ("Expected " ^ (PrettyPrint.typepattern2str tp) ^ " and received " ^ (PrettyPrint.kecexp2nickname rhs))
		end
	      | KEC.PROPERTYEXP {write=SOME runnable, ...}
		=> (execRHS (KEC.APPLY {func=KEC.RUNNABLE runnable, args=KEC.TUPLE [source]}); ())
	      | KEC.PROPERTYEXP {name=name, write=NONE, ...}
		=> raise DynException.TypeMismatch ("Cannot assign to a read-only property " ^ (Symbol.name name))
	      | exp 
		=> raise DynException.TypeMismatch ("Cannot assign to a non-variable " ^ (pretty exp)))

    end
	

fun recursive_decell (exp as KEC.LITERAL _) = exp
  | recursive_decell (exp as KEC.TYPEEXP _) = exp
  | recursive_decell (exp as KEC.SYMBOL _) = exp
  | recursive_decell (exp as KEC.UNIT) = exp
  | recursive_decell (exp as KEC.UNDEFINED) = exp
  | recursive_decell (exp as KEC.VECTOR _) = exp
  | recursive_decell (exp as KEC.OBJECT _) = exp (* not decelling objects by design; there's no need to print all the contents of an object and it's inefficient *)
  | recursive_decell (exp as KEC.RUNNABLE _) = exp

  | recursive_decell (KEC.CELL (_, KEC.REFERENCE r)) =
    recursive_decell (! r)

  | recursive_decell (KEC.CELL (_, KEC.GETSET (g,s))) =
    recursive_decell (g())

  | recursive_decell (KEC.TUPLE exps) =
    KEC.TUPLE (map recursive_decell exps)

  | recursive_decell (KEC.LAMBDA {args, body, closure, undefined_args_allowed}) =
    KEC.LAMBDA {body=recursive_decell body, args=args, closure=closure, undefined_args_allowed=undefined_args_allowed}

  | recursive_decell exp =
    raise DynException.TypeMismatch ("Cannot recursive_decell " ^ (PrettyPrint.kecexp2nickname exp))


fun run parse env stms =
    let 
	fun process_stm (stm, (_, env)) = 
	    exec_stm parse (stm, env)

	    
	val (retval, env) = foldl process_stm (KEC.UNIT, env) stms
    in
	(env, recursive_decell(retval)) 
    end
    

end
