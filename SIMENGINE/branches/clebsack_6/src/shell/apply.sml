(* Copyright Simatra - 2006 - 2008
 * apply.sml - Object Oriented Lambda Calculus execution engine
 *
 * Evaluation of functional application.
 *)
signature APPLY =
sig
    (* Indicates whether a given expression may be used as the arguments applied to a function. *)
    val isValidFunArgs: KEC.exp -> bool

    (* Indicates whether a given expression may have arguments applied. *)
    val isAppable: KEC.exp -> bool

    (* Evaluates the application of arguments to a callable object in a given environment.
       For a lambda expression or runnable, returns the result of executing the body with the arguments substituted for the formal parameters.
       For objects, vectors, tuples, strings, and binaries, returns the result of sending the "()" message to the object with the given arguments. *)
    val apply: ((KEC.exp Env.env ref * (KEC.exp Env.env * KEC.exp Env.env) * PosLog.pos list) * KEC.exp -> KEC.exp)
	       -> (KEC.exp Env.env ref * (KEC.exp Env.env * KEC.exp Env.env) * PosLog.pos list)
	       -> KEC.exp -> KEC.exp
	       -> KEC.exp
end

structure Apply : APPLY =
struct

val TypeMismatch = DynException.TypeMismatch
and IncorrectNumberOfArguments = DynException.IncorrectNumberOfArguments

fun decell (KEC.CELL (_,KEC.REFERENCE c)) = decell (!c)
  | decell (KEC.CELL (_,KEC.GETSET (g,_))) = decell(g())
  | decell exp = exp

fun isValidFunArgs args =
    case args of
	KEC.UNIT => true
      | KEC.TUPLE _ => true
      | KEC.LITERAL _ => true
      | KEC.TYPEEXP _ => true
      | KEC.UNDEFINED => true
      | KEC.VECTOR _ => true
      | KEC.LAMBDA _ => true
      | KEC.RUNNABLE _ => true
      | KEC.OBJECT _ => true
      | KEC.CELL _ => true
      | _ => false

fun isAppable func =
    case func of
	KEC.LAMBDA _ => true
      | KEC.RUNNABLE _ => true
      | KEC.OBJECT _ => true
      | KEC.VECTOR _ => true
      | KEC.TUPLE _ => true
      | KEC.LITERAL (KEC.CONSTSTR _) => true
      | KEC.LITERAL (KEC.CONSTBINARY _) => true
      | _ => false

fun apply exec env lambda args =
    case lambda of
	KEC.LAMBDA {args=lamargs, body, closure, undefined_args_allowed}
	=>
	let
	    val pretty = PrettyPrint.kecexp2prettystr (fn (exp) => decell (exec (env, exp)))

	    fun verify_defined undefined_args_allowed KEC.UNDEFINED =
		if not undefined_args_allowed then
		    raise TypeMismatch ("argument with undefined value passed to " ^ (pretty lambda))
		else
		    [KEC.UNDEFINED]
	      | verify_defined _ _ = nil

	    val args_list = 
		case args of
		    KEC.UNIT => []
		  | KEC.TUPLE t => 
		    let 
			val args = map decell t
			val _ = map (verify_defined undefined_args_allowed) args
		    in 
			args
		    end
		  | _ => raise TypeMismatch ("Arguments to function " ^ (pretty lambda) ^ " must be a tuple.")

	    val _ = if (length lamargs) <> (length args_list) then
			raise IncorrectNumberOfArguments {expected=(length lamargs), actual=(length args_list)}
		    else
			()
			
	    (* we may have already gone over this, but we may not *)
	    val args_list = map (fn (a) => exec (env,a)) args_list
	    val arg_bindings = ListPair.zip (lamargs, args_list)
	    val (_, localenv, _) = env

(*	    val _ = print ("Lambda args = " ^ (String.concatWith ", " (lamargs)) ^ "\n")*)
	in
	    exec ((foldl (Env.local_add pretty) (Env.system_union (Env.create_local env, closure)) arg_bindings), body)
(*	    exec (foldl (Env.local_add pretty) (Env.replace_local (env, closure)) (arg_bindings), body)*)
	end

      | KEC.RUNNABLE funs
	=> (* find the appropriate entry and transform it into a lambda, then recurse with that *)
	let
	    val pretty = PrettyPrint.kecexp2prettystr (fn (exp) => decell (exec (env, exp)))

	    fun typecheck pattern exp =	TypeLib.check (fn (a) => exec (env,a)) (pattern, exp)		    

	    fun verify_defined undefined_args_allowed KEC.UNDEFINED =
		if not undefined_args_allowed then
		    raise TypeMismatch ("argument with undefined value passed to " ^ (pretty lambda))
		else
		    [KEC.UNDEFINED]
	      | verify_defined _ _ = nil

	    val args_list = 
		case args of
		    KEC.UNIT => []
		  | KEC.TUPLE t => 
		    let 
			val args = map decell t
			val _ = map (verify_defined false) args
		    in 
			args
		    end
		  | _ => raise TypeMismatch ("Arguments to function " ^ (pretty lambda) ^ " must be a tuple.")

	    fun fun_args {args, ...} = args
	    fun arg_name (name, pattern) = name
	    fun arg_pattern (name, pattern) = pattern

	    (* Indicates whether the args_list matches against a given list of patterns. *)
	    fun typematch_args argpatterns =
		((length argpatterns) = (length args_list)
		 andalso typecheck (KEC.TUPLETYPE argpatterns) (KEC.TUPLE args_list))


	    val (return, lambda') =
		let
		    val {name, args, return, stms, closure} = 
			case List.find (typematch_args o (map arg_pattern) o fun_args) funs of
			    SOME func => func
			  (* TODO: detect incorrect number of arguments and raise appropriately. *)
			  | NONE => raise TypeMismatch ("received " ^ (pretty args) ^ " as arguments to " ^ (pretty lambda))
		in
		    (return, KEC.LAMBDA {args=map arg_name args,
					 body=KEC.STMS stms,
					 undefined_args_allowed=false,
					 closure=Env.add ((name, lambda), closure)})
		end

	    val value = apply exec env lambda' args
	in
	    if typecheck return value then value
	    else raise TypeMismatch ((pretty lambda) ^ " returned " ^ (pretty value))
	end
	
      | KEC.OBJECT _
	=> exec (env, KEC.APPLY {func=KEC.SEND {message=Symbol.symbol "()", object=lambda}, args=args})
      | KEC.VECTOR _
	=> exec (env, KEC.APPLY {func=KEC.SEND {message=Symbol.symbol "()", object=lambda}, args=args})
      | KEC.TUPLE _
	=> exec (env, KEC.APPLY {func=KEC.SEND {message=Symbol.symbol "()", object=lambda}, args=args})
      | KEC.LITERAL lit
	=> 
	(case lit of
	     KEC.CONSTSTR _ => exec (env, KEC.APPLY {func=KEC.SEND {message=Symbol.symbol "()", object=lambda}, args=args})
	   | KEC.CONSTBINARY _ => exec (env, KEC.APPLY {func=KEC.SEND {message=Symbol.symbol "()", object=lambda}, args=args})
	   | _ => raise TypeMismatch ("arguments applied to " ^ (PrettyPrint.kecexp2nickname lambda) ^ " that is not callable"))
      | _  => raise TypeMismatch ("arguments applied to " ^ (PrettyPrint.kecexp2nickname lambda) ^ " that is not callable")
end
