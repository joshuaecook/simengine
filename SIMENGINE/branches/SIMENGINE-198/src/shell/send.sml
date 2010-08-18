(* Copyright Simatra - 2006 - 2008
 * send.sml - Object Oriented Lambda Calculus execution engine
 *
 * Evaluation of object messages.
 *)
signature SEND =
sig
    val send: bool -> (KEC.exp -> KEC.exp)
	      -> (KEC.exp Env.env ref * (KEC.exp Env.env * KEC.exp Env.env * KEC.exp Env.env option ref) * PosLog.pos list)
	      -> Symbol.symbol -> KEC.exp
	      -> KEC.exp
end

structure Send : SEND = 
struct

val TypeMismatch = DynException.TypeMismatch
and IncorrectNumberOfArguments = DynException.IncorrectNumberOfArguments

fun error (env as (_, _, poslog)) text = 
    KEC.UNDEFINED
    before (Logger.log_error_with_position poslog (text);
	    DynException.setErrored())

fun decell (KEC.CELL (_,KEC.REFERENCE c)) = decell (!c)
  | decell (KEC.CELL (_,KEC.GETSET (g,_))) = decell(g())
  | decell exp = exp

(* Returns a SYMBOL expression with the given name. *)
fun sym s = KEC.SYMBOL (Symbol.symbol s)

fun methodNotFound (name) =
    raise DynException.NameError (Symbol.name name)

fun methodHidden (name) =
    raise DynException.NameError (Symbol.name name)

fun strlist strs =
    KEC.list2kecvector (map (fn(s) => KEC.LITERAL (KEC.CONSTSTR s)) strs)

fun str2exp str =
    KEC.LITERAL (KEC.CONSTSTR str)

fun list2exp exps =
    KEC.list2kecvector exps

fun exp2list env (KEC.VECTOR v) = 
    KEC.kecvector2list v
  | exp2list env exp = raise DynException.TypeMismatch ("Expected a Vector but received " ^ (PrettyPrint.kecexp2nickname exp))
			     
fun isBooleanExp (KEC.LITERAL (KEC.CONSTBOOL _)) = true
  | isBooleanExp _ = false

fun isbinvec env list =
    List.all isBooleanExp (exp2list env list)

(* Returns a function application. *)
fun apply (f, a) = KEC.APPLY {func=f, args=KEC.TUPLE a}

(* Returns a library function invocation. *)
fun libfun exec name args =
    KEC.LIBFUN (Symbol.symbol name, KEC.TUPLE args)
(*     Library.exec exec (Symbol.symbol name) (map exec args) *)

(* Returns an application of an object instance method to the given arguments. *)
fun applySend message object args = 
    case args of
	SOME args' => apply (KEC.SEND {message=Symbol.symbol message, object=object}, args')
      | NONE => apply (KEC.SEND {message=Symbol.symbol message, object=object}, [])

fun callfun name args =
    apply (KEC.SYMBOL (Symbol.symbol name),
	   args)

fun method name exp =
    KEC.RUNNABLE ([{name=name, args=[], return=KEC.DONTCARE, 
		    stms=[KEC.ACTION(KEC.EXP exp, PosLog.NOPOS)], 
		    closure=Env.new()}])

fun argmethod name args exp =
    KEC.RUNNABLE ([{name=name, args=args, return=KEC.DONTCARE, 
		    stms=[KEC.ACTION(KEC.EXP exp, PosLog.NOPOS)], 
		    closure=Env.new()}])

    

fun send isLHS exec env message object =
    let
	val pretty = PrettyPrint.kecexp2prettystr exec

        val libfun = libfun exec

	(* Wraps an expression in a lambda application which binds the "self" variable reference. *)
	fun injectSelf self exp =
	    let
		val exp' =
		    case exp of
			KEC.PROPERTYEXP {name, read, write, ...}
			=> KEC.PROPERTYEXP {expansionAllowed=not isLHS, name=name, read=read, write=write}
		      | _ => exp
	    in
		KEC.APPLY {args=KEC.TUPLE [self],
			   func=KEC.LAMBDA {args=[Symbol.symbol "self"],
					    closure=Env.new (),
					    undefined_args_allowed=false,
					    body=KEC.STMS [KEC.ACTION (KEC.OPEN {obj=sym "self", excludes=nil, include_privates=true}, PosLog.NOPOS),
							   KEC.ACTION (KEC.EXP exp', PosLog.NOPOS)]}}
	    end

	fun obj2exp hasBeenExecuted object =
	    case object of
		KEC.OBJECT {members=members as ref obj, allPublic} =>
		let
		    fun copy_member (KEC.VAR {name,visibility,typepattern,value=ref e}) =
			KEC.VAR {value=ref (exec e), name=name, visibility=visibility, typepattern=typepattern}
		      | copy_member m = m


		    fun member2exp (KEC.CONSTANT (_, _, value)) = value
		      | member2exp (KEC.VAR {value, typepattern, ...}) = KEC.CELL (typepattern, KEC.REFERENCE value)
		      | member2exp (KEC.METHOD (_, _, runnable)) = 
			injectSelf (KEC.OBJECT {members=members, allPublic=true}) 
				   (KEC.RUNNABLE runnable)
		      | member2exp (KEC.PROPERTY (_, prop)) =
			injectSelf (KEC.OBJECT {members=members, allPublic=true})
				   (KEC.PROPERTYEXP prop)
		      | member2exp (KEC.CONSTRUCTOR {name, init_methods, new_object}) =
			let
			    val new_object' = if SOME (Symbol.symbol "super") = name then new_object
					      else map copy_member new_object
						   
						   
			    (* open the object and run the stms*)
			    fun constructor2runnable {args, return, stms, super, closure} =
				let
				    val super' = 
					case super of
					    KEC.CONSTRUCTOR {name, init_methods, new_object}
					    => KEC.CONSTRUCTOR {new_object=new_object', name=name, init_methods=init_methods}
					  | _ => raise DynException.TypeMismatch "Expected a constructor but received something else."

				    val obj_ref = ref new_object'
				    fun arg_name (name, pattern) = name
				    val arg_names = map arg_name args
				in				    
				    {name=case name of
					      SOME n => n
					    | NONE => Symbol.symbol "new",
				     args=args,
				     return=return,
				     closure=closure,
				     stms=case GeneralUtil.applyOpt Symbol.name name of
					      SOME "super" =>
					      (KEC.DEFINITION(KEC.DEFLOCAL (KEC.REPLACE,  
 									    Symbol.symbol "super", 
									    KEC.DONTCARE, 
									    member2exp super),  
							      PosLog.NOPOS)) 
					      :: stms (* if it is a super constructor, don't reopen the restricted object *) 
					    | _ =>      
					      [KEC.ACTION (KEC.EXP (KEC.APPLY {func=KEC.LAMBDA {args=[Symbol.symbol "self"],
											     body=KEC.STMS ([KEC.ACTION(KEC.OPEN {obj=sym "self", excludes=arg_names, include_privates=true}, 
															PosLog.NOPOS),
													     KEC.ACTION(KEC.OPEN {obj=KEC.OBJECT {members=ref [super'], allPublic=true},
																  excludes=nil, include_privates=true}, 
															PosLog.NOPOS),
													     KEC.ACTION(KEC.EXP(KEC.APPLY{func=KEC.SYMBOL(Symbol.symbol "addConst"),
																	  args=KEC.TUPLE[KEC.LITERAL(KEC.CONSTSTR "class"),
																			 object]}),
															PosLog.NOPOS)]
													    @ stms @ 
													    [KEC.ACTION(KEC.EXP(sym "self"), 
															PosLog.NOPOS)]),
											     undefined_args_allowed=false,
											     closure=Env.new()},
									     args=KEC.TUPLE [KEC.OBJECT {members=obj_ref, allPublic=true}]}), 
							   PosLog.NOPOS)]}
				end
			in
			    KEC.RUNNABLE (map constructor2runnable init_methods)
			end


		    val member = 
			if message = (Symbol.symbol "members") then
			    KEC.CONSTANT (message, KEC.PUBLIC, KEC.TUPLE (KEC.LITERAL (KEC.CONSTSTR "members") ::
									  (map (fn (m) => KEC.LITERAL (KEC.CONSTSTR (Symbol.name (Objects.memberName m))))
									       (List.filter Objects.memberIsPublic obj))))
			else 
			    case List.find (fn (m) => message = (Objects.memberName m)) obj of
				SOME m => if (allPublic orelse Objects.memberIsPublic m) then m
					  else methodHidden message
			      | NONE => methodNotFound message
					
		in
		    exec (member2exp member)
		end

	      | KEC.LITERAL(KEC.CONSTSTR s) => 
		(case Symbol.name message of
		     "members" => strlist ["members", "length", "tonumber", "first", "rest", "substring",
					   "contains", "startsWith", "endsWith", "replace", "translate",
					   "split", "strip", "lstrip", "rstrip", "center", "join"]
		   | "length" => method message (KEC.LITERAL (KEC.CONSTREAL (Real.fromInt (String.size s))))
		   | "tonumber" => (case Real.fromString s of
					SOME r => method message (KEC.LITERAL(KEC.CONSTREAL r))
				      | NONE => method message (KEC.UNIT))
		   | "first" => 
		     let
			 fun strhd "" = ""
			   | strhd s = (String.str (String.sub (s, 0)))
		     in 
			 method message (KEC.LITERAL(KEC.CONSTSTR (strhd s)))
		     end
		   | "rest" => KEC.LAMBDA{args=[],
					  body=KEC.LITERAL(KEC.CONSTSTR (if s = "" then "" else (implode o tl o explode) s)),
					  undefined_args_allowed=false,
					  closure=Env.new()}
		   | "substring" => argmethod message [(Symbol.symbol "pos", KEC.TYPE (Symbol.symbol "Number")), 
						       (Symbol.symbol "len", KEC.TYPE (Symbol.symbol "Number"))]
					      (libfun "substring" [object, sym "pos", sym "len"])
		   | "contains" => argmethod message [(Symbol.symbol "substr", KEC.TYPE (Symbol.symbol "String"))]
					     (libfun "str_contains" [object, sym "substr"])
		   | "startsWith" => argmethod message [(Symbol.symbol "substr", KEC.TYPE (Symbol.symbol "String"))]
					       (libfun "str_startsWith" [object, sym "substr"])
		   | "endsWith" => argmethod message [(Symbol.symbol "substr", KEC.TYPE (Symbol.symbol "String"))]
					     (libfun "str_endsWith" [object, sym "substr"])
		   | "replace" => argmethod message [(Symbol.symbol "old", KEC.TYPE (Symbol.symbol "String")),
						     (Symbol.symbol "new", KEC.TYPE (Symbol.symbol "String"))]
					    (libfun "str_replace" [object, sym "old", sym "new"])
		   | "translate" => argmethod message [(Symbol.symbol "old", KEC.TYPE (Symbol.symbol "String")),
						       (Symbol.symbol "new", KEC.TYPE (Symbol.symbol "String"))]
					      (libfun "str_translate" [object, sym "old", sym "new"])
		   | "split" => argmethod message [(Symbol.symbol "sep", KEC.TYPE (Symbol.symbol "String"))]
					  (libfun "str_split" [object, sym "sep"])
		   | "strip" => argmethod message [(Symbol.symbol "chars", KEC.TYPE (Symbol.symbol "String"))]
					  (libfun "str_strip" [sym "chars", object])
		   | "lstrip" => argmethod message [(Symbol.symbol "chars", KEC.TYPE (Symbol.symbol "String"))]
					   (libfun "str_lstrip" [sym "chars", object])
		   | "rstrip" => argmethod message [(Symbol.symbol "chars", KEC.TYPE (Symbol.symbol "String"))]
					   (libfun "str_rstrip" [sym "chars", object])
		   | "center" => argmethod message [(Symbol.symbol "width", KEC.TYPE (Symbol.symbol "Number")),
						    (Symbol.symbol "fill", KEC.TYPE (Symbol.symbol "String"))]
					   (applySend "center" (sym "Operations") (SOME [object, sym "width", sym "fill"]))
		   | "join" => argmethod message [(Symbol.symbol "substrings", (KEC.COMPOUNDTYPE (Symbol.symbol "Vector", KEC.DONTCARE)))]
					 (applySend "join" (sym "Operations") (SOME [object, sym "substrings"]))
		   | "()" => 
		     let
			 val strvec = map (fn(c) => (KEC.LITERAL(KEC.CONSTSTR (Char.toString c)))) (explode s)
		     in
			 KEC.LAMBDA{args=[Symbol.symbol "pos"],
				    body=KEC.APPLY{func=KEC.SEND{message=Symbol.symbol "()", 
								 object=KEC.list2kecvector strvec},
						   args=KEC.TUPLE [sym "pos"]},
				    undefined_args_allowed=false,
				    closure=Env.new()}
		     end
		   | "tostring" => method message (object)
		   | _ => methodNotFound(message))

	      | KEC.LITERAL(KEC.CONSTBOOL b) =>
		(case Symbol.name message of
		     "members" => strlist ["members", "tostring"]
		   | "tostring" => method message (KEC.LITERAL (KEC.CONSTSTR (pretty object)))
		   | _ => methodNotFound(message)
		)

	      | KEC.PROCESS (p, file, args) =>
		(case Symbol.name message of
		     "members" => strlist ["members", "tostring"]
		   | "tostring" => method message (KEC.LITERAL (KEC.CONSTSTR (pretty object)))
		   | _ => methodNotFound(message)
		)

	      | KEC.LITERAL(KEC.CONSTREAL r) =>
		(case Symbol.name message of
		     "members" => strlist ["abs", "ceil", "floor", "members", "round", "tostring"]
		   | "tostring" => method message (KEC.LITERAL (KEC.CONSTSTR (pretty object)))
		   | "floor" => method message (KEC.LITERAL (KEC.CONSTREAL (Real.fromInt (Real.floor r))))
		   | "ceil"  => method message (KEC.LITERAL (KEC.CONSTREAL (Real.fromInt (Real.ceil r))))
		   (* There is a bug in the implementation of Real.round 
								  where negative numbers get alternatively rounded up or down, e.g.
																   Real.round(~0.5) => 0
																			   Real.round(~1.5) => 2
																						   Real.round(~2.5) => 2
		    *)
		   | "round" => method message (KEC.LITERAL (KEC.CONSTREAL (Real.fromInt (Real.floor (0.5+r))))) 
		   | "abs"   => method message (KEC.LITERAL (KEC.CONSTREAL (Real.abs r)))
		   | _ => methodNotFound(message)
		)

	      | KEC.LITERAL(KEC.CONSTBINARY (bits, value)) =>
		(case Symbol.name message of
		     "members" => strlist ["length", "tostring", "tovector"]
		   | "tostring" => method message (KEC.LITERAL (KEC.CONSTSTR (pretty object)))
		   | "length" => method message (KEC.LITERAL (KEC.CONSTREAL (Real.fromInt bits)))
		   | "tovector" => 
		     let 
			 fun strrep str x = 
			     if x <= 0 then
				 ""
			     else
				 str ^ (strrep str (x-1))
				 
			 val str = IntInf.fmt StringCvt.BIN value
				   
			 val padding = strrep "0" (bits - (String.size str))
			 val boolstr = padding ^ str
				       
		     in 
			 method message (list2exp (map (fn(c) => case c of
								     #"0" => KEC.LITERAL (KEC.CONSTBOOL false)
								   | #"1" => KEC.LITERAL (KEC.CONSTBOOL true)
								   | _ => error env ($("Internal error encountered trying to convert binary to value: Encountered " ^ (Char.toString c)))
						       )
						       (explode boolstr)))
		     end
		   (* Indexing a binary is equivalent to converting it to a vector then indexing that vector. *)
		   | "()" => argmethod message [(Symbol.symbol "index", KEC.TYPE (Symbol.symbol "Number"))]
				       (KEC.APPLY {func=(KEC.APPLY {func=(KEC.SEND {message=Symbol.symbol "tovector", object=object}),
								    args=KEC.UNIT}),
						   args=KEC.TUPLE [sym "index"]})
		   | _ => methodNotFound message
		)
		
	      | KEC.VECTOR exps =>
		(case Symbol.name message of
		     "members" => strlist (["app", "at", "clone", "filter", "first", "isempty", "last", "length", "map", "members", "push_back", "push_front", "rest", "rev", "slice", "tostring", "totuple"] @ (if isbinvec env object then ["tobinary"] else []))
		   | "tovector" => method message (object)
		   | "clone" => method message (libfun "clone" [object])
		   | "length" => method message (KEC.LITERAL (KEC.CONSTREAL (Real.fromInt (!(#back_index exps) - !(#front_index exps)))))
		   | "rev" => method message (KEC.list2kecvector (rev (KEC.kecvector2list exps)))
		   | "first" => method message (if (! (#back_index exps)) - (!(#front_index exps)) = 0 then
						    KEC.ERROR (str2exp "Cannot access first element of empty vector")
						else
						    (* TODO: propogate type information for Vector cells *)
						    libfun "index" [object, KEC.LITERAL(KEC.CONSTREAL (1.0))])
		   | "last" => method message (if (!(#back_index exps)) - (!(#front_index exps)) = 0 then
						   KEC.ERROR (str2exp "Cannot access last element of empty vector")
					       else
						   (* TODO: propogate type information for Vector cells *)
						   libfun "index" [object, KEC.LITERAL(KEC.CONSTREAL (Real.fromInt(Array.length(!(#array exps)))))])
		   | "rest" => method message (if (!(#back_index exps)) - (!(#front_index exps)) = 0 then
						   KEC.ERROR (str2exp "Cannot access remaining elements of empty vector")
					       else
						   let
						       val vec = KEC.restOfVector(exps)
(*						       val list = (KEC.kecvector2list exps)
						       val vec = KEC.buildVector (tl list)*)
						   in
						       vec
						   end)
		   | "at" => argmethod message [(Symbol.symbol "index", KEC.TYPE (Symbol.symbol "Number"))] 
				       (libfun "index" [object, sym "index"])
		   | "()" => argmethod message [(Symbol.symbol "index", KEC.TYPE (Symbol.symbol "Vector"))] 
				       (callfun "vector_index" [object, sym "index"])  
 		   | "map" => argmethod message [(Symbol.symbol "function", KEC.ARROW (KEC.DONTCARE,KEC.DONTCARE))] 
					(libfun "map" [object, sym "function"])
 		   | "app" => argmethod message [(Symbol.symbol "function", KEC.ARROW (KEC.DONTCARE,KEC.DONTCARE))] 
					(libfun "app" [object, sym "function"])
		   | "filter" => argmethod message [(Symbol.symbol "predicate", KEC.ARROW (KEC.DONTCARE, KEC.DONTCARE))]
					   (applySend "filter" (sym "Operations") (SOME [sym "predicate", object]))
		   | "slice" => argmethod message [(Symbol.symbol "first", KEC.TYPE (Symbol.symbol "Number")), (Symbol.symbol "last", KEC.TYPE (Symbol.symbol "Number"))]
					  (libfun "slice" [object, sym "first", sym "last"])
 		   | "tostring" => method message (KEC.LITERAL (KEC.CONSTSTR (pretty object)))
 		   | "push_front" => argmethod message [(Symbol.symbol "element", KEC.DONTCARE)] 
					       (libfun "push_front" [object, sym "element"])
 		   | "push_back"  => argmethod message [(Symbol.symbol "element", KEC.DONTCARE)] 
					       (libfun "push_back" [object, sym "element"])
 		   | "isempty" => method message (KEC.LITERAL (KEC.CONSTBOOL ((!(#back_index exps)) - (!(#front_index exps)) = 0) ))
		   | "totuple" => method message (KEC.TUPLE (KEC.kecvector2list exps))
		   | "tobinary" => if isbinvec env object then
				       let
					   fun boolexp2char (KEC.LITERAL (KEC.CONSTBOOL b)) =
					       if b then #"1" else #"0"
					     | boolexp2char exp =
					       raise DynException.ValueError ("Error converting boolean vector to binary, encountered: " ^ (PrettyPrint.kecexp2nickname exp))
						     
					   val bools = exp2list env object
					   val str = implode (map boolexp2char bools)
					   val int = case StringCvt.scanString (IntInf.scan (StringCvt.BIN)) ("0000" ^ str) of
							 SOME i => i
						       | NONE => raise DynException.ValueError ("Unable to convert boolean to binary value in tobinary")
				       in
					   method message (KEC.LITERAL(KEC.CONSTBINARY (String.size str, int)))
				       end
				       
				   else
				       methodNotFound(message)
		   | _ => methodNotFound message
		)

	      | KEC.LAMBDA {args, body, closure, undefined_args_allowed} =>
		(case Symbol.name message of
		     "members" => strlist []
		   | "tostring" => method message (KEC.LITERAL (KEC.CONSTSTR (pretty object)))
		   | _ => methodNotFound(message)
		)

	      | KEC.RUNNABLE (funcs) =>
		(case Symbol.name message of
		     "members" => strlist []
		   | "tostring" => method message (KEC.LITERAL (KEC.CONSTSTR (pretty object)))
		   | _ => methodNotFound(message)
		)

	      | KEC.UNIT =>
		(case Symbol.name message of
		     "members" => strlist []
		   | "tostring" => method message (KEC.LITERAL (KEC.CONSTSTR (pretty object)))
		   | _ => methodNotFound(message)
		)

	      | KEC.TUPLE exps =>
		(case Symbol.name message of
		     "members" => strlist ["length", "at", "tostring"]
		   | "length" => method message (KEC.LITERAL (KEC.CONSTREAL (Real.fromInt (length exps))))
		   | "at" => argmethod message [(Symbol.symbol "index", KEC.TYPE (Symbol.symbol "Number"))] 
				       (libfun "index" [object, sym "index"]) (*TODO: multiple args *)
		   | "()" => argmethod message [(Symbol.symbol "index", KEC.TYPE (Symbol.symbol "Number"))] 
				       (libfun "index" [object, sym "index"]) (*TODO: multiple args *)
 		   | "map" => argmethod message [(Symbol.symbol "function", KEC.ARROW (KEC.DONTCARE,KEC.DONTCARE))] 
					(libfun "map" [object, sym "function"])
 		   | "app" => argmethod message [(Symbol.symbol "function", KEC.ARROW (KEC.DONTCARE,KEC.DONTCARE))] 
					(libfun "app" [object, sym "function"])
 		   | "isempty" => method message (KEC.LITERAL (KEC.CONSTBOOL (null exps)))
		   | "first" => method message (if null exps then
						    KEC.ERROR (str2exp "Cannot access first element of empty tuple")
						else
						    hd exps)
		   | "last" => method message (if null exps then
						   KEC.ERROR (str2exp "Cannot access last element of empty tuple")
					       else
						   hd (rev exps))
		   | "rest" => method message (if null exps then
						   KEC.ERROR (str2exp "Cannot access remaining elements of empty tuple")
					       else
						   KEC.TUPLE (tl exps))
		   | "tostring" => method message (KEC.LITERAL (KEC.CONSTSTR (pretty object)))
		   | "tovector" => method message (list2exp exps)
		   | _ => methodNotFound(message)
		)

	      | KEC.TYPEEXP pattern =>
		(case Symbol.name message of
		     "members" => strlist ["members", "tostring"]
		   | "tostring" => method message (KEC.LITERAL (KEC.CONSTSTR (pretty object)))
		   | _ => methodNotFound(message)
		)

	      | KEC.PROPERTYEXP {name, read, expansionAllowed, write} =>
		 let
		     fun inject {name, args, return, stms, closure} =
			 {closure=Env.closure_union (env, closure), name=name, args=args, return=return, stms=stms}
		 in		     
		     case read of
			 SOME reader =>
			 send isLHS exec env message (exec (KEC.APPLY {func=KEC.RUNNABLE(map inject reader), args=KEC.UNIT}))
		       | NONE => error env ($("Attempt to read non-readable property " ^ (Symbol.name name)))
		 end

	      | _ => if not hasBeenExecuted then
			 obj2exp true (decell (exec object))
		     else
			 error env ($("Method '" ^ (Symbol.name message) ^ "' invoked on thing that is not an object: " ^ (pretty object)))
    in
	(exec (obj2exp false object))
    end

end

