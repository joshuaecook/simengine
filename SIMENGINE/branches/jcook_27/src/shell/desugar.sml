structure Desugar =
struct

exception NotImplemented of string
exception UnexpectedMethodType
exception UnexpectedClassDef

fun trans_visibility (HLEC.PUBLIC) = KEC.PUBLIC
  | trans_visibility (HLEC.HIDDEN) = KEC.HIDDEN

fun desugar_lit lit =
    case lit of
	HLEC.CONSTREAL r 
	=> KEC.CONSTREAL r
      | HLEC.CONSTBOOL b
	=> KEC.CONSTBOOL b
      | HLEC.CONSTSTR s
	=> KEC.CONSTSTR s
      | HLEC.CONSTBINARY b
	=> KEC.CONSTBINARY b
    
fun desugar_runmod (HLEC.OVERLOAD) = KEC.OVERLOAD
  | desugar_runmod (HLEC.REPLACE) = KEC.REPLACE

fun desugar_typepattern (HLEC.TYPE s) = KEC.TYPE s
  | desugar_typepattern (HLEC.COMPOUNDTYPE (s, types)) = KEC.COMPOUNDTYPE (s, desugar_typepattern types)
  | desugar_typepattern (HLEC.ARROW (type1, type2)) = KEC.ARROW (desugar_typepattern type1, desugar_typepattern type2)
  | desugar_typepattern (HLEC.TUPLETYPE types) = KEC.TUPLETYPE (map desugar_typepattern types)
  | desugar_typepattern (HLEC.UNITTYPE) = KEC.UNITTYPE
  | desugar_typepattern (HLEC.DONTCARE) = KEC.DONTCARE

fun desugar_typ NONE = KEC.DONTCARE
  | desugar_typ (SOME at) = desugar_typepattern at

fun desugar_arg (name, typ) =
    (name, desugar_typ typ)

fun desugar_exp exp =
    case exp of
	HLEC.LITERAL lit
	=> KEC.LITERAL (desugar_lit lit)

      | HLEC.STRINGEXP exps
	=> KEC.APPLY {func = KEC.SEND {message = Symbol.symbol "join",
				       object = KEC.LITERAL(KEC.CONSTSTR "")},
		      args = (KEC.TUPLE [KEC.list2kecvector (map desugar_exp exps)])}

      | HLEC.SYMBOL sym
	=> KEC.SYMBOL sym

      | HLEC.LIBFUN (lib, args)
	=> KEC.LIBFUN (lib, desugar_exp args)

      | HLEC.LAMBDA {args, body}
	=> KEC.LAMBDA {args=args, body=desugar_exp body, closure=Env.new(), undefined_args_allowed=false}

      | HLEC.APPLY {func, args}
	=> KEC.APPLY {func=desugar_exp func, args=desugar_exp args}

      | HLEC.IFEXP {cond, ift, iff}
	=> KEC.IFEXP {cond=desugar_exp cond, ift=desugar_exp ift, iff=desugar_exp iff}

      | HLEC.VECTOR nil
	=> KEC.list2kecvector nil
      | HLEC.VECTOR exps
	=>
	let
	    fun pushVector items =
		foldl (fn(i, v) => HLEC.APPLY{func=HLEC.SEND{message=Symbol.symbol "push_back", object=v},
					     args=HLEC.TUPLE [i]}) 
		      (HLEC.LIBFUN(Symbol.symbol "deepclone", HLEC.VECTOR[])) 
		      items 
	in
	    desugar_exp (pushVector exps)
	end

      | HLEC.TUPLE exps
	=> KEC.TUPLE (map desugar_exp exps)

      | HLEC.UNIT
	=> KEC.UNIT

      | HLEC.UNDEFINED
	=> KEC.UNDEFINED

      | HLEC.SEND {message, object}
	=> KEC.SEND {message=message, object=desugar_exp object}

      | HLEC.SATISFIES {class, interface}
	=> KEC.SATISFIES {class = desugar_exp class, 
			  interface = desugar_exp interface}

      | HLEC.ASSERTION exp
	=> KEC.IFEXP {cond = desugar_exp exp,
		      ift  = KEC.UNIT,
		      iff  = KEC.ERROR (KEC.LITERAL (KEC.CONSTSTR ("assertion failed")))}

      | HLEC.ERROR msg
	=> KEC.ERROR (desugar_exp msg)

      | HLEC.POS (exp, pos) 
	=> KEC.POS (desugar_exp exp, pos)

      | HLEC.AND nil 
	=> KEC.LITERAL(KEC.CONSTBOOL true)

      | HLEC.AND (exp::rest)
	=> KEC.IFEXP {cond = desugar_exp exp,
		      ift  = desugar_exp (HLEC.AND rest),
		      iff  = KEC.LITERAL(KEC.CONSTBOOL false)}

      | HLEC.OR nil 
	=> KEC.LITERAL(KEC.CONSTBOOL false)

      | HLEC.OR (exp::rest)
	=> KEC.IFEXP {cond = desugar_exp exp,
		      ift  = KEC.LITERAL(KEC.CONSTBOOL true),
		      iff  = desugar_exp (HLEC.OR rest)}

      | HLEC.LET {vals, body}
	=> foldr (fn((n,v),b)=> KEC.APPLY{func=KEC.LAMBDA{args=n,
							  body=b,
							  undefined_args_allowed=false,
							  closure=Env.new()},
					  args=KEC.TUPLE [desugar_exp v]})
		 (desugar_exp body) 
		 vals

      | HLEC.LETREC {vals, body}
	=> raise NotImplemented "desugar(HLEC.LETREC _)"

      | HLEC.STMS stms
	=> KEC.STMS (map desugar_stm stms)

      | HLEC.TYPEEXP typepattern
	=> KEC.TYPEEXP (desugar_typepattern typepattern)

      | HLEC.FORGENERATOR (forclauses, exp) (* make the bottom level exp a vector so that we can flatten all results of map in build_generator *)
	=> foldr build_generator (desugar_exp exp) forclauses
      | HLEC.FORALL {var, collection, test}
	=> KEC.APPLY{func=KEC.SYMBOL(Symbol.symbol "operator_forall"), 
		     args=KEC.TUPLE [KEC.LAMBDA{args=[var], body=desugar_exp test, undefined_args_allowed=false, closure=Env.new()}, 
				     desugar_exp collection]}

      | HLEC.EXISTS {var, collection, test}
	=> KEC.APPLY{func=KEC.SYMBOL(Symbol.symbol "operator_exists"), 
		     args=KEC.TUPLE [KEC.LAMBDA{args=[var], body=desugar_exp test, undefined_args_allowed=false, closure=Env.new()}, 
				     desugar_exp collection]}

      | HLEC.TABLE entries
	=> 
	let
	    fun entry2mem (name, exp) =
		(KEC.TUPLE [KEC.LITERAL(KEC.CONSTSTR (Symbol.name name)), desugar_exp exp])
	in
	    KEC.APPLY{func=KEC.SEND{message=Symbol.symbol "new", object=(KEC.SYMBOL (Symbol.symbol "Table"))},
		      args=KEC.TUPLE [KEC.list2kecvector (map entry2mem entries)]}
	end



and build_generator ({var, collection, condition}, exp) =
    let
	val collection = KEC.APPLY {func=KEC.SEND{message=Symbol.symbol "tovector", object=desugar_exp collection},
				    args=KEC.UNIT}
    in
	(*KEC.APPLY{func=KEC.SYMBOL (Symbol.symbol "flatten"),
		  args=KEC.TUPLE [*)KEC.APPLY {func=KEC.SEND{message=(Symbol.symbol "map"),
							   object=case condition of
								      SOME condition =>
								      KEC.APPLY{func=KEC.SYMBOL (Symbol.symbol "filter"),
										args=KEC.TUPLE [KEC.LAMBDA{args=[var],
													   body=desugar_exp condition,
													   undefined_args_allowed=false,
													   closure=Env.new()},
												collection]}
								    | NONE => collection},
					       args=KEC.TUPLE [KEC.LAMBDA {args=[var],
									   body=exp,
									   undefined_args_allowed=false,
									   closure=Env.new()}]}(*]}*)
    end

and desugar_property {name, read, write} =
    {name=name,
     expansionAllowed=true,
     read=case read of
	      SOME stms =>
	      SOME [{name=name,
		     args=[],
		     return=KEC.DONTCARE,
		     stms=map desugar_stm stms,
		     closure=Env.new()}]
	    | NONE => NONE,
     write=case write of
	       SOME (arg, stms) =>
	       SOME [{name=name,
		      args=[(arg, KEC.DONTCARE)],
		      return=KEC.DONTCARE,
		      stms=map desugar_stm stms,
		      closure=Env.new()}]
	     | NONE => NONE}


(*TODO: remove STUBBED OUT code *)
and desugar_def (definition, pos) =
    case definition of
	HLEC.DEFFUN (runmod, functiondefs) (*header as {name, args, return}, stms)*)
	=>
	let
	    val name = case functiondefs of
			   nil => DynException.stdException ("empty function definition encountered", "Desugar.desugar_def.DEFFUN", Logger.INTERNAL)
			 | (header as {name, ...}, _)::_ => name
	    val _ = if not (List.all (fn({name=n, ...}, _) => n = name) functiondefs) then
			Logger.log_usererror [pos] (Printer.$("Mutually recursive function definitions must share the same name"))
		    else
			()

	    val func = map (fn({name, args, return}, stms) => {name=name,
							       args=map desugar_arg args,
							       return=desugar_typ return,
							       stms=map desugar_stm stms,
							       closure=Env.new()}) 
			   functiondefs
	in
	    KEC.DEFINITION (KEC.DEFLOCAL(desugar_runmod runmod,
					 name,
					 KEC.DONTCARE,
					 KEC.RUNNABLE(func)),
			    pos)
	end
	
      | HLEC.DEFNAMESPACE {name, stms}
	=>
	KEC.DEFINITION(KEC.DEFLOCAL(KEC.REPLACE,
				    name,
				    KEC.DONTCARE,
				    KEC.NAMESPACEDEF {name=name, stms=map (fn(v,s) => (trans_visibility v, desugar_stm s)) stms}),
		       pos)

      | HLEC.DEFINTERFACE {name, headers}
	=> KEC.DEFINITION (KEC.DEFLOCAL(KEC.REPLACE, Symbol.symbol "STUBBEDOUT", KEC.DONTCARE, KEC.UNIT), pos)

      | HLEC.DEFPROTOCOL (runmod, header, stms)
	=> KEC.DEFINITION (KEC.DEFLOCAL(KEC.REPLACE, Symbol.symbol "STUBBEDOUT", KEC.DONTCARE, KEC.UNIT), pos)

      | HLEC.DEFCLASS {name, classheader={inheritance,interfaces}, methods}
	=>
	KEC.DEFINITION (KEC.DEFLOCAL(KEC.REPLACE, 
				     name, 
				     KEC.DONTCARE, 
				     KEC.CLASSDEF {name=name, 
						   members=map method2objmember methods, 
						   parent=case inheritance of
							      SOME parent => desugar_exp parent
							    | NONE => KEC.SYMBOL (Symbol.symbol "Object")}),
			pos)

      | HLEC.DEFGLOBAL (name, typepattern, exp) => 
	let
	    val tp = desugar_typepattern typepattern
	in
	    KEC.DEFINITION (KEC.DEFGLOBAL(KEC.REPLACE, name, tp, KEC.MAKEREF(tp, desugar_exp exp)), pos)
	end

      | HLEC.DEFLOCAL (name, typepattern, exp) => 
	let
	    val tp = desugar_typepattern typepattern
	in
	    KEC.DEFINITION (KEC.DEFLOCAL(KEC.REPLACE, name, tp, KEC.MAKEREF(tp, desugar_exp exp)), pos)
	end

      | HLEC.DEFCONST (name, typepattern, exp)
	=> KEC.DEFINITION (KEC.DEFLOCAL(KEC.REPLACE, name, desugar_typepattern typepattern, desugar_exp exp), pos)

      | HLEC.DEFPROPERTY (property as {name, read, write})
	=> KEC.DEFINITION (KEC.DEFLOCAL(KEC.REPLACE, name, 
					KEC.DONTCARE, 
					KEC.PROPERTYEXP (desugar_property property)),
			   pos)
	   
and method2objmember method =
    let
    in
	case method of
	    HLEC.CONSTRUCTOR {args, body}
	    =>
	    (KEC.OVERLOAD, KEC.CONSTRUCTOR {name=NONE,
					    init_methods=[{args=map desugar_arg args,
							   return=KEC.DONTCARE,
							   stms=map desugar_stm body,
							   closure=Env.new(),					    
							   super=KEC.CONSTANT(Symbol.symbol "#ToBeFilledInAtDef#", KEC.PUBLIC, KEC.LITERAL(KEC.CONSTBOOL false))}],
					    new_object=nil}) (* constructors are initially nilled.  The object they construct must be specified when CLASSDEF is handled in exec *)

	  | HLEC.METHODDEF (visibility, HLEC.DEFFUN (runmod, functiondefs))
	    =>
	    let
		val name = case functiondefs of
			       nil => DynException.stdException ("empty function definition encountered", "Desugar.desugar_def.DEFFUN", Logger.INTERNAL)
			     | (header as {name, ...}, _)::_ => name
		val _ = if not (List.all (fn({name=n, ...}, _) => n = name) functiondefs) then
			    Logger.log_usererror [] (Printer.$("Mutually recursive function definitions must share the same name in definition of " ^ (Symbol.name name)))
			else
			    ()

		val func = map (fn({name, args, return}, stms) => {name=name,
								   args=map desugar_arg args,
								   return=desugar_typ return,
								   stms=map desugar_stm stms,
								   closure=Env.new()}) 
			       functiondefs
	    in
		(desugar_runmod runmod,
		 KEC.METHOD(name,
			    trans_visibility visibility,
			    func))
		
	    end
	    
	  | HLEC.METHODDEF (visibility, HLEC.DEFPROTOCOL _)
	    =>
	    (KEC.REPLACE, KEC.CONSTANT (Symbol.symbol "stubbedout", trans_visibility visibility, KEC.UNIT))

	  | HLEC.METHODDEF (visibility, HLEC.DEFCLASS {name, classheader={inheritance,interfaces}, methods})
	    =>
	    (KEC.REPLACE,
	     KEC.CONSTANT (name, 
			   trans_visibility visibility, 
			   KEC.CLASSDEF {name=name, 
					 members=map method2objmember methods, 
					 parent=case inheritance of
						    SOME parent => desugar_exp parent
						  | NONE => KEC.SYMBOL (Symbol.symbol "Object")}))
	  | HLEC.METHODDEF (visibility, HLEC.DEFNAMESPACE {name, stms})
	    =>
	    (KEC.REPLACE, 
	     KEC.CONSTANT (name, 
			   trans_visibility visibility,
			   KEC.NAMESPACEDEF {name=name, stms=map (fn(v,s) => (trans_visibility v, desugar_stm s)) stms}))
	    
	    
	  | HLEC.METHODDEF (visibility, HLEC.DEFINTERFACE interface)
	    =>
	    (KEC.REPLACE, 	     
	     KEC.CONSTANT (Symbol.symbol "stubbedout", trans_visibility visibility, KEC.UNIT))

	  | HLEC.METHODDEF (visibility, HLEC.DEFCONST (name, typepattern, exp))
	    =>
	    (KEC.REPLACE, KEC.CONSTANT(name,
				       trans_visibility visibility,
				       desugar_exp exp))

	  | HLEC.METHODDEF (visibility, HLEC.DEFGLOBAL (name, typepattern, exp))
	    =>
	    (KEC.REPLACE, KEC.VAR {name = name, 
				   visibility = trans_visibility visibility,
				   typepattern = desugar_typepattern typepattern,
				   value = ref (desugar_exp exp)}) (*TODO: make this actually global*)

	  | HLEC.METHODDEF (visibility, HLEC.DEFLOCAL  (name, typepattern,  exp))
	    =>
	    (KEC.REPLACE, KEC.VAR {name = name, 
				   visibility = trans_visibility visibility,
				   typepattern = desugar_typepattern typepattern,
				   value = ref (desugar_exp exp)})
	    
	  | HLEC.METHODDEF (visibility,
			    HLEC.DEFPROPERTY (property as {name, read, write}))
	    =>
	    (KEC.REPLACE,
	     KEC.PROPERTY (trans_visibility visibility, desugar_property property))
			   
    end


and desugar_act (action, pos) =
    let
	val result = 
	    case action of
		HLEC.EXP (exp) 
		=> KEC.EXP (desugar_exp (exp))

	      | HLEC.IMPORT file
		=> KEC.IMPORT file

	      | HLEC.OPEN exp
		=> KEC.OPEN {obj=desugar_exp exp, excludes=nil, include_privates=false}

	      | HLEC.ASSIGN (dest, source)
		=> KEC.ASSIGN (desugar_exp dest, desugar_exp source)

	      | HLEC.COND {cond, ift, iff}
		=> KEC.EXP (KEC.IFEXP{cond=desugar_exp cond, ift=KEC.STMS (map desugar_stm ift), iff=KEC.STMS (map desugar_stm iff)})

	      | HLEC.WHILE {cond, stms}
		=> KEC.EXP (desugar_exp 
				(HLEC.LET {vals=[([Symbol.symbol "#while"], 
						  HLEC.LAMBDA {args=[], 
							       body=HLEC.IFEXP {cond=cond, 
										ift=HLEC.STMS (stms @ [HLEC.ACTION (HLEC.EXP (HLEC.APPLY {func=HLEC.SYMBOL (Symbol.symbol "#while"), 
																	  args=HLEC.UNIT}), PosLog.NOPOS)]),
										iff=HLEC.UNIT}})],
					   body=HLEC.APPLY{func=HLEC.SYMBOL (Symbol.symbol "#while"),
							   args=HLEC.UNIT}}))
		   
	      | HLEC.FOR {var, collection, stms}
		=> KEC.EXP (KEC.APPLY {func=KEC.SYMBOL (Symbol.symbol "app"),
				       args=KEC.TUPLE [desugar_exp collection, KEC.LAMBDA {args=[var],
											   body=KEC.STMS (map desugar_stm stms),
											   undefined_args_allowed=false,
											   closure=Env.new()}]})
		   
    in
	KEC.ACTION (result, pos)
    end

and desugar_stm stm =
    case stm of
	HLEC.DEFINITION (definition, pos)
	=> desugar_def (definition, pos)
      
      | HLEC.ACTION (action, pos)
	=> desugar_act (action, pos)


    

fun hlec2kec stms =
    map desugar_stm stms

end
