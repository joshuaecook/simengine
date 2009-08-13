structure ASTTrans =
struct

open Printer

fun flatten x = foldr (op @) nil x

fun error msg =
    (Logger.log_usererror (PosLog.new()) msg;
     DynException.setErrored())

fun internalerror msg =
    (Logger.log_failure (Printer.$ msg))


fun trans_pattern patt = 
    case patt of
	Ast.TYPE typ
	=> HLEC.TYPE typ
      | Ast.COMPOUNDTYPE (typ, subtypes)
	=> HLEC.COMPOUNDTYPE (typ, trans_pattern subtypes)
      | Ast.ARROW (typ1, typ2)
	=> HLEC.ARROW (trans_pattern typ1, trans_pattern typ2)
      | Ast.TUPLETYPE types
	=> HLEC.TUPLETYPE (map trans_pattern types)
      | Ast.UNITTYPE
	=> HLEC.UNITTYPE
      | Ast.DONTCARE 
	=> HLEC.DONTCARE

fun trans_optpattern opatt =
    case opatt of
	SOME patt => SOME (trans_pattern patt)
      | NONE => NONE

fun trans_header {name, args, returns} =
    {name=name,
     args = map (fn(arg, patt) => (arg, trans_optpattern patt)) args,
     return = case returns of
		   SOME returns => SOME (HLEC.TUPLETYPE (map (fn(arg, patt) => (case patt of SOME patt => trans_pattern patt | NONE => HLEC.DONTCARE)) returns)) (*TODO: integrate with josh's changes *)
		 | NONE => NONE}

fun trans_visibility (NONE) = HLEC.PUBLIC
  | trans_visibility (SOME Ast.PUBLIC) = HLEC.PUBLIC
  | trans_visibility (SOME Ast.HIDDEN) = HLEC.HIDDEN

fun trans_method method =
    case method of
	Ast.METHODDEF (v, def)
	=> map (fn(def') => HLEC.METHODDEF (trans_visibility v, def')) (trans_definition def)

      | Ast.CONSTRUCTOR {args, body}
	=> HLEC.CONSTRUCTOR {args=map (fn(arg, patt) => (arg, trans_optpattern patt)) args,
			     body=trans_stms body}
	   :: nil

and trans_exp (exp : Ast.exp) : HLEC.exp =
    case exp of
	Ast.LITERAL (Ast.CONSTREAL r)
	=> HLEC.LITERAL(HLEC.CONSTREAL r)

      | Ast.LITERAL (Ast.CONSTBOOL b)
	=> HLEC.LITERAL(HLEC.CONSTBOOL b)

      | Ast.LITERAL (Ast.CONSTSTR s)
	=> HLEC.LITERAL(HLEC.CONSTSTR s)

      | Ast.STRINGEXP exps
	=> HLEC.STRINGEXP (map trans_exp exps)

      | Ast.LITERAL (Ast.CONSTBINARY b)
	=> HLEC.LITERAL (HLEC.CONSTBINARY b)

      | Ast.SYMBOL s
	=> HLEC.SYMBOL s

      | Ast.ASSERTION exp
	=> HLEC.ASSERTION (trans_exp exp)

      | Ast.LIBFUN (name, exp)
	=> HLEC.LIBFUN (name, trans_exp exp)

      | Ast.LAMBDA {args, body}
	=> HLEC.LAMBDA {args=args, body=trans_exp body}

      | Ast.APPLY {func, args}
	=> HLEC.APPLY {func=trans_exp func, args=trans_exp args}

      | Ast.IFEXP {cond, ift, iff}
	=> HLEC.IFEXP {cond=trans_exp cond,
		       ift=trans_exp ift,
		       iff=trans_exp iff}

      | Ast.VECTOR exps
	=> 
	if List.exists (fn(e) => case e of Ast.NAMEDPATTERN _ => true | _ => false) exps then
	    HLEC.APPLY{func=HLEC.SEND {message=Symbol.symbol "new", object=HLEC.SYMBOL (Symbol.symbol "VectorPattern")},
		       args=HLEC.APPLY{func=HLEC.SEND{message=Symbol.symbol "clone", 
						      object=(HLEC.VECTOR (map trans_exp exps))},
				       args=HLEC.UNIT}}
	else
	    HLEC.APPLY{func=HLEC.SEND{message=Symbol.symbol "clone", 
				      object=(HLEC.VECTOR (map trans_exp exps))},
		       args=HLEC.UNIT}

      | Ast.NAMEDPATTERN (id, exp)
	=> HLEC.APPLY{func=HLEC.SEND {message=Symbol.symbol "new", object=HLEC.SYMBOL (Symbol.symbol "Pattern")},
		      args=HLEC.TUPLE [HLEC.LITERAL(HLEC.CONSTSTR (Symbol.name id)),
				       trans_exp exp]}

      | Ast.TUPLE exps 
	=> HLEC.TUPLE (map trans_exp exps)

      | Ast.WILDCARD
	=> HLEC.APPLY{func=HLEC.SEND{message=Symbol.symbol "new", object=HLEC.SYMBOL (Symbol.symbol "Wildcard")},
		      args=HLEC.UNIT}

      | Ast.UNIT
	=> HLEC.UNIT

      | Ast.UNDEFINED
	=> HLEC.UNDEFINED

(*      | Ast.OBJECT (methods)
	=> HLEC.OBJECT (map trans_method methods)*)

      | Ast.SEND {message, object}
	=> HLEC.SEND {message=message,
		      object=trans_exp object}

      | Ast.SATISFIES {class, interface}
	=> HLEC.SATISFIES {class=trans_exp class, interface=trans_exp interface}

      | Ast.ERROR msg
	=> HLEC.ERROR (trans_exp msg)

      | Ast.POS (exp, pos)
	=> HLEC.POS (trans_exp exp, pos)

      | Ast.AND items
	=> HLEC.AND (map trans_exp items)

      | Ast.OR items
	=> HLEC.OR (map trans_exp items)

      | Ast.TYPEEXP pattern
	=> HLEC.TYPEEXP (trans_pattern pattern)

      | Ast.FORGENERATOR (forclauses, exp) 
	=> 
	let
	    fun trans_generator {var, collection, condition} =
		{var=var,
		 collection=trans_exp collection,
		 condition=GeneralUtil.applyOpt trans_exp condition}
	in
	    HLEC.FORGENERATOR (map trans_generator forclauses, trans_exp exp)
	end
      | Ast.FORALL {var, collection, test}
	=> 
	HLEC.FORALL {var=var, collection=trans_exp collection, test=trans_exp test}

      | Ast.EXISTS {var, collection, test}
	=> 
	HLEC.EXISTS {var=var, collection=trans_exp collection, test=trans_exp test}
	
      | Ast.TABLE (entries)
	=>
	let
	    fun trans_entry (name, exp) = (name, trans_exp exp)
	in
	    HLEC.TABLE (map trans_entry entries)
	end

      | Ast.LET (name, exp1, exp2)
	=>
	HLEC.LET{vals=[([name], trans_exp exp1)], body=trans_exp exp2}

      | Ast.RULEMATCH {find, conds, replace}
	=>
	HLEC.APPLY{func=HLEC.SEND {message=(Symbol.symbol "new"), object=HLEC.SYMBOL (Symbol.symbol "Rule")},
		   args=HLEC.TUPLE [trans_exp find,
				    trans_exp conds,
				    trans_exp replace]}

and trans_interface header =
    case header of
	Ast.FUNHEADER header
	=> HLEC.FUNHEADER (trans_header header)

      | Ast.CONSHEADER args 
	=> HLEC.CONSHEADER (map (fn(arg, patt) => (arg, trans_optpattern patt)) args)

and trans_runmod (SOME Ast.OVERLOAD) = HLEC.OVERLOAD
  | trans_runmod (SOME Ast.REPLACE) = HLEC.REPLACE
  | trans_runmod NONE = HLEC.REPLACE

and trans_definition definition =
    case definition of
	Ast.DEFFUN (runmod, funs)
	=> HLEC.DEFFUN (trans_runmod runmod, map (fn(header, stms) => (trans_header header, trans_stms stms)) funs)
	   :: nil

      | Ast.DEFPROTOCOL (runmod, header, stms)
	=> HLEC.DEFPROTOCOL (trans_runmod runmod, trans_header header, trans_stms stms)
	   :: nil

      | Ast.DEFCLASS {name, classheader={inheritance, interfaces}, methods}
	=> HLEC.DEFCLASS {name=name, 
			  classheader={inheritance=case inheritance of 
						       SOME inheritance => SOME (trans_exp inheritance)
						     | NONE => NONE, 
				       interfaces=interfaces},
			  methods=flatten (map trans_method methods)}
	   :: nil

      | Ast.DEFNAMESPACE {name, stms}
	=> 
	let
	    fun flattenStms nil = nil
	      | flattenStms ((v,stms)::rest) = 
		(map (fn(s) => (v,s)) stms) @ (flattenStms rest)

	in
		HLEC.DEFNAMESPACE {name=name,
				   stms=flattenStms (map (fn(v,s) => (trans_visibility v, trans_stm s)) stms)}
		:: nil
	end

      | Ast.DEFINTERFACE {name, headers}
	=> HLEC.DEFINTERFACE {name=name,
			      headers= map trans_interface headers}
	   :: nil

      | Ast.DEFGLOBAL (id, pattern, exp)
	=> HLEC.DEFGLOBAL (id, 
			   case pattern of 
			       NONE => HLEC.DONTCARE
			     | SOME pattern => trans_pattern pattern, 
			   case exp of
			       SOME exp => trans_exp exp
			     | NONE => HLEC.UNDEFINED)
	   :: nil

      | Ast.DEFLOCAL (id, pattern, exp)
	=> HLEC.DEFLOCAL (id, 
			  case pattern of 
			      NONE => HLEC.DONTCARE
			    | SOME pattern => trans_pattern pattern, 
			  case exp of
			       SOME exp => trans_exp exp
			     | NONE => HLEC.UNDEFINED)
	   :: nil

      | Ast.DEFCONST (name, pattern, exp) 
	=> HLEC.DEFCONST(name,
			 case pattern of 
			     NONE => HLEC.DONTCARE
			   | SOME pattern => trans_pattern pattern, 
			 trans_exp exp)
	   :: nil

      | Ast.DEFENUM {name=enumname, parent, args}
	=> 
	let
	    open Printer
		 
	    fun str s = HLEC.LITERAL (HLEC.CONSTSTR s)
	    fun num i = HLEC.LITERAL (HLEC.CONSTREAL (Real.fromInt i))

	    fun build_enum_def ((name, NONE), (index, defs)) =
		(index+1, 
		 defs @ [HLEC.DEFLOCAL (name, HLEC.DONTCARE, HLEC.APPLY{func=HLEC.SEND {message=Symbol.symbol "new", 
											object=HLEC.SYMBOL (Symbol.symbol "Enum")},
									args=HLEC.TUPLE [str (Symbol.name name), str (Symbol.name enumname), num index]})])
	      | build_enum_def ((name, SOME i), (index, defs)) =
		if i < index then
		    (index+1,
		     defs)
		    before error ($("Indices in enumeration " ^ (Symbol.name enumname) ^ " at " ^ (Symbol.name name) ^ " must have increasing index.  Previous was " ^ (Int.toString index)))
		else
		    (i+1,
		     defs @ [HLEC.DEFLOCAL (name, HLEC.DONTCARE, HLEC.APPLY{func=HLEC.SEND {message=Symbol.symbol "new", 
											    object=HLEC.SYMBOL (Symbol.symbol "Enum")},
									    args=HLEC.TUPLE [str (Symbol.name name), str (Symbol.name enumname), num i]})])
	in
	    (* for each arg, define it*)
	    (#2 (foldl build_enum_def (0, nil) args))
	    (* define overall enumeration *)
	    @[HLEC.DEFLOCAL (enumname, HLEC.DONTCARE, HLEC.VECTOR (map (fn(a) => HLEC.SYMBOL (#1 a)) args))]
	end

      | Ast.DEFMODEL {header, parts}
	=>
	let
	    val {name, args, returns} = header

	    fun build_stm part =
		case part of
		    Ast.SUBMODELDEF (def as Ast.DEFMODEL {header={name, ...}, ...})
		    => 
		    (map (fn(d) => HLEC.DEFINITION (d, PosLog.NOPOS)) (trans_definition def))
		    @ [HLEC.ACTION(HLEC.EXP(HLEC.APPLY{func=HLEC.SEND{object=HLEC.SYMBOL (Symbol.symbol "self"), message=Symbol.symbol "addConst"},  
								     args=HLEC.TUPLE [HLEC.LITERAL(HLEC.CONSTSTR (Symbol.name name)),
										      HLEC.SYMBOL (name)]}),
				   PosLog.NOPOS)]
			

		  | Ast.SUBMODELDEF _
		    => (internalerror "Unexpected model definition component found";
			[])

		  | Ast.SUBMODELINST {class, name, opttable, optdimensions}
		    => 
		    [HLEC.ACTION(HLEC.EXP (HLEC.APPLY{func=HLEC.SYMBOL (Symbol.symbol "instantiateSubModel"),
						      args=HLEC.TUPLE [HLEC.SYMBOL class,
								       HLEC.LITERAL (HLEC.CONSTSTR (Symbol.name name)),
								       case opttable of
									   NONE => HLEC.TABLE []
									 | SOME table => trans_exp table,
								       HLEC.VECTOR (case optdimensions of
											NONE => []
										      | SOME dims => map (fn(s) => HLEC.SYMBOL s) dims)]}), 
				 PosLog.NOPOS),
		     
		     HLEC.DEFINITION(HLEC.DEFCONST (name, HLEC.DONTCARE, HLEC.SEND{message=name,
										   object=HLEC.SYMBOL (Symbol.symbol "self")}),
				     PosLog.NOPOS)]

		  | Ast.OUTPUTDEF {name, quantity, settings} 
 		    =>  
		    let 
			fun outerror () =
			    error ($("Output " ^ (Symbol.name name) ^ " does not appear as a returned quantity in the model header"))
			    
			val _ = case returns of
				    NONE => outerror()
				  | SOME rets => if (not (List.exists (fn(s,_) => s = name) rets)) then
						     outerror ()
						 else
						     ()

			val obj = HLEC.APPLY{func=HLEC.SEND{object=HLEC.SYMBOL (Symbol.symbol "Output"),
							    message=Symbol.symbol "new"},
					     args=HLEC.TUPLE [HLEC.LITERAL(HLEC.CONSTSTR (Symbol.name name)), 
							      trans_exp quantity]}

			val obj = case settings of
				      NONE => obj
				    | SOME settings =>
				      HLEC.APPLY{func=obj,
						 args=HLEC.TUPLE[trans_exp settings]}
		    in
			[HLEC.ACTION(HLEC.EXP (HLEC.APPLY{func=HLEC.SEND{message=Symbol.symbol "add",
									 object=HLEC.SYMBOL (Symbol.symbol "outputDefs")},
							  args=HLEC.TUPLE [HLEC.LITERAL(HLEC.CONSTSTR (Symbol.name name)), 
									   HLEC.LAMBDA{args=[], body=obj}]}),
				     PosLog.NOPOS)] 
		    end
		  | Ast.INPUTDEF {name, settings} 
 		    =>  
		    let 
			fun inerror () =
			    error ($("Input " ^ (Symbol.name name) ^ " does not appear in the model header"))
			    
			val _ = if (not (List.exists (fn(s,_) => s = name) args)) then
				    inerror ()
				else
				    ()
		    in
			(case settings of
			     NONE => []
			   | SOME settings =>
			     let
			     (*TODO: add check that name is actually an input in this system*)
			     in
				 [HLEC.ACTION (HLEC.EXP (HLEC.APPLY {func=HLEC.SYMBOL name,
								     args=HLEC.TUPLE[trans_exp settings]}),
					       PosLog.NOPOS)]
			     end)
		    end

		  | Ast.ITERATORDEF {name, value, settings} 
 		    =>  
		    [HLEC.DEFINITION(HLEC.DEFCONST (name, HLEC.DONTCARE, HLEC.APPLY{func=HLEC.SEND{object=HLEC.SYMBOL (Symbol.symbol "SimIterator"),
												   message=Symbol.symbol "new"},
										    args=HLEC.TUPLE [HLEC.LITERAL(HLEC.CONSTSTR (Symbol.name name))]}),
				     PosLog.NOPOS),
		     HLEC.ACTION (HLEC.EXP(HLEC.APPLY{func=HLEC.SEND{object=HLEC.SYMBOL(Symbol.symbol "self"), message=Symbol.symbol "addConst"},  
						      args=HLEC.TUPLE[HLEC.LITERAL(HLEC.CONSTSTR (Symbol.name name)),
									  HLEC.SYMBOL name]}),
				  PosLog.NOPOS),
		     HLEC.ACTION(HLEC.EXP (HLEC.APPLY{func=HLEC.SEND{message=Symbol.symbol "push_back",
								     object=HLEC.SYMBOL (Symbol.symbol "iterators")}, 
						      args=HLEC.TUPLE[HLEC.SYMBOL name]}), PosLog.NOPOS)]
		    @ (case value of
			  SOME value => 
			  [HLEC.ACTION(HLEC.EXP(HLEC.APPLY{func=HLEC.SEND{object=HLEC.SYMBOL(name), message=Symbol.symbol "setValue"},  
							   args=HLEC.TUPLE[trans_exp value]}),
				       PosLog.NOPOS)]
			| NONE => [])
		    @ (case settings of
			     NONE => []
			   | SOME settings =>
			     [HLEC.ACTION (HLEC.EXP (HLEC.APPLY {func=HLEC.SYMBOL name,
								 args=HLEC.TUPLE[trans_exp settings]}),
					   PosLog.NOPOS)])

		  | Ast.QUANTITYDEF {modifiers, basetype, name, precision, exp, settingstable, dimensions}
		    => 
		    (* create quantity *)
		    let 
		    (* set basetype *)
			val baseclass = case basetype of
					    Ast.GENERIC_QUANTITY => "SimQuantity"
					  | Ast.STATE_QUANTITY => "State"
					  | Ast.PARAMETER_QUANTITY => "Parameter"
		    in
			[HLEC.DEFINITION(HLEC.DEFCONST (name, HLEC.DONTCARE, HLEC.APPLY{func=HLEC.SEND{object=HLEC.SYMBOL (Symbol.symbol baseclass),
												       message=Symbol.symbol "new"},
											args=HLEC.TUPLE [HLEC.LITERAL(HLEC.CONSTSTR (Symbol.name name))]}),
					 PosLog.NOPOS),
			 HLEC.ACTION (HLEC.EXP(HLEC.APPLY{func=HLEC.SEND{object=HLEC.SYMBOL(Symbol.symbol "self"), message=Symbol.symbol "addConst"},  
							  args=HLEC.TUPLE[HLEC.LITERAL(HLEC.CONSTSTR (Symbol.name name)),
									  HLEC.SYMBOL name]}),
				      PosLog.NOPOS),
			 
			 HLEC.ACTION(HLEC.EXP (HLEC.APPLY{func=HLEC.SEND{message=Symbol.symbol "push_back",
									 object=HLEC.SYMBOL (Symbol.symbol "quantities")}, 
							  args=HLEC.TUPLE[HLEC.SYMBOL name]}), PosLog.NOPOS)]
		    end
		    
		    (* set modifiers*)
		    @ 
		    let
			fun set_modifier modifier =
			    case modifier of
				Ast.VISIBLE => 
				HLEC.ACTION (HLEC.EXP(HLEC.APPLY{func=HLEC.SEND{object=HLEC.SYMBOL name, message=(Symbol.symbol "setIsVisible")}, 
								 args=HLEC.TUPLE [HLEC.LITERAL(HLEC.CONSTBOOL true)]}), 
					     PosLog.NOPOS)
			      | Ast.TUNABLE =>
				HLEC.ACTION (HLEC.EXP(HLEC.APPLY{func=HLEC.SEND{object=HLEC.SYMBOL name, message=(Symbol.symbol "setIsTunable")}, 
								 args=HLEC.TUPLE [HLEC.LITERAL(HLEC.CONSTBOOL true)]}), 
					     PosLog.NOPOS)
			      | Ast.STATEFUL =>
				HLEC.ACTION (HLEC.EXP(HLEC.APPLY{func=HLEC.SEND{object=HLEC.SYMBOL name, message=(Symbol.symbol "setIsIterable")}, 
								 args=HLEC.TUPLE [HLEC.LITERAL(HLEC.CONSTBOOL true)]}), 
					     PosLog.NOPOS)
		    in
			map set_modifier modifiers
		    end
		    (* set precision*)
		    @ (case precision of
			   NONE =>
			   [HLEC.ACTION (HLEC.EXP(HLEC.APPLY{func=HLEC.SEND{object=HLEC.SYMBOL name, message=Symbol.symbol "setPrecision"}, 
							     args=HLEC.TUPLE [HLEC.APPLY{func=HLEC.SEND{object=HLEC.SYMBOL (Symbol.symbol "InfinitePrecision"), message=Symbol.symbol "new"}, args=HLEC.UNIT}]}), 
					 PosLog.NOPOS)]
			 | SOME exp =>
			   [HLEC.ACTION (HLEC.EXP(HLEC.APPLY{func=HLEC.SEND{object=HLEC.SYMBOL name, message=Symbol.symbol "setPrecision"}, 
							     args=HLEC.TUPLE [trans_exp exp]}), 
					 PosLog.NOPOS)])
		  (* set dimensions *)
		    @ (case dimensions of
			   NONE => []
			 | SOME dims => [HLEC.ACTION (HLEC.EXP(HLEC.APPLY{func=HLEC.SEND{object=HLEC.SYMBOL name, message=Symbol.symbol "setDimensions"},
									  args=HLEC.TUPLE [trans_exp (Ast.VECTOR (map (fn(s) => Ast.LITERAL (Ast.CONSTSTR (Symbol.name s))) dims))]}),
						      PosLog.NOPOS)])		  (* set initial value exp *)
		    @ (case exp of
			   NONE => []
			 | SOME exp => [HLEC.ACTION (HLEC.EXP(HLEC.APPLY{func=HLEC.SEND{object=HLEC.SYMBOL name, message=Symbol.symbol "setInitialValue"}, 
									 args=HLEC.TUPLE [trans_exp exp]}), 
						     PosLog.NOPOS)])

		  (* apply table *)
		    @ (case settingstable of
			   NONE => []
			 | SOME table => [HLEC.ACTION (HLEC.EXP(HLEC.APPLY{func=HLEC.SYMBOL name,
									   args=HLEC.TUPLE [trans_exp table]}),
						       PosLog.NOPOS)])
		    

		  | Ast.STM (stm)
		    => (trans_stm stm)
		    (* if it is a definition, add it to the object *)
		    @
		    let
			fun def2nameAndStorage (Ast.DEFFUN (_, (({name, ...}, _)::_))) =
			    [HLEC.ACTION(HLEC.EXP(HLEC.APPLY{func=HLEC.SEND{object=HLEC.SYMBOL (Symbol.symbol "self"), message=(Symbol.symbol "addMethod")},
							     args=HLEC.TUPLE [HLEC.LITERAL(HLEC.CONSTSTR (Symbol.name name)),
									      HLEC.SYMBOL name]}),
					 PosLog.NOPOS)]
			  | def2nameAndStorage (Ast.DEFFUN (_, nil)) =
			    DynException.stdException ("empty function definition encountered", "AstTrans.trans_definition.model.STM.def2nameAndStorage.DEFFUN", Logger.INTERNAL)
			  | def2nameAndStorage (Ast.DEFPROTOCOL (_, {name, ...}, _)) =
			    [HLEC.ACTION(HLEC.EXP(HLEC.APPLY{func=HLEC.SEND{object=HLEC.SYMBOL(Symbol.symbol "self"), message=Symbol.symbol "addMethod"},
							     args=HLEC.TUPLE [HLEC.LITERAL(HLEC.CONSTSTR (Symbol.name name)),
									      HLEC.SYMBOL name]}),
					 PosLog.NOPOS)]
			  | def2nameAndStorage (Ast.DEFCLASS {name, ...}) =
			    [HLEC.ACTION(HLEC.EXP(HLEC.APPLY{func=HLEC.SEND{object=HLEC.SYMBOL(Symbol.symbol "self"), message=Symbol.symbol "addConst"},
							     args=HLEC.TUPLE [HLEC.LITERAL(HLEC.CONSTSTR (Symbol.name name)),
									      HLEC.SYMBOL name]}),
					 PosLog.NOPOS)]
			  | def2nameAndStorage (Ast.DEFNAMESPACE {name, ...}) =
			    [HLEC.ACTION(HLEC.EXP(HLEC.APPLY{func=HLEC.SEND{object=HLEC.SYMBOL(Symbol.symbol "self"), message=Symbol.symbol "addConst"},
							     args=HLEC.TUPLE [HLEC.LITERAL(HLEC.CONSTSTR (Symbol.name name)),
									      HLEC.SYMBOL name]}),
					 PosLog.NOPOS)]
			  | def2nameAndStorage (Ast.DEFINTERFACE {name, ...}) =
			    [HLEC.ACTION(HLEC.EXP(HLEC.APPLY{func=HLEC.SEND{object=HLEC.SYMBOL(Symbol.symbol "self"), message=Symbol.symbol "addConst"},
							     args=HLEC.TUPLE [HLEC.LITERAL(HLEC.CONSTSTR (Symbol.name name)),
									      HLEC.SYMBOL name]}),
					 PosLog.NOPOS)]
			  | def2nameAndStorage (Ast.DEFGLOBAL (name, _, _)) = [] (*TODO: what does this mean? is this an error? *)
			  | def2nameAndStorage (Ast.DEFLOCAL (name, _, _)) =
			    [HLEC.ACTION(HLEC.EXP(HLEC.APPLY{func=HLEC.SEND{object=HLEC.SYMBOL(Symbol.symbol "self"), message=Symbol.symbol "addVar"},
							     args=HLEC.TUPLE [HLEC.LITERAL(HLEC.CONSTSTR (Symbol.name name))]}),
					 PosLog.NOPOS),
			     HLEC.ACTION (HLEC.ASSIGN (HLEC.SEND {object=HLEC.SYMBOL (Symbol.symbol "self"), message=name}, HLEC.SYMBOL name),
 					  PosLog.NOPOS)
			    ]
			  | def2nameAndStorage (Ast.DEFENUM {name, ...}) =
			    [HLEC.ACTION(HLEC.EXP(HLEC.APPLY{func=HLEC.SEND{object=HLEC.SYMBOL(Symbol.symbol "self"), message=Symbol.symbol "addConst"},
							     args=HLEC.TUPLE [HLEC.LITERAL(HLEC.CONSTSTR (Symbol.name name)),
									      HLEC.SYMBOL name]}),
					 PosLog.NOPOS)]
			  | def2nameAndStorage (Ast.DEFCONST (name, _, _)) =
			    [HLEC.ACTION(HLEC.EXP(HLEC.APPLY{func=HLEC.SEND{object=HLEC.SYMBOL(Symbol.symbol "self"), message=Symbol.symbol "addConst"},
							     args=HLEC.TUPLE [HLEC.LITERAL(HLEC.CONSTSTR (Symbol.name name)),
									      HLEC.SYMBOL name]}),
					 PosLog.NOPOS)]
			  | def2nameAndStorage (Ast.DEFMODEL {header={name, ...}, ...}) =
			    [HLEC.ACTION(HLEC.EXP(HLEC.APPLY{func=HLEC.SEND{object=HLEC.SYMBOL(Symbol.symbol "self"), message=Symbol.symbol "addConst"},
							     args=HLEC.TUPLE [HLEC.LITERAL(HLEC.CONSTSTR (Symbol.name name)),
									      HLEC.SYMBOL name]}),
					 PosLog.NOPOS)]
			  | def2nameAndStorage (Ast.INSTMODEL {name, ...}) =
			    [HLEC.ACTION(HLEC.EXP(HLEC.APPLY{func=HLEC.SEND{object=HLEC.SYMBOL(Symbol.symbol "self"), message=Symbol.symbol "addConst"},
							     args=HLEC.TUPLE [HLEC.LITERAL(HLEC.CONSTSTR (Symbol.name name)),
									      HLEC.SYMBOL name]}),
					 PosLog.NOPOS)]

			  | def2nameAndStorage (Ast.DEFPROPERTY {name, ...}) =
			    [HLEC.ACTION(HLEC.EXP(HLEC.APPLY{func=HLEC.SEND{object=HLEC.SYMBOL(Symbol.symbol "self"), message=Symbol.symbol "addConst"},
							     args=HLEC.TUPLE [HLEC.LITERAL(HLEC.CONSTSTR (Symbol.name name)),
									      HLEC.SYMBOL name]}),
					 PosLog.NOPOS)]

		    in
			case stm of
			    Ast.ACTION _ => []
			  | Ast.DEFINITION (def, _) => def2nameAndStorage def
										    
		    end	

	    fun build_input (name, pattern) =
		[HLEC.DEFINITION (HLEC.DEFLOCAL(name, HLEC.DONTCARE, HLEC.APPLY{func=HLEC.SEND{message=Symbol.symbol "new", object=HLEC.SYMBOL(Symbol.symbol "Input")},
										args=HLEC.TUPLE[HLEC.LITERAL (HLEC.CONSTSTR (Symbol.name name))]}),
				  PosLog.NOPOS),
		 HLEC.ACTION(HLEC.EXP(HLEC.APPLY{func=HLEC.SEND{object=HLEC.SYMBOL(Symbol.symbol "self"), message=Symbol.symbol "addConst"},
						 args=HLEC.TUPLE [HLEC.LITERAL(HLEC.CONSTSTR (Symbol.name name)),
								  HLEC.SYMBOL name]}),
			     PosLog.NOPOS),
		 HLEC.ACTION (HLEC.EXP (HLEC.APPLY{func=HLEC.SEND{message=Symbol.symbol "push_back",
								  object=HLEC.SYMBOL(Symbol.symbol "inputs")},
						   args=HLEC.TUPLE[HLEC.SYMBOL (name)]}),
			      PosLog.NOPOS)]
	
	    fun build_output (name, pattern) =
		HLEC.ACTION (HLEC.EXP (HLEC.APPLY{func=HLEC.SYMBOL (Symbol.symbol "buildOutput"),
						  args=HLEC.TUPLE [HLEC.LITERAL (HLEC.CONSTSTR (Symbol.name name))]}),
			     PosLog.NOPOS)
	    val modelstms = 
		(* set name *)
		HLEC.METHODDEF(HLEC.PUBLIC, HLEC.DEFLOCAL(Symbol.symbol "name", HLEC.DONTCARE, HLEC.LITERAL(HLEC.CONSTSTR (Symbol.name name))))
		:: [] 

	    val template_constructor_stms = 
 		(flatten (map build_input args))
		@ (flatten(map build_stm parts))
		@ (case returns of
		       SOME returns => map build_output returns
		     | NONE => [])

	    val templateconstructor = 
		HLEC.CONSTRUCTOR {args=[], 
	    			  body= (HLEC.ACTION(HLEC.EXP (HLEC.APPLY {func=HLEC.SYMBOL (Symbol.symbol "super"),
	    								   args=HLEC.UNIT}), PosLog.NOPOS))
	    				:: (template_constructor_stms)}

	    val templatename = Symbol.symbol ((Symbol.name name) ^ "Template")

	    val hiddenModelTemplateDef =
		HLEC.DEFINITION(HLEC.DEFCLASS {name=templatename,
					       classheader={inheritance=SOME (HLEC.SYMBOL (Symbol.symbol "Model")),  
							    interfaces=[]}, 
					       methods= templateconstructor :: modelstms},					       
				PosLog.NOPOS)

	    val hiddenModelTemplate = 
		HLEC.DEFINITION(HLEC.DEFLOCAL (Symbol.symbol "template", HLEC.DONTCARE, HLEC.APPLY {func=HLEC.SEND{message=Symbol.symbol "new", object=HLEC.SYMBOL templatename},
												    args=HLEC.UNIT}),
				PosLog.NOPOS)



	    val wrapperMembers =
		let
		    fun makeVar (sym, typepattern) =
			HLEC.METHODDEF (HLEC.PUBLIC, HLEC.DEFLOCAL(sym, HLEC.DONTCARE, HLEC.UNDEFINED))
		in
		    map makeVar (case #returns header of SOME r => r | NONE => [])
		end
		@
		let
		    fun makeProperty (sym, typepattern) =
			let
			    val varsym = Symbol.symbol ((Symbol.name sym) ^ "_var")
			in
			    [HLEC.METHODDEF (HLEC.HIDDEN, HLEC.DEFLOCAL(varsym, HLEC.DONTCARE, HLEC.UNDEFINED)),
			     HLEC.METHODDEF (HLEC.PUBLIC, HLEC.DEFPROPERTY {name=sym,
									    read=SOME [HLEC.ACTION(HLEC.EXP(HLEC.SEND{message=varsym, object=HLEC.SYMBOL(Symbol.symbol "self")}), 
												   PosLog.NOPOS)],
									    write=SOME (Symbol.symbol "arg", 
											[HLEC.ACTION(HLEC.EXP(HLEC.APPLY{func=HLEC.SEND {message=Symbol.symbol "setInputVal", 
																	 object=HLEC.SEND {message=varsym, 
																			   object=HLEC.SYMBOL (Symbol.symbol "self")}},
															 args=HLEC.TUPLE [HLEC.SYMBOL (Symbol.symbol "arg")]}), 
												     PosLog.NOPOS)])})
			    ]
			end
			 
		in
		    flatten (map makeProperty (#args header))
		end
		@ 		(* assign template *)
		[HLEC.METHODDEF(HLEC.PUBLIC,
				HLEC.DEFLOCAL(Symbol.symbol "modeltemplate", HLEC.DONTCARE, HLEC.SYMBOL(Symbol.symbol "template")))]


	    

	    val wrapperConstructorStms = 		
                (* tie in inputs *)
		(flatten (map (fn(arg, patt) => [HLEC.ACTION (HLEC.ASSIGN (HLEC.SEND{message=Symbol.symbol ((Symbol.name arg) ^ "_var"), object=HLEC.SYMBOL (Symbol.symbol "self")},
									   HLEC.APPLY{func=HLEC.SEND{message=Symbol.symbol "new",
												     object=HLEC.SYMBOL (Symbol.symbol "InputBinding")},
										      args=HLEC.TUPLE[HLEC.SEND{message=arg, object=HLEC.SYMBOL(Symbol.symbol "modeltemplate")}]}),
							      PosLog.NOPOS),
						 HLEC.ACTION (HLEC.EXP (HLEC.APPLY{func=HLEC.SEND {message=Symbol.symbol "push_back",
												   object=HLEC.SYMBOL (Symbol.symbol "inputs")},
										   args=HLEC.TUPLE[HLEC.SEND{message=arg, object=HLEC.SYMBOL (Symbol.symbol "self")}]}),
							      PosLog.NOPOS)])
		     (#args header)))
		@ 
                (* create and tie in outputs *)
		(case #returns header of
		     NONE => nil
		   | SOME returns =>
		     flatten (map (fn(arg, patt) => (* if isdefined modeltemplate.arg then set it in outputbinding, push onto outputs, else error that it wasn't declared *)
				     [HLEC.ACTION (HLEC.ASSIGN (HLEC.SEND{message=arg, object=HLEC.SYMBOL (Symbol.symbol "self")},
								HLEC.APPLY{func=HLEC.SEND{message=Symbol.symbol "new",
											  object=HLEC.SYMBOL (Symbol.symbol "OutputBinding")},
									   args=HLEC.TUPLE[HLEC.LITERAL (HLEC.CONSTSTR (Symbol.name arg)),
											   HLEC.SEND{message=arg, object=HLEC.SEND{message=Symbol.symbol "outputs", object=HLEC.SYMBOL(Symbol.symbol "modeltemplate")}}]}),
						   PosLog.NOPOS),
				      HLEC.ACTION (HLEC.EXP (HLEC.APPLY{func=HLEC.SEND {message=Symbol.symbol "push_back",
											object=HLEC.SYMBOL (Symbol.symbol "outputs")},
									args=HLEC.TUPLE[HLEC.SEND{message=arg, object=HLEC.SYMBOL (Symbol.symbol "self")}]}),
						   PosLog.NOPOS)])
				  (returns)))

	    val wrapperConstructor = HLEC.CONSTRUCTOR {args=nil (*map (fn(arg, patt) => (arg, trans_optpattern patt)) (#args header)*), 
	    					       body= (HLEC.ACTION(HLEC.EXP (HLEC.APPLY {func=HLEC.SYMBOL (Symbol.symbol "super"),
	    											args=HLEC.UNIT}), PosLog.NOPOS))
	    						     :: (wrapperConstructorStms)}

	    val wrapperDef =
		HLEC.DEFINITION(HLEC.DEFCLASS {name=name,
					       classheader={inheritance=SOME (HLEC.SYMBOL (Symbol.symbol ("ModelInstance"))),  
							    interfaces=[]}, 
					       methods= wrapperMembers @ [wrapperConstructor]},					       
				PosLog.NOPOS)

	    val fakeConstructorStms1 =
		let
		    fun buildArg (sym, patt) =
			HLEC.SYMBOL sym
		in
		    [HLEC.DEFINITION(HLEC.DEFLOCAL (Symbol.symbol "model",
						    HLEC.DONTCARE,
						    HLEC.APPLY{func=HLEC.SEND{message=Symbol.symbol "new", 
									      object=HLEC.SYMBOL name},
							       args=HLEC.UNIT}), PosLog.NOPOS)] @
		    (map (fn(name, patt) => HLEC.ACTION(HLEC.EXP(HLEC.APPLY{func=HLEC.SEND{message=Symbol.symbol "setInputVal",
											   object=HLEC.SEND{message=name,
													    object=HLEC.SYMBOL (Symbol.symbol "model")}},
									    args=HLEC.TUPLE [HLEC.SYMBOL (name)]}), 
							PosLog.NOPOS))
			 (#args header))
		    @ [HLEC.ACTION(HLEC.EXP (HLEC.SYMBOL (Symbol.symbol "model")), PosLog.NOPOS)]
		end

	    val fakeConstructorStms2 =
		let
		    fun buildArg (sym, patt) =
			HLEC.SYMBOL sym
		in
		    [HLEC.ACTION(HLEC.EXP (HLEC.APPLY{func=HLEC.SEND{message=Symbol.symbol "new", 
								     object=HLEC.SYMBOL name},
						      args=HLEC.UNIT}), PosLog.NOPOS)]
		end

	    val fakeconstructor1 = HLEC.DEFINITION(HLEC.DEFFUN(HLEC.REPLACE, 
							  [({args=map (fn(arg, patt) => (arg, trans_optpattern patt)) (#args header), 
							     name=Symbol.symbol "createSubModel",
							     return=NONE},
							    fakeConstructorStms1)]),
					      PosLog.NOPOS)

	    val fakeconstructor2 = HLEC.DEFINITION(HLEC.DEFFUN(HLEC.REPLACE, 
							  [({args=nil, 
							     name=Symbol.symbol "instantiate",
							     return=NONE},
							    fakeConstructorStms2)]),
					      PosLog.NOPOS)

	in
	    [HLEC.DEFNAMESPACE {name=name,				
				stms=(HLEC.PUBLIC, hiddenModelTemplateDef) :: 
				     (HLEC.PUBLIC, hiddenModelTemplate) :: 
				     (HLEC.PUBLIC, wrapperDef):: 
				     (HLEC.PUBLIC, fakeconstructor1) :: 
				     (HLEC.PUBLIC, fakeconstructor2) :: 
				     nil}]
	end


      | Ast.INSTMODEL {name, exp} 
	=>
	HLEC.DEFLOCAL (name, 
		       HLEC.DONTCARE,
		       trans_exp exp)
	:: nil

      | Ast.DEFPROPERTY {name, io={read, write}}
	=> [HLEC.DEFPROPERTY {name=name, 
			      read=GeneralUtil.applyOpt (fn(stms) => flatten (map trans_stm stms)) read,
			      write=GeneralUtil.applyOpt (fn(s, stms) => (s, flatten(map trans_stm stms))) write}] 

and trans_action pos action =
    case action of
	Ast.EXP exp
	=> [HLEC.ACTION(HLEC.EXP (trans_exp exp), pos)]

      | Ast.IMPORT file
	=> [HLEC.ACTION(HLEC.IMPORT file, pos)]

      | Ast.OPEN exp
	=> [HLEC.ACTION(HLEC.OPEN (trans_exp exp), pos)]

      | Ast.ASSIGN (destination, source)
	=> [HLEC.ACTION(HLEC.ASSIGN(trans_exp destination, trans_exp source), pos)]

      | Ast.COND {cond, ift, iff}
	=> [HLEC.ACTION(HLEC.COND {cond=trans_exp cond, ift=trans_stms ift, iff=trans_stms iff}, pos)]

      | Ast.WHILE{cond, stms}
	=> [HLEC.ACTION(HLEC.WHILE{cond=trans_exp cond, stms=trans_stms stms}, pos)]

      | Ast.FOR{var, collection, stms}
	=> [HLEC.ACTION(HLEC.FOR{var=var, collection=trans_exp collection, stms=trans_stms stms}, pos)]

      | Ast.EQUATIONS eqs 
	=> 
	let
	    open Printer

	    exception Skip

	    fun trans_eq (Ast.MATHFUNCTION (funcexp, bodyexp)) =
		let
		    val (name, args) =
			case funcexp of
			    Ast.POS(Ast.APPLY{func=Ast.SYMBOL name,
					      args=Ast.UNIT}, pos)
			    => (name, [])
			  | Ast.POS(Ast.APPLY{func=Ast.SYMBOL name,
					      args=Ast.TUPLE args}, pos)
			    => (name, args)
			  | _ => (error ($"Error creating mathematical function: Invalid function header");
				  raise Skip)

		    fun exp2sym (Ast.SYMBOL a) =
			a
		      | exp2sym exp =
			(error ($"Invalid argument in mathematical function definition");
			 raise Skip)

		    val fun_name = HLEC.LITERAL (HLEC.CONSTSTR (Symbol.name name))
		    val fun_lambda = HLEC.LAMBDA {args=map exp2sym args, body=trans_exp bodyexp}

		    val new_fun = HLEC.SEND {message=Symbol.symbol "new", object=HLEC.SYMBOL (Symbol.symbol "MathFunction")}
		    val funobj = HLEC.APPLY {func=new_fun, args=HLEC.TUPLE [fun_name, fun_lambda]}
		in
		    [HLEC.DEFINITION(HLEC.DEFCONST(name, HLEC.DONTCARE, funobj),
				     pos)]
		end

	      | trans_eq (Ast.EQUATION (lhs, rhs)) =
		let
		    fun findSymbols exp =
			case exp of
			    Ast.SYMBOL s => [s]
			  | Ast.POS (exp, _) => findSymbols exp
			  | Ast.APPLY {func=Ast.SYMBOL s, args=Ast.TUPLE [_, e]} 
			    => if s = (Symbol.symbol "operator_deriv") then
				   findSymbols e
			       else
				   findSymbols Ast.UNDEFINED
			  | Ast.APPLY {func, args=Ast.TUPLE [Ast.VECTOR _]} 
			    => findSymbols func
			       
			  | _ => nil

		    fun findDimensions exp =
			case exp of
			    Ast.SYMBOL s => []
			  | Ast.POS (exp, _) => findDimensions exp
			  | Ast.APPLY {func=Ast.SYMBOL s, args=Ast.TUPLE [_, e]} 
			    => findDimensions e
			  | Ast.APPLY {func, args=Ast.TUPLE [Ast.VECTOR v]} 
			    => map trans_exp v			       
			  | _ => nil

		    val syms = findSymbols lhs

		    val sym = case syms of
				[sym] => sym
			      | _ =>
				(error ($"Malformed equation encountered: unexpected number of symbols on left hand side");
				 raise Skip)

		    val dimensions = findDimensions lhs
		in
		    [HLEC.ACTION(HLEC.EXP(HLEC.APPLY{func=HLEC.SYMBOL (Symbol.symbol "makeEquation"),
										 args=HLEC.TUPLE[HLEC.LITERAL(HLEC.CONSTSTR (Symbol.name sym)),
												 HLEC.VECTOR dimensions,
												 HLEC.LAMBDA{args=syms, body=trans_exp lhs},
												 HLEC.LAMBDA{args=syms, body=trans_exp rhs}]}),
				 pos),
		     HLEC.DEFINITION(HLEC.DEFCONST(sym, HLEC.DONTCARE, HLEC.SEND {object=HLEC.SYMBOL(Symbol.symbol "self"), message=sym}),
				     pos)]
		end


	    fun safe_trans_eq default eq =
		trans_eq eq
		handle Skip => default

	in
	    flatten(map (safe_trans_eq [HLEC.ACTION (HLEC.EXP HLEC.UNIT, pos)]) eqs)
	end

and trans_stm (stm:Ast.stm) : HLEC.stm list = 
    case stm of
	Ast.DEFINITION (def, pos)
	=> map (fn(def') => HLEC.DEFINITION (def', pos)) (trans_definition def)

      | Ast.ACTION (action, pos)
	=> trans_action pos action

and trans_stms stms =
    let
    in
	flatten (map trans_stm (List.filter filter_dead_stm stms))
    end

(* these stms are generated in the grammar by excess newlines *)
and filter_dead_stm (Ast.ACTION(Ast.EXP(Ast.SYMBOL (sym)), _)) =
    (Symbol.name sym) <> "###EMPTY"
  | filter_dead_stm _ = 
    true
	    
fun ast2hlec stm =
    if filter_dead_stm stm then	
	trans_stm stm
    else (* ignore empty stms generated by typing only a newline *)
	[HLEC.ACTION(HLEC.EXP(HLEC.UNIT), PosLog.NOPOS)]

end
