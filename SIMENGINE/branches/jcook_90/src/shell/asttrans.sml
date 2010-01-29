structure ASTTrans =
struct

open Printer

fun flatten x = foldr (op @) nil x

fun error msg =
    (Logger.log_error msg;
     DynException.setErrored())
fun internalerror msg =
    (Logger.log_failure (Printer.$ msg))


(* Returns a SYMBOL expression with a given name. *)
val sym = HLEC.SYMBOL o Symbol.symbol

(* Returns a LITERAL string representation of a given symbol. *)
val sym2strlit = HLEC.LITERAL o HLEC.CONSTSTR o Symbol.name
	    
(* Returns an object instance method or attribute. *)
fun send name object =    
    HLEC.SEND {message=Symbol.symbol name, object=object}

(* Returns a function application. *)
fun apply (f, nil) = HLEC.APPLY {func=f, args=HLEC.UNIT}
  | apply (f, a) = HLEC.APPLY {func=f, args=HLEC.TUPLE a}


(* Common shortcut functions *)
val self = sym "self"
val addConst = send "addConst" self
val addMethod = send "addMethod" self
val addVar = send "addVar" self
val getMember = send "getMember" self


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
		 defs @ 
		 [HLEC.DEFLOCAL (name, HLEC.DONTCARE, apply (send "new" (sym "Enum"), [sym2strlit name, sym2strlit enumname, num index]))])
	      | build_enum_def ((name, SOME i), (index, defs)) =
		if i < index then
		    (index+1,
		     defs)
		    before error ($("Indices in enumeration " ^ (Symbol.name enumname) ^ " at " ^ (Symbol.name name) ^ " must have increasing index.  Previous was " ^ (Int.toString index)))
		else
		    (i+1,
		     defs @ 
		     [HLEC.DEFLOCAL (name, HLEC.DONTCARE, apply (send "new" (sym "Enum"), [sym2strlit name, sym2strlit enumname, num i]))])
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
		    => (map (fn(d) => HLEC.DEFINITION (d, PosLog.NOPOS)) (trans_definition def)) @ 
		       [HLEC.ACTION (HLEC.EXP (apply (addConst, [sym2strlit name, HLEC.SYMBOL name])),
				     PosLog.NOPOS)]
			

		  | Ast.SUBMODELDEF _
		    => (internalerror "Unexpected model definition component found";
			[])

		  | Ast.SUBMODELINST {class, name, opttable, optdimensions}
		    => 
		    let
			val table = case opttable of SOME table => trans_exp table | _ => HLEC.TABLE nil
		    in
			[HLEC.ACTION(HLEC.EXP (apply(HLEC.SYMBOL (Symbol.symbol "instantiateSubModel"),
						     [HLEC.SYMBOL class,
						      HLEC.LITERAL (HLEC.CONSTSTR (Symbol.name name)),
						      table,
						      HLEC.VECTOR (case optdimensions of
								       NONE => []
								     | SOME dims => map (fn(s) => HLEC.SYMBOL s) dims)])), 
				     PosLog.NOPOS),
			 
			 HLEC.DEFINITION(HLEC.DEFCONST (name, HLEC.DONTCARE, HLEC.SEND{message=name,
										       object=self}),
					 PosLog.NOPOS)]
		    end
(*		    => let val table = case opttable of SOME table => trans_exp table | _ => HLEC.TABLE nil
		       in
			   [HLEC.ACTION (HLEC.EXP (apply (sym "instantiateSubModel", [HLEC.SYMBOL class, sym2strlit name, table])),
					 PosLog.NOPOS),
			    HLEC.DEFINITION (HLEC.DEFLOCAL (name, HLEC.DONTCARE, HLEC.SEND {message=name, object=self}),
					     PosLog.NOPOS)
			   ]
		       end*)

		  | Ast.OUTPUTDEF {name, quantity, dimensions, settings, condition} 
 		    =>
		    let 
			fun outerror () =
			    error ($("Output " ^ (Symbol.name name) ^ " does not appear as a returned quantity in the model header"))
			    
			val _ = case returns of
				    NONE => outerror()
				  | SOME rets => if (not (List.exists (fn(s) => s = name) rets)) then
						     outerror ()
						 else
						     ()

			val obj = apply (send "new" (sym "Output"), [sym2strlit name, trans_exp quantity])

			val obj = case dimensions of
				      SOME [dim] => apply (HLEC.SEND{message=Symbol.symbol "setIter",
								     object=obj},
							   [HLEC.SYMBOL dim])
				    | SOME _ => obj before error($("Multiple dimensions are not supported on outputs"))
				    | NONE => obj


			val obj = case settings 
				   of SOME table => apply (obj, [trans_exp table])
				    | _ => obj

			val obj = case condition
				   of SOME cond => apply (obj, [HLEC.TABLE [(Symbol.symbol "condition", trans_exp cond)]])
				    | _ => obj
		    in
			[HLEC.ACTION(HLEC.EXP (apply (HLEC.SEND{message=Symbol.symbol "add",
								object=HLEC.SYMBOL (Symbol.symbol "outputDefs")},
						      [HLEC.LITERAL(HLEC.CONSTSTR (Symbol.name name)), 
									   HLEC.LAMBDA{args=[], body=obj}])),
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
			case settings
			 of SOME table =>
			    [HLEC.ACTION (HLEC.EXP (apply (HLEC.SYMBOL name, [trans_exp table])), PosLog.NOPOS)]
			  | _ => nil
		    end

		  | Ast.ITERATORDEF {name, value, settings} 
 		    =>  
		    let 
			val table = 		 
			    case settings
			     of SOME table =>
				trans_exp table
			      | NONE => HLEC.TABLE []
		    in
			[HLEC.ACTION(HLEC.COND {cond=HLEC.AND [apply (sym "objectContains",
                                                                      [sym "self",
                                                                       HLEC.LITERAL(HLEC.CONSTSTR (Symbol.name name))]),
                                                               apply (sym "istype",
                                                                      [HLEC.TYPEEXP (HLEC.TYPE (Symbol.symbol "TimeIterator")),
                                                                       HLEC.SYMBOL (name)])],
						ift=[HLEC.ACTION (HLEC.EXP (apply (send "reset" (HLEC.SYMBOL name),
                                                                                   [])),
                                                                  PosLog.NOPOS),
                                                     HLEC.ACTION (HLEC.EXP (apply (HLEC.SYMBOL name,
                                                                                   [table])),
                                                                  PosLog.NOPOS)] @ 
                                                    (case value
                                                      of SOME value => 
                                                         [HLEC.ACTION (HLEC.EXP (apply (send "setValue" (HLEC.SYMBOL name), [trans_exp value])), PosLog.NOPOS)]
                                                       | NONE => []),
						iff=[HLEC.DEFINITION (HLEC.DEFLOCAL (name, HLEC.DONTCARE, apply (sym "makeIterator", [sym2strlit name, table])), PosLog.NOPOS),
                                                     HLEC.ACTION (HLEC.EXP (apply (addConst, [sym2strlit name, HLEC.SYMBOL name])), PosLog.NOPOS),
                                                     HLEC.ACTION (HLEC.EXP (apply (send "push_back" (sym "iterators"), [HLEC.SYMBOL name])), PosLog.NOPOS)] @ 
                                                    (case value
                                                      of SOME value => 
                                                         [HLEC.ACTION (HLEC.EXP (apply (send "setValue" (HLEC.SYMBOL name), [trans_exp value])), PosLog.NOPOS)]
                                                       | NONE => [])},
                                     PosLog.NOPOS),
			 HLEC.DEFINITION (HLEC.DEFLOCAL (name, HLEC.DONTCARE, apply (getMember, [sym2strlit name])),
					  PosLog.NOPOS)]
			

		    end
(*		    [HLEC.DEFINITION (HLEC.DEFLOCAL (name, HLEC.DONTCARE, apply (send "new" (sym "SimIterator"), [sym2strlit name])), PosLog.NOPOS),
		     HLEC.ACTION (HLEC.EXP (apply (addConst, [sym2strlit name, HLEC.SYMBOL name])), PosLog.NOPOS),
		     HLEC.ACTION (HLEC.EXP (apply (send "push_back" (sym "iterators"), [HLEC.SYMBOL name])), PosLog.NOPOS)] @ 
		    (case value
		      of SOME value => 
			 [HLEC.ACTION (HLEC.EXP (apply (send "setValue" (HLEC.SYMBOL name), [trans_exp value])), PosLog.NOPOS)]
		       | NONE => []) @ 
		    (case settings
		      of SOME table =>
			 [HLEC.ACTION (HLEC.EXP (apply (HLEC.SYMBOL name, [trans_exp table])), PosLog.NOPOS)]
		       | NONE => [])
*)
		  | Ast.QUANTITYDEF {modifiers, basetype, name, precision, exp, settingstable, dimensions}
		    => 
		    let 
			val baseclass = case basetype 
					 of Ast.GENERIC_QUANTITY => "SimQuantity"
					  | Ast.STATE_QUANTITY => "State"
					  | Ast.RANDOM_QUANTITY => "Random"
					  | Ast.PARAMETER_QUANTITY => "Parameter"

			val dims = case dimensions
				    of SOME dims => trans_exp (Ast.VECTOR (map (Ast.LITERAL o Ast.CONSTSTR o Symbol.name) dims))
				     | NONE => HLEC.VECTOR nil

			val table = case settingstable
				     of SOME table => trans_exp table
				      | NONE => HLEC.TABLE nil

			val quantity = apply (send "new" (sym baseclass), [sym2strlit name])

		    in
			[HLEC.DEFINITION (HLEC.DEFLOCAL (name, HLEC.DONTCARE, quantity), PosLog.NOPOS),
			 HLEC.ACTION (HLEC.ASSIGN (send "iter" (HLEC.SYMBOL name), HLEC.SYMBOL (Symbol.symbol "t")), PosLog.NOPOS),
			 HLEC.ACTION (HLEC.EXP (apply (addConst, [sym2strlit name, HLEC.SYMBOL name])), PosLog.NOPOS),
			 HLEC.ACTION (HLEC.EXP (apply (send "push_back" (sym "quantities"), [HLEC.SEND {message=name, object=self}])), PosLog.NOPOS)] @
			(case exp 
			  of SOME exp => 
			     [HLEC.ACTION (HLEC.EXP (apply (send "setInitialValue" (HLEC.SYMBOL name), [trans_exp exp])), PosLog.NOPOS)]
			   | NONE => []) @
			[HLEC.ACTION (HLEC.EXP (apply (send "setDimensions" (HLEC.SYMBOL name), [dims])), PosLog.NOPOS),
			 HLEC.ACTION (HLEC.EXP (apply (HLEC.SYMBOL name, [table])), PosLog.NOPOS)]
		    end
		    

		  | Ast.STM (stm)
		    => (trans_stm stm) @
		       let
			   fun def2nameAndStorage def =
			       let val (func, name) = 
				       case def
					of Ast.DEFPROTOCOL (_, {name, ...}, _) => (addMethod, name)
					 | Ast.DEFFUN (_, ({name, ...}, _)::_) => (addMethod, name)
					 | Ast.DEFCONST (name, _, _) => (addConst, name)
					 | Ast.DEFCLASS {name, ...} => (addConst, name)
					 | Ast.DEFMODEL {header={name, ...}, ...} => (addConst, name)
					 | Ast.INSTMODEL {name, ...} => (addConst, name)
					 | Ast.DEFNAMESPACE {name, ...} => (addConst, name)
					 | Ast.DEFINTERFACE {name, ...} => (addConst, name)
					 | Ast.DEFENUM {name, ...} => (addConst, name)
					 | Ast.DEFPROPERTY {name, ...} => (addConst, name)
					 | Ast.DEFLOCAL (name, _, _) => (addVar, name)
					 | Ast.DEFFUN (_, nil) => 
					   DynException.stdException ("empty function definition encountered", 
								      "AstTrans.trans_definition.model.STM.def2nameAndStorage.DEFFUN", Logger.INTERNAL)
					 | Ast.DEFGLOBAL _ => 
					   DynException.stdException ("cannot define a global within a model", 
								      "AstTrans.trans_definition.model.STM.def2nameAndStorage.DEFGLOBAL", Logger.INTERNAL)
					   
			       in 
				   [HLEC.ACTION (HLEC.EXP (apply (func, [sym2strlit name, HLEC.SYMBOL name])), PosLog.NOPOS)]
			       end
		       in
			   case stm of
			       Ast.ACTION _ => []
			     | Ast.DEFINITION (def, _) => def2nameAndStorage def
							  
		       end	

	    fun build_input (name, pattern) =
		[HLEC.DEFINITION (HLEC.DEFLOCAL (name, HLEC.DONTCARE, apply (send "new" (sym "Input"), [sym2strlit name])),
				  PosLog.NOPOS),
		 HLEC.ACTION (HLEC.EXP (apply (addConst, [sym2strlit name, HLEC.SYMBOL name])),
			      PosLog.NOPOS),
		 HLEC.ACTION (HLEC.EXP (apply (send "push_back" (sym "inputs"), [HLEC.SYMBOL name])),
			      PosLog.NOPOS)]
	
	    fun build_output (name) =
		HLEC.ACTION (HLEC.EXP (apply (sym "buildOutput", [sym2strlit name])),
			     PosLog.NOPOS)
	    val modelstms = 
		(* set name *)
		HLEC.METHODDEF (HLEC.PUBLIC, HLEC.DEFLOCAL (Symbol.symbol "name", HLEC.DONTCARE, HLEC.LITERAL (HLEC.CONSTSTR (Symbol.name name))))
		:: HLEC.METHODDEF (HLEC.PUBLIC, HLEC.DEFLOCAL(Symbol.symbol "imports",
							      HLEC.DONTCARE,
							      HLEC.TUPLE nil))


		:: [] 

	    val template_constructor_stms = 
 		(flatten (map build_input args))
		@ (flatten(map build_stm parts))
		@ (case returns of
		       SOME returns => map build_output returns
		     | NONE => [])

	    val templateconstructor = 
		HLEC.CONSTRUCTOR {args=[], 
	    			  body= (HLEC.ACTION (HLEC.EXP (apply (sym "super", nil)), PosLog.NOPOS))
	    				:: (template_constructor_stms)}


	    val default_model_settings = 
		HLEC.TABLE [(Symbol.symbol "target", HLEC.LITERAL (HLEC.CONSTSTR "cpu")),
			    (Symbol.symbol "precision", HLEC.LITERAL (HLEC.CONSTSTR "double")),
			    (Symbol.symbol "num_models", HLEC.LITERAL (HLEC.CONSTREAL 1.0)),
			    (Symbol.symbol "debug", HLEC.LITERAL (HLEC.CONSTBOOL false)),
			    (Symbol.symbol "profile", HLEC.LITERAL (HLEC.CONSTBOOL false)),
			    (Symbol.symbol "emulate", HLEC.LITERAL (HLEC.CONSTBOOL false))]

	    val modelsettings =
		HLEC.METHODDEF (HLEC.PUBLIC, HLEC.DEFLOCAL(Symbol.symbol "settings",
							   HLEC.DONTCARE,
							   default_model_settings))


	    val templatename = Symbol.symbol ((Symbol.name name) ^ "Template")

	    val hiddenModelTemplateDef =
		HLEC.DEFINITION(HLEC.DEFCLASS {name=templatename,
					       classheader={inheritance=SOME (HLEC.SYMBOL (Symbol.symbol "Model")),  
							    interfaces=[]}, 
					       methods= templateconstructor :: modelsettings :: modelstms},					       
				PosLog.NOPOS)

	    val hiddenModelTemplate = 
		HLEC.DEFINITION(HLEC.DEFLOCAL (Symbol.symbol "template", HLEC.DONTCARE, apply (send "new" (HLEC.SYMBOL templatename), nil)),
				PosLog.NOPOS)



	    val wrapperMembers =
		let
		    fun makeVar (sym) =
			HLEC.METHODDEF (HLEC.PUBLIC, HLEC.DEFLOCAL(sym, HLEC.DONTCARE, HLEC.UNDEFINED))
		in
		    map makeVar (case #returns header of SOME r => r | NONE => [])
		end
		@
		let
		    fun makeProperty (sym, typepattern) =
			let
			    val id = (Symbol.name sym) ^ "_var"
			    val var = send id self
			in
			    [HLEC.METHODDEF (HLEC.HIDDEN, HLEC.DEFLOCAL(Symbol.symbol id, HLEC.DONTCARE, HLEC.UNDEFINED)),
			     HLEC.METHODDEF (HLEC.PUBLIC, HLEC.DEFPROPERTY {name=sym,
									    read=SOME [HLEC.ACTION (HLEC.EXP var, PosLog.NOPOS)],
									    write=SOME (Symbol.symbol "arg", 
											[HLEC.ACTION (HLEC.EXP (apply (send "setInputVal" var, [HLEC.SYMBOL (Symbol.symbol "arg")])), 
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
		     flatten (map (fn(arg) => (* if isdefined modeltemplate.arg then set it in outputbinding, push onto outputs, else error that it wasn't declared *)
				     [HLEC.ACTION (HLEC.ASSIGN (HLEC.SEND{message=arg, object=HLEC.SYMBOL (Symbol.symbol "self")},
								HLEC.APPLY{func=HLEC.SEND{message=Symbol.symbol "new",
											  object=HLEC.SYMBOL (Symbol.symbol "OutputBinding")},
									   args=HLEC.TUPLE[sym2strlit arg,
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
						    apply (send "new" (HLEC.SYMBOL name), nil)), PosLog.NOPOS)] @
		    (map (fn(name, patt) => HLEC.ACTION(HLEC.EXP(HLEC.APPLY{func=HLEC.SEND{message=Symbol.symbol "setInputVal",
											   object=HLEC.SEND{message=name,
													    object=HLEC.SYMBOL (Symbol.symbol "model")}},
									    args=HLEC.TUPLE [HLEC.SYMBOL (name)]}), 
							PosLog.NOPOS))
			 (#args header)) @ 
		    [HLEC.ACTION(HLEC.EXP (HLEC.SYMBOL (Symbol.symbol "model")), PosLog.NOPOS)]
		end

	    val fakeConstructorStms2 =
		let
		    fun buildArg (sym, patt) =
			HLEC.SYMBOL sym
		in
		    [HLEC.ACTION (HLEC.EXP (apply (send "new" (HLEC.SYMBOL name), nil)), PosLog.NOPOS)]
		end

	    val fakeconstructor1 = HLEC.DEFINITION(HLEC.DEFFUN(HLEC.REPLACE, 
							  [({args=map (fn(a, dims) => (a, NONE)) (#args header), 
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

	    val stringRepresentation = "model definition " ^ (Symbol.name name)

	    val tostring = HLEC.DEFINITION(HLEC.DEFFUN(HLEC.REPLACE,
						       [({args=nil,
							  name=Symbol.symbol "tostring",
							  return=NONE},
							 [HLEC.ACTION(HLEC.EXP (HLEC.LITERAL (HLEC.CONSTSTR (stringRepresentation))), PosLog.NOPOS)])]),
					   PosLog.NOPOS)

	in
	    [HLEC.DEFNAMESPACE {name=name,				
				stms=(HLEC.PUBLIC, hiddenModelTemplateDef) :: 
				     (HLEC.PUBLIC, hiddenModelTemplate) :: 
				     (HLEC.PUBLIC, wrapperDef):: 
				     (HLEC.PUBLIC, fakeconstructor1) :: 
				     (HLEC.PUBLIC, fakeconstructor2) ::
				     (HLEC.PUBLIC, tostring) ::
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

		    val fun_lambda = HLEC.LAMBDA {args=map exp2sym args, body=trans_exp bodyexp}

		    val eq = apply (send "new" (sym "MathFunction"), [sym2strlit name, fun_lambda])
		in
		    (*		    [HLEC.DEFINITION(HLEC.DEFCONST(name, HLEC.DONTCARE, funobj),
						     pos)]*)
		    [HLEC.DEFINITION (HLEC.DEFLOCAL (name, HLEC.DONTCARE, eq), pos),
		     HLEC.ACTION (HLEC.EXP (apply (addConst, [sym2strlit name, HLEC.SYMBOL name])), pos)]
		end

	      | trans_eq (Ast.EVENT (name, cond)) =
		let
		    val eq = apply (send "new" (sym "Event"), [sym2strlit name, trans_exp cond])
		in
		    (*		    [HLEC.DEFINITION(HLEC.DEFCONST(name, HLEC.DONTCARE, funobj),
						     pos)]*)
		    [HLEC.DEFINITION (HLEC.DEFLOCAL (name, HLEC.DONTCARE, eq), pos),
		     HLEC.ACTION (HLEC.EXP (apply (send "push_back" (sym "quantities"), [HLEC.SYMBOL name])), PosLog.NOPOS),
		     HLEC.ACTION (HLEC.EXP (apply (addConst, [sym2strlit name, HLEC.SYMBOL name])), pos)]
		end


	      | trans_eq (Ast.EQUATION (lhs, rhs, optcond)) =
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
				  [sym] => SOME sym
				| _ => NONE
(*				  (error ($"Malformed equation encountered: the left hand side of an equation can only contain a state, derivative, quantity, or an output.");
				   raise Skip)*)

		    val dimensions = findDimensions lhs
		in
		    case sym of 
			SOME sym => 
			[HLEC.ACTION(HLEC.EXP(apply(HLEC.SYMBOL (Symbol.symbol "makeEquation"),
						    [HLEC.LITERAL(HLEC.CONSTSTR (Symbol.name sym)),
						     HLEC.VECTOR dimensions,
						     HLEC.LAMBDA{args=syms, body=trans_exp lhs},
						     HLEC.LAMBDA{args=syms, body=trans_exp rhs}] @ 
						    (case optcond of
							 NONE => []
						       | SOME c => [trans_exp c]))),
				     pos),
			 HLEC.DEFINITION(HLEC.DEFLOCAL(sym, HLEC.DONTCARE, 
						       HLEC.SEND {object=HLEC.SYMBOL(Symbol.symbol "self"), message=sym}),
					 pos)]
		      | NONE => [HLEC.ACTION(HLEC.EXP(HLEC.ERROR (HLEC.LITERAL(HLEC.CONSTSTR ("The left hand side of an equation can only contain a previously unused variable name, a state, a derivative, or an output.")))), 
					     pos)]
		end

	     (* | trans_eq (Ast.EQUATION (Ast.SYMBOL name, exp)) =
		(* equation x = exp *)
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
		    val eq = apply (sym "makeIntermediate", [sym2strlit name, trans_exp exp])
		    fun findDimensions exp =
			case exp of
			    Ast.SYMBOL s => []
			  | Ast.POS (exp, _) => findDimensions exp
			  | Ast.APPLY {func=Ast.SYMBOL s, args=Ast.TUPLE [_, e]} 
			    => findDimensions e
			  | Ast.APPLY {func, args=Ast.TUPLE [Ast.VECTOR v]} 
			    => map trans_exp v			       
			  | _ => nil

(*		    val syms = findSymbols lhs

		    val sym = case syms of
				[sym] => sym
			      | _ =>
				(error ($"Malformed equation encountered: unexpected number of symbols on left hand side");
				 raise Skip)

		    val dimensions = findDimensions lhs*)
		in
		    [HLEC.DEFINITION (HLEC.DEFLOCAL (name, HLEC.DONTCARE, eq), pos),
		     HLEC.ACTION (HLEC.EXP (apply (addConst, [sym2strlit name, HLEC.SYMBOL name])), pos),
		     HLEC.ACTION (HLEC.EXP (apply (send "push_back" (sym "quantities"), [HLEC.SYMBOL name])), pos)]
		(*    [HLEC.ACTION(HLEC.EXP(HLEC.APPLY{func=HLEC.SYMBOL (Symbol.symbol "makeEquation"),
										 args=HLEC.TUPLE[HLEC.LITERAL(HLEC.CONSTSTR (Symbol.name sym)),
												 HLEC.VECTOR dimensions,
												 HLEC.LAMBDA{args=syms, body=trans_exp lhs},
												 HLEC.LAMBDA{args=syms, body=trans_exp rhs}]}),
				 pos),
		     HLEC.DEFINITION(HLEC.DEFCONST(sym, HLEC.DONTCARE, HLEC.SEND {object=HLEC.SYMBOL(Symbol.symbol "self"), message=sym}),
				     pos)]*)
		end
	      | trans_eq (Ast.EQUATION (Ast.APPLY {func=state, args=Ast.TUPLE args}, exp)) =
		let
		    val state = trans_exp state
		    val is_diffeq = case state of HLEC.SYMBOL name => name = (Symbol.symbol "operator_deriv") 
						| _ => false
		in
		    if is_diffeq then
		    (* equation x' = exp *)
			(case args
			  of degree :: state :: nil =>
			     let
				 val state = trans_exp state
				 val name = case state of HLEC.SYMBOL name => Symbol.name name | _ => "unknown state"
				 val degree = trans_exp degree
										
				 val has_eq = apply (send "hasEquation" state, nil)
				 val make_eq = apply (send "new" (sym "DifferentialEquation"), 
						      [degree, 
						       state, 
						       trans_exp exp])
				 val set_eq = apply (send "setEquation" state, [make_eq])
				 val flunk = HLEC.ERROR (HLEC.LITERAL (HLEC.CONSTSTR ("Equation for " ^name^" has already been defined.")))
			     in
				 [HLEC.ACTION(HLEC.COND {cond=has_eq, 
							 ift=[HLEC.ACTION (HLEC.EXP flunk, PosLog.NOPOS)], 
							 iff=[HLEC.ACTION (HLEC.EXP set_eq, PosLog.NOPOS)]}, pos)]
			     end

			   | _ => (error ($"Malformed differential equation encountered"); raise Skip))
		    else
			(case args
			  of [Ast.VECTOR [Ast.LITERAL (Ast.CONSTREAL r)]] =>
			     (* Spatial references *)
			     if 0 = Real.floor r andalso 0 = Real.ceil r then
				 (* equation x[0] = exp *)
				 let
				     val set_init = HLEC.SEND {object=state, message=(Symbol.symbol "setInitialValue")}
				 in
				     [HLEC.ACTION (HLEC.EXP (HLEC.APPLY {func=set_init, args=HLEC.TUPLE [trans_exp exp]}),
						   pos)]
				 end
			     else
				 (* equation x[r] = exp *)
				 let
				     (* Creates a new equation {exp when n == r, state.getEquation().getExp() otherwise} *)

				     (* Ensures that a previous equation was defined before we try to override it. *)
				     val old_eq = HLEC.APPLY {func=HLEC.SEND {object=state, message=(Symbol.symbol "getEquation")}, args=HLEC.UNIT}
				     val cond_exp = HLEC.IFEXP {cond=HLEC.APPLY {func=HLEC.SEND {object=state, message=(Symbol.symbol "hasEquation")}, args=HLEC.UNIT},
								ift=HLEC.APPLY {func=HLEC.SEND {object=old_eq, message=(Symbol.symbol "getExp")}, args=HLEC.UNIT}, 
								iff=state}

				     val cond_exp = HLEC.IFEXP {cond=HLEC.APPLY {func=HLEC.SYMBOL (Symbol.symbol "operator_eq"), 
										 args=HLEC.TUPLE [HLEC.SYMBOL (Symbol.symbol "n"), HLEC.LITERAL (HLEC.CONSTREAL r)]},
								ift=trans_exp exp,
								iff=cond_exp}
						    

				     val new_eq = HLEC.APPLY {func=HLEC.SEND {message=Symbol.symbol "new", object=HLEC.SYMBOL (Symbol.symbol "Equation")},
							      args=HLEC.TUPLE [state, cond_exp]}
				 in
				     [HLEC.ACTION (HLEC.EXP (HLEC.APPLY {func=HLEC.SEND {object=state, message=(Symbol.symbol "setEquation")},
									 args=HLEC.TUPLE [new_eq]}),
						   pos)]
				 end
				 
			   | [Ast.VECTOR [newtime]] =>
			     (* Temporal references *)
			     (* equation x[n] = exp *)
			     let
				 val new_eq = HLEC.SEND {message=Symbol.symbol "new", object=HLEC.SYMBOL (Symbol.symbol "DifferenceEquation")}
				 val make_eq = HLEC.APPLY {func=new_eq, args=HLEC.TUPLE [trans_exp newtime, state, trans_exp exp]}
				 val set_eq = HLEC.SEND {object=state, message=(Symbol.symbol "setEquation")}
			     in
				 [HLEC.ACTION (HLEC.EXP (HLEC.APPLY {func=set_eq, args=HLEC.TUPLE [make_eq]}),
					       pos)]
			     end
			   | [Ast.VECTOR nil] =>
			     (* equation x[] = exp *)
			     (error ($"No index in a difference equation."); raise Skip)
			   | [Ast.VECTOR _] =>
			     (* equation x[n,m] = exp *)
			     (error ($"Too many indices in a difference equation."); raise Skip)
			   | _ => 
			     (error ($"Malformed difference equation encountered"); raise Skip))
		end

	      | trans_eq (Ast.EQUATION (Ast.APPLY {func=state, args=args}, exp)) =
		(error ($"Arguments of function application on LHS of equation is not a TUPLE.");
		 raise Skip)

	      | trans_eq (Ast.EQUATION (Ast.POS (exp1, pos), exp2)) =
		trans_eq (Ast.EQUATION (exp1,exp2))

	      | trans_eq eq =
		(error ($"Malformed equation encountered");
		 raise Skip)
		*)

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
