signature MODELTRANSLATE =
sig
    (* The string returned by translate is the name of the model *)
    val translate : ((KEC.exp -> KEC.exp) * KEC.exp) -> (DOF.model) option
end

structure ModelTranslate : MODELTRANSLATE=
struct

fun error (msg) =
    (*TODO: replace *)
    DynException.stdException (msg, "ModelTranslate", Logger.USER)

fun translate (exec, object) =
    let
	(* helper methods *)
	val pretty = PrettyPrint.kecexp2prettystr exec

	val TypeMismatch = DynException.TypeMismatch
	and ValueError = DynException.ValueError

	(* This method assumes the exp has already been exec'd *)
	fun exp2bool (KEC.LITERAL(KEC.CONSTBOOL b)) =
	    b
	  | exp2bool exp =
	    DynException.stdException ("Expected a boolean but received " ^ (PrettyPrint.kecexp2nickname exp) ^ ": " ^ (pretty exp), "ModelTranslate.translate.exp2bool", Logger.INTERNAL)


	(* This method assumes the exp has already been exec'd *)
	fun exp2real (KEC.LITERAL(KEC.CONSTREAL b)) =
	    b
	  | exp2real exp =
	    DynException.stdException ("Expected a number but received " ^ (PrettyPrint.kecexp2nickname exp) ^ ": " ^ (pretty exp), "ModelTranslate.translate.exp2real", Logger.INTERNAL)


	(* This method assumes the exp has already been exec'd *)
	fun exp2int (KEC.LITERAL(KEC.CONSTREAL b)) =
	    Real.floor b
	  | exp2int exp =
	    DynException.stdException ("Expected a number but received " ^ (PrettyPrint.kecexp2nickname exp) ^ ": " ^ (pretty exp), "ModelTranslate.translate.exp2int", Logger.INTERNAL)


	fun exp2realoption (KEC.UNDEFINED) =
	    NONE 
	  | exp2realoption r =
	    SOME (exp2real r)

	fun real2exp r =
	    KEC.LITERAL (KEC.CONSTREAL r)

	fun int2exp i =
	    KEC.LITERAL (KEC.CONSTREAL (Real.fromInt i))


	(* This method assumes the exp has already been exec'd *)
	fun exp2str (KEC.LITERAL (KEC.CONSTSTR s)) = s
	  | exp2str exp =
	    DynException.stdException ("Expected a string but received " ^ (PrettyPrint.kecexp2nickname exp) ^ ": " ^ (pretty exp), "ModelTranslate.translate.exp2str", Logger.INTERNAL)
	    
	(* Returns a SYMBOL expression with the given name. *)
	fun sym s = KEC.SYMBOL (Symbol.symbol s)
		    
	(* Returns an object instance method or attribute. *)
	fun method name object =    
	    exec (KEC.SEND {message=Symbol.symbol name, object=object})

	(* Evaluates a function application. *)
	fun apply (f, a) = exec (KEC.APPLY {func=f, args=KEC.TUPLE a})

	(* Applies an object instance method to the given arguments. *)
	fun send message object args = 
	    case args of
		SOME args' => apply (method message object, args')
	      | NONE => apply (method message object, [])


	fun dslname obj = method "dslname" obj
	fun getInitialValue obj = send "getInitialValue" obj NONE

	(* Indicates whether an object conforms to a type using the DSL typechecker. *)
	fun istype (object, typ) =
	    let
		val checkexp = 
		    KEC.LIBFUN (Symbol.symbol "istype", KEC.TUPLE [KEC.TYPEEXP (KEC.TYPE (Symbol.symbol typ)),
								   object])

		val result = exec checkexp
	    in
		exp2bool result
	    end
	    
	    
	fun vec2list (KEC.VECTOR vec) = KEC.kecvector2list (vec)
	  | vec2list exp =
	    DynException.stdException ("Expected a vector but received " ^ (PrettyPrint.kecexp2nickname exp) ^ ": " ^ (pretty exp), "ModelTranslate.translate.vec2list", Logger.INTERNAL)
	    





	fun model2classname object =
	    Symbol.symbol(exp2str(method "name" object))
(*
	fun obj2param obj =
	    {name=exp2str(method "name" obj),
	     dimensions=map (Real.floor o exp2real) (vec2list (method "dimensions" obj)),
	     initialval=exp2real (getInitialValue obj),
	     properties=SymbolTable.empty}

	fun kecexp2dofexp obj =
	    if istype (obj, "ModelOperation") then
		    let
			val name = case exp2str (method "name" obj) of
				       "branch" => "IF"
				     | n => n
		    in
			DOF.OP {name=Symbol.symbol name,
				args=map kecexp2dofexp (vec2list(method "args" obj))}
		    end
	    else if istype (obj, "SimQuantity") then
		if exp2bool (send "getIsIntermediate" obj NONE) then
		    kecexp2dofexp (send "getExp" (send "getEquation" obj NONE) NONE) (*TODO: make this a read of the name with a notation that its an interm *)
		else if (exp2bool (send "getIsConstant" obj NONE))  then
		    kecexp2dofexp (getInitialValue obj)
		else
		    let
			(*TODO: fixme to be a global name *)
			val name = exp2str (method "name" obj)
		    in
			DOF.SYMBOL (DOF.QUANTITY (Symbol.symbol name), SymbolTable.empty)
		    end
	    else 
		case obj
		 of KEC.LITERAL (KEC.CONSTREAL r) => DOF.REAL r
		  | KEC.LITERAL (KEC.CONSTBOOL b) => DOF.BOOLEAN b
		  (* FIXME: Is this an acceptable way to handle undefined? *)
		  (* 		  | KEC.UNDEFINED => ExpTree.LITERAL (ExpTree.CONSTREAL 0.0) *)
		  | _ => 
		    raise TypeMismatch ("Unexpected type of expression object; received " ^ (pretty obj))

	fun obj2idquant obj =
	    {name=Symbol.symbol(exp2str(method "name" obj)), 
	     initialValue=exp2real(getInitialValue obj),
	     dimensions=map (Real.floor o exp2real) (vec2list (method "dimensions" obj)),
	     properties=SymbolTable.empty,
	     equation=
	     DOF.INTERMEDIATE (kecexp2dofexp (send "getExp" (send "getEquation" obj NONE) NONE))}
	    
	fun obj2sdquant obj =
	    let
		val eq = send "getEquation" obj NONE
	    in
		{name=Symbol.symbol(exp2str(method "name" obj)), 
		 initialValue=exp2real(getInitialValue obj),
		 dimensions=map (Real.floor o exp2real) (vec2list (method "dimensions" obj)),
		 properties=SymbolTable.empty,
		 equation=
		 if (istype (eq, "DifferentialEquation")) then
		     DOF.DIFFERENTIAL {order=Real.floor(exp2real(method "degree" eq)), 
				       exp=kecexp2dofexp (send "getExp" eq NONE)}
		 else
		     DOF.DIFFERENCE {offset=1(*TODO: fixme*), 
				     exp=kecexp2dofexp(send "getExp" eq NONE)}}
	    end

	fun addClass(object, dof) =
	    let
		val name = model2classname object
		val properties = SymbolTable.empty
		val functions = nil
		val tunables = map (obj2param) (vec2list(send "getLocalParameters" object NONE))
		val equations = map (obj2idquant) (vec2list(send "getLocalIntermediates" object NONE))
			      @ map (obj2sdquant) (vec2list(send "getLocalStates" object NONE))
		val submodels = nil
		val outputs = nil

		val class =
		    {name=SOME name,
		     properties=properties,
		     (*	     functions=functions,*)
		     (*tunables=tunables,*)
		     quants=ref equations,
		     submodels=submodels,
		     outputs=outputs,
		     instances=ref []}
	    in 
		class :: dof 
	    end
	    

	fun addInstance(object, dof) =
	    let
		val objectClassName = model2classname object
		val class = 
		    case List.find (fn(c) => case #name c of SOME n => n = objectClassName | NONE => false) dof of
			SOME c => c
		      | NONE => DynException.stdException ("Couldn't find class for " ^ (Symbol.name objectClassName), "ModelTranslate.translate.addInstance", Logger.INTERNAL)

		val instance = ref {position=PosLog.NOPOS,
				    uniqueid= 0, (*TODO: fill in *)
				    instanceName= (Symbol.symbol (exp2str(method "name" object))), 
				    inputvals=[],
				    submodelids=[],
				    dimensions = map (Real.floor o exp2real) (vec2list (method "dimensions" object))}

		val _ = #instances class := instance :: (!(#instances class))
	    in
		(dof, instance)
	    end

	fun model2dof (object, dof) =
	    let
		val objectclass = model2classname object
		val _ = print ("model2dofing " ^ (Symbol.name objectclass) ^ "\n")
		val dof = if not(List.exists (fn(class) => case (#name class) of SOME name => name  = objectclass | NONE => false) dof) then
			      addClass(object, dof)
			  else
			      dof

		val class = case List.find(fn(class) => case (#name class) of SOME name => name = objectclass | NONE => false)
					  dof of
				SOME class => class
			      | NONE => DynException.stdException ("Couldn't find class for " ^ (Symbol.name objectclass), "ModelTranslate.translate.addInstance", Logger.INTERNAL)

		val dof = foldl (fn(object, dof) => #1(model2dof (object, dof))) 
				dof 
				(vec2list(method "submodels" object)) 

		val (dof, instance) = addInstance(object, dof)
			  
	    in
		(dof, (class, instance), SymbolTable.empty, UniqueTable.empty(*FIXME*))
	    end

*)

	fun kecexp2dofexp obj =
	    if istype (obj, "ModelOperation") then
		let
		    val name = exp2str (method "name" obj)
					
		in
		    if name = "deriv" then
			let
			    val args = vec2list (method "args" obj)
			    val (order, quantity) = case args of
							[order, q] => (order, q)
						      | _ => DynException.stdException ("Unexpected arguments to derivative operation", 
											"ModelTranslate.translate.kecexp2dofexp", Logger.INTERNAL)

			    val order = exp2int order
			    val sym = kecexp2dofexp quantity
			in
			    case sym of
				Exp.TERM(Exp.SYMBOL(s, props)) =>
				let
				    val props = case Property.getIterator props of
						    NONE => Property.setIterator props [(Symbol.symbol "t", Iterator.RELATIVE 0)]
						  | SOME iters => if List.exists (fn(s,_) => s = (Symbol.symbol "t") orelse s = (Symbol.symbol "n")) iters then
								      props
								  else
								      Property.setIterator props ((Symbol.symbol "t", Iterator.RELATIVE 0) :: iters)
				in
				    Exp.TERM(Exp.SYMBOL(s, Property.setDerivative props (order,[Symbol.symbol "t"])))
				end
			      | _ => error "Derivatives of expressions is not supported"
			end
		    else
			Exp.FUN (Fun.BUILTIN (Fun.name2op (Symbol.symbol name)),
				 map kecexp2dofexp (vec2list(method "args" obj)))
		end
(*	    else if istype (obj, "IteratorReference") then
		let
		    val indices = vec2list (method "indices" obj)
		    val quantity = method "referencedQuantity" obj
		    val sym = kecexp2dofexp quantity

		    val expected_iterators = 

		    fun buildIterator index =
			if istype (index, "Wildcard") then
			    Iterator.ALL
			else if istype (index, "Interval") then
			    Iterator.RANGE ()
			else if istype (index, "Number") then
			    Iterator.ABSOLUTE ()
			else if istype (index, "ModelOperation") then
			    Iterator.RELATIVE ()
			else if istype (index, "Iterator") then
			    (, Iterator.RELATIVE (0))
			else 
			    error "Invalid index encountered"

		    val iterators = map buildIterator ListPair.zip(indices
		in
		    case sym of
			Exp.TERM(Exp.SYMBOL(s, props)) =>
			Exp.TERM(Exp.SYMBOL(s, Property.setIterator props iterators))
		      | _ => error "Indexing performed on invalid quantity"
		end
*)	    else if istype (obj, "SimQuantity") orelse istype (obj, "Input") then

(*		if exp2bool (send "getIsIntermediate" obj NONE) then
		    kecexp2dofexp (send "getExp" (send "getEquation" obj NONE) NONE) (*TODO: make this a read of the name with a notation that its an interm *)
		else 
		    if (exp2bool (send "getIsConstant" obj NONE))  then
			kecexp2dofexp (getInitialValue obj)
		    else*)
		let
		    (*TODO: fixme to be a global name *)
		    val sym = 
			if (istype (obj, "OutputBinding")) then		    
			    ExpBuild.tvar((exp2str (method "instanceName" obj)) ^ "." ^ (exp2str (method "name" obj)))
			else if (istype (obj, "Intermediate")) then 
			    ExpBuild.var(exp2str (method "name" obj))
			(* else if  ((istype (obj, "State")) andalso istype (method "eq" obj, "DifferentialEquation")) then *)
			(*     ExpBuild.tvar_from_state(exp2str (method "name" obj)) *)
			else if istype (obj, "IteratorReference") then
			    let
				val name = exp2str (method "name" (method "referencedQuantity" obj))
				val args = vec2list (method "indices" obj)

				val iterators = map (Symbol.symbol o exp2str) 
						    (vec2list (send "getDimensions" 
								    (method "referencedQuantity" obj)
								    NONE))

				val iterators = 
				    if length args > 0 andalso (istype (hd args, "TimeIterator") orelse 
								(istype (hd args, "ModelOperation") andalso 
								 List.exists (fn(a) => istype (a, "TimeIterator")) 
									     (vec2list (method "args" (hd args)))) orelse 
								(istype (hd args, "RelativeOffset") andalso
								 istype (method "simIterator" (hd args), "TimeIterator")))
				    then
					let
					    val time = if istype (hd args, "TimeIterator") then
							   hd args
						       else if istype (hd args, "ModelOperation") then
							   valOf (List.find (fn(a) => istype (a, "TimeIterator")) 
									    (vec2list (method "args" (hd args))))
						       else
							   method "simIterator" (hd args)
						
					in
					    ((Symbol.symbol o exp2str) (method "name" time)) :: iterators
					end
				    else
					iterators
				val namedargs = ListPair.zip (iterators, args)

				val _ = if length namedargs <> length args then
					    error ("Incorrect number of indices encountered on index of " ^ name ^ ": expected " ^ (Int.toString (length namedargs)) ^ " and received " ^ (Int.toString (length args)))
					else
					    ()

				fun buildIndex (iterator, arg) =
				    if istype (arg, "Number") then
					(iterator, Iterator.ABSOLUTE (exp2int arg))
				    else if istype (arg, "Interval") then
					(iterator, Iterator.RANGE (exp2int (method "low" arg), exp2int (method "high" arg)))
				    else if istype (arg, "SimIterator") then
					if iterator = (Symbol.symbol (exp2str (method "name" arg))) then
					    (iterator, Iterator.RELATIVE 0)
					else
					    error ("Encountered iterator "^(exp2str (method "name" arg))^" in index of "^(name)^" where "^(Symbol.name iterator)^" was expected")
				    else if istype (arg, "RelativeOffset") then
					if iterator = (Symbol.symbol (exp2str (method "name" (method "simIterator" arg)))) then
					    (iterator, Iterator.RELATIVE (exp2int(method "step" arg)))
					else
					    error ("Encountered iterator "^(exp2str (method "name" (method "simIterator" arg)))^" in index of "^(name)^" where "^(Symbol.name iterator)^" was expected")
				    else if istype (arg, "TimeIterator") then
					(iterator, Iterator.RELATIVE 0)
				    else if istype (arg, "Wildcard") then
					(iterator, Iterator.ALL)
				    else 
					error ("Invalid index detected on index of " ^ name ^ ": " ^ (pretty arg))
			    in
				Exp.TERM (Exp.SYMBOL (Symbol.symbol name, 
						      (Property.setIterator 
							   Property.default_symbolproperty 
							   (map buildIndex namedargs))))
			    end
(*			else if istype (obj, "TemporalReference") then
			    ExpBuild.relvar (Symbol.symbol(exp2str(method "name" (method "internalState" obj))),
					     Symbol.symbol(exp2str(method "name" (method "iterator" obj))),
					     Real.floor(exp2real(method "step" obj)))
*)			else if (istype (obj, "State")) then
			    ExpBuild.var(exp2str (method "name" obj))
			else
			    ExpBuild.var(exp2str (method "name" obj))
		in
		    (*DOF.SYMBOL (DOF.QUANTITY (Symbol.symbol name), SymbolTable.empty)*)
		    sym
		end		    
	    else 
		case obj
		 of KEC.LITERAL (KEC.CONSTREAL r) => ExpBuild.real r
		  | KEC.LITERAL (KEC.CONSTBOOL b) => ExpBuild.bool b
		  | KEC.VECTOR v =>
		    let
			val list = KEC.kecvector2list v

			fun exp2term (Exp.TERM t) = t
			  | exp2term _ = Exp.NAN

			fun kecexp2dofterm exp =
			    exp2term (kecexp2dofexp exp)
		    in
			Exp.TERM(Exp.LIST (map kecexp2dofterm list, [length list]))
		    end
		  (* FIXME: Is this an acceptable way to handle undefined? *)
		  (* 		  | KEC.UNDEFINED => ExpTree.LITERAL (ExpTree.CONSTREAL 0.0) *)
		  | _ => 
		    raise TypeMismatch ("Unexpected type of expression object; received " ^ (pretty obj))

	fun vecIndex (vec, index) =
	    send "at" vec (SOME [int2exp index])

	fun createClass classes object =
	    let
		val name =Symbol.symbol (exp2str (method "name" object))

		fun exp2term (Exp.TERM t) = t
		  | exp2term _ = Exp.NAN

		fun obj2output object =		    
		    (* object = [name, value] *)
		    let
			val name = (*Symbol.symbol (exp2str(vecIndex (object, 1)))*)
			    exp2term (ExpBuild.var (exp2str(vecIndex (object, 1))))
			val value = vecIndex (object, 2)
			val (contents, condition) =
			    if istype (value, "Output") then
				(case method "contents" value of
				     KEC.TUPLE args => map kecexp2dofexp args
				   | exp => [kecexp2dofexp exp],
				 kecexp2dofexp(method "condition" value))
			    else
				([kecexp2dofexp(value)],
				 Exp.TERM(Exp.BOOL true))
		    in
			{name=name,
			 contents=contents,
			 condition=condition}
		    end

		fun obj2input object =
		    {name=exp2term (ExpBuild.var (exp2str (method "name" object))),
		     default=case exp2realoption (method "default" object) of
				 SOME r => SOME (ExpBuild.real r)
			       | NONE => NONE}
(*
		    (Symbol.symbol(exp2str (method "name" object)),
		     {defaultValue= 
		      case exp2realoption (method "default" object) of
			SOME r => SOME (Exp.REAL r)
		      | NONE => NONE,
		      sourcepos=PosLog.NOPOS})
*)

		fun quantity2exp object =
		    if (istype (object, "Intermediate")) then
			[ExpBuild.equals (kecexp2dofexp (method "lhs" (method "eq" object))(*ExpBuild.var(exp2str (method "name" object)),*),
					  kecexp2dofexp (method "rhs" (method "eq" object)))]
		    else if (istype (object, "State")) then
			let
			    val lhs = kecexp2dofexp (method "lhs" (method "eq" object))
			    val rhs = kecexp2dofexp (method "rhs" (method "eq" object))
			    val eq = ExpBuild.equals(lhs, rhs)						     

			    val timeiterator = case (ExpProcess.exp2termsymbols lhs) of
						   [Exp.SYMBOL (sym,props)] => 
						   (#1(hd (valOf (Property.getIterator props)))
						    handle _ => error "Malformed left hand side of equation")
						 | _ => error "Invalid number of symbols on left hand side of equation"

			    val init = ExpBuild.equals(ExpBuild.initavar(exp2str (method "name" object), Symbol.name timeiterator),
						       kecexp2dofexp (getInitialValue object))
			in
			    [init, eq]
			end
		    else
			DynException.stdException ("Unexpected quantity encountered", "ModelTranslate.translate.createClass.quantity2exp", Logger.INTERNAL)			
		    

		fun submodel2exp (object, (submodelclasses, exps)) =
		    let			
			val classes = submodelclasses (* rkw - added this so that the foldl adds classes *)
			val (class, classes) = getClass (method "modeltemplate" object, classes)

			fun outbinding2name obj = 
			    (exp2str (method "instanceName" obj)) ^ "." ^ (exp2str (method "name" obj))

			val output_names = map outbinding2name
					       (vec2list (method "outputs" object))

			val input_exps = map (fn(inp) => method "inputVal" inp) 
					       (vec2list (method "inputs" object))

			val name = #name class

			val objname = Symbol.symbol (exp2str (method "name" object))

			val lhs = Exp.TUPLE (map (fn(out) => exp2term (ExpBuild.tvar out)) output_names)

			(* check for NaN on inputs *)
			val _ = app (fn(i) => case i of
				    KEC.LITERAL(KEC.CONSTREAL (r)) => if Real.isNan r then
									  (Logger.log_usererror nil (Printer.$("Value NaN detected on input in submodel " ^(Symbol.name objname)^ ".  Possibly input value was not specified."));
									   DynException.setErrored())
								      else ()
				  | _ => ()) 
				    input_exps

			val iterators = map (fn(e) => Symbol.symbol (exp2str (method "name" e))) (vec2list (method "dimensions" object))

			val rhs = Exp.FUN (Fun.INST {classname=name,
						     instname=objname,
						     props=Fun.setIterators Fun.emptyinstprops iterators},
					   map (fn(i) => kecexp2dofexp i) input_exps)

			val exp = ExpBuild.equals (Exp.TERM lhs, rhs)
(*
			val eq = {eq_type=DOF.INSTANCE {name=objname, classname=name, offset=nil},
				  sourcepos=PosLog.NOPOS,
				  lhs=lhs,
				  rhs=rhs}
*)				  

			val exps = exp::exps
		    in
			(classes, exps)
		    end

		fun flatten x = foldr (op @) nil x

		val quant_exps = 
		    flatten (map quantity2exp (vec2list (method "quantities" object)))
		    
		val (submodelclasses, submodel_exps) =
		    (foldl submodel2exp (classes, nil) (vec2list (method "submodels" object)))

		val exps = quant_exps @ submodel_exps

		fun expHasIterator iter exp =
		    if ExpProcess.isInitialConditionEq exp then
			case ExpProcess.getLHSSymbol exp of
			    Exp.SYMBOL (_, props) => 
			    (case Property.getIterator props of
				 SOME iters =>
				 (List.exists (fn(s,p) => s = iter) iters)
			       | NONE => false)
			  | _ => DynException.stdException(("Invalid initial condition generated, lhs is not a symbol: " ^ (ExpProcess.exp2str exp)), "ModelTranslate.translate.expHasIter", Logger.INTERNAL)
		    else
			false

		val classHasN = List.exists (expHasIterator (Symbol.symbol "n")) exps
		val classHasT = List.exists (expHasIterator (Symbol.symbol "t")) exps

		val classform = DOF.INSTANTIATION 
				    {readstates=(if classHasT then [Symbol.symbol "y"] else []) @ (if classHasN then [Symbol.symbol "x_n"] else []),
				     writestates=(if classHasT then [Symbol.symbol "dydt"] else []) @ (if classHasN then [Symbol.symbol "y_n"] else [])}

		fun buildIterator exp =
		    {name=(Symbol.symbol o exp2str) (method "name" exp),
		     low=exp2real (method "low" (method "value" exp)),
		     step=exp2real (method "step" (method "value" exp)),
		     high=exp2real (method "high" (method "value" exp))}

	    in
		({name=name, 
		  properties={sourcepos=PosLog.NOPOS,classform=classform,classtype=DOF.MASTER name},
		  inputs=ref (map obj2input(vec2list(method "inputs" object))),
		  outputs=ref (map obj2output(vec2list(method "contents" (method "outputs" object)))),
		  iterators=map buildIterator (vec2list (method "iterators" object)),
		  exps=ref exps},
		 submodelclasses)
	    end


	and getClass (object, classes) =
	    let
		val classname = Symbol.symbol(exp2str (method "name" object))
	    in
		case List.find (fn(c) => #name c = classname) classes of
		    SOME c => (c, classes)
		  | NONE => 
		    let
			val (c, classes) = createClass classes object
		    in
			(c, c :: classes)
		    end
	    end


	fun obj2modelinstance object =
	    let
		val classes = []
		val (class, classes) = getClass (method "modeltemplate" object, classes)
			     
	    in
		(classes, {name=NONE,  (*TODO: tie in*)
			   classname=(#name class)})
	    end

	fun obj2dofmodel object =
	    let
		val (classes, topinstance) = obj2modelinstance (object)
		val solverobj = method "solver" (method "modeltemplate" object)
		val solver = case exp2str(method "name" solverobj) of
				 "forwardeuler" => Solver.FORWARD_EULER {dt = exp2real(method "dt" solverobj)}
			       | "rk4" => Solver.RK4 {dt = exp2real(method "dt" solverobj)}
			      (* | "midpoint" => Solver.MIDPOINT {dt = exp2real(method "dt" solverobj)}
			       | "heun" => Solver.HEUN {dt = exp2real(method "dt" solverobj)}*)
			       | "ode23" => Solver.ODE23 {dt = exp2real(method "dt" solverobj),
							  abs_tolerance = exp2real(method "abstol" solverobj),
							  rel_tolerance = exp2real(method "reltol" solverobj)}
			       | "ode45" => Solver.ODE45 {dt = exp2real(method "dt" solverobj),
							  abs_tolerance = exp2real(method "abstol" solverobj),
							  rel_tolerance = exp2real(method "reltol" solverobj)}
			       | "cvode" => Solver.CVODE {dt = exp2real(method "dt" solverobj),
							  abs_tolerance = exp2real(method "abstol" solverobj),
							  rel_tolerance = exp2real(method "reltol" solverobj)}
			       | name => DynException.stdException ("Invalid solver encountered: " ^ name, "ModelTranslate.translate.obj2dofmodel", Logger.INTERNAL)

		fun expHasN exp =
		    if ExpProcess.isInitialConditionEq exp then
			case ExpProcess.getLHSSymbol exp of
			    Exp.SYMBOL (_, props) => 
			    (case Property.getIterator props of
				 SOME iters =>
				 (List.exists (fn(s,p) => s = (Symbol.symbol "n")) iters)
			       | NONE => false)
			  | _ => DynException.stdException(("Invalid initial condition generated, lhs is not a symbol: " ^ (ExpProcess.exp2str exp)), "ModelTranslate.translate.expHasN", Logger.INTERNAL)
		    else
			false

		fun classHasN ({exps, ...}: DOF.class) =
		    List.exists expHasN (!exps)

		val discrete_iterators = 
		    if List.exists classHasN classes then
			[(Symbol.symbol "n", DOF.DISCRETE)]
		    else
			[]

		val systemproperties = (*{solver=solver}*){iterators=[(Symbol.symbol "t", DOF.CONTINUOUS solver)] @ discrete_iterators,
							   time=(exp2real (method "min_t" solverobj), exp2real (method "max_t" solverobj)),
							   precision=DOF.DOUBLE (* TODO: Make this configurable *)}
	    in
		(classes, topinstance, systemproperties)
	    end

	exception TranslationError

    in
	if (not (istype (object, "ModelInstance"))) then
	    raise TypeMismatch ("Expected a Model instance but received " ^ (pretty object))
	else
	    (SOME (obj2dofmodel object) before DynException.checkToProceed())
	    handle TranslationError => NONE
		 | e => NONE before 
			(app (fn(s) => print(s ^ "\n")) (MLton.Exn.history e);
			 DynException.checkpoint "ModelTranslate.translate" e)
    end
end
