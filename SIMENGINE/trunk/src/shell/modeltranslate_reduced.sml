signature MODELTRANSLATE =
sig
    (* The string returned by translate is the name of the model *)
    val translate : ((KEC.exp -> KEC.exp) * KEC.exp) -> (DOF.model) option

    val translateExp : ((KEC.exp -> KEC.exp) * KEC.exp) -> (Exp.exp) option
    val reverseExp : ((KEC.exp -> KEC.exp) * Exp.exp) -> (KEC.exp) option
    val rule2rewriterule : ((KEC.exp -> KEC.exp) * KEC.exp) -> (Rewrite.rewrite) option
    val rules2rewriterules : ((KEC.exp -> KEC.exp) * KEC.exp) -> (Rewrite.rewrite list) option
    val exp2str : KEC.exp -> string
end

structure ModelTranslate : MODELTRANSLATE=
struct

val e2s = ExpPrinter.exp2str
val b2s = Util.b2s
val i2s = Util.i2s
val r2s = Util.r2s

(* fill in this reference when calling translate/translateExp *)
val exec = ref (fn(e) => e)

fun error (msg) =
    (*TODO: replace *)
    DynException.stdException (msg, "ModelTranslate", Logger.USER)

(* helper methods *)
val pretty = PrettyPrint.kecexp2prettystr (!exec)

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
    (!exec) (KEC.SEND {message=Symbol.symbol name, object=object})

(* Evaluates a function application. *)
fun apply (f, a) = (!exec) (KEC.APPLY {func=f, args=KEC.TUPLE a})

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

	val result = (!exec) checkexp
    in
	exp2bool result
    end

fun isdefined KEC.UNDEFINED = false
  | isdefined _ = true
    
fun vec2list (KEC.VECTOR vec) = KEC.kecvector2list (vec)
  | vec2list exp =
    DynException.stdException ("Expected a vector but received " ^ (PrettyPrint.kecexp2nickname exp) ^ ": " ^ (pretty exp), "ModelTranslate.translate.vec2list", Logger.INTERNAL)
    





fun model2classname object =
    Symbol.symbol(exp2str(method "name" object))

fun dofexp2kecexp exp =
    case exp of 
	Exp.TERM (Exp.INT i) => KEC.LITERAL (KEC.CONSTREAL (Real.fromInt i))
      | Exp.TERM (Exp.REAL r) => KEC.LITERAL (KEC.CONSTREAL (r))
      | Exp.TERM (Exp.BOOL b) => KEC.LITERAL (KEC.CONSTBOOL (b))
      | Exp.TERM (Exp.SYMBOL (s, props)) => 
	let
	    (*TODO: use props *)
	in 
	    (!exec) (KEC.APPLY{func=KEC.SEND{message=Symbol.symbol "new", object=KEC.SYMBOL (Symbol.symbol "Symbol")},
			       args=KEC.TUPLE [KEC.LITERAL(KEC.CONSTSTR (Symbol.name s))]})
	end
      | Exp.FUN (func as Fun.BUILTIN oper, args) =>
(*	let
	    val funname = case oper of
			      Fun.ADD => "operator_add"
			    | Fun.SUB => "operator_subtract"
			    | Fun.NEG => "operator_neg"
			    | Fun.MUL => "operator_multiply"
			    | Fun.DIVIDE => "operator_divide"
			    | Fun.MODULUS => "operator_modulus"
			    | Fun.POW => "power"

			    | Fun.DERIV => "operator_deriv"
			    | _ => error "Unsupported dof operation"

	    val exp =
		KEC.APPLY{func=KEC.SYMBOL (Symbol.symbol funname),
			  args=KEC.TUPLE (map dofexp2kecexp args)}
*)
	let
	    val funname = FunProps.op2name func

	    val exp =
		KEC.APPLY{func=KEC.SYMBOL (Symbol.symbol "modelop"),
			  args=KEC.TUPLE [KEC.LITERAL (KEC.CONSTSTR funname), KEC.list2kecvector (map dofexp2kecexp args)]}
	in
	    (!exec) (exp)
	end
	    
(*      | Exp.FUN (Fun.INST {classname, instname, props}) =>*)
      | _ => error ("Unsupported dof exp encountered: " ^ (ExpPrinter.exp2str exp))


fun quantity_to_dof_exp (KEC.LITERAL (KEC.CONSTREAL r)) = ExpBuild.real r
  | quantity_to_dof_exp (KEC.LITERAL (KEC.CONSTBOOL b)) = ExpBuild.bool b
  | quantity_to_dof_exp (quantity as KEC.VECTOR _) = 
    let val expressions = vec2list quantity

	fun exp_to_term (Exp.TERM t) = t
	  | exp_to_term _ = Exp.NAN
    in
	Exp.TERM (Exp.LIST (map (exp_to_term o quantity_to_dof_exp) expressions, [List.length expressions]))
    end

  | quantity_to_dof_exp quantity =
    if istype (quantity, "ModelOperation") then
	modeloperation_to_dof_exp quantity
    else if istype (quantity, "SimQuantity") orelse istype (quantity, "Input") then
	simquantity_to_dof_exp quantity
    else raise TypeMismatch ("Unexpected type of expression object; received " ^ (pretty quantity))

	
(* Derivatives are reduced to a symbol with a derivative property.
 * All other operations are mapped to builtin functions. *)
and modeloperation_to_dof_exp quantity =
    case exp2str (method "name" quantity)
     of "deriv" =>
	let val (order, quantity')
	      = case vec2list (method "args" quantity)
		 of [ord, quant] => (exp2int ord, quant)
		  | _ => DynException.stdException ("Unexpected arguments to derivative operation", 
						    "ModelTranslate.translate.modeloperation_to_dof_exp", Logger.INTERNAL)
	in
	    case quantity_to_dof_exp quantity'
	     of Exp.TERM (Exp.SYMBOL (name, properties)) =>
		let 
		    val iterators = 
			if istype (quantity', "IteratorReference") then
			    map (fn(e) => Symbol.symbol (exp2str (method "name" e))) (vec2list (method "indices" quantity'))
			else			    
			    [Symbol.symbol (exp2str (method "name" (method "iter" quantity')))]
		    val properties' = Property.setDerivative properties (order, iterators)
		in
		    Exp.TERM (Exp.SYMBOL (name, properties'))
		end
	      | _ => error "Derivatives of arbitrary expressions are not supported."
	end
      | name => 
	Exp.FUN (Fun.BUILTIN (FunProps.name2op (Symbol.symbol name)),
		 map quantity_to_dof_exp (vec2list (method "args" quantity)))

and simquantity_to_dof_exp quantity =
    if (istype (quantity, "OutputBinding")) then		    
	ExpBuild.var((exp2str (method "instanceName" quantity)) ^ "." ^ (exp2str (method "name" quantity)))

    else if (istype (quantity, "Intermediate")) then 
	ExpBuild.var(exp2str (method "name" quantity))

    else if (istype (quantity, "GenericIterator")) then
	ExpBuild.itervar (exp2str (method "name" quantity))

    else if istype (quantity, "IteratorReference") then
	let
	    val obj = quantity
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
		else if (istype (arg, "PreviousTimeIterator")) then
		    (iterator, Iterator.RELATIVE (exp2int (method "index" arg)))
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

    else if (istype (quantity, "SymbolPattern")) then
	let val name = Symbol.symbol (exp2str (method "name" quantity))
	    val arity
	      = case (exp2int (method "min" quantity), exp2int (method "max" quantity))
		 of (1, 1) => Pattern.ONE
		  | (1, ~1) => Pattern.ONE_OR_MORE
		  | (0, ~1) => Pattern.ZERO_OR_MORE
		  | (x, y) => if x = y then Pattern.SPECIFIC_COUNT x
			      else Pattern.SPECIFIC_RANGE (x, y)
	in
	    Exp.TERM (Exp.PATTERN (name, PatternProcess.predicate_any, arity))
	end

    else if (istype (quantity, "State")) andalso isdefined (method "iter" quantity) then
	ExpBuild.ivar (exp2str (method "name" quantity)) [(Symbol.symbol(exp2str(method "name" (method "iter" quantity))), Iterator.RELATIVE 0)]

    else if (istype (quantity, "Symbol")) then
        Exp.TERM (Exp.SYMBOL (Symbol.symbol (exp2str (method "name" quantity)), 
                              Property.setIsRewriteSymbol Property.default_symbolproperty true))
    else
	ExpBuild.var(exp2str (method "name" quantity))



fun vecIndex (vec, index) =
    send "at" vec (SOME [int2exp index])


fun expHasIterator iter exp =
    List.exists 
	(fn(exp')=>
	   case exp' of
	       Exp.SYMBOL (_, props) => 
	       (case Property.getIterator props of
		    SOME iters =>
		    (List.exists (fn(s,p) => s = iter) iters)
		  | NONE => false)
	     | _ => DynException.stdException(("Invalid initial condition generated, lhs is not a symbol: " ^ (e2s exp)), "ModelTranslate.expHasIterator", Logger.INTERNAL))
	(ExpProcess.getLHSTerms exp)


fun createClass classes object =
    let
	val name =Symbol.symbol (exp2str (method "name" object))

	fun exp2term (Exp.TERM t) = t
	  | exp2term _ = Exp.NAN

	fun obj2output object =		    
	    (* object = [name, value] *)
	    let
		val value = vecIndex (object, 2)
		val name = (* check if the output has a supplied temporal iterator.  If not, the value is unit *)
		    if istype (value, "Output") then
			case method "iter" (value) of
			    KEC.UNIT =>
			    exp2term (ExpBuild.var (exp2str(vecIndex (object, 1))))
			  | iter => exp2term (ExpBuild.ivar (exp2str(vecIndex (object, 1))) 
							    [(Symbol.symbol (exp2str (method "name" iter)), 
							      Iterator.RELATIVE 0)])
		    else
			exp2term (ExpBuild.var (exp2str(vecIndex (object, 1))))

		val (contents, condition) =
		    if istype (value, "Output") then
			(case method "contents" value of
			     KEC.TUPLE args => map quantity_to_dof_exp args
			   | exp => [quantity_to_dof_exp exp],
			 quantity_to_dof_exp(method "condition" value))
		    else
			([quantity_to_dof_exp(value)],
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

	fun quantity2exp object =
	    (* FIXME add iterators appearing on rhs to symbols on lhs. *)
	    if (istype (object, "Intermediate")) then
		let val (lhs, rhs) = (quantity_to_dof_exp (method "lhs" (method "eq" object)),
				      quantity_to_dof_exp (method "rhs" (method "eq" object)))
				     
		    val rhs_iterators = ExpProcess.iterators_of_expression rhs

		    val (lhs_name, lhs_properties)
		      = case lhs
			 of Exp.TERM (Exp.SYMBOL s) => s
			  | _ => error ("Unexpected expression on lhs of intermediate " ^ (pretty object))

		    val lhs_properties' = (*Property.setIterator lhs_properties (map (fn iter_name => (iter_name, Iterator.RELATIVE 0)) (SymbolSet.listItems rhs_iterators))*)lhs_properties
		in
		    [ExpBuild.equals (Exp.TERM (Exp.SYMBOL (lhs_name, lhs_properties')), rhs)]
		end
	    else if (istype (object, "State")) then
		let
		    val hasEquation = exp2bool (send "hasEquation" object NONE)
		    val name = exp2str (method "name" object)

		    val (lhs,rhs) = 
			(quantity_to_dof_exp (method "lhs" (method "eq" object)),
			 quantity_to_dof_exp (method "rhs" (method "eq" object)))

		    val eq = ExpBuild.equals(lhs, rhs)			

		    val timeiterator = (exp2str (method "name" (method "iter" object)))

		    val spatialiterators = case (ExpProcess.exp2termsymbols lhs) of
					       [Exp.SYMBOL (sym,props)] => 
					       (case Property.getIterator props of
						    SOME (possibletemporal::spatial) => 
						    if (#1 possibletemporal) = (Symbol.symbol timeiterator) then
							spatial
						    else
							possibletemporal :: spatial
						  | _ => nil)
					     | _ => error "Invalid number of symbols on left hand side of equation"
						 
		    val spatialiterators = map (Symbol.name o #1) spatialiterators
   
		    val initlhs = ExpBuild.initavar(name, 
						    if hasEquation then timeiterator else Symbol.name(Iterator.postProcessOf timeiterator),
						    spatialiterators)

		    val init = ExpBuild.equals(initlhs,
					       quantity_to_dof_exp (getInitialValue object))

		    val keccondeqs = vec2list (method "condEqs" object)

		    val sym = ExpBuild.avar name timeiterator

		    val condeqs = case keccondeqs of
				      nil => 
				      if hasEquation then nil
				      else
					  [ExpBuild.equals (ExpBuild.ivar name [(Iterator.postProcessOf timeiterator, Iterator.RELATIVE 1)],
							    ExpBuild.ivar name [(Iterator.postProcessOf timeiterator, Iterator.RELATIVE 0)])]
				    | keccondeqs => 
				      let
					  val (lhs, defaultval) = 
					      if hasEquation then
						  (ExpBuild.ivar name
								[(Iterator.updateOf timeiterator, Iterator.RELATIVE 1)],
						   ExpBuild.ivar name
								[(Iterator.updateOf timeiterator, Iterator.RELATIVE 0)])
					      else
						  (ExpBuild.ivar name
								 [(Iterator.postProcessOf timeiterator, Iterator.RELATIVE 1)],
						   ExpBuild.ivar name
								 [(Iterator.postProcessOf timeiterator, Iterator.RELATIVE 0)])

					  fun buildIf (condeq, exp) =
					      Exp.FUN (Fun.BUILTIN (FunProps.name2op (Symbol.symbol "if")),
						       [quantity_to_dof_exp (method "cond" condeq), quantity_to_dof_exp (method "rhs" condeq), exp])


					  val condexp = foldl buildIf defaultval keccondeqs
				      in
					  [ExpBuild.equals (lhs, condexp)]
				      end

		in
		    init :: (if hasEquation then [eq] else []) @ condeqs
		end
	    else if istype (object, "Event") then
		let
		    val name = ExpBuild.event (exp2str (method "name" object))
		    val condition = quantity_to_dof_exp (method "condition" object)
		in
		    [ExpBuild.equals (name, condition)]
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

		val lhs = Exp.TUPLE (map (fn(out) => exp2term (ExpBuild.var out)) output_names)

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
					     props=InstProps.setIterators InstProps.emptyinstprops iterators},
				   map (fn(i) => quantity_to_dof_exp i) input_exps)

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

	val classHasN = List.exists (expHasIterator (Symbol.symbol "n")) (List.filter ExpProcess.isInitialConditionEq exps)
	val classHasT = List.exists (expHasIterator (Symbol.symbol "t")) (List.filter ExpProcess.isInitialConditionEq exps)

	val classform = DOF.INSTANTIATION 
			    {readstates=(if classHasT then [Symbol.symbol "t"] else []) @ (if classHasN then [Symbol.symbol "n"] else []),
			     writestates=(if classHasT then [Symbol.symbol "t"] else []) @ (if classHasN then [Symbol.symbol "n"] else [])}

	fun buildIterator exp =
	    {name=(Symbol.symbol o exp2str) (method "name" exp),
	     low=exp2real (method "low" (method "value" exp)),
	     step=exp2real (method "step" (method "value" exp)),
	     high=exp2real (method "high" (method "value" exp))}

    in
	({name=name, 
	  properties={sourcepos=PosLog.NOPOS,basename=name,classform=classform,classtype=DOF.MASTER name},
	  inputs=ref (map obj2input (vec2list(method "inputs" object))),
	  outputs=ref (map obj2output (vec2list (method "contents" (method "outputs" object)))),
	  iterators=map buildIterator (vec2list (send "getSpatialIterators" object NONE)),
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
	fun transSolver solverobj =
	    case exp2str(method "name" solverobj) of			 
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
						  rel_tolerance = exp2real(method "reltol" solverobj),
						  max_order = exp2int (method "cv_maxorder" solverobj),
						  lmm = case exp2str (method "cv_lmm" solverobj) of
							    "CV_BDF" => Solver.CV_BDF
							  | "CV_ADAMS" => Solver.CV_ADAMS
							  | s => (Logger.log_warning (Printer.$("Invalid linear method '"^s^"' chosen: Valid options are CV_BDF or CV_ADAMS.  Defaulting to CV_BDF"));Solver.CV_BDF),
						  iter = case exp2str (method "cv_iter" solverobj) of
							     "CV_FUNCTIONAL" => Solver.CV_FUNCTIONAL
							   | "CV_NEWTON" => Solver.CV_NEWTON
							   | s => (Logger.log_warning (Printer.$("Invalid iteration method '"^s^"' chosen: Valid options are CV_FUNCTIONAL or CV_NEWTON.  Defaulting to CV_NEWTON"));Solver.CV_NEWTON),
						  solv = case exp2str (method "cv_solv" solverobj) of
							     "CVDENSE" => Solver.CVDENSE
							   | "CVDIAG" => Solver.CVDIAG
							   | "CVBAND" => Solver.CVBAND {upperhalfbw=exp2int (method "cv_upperhalfbw" solverobj),
											lowerhalfbw=exp2int (method "cv_lowerhalfbw" solverobj)}
							   | s => (Logger.log_warning (Printer.$("Invalid solver method '"^s^"' chosen: Valid options are CVDENSE, CVDIAG or CVBAND.  Defaulting to CVDENSE"));Solver.CVDENSE)
							 }
		       | name => DynException.stdException ("Invalid solver encountered: " ^ name, "ModelTranslate.translate.obj2dofmodel", Logger.INTERNAL)

	fun buildTemporalIterator (obj) =
	    let
		val name = exp2str (method "name" obj)
	    in
		[(Symbol.symbol name, if exp2bool(method "isContinuous" obj) then
					  DOF.CONTINUOUS (transSolver (method "solver" obj))
				      else
					  DOF.DISCRETE{sample_period=exp2real(method "sample_period" obj)}),
		 (Iterator.postProcessOf name, DOF.POSTPROCESS (Symbol.symbol name)),
		 (Iterator.updateOf name, DOF.UPDATE (Symbol.symbol name))]
	    end

	val temporal_iterators = 
	    (Symbol.symbol "always", DOF.IMMEDIATE) ::
	    (foldl (op @) nil (map buildTemporalIterator (vec2list (send "getTemporalIterators" (method "modeltemplate" object) NONE))))

	(*val key_value_pairs = vec2list (method "contents" (method "settings" (method "modeltemplate" object)))*)
	val precision = exp2str (method "precision" (method "settings" (method "modeltemplate" object)))
	val target = exp2str (method "target" (method "settings" (method "modeltemplate" object)))
	val num_models = exp2int (method "num_models" (method "settings" (method "modeltemplate" object)))
	val debug = exp2bool (method "debug" (method "settings" (method "modeltemplate" object)))
	val profile = exp2bool (method "profile" (method "settings" (method "modeltemplate" object)))
		      
	(* only support openmp right now *)
	val target = if StdFun.toLower target = "parallelcpu" then
			 "openmp"
		     else
			 target

	(* only support cuda right now *)
	val target = if StdFun.toLower target = "gpu" then
			 "cuda"
		     else
			 target

	(* evaluate cuda *)
	val cuda_namespace = method "CUDA" (KEC.SYMBOL (Symbol.symbol "Devices"))
	val num_cuda_devices = exp2int (send "numDevices" cuda_namespace NONE)
	val target = if StdFun.toLower target = "cuda" andalso num_cuda_devices = 0 then
			 (Logger.log_userwarning nil (Printer.$("No CUDA capable device found, using a parallel CPU implementation instead"));
			  "openmp")
		     else
			 target

	val (deviceCapability, numMPs, globalMemory) = 
	    if StdFun.toLower target = "cuda" then
		let
		    (* choose largest cuda device by numMPs *)
		    val id_size_map = map (fn(id)=>(id, exp2int (send "deviceMultiprocessors" cuda_namespace (SOME [int2exp id])))) (List.tabulate (num_cuda_devices,fn(x)=>x+1))
		    val (id, numMPs) = (StdFun.max_by_fun (fn((_,n1),(_,n2))=>n1>n2) id_size_map)

		    val computeCapability = exp2str (send "deviceCapability" cuda_namespace (SOME [int2exp id]))
		in
		    (case computeCapability 
		      of "1.1" => Target.COMPUTE11
		       | "1.3" => Target.COMPUTE13
		       | "9999.9999" => (Logger.log_usererror nil (Printer.$("Only the emulation CUDA device has been found.  Please verify the device driver is installed properly and that you have r/w permissions on /dev/nvidia*"));
					DynException.setErrored();
					DynException.checkToProceed();
					Target.COMPUTE11)
		       | _ => (Logger.log_userwarning nil (Printer.$("Unexpected compute capability "^computeCapability^" on CUDA device, reverting to 1.1"));
			       Target.COMPUTE11),
		     numMPs,
		     exp2int (send "deviceGlobalMem" cuda_namespace (SOME [int2exp id])))
		end
	    else
		(Target.COMPUTE13, 0, 0)

	val systemproperties = {iterators=temporal_iterators,
				precision= case (StdFun.toLower precision)
					    of "single" => DOF.SINGLE
					     | "float" => DOF.SINGLE
					     | "double" => DOF.DOUBLE
					     | _ => DynException.stdException
							(("Unexpected precision value " ^ (precision)),
							 "ModelTranslate.obj2dofmodel", 
							 Logger.INTERNAL),
				target= case (StdFun.toLower target)
					 of "cpu" => Target.CPU
					  | "openmp" => Target.OPENMP
					  | "cuda" => Target.CUDA {compute=deviceCapability, 
								   multiprocessors=numMPs, 
								   globalMemory=globalMemory}
					  | _ => DynException.stdException
						     (("Unexpected target value " ^ (target)),
						      "ModelTranslate.obj2dofmodel", 
						      Logger.INTERNAL),
				num_models=num_models,
				debug=debug,
				profile=profile}
    in
	(classes, topinstance, systemproperties)
    end

exception TranslationError

fun translate (execFun, object) =
    (exec := execFun;
     if (not (istype (object, "ModelInstance"))) then
	 raise TypeMismatch ("Expected a Model instance but received " ^ (pretty object))
     else
	 (SOME (obj2dofmodel object) before DynException.checkToProceed())
	 handle TranslationError => NONE
	      | e => NONE before 
		     (app (fn(s) => print(s ^ "\n")) (MLton.Exn.history e);
		      DynException.checkpoint "ModelTranslate.translate" e))

fun translateExp (execFun, exp) =
    (exec := execFun;
     SOME (quantity_to_dof_exp exp))
	 handle TranslationError => NONE
	      | e => NONE before 
		     (app (fn(s) => print(s ^ "\n")) (MLton.Exn.history e);
		      DynException.checkpoint "ModelTranslate.translate" e)

fun reverseExp (execFun, exp) =
    (exec := execFun;
     SOME (dofexp2kecexp exp))
	 handle TranslationError => NONE
	      | e => NONE before 
		     (app (fn(s) => print(s ^ "\n")) (MLton.Exn.history e);
		      DynException.checkpoint "ModelTranslate.translate" e)
     

fun rule2rewriterule (execFun, exp) =
    let
	val _ = exec := execFun
	val find = quantity_to_dof_exp (method "find" exp)
	val test =  NONE (*TODO: fill this in *)
	val replace = Rewrite.RULE (quantity_to_dof_exp (method "replacement" exp))

    in
	SOME {find=find,
	      test=test,
	      replace=replace}
    end

fun rules2rewriterules (execFun, exp) =
    (exec := execFun;
     SOME (map (valOf o (fn(e) => rule2rewriterule (execFun, e))) (vec2list exp)))

end


