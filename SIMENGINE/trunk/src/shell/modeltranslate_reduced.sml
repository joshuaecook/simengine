signature MODELTRANSLATE =
sig
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
val exec = ref (fn(e) => e before DynException.stdException ("exec not filled in properly in model translation", "ModelTranslate.exec", Logger.INTERNAL))

exception TranslationError

fun error (msg) =
    (Logger.log_error (Printer.$ msg);
     DynException.setErrored();
     raise TranslationError)

fun warning (msg) =
    Logger.log_warning (Printer.$ msg)

(* helper methods *)
fun pretty (s) = PrettyPrint.kecexp2prettystr (!exec) s

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
	let
	    val funname = FunProps.op2name func

	    val exp =
		KEC.APPLY{func=KEC.SYMBOL (Symbol.symbol "modelop"),
			  args=KEC.TUPLE [KEC.LITERAL (KEC.CONSTSTR funname), KEC.list2kecvector (map dofexp2kecexp args)]}
	in
	    (!exec) (exp)
	end
	    
      | _ => error ("Unsupported dof exp encountered: " ^ (ExpPrinter.exp2str exp))


fun quantity_to_dof_exp (KEC.LITERAL (KEC.CONSTREAL r)) = ExpBuild.real r
  | quantity_to_dof_exp (KEC.LITERAL (KEC.CONSTBOOL b)) = ExpBuild.bool b
  | quantity_to_dof_exp (KEC.UNDEFINED) = Exp.TERM Exp.NAN
  | quantity_to_dof_exp (quantity as KEC.VECTOR _) = 
    let val expressions = vec2list quantity

	fun exp_to_term (Exp.TERM t) = t
	  | exp_to_term _ = Exp.NAN
    in
	(*Exp.TERM (Exp.LIST (map (exp_to_term o quantity_to_dof_exp) expressions, [List.length expressions]))*)
	ExpBuild.explist (map quantity_to_dof_exp expressions)
    end

  | quantity_to_dof_exp quantity =
    if istype (quantity, "ModelOperation") then
	modeloperation_to_dof_exp quantity
    else if istype (quantity, "SimQuantity") orelse istype (quantity, "Input") then
	simquantity_to_dof_exp quantity
    else (raise TypeMismatch ("Unexpected type of expression object; received " ^ (pretty quantity)))

	
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
			else if istype (quantity', "State") then
			    [Symbol.symbol (exp2str (method "name" (method "iter" quantity')))]
			else
			    error ("Derivative of quantity "^(exp2str (method "name" quantity'))^" not declared as a state is not supported.")
		    val properties' = Property.setDerivative properties (order, iterators)
		in
		    Exp.TERM (Exp.SYMBOL (name, properties'))
		end
	      | _ => error "Derivatives of arbitrary expressions are not supported."
	end
      | name => 
	Exp.FUN (Fun.BUILTIN (MathFunctionProperties.name2op (Symbol.symbol name)),
		 map quantity_to_dof_exp (vec2list (method "args" quantity)))

and simquantity_to_dof_exp quantity =
    if (istype (quantity, "OutputBinding")) then
	ExpBuild.var((exp2str (method "instanceName" quantity)) ^ "." ^ (exp2str (method "name" quantity)))

    else if (istype (quantity, "Intermediate")) then 
	ExpBuild.var(exp2str (method "name" quantity))

    else if (istype (quantity, "GenericIterator")) then
	let
	    val name = exp2str (method "name" quantity)
	in
	    if (istype (quantity, "PreviousTimeIterator")) then
		(* this will handle cases such as equation prev_t = t[-1] *)
		let
		    val ind = exp2int (method "index" quantity)
		in
		    ExpBuild.relitervar (name, ind)
		end
	    else
		(* otherwise, this creates references to the value of the iterator *)
		ExpBuild.itervar name
	end

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
			error ("Encountered iterator "^(Symbol.name iterator)^" ["^(exp2str (method "name" arg))^"] in index of "^(name)^" where "^(Symbol.name iterator)^" was expected")
		else if istype (arg, "RelativeOffset") then
		    if iterator = (Symbol.symbol (exp2str (method "name" (method "simIterator" arg)))) then
			(iterator, Iterator.RELATIVE (exp2int(method "step" arg)))
		    else
			error ("Encountered iterator "^(exp2str (method "name" (method "simIterator" arg)))^" in index of "^(name)^" where "^(Symbol.name iterator)^" was expected")
		else if (istype (arg, "PreviousTimeIterator")) then
		    let 
			val idx = exp2int (method "index" arg)
			val name = exp2str (method "name" arg)
		    in
			(iterator, Iterator.RELATIVE idx)
		    end
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

    else if (istype (quantity, "Random")) andalso isdefined (method "iter" quantity) then
	((*Util.log("Found random quantity: " ^ (exp2str (method "name" quantity)));*)
	 ExpBuild.ivar (exp2str (method "name" quantity)) [(Iterator.preProcessOf (exp2str(method "name" (method "iter" quantity))), Iterator.RELATIVE 0)])

    else if (istype (quantity, "State")) andalso isdefined (method "iter" quantity) then
	ExpBuild.ivar (exp2str (method "name" quantity)) [(Symbol.symbol(exp2str(method "name" (method "iter" quantity))), Iterator.RELATIVE 0)]
    else if (istype (quantity, "Symbol")) then
        ExpBuild.pvar(exp2str (method "name" quantity))
    else if (istype (quantity, "RandomValue")) then
	if isdefined (method "normal" quantity) andalso exp2bool (method "normal" quantity) then
	    ExpBuild.normal_rand()
	else
	    ExpBuild.uniform_rand()
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


fun createClass top_class classes object =
    let
	val name =Symbol.symbol (exp2str (method "name" object))
	(*val _ = Logger.log_notice (Printer.$ ("Translating model '"^(Symbol.name name)^"'"))*)

	(* some quick error checking in the class *)
	(* - start with output counts *)
	val classOutputs = vec2list (method "contents" (method "outputs" object))
	val _ = if List.length classOutputs > 0 then
		    () (* this is good *)
		else if top_class then
		    warning ("Model " ^ (Symbol.name name) ^ " does not have any outputs defined.  The final state values generated from a simulation will still be recorded.")
		else
		    error ("Sub-model " ^ (Symbol.name name) ^ " does not have any outputs defined.")

	fun exp2term (Exp.TERM t) = t
	  | exp2term _ = Exp.NAN

	fun obj2input obj =
	    let
		val iter = method "iter" obj
		val name =
		    if isdefined iter then
			ExpBuild.initavar (exp2str(method "name" obj), exp2str (method "name" (method "iter" obj)), nil)
		    else
			ExpBuild.var (exp2str (method "name" obj))

		val input = 
		    DOF.Input.make
			{name=exp2term name,
			 default=case exp2realoption (method "default" obj) of
				     SOME r => SOME (ExpBuild.real r)
				   | NONE => NONE,
			 behaviour=case exp2str (method "when_exhausted" obj)
				    of "cycle" => DOF.Input.CYCLE
				     | "halt" => DOF.Input.HALT
				     | "hold" => DOF.Input.HOLD
				     | str => DynException.stdException ("Unrecognized input behaviour " ^ str ^ ".", "ModelTranslate.createClass.obj2input", Logger.INTERNAL)}
	    in
		case DOF.Input.behaviour input
		 of DOF.Input.HALT =>
		    (case DOF.Input.default input
		      of NONE => input
		       | SOME (Exp.TERM Exp.NAN) => input
		       | _ =>
			 error ("Default value is not allowed for input " ^ (Term.sym2name (DOF.Input.name input)) ^ " with {halt_when_exhausted}."))
		  | _ => input
	    end

	val inputs = map obj2input (vec2list(method "inputs" object))
	val inputNames = map DOF.Input.name inputs

	fun obj2output obj =		    
	    (* object = [name, value] *)
	    let
		val value = vecIndex (obj, 2)
		val name = (* check if the output has a supplied temporal iterator.  If not, the value is unit *)
		    if istype (value, "Output") then
			case method "iter" (value) of
			    KEC.UNIT =>
			    exp2term (ExpBuild.var (exp2str(vecIndex (obj, 1))))
			  | iter => exp2term (ExpBuild.ivar (exp2str(vecIndex (obj, 1))) 
							    [(Symbol.symbol (exp2str (method "name" iter)), 
							      Iterator.RELATIVE 0)])
		    else
			exp2term (ExpBuild.var (exp2str(vecIndex (obj, 1))))

		val (contents, condition) =
		    if istype (value, "Output") then
			(case method "contents" value of
			     KEC.TUPLE args => map quantity_to_dof_exp args
			   | KEC.UNIT => []
			   | exp => [quantity_to_dof_exp exp],
			 let
			     val exp = method "condition" value
			 in
			     quantity_to_dof_exp(exp)
			 end)
		    else
			([quantity_to_dof_exp(value)],
			 Exp.TERM(Exp.BOOL true))
	    in
		DOF.Output.make
		    {name=name,
		     inputs=ref inputNames,
		     contents=contents,
		     condition=condition}
	    end

	fun quantity2exp obj =
	    (* FIXME add iterators appearing on rhs to symbols on lhs. *)
	    (if (istype (obj, "Intermediate")) then
		let val (lhs, rhs) = (quantity_to_dof_exp (method "lhs" (method "eq" obj)),
				      quantity_to_dof_exp (method "rhs" (method "eq" obj)))
				     
		    val rhs_iterators = ExpProcess.iterators_of_expression rhs

		    val (lhs_name, lhs_properties)
		      = case lhs
			 of Exp.TERM (Exp.SYMBOL s) => s
			  | _ => error ("Unexpected expression on lhs of intermediate " ^ (pretty obj))

		    val lhs_properties' = (*Property.setIterator lhs_properties (map (fn iter_name => (iter_name, Iterator.RELATIVE 0)) (SymbolSet.listItems rhs_iterators))*)lhs_properties
		in
		    [ExpBuild.equals (Exp.TERM (Exp.SYMBOL (lhs_name, lhs_properties')), rhs)]
		end
	    else if (istype (obj, "Random")) then 
		(* we treat Random numbers like states so to ensure that the same random number is read from the same name 
                   in a given iteration *)
		let
		    (* pull out the name of the random value defined *)
		    val name = exp2str (method "name" obj)

		    (* create the initial condition *)
		    val timeiterator = (exp2str (method "name" (method "iter" obj)))
		    val spatialiterators = []
		    val inprocessiterator = Iterator.inProcessOf timeiterator
		    val initlhs = ExpBuild.initavar(name, 
						    Symbol.name inprocessiterator,
						    spatialiterators)
		    val init = ExpBuild.equals(initlhs,
					       quantity_to_dof_exp (getInitialValue obj))


		    (* create the equation to generate random numbers *)
		    val (lhs,rhs) = 
			(quantity_to_dof_exp (method "lhs" (method "eq" obj)),
			 quantity_to_dof_exp (method "rhs" (method "eq" obj)))
		    val lhs' = ExpProcess.updateTemporalIterator (inprocessiterator, Iterator.RELATIVE 1) lhs
		    val eq = ExpBuild.equals(lhs', rhs)			
		    
		in
		    [init, eq]
		end
		handle TranslationError => raise TranslationError 
		     | e => DynException.checkpoint ("ModelTranslate.createClass.quantity2exp.RANDOM [name="^(exp2str (method "name" obj))^"]") e
	    else if (istype (obj, "State")) then
		let
		    val hasEquation = exp2bool (send "hasEquation" obj NONE)
		    val name = exp2str (method "name" obj)

		    val (lhs,rhs) = 
			(quantity_to_dof_exp (method "lhs" (method "eq" obj)),
			 quantity_to_dof_exp (method "rhs" (method "eq" obj)))

		    val eq = ExpBuild.equals(lhs, rhs)			

		    val timeiterator = (exp2str (method "name" (method "iter" obj)))

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
						    if hasEquation then timeiterator else Symbol.name(Iterator.inProcessOf timeiterator),
						    spatialiterators)

		    val init = ExpBuild.equals(initlhs,
					       quantity_to_dof_exp (getInitialValue obj))

		    val keccondeqs = vec2list (method "condEqs" obj)

		    val sym = ExpBuild.avar name timeiterator

		    val condeqs = case keccondeqs of
				      nil => 
				      if hasEquation then nil
				      else
					  [ExpBuild.equals (ExpBuild.ivar name [(Iterator.inProcessOf timeiterator, Iterator.RELATIVE 1)],
							    ExpBuild.ivar name [(Iterator.inProcessOf timeiterator, Iterator.RELATIVE 0)])]
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
								 [(Iterator.inProcessOf timeiterator, Iterator.RELATIVE 1)],
						   ExpBuild.ivar name
								 [(Iterator.inProcessOf timeiterator, Iterator.RELATIVE 0)])

					  fun buildIf (condeq, exp) =
					      Exp.FUN (Fun.BUILTIN (MathFunctionProperties.name2op (Symbol.symbol "if")),
						       [quantity_to_dof_exp (method "cond" condeq), quantity_to_dof_exp (method "rhs" condeq), exp])


					  val condexp = foldl buildIf defaultval keccondeqs
				      in
					  [ExpBuild.equals (lhs, condexp)]
				      end

		in
		    init :: (if hasEquation then [eq] else []) @ condeqs
		end
	    else if istype (obj, "Event") then
		let
		    val name = ExpBuild.event (exp2str (method "name" obj))
		    val condition = quantity_to_dof_exp (method "condition" obj)
		in
		    [ExpBuild.equals (name, condition)]
		end
	    else
		DynException.stdException ("Unexpected quantity encountered", "ModelTranslate.translate.createClass.quantity2exp", Logger.INTERNAL))	
	    handle e => DynException.checkpoint ("ModelTranslate.createClass.quantity2exp [name="^(exp2str (method "name" object))^"]") e
		

	fun submodel2exp (obj, (submodelclasses, exps)) =
	    let			
		val classes = submodelclasses (* rkw - added this so that the foldl adds classes *)
		val (class, classes) = getClass false (method "modeltemplate" obj, classes)

		fun outbinding2name obj = 
		    (exp2str (method "instanceName" obj)) ^ "." ^ (exp2str (method "name" obj))

		val output_names = map outbinding2name
				       (vec2list (method "outputs" obj))

		val input_exps = map (fn(inp) => (inp, method "inputVal" inp))
				     (vec2list (method "inputs" obj))

		val name' = #name class

		val objname = Symbol.symbol (exp2str (method "name" obj))

		val lhs = Exp.TUPLE (map (fn(out) => exp2term (ExpBuild.var out)) output_names)

		(* check for NaN on inputs *)
		val _ = app (fn(inp,i) => case i of
					  KEC.UNDEFINED => error ("Undefined Value detected on input " ^ (exp2str (method "name" inp)) ^ " in submodel " ^
								  (Symbol.name objname)^ " in " ^ (Symbol.name name) ^
								  ".  Possibly input value was not specified.")
					| KEC.LITERAL(KEC.CONSTREAL (r)) => if Real.isNan r then
										error ("Value NaN detected on input in submodel " ^
										       (Symbol.name objname)^ 
										       ".  Possibly input value was not specified.")
									    else 
										()
					| _ => ()) 
			    input_exps

		val iterators = map (fn(e) => Symbol.symbol (exp2str (method "name" e))) (vec2list (method "dimensions" obj))

		val instanceInputs = 
		    Exp.CONTAINER
			(Exp.ASSOC
			     (foldl (fn ((obj,quant), tab) => 
					SymbolTable.enter (tab, (Symbol.symbol (exp2str (method "name" obj))), quantity_to_dof_exp quant)
				    ) SymbolTable.empty input_exps))

		val rhs = Exp.FUN (Fun.INST {classname=name',
					     instname=objname,
					     props=InstProps.setIterators InstProps.emptyinstprops iterators},
				   [instanceInputs])

		val exp = ExpBuild.equals (Exp.TERM (Exp.TUPLE []), rhs)

		val output_exps = 
		    map (fn out => 
			    let
				val outname = exp2str (method "name" out)
				val lhs = Exp.TERM (Exp.TUPLE [exp2term (ExpBuild.var ((exp2str (method "instanceName" out)) ^ "." ^ outname))])
				val rhs = Exp.FUN (Fun.OUTPUT {classname=name',
							       instname=objname,
							       outname=Symbol.symbol outname,
							       props=InstProps.emptyinstprops},
						   [instanceInputs])
			    in
				ExpBuild.equals (lhs, rhs)
			    end
			) (vec2list (method "outputs" obj))

		val exps = (exp :: output_exps) @ exps

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
	  properties={sourcepos=PosLog.NOPOS,preshardname=name,classform=classform},
	  inputs=ref inputs,
	  outputs=ref (map obj2output classOutputs),
	  exps=ref exps},
	 submodelclasses)
    end
    handle TranslationError => raise TranslationError
	 | DynException.RestartRepl => raise DynException.RestartRepl
	 | e => DynException.checkpoint "ModelTranslate.createClass" e


and getClass top_class (object, classes) =
    let
	val classname = Symbol.symbol(exp2str (method "name" object))
    in
	case List.find (fn(c) => #name c = classname) classes of
	    SOME c => (c, classes)
	  | NONE => 
	    let
		val (c, classes) = createClass top_class classes object
	    in
		(c, c :: classes)
	    end
    end

fun obj2modelinstance object =
    let
	val classes = []
	val (class, classes) = getClass true (method "modeltemplate" object, classes)
			       
    in
	(classes, {name=NONE,  (*TODO: tie in*)
		   classname=(#name class)})
    end



fun obj2dofmodel object =
    let
	(* grab settings from the registry *)
	val precision = DynamoOptions.getStringSetting "precision"
	val target = DynamoOptions.getStringSetting "target"
	val parallel_models = DynamoOptions.getIntegerSetting "parallel_models"
	val debug = DynamoOptions.isFlagSet "debug"
	val profile = DynamoOptions.isFlagSet "profile"
			
	(* update to licensing default if set to default *)
	val target = if StdFun.toLower target = "default" then
			 Features.defaultTarget()
		     else
			 target			 
		      
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


	fun transSolver solverobj =
	    case exp2str(method "name" solverobj) of			 
			 "forwardeuler" => Solver.FORWARD_EULER {dt = exp2real(method "dt" solverobj)}
		       | "exponentialeuler" => Solver.EXPONENTIAL_EULER {dt = exp2real(method "dt" solverobj)}
		       | "linearbackwardeuler" => Solver.LINEAR_BACKWARD_EULER {dt = exp2real(method "dt" solverobj),
										solv = case exp2str (method "lbe_solv" solverobj) of
											   "LSOLVER_DENSE" => Solver.LSOLVER_DENSE
											 | "LSOLVER_BANDED" => Solver.LSOLVER_BANDED { upperhalfbw = exp2int (method "lbe_upperhalfbw" solverobj),
                                                                                                                                       lowerhalfbw = exp2int (method "lbe_lowerhalfbw" solverobj)}
											 | s => (Logger.log_warning (Printer.$("Invalid linear solver '"^s^"' chosen: Valid options are LSOLVER_DENSE or LSOLVER_BANDED.  Defaulting to LSOLVER_BANDED"));Solver.LSOLVER_BANDED { upperhalfbw = exp2int (method "lbe_upperhalfbw" solverobj),
                                                                                                       lowerhalfbw = exp2int (method "lbe_lowerhalfbw" solverobj)})
									       }
		       | "rk4" => Solver.RK4 {dt = exp2real(method "dt" solverobj)}
		       | "midpoint" => Solver.MIDPOINT {dt = exp2real(method "dt" solverobj)}
		       | "heun" => Solver.HEUN {dt = exp2real(method "dt" solverobj)}
		       | "ode23" => Solver.ODE23 {dt = exp2real(method "dt" solverobj),
						  abs_tolerance = exp2real(method "abstol" solverobj),
						  rel_tolerance = exp2real(method "reltol" solverobj)}
		       | "ode45" => Solver.ODE45 {dt = exp2real(method "dt" solverobj),
						  abs_tolerance = exp2real(method "abstol" solverobj),
						  rel_tolerance = exp2real(method "reltol" solverobj)}
		       | "cvode" => 
			 let
			     val _ = if target = "cuda" then
					(Logger.log_error (Printer.$("CVODE is not currently supported on the GPU"));
					 DynException.setErrored())
				     else
					 ()
			 in
			     Solver.CVODE {dt = exp2real(method "dt" solverobj),
					   abs_tolerance = exp2real(method "abstol" solverobj),
					   rel_tolerance = exp2real(method "reltol" solverobj),
					   max_order = exp2int (method "cv_maxorder" solverobj),
					   lmm = case exp2str (method "cv_lmm" solverobj) of
						     "CV_BDF" => Solver.CV_BDF
						   | "CV_ADAMS" => Solver.CV_ADAMS
						   | s => (Logger.log_warning (Printer.$("Invalid linear method '"^s^
											 "' chosen: Valid options are CV_BDF or CV_ADAMS.  "^
											 "Defaulting to CV_BDF"));
							   Solver.CV_BDF),
					   iter = case exp2str (method "cv_iter" solverobj) of
						      "CV_FUNCTIONAL" => Solver.CV_FUNCTIONAL
						    | "CV_NEWTON" => Solver.CV_NEWTON
						    | s => (Logger.log_warning (Printer.$("Invalid iteration method '"^s^
											  "' chosen: Valid options are CV_FUNCTIONAL or CV_NEWTON.  "^
											  "Defaulting to CV_NEWTON"));Solver.CV_NEWTON),
					   solv = case exp2str (method "cv_solv" solverobj) of
						      "CVDENSE" => Solver.CVDENSE
						    | "CVDIAG" => Solver.CVDIAG
						    | "CVBAND" => Solver.CVBAND {upperhalfbw=exp2int (method "cv_upperhalfbw" solverobj),
										 lowerhalfbw=exp2int (method "cv_lowerhalfbw" solverobj)}
						    | s => (Logger.log_warning (Printer.$("Invalid solver method '"^s^"' chosen: Valid options are CVDENSE, CVDIAG or CVBAND.  Defaulting to CVDENSE"));Solver.CVDENSE)
					  }
			 end
		       | "undefined" => Solver.UNDEFINED
		       | name => DynException.stdException ("Invalid solver encountered: " ^ name, "ModelTranslate.translate.obj2dofmodel", Logger.INTERNAL)

	fun buildTemporalIterator (obj) =
	    let
		val name = exp2str (method "name" obj)
	    in
		[(Symbol.symbol name, if exp2bool(method "isContinuous" obj) then
					  DOF.CONTINUOUS (transSolver (method "solver" obj))
				      else
					  DOF.DISCRETE{sample_period=exp2real(method "sample_period" obj)}),
		 (Iterator.preProcessOf name, DOF.ALGEBRAIC (DOF.PREPROCESS, (Symbol.symbol name))),
		 (Iterator.inProcessOf name, DOF.ALGEBRAIC (DOF.INPROCESS, (Symbol.symbol name))),
		 (Iterator.postProcessOf name, DOF.ALGEBRAIC (DOF.POSTPROCESS, (Symbol.symbol name))),
		 (Iterator.updateOf name, DOF.UPDATE (Symbol.symbol name))]
	    end

	val temporal_iterators = 
	    (Symbol.symbol "always", DOF.IMMEDIATE) ::
	    (foldl (op @) nil (map buildTemporalIterator (vec2list (send "getTemporalIterators" (method "modeltemplate" object) NONE))))

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
					  | "cuda" => Target.CUDA (*{compute=deviceCapability, 
								   multiprocessors=numMPs, 
								   globalMemory=globalMemory} *)
					  | _ => DynException.stdException
						     (("Unexpected target value " ^ (target)),
						      "ModelTranslate.obj2dofmodel", 
						      Logger.INTERNAL),
				parallel_models=parallel_models,
				debug=debug,
				profile=profile}

	(* create a skeletal current model that we can use for creating the model instance.  The most 
	 * important aspect is to make sure that we have a current list of temporal iterators.  This 
	 * is needed for all calls to ExpProcess.  *)
	local
	    val undef = Symbol.symbol "undefined"
	    val top_class = {name=undef,
			     properties={sourcepos=PosLog.NOPOS,preshardname=undef,classform=DOF.INSTANTIATION {readstates=[],writestates=[]}},
			     inputs=ref [],
			     outputs=ref [],
			     exps=ref []}
	    val model = ([top_class], {name=NONE, classname=undef}, systemproperties)
	in
	val _ = CurrentModel.setCurrentModel(model)
	val _ = DOFPrinter.printModel model
	end
			       
	(* now, here, process the entire model *)
	val (classes, topinstance) = obj2modelinstance (object)

    in
	(classes, topinstance, systemproperties)
    end

fun translate (execFun, object) =
    (exec := execFun;
     if (not (istype (object, "ModelInstance"))) then
	 raise TypeMismatch ("Expected a Model instance but received " ^ (pretty object))
     else
	 (SOME (obj2dofmodel object) before DynException.checkToProceed())
	 handle TranslationError => NONE 
	      | DynException.RestartRepl => NONE
	      | DynException.TypeMismatch m => NONE)
    handle e => DynException.checkpoint "ModelTranslate.translate" e

fun translateExp (execFun, exp) =
    (exec := execFun;
     SOME (quantity_to_dof_exp exp))
	 handle TranslationError => NONE
	      | e => NONE before 
		     (DynException.checkpoint "ModelTranslate.translate" e)

fun reverseExp (execFun, exp) =
    (exec := execFun;
     SOME (dofexp2kecexp exp))
	 handle TranslationError => NONE
	      | e => NONE before 
		     (DynException.checkpoint "ModelTranslate.translate" e)
     

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


