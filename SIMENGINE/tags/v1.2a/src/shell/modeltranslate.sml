signature MODELTRANSLATE =
sig
    (* The string returned by translate is the name of the model *)
    val translate : ((KEC.exp -> KEC.exp) * KEC.exp) -> (string * System.treeforest) option
end

structure ModelTranslate : MODELTRANSLATE=
struct

fun translate (exec, object) =
    let
	exception TranslationError

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

	fun real2exp r =
	    KEC.LITERAL (KEC.CONSTREAL r)

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
		
	    
	fun vec2list (KEC.VECTOR {array, ...}) = GeneralUtil.array2list (!array)
	  | vec2list exp =
	    DynException.stdException ("Expected a vector but received " ^ (PrettyPrint.kecexp2nickname exp) ^ ": " ^ (pretty exp), "ModelTranslate.translate.vec2list", Logger.INTERNAL)
	    

	fun expobj2exptree obj =
	    if istype (obj, "ModelOperation") then
		case exp2str (method "name" obj) of
		    "branch" =>
		    let
			val args = (vec2list (method "args" obj))
		    in
			case args of
			    [cond, ift, iff] => ExpTree.IF {cond=expobj2exptree(cond), ift=expobj2exptree(ift), iff=expobj2exptree(iff)}
			  | _ => DynException.stdException ("Incorrect number of arguments in ModelOperation", 
							    "ModelTranslate.translate.expobj2exptree.branch", 
							    Logger.INTERNAL)
		    end
		  | _ =>
		    ExpTree.OP (exp2str (method "name" obj), 
				map expobj2exptree (vec2list(method "args" obj)))
	    else if istype (obj, "SimQuantity") then
		if exp2bool (send "getIsIntermediate" obj NONE) then
		    expobj2exptree (send "getExp" (send "getEquation" obj NONE) NONE)
		else if (exp2bool (send "getIsConstant" obj NONE))  then
		    expobj2exptree (getInitialValue obj)
		else
		    ExpTree.READ (exp2str (dslname obj))
	    else 
		case obj
		 of KEC.LITERAL (KEC.CONSTREAL r) => ExpTree.LITERAL (ExpTree.CONSTREAL r)
		  | KEC.LITERAL (KEC.CONSTBOOL b) => ExpTree.LITERAL (ExpTree.CONSTBOOL b)
		  (* FIXME: Is this an acceptable way to handle undefined? *)
(* 		  | KEC.UNDEFINED => ExpTree.LITERAL (ExpTree.CONSTREAL 0.0) *)
		  | _ => 
		    raise TypeMismatch ("Unexpected type of expression object; received " ^ (pretty obj))

	fun quantity2initdef quantityType (object) =
	    if not (istype (object, quantityType)) then
		raise TypeMismatch ("Expected a " ^ quantityType ^ " instance but received " ^ (pretty object))
	    else
		let
		    val name = exp2str (dslname object)
		    val exp = expobj2exptree (getInitialValue object)
		in
		    (name, exp)
		end

	fun build_history_init (object) =
	    let
		val name = exp2str (dslname object)
		val exp = expobj2exptree (getInitialValue object)
		val depth = Real.floor(exp2real (method "historyDepth" object))

		fun fixname name 0 = name
		  | fixname name i = name ^ "[n + " ^ (GeneralUtil.int2str (i)) ^"]"

		val inits = 
		    if depth < 0 then
			List.tabulate (~1 * depth, 
				    fn (i) => (fixname name (~1 * (i+1)), 
					       exp))
		    else
			[]
	    in
		inits
	    end
	    


	fun equation2def eqobject =
	    if not (istype (eqobject, "Equation")) then
		raise TypeMismatch ("Expected an Equation instance but received " ^ (pretty eqobject))
	    else
		let
		    val statename = exp2str (dslname (method "assigned_state" eqobject))
		    val exp = expobj2exptree (method "expression" eqobject)
		in
		    (statename, exp)
		end

	(* Returns a record option of "high", "low", and "step" values for a Range instance. *)
	fun precision2range object =
	    if istype (object, "InfinitePrecision") then
		raise ValueError ("Cannot convert an infinite precision to a range.")
	    else if istype (object, "Range") then
		{high=exp2real (method "high" object),
		 low=exp2real (method "low" object),
		 step=exp2real (method "step" object)}
	    else
		raise TypeMismatch ("Expected a Precision instance but received " ^ (pretty object))


	fun quantity2range object =
	    if not (istype (object, "SimQuantity")) then
		raise TypeMismatch ("Expected a SimQuantity instance but received " ^ (pretty object))
	    else
		(exp2str (dslname object),
		 precision2range (send "getPrecision" object NONE))

	(*this is to remove any final quantity indexing*)
	fun fixname s = 
	    if (hd (rev (String.explode s)) = #"]") then
		(String.concatWith "[" o rev o tl o rev) (String.tokens(fn(c) => c = #"[") s)
	    else
		s

	fun add_to_table qtype (name, table) =
	    (fn(n) => if fixname n  = name then 
			  qtype
		      else
			  table n)


	fun prefixAdd "" x = x
	  | prefixAdd prefix x = prefix ^ "." ^ x


	fun merge_trees (NONE, atree) = atree
	  | merge_trees (atree, NONE) = atree
	  | merge_trees (SOME atree, SOME btree) = 
	    let
		val (name1, {params=params1,
			     states_init=states_init1,
			     states_run=states_run1,
			     ranges=ranges1,
			     typetable=typetable1,
			     inputs=inputs1,
			     outputs=outputs1,
			     samplerate=samplerate1}) = atree

		val (name2, {params=params2,
			     states_init=states_init2,
			     states_run=states_run2,
			     ranges=ranges2,
			     typetable=typetable2,
			     inputs=inputs2,
			     outputs=outputs2,
			     samplerate=samplerate2}) = btree
	    in
		SOME
		    (name1,
		     {params = params1 @ params2,
		      states_init = states_init1 @ states_init2,
		      states_run = states_run1 @ states_run2,
		      ranges = ranges1 @ ranges2,
		      typetable = fn(x) => (case typetable1 x of
						System.UNKNOWN =>
						typetable2 x
					      | result => result),
		      inputs = inputs1 @ inputs2,
		      outputs = outputs1 @ outputs2,
		      samplerate = samplerate1})
	    end
	    
		
	fun build_history object =
	    let
		val name = exp2str (dslname object)
		val depth = Real.floor(exp2real (method "historyDepth" object))

		fun fixname name 0 = name
		  | fixname name i = name ^ "[n + " ^ (GeneralUtil.int2str (i)) ^"]"

		val eqs = 
		    if depth < 0 then
			List.tabulate (~1 * (depth), 
				    fn(i) => (fixname name (~1 * (i+1)), 
					      ExpTree.READ (fixname name (~1 * i))))
		    else
			[]
	    in
		eqs
	    end

	fun flag_visible_quantity quantity =
	    let
		val name = exp2str (dslname quantity)
		val visi = 
		    case send "getIsVisible" quantity NONE
		     of KEC.LITERAL (KEC.CONSTBOOL true) => NONE
		      | KEC.LITERAL (KEC.CONSTBOOL false) =>
			 DynException.stdException ("Expected true but received false", "ModelTranslate.translate.flag_visible_quantity", Logger.INTERNAL)
		      | obj as KEC.OBJECT object =>
			if istype (obj, "ModelOperation") orelse istype (obj, "SimQuantity") then 
			    SOME (expobj2exptree obj)
			else DynException.stdException ("Expected a ModelOperation but received " ^ (PrettyPrint.kecexp2nickname obj) ^ ": " ^ (pretty quantity), "ModelTranslate.translate.flag_visible_quantity", Logger.INTERNAL)
		      | obj => DynException.stdException ("Expected a Boolean or ModelOperation but received " ^ (PrettyPrint.kecexp2nickname obj), "ModelTranslate.translate.flag_visible_quantity", Logger.INTERNAL)
	    in
		{name=name, condition=visi}
	    end
(*
    // adding equations here
    foreach s in m.getStates() do
      foreach i in 2 .. historyDepth do
        equation m.s[n-(i+1)] = m.s[n-i]
      end
    end

*)

	fun workTrack f object =
	    f object
	    before StatusReporter.reportWork 1

	fun doWork label f object =
	    (StatusReporter.beginProcess (label, 1);
	     workTrack f object)

	fun doWorkList label f objects =
	    (StatusReporter.beginProcess (label, length objects);
	     map (workTrack f) objects)


	fun model2forest (object) =
	    if (istype (object, "Model")) then
		let
		    fun getParameters obj = send "getParameters" obj NONE
		    fun getStates obj = send "getStates" obj NONE
		    fun getIntermediates obj = send "getIntermediates" obj NONE
		    fun getVisibleQuantities obj = send "getVisibleQuantities" obj NONE

		    val tree =
			let
			    val _ = StatusReporter.beginPhase ("Translating Model Description")

			    val outputs = doWorkList "Flagging Visible Quantities"
						     flag_visible_quantity
						     (vec2list (getVisibleQuantities object))

(* 			    val _ = print ("\noutputs:\n\t" ^ (String.concatWith "\n\t" *)
(* 							     (map (fn (out) => (#name out)) *)
(* 								  outputs))) *)

			    val params = doWorkList "Translating Parameters" (quantity2initdef "Parameter")
						    (vec2list (getParameters object))

			    val states_init = doWorkList "Translating State Initial Values" (quantity2initdef "State")
							 (vec2list (getStates object))

			    val history_init = GeneralUtil.flatten (map build_history_init ((vec2list (getStates object))))

			    val visi_init = List.mapPartial (fn {name,condition} =>
								case condition 
								 of SOME visi => SOME (visi, ExpTree.LITERAL (ExpTree.CONSTBOOL false))
								  | _ => NONE) outputs

(* 			    val _ = print ("\nstates_init:\n\t" ^ (String.concatWith "\n\t" *)
(* 								 (map (fn (n,x) => n ^ ": " ^ (ExpTree.exp2str x)) *)
(* 								      (states_init @ history_init @ visi_init)))) *)

			    val eqs_exp = doWork "Transforming Equations" (fn (obj) => send "getEquations" obj NONE)
						 object

			    val states_run = doWorkList "Translating Equations" equation2def
							(vec2list eqs_exp)

			    val history_run = GeneralUtil.flatten (map build_history ((vec2list (getStates object))))

(* 			    val _ = print ("\nstates_run:\n\t" ^ (String.concatWith "\n\t" *)
(* 								 (map (fn (n,x) => n ^ ": " ^ (ExpTree.exp2str x)) *)
(* 								      (states_run @ history_run)))) *)

			    val ranges = doWorkList "Gathering Precision Information" quantity2range
						    ((vec2list (getParameters object)) @ (vec2list (getStates object)))


			    val statenames = map (fn(n,e) => n) states_init
			    val paramnames = map (fn(n,e) => n) params
			    val typetable = (fn(x) => System.UNKNOWN)
			    val typetable = foldl (add_to_table System.STATE)     typetable statenames
			    val typetable = foldl (add_to_table System.PARAMETER) typetable paramnames

					  
                            val visi_init = List.mapPartial (fn {name,condition} =>
								case condition 
                                                                 of SOME visi => SOME (visi, ExpTree.LITERAL (ExpTree.CONSTBOOL false))
                                                                  | _ => NONE) outputs
					    
			    val name = exp2str (method "name" (method "class" object))
			in
			    SOME (name, 
				  {params = params,
				   states_init = states_init @ history_init (*@ visi_init*),
				   states_run = states_run @ history_run,
				   ranges = ranges,
				   typetable = typetable,
				   inputs = [],
				   outputs = outputs,
				   samplerate = NONE})
			end
		in
		    tree
		end

	    else if (istype (object, "Vector")) then
		let
		    val subobjs = case object of 
				      KEC.VECTOR {array, ...} => GeneralUtil.array2list (!array)
				    | _ => (*INTERNAL ERROR *) raise TranslationError

		    val subtrees = map model2forest subobjs

		    val aggregate_tree = foldl merge_trees NONE subtrees
		in
		    aggregate_tree
		end
	    else
		raise TypeMismatch ("Expected a Vector or Model instance but received " ^ (pretty object))

    in
	if (not (istype (object, "Model"))) then
	    raise TypeMismatch ("Expected a Model instance but received " ^ (pretty object))
	else
	    model2forest (object)
	    handle TranslationError => NONE
    end
end
