structure Ordering : sig
    (* TODO document these signatures. *)

    val orderModel : DOF.model -> DOF.model

end = struct

exception SortFailed

type eq = {eq_type: DOF.eq_type,
	   sourcepos: PosLog.pos,
	   lhs: Exp.term,
	   rhs: DOF.expression}

(*remove line for debugging *)
fun print x = ()
fun printModel x =  (*DOFPrinter.printModel x*) CurrentModel.setCurrentModel x 

fun valOf (SOME thing) = thing
  | valOf NONE =
    DynException.stdException("Expected element was not found", 
			      "Ordering.valOf", 
			      Logger.INTERNAL)

fun setValOf (SOME thing) = thing
  | setValOf (NONE) = SymbolSet.empty

(* Returns the given value. *)
fun id a = a
(* Compares two values using the builtin polymorphic equality operator. (Allows partial application.) *)
fun equals a b = a = b


fun printClassMap classMap =
    let
	fun printExp expMap name =
	    let
		val deps = SymbolSet.listItems(valOf (SymbolTable.look(expMap, name)))
		val _ = print ("    " ^ (Symbol.name name) ^ " depends on [" ^ (String.concatWith ", " (map Symbol.name deps)) ^ "]\n")
	    in
		()
	    end

	fun printClass name =
	    let
		val _ = print ("  Class: " ^ (Symbol.name name) ^ "\n")
		val expMap = valOf (SymbolTable.look(classMap, name))

		val _ = app (printExp expMap) (SymbolTable.listKeys expMap)
	    in
		()
	    end

	val _ = print "Class Map:\n============\n\n"
	val _ = app printClass (SymbolTable.listKeys classMap)
    in
	()
    end

fun printClassIOMap classIOMap =
    let
	fun printExp expMap name =
	    let
		val deps = SymbolSet.listItems(valOf (SymbolTable.look(expMap, name)))
		val _ = print ("    " ^ (Symbol.name name) ^ " depends on [" ^ (String.concatWith ", " (map Symbol.name deps)) ^ "]\n")
	    in
		()
	    end

	fun printClass name =
	    let
		val _ = print ("  Class: " ^ (Symbol.name name) ^ "\n")
		val expMap = valOf (SymbolTable.look(classIOMap, name))

		val _ = app (printExp expMap) (SymbolTable.listKeys expMap)
	    in
		()
	    end

	val _ = print "Class IO Map:\n============\n\n"
	val _ = app printClass (SymbolTable.listKeys classIOMap)
		
    in
	()
    end

fun inp2sym {name, ...} =
    case ExpProcess.exp2symbols (Exp.TERM (name)) of
	[sym] => sym
      | _ => DynException.stdException("Class has an input that is not a single symbol", 
				       "Ordering.inp2sym", 
				       Logger.INTERNAL)

fun out2sym {name, contents, condition} = 
    case ExpProcess.exp2symbols (Exp.TERM (name)) of
	[sym] => sym
      | _ => DynException.stdException("Class has an output that's name is not a single symbol", 
				       "Ordering.out2sym", 
				       Logger.INTERNAL)


fun term2sym term =
    case ExpProcess.exp2symbols (Exp.TERM (term)) of
	[sym] => sym
      | _ => DynException.stdException("Encountered a term that is supposed to be a symbol and isn't", 
				       "Ordering.term2sym", 
				       Logger.INTERNAL)
	handle e => DynException.checkpoint "Ordering.term2sym" e

fun termexp2sym exp = (case ExpProcess.lhs exp of
			   Exp.TERM t => term2sym t
			 | _ => DynException.stdException(("Unexpected non term on lhs of exp: " ^ (ExpProcess.exp2str exp)), "Ordering.termexp2sym", Logger.INTERNAL))
    handle e => DynException.checkpoint "Ordering.termexp2sym" e

fun classUsage2key mainexpsFlag outputNums =
    let
	val strs = (Bool.toString mainexpsFlag) :: (map Int.toString outputNums)
	val sorted = GeneralUtil.sort_compare (op <) strs
    in
	Symbol.symbol (String.concatWith "#" sorted)
    end

local
    fun isSymbol sym (Exp.SYMBOL (s,_)) = sym = s
      | isSymbol _ _ = DynException.stdException("Encountered invalid LHS symbol", 
						 "Ordering.orderModel.buildInstance", 
						 Logger.INTERNAL)

    fun sym2output orig_outs sym =
	let
	    val oldprops = case List.find (isSymbol sym) orig_outs 
			    of SOME (Exp.SYMBOL (_, p)) => p
			     | _ => DynException.stdException("Couldn't find LHS output", 
							      "Ordering.orderModel.buildInstance", 
							      Logger.INTERNAL)
				    
	in
	    Exp.SYMBOL (sym, oldprops)
	end
	handle e => DynException.checkpoint "Ordering.orderModel.buildInstances.sym2output" e
in
fun buildInstance (class, outputs, inputMap, original_instance_exp) : Exp.exp =
    let
	val {instname=orig_inst_name, classname=orig_class_name,props,inpargs=oldinputs, outargs=orig_outs} = ExpProcess.deconstructInst original_instance_exp

	val instName = Symbol.symbol ((Symbol.name (orig_inst_name)) ^ (Int.toString (Unique.genid())))

	val lhs' = Exp.TUPLE (map (sym2output orig_outs) outputs)

	val inputs = map (fn(i) => List.nth (oldinputs, i)) inputMap

	val rhs' = Exp.FUN (Fun.INST {classname= #name class, 
				      instname=instName,
				      props=
				      Fun.setRealInstName (Fun.setRealClassName Fun.emptyinstprops orig_class_name) orig_inst_name},
			    inputs)

	val exp' = ExpBuild.equals (Exp.TERM lhs', rhs')

    in
	exp'
    end
    handle e => DynException.checkpoint "Ordering.orderModel.buildInstance" e
end


fun sortExps _ _ nil = nil
  | sortExps classname satisfiedDeps exps = 
    let
	(*		(* partition into the two types of sortable equations *)
	 val (intermediate_eqs, instance_eqs) 
	   = List.partition (fn(_, eq) => EqUtil.isIntermediate eq) eqs*)

	(* select satisfied equations *)
	fun depsSatisfied (deps, _) =
	    SymbolSet.isSubset(foldl SymbolSet.union SymbolSet.empty deps, satisfiedDeps)


	val (satisfied_exps, unsatisfied_exps) 
	  = List.partition depsSatisfied exps

	fun getLHSSyms (_, exp):(Symbol.symbol list) =
	    ExpProcess.exp2symbols (ExpProcess.lhs exp)

	val satisfiedDeps' = SymbolSet.union (satisfiedDeps, 
					      SymbolSet.fromList (GeneralUtil.flatten (map getLHSSyms
											   satisfied_exps)))

	fun expanddep2str (deps, exp) = 
	    let
		val expstr:string = 
		    "exp = " ^ 
		    (if ExpProcess.isInstanceEq exp then
			 Symbol.name (#instname (ExpProcess.deconstructInst exp))
		     else
			 (String.concatWith ", " (map Symbol.name (getLHSSyms (deps,exp)))))

		val depstr:string = " deps on " ^ (String.concatWith ", " (map Symbol.name (GeneralUtil.flatten(map SymbolSet.listItems deps))))
	    in
		expstr ^ depstr
	    end
	    handle e => DynException.checkpoint "Ordering.orderModel.sortExps.expanddep2str" e

	val _ = print ("Satisfied deps for " ^ (String.concatWith ", " (map Symbol.name (GeneralUtil.flatten (map getLHSSyms satisfied_exps)))) ^ "\n")
	val _ = print ("  deps are  " ^ (String.concatWith "\n    " (map expanddep2str (satisfied_exps))) ^ "\n")
	val _ = print ("  satisfiedDeps' = " ^ (String.concatWith ", " (map Symbol.name (SymbolSet.listItems satisfiedDeps'))) ^ "\n")
	val _ = print ("UnSatisfied deps for " ^ (String.concatWith ", " (map (fn(d,e) => "(" ^ (String.concatWith ", " (map Symbol.name ((ExpProcess.exp2symbols (ExpProcess.lhs e))))) ^ ")<-[" ^ (String.concatWith ", " (map Symbol.name (GeneralUtil.flatten(map SymbolSet.listItems (d))))) ^ "]") unsatisfied_exps)) ^ "\n\n")
	val _ = print ("  deps are  " ^ (String.concatWith "\n    " (map expanddep2str (unsatisfied_exps))) ^ "\n")

	val satisfied_exps = map #2 satisfied_exps
			     
	val sorted_exps = if SymbolSet.equal (satisfiedDeps, satisfiedDeps') andalso length(unsatisfied_exps) > 0 then
			      (Logger.log_error (Printer.$("Can't sort equations in model " ^ (Symbol.name classname)));				      
			       DynException.setErrored();
			       raise SortFailed)
			  else
			      sortExps classname satisfiedDeps' unsatisfied_exps
    in
	satisfied_exps @ sorted_exps
    end

local 
    fun pairExpWithDeps mapping exp =
	(if ExpProcess.isInstanceEq exp then
	     let
		 val {instname=name, classname, ...} = ExpProcess.deconstructInst exp
		 val instanceDep = setValOf (SymbolTable.look (mapping, name)) 
		 val outputDeps = map (fn(sym) => setValOf (SymbolTable.look (mapping, sym)))
				      (ExpProcess.exp2symbols (ExpProcess.rhs exp))
	     in
		 (instanceDep::outputDeps, exp)			    
	     end
	 else if ExpProcess.isIntermediateEq exp then
	     ([valOf (SymbolTable.look (mapping, termexp2sym exp))], exp) 			
	 else
	     DynException.stdException("Encountered unexpected equation type", 
				       "Ordering.orderModel.orderClass.pairEqsWithDeps", 
				       Logger.INTERNAL))
	handle e => DynException.checkpoint "Ordering.orderModel.orderClass.pairEqsWithDeps" e
in
fun orderClass classMap (class:DOF.class) =
    let
	val exps = (!(#exps class))
	val _ = print ("looking up class with name " ^ (Symbol.name (#name class)) ^ "\n")
	val mapping = valOf (SymbolTable.look (classMap, #name class))

	val init_exps = List.filter ExpProcess.isInitialConditionEq exps
	val state_exps = List.filter ExpProcess.isFirstOrderDifferentialEq exps
	(*val state_eqs = (EqUtil.getDerivativeEqs eqs) @ (EqUtil.getDifferenceEqs eqs)*)
	val other_exps = List.filter (fn(exp)=> (ExpProcess.isInstanceEq exp) orelse
						(ExpProcess.isIntermediateEq exp)) exps

	val sortable_exps = map (pairExpWithDeps mapping) other_exps

	val availSyms = (GeneralUtil.flatten (map (ExpProcess.exp2symbols o ExpProcess.lhs) init_exps))
			@ (map (term2sym o #name) (!(#inputs class)))


	val _ = print ("availsyms = " ^ (String.concatWith ", " (map Symbol.name availSyms)) ^ "\n")

	val satisfiedDeps = SymbolSet.fromList availSyms

	val sorted_exps = sortExps (#name class) satisfiedDeps sortable_exps

	val _ = #exps class := init_exps @ sorted_exps @ state_exps

    in
	()
    end
    handle e => DynException.checkpoint "Ordering.orderModel.orderClass" e
end

fun orderModel (model:DOF.model)=
    let
	(* construct per class eq dep mappings and class IO dep mappings *)
	val classMap = SymbolTable.empty
	val classIOMap = SymbolTable.empty
			 


	val (classes, topInstance, props) = model


	fun addExpToExpMap classes (exp:Exp.exp, (expMap, classMap, classIOMap)) =
	    (if ExpProcess.isInstanceEq exp then
		 let
		     val {instname=name,classname,...} = ExpProcess.deconstructInst exp
		     val _ = print ("in add exp to exp map looking for class " ^ (Symbol.name classname) ^ "\n")
		     val _ = print ("classmap keys are " ^ (String.concatWith ", " (map Symbol.name (SymbolTable.listKeys classMap))) ^ "\n")
		     val _ = print ("classes are " ^ (String.concatWith ", " (map (Symbol.name o #name) classes)) ^ "\n")
		     val class = (valOf (List.find ((equals classname) o #name) classes))
			 handle e => DynException.checkpoint "Ordering.orderModel.addExpToExpMap.class" e
		     val _ = print ("    got it\n")

		     val (classMap', classIOMap') = addClassToClassMap classes (class, (classMap, classIOMap))

		     val ioMap = (valOf (SymbolTable.look (classIOMap', classname)))
			 handle e => DynException.checkpoint "Ordering.orderModel.addExpToExpMap.ioMap" e

		     fun inp2sym {name, ...} =
			 case ExpProcess.exp2symbols (Exp.TERM (name)) of
			     [sym] => sym
			   | _ => DynException.stdException("Class " ^ (Symbol.name classname) ^ " has an input that is not a single symbol", 
							    "Ordering.orderModel.addExpToExpMap.inp2sym", 
							    Logger.INTERNAL)

		     fun out2sym {name, ...} = 
			 case ExpProcess.exp2symbols (Exp.TERM (name)) of
			     [sym] => sym
			   | _ => DynException.stdException("Class " ^ (Symbol.name classname) ^ " has an output that's name is not a single symbol", 
							    "Ordering.orderModel.addExpToExpMap.out2sym", 
							    Logger.INTERNAL)

		     val lhs = ListPair.zip (ExpProcess.exp2symbols (ExpProcess.lhs exp),
					     map out2sym (!(#outputs class)))

		     val inputargs = case (ExpProcess.rhs exp) of
					 Exp.FUN (name, args) => args
				       | e => DynException.stdException("Class " ^ (Symbol.name classname) ^ " has an instance with a malformed rhs", 
									"Ordering.orderModel.addExpToExpMap.out2sym", 
									Logger.INTERNAL)
					      
					      
		     val rhs = ListPair.zip (map ExpProcess.exp2symbols inputargs,
					     map inp2sym (!(#inputs class)))


		     fun classInputDeps2instanceInputDeps (classInputDeps) =
			 let
			     fun classInput2instanceDeps (inputsym) =
				 case List.find ((equals inputsym) o #2) rhs of
				     NONE => DynException.stdException ("Found a symbol in output to input dependencies of " ^ (Symbol.name classname) ^ " which is not an input symbol: " ^ (Symbol.name inputsym),
									"Ordering.orderModel.addExpToExpMap.buildOutputMapping.classInput2instanceDeps",
									Logger.INTERNAL)
				   | SOME (deps, inpsym) => deps
			 in
			     foldl SymbolSet.union SymbolSet.empty (map SymbolSet.fromList (map classInput2instanceDeps classInputDeps))
			 end
			 
		     fun buildOutputMapping ((instanceOutput, classOutput), expMap) =
			 let
			     val classInputDeps = SymbolSet.listItems (valOf (SymbolTable.look (ioMap, classOutput)))

			     val instanceInputs = classInputDeps2instanceInputDeps classInputDeps
						  
			 in
			     SymbolTable.enter (expMap, instanceOutput, instanceInputs)
			 end
			 handle e => DynException.checkpoint "Ordering.orderModel.addExpToExpMap.buildOutputMapping" e

		     val expMap = SymbolTable.enter (expMap, name, classInputDeps2instanceInputDeps(map inp2sym (!(#inputs class))))


		     val expMap' = foldl buildOutputMapping
					 expMap
					 lhs
					 
		 in
		     (expMap', classMap', classIOMap')
		 end
	     else if ExpProcess.isIntermediateEq exp then
		 let		    
		     val lhsSyms = ExpProcess.exp2symbols (ExpProcess.lhs exp)
		     val rhsSyms = ExpProcess.exp2symbols (ExpProcess.rhs exp) 
		     val expMap' = foldl (fn(sym, expMap) => SymbolTable.enter (expMap, sym, SymbolSet.fromList rhsSyms)) 
					 expMap 
					 lhsSyms
		 in
		     (expMap', classMap, classIOMap)
		 end
	     else
		 DynException.stdException(("Non instance or intermediate expression received: " ^ (ExpProcess.exp2str exp)), "Ordering.orderModel.addExpToExpMap", Logger.INTERNAL))
	    handle e => DynException.checkpoint "Ordering.addExpToExpMap" e

	and addClassToClassMap classes (class, (classMap, classIOMap)) =
	    (case SymbolTable.look(classMap, #name class) of
		SOME _ => (classMap, classIOMap)
	      | NONE 
		=>
		let
		    val {name, properties, inputs, outputs, iterators, exps} = class

		     val _ = print ("Adding class to class map: " ^ (Symbol.name name) ^ "\n")
		     val _ = print ("classes are " ^ (String.concatWith ", " (map (Symbol.name o #name) classes)) ^ "\n")
			     
		     fun isRelevantExp exp = ExpProcess.isIntermediateEq exp orelse
					     ExpProcess.isInstanceEq exp

		     val relevantExps = List.filter isRelevantExp (!exps)

		     val (expMap, classMap, classIOMap) = (foldl (addExpToExpMap classes) (SymbolTable.empty, classMap, classIOMap) relevantExps)
		     val expMap = ref expMap
		     val changed = ref true

		     (* evolve expMap deps *)
		     fun evolveExp expMap changed symbol =
			 let
			     val depSet = valOf (SymbolTable.look(!expMap, symbol))

			     fun addSymDeps (symbol, depSet) =
				 SymbolSet.union (depSet,
						  setValOf (SymbolTable.look(!expMap, symbol)))

			     val depSet' = foldl addSymDeps depSet (SymbolSet.listItems (depSet))

			     val _ = if SymbolSet.numItems depSet <> SymbolSet.numItems depSet' then 
					 (changed := true;
					  expMap := SymbolTable.enter (!expMap, symbol, depSet'))
				     else
					 ()

			 in
			     ()
			 end
			 handle e => DynException.checkpoint "Ordering.order_model.addClassToClassMap.evolveExp" e

		     val _ = 
			 while !changed do
			 (changed := false;
			  app (evolveExp expMap changed) (SymbolTable.listKeys (!expMap)))
			 
			 
		     (* Check for circular references*)
		     val classMap' = SymbolTable.enter(classMap, name, !expMap)

		     (* construct classIOMap mappings*)


		     fun addIOMapEntry ({name, contents, condition}, ioMap) =
			 let
			     val namesym = 
				 case ExpProcess.exp2symbols (Exp.TERM (name)) of
				     [sym] => sym
				   | _ => DynException.stdException("Class " ^ (Symbol.name (#name class)) ^ " has an output that's name is not a single symbol", 
								    "Ordering.orderModel.addClassToClassMap.addIOMapEntry", 
								    Logger.INTERNAL)

			     val depSyms = (GeneralUtil.flatten (map ExpProcess.exp2symbols contents))
					   @ (ExpProcess.exp2symbols condition)
			     val depSymsSet = SymbolSet.fromList depSyms

			     fun unionDeps (symbol, set) =
				 case SymbolTable.look(!expMap, symbol) of
				     SOME deps => (*SymbolSet.union (set, SymbolSet.add(deps, symbol))*)SymbolSet.union (set, deps)
				   | NONE => (*SymbolSet.add(set, symbol)*)set

			     val depSet = SymbolSet.foldl unionDeps SymbolSet.empty depSymsSet
			     (* TODO: prune this down to only inputs *)
			     fun symIsInput s =
				 let
				     val inputsyms = map (term2sym o #name) (!(#inputs class))
				 in
				     List.exists (equals s) inputsyms
				 end


			     val depSet' = SymbolSet.filter symIsInput depSet
			     val _ = print ("~~~~~~~~~~~~~~~~~~~~~~~~~~\nadding to class IO map for name=" ^ (Symbol.name (term2sym name)) ^ " deps=" ^ (String.concatWith ", " (map Symbol.name (SymbolSet.listItems depSet))) ^"\n")
			     val _ = print ("  depsyms=" ^ (String.concatWith ", " (map Symbol.name (depSyms))) ^"\n")
			     val _ = print ("  deps'=" ^ (String.concatWith ", " (map Symbol.name (SymbolSet.listItems depSet'))) ^"\n~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
			 in
			     SymbolTable.enter(ioMap, namesym, depSet')
			 end

		     val ioMap = foldl addIOMapEntry SymbolTable.empty (!(#outputs class))

		     val classIOMap' = SymbolTable.enter(classIOMap, name, ioMap)

		 in
		     (classMap', classIOMap')
		 end)
	    handle e => DynException.checkpoint "Ordering.addClassToClassMap" e

	val (classMap, classIOMap) = foldl (addClassToClassMap classes)
					   (classMap, classIOMap)
					   classes


	val _ = printClassMap classMap
	val _ = printClassIOMap classIOMap
	val _ = printModel model


	(* NOT NEEDED, ACTUALLY MUST BE MERGED split instances where IO deps have cycles *)
		
	(* construct splitted (splat?) classes and provide limited output mapping*)



(*		val instName = Symbol.symbol ((Symbol.name (orig_inst_name)) ^ (Int.toString (Unique.genid())))

		fun sym2output sym =
		    let
			val oldprops = case List.find (fn(s) => case s of 
								    Exp.SYMBOL (os, p) => sym = os 
								  | _ => DynException.stdException("Encountered invalid LHS symbol", 
												   "Ordering.orderModel.buildInstance.sym2output", 
												   Logger.INTERNAL))
						      orig_outs of
					   SOME (Exp.SYMBOL (os, p)) => p
					 | _ => DynException.stdException("Couldn't find LHS output", 
									  "Ordering.orderModel.buildInstance.sym2output", 
									  Logger.INTERNAL)
			
		    in
			Exp.SYMBOL (sym, oldprops)
		    end
		    handle e => DynException.checkpoint "Ordering.orderModel.buildInstances.sym2output" e

		val lhs' = Exp.TUPLE (map sym2output outputs)

		val inputs = map (fn(i) => List.nth (oldinputs, i)) inputMap

		val rhs' = Exp.FUN (Fun.INST {classname= #name class, 
					      instname=instName,
					      props=
					      InstProps.setRealInstName (InstProps.setRealClassName InstProps.emptyinstprops orig_class_name) orig_inst_name},
				    inputs)

		val exp' = ExpBuild.equals (Exp.TERM lhs', rhs')*)

	fun outsym2pos outputSyms =
	    let val numberedOuts = ListPair.zip (outputSyms, List.tabulate(length outputSyms, id))
	    in
	     fn newout =>
		case List.find ((equals newout) o #1) numberedOuts
		 of SOME (_, i) => i
		  | NONE => DynException.stdException("Couldn't find an expected output, possible corrupted output list", 
						      "Ordering.orderModel.buildSplit.outsym2pos", 
						      Logger.INTERNAL)
	    end


	fun buildSplit (instanceClass:DOF.class, orig_instance_exp) (partGroup, (splitMap, classMap, classIOMap, exps)) =
	    let
		val _ = print ("calling buildsplit\n")

		val classname = #name instanceClass
		val outputSyms = ExpProcess.exp2symbols (ExpProcess.lhs orig_instance_exp)
		val {instname=instancename,...} = ExpProcess.deconstructInst orig_instance_exp

		fun isMainInstance sym =
		    sym = instancename

		val outputs = List.filter (not o isMainInstance) partGroup
		val mainInstance = List.find isMainInstance partGroup

		val candidateClasses = (valOf(SymbolTable.look (splitMap, classname)))
		    handle e => DynException.checkpoint "Ordering.orderModel.buildSplit.candidateClasses" e

		val outputMap = map (outsym2pos outputSyms) outputs

		val key = classUsage2key (Option.isSome mainInstance) outputMap 

		val _ = print ("Looking at candidates: " ^ (String.concatWith ", " (map Symbol.name (SymbolTable.listKeys candidateClasses))) ^ " for key: " ^ (Symbol.name key) ^ "\n")

		val (class, instance_exp, (splitMap', classMap', classIOMap')) = 
  		    case SymbolTable.look(candidateClasses, key) of
			SOME (class, inputMap) =>
			let
			    val instance_exp = buildInstance (class, outputs, inputMap, orig_instance_exp)
			in
			    (class, instance_exp, (splitMap, classMap, classIOMap))
			end
		      | NONE =>
			let

			    val (newclass, inputMap, splitMap) = 
				buildClass (classMap, splitMap, instanceClass, outputMap, Option.isSome mainInstance)
				
			    val instance_exp = buildInstance (newclass, outputs, inputMap, orig_instance_exp)
					       

			    val classes = map #1 (GeneralUtil.flatten (map SymbolTable.listItems (SymbolTable.listItems (splitMap))))

			    val (classMap', classIOMap') = addClassToClassMap classes (newclass, (classMap, classIOMap))
			    val _ = print ("class map keys are " ^ (String.concatWith ", " (map Symbol.name (SymbolTable.listKeys classMap'))) ^ "\n")
			in
			    (newclass, instance_exp, (SymbolTable.enter (splitMap, classname, SymbolTable.enter(candidateClasses, key, (newclass, inputMap))),
						      classMap',
						      classIOMap'))
			end


	    in
		(splitMap', classMap', classIOMap', instance_exp :: exps)
	    end
	    handle e => DynException.checkpoint "Ordering.buildSplit" e

	and buildClass (classMap, splitMap, oldClass:DOF.class, outputMap, includeMainExps) =
	    let
		val newname = Symbol.symbol ((Symbol.name (#name oldClass)) ^ (Int.toString (Unique.genid())))

		val expMap = (valOf(SymbolTable.look(classMap, #name oldClass)))
		    handle e => DynException.checkpoint "Ordering.orderModel.buildClass.expMap" e


		(* compute new outputs *)
		val oldoutputs = !(#outputs oldClass)
		val outputs = map (fn(i) => List.nth(oldoutputs, i)) outputMap


		(* compute required inputs *)
		val oldinputs = (!(#inputs oldClass))
		val oldinputsyms = map inp2sym (!(#inputs oldClass))

		val oldinputset = SymbolSet.fromList oldinputsyms

		val _ = print ("class name = " ^ (Symbol.name (#name oldClass)) ^ "\n")

		val _ = print ("includeMainExps = " ^ (Bool.toString includeMainExps) ^ "\n")

		val _ = print ("outputMap = " ^ (String.concatWith ", " (map Int.toString outputMap)) ^ "\n")


		(* Use this on syms in RHS of eq that doesn't have dependency info (ie, it's not an instance or an interm) *)
		fun depsOfUsedSym (sym:Symbol.symbol): SymbolSet.set =
		    case (SymbolTable.look(expMap, sym)) of
			SOME set => SymbolSet.add(set, sym)
		      | NONE => (*SymbolSet.empty*) SymbolSet.singleton(sym)

		fun output2deps output =
		    let
			val usedSyms =
			    SymbolSet.union(foldl SymbolSet.union SymbolSet.empty (map (SymbolSet.fromList o ExpProcess.exp2symbols) (#contents output)),
					    SymbolSet.fromList (ExpProcess.exp2symbols (#condition output)))
		    in
			foldl SymbolSet.union SymbolSet.empty (map depsOfUsedSym (SymbolSet.listItems usedSyms))
		    end

		fun computeInputs (output, set) =
		    let
			val symsInOutput = output2deps output

			val neededInps = SymbolSet.intersection (foldl SymbolSet.union
								       SymbolSet.empty 
								       (map depsOfUsedSym (SymbolSet.listItems symsInOutput)), 
								 
								 oldinputset)
		    in
			SymbolSet.union (set, neededInps)
		    end

		(* compute equations needed *)
		(*   to consider: intermediate eqs needed for outputs and intermediate eqs needed for mainEqs*)
		(*   to consider: instances and how to split them*)

		(* main equations *)
		val mainExps = List.filter (fn(exp)=> (ExpProcess.isFirstOrderDifferentialEq exp) orelse
						      (ExpProcess.isDifferenceEq exp))
					   (!(#exps oldClass))
		(*val _ = app (fn(exp)=>Util.log ("mainExps: " ^ (ExpProcess.exp2str exp))) mainExps*)

		val instances = List.filter ExpProcess.isInstanceEq (!(#exps oldClass))


		fun depsOfExp exp =
		    (if ExpProcess.isInstanceEq exp then
			 let
			     val {instname=name,...} = ExpProcess.deconstructInst exp
			 in
			     (case SymbolTable.look(expMap, name) of
				  SOME set => set before print ("\n\n\n##########################-##################\n" ^ (Symbol.name name) ^ " " ^ (String.concatWith ", " (map Symbol.name (SymbolSet.listItems set))) ^ "\n")
				| NONE => 
				  let
				      val ret = 
					  (foldl SymbolSet.union 
						 SymbolSet.empty 
						 (map (fn(sym) => SymbolSet.add(depsOfUsedSym sym, sym)) (ExpProcess.exp2symbols (ExpProcess.rhs exp))))
				  in
				      ret before print ("\n\n@@@@@@@@@@@@@@@@@@@@@@@@-@@@@@@@@@@@@@@@@@@@@@@\n" ^ (Symbol.name name) ^ " " ^ (String.concatWith ", " (map Symbol.name (SymbolSet.listItems ret))) ^ "\n")
				  end)
			 end
		     else
			 let
			     val sym = termexp2sym exp
			 in
			     case SymbolTable.look(expMap, sym) of
				 SOME set => set
			       | NONE => (foldl SymbolSet.union 
						SymbolSet.empty 
						(map (fn(sym) => SymbolSet.add(depsOfUsedSym sym, sym)) (ExpProcess.exp2symbols (ExpProcess.rhs exp))))
			 end)
		    handle e => DynException.checkpoint "Ordering.buildClass.depsOfExp" e

				
		fun instance2deps inst = 
		    let
			val {classname = name,...} = ExpProcess.deconstructInst inst
		    in
			if includeMainExps then
			    name :: (ExpProcess.exp2symbols (ExpProcess.rhs inst))
			else
			    name :: nil
		    end

		val instanceDeps =  SymbolSet.fromList (foldl (op @) nil (map instance2deps instances)) (*[SymbolSet.fromList(map nameOfInstance instances)]*)

		val mainExpDeps = foldl SymbolSet.union 
					(SymbolSet.fromList (map termexp2sym mainExps))
					(instanceDeps :: (map depsOfExp mainExps))
		    handle e => DynException.checkpoint "Ordering.buildClass.mainExpDeps" e

		val outputDeps = foldl SymbolSet.union SymbolSet.empty (map output2deps outputs)

		val _ = print ("______________________________________\n")
		val _ = print ("name = " ^ (Symbol.name newname) ^"\n")
		val _ = print ("   mainExps = " ^ (String.concatWith ", " (map Symbol.name (map termexp2sym mainExps))) ^ "\n")
		val _ = print ("   mainExpDeps = " ^ (String.concatWith ", " (map Symbol.name (SymbolSet.listItems mainExpDeps))) ^ "\n")
		val _ = print ("   outputDeps = " ^ (String.concatWith ", " (map Symbol.name (SymbolSet.listItems outputDeps))) ^ "\n")

		val deps = SymbolSet.union(outputDeps,  
					   if includeMainExps then
					       mainExpDeps
					   else
					       SymbolSet.empty)


		(* compute equations to include based upon dependencies *)
		fun depInExp dep exp =
		    (if ExpProcess.isInstanceEq exp then
			let 
			    val {instname=name, classname, ...} = ExpProcess.deconstructInst exp
			in
			    dep = name orelse List.exists (fn(s) => s = dep) 
							  (ExpProcess.exp2symbols (ExpProcess.lhs exp))
			end
		    (*else if ExpProcess.isInitialConditionEq exp then
			false*)
		    else
			dep = (termexp2sym exp))
		    handle e => DynException.checkpoint "Ordering.buildClass.depInExp" e

		fun dep2exp (dep, (maps as (splitMap, classMap, classIOMap, generatedInstances), exps)) =
		    (case List.find (depInExp dep) (!(#exps oldClass)) of
			 SOME exp =>
			 if ExpProcess.isFirstOrderDifferentialEq exp then
			     (maps, if includeMainExps then exp :: exps else exps)
			 else if ExpProcess.isInstanceEq exp then
			     let
				 val {instname=name,classname,...} = ExpProcess.deconstructInst exp
			     in
				 if SymbolSet.member(generatedInstances, name) then
				     (maps, exps)
				 else
				     let	
					 (* we have to do all of the following redundant work (redundant with buildsplit) to find instanceClass.  Refactor this later *)
					 val outputSyms = ExpProcess.exp2symbols (ExpProcess.lhs exp)
					 val outputs = List.filter (fn(out) => SymbolSet.member(deps, out)) outputSyms
					 val mainInstance = if includeMainExps then
								SOME name
							    else
								NONE
					 (*val partGroup = (case mainInstance of SOME x => [x] | NONE => []) @ outputs*)
					 val partGroups = case mainInstance of
							      SOME x => outputs :: [x] :: nil
							    | NONE => [outputs]
								      
					 (*TODO: do we need to split the subclass before we get here?*)
					 (*val _ = print ("for instance "^(Symbol.name name)^" partgroup = " ^ (String.concatWith ", " (map Symbol.name partGroup)) ^ "\n")*)

					 fun outsym2pos newout =
					     let
						 val numberedOuts = ListPair.zip (outputSyms, 
										  List.tabulate(length(outputSyms), 
											     fn(i) => i))
					     in
						 case List.find ((equals newout) o #1) numberedOuts of
						     SOME (_, i) => i
						   | NONE => DynException.stdException("Couldn't find an expected output, possible corrupted output list", 
										       "Ordering.orderModel.buildClass.dep2exp.outsym2pos", 
										       Logger.INTERNAL)
					     end				

					 val outputMap = map outsym2pos outputs
					 val completeOutputMap = map outsym2pos outputSyms
								 
					 (*				 val key = classUsage2key (Option.isSome mainInstance) outputMap *)
					 val key = classUsage2key true completeOutputMap
						   
					 val _ = print ("looking for classname " ^ (Symbol.name classname) ^ " with key " ^ (Symbol.name key) ^ "\n")
					 val candidateClasses = (valOf(SymbolTable.look (splitMap, classname)))
					     handle e => DynException.checkpoint "Ordering.orderModel.buildClass.dep2exp.candidateClasses" e
							 
					 val (instanceClass, inputMap) =  valOf(SymbolTable.look(candidateClasses, key))
									  
					 val (splitMap', classMap', classIOMap', exps) =
					     foldl (buildSplit (instanceClass, exp))
						   (splitMap, classMap, classIOMap, exps)
						   partGroups

					 val generatedInstances' = SymbolSet.add(generatedInstances, name)
				     in
					 ((splitMap', classMap', classIOMap', generatedInstances'), exps)
				     end	
				     handle e => DynException.checkpoint "Ordering.orderModel.buildClass.dep2exp [not member]" e	
						 
			     end
			 else if ExpProcess.isIntermediateEq exp then
			     (maps, exp :: exps)
			 else
			     (maps, exps)
		       | NONE => (maps, exps))
		    handle e => DynException.checkpoint "Ordering.orderModel.buildClass.dep2exp" e
				
		val ((splitMap, classMap, classIOMap, _), exps) = foldl dep2exp ((splitMap, classMap, classIOMap, SymbolSet.empty), nil) (SymbolSet.listItems deps)

		(* In order to distinguish between true initial values and placeholders to determine
											state offset locations, in the latter case we map initial values from absolute iterator
																      offsets to relative.  The latter would never naturally occur, so it is distinguishable.
																								       This code should be replaced eventually when we have a better mechanism to refer to state space.
		 *)
		fun buildInitialValues (exp) =
		    if includeMainExps then
			exp
		    else
			let
			    fun abs2rel (sym, Iterator.ABSOLUTE 0) =
				(sym, Iterator.RELATIVE 0)
			      | abs2rel (sym, iterator) =
				(sym, iterator)

			    val lhs' = (*case ExpProcess.exp2term (ExpProcess.lhs exp) of
					     Exp.SYMBOL (sym, prop) => 
					     Exp.TERM (Exp.SYMBOL (sym, 
								   (case Property.getIterator (prop) of
									SOME (iterators) => 
									Property.setIterator prop (map abs2rel iterators)
								      | NONE => prop)))
					   | lhs =>*) (ExpProcess.lhs exp)
						      
						      
			in
			    ExpBuild.equals (lhs', ExpProcess.rhs exp)
			end
			

		val _ = print ("   exps' = " ^ (String.concatWith ", " (map Symbol.name (GeneralUtil.flatten ((map (ExpProcess.exp2symbols o ExpProcess.lhs) exps))))) ^ "\n")


		(* add in initial value eqs ALWAYS *)
		(* we shouldn't need this anymore, but if you remove it, ordering fails ... *)
		(*val exps = exps @ (map buildInitialValues (List.filter ExpProcess.isInitialConditionEq (!(#exps oldClass))))*)

		val expDeps = foldl SymbolSet.union SymbolSet.empty (map depsOfExp exps)
		val expInputDeps = 
		    SymbolSet.intersection(expDeps,
					   oldinputset)

		val _ = print ("   oldinputs = " ^ (String.concatWith ", " (map Symbol.name (SymbolSet.listItems oldinputset))) ^ "\n")
		val _ = print ("   expDeps = " ^ (String.concatWith ", " (map Symbol.name (SymbolSet.listItems expDeps))) ^ "\n")
		val _ = print ("   expInputDeps = " ^ (String.concatWith ", " (map Symbol.name (SymbolSet.listItems expInputDeps))) ^ "\n")



		(* compute needed inputs using outputs and maineqs if applicable *)
		val neededoldinputs = SymbolSet.listItems (foldl computeInputs expInputDeps outputs)

				      
		val _ = print ("  neededoldinputs = " ^ (String.concatWith ", " (map Symbol.name neededoldinputs)) ^ "\n")

		(* number the inputs symbols so we can create an input mapping and a list of inputs *)
		fun inp2pos inp =
		    let
			val numberedInps = ListPair.zip (oldinputsyms, 
							 List.tabulate(length(oldinputsyms), 
								    fn(i) => i))
		    in
			case List.find ((equals inp) o #1) numberedInps of
			    SOME (_, i) => i
			  | NONE => DynException.stdException("Couldn't find an expected input, possible corrupted input list", 
							      "Ordering.orderModel.buildClass.inp2pos", 
							      Logger.INTERNAL)
		    end				

		val inputMap = map inp2pos neededoldinputs
		val inputs = map (fn(i) => List.nth(oldinputs, i)) inputMap



		val newclass = {name=newname,
				properties= if includeMainExps then
						#properties oldClass
					    else (* convert it to a slave of the orignal *)
						ClassProcess.makeSlaveClassProperties (#properties oldClass),
				iterators= #iterators oldClass,
				inputs= ref inputs,
				outputs=ref outputs,
				exps=ref exps}

		val splitMap' = splitMap 
	    in
		(newclass, inputMap, splitMap')
	    end
	    handle e => (DOFPrinter.printClass oldClass;
			 DynException.checkpoint "Ordering.buildClass" e)

	(* split an instance if there are cycles *)
	and processInstance containerClass (instance_exp: Exp.exp, (exps, (splitMap, classMap, classIOMap))) =
	    let
		val {classname,instname=instancename,...} = ExpProcess.deconstructInst instance_exp
		    
		val _ = print ("classname = " ^ (Symbol.name classname) ^ "\n") 
		val _ = print ("  classnames = " ^ (String.concatWith ", " (map Symbol.name (map #name classes))) ^ "\n")
		val instanceClass = valOf (List.find ((equals classname) o #name) classes)

		val _ = print ("got here\n")

		val expMap = valOf(SymbolTable.look(classMap, #name containerClass))
		val _ = print ("got here again and instancename = " ^ (Symbol.name instancename) ^ "\n")
		val _ = print ("  keys at this level are = " ^ (String.concatWith ", " (map Symbol.name (SymbolTable.listKeys expMap))) ^ "\n")
		val instanceDeps = valOf(SymbolTable.look(expMap,
							  instancename))


		val _ = print ("got here too\n")
			
		val outputSyms = ExpProcess.exp2symbols (ExpProcess.lhs instance_exp)

		val _ = print ("=!!!!!!!!!!!!!!!!!!!!!!!!!!!= instancedeps deps:" ^ (String.concatWith ", " (map Symbol.name (SymbolSet.listItems instanceDeps))) ^ "\n")
		val _ = print ("=!!!!!!!!!!!!!!!!!!!!!!!!!!!= outputSyms:" ^ (String.concatWith ", " (map Symbol.name outputSyms)) ^ "\n")

		(* if the deps of the instance in question depend upon any outputs that are from an instance that depends upon an output *)
		(*   ie we are in instance a and a depends upon x.o and x depends upon a.something *)
		(*   so we take our instance deps (including x.o) and find instanceDepInstances (ie x)*)
		(*   then we compute the deps of the instanceDepInstances (which should include any of our outputs if there is a cycle)*)
		(* TODO: does this scale if you have multiple levels backwards to go? *)		    
		fun depoutput2instance sym =
		    let
			fun isInstanceAndContainsOutput exp =
			    if ExpProcess.isInstanceEq exp then
				List.exists (equals sym) (ExpProcess.exp2symbols (ExpProcess.lhs exp))
			    else
				false
		    in
			case List.find isInstanceAndContainsOutput (!(#exps containerClass)) of
			    NONE => NONE
			  | SOME exp => 
			    if ExpProcess.isInstanceEq exp then
				SOME (#instname (ExpProcess.deconstructInst exp))
			    else			
				DynException.stdException ("non-instance eq encountered",
							   "Ordering.orderModel.processinstance.depoutput2instance",
							   Logger.INTERNAL)
		    end
		    handle e => DynException.checkpoint "Ordering.orderModel.processinstance.depoutput2instance" e

		val instanceDepInstances = List.mapPartial depoutput2instance (SymbolSet.listItems instanceDeps)

		val depsOfInstanceDepInstances = foldl SymbolSet.union SymbolSet.empty (map (fn(i) => valOf (SymbolTable.look(expMap, i))) instanceDepInstances)

		val _ = print ("instanceDepInstances = " ^ (String.concatWith ", " (map Symbol.name instanceDepInstances)) ^ "\n")
		val _ = print ("depsOfInstanceDepInstances = " ^ (String.concatWith ", " (map Symbol.name (SymbolSet.listItems depsOfInstanceDepInstances))) ^ "\n")
		val mutuallyRecursiveInstances = List.exists (fn(os) => SymbolSet.member (depsOfInstanceDepInstances, os)) outputSyms
	    in
		(* if the instance depends on any of its outputs, or if the deps of the instance depend upon any outputs that are from an instance that depends upon an output, then we know that a cycle exists *)

		if List.exists (fn(os) => SymbolSet.member (instanceDeps, os)) outputSyms orelse mutuallyRecursiveInstances then
		    let
			fun orderParts syms =
			    map (fn(s) => [s]) syms
			    
			val orderedParts = 
			    if mutuallyRecursiveInstances then
				(orderParts outputSyms) @ [[instancename]]
			    else
				orderParts (instancename :: outputSyms)



			val _ = print ("  Splitting occurred on " ^ (Symbol.name instancename) ^ "\n")
			val _ = app (fn(order) => print ("  Group: " ^ (String.concatWith ", " (map Symbol.name order)) ^ "\n")) orderedParts


			val (splitMap, classMap, classIOMap, instance_exps') =
			    foldl (buildSplit (instanceClass, instance_exp))
				  (splitMap, classMap, classIOMap, nil)
				  orderedParts
			val _ = print ("done with foldl of  buildsplit\n")

		    in
			(instance_exps' @ exps, (splitMap, classMap, classIOMap))
		    end
		else
		    (instance_exp::exps, (splitMap, classMap, classIOMap))
	    end
	    handle e => DynException.checkpoint "Ordering.processInstance" e

	fun splitClasses (class: DOF.class, (splitMap, classMap, classIOMap)) =
	    let
		val _ = print ("===splitting class: " ^ (Symbol.name (#name class)) ^ "\n")
		val (instance_exps, other_exps) = List.partition ExpProcess.isInstanceEq (!(#exps class))

		val (instance_exps', (splitMap', classMap', classIOMap')) 
		  = foldl (processInstance class) (nil, (splitMap, classMap, classIOMap)) instance_exps

		val _ = #exps class := instance_exps' @ other_exps

		val _ = print ("class map keys are " ^ (String.concatWith ", " (map Symbol.name (SymbolTable.listKeys classMap'))) ^ "\n")
		val _ = print ("===about to reprocess splitting class: " ^ (Symbol.name (#name class)) ^ "\n")

		(* reprocess to catch renamed instances *)
		val classes = map #1 (GeneralUtil.flatten (map SymbolTable.listItems (SymbolTable.listItems (splitMap'))))
		val _ = print ("split classes classes are " ^ (String.concatWith ", " (map (Symbol.name o #name) classes)) ^ "\n")

		val (classMap'', classIOMap'') = addClassToClassMap classes (class, (#1(SymbolTable.remove(classMap', #name class)), (#1(SymbolTable.remove(classIOMap', #name class)))))
		val _ = print ("===done splitting class: " ^ (Symbol.name (#name class)) ^ "\n")
	    in
		(splitMap', classMap'', classIOMap'')
	    end
	    handle e => DynException.checkpoint "Ordering.splitClasses" e

	val splitMap = foldl (fn(c,t) => SymbolTable.enter(t,
							   #name c, 
							   SymbolTable.enter(SymbolTable.empty, 
									     classUsage2key true (List.tabulate (length (!(#outputs c)), fn(i) => i)),
									     (c, List.tabulate (length (!(#inputs c)), fn(i) =>i)))))
			     SymbolTable.empty 
			     classes

	val (splitMap, classMap, classIOMap) = foldl splitClasses (splitMap, classMap, classIOMap) classes

	val _ = print ("done splitting classes\n")

	val classes' = map #1 (GeneralUtil.flatten (map SymbolTable.listItems (SymbolTable.listItems (splitMap))))



	(* loop through classes and order eqs by deps, construct instances *)



	val classes'' = classes' (* removed pruning for now *)
			
	val _ = print ("splitting performed\n==========\n\n")	    
	val _ = printClassMap classMap
	val _ = printClassIOMap classIOMap
	val _ = printModel (classes', topInstance, props)

	val _ = print ("pruning performed\n=====================\n\n")
	val _ = printModel (classes'', topInstance, props)

	val _ = app (orderClass classMap) classes''
		
	val _ = if DynException.isErrored() then
		    (print ("Errors found when ordering ...\n");
		     print ("Data structure after ordering attempt\n==================\n"))
		else
		    print("ordering performed\n==============\n\n")

	val _ = printModel (classes'', topInstance, props)


	val model' = (classes'', topInstance, props)
    in
	model' 
	handle SortFailed => model before (DynException.checkToProceed())
    end
    handle e => model before 
		(app (fn(s) => print(s ^ "\n")) (MLton.Exn.history e);
		 DynException.checkpoint "Ordering.orderModel" e)


		
end
