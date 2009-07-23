structure Ordering =
struct

(*remove line for debugging *)
fun print x = ()
fun printModel x = (*DOFPrinter.printModel x *) CurrentModel.setCurrentModel x

fun printClassMap classMap =
    let
	fun printEq eqMap name =
	    let
		val deps = SymbolSet.listItems(valOf (SymbolTable.look(eqMap, name)))
		val _ = print ("    " ^ (Symbol.name name) ^ " depends on [" ^ (String.concatWith ", " (map Symbol.name deps)) ^ "]\n")
	    in
		()
	    end

	fun printClass name =
	    let
		val _ = print ("  Class: " ^ (Symbol.name name) ^ "\n")
		val eqMap = valOf (SymbolTable.look(classMap, name))

		val _ = app (printEq eqMap) (SymbolTable.listKeys eqMap)
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
	fun printEq eqMap name =
	    let
		val deps = SymbolSet.listItems(valOf (SymbolTable.look(eqMap, name)))
		val _ = print ("    " ^ (Symbol.name name) ^ " depends on [" ^ (String.concatWith ", " (map Symbol.name deps)) ^ "]\n")
	    in
		()
	    end

	fun printClass name =
	    let
		val _ = print ("  Class: " ^ (Symbol.name name) ^ "\n")
		val eqMap = valOf (SymbolTable.look(classIOMap, name))

		val _ = app (printEq eqMap) (SymbolTable.listKeys eqMap)
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



fun orderModel (model:DOF.model)=
    let
	val (classes, topInstance, props) = model

	exception SortFailed

	(* construct per class eq dep mappings and class IO dep mappings *)
	val classMap = SymbolTable.empty
	val classIOMap = SymbolTable.empty
			 
	fun valOf (SOME thing) = thing
	  | valOf NONE =
	    DynException.stdException("Expected element was not found", 
				      "Ordering.valOf", 
				      Logger.INTERNAL)

	fun setValOf (SOME thing) = thing
	  | setValOf (NONE) = SymbolSet.empty

	fun term2sym term =
 	    case ExpProcess.exp2symbols (Exp.TERM (term)) of
		[sym] => sym
	      | _ => DynException.stdException("Encountered a term that is supposed to be a symbol and isn't", 
					       "Ordering.orderModel.term2sym", 
					       Logger.INTERNAL)

	type eq = {eq_type: DOF.eq_type,
		   sourcepos: PosLog.pos,
		   lhs: Exp.term,
		   rhs: DOF.expression}

	fun addEqToEqMap classes (eq:eq, (eqMap, classMap, classIOMap)) =
	    case #eq_type eq of
		DOF.INTERMEDIATE_EQ 
		=>
		let		    
		    val lhsSyms = ExpProcess.exp2symbols (Exp.TERM (#lhs eq)) 
		    val rhsSyms = ExpProcess.exp2symbols (#rhs eq) 
		    val eqMap' = foldl (fn(sym, eqMap) => SymbolTable.enter (eqMap, sym, SymbolSet.fromList rhsSyms)) 
				       eqMap 
				       lhsSyms
		in
		    (eqMap', classMap, classIOMap)
		end
	      | DOF.INSTANCE {name, classname, offset} =>
		let
		    val _ = print ("in add eq to eq map looking for class " ^ (Symbol.name classname) ^ "\n")
		    val _ = print ("classmap keys are " ^ (String.concatWith ", " (map Symbol.name (SymbolTable.listKeys classMap))) ^ "\n")
		    val _ = print ("classes are " ^ (String.concatWith ", " (map (fn(c) => Symbol.name (#name c)) classes)) ^ "\n")
		    val class = valOf( List.find (fn(c) => #name c = classname) classes)
		    val _ = print ("    got it\n")

		    val (classMap', classIOMap') = addClassToClassMap classes (class, (classMap, classIOMap))

		    val ioMap = valOf (SymbolTable.look (classIOMap', classname)) 

		    fun inp2sym {name, ...} =
			case ExpProcess.exp2symbols (Exp.TERM (name)) of
			    [sym] => sym
			  | _ => DynException.stdException("Class " ^ (Symbol.name classname) ^ " has an input that is not a single symbol", 
							   "Ordering.orderModel.addEqToEqMap.inp2sym", 
							   Logger.INTERNAL)

		    fun out2sym {name, ...} = 
			case ExpProcess.exp2symbols (Exp.TERM (name)) of
			    [sym] => sym
			  | _ => DynException.stdException("Class " ^ (Symbol.name classname) ^ " has an output that's name is not a single symbol", 
							   "Ordering.orderModel.addEqToEqMap.out2sym", 
							   Logger.INTERNAL)

		    val lhs = ListPair.zip (ExpProcess.exp2symbols (Exp.TERM (#lhs eq)),
					    map out2sym (!(#outputs class)))

		    val inputargs = case (#rhs eq) of
					Exp.FUN (name, args) => args
				      | e => DynException.stdException("Class " ^ (Symbol.name classname) ^ " has an instance with a malformed rhs", 
								       "Ordering.orderModel.addEqToEqMap.out2sym", 
								       Logger.INTERNAL)
					     
					     
		    val rhs = ListPair.zip (map ExpProcess.exp2symbols inputargs,
					    map inp2sym (!(#inputs class)))


		    fun classInputDeps2instanceInputDeps (classInputDeps) =
			let
			    fun classInput2instanceDeps (inputsym) =
				case List.find (fn(_, inpsym) => inpsym = inputsym) rhs of
				    NONE => DynException.stdException ("Found a symbol in output to input dependencies of " ^ (Symbol.name classname) ^ " which is not an input symbol: " ^ (Symbol.name inputsym),
								       "Ordering.orderModel.addEqToEqMap.buildOutputMapping.classInput2instanceDeps",
								       Logger.INTERNAL)
				  | SOME (deps, inpsym) => deps
			in
			    foldl SymbolSet.union SymbolSet.empty (map SymbolSet.fromList (map classInput2instanceDeps classInputDeps))
			end
							   
		    fun buildOutputMapping ((instanceOutput, classOutput), eqMap) =
			let
			    val classInputDeps = SymbolSet.listItems (valOf (SymbolTable.look (ioMap, classOutput)))

			    val instanceInputs = classInputDeps2instanceInputDeps classInputDeps
				
			in
			    SymbolTable.enter (eqMap, instanceOutput, instanceInputs)
			end

		    val eqMap = SymbolTable.enter (eqMap, name, classInputDeps2instanceInputDeps(map inp2sym (!(#inputs class))))


		    val eqMap' = foldl buildOutputMapping
				       eqMap
				       lhs
				       
		in
		    (eqMap', classMap', classIOMap')
		end
	      | _ => DynException.stdException("Filtering of equations failed", "Ordering.orderModel.addEqToEqMap", Logger.INTERNAL)

	and addClassToClassMap classes (class, (classMap, classIOMap)) =
	    case SymbolTable.look(classMap, #name class) of
		SOME _ => (classMap, classIOMap)
	      | NONE 
		=>
		let
		    val {name, properties, inputs, outputs, eqs} = class

		    val _ = print ("Adding class to class map: " ^ (Symbol.name name) ^ "\n")
		    val _ = print ("classes are " ^ (String.concatWith ", " (map (fn(c) => Symbol.name (#name c)) classes)) ^ "\n")
								   
		    fun isRelevantEq {eq_type=DOF.INTERMEDIATE_EQ, ...} = true
		      | isRelevantEq {eq_type=DOF.INSTANCE _, ...} = true
		      | isRelevantEq _ = false

		    val relevantEqs = List.filter isRelevantEq (!eqs)

		    val (eqMap, classMap, classIOMap) = (foldl (addEqToEqMap classes) (SymbolTable.empty, classMap, classIOMap) relevantEqs)
		    val eqMap = ref eqMap
		    val changed = ref true

		    (* evolve eqMap deps *)
		    fun evolveEq eqMap changed symbol =
			let
			    val depSet = valOf (SymbolTable.look(!eqMap, symbol))

			    fun addSymDeps (symbol, depSet) =
				SymbolSet.union (depSet,
						 setValOf (SymbolTable.look(!eqMap, symbol)))

			    val depSet' = foldl addSymDeps depSet (SymbolSet.listItems (depSet))

			    val _ = if SymbolSet.numItems depSet <> SymbolSet.numItems depSet' then 
					(changed := true;
					 eqMap := SymbolTable.enter (!eqMap, symbol, depSet'))
				    else
					()

			in
			    ()
			end

		    val _ = 
			while !changed do
			(changed := false;
			 app (evolveEq eqMap changed) (SymbolTable.listKeys (!eqMap)))
			
			
		    (* Check for circular references*)
		    val classMap' = SymbolTable.enter(classMap, name, !eqMap)

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
				case SymbolTable.look(!eqMap, symbol) of
				    SOME deps => (*SymbolSet.union (set, SymbolSet.add(deps, symbol))*)SymbolSet.union (set, deps)
				  | NONE => (*SymbolSet.add(set, symbol)*)set

			    val depSet = SymbolSet.foldl unionDeps SymbolSet.empty depSymsSet
			(* TODO: prune this down to only inputs *)
			    fun symIsInput s =
				let
				    val inputsyms = map (fn(i) => term2sym (#name i)) (!(#inputs class))
				in
				    List.exists (fn(is) => is = s) inputsyms
				end


			    val depSet' = SymbolSet.filter (fn(s) => symIsInput s) depSet
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
		end

	val (classMap, classIOMap) = foldl (addClassToClassMap classes)
					   (classMap, classIOMap)
					   classes


	val _ = printClassMap classMap
	val _ = printClassIOMap classIOMap
	val _ = printModel model


    (* NOT NEEDED, ACTUALLY MUST BE MERGED split instances where IO deps have cycles *)
			     
    (* construct splitted (splat?) classes and provide limited output mapping*)

	fun classUsage2key maineqsFlag outputNums =
	    let
		val strs = (Bool.toString maineqsFlag) :: (map Int.toString outputNums)
		val sorted = GeneralUtil.sort_compare (op <) strs
	    in
		Symbol.symbol (String.concatWith "#" sorted)
	    end

	fun buildInstance (class, outputs, inputMap, original_instance_eq) : eq =
	    let
		val orig_inst_name = case #eq_type original_instance_eq of
				 DOF.INSTANCE {name, ...} => name
			       | _ => DynException.stdException("Malformed equation eq_type", 
								"Ordering.orderModel.buildInstance", 
								Logger.INTERNAL)
				      
		val instName = Symbol.symbol ((Symbol.name (orig_inst_name)) ^ (Int.toString (Unique.genid())))

		fun sym2output sym =
		    let
			val orig_outs = case (#lhs original_instance_eq) of
					    Exp.TUPLE terms => terms
					  | _ => DynException.stdException("Encountered invalid LHS", 
									   "Ordering.orderModel.buildInstance.sym2output", 
									   Logger.INTERNAL)
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

		val lhs' = Exp.TUPLE (map sym2output outputs)

		val oldinputs = case #rhs original_instance_eq of
				    Exp.FUN (_, outs) => outs
				  | _ => DynException.stdException("Malformed equation rhs", 
								   "Ordering.orderModel.buildInstance", 
								   Logger.INTERNAL)

		val inputs = map (fn(i) => List.nth (oldinputs, i)) inputMap

		val rhs' = Exp.FUN (#name class,
				    inputs)

		val offset = case #eq_type original_instance_eq of
				 DOF.INSTANCE {offset, ...} => offset
			       | _ => DynException.stdException("Malformed equation eq_type", 
								"Ordering.orderModel.buildInstance", 
								Logger.INTERNAL)

	    in
		{eq_type=DOF.INSTANCE {name=instName,
				       classname= #name class,
				       offset = offset},
		 sourcepos = #sourcepos original_instance_eq,
		 lhs=lhs',
		 rhs=rhs'}
	    end


	fun buildSplit (instanceClass:DOF.class, orig_instance_eq) (partGroup, (splitMap, classMap, classIOMap, eqs)) =
	    let
		val _ = print ("calling buildsplit\n")

		val classname = #name instanceClass
		val outputSyms = ExpProcess.exp2symbols (Exp.TERM(#lhs orig_instance_eq))
		val instancename = case #eq_type orig_instance_eq of
				       DOF.INSTANCE {name, ...} => name
				     | _ => DynException.stdException("Encountered non-instance", 
								      "Ordering.orderModel.buildSplit", 
								      Logger.INTERNAL)

		fun isMainInstance sym =
		    sym = instancename

		val outputs = List.filter (not o isMainInstance) partGroup
		val mainInstance = List.find isMainInstance partGroup

		val candidateClasses = valOf(SymbolTable.look (splitMap, classname))

		fun outsym2pos newout =
		    let
			val numberedOuts = ListPair.zip (outputSyms, 
							 List.tabulate(length(outputSyms), 
								    fn(i) => i))
		    in
			case List.find(fn(s,i) => s = newout) numberedOuts of
			    SOME (_, i) => i
			  | NONE => DynException.stdException("Couldn't find an expected output, possible corrupted output list", 
							      "Ordering.orderModel.buildSplit.outsym2pos", 
							      Logger.INTERNAL)
		    end				
		    
		val outputMap = map outsym2pos outputs

		val key = classUsage2key (Option.isSome mainInstance) outputMap 

		val _ = print ("Looking at candidates: " ^ (String.concatWith ", " (map Symbol.name (SymbolTable.listKeys candidateClasses))) ^ " for key: " ^ (Symbol.name key) ^ "\n")

		val (class, instance_eq, (splitMap', classMap', classIOMap')) = 
  		    case SymbolTable.look(candidateClasses, key) of
			SOME (class, inputMap) =>
			let
			    val instance_eq = buildInstance (class, outputs, inputMap, orig_instance_eq)
			in
			    (class, instance_eq, (splitMap, classMap, classIOMap))
			end
		      | NONE =>
			let

			    val (newclass, inputMap, splitMap) = 
				buildClass (classMap, splitMap, instanceClass, outputMap, Option.isSome mainInstance)
				
			    val instance_eq = buildInstance (newclass, outputs, inputMap, orig_instance_eq)
					      

			    val classes = map (fn(c,_) => c) (GeneralUtil.flatten (map SymbolTable.listItems (SymbolTable.listItems (splitMap))))

			    val (classMap', classIOMap') = addClassToClassMap classes (newclass, (classMap, classIOMap))
			    val _ = print ("class map keys are " ^ (String.concatWith ", " (map Symbol.name (SymbolTable.listKeys classMap'))) ^ "\n")
			in
			    (newclass, instance_eq, (SymbolTable.enter (splitMap, classname, SymbolTable.enter(candidateClasses, key, (newclass, inputMap))),
						     classMap',
						     classIOMap'))
			end


	    in
		(splitMap', classMap', classIOMap', instance_eq :: eqs)
	    end

	and buildClass (classMap, splitMap, oldClass:DOF.class, outputMap, includeMainEqs) =
	    let
		val newname = Symbol.symbol ((Symbol.name (#name oldClass)) ^ (Int.toString (Unique.genid())))

		val eqMap = valOf(SymbolTable.look(classMap, #name oldClass))


		(* compute new outputs *)
		val oldoutputs = !(#outputs oldClass)
		val outputs = map (fn(i) => List.nth(oldoutputs, i)) outputMap


		(* compute required inputs *)
		val oldinputs = (!(#inputs oldClass))
		val oldinputsyms = map inp2sym (!(#inputs oldClass))

		val oldinputset = SymbolSet.fromList oldinputsyms

		val _ = print ("class name = " ^ (Symbol.name (#name oldClass)) ^ "\n")

		val _ = print ("includeMainEqs = " ^ (Bool.toString includeMainEqs) ^ "\n")

		val _ = print ("outputMap = " ^ (String.concatWith ", " (map Int.toString outputMap)) ^ "\n")


		(* Use this on syms in RHS of eq that doesn't have dependency info (ie, it's not an instance or an interm) *)
		fun depsOfUsedSym (sym:Symbol.symbol): SymbolSet.set =
		    case (SymbolTable.look(eqMap, sym)) of
			SOME set => SymbolSet.add(set, sym)
		      | NONE => SymbolSet.empty

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
		val mainEqs = ((EqUtil.getDerivativeEqs (!(#eqs oldClass))) @ (EqUtil.getDifferenceEqs (!(#eqs oldClass))))

		val instances = EqUtil.getInstances (!(#eqs oldClass))

		fun depsOfEq eq =
		    case #eq_type eq of
			DOF.INSTANCE {name,...} => 
			(case SymbolTable.look(eqMap, name) of
			     SOME set => set
			   | NONE => 
			     (foldl SymbolSet.union 
				    SymbolSet.empty 
				    (map (fn(sym) => SymbolSet.add(depsOfUsedSym sym, sym)) (ExpProcess.exp2symbols (#rhs eq)))))
		      | _ => 
			let
			    val sym = term2sym (#lhs eq) 
			in
			    case SymbolTable.look(eqMap, sym) of
				SOME set => set
			      | NONE => (foldl SymbolSet.union 
					       SymbolSet.empty 
					       (map (fn(sym) => SymbolSet.add(depsOfUsedSym sym, sym)) (ExpProcess.exp2symbols (#rhs eq))))
			end
			 
		fun nameOfInstance inst =
		    case #eq_type inst of
			DOF.INSTANCE {name, ...} => name
		      | _ => DynException.stdException("Unexpected non-instance encountered", 
						       "Ordering.orderModel.buildClass.nameOfInstance", 
						       Logger.INTERNAL)

		val mainEqDeps = foldl SymbolSet.union 
				       (SymbolSet.fromList (map (fn(eq) => term2sym (#lhs eq)) mainEqs))
				       ((map depsOfEq mainEqs) @ [SymbolSet.fromList(map nameOfInstance instances)])

		val outputDeps = foldl SymbolSet.union SymbolSet.empty (map output2deps outputs)

		val _ = print ("______________________________________\n")
		val _ = print ("name = " ^ (Symbol.name newname) ^"\n")
		val _ = print ("   mainEqs = " ^ (String.concatWith ", " (map Symbol.name (map (fn(eq) => term2sym (#lhs eq)) mainEqs))) ^ "\n")
		val _ = print ("   mainEqDeps = " ^ (String.concatWith ", " (map Symbol.name (SymbolSet.listItems mainEqDeps))) ^ "\n")
		val _ = print ("   outputDeps = " ^ (String.concatWith ", " (map Symbol.name (SymbolSet.listItems outputDeps))) ^ "\n")

		val deps = SymbolSet.union(outputDeps,  
					   if includeMainEqs then
					       mainEqDeps
					   else
					       SymbolSet.empty)


		(* compute equations to include based upon dependencies *)
		fun depInEq dep eq =
		    case #eq_type eq of
			DOF.INSTANCE {name, classname, offset} => 
			dep = name orelse List.exists (fn(s) => s = dep) 
						      (ExpProcess.exp2symbols (Exp.TERM(#lhs eq)))
		      | DOF.INITIAL_VALUE _ => false
		      | _ => dep = (term2sym (#lhs eq))

		fun dep2eq (dep, (maps as (splitMap, classMap, classIOMap, generatedInstances), eqs)) =
		    case List.find (depInEq dep) (!(#eqs oldClass)) of
			SOME eq =>
			(case #eq_type eq of
			     DOF.INTERMEDIATE_EQ => (maps, eq :: eqs)
			   | DOF.DIFFERENCE_EQ _ => (maps, if includeMainEqs then eq :: eqs else eqs)
			   | DOF.DERIVATIVE_EQ _ => (maps, if includeMainEqs then eq :: eqs else eqs)
			   | DOF.INSTANCE {name, classname, offset}
			     =>
			     if SymbolSet.member(generatedInstances, name) then
				 (maps, eqs)
			     else
				 let	
				     (* we have to do all of the following redundant work (redundant with buildsplit) to find instanceClass.  Refactor this later *)
				     val outputSyms = ExpProcess.exp2symbols (Exp.TERM(#lhs eq))
				     val outputs = List.filter (fn(out) => SymbolSet.member(deps, out)) outputSyms
				     val mainInstance = if includeMainEqs then
							    SOME name
							else
							    NONE
				     val partGroup = (case mainInstance of SOME x => [x] | NONE => []) @ outputs

				     (*TODO: do we need to split the subclass before we get here?*)
				     val _ = print ("for instance "^(Symbol.name name)^" partgroup = " ^ (String.concatWith ", " (map Symbol.name partGroup)) ^ "\n")

				     fun outsym2pos newout =
					 let
					     val numberedOuts = ListPair.zip (outputSyms, 
									      List.tabulate(length(outputSyms), 
											 fn(i) => i))
					 in
					     case List.find(fn(s,i) => s = newout) numberedOuts of
						 SOME (_, i) => i
					       | NONE => DynException.stdException("Couldn't find an expected output, possible corrupted output list", 
										   "Ordering.orderModel.buildSplit.outsym2pos", 
										   Logger.INTERNAL)
					 end				

				     val outputMap = map outsym2pos outputs
				     val completeOutputMap = map outsym2pos outputSyms
							     
				     (*				 val key = classUsage2key (Option.isSome mainInstance) outputMap *)
				     val key = classUsage2key true completeOutputMap

				     val _ = print ("looking for classname " ^ (Symbol.name classname) ^ " with key " ^ (Symbol.name key) ^ "\n")
				     val candidateClasses = valOf(SymbolTable.look (splitMap, classname))
							    
				     val (instanceClass, inputMap) =  valOf(SymbolTable.look(candidateClasses, key))
								      
				     val (splitMap', classMap', classIOMap', eqs) = buildSplit (instanceClass, eq) (partGroup, (splitMap, classMap, classIOMap, eqs))

				     val generatedInstances' = SymbolSet.add(generatedInstances, name)
				 in
				     ((splitMap', classMap', classIOMap', generatedInstances'), eqs)
				 end
			   | _ => (maps, eqs)) 
		      | NONE => ((maps, eqs))


		val ((splitMap, classMap, classIOMap, _), eqs) = foldl dep2eq ((splitMap, classMap, classIOMap, SymbolSet.empty), nil) (SymbolSet.listItems deps)

		(* add in main eqs if applicable *)
	(*	val eqs = eqs @ 
		    (if includeMainEqs then
			 mainEqs 
		     else
			 nil )

	 *)
		(* In order to distinguish between true initial values and placeholders to determine
                   state offset locations, in the latter case we map initial values from absolute iterator
                   offsets to relative.  The latter would never naturally occur, so it is distinguishable.
                   This code should be replaced eventually when we have a better mechanism to refer to state space.
		 *)
		fun buildInitialValues (eq) =
		    if includeMainEqs then
			eq
		    else
			let
			    fun abs2rel (sym, Iterator.ABSOLUTE 0) =
				(sym, Iterator.RELATIVE 0)
			      | abs2rel (sym, iterator) =
				(sym, iterator)

			    val {eq_type, sourcepos, lhs, rhs} = eq
			    val lhs' = case lhs of
					   Exp.SYMBOL (sym, prop) => 
					   (Exp.SYMBOL (sym, 
							(case Property.getIterator (prop) of
							     SOME (iterators) => 
							     Property.setIterator prop (map abs2rel iterators)
							   | NONE => prop)))
					 | lhs => lhs
					   
					   
			in
			    {eq_type = eq_type,
			     sourcepos=sourcepos,
			     lhs=lhs',
			     rhs=rhs}
			end
			

		val _ = print ("   eqs' = " ^ (String.concatWith ", " (map Symbol.name (GeneralUtil.flatten ((map (fn(eq) => ExpProcess.exp2symbols (Exp.TERM(#lhs eq))) eqs))))) ^ "\n")


		(* add in initial value eqs ALWAYS *)
		val eqs = eqs @ (map buildInitialValues (EqUtil.getInitialValueEqs (!(#eqs oldClass))))


		val eqDeps = foldl SymbolSet.union SymbolSet.empty (map depsOfEq eqs)
		val eqInputDeps = 
		    SymbolSet.intersection(eqDeps,
					   oldinputset)

		val _ = print ("   oldinputs = " ^ (String.concatWith ", " (map Symbol.name (SymbolSet.listItems oldinputset))) ^ "\n")
		val _ = print ("   eqDeps = " ^ (String.concatWith ", " (map Symbol.name (SymbolSet.listItems eqDeps))) ^ "\n")
		val _ = print ("   eqInputDeps = " ^ (String.concatWith ", " (map Symbol.name (SymbolSet.listItems eqInputDeps))) ^ "\n")



		(* compute needed inputs using outputs and maineqs if applicable *)
		val neededoldinputs = SymbolSet.listItems (foldl computeInputs eqInputDeps outputs)

				      
		val _ = print ("  neededoldinputs = " ^ (String.concatWith ", " (map Symbol.name neededoldinputs)) ^ "\n")

		(* number the inputs symbols so we can create an input mapping and a list of inputs *)
		fun inp2pos inp =
		    let
			val numberedInps = ListPair.zip (oldinputsyms, 
							 List.tabulate(length(oldinputsyms), 
								    fn(i) => i))
		    in
			case List.find(fn(s,i) => s = inp) numberedInps of
			    SOME (_, i) => i
			  | NONE => DynException.stdException("Couldn't find an expected input, possible corrupted input list", 
							      "Ordering.orderModel.buildClass.inp2pos", 
							      Logger.INTERNAL)
		    end				

		val inputMap = map inp2pos neededoldinputs
		val inputs = map (fn(i) => List.nth(oldinputs, i)) inputMap



		val newclass = {name=newname,
				properties= #properties oldClass,
				inputs= ref inputs,
				outputs=ref outputs,
				eqs=ref eqs}

		val splitMap' = splitMap 
	    in
		(newclass, inputMap, splitMap')
	    end

		(* split an instance if there are cycles *)
	and processInstance containerClass (instance_eq: eq, (eqs, (splitMap, classMap, classIOMap))) =
	    let
		val classname = case #eq_type instance_eq of
				    DOF.INSTANCE {classname, ...} => classname
				  | _ => DynException.stdException("Encountered non-instance", 
								   "Ordering.orderModel.processInstance.instancename", 
								   Logger.INTERNAL)
					
		val _ = print ("classname = " ^ (Symbol.name classname) ^ "\n") 
		val _ = print ("  classnames = " ^ (String.concatWith ", " (map Symbol.name (map (fn(c) => #name c) classes))) ^ "\n")
		val instanceClass = valOf (List.find (fn(c) => (#name c) = classname) classes)

		val instancename = case #eq_type instance_eq of
				       DOF.INSTANCE {name, ...} => name
				     | _ => DynException.stdException("Encountered non-instance", 
								      "Ordering.orderModel.processInstance.instancename", 
								      Logger.INTERNAL)

		val _ = print ("got here\n")

		val eqMap = valOf(SymbolTable.look(classMap, #name containerClass))
		val _ = print ("got here again and instancename = " ^ (Symbol.name instancename) ^ "\n")
		val _ = print ("  keys at this level are = " ^ (String.concatWith ", " (map Symbol.name (SymbolTable.listKeys eqMap))) ^ "\n")
		val instanceDeps = valOf(SymbolTable.look(eqMap,
							  instancename))


		val _ = print ("got here too\n")
			     
		val outputSyms = ExpProcess.exp2symbols (Exp.TERM(#lhs instance_eq))

		val _ = print ("=!!!!!!!!!!!!!!!!!!!!!!!!!!!= instancedeps deps:" ^ (String.concatWith ", " (map Symbol.name (SymbolSet.listItems instanceDeps))) ^ "\n")
		val _ = print ("=!!!!!!!!!!!!!!!!!!!!!!!!!!!= outputSyms:" ^ (String.concatWith ", " (map Symbol.name outputSyms)) ^ "\n")

		(* if the deps of the instance in question depend upon any outputs that are from an instance that depends upon an output *)
		(*   ie we are in instance a and a depends upon x.o and x depends upon a.something *)
		(*   so we take our instance deps (including x.o) and find instanceDepInstances (ie x)*)
		(*   then we compute the deps of the instanceDepInstances (which should include any of our outputs if there is a cycle)*)
		(* TODO: does this scale if you have multiple levels backwards to go? *)		    
		fun depoutput2instance sym =
		    let
			fun isInstanceAndContainsOutput eq =
			    case #eq_type eq of
				DOF.INSTANCE _ => List.exists (fn(s) => s = sym) (ExpProcess.exp2symbols(Exp.TERM(#lhs eq)))
				
			      | _ => false
		    in
			case List.find isInstanceAndContainsOutput (!(#eqs containerClass)) of
			    NONE => NONE
			  | SOME eq => 
			    (case #eq_type eq of
				 DOF.INSTANCE {name, ...} => SOME name
			       | _ => DynException.stdException ("non-instance eq encountered",
								 "Ordering.orderModel.processinstance.depoutput2instance",
								 Logger.INTERNAL))
		    end

		val instanceDepInstances = List.mapPartial depoutput2instance (SymbolSet.listItems instanceDeps)

		val depsOfInstanceDepInstances = foldl SymbolSet.union SymbolSet.empty (map (fn(i) => valOf (SymbolTable.look(eqMap, i))) instanceDepInstances)

		val _ = print ("instanceDepInstances = " ^ (String.concatWith ", " (map Symbol.name instanceDepInstances)) ^ "\n")
		val _ = print ("depsOfInstanceDepInstances = " ^ (String.concatWith ", " (map Symbol.name (SymbolSet.listItems depsOfInstanceDepInstances))) ^ "\n")
		val mutuallyRecursiveInstances = List.exists (fn(os) => SymbolSet.member (depsOfInstanceDepInstances, os)) outputSyms
	    in
		(* if the instance depends on any of its outputs, or if the deps of the instance depend upon any outputs that are from an instance that depends upon an output, then we know that a cycle exists *)

		if List.exists (fn(os) => SymbolSet.member (instanceDeps, os)) outputSyms orelse mutuallyRecursiveInstances then
		    let
			fun orderParts unsatisfiedParts =
			    if (SymbolSet.isEmpty unsatisfiedParts) then
				nil
			    else
				let
				    val (readyParts, remainingParts) = 
					List.partition (fn(p) => SymbolSet.isEmpty (SymbolSet.intersection (unsatisfiedParts,
													    valOf(SymbolTable.look(eqMap, p)))))
						       (SymbolSet.listItems unsatisfiedParts)
						       
				in
				    readyParts :: (orderParts (SymbolSet.fromList remainingParts))
				end
			    
			val orderedParts = 
			    if mutuallyRecursiveInstances then
				(orderParts (SymbolSet.fromList outputSyms)) @ [[instancename]]
			    else
				orderParts (SymbolSet.fromList(instancename :: outputSyms))



			val _ = print ("  Splitting occurred on " ^ (Symbol.name instancename) ^ "\n")
			val _ = app (fn(order) => print ("  Group: " ^ (String.concatWith ", " (map Symbol.name order)) ^ "\n")) orderedParts


			val (splitMap, classMap, classIOMap, instance_eqs') =
			    foldl (buildSplit (instanceClass, instance_eq))
				  (splitMap, classMap, classIOMap, nil)
				  orderedParts
			val _ = print ("done with foldl of  buildsplit\n")

		    in
			(instance_eqs' @ eqs, (splitMap, classMap, classIOMap))
		    end
		else
		    (instance_eq::eqs, (splitMap, classMap, classIOMap))
	    end

	fun splitClasses (class: DOF.class, (splitMap, classMap, classIOMap)) =
	    let
		val _ = print ("===splitting class: " ^ (Symbol.name (#name class)) ^ "\n")
		val (instance_eqs, other_eqs) = List.partition EqUtil.isInstance (!(#eqs class))

		val (instance_eqs', (splitMap', classMap', classIOMap')) 
		  = foldl (processInstance class) (nil, (splitMap, classMap, classIOMap)) instance_eqs

		val _ = #eqs class := instance_eqs' @ other_eqs

		val _ = print ("class map keys are " ^ (String.concatWith ", " (map Symbol.name (SymbolTable.listKeys classMap'))) ^ "\n")
		val _ = print ("===about to reprocess splitting class: " ^ (Symbol.name (#name class)) ^ "\n")

		(* reprocess to catch renamed instances *)
		val classes = map (fn(c,_) => c) (GeneralUtil.flatten (map SymbolTable.listItems (SymbolTable.listItems (splitMap'))))
		    val _ = print ("split classes classes are " ^ (String.concatWith ", " (map (fn(c) => Symbol.name (#name c)) classes)) ^ "\n")

		val (classMap'', classIOMap'') = addClassToClassMap classes (class, (#1(SymbolTable.remove(classMap', #name class)), (#1(SymbolTable.remove(classIOMap', #name class)))))
		val _ = print ("===done splitting class: " ^ (Symbol.name (#name class)) ^ "\n")
	    in
		(splitMap', classMap'', classIOMap'')
	    end


	val splitMap = foldl (fn(c,t) => SymbolTable.enter(t,
							   #name c, 
							   SymbolTable.enter(SymbolTable.empty, 
									     classUsage2key true (List.tabulate (length (!(#outputs c)), fn(i) => i)),
									     (c, List.tabulate (length (!(#inputs c)), fn(i) =>i)))))
			     SymbolTable.empty 
			     classes

	val (splitMap, classMap, classIOMap) = foldl splitClasses (splitMap, classMap, classIOMap) classes

	val _ = print ("done splitting classes\n")

	val classes' = map (fn(c,_) => c) (GeneralUtil.flatten (map SymbolTable.listItems (SymbolTable.listItems (splitMap))))



	(* loop through classes and order eqs by deps, construct instances *)

	fun sortEqs _ _ nil = nil
	  | sortEqs classname satisfiedDeps eqs = 
	    let
(*		(* partition into the two types of sortable equations *)
		val (intermediate_eqs, instance_eqs) 
		  = List.partition (fn(_, eq) => EqUtil.isIntermediate eq) eqs*)

		(* select satisfied equations *)
		fun depsSatisfied (deps, _) =
		    SymbolSet.isSubset(foldl SymbolSet.union SymbolSet.empty deps, satisfiedDeps)


		val (satisfied_eqs, unsatisfied_eqs) 
		  = List.partition depsSatisfied eqs

		fun getLHSSyms (_, eq):(Symbol.symbol list) =
		    ExpProcess.exp2symbols (Exp.TERM(#lhs eq))

		val satisfiedDeps' = SymbolSet.union (satisfiedDeps, 
						      SymbolSet.fromList (GeneralUtil.flatten (map getLHSSyms
												   satisfied_eqs)))

		fun eqanddep2str (deps, eq) = 
		    let
			val eqstr:string = 
			    "eq = " ^ 
			    (case #eq_type eq of 
				 DOF.INSTANCE {name, ...} => (Symbol.name name)
			       | _ => 
				 (String.concatWith ", " (map Symbol.name (getLHSSyms (deps,eq)))))

			val depstr:string = " deps on " ^ (String.concatWith ", " (map Symbol.name (GeneralUtil.flatten(map SymbolSet.listItems deps))))
		    in
			eqstr ^ depstr
		    end


		val _ = print ("Satisfied deps for " ^ (String.concatWith ", " (map Symbol.name (GeneralUtil.flatten (map getLHSSyms satisfied_eqs)))) ^ "\n")
		val _ = print ("  deps are  " ^ (String.concatWith "\n    " (map eqanddep2str (satisfied_eqs))) ^ "\n")
		val _ = print ("  satisfiedDeps' = " ^ (String.concatWith ", " (map Symbol.name (SymbolSet.listItems satisfiedDeps'))) ^ "\n")
		val _ = print ("UnSatisfied deps for " ^ (String.concatWith ", " (map Symbol.name (GeneralUtil.flatten (map getLHSSyms unsatisfied_eqs)))) ^ "\n\n")
		val _ = print ("  deps are  " ^ (String.concatWith "\n    " (map eqanddep2str (unsatisfied_eqs))) ^ "\n")

		val satisfied_eqs = map (fn(_,eq) => eq) satisfied_eqs
				     
		val sorted_eqs = if SymbolSet.equal (satisfiedDeps, satisfiedDeps') andalso length(unsatisfied_eqs) > 0 then
				     (Logger.log_error (Printer.$("Can't sort equations in model " ^ (Symbol.name classname)));				      
				      DynException.setErrored();
				      raise SortFailed)
				 else
				     sortEqs classname satisfiedDeps' unsatisfied_eqs
	    in
		satisfied_eqs @ sorted_eqs
	    end

	fun orderClass (class:DOF.class) =
	    let
		val eqs = (!(#eqs class))
		val _ = print ("looking up class with name " ^ (Symbol.name (#name class)) ^ "\n")
		val mapping = valOf (SymbolTable.look (classMap, #name class))

		val init_eqs = EqUtil.getInitialValueEqs eqs
		val state_eqs = (EqUtil.getDerivativeEqs eqs) @ (EqUtil.getDifferenceEqs eqs)
		val other_eqs = (EqUtil.getInstances eqs) @ (EqUtil.getIntermediateEqs eqs)

		fun pairEqWithDeps eq =
		    case #eq_type eq of
			DOF.INTERMEDIATE_EQ =>
			([valOf (SymbolTable.look (mapping, term2sym (#lhs eq)))], eq) 
		      | DOF.INSTANCE {name, classname, offset} =>
			let
			    val instanceDep = setValOf (SymbolTable.look (mapping, name)) 
			    val outputDeps = map (fn(sym) => setValOf (SymbolTable.look (mapping, sym)))
						 (ExpProcess.exp2symbols (#rhs eq))
			in
			    (instanceDep::outputDeps, eq)
			end
		      | _ => DynException.stdException("Encountered unexpected equation type", 
						       "Ordering.orderModel.orderClass.pairEqsWithDeps", 
						       Logger.INTERNAL)

		val sortable_eqs = map pairEqWithDeps other_eqs

		val availSyms = (GeneralUtil.flatten (map (fn(eq) => (ExpProcess.exp2symbols (Exp.TERM(#lhs eq)))) init_eqs))
				@ (map (fn(i) => term2sym (#name i)) (!(#inputs class)))


		val _ = print ("availsyms = " ^ (String.concatWith ", " (map Symbol.name availSyms)) ^ "\n")

		val satisfiedDeps = SymbolSet.fromList availSyms

		val sorted_eqs = sortEqs (#name class) satisfiedDeps sortable_eqs

		val _ = #eqs class := init_eqs @ sorted_eqs @ state_eqs

	    in
		()
	    end

	fun reachableClasses classes name =
	    let
		fun eq2reachableclasses ({eq_type = DOF.INSTANCE {classname, ...}, ...}, set) =
		    SymbolSet.union(reachableClasses classes classname, set)
		  | eq2reachableclasses (_, set) = set

		val class = valOf (List.find (fn(c) => #name c = name) classes)
		val set = foldl eq2reachableclasses SymbolSet.empty (!(#eqs class))
	    in
		SymbolSet.add(set, name)
	    end

	(* prune out dead classes *)
	val liveclasses = reachableClasses classes' (#classname topInstance)
	val classes'' = List.filter (fn(c) => SymbolSet.member (liveclasses, #name c)) classes'

			
	val _ = print ("splitting performed\n==========\n\n")	    
	val _ = printClassMap classMap
	val _ = printClassIOMap classIOMap
	val _ = printModel (classes', topInstance, props)

	val _ = print ("pruning performed\n=====================\n\n")
	val _ = printModel (classes'', topInstance, props)

	val _ = app orderClass classes''
		
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
