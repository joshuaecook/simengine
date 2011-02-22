signature DOFOUTLINE =
sig

    (* Define the type of a dependency - this is a record containing a symbol set of all the
     * symbols that are on the rhs of an equation, and a second symbol set to contain all 
     * the iterators. *)
    type dependencies = {syms: SymbolSet.set, iter_syms: SymbolSet.set}
    type dependency_table = dependencies SymbolTable.table

    (* Instance dependencies are like the above dependencies, except we need to keep track 
     * of the class name associated with the instance so we can grab the dependencies through
     * the hierarchy. *)
    type instance_dependencies = {instclass: Symbol.symbol, syms: SymbolSet.set, 
				  input_map: SymbolSet.set SymbolTable.table, output_map: Symbol.symbol SymbolTable.table, 
				  iter_syms: SymbolSet.set}
    type instance_dependency_table = instance_dependencies SymbolTable.table

    (* Each class outline mirrors the original class, but everything is a lot more explicit. 
     * From this structure, it's very easy to determine the states, or the state inits, or 
     * traverse only from outputs through intermediates until reaching states or inputs.  *)
    type class_outline = {name: Symbol.symbol,
			  iterators: SymbolSet.set,
			  inputs: dependency_table,
			  init_exps: dependency_table,
			  intermediate_exps: dependency_table,
			  instance_exps: instance_dependency_table,
			  state_exps: dependency_table,
			  outputs: dependency_table}

    (* The model outline is just a table mapping a classname to a class outline.  Everything here 
     * is a symbol table or a symbol set to absolutely maximize performance retrieving data 
     * from this data structure *)
    type model_outline = class_outline SymbolTable.table

    (* class_to_outline and model_to_outline takes a class or model, respectively, and returns 
     * its outline. *)
    val class_to_outline : model_outline -> Symbol.symbol -> (model_outline * class_outline)
    val model_to_outline : DOF.model -> model_outline

    (* for debugging - this will log the outlines *)
    val log_class_outline : class_outline -> unit
    val log_model_outline : model_outline -> unit
					
    (* traverse through a class and find all the dependent symbols or iterators *)
    val symbol_to_deps : class_outline -> Symbol.symbol -> SymbolSet.set
    val symbol_to_iters : class_outline -> Symbol.symbol -> SymbolSet.set

    (* perform tests on the outline *)
    val class_to_assigned_symbols : class_outline -> SymbolSet.set
    val class_to_used_symbols : class_outline -> SymbolSet.set
    val class_to_undefined_symbols : class_outline -> SymbolSet.set
    val model_to_cycles : model_outline -> {classname: Symbol.symbol, deps: Symbol.symbol list} list
end
structure DOFOutline : DOFOUTLINE =
struct

fun error msg = (Logger.log_error (Printer.$ (msg)); 
		 DynException.setErrored())

fun error_no_return msg = DynException.stdException (msg, "DOFOutline", Logger.INTERNAL)

    type dependencies = {syms: SymbolSet.set, iter_syms: SymbolSet.set}
    type dependency_table = dependencies SymbolTable.table

    type instance_dependencies = {instclass: Symbol.symbol, syms: SymbolSet.set, 
				  input_map: SymbolSet.set SymbolTable.table, output_map: Symbol.symbol SymbolTable.table, 
				  iter_syms: SymbolSet.set}
    type instance_dependency_table = instance_dependencies SymbolTable.table

    type class_outline = {name: Symbol.symbol,
			  iterators: SymbolSet.set,
			  inputs: dependency_table,
			  init_exps: dependency_table,
			  intermediate_exps: dependency_table,
			  state_exps: dependency_table,
			  instance_exps: instance_dependency_table,
			  outputs: dependency_table}

    type model_outline = class_outline SymbolTable.table

    local
	open Layout
	fun dependencies_to_layout {syms, iter_syms} =
	    mayAlign [label ("syms", SymbolSet.toLayout syms),
		      label ("iter_syms", SymbolSet.toLayout iter_syms)]
	fun dep_table_to_layout table = 
	    align (map (fn(key, dep)=> heading(Symbol.name key, dependencies_to_layout dep)) (SymbolTable.listItemsi table))
	fun symset_table_to_layout table = 
	    mayAlign (map (fn(key, set)=> heading(Symbol.name key, SymbolSet.toLayout set)) (SymbolTable.listItemsi table))	    
	fun symbol_table_to_layout table = 
	    mayAlign (map (fn(key, sym)=> heading(Symbol.name key, Symbol.layout sym)) (SymbolTable.listItemsi table))	    
	fun instance_dependencies_to_layout {syms, iter_syms, instclass, input_map, output_map} =
	    align [label ("instclass", Symbol.layout instclass),
		   label ("syms", SymbolSet.toLayout syms),
		   label ("iter_syms", SymbolSet.toLayout iter_syms),
		   heading ("input_map", symset_table_to_layout input_map),
		   heading ("output_map", symbol_table_to_layout output_map)]
	fun inst_dep_table_to_layout table = 
	    align (map (fn(key, dep)=> heading(Symbol.name key, instance_dependencies_to_layout dep)) (SymbolTable.listItemsi table))
	fun class_outline_to_layout {name, iterators, inputs, init_exps, intermediate_exps, state_exps, instance_exps, outputs} = 
	    heading (Symbol.name name,
		     align [heading ("Iterators", SymbolSet.toLayout iterators),
			    heading ("Inputs", dep_table_to_layout inputs),
			    heading ("Init Exps", dep_table_to_layout init_exps),
			    heading ("Intermediate Exps", dep_table_to_layout intermediate_exps),
			    heading ("State Exps", dep_table_to_layout state_exps),
			    heading ("Instance Exps", inst_dep_table_to_layout instance_exps),
			    heading ("Outputs", dep_table_to_layout outputs)])

	fun model_outline_to_layout outline =
	    heading ("Model Outline",
		     align (map class_outline_to_layout (SymbolTable.listItems outline)))
    in
    fun log_model_outline outline = 
	if DynamoOptions.isFlagSet "logoutline" then
	    Layout.log (add_newline (model_outline_to_layout outline))
	else
	    ()

    fun log_class_outline outline = 
	if DynamoOptions.isFlagSet "logoutline" then
	    Layout.log (add_newline (heading ("Class Outline", class_outline_to_layout outline)))
	else
	    ()
    end


    fun exp_to_symbols_and_iterators exp =
	let
	    val terms = ExpProcess.exp2freetermsymbols exp
	    val syms = SymbolSet.fromList (map Term.sym2curname terms)
	    val embedded_iterators = SymbolSet.fromList
					 (map (fn(iter_sym, _)=>iter_sym)
					      (List.mapPartial TermProcess.symbol2temporaliterator terms))
	    val literal_iterators = SymbolSet.fromList 
					(map Term.sym2curname 
					     (List.filter Term.isIterator terms))
	in
	    (syms, SymbolSet.union (embedded_iterators, literal_iterators))
	end

    fun exps_to_symbols_and_iterators nil =
	{syms=SymbolSet.empty, iter_syms=SymbolSet.empty}
      | exps_to_symbols_and_iterators (exp::rest) =
	let
	    val (syms, iter_syms) = exp_to_symbols_and_iterators exp
	    val {syms=syms', iter_syms=iter_syms'} = exps_to_symbols_and_iterators rest
	in
	    {syms=SymbolSet.union (syms, syms'), iter_syms=SymbolSet.union (iter_syms, iter_syms')}
	end

    (* class_to_outline - construct a simpler representation of the common elements that need to be
     * traversed, such as finding all the symbols or iterators that are arguments of an intermediate *)
    fun class_to_outline model_outline classname =
	case SymbolTable.look (model_outline, classname) of
	    SOME v => (model_outline, v)
	  | NONE => 
	    let
		(* pull out the class *)
		val class = CurrentModel.classname2class classname

		(* few helper calls *)
		val empty = SymbolTable.empty
		val emptyset = SymbolSet.empty
		val singleton = SymbolSet.singleton

		(* first, pull out the relevent pieces *)
		val name = #name class
		val inputs = !(#inputs class)
		val exps = !(#exps class)
		val (init_exps, rest) = List.partition ExpProcess.isInitialConditionEq exps
		val (instance_exps, rest) = List.partition (fn(exp)=>ExpProcess.isInstanceEq exp orelse ExpProcess.isOutputEq exp) rest
		val (state_exps, intermediate_exps) = List.partition ExpProcess.isStateEq rest
		val outputs = !(#outputs class)
		val iterators = CurrentModel.iterators()

		(* first, the inputs - here just grab the iterator present *)
		val inputs_outline = 
		    foldl 
			(fn(inp, table)=> 
			   let
			       val name = DOF.Input.name inp
			       val sym = Term.sym2curname name
			       val entry = {syms=emptyset,
					    iter_syms=case TermProcess.symbol2temporaliterator name of
							  SOME (iter_sym, _) => singleton iter_sym
							| NONE => emptyset}
			   in
			       SymbolTable.enter (table, sym, entry)
 			   end)
			empty
			inputs

		(* next, the outputs *)
		val outputs_outline =
		    foldl
			(fn(out, table)=>
			   let
			       val name = DOF.Output.name out
			       val sym = Term.sym2curname name
			       val contents = DOF.Output.contents out
			       val condition = DOF.Output.condition out
			       val entry as {syms, iter_syms} = exps_to_symbols_and_iterators (condition::contents)
			       val entry' = case TermProcess.symbol2temporaliterator name of
						SOME (iter_sym, _) => {syms=syms, iter_syms=SymbolSet.add(iter_syms, iter_sym)}
					      | NONE => entry
			   in
			       SymbolTable.enter (table, sym, entry')
			   end)
			empty
			outputs

		(* next, the expressions *)
		fun exps_to_outline exps =
		    foldl
			(fn(exp, table)=>
			   let
			       val (lhs_syms, lhs_iter_syms) = exp_to_symbols_and_iterators (ExpProcess.lhs exp)
			       val (rhs_syms, rhs_iter_syms) = exp_to_symbols_and_iterators (ExpProcess.rhs exp)
			       val syms = ExpProcess.getLHSSymbols exp
			       fun add_sym (table, sym) =
				   let val entry = {syms=rhs_syms, iter_syms=SymbolSet.union (lhs_iter_syms, rhs_iter_syms)}
				   in SymbolTable.enter (table, sym, entry)
				   end

			   in
			       case syms of
				   nil => table (* unexpected, but why not ... *)
				 | [sym] => add_sym (table, sym)
				 | syms => foldl
					       (fn(sym', table')=> add_sym (table', sym'))
					       table
					       syms
			   end	     
			   handle e => DynException.checkpoint ("DOFOutline.class_to_outline.exp_to_outline ["^(ExpPrinter.exp2str exp)^"]") e)
			empty
			exps
			handle e => DynException.checkpoint "DOFOutline.class_to_outline.exps_to_outline" e

		val init_exps_outline = exps_to_outline init_exps
		val state_exps_outline = exps_to_outline state_exps
		val intermediate_exps_outline = exps_to_outline intermediate_exps

		(* finally, the instances are the hardest ... we are going to need a foldl inside a foldl since there
		 * could be multiple outputs for a given instance *)
		val (model_outline, instance_outline) = 
		    foldl
			(fn(inst, (model_outline, table))=>
			   let
			       val {classname, outargs, inpargs,...} = ExpProcess.deconstructInst inst
			       val (syms, iter_syms) = exp_to_symbols_and_iterators (ExpProcess.rhs inst)
			       val (model_outline', submodel_class) = class_to_outline model_outline classname										  
					
			       (* create the instance output to class output map *)
			       val output_map = 
				   SymbolTable.foldli
				       (fn(class_output, instance_term, map)=>
					  SymbolTable.enter (map, Term.sym2curname instance_term, class_output)
				       )
				       SymbolTable.empty
				       outargs
				
			       (* create the class input to instance input map *)
			       val exp_to_symbols = #1 o exp_to_symbols_and_iterators
			       val input_map = 
				   SymbolTable.foldli
				       (fn(class_input, instance_exp, map)=>
					  SymbolTable.enter (map, class_input, exp_to_symbols instance_exp)
				       )
				       SymbolTable.empty
				       inpargs

			       val table' = SymbolTable.foldl
						(fn(term, table')=>
						   let
						       (* use the output name as the key for the symbol table *)
						       val sym = Term.sym2curname term

						       val iter_syms' = case TermProcess.symbol2temporaliterator term of
									    SOME (iter_sym, _) => SymbolSet.add (iter_syms, iter_sym)
									  | NONE => iter_syms

						       (* produce the entry for the output table *)
						       val entry' = {instclass=classname, syms=syms, iter_syms=iter_syms',
								     input_map=input_map, output_map=output_map}
						   in
						       SymbolTable.enter (table', sym, entry')
						   end)
						table
						outargs
			   in
			       (model_outline', table')
			   end)
			(model_outline, empty)
			instance_exps

		val iterator_outline =
		    SymbolSet.fromList (map #1 iterators)

	    in
		(model_outline,
		 {name=name,
		  iterators=iterator_outline,
		  inputs=inputs_outline,
		  init_exps=init_exps_outline,
		  intermediate_exps=intermediate_exps_outline,
		  state_exps=state_exps_outline,
		  instance_exps=instance_outline,
		  outputs=outputs_outline})
	    end
	    handle e => DynException.checkpoint "DOFOutline.class_to_outline" e

    fun model_to_outline ((classes, _, _):DOF.model) = 
	foldl
	    (fn(c as {name,...}, table)=>
	       let
		   val (table', outline) = class_to_outline table name
	       in
		   SymbolTable.enter (table', name, outline )
	       end)
	    SymbolTable.empty
	    classes
	handle e => DynException.checkpoint "DOFOutline.model_to_outline" e

    fun inTable (table, sym) = 
	case SymbolTable.look (table, sym) of
	    SOME _ => true
	  | NONE => false

    fun symbol_is_leaf (outline:class_outline) sym =
	inTable (#inputs outline, sym) orelse
	inTable (#init_exps outline, sym) orelse
	SymbolSet.member (#iterators outline, sym)

    fun symbol_is_intermediate (outline:class_outline) sym = 
	inTable (#intermediate_exps outline, sym) orelse
	inTable (#instance_exps outline, sym) orelse
	inTable (#outputs outline, sym)

    (* Take a symbol, find all the dependencies *)
    fun symbol_to_deps (outline:class_outline) sym =
	let
	    val {name, iterators, inputs, 
		 init_exps, intermediate_exps, state_exps, instance_exps, 
		 outputs} = outline

	    fun recurse syms = 
		foldl
		    (fn(sym, set)=>SymbolSet.union (symbol_to_deps outline sym, set))
		    SymbolSet.empty
		    (SymbolSet.listItems syms)
	in
	    if symbol_is_leaf outline sym then
		SymbolSet.singleton sym
	    else 
		case SymbolTable.look (#intermediate_exps outline, sym) of
		    SOME {syms,...} => recurse syms
		  | NONE => (case SymbolTable.look (#instance_exps outline, sym) of
				 SOME {syms,...} => recurse syms
			       | NONE => (case SymbolTable.look (#outputs outline, sym) of 
					      SOME {syms, ...} => recurse syms
					    | NONE => SymbolSet.empty))
	end
	handle e => DynException.checkpoint "DOFOutline.symbol_to_deps" e

    (* now repeat for iterators *)
    fun symbol_to_iters (outline:class_outline) sym =
	let
	    val {name, iterators, inputs, 
		 init_exps, intermediate_exps, state_exps, instance_exps, 
		 outputs} = outline

	    fun recurse syms = 
		foldl
		    (fn(sym, set)=>SymbolSet.union (symbol_to_iters outline sym, set))
		    SymbolSet.empty
		    (SymbolSet.listItems syms)
	in
	    case SymbolTable.look (#inputs outline, sym) of
		SOME {syms,iter_syms} => iter_syms
	      | NONE => (case SymbolTable.look (#init_exps outline, sym) of
			     SOME {syms,iter_syms} => iter_syms
			   | NONE => (case SymbolTable.look (#intermediate_exps outline, sym) of
					  SOME {syms,iter_syms} => SymbolSet.union (iter_syms, recurse syms)
					| NONE => (case SymbolTable.look (#instance_exps outline, sym) of
						       SOME {syms,iter_syms,...} => SymbolSet.union (iter_syms, recurse syms)
						     | NONE => (case SymbolTable.look (#outputs outline, sym) of 
								    SOME {syms,iter_syms} => SymbolSet.union (iter_syms, recurse syms)
								  | NONE => SymbolSet.empty))))
	end
	handle e => DynException.checkpoint "DOFOutline.symbol_to_iters" e

    (* class_to_assigned_symbols - list of all symbols that are defined as states, the lhs of intermediates, inputs, instance outputs, or iterators *)
    fun class_to_assigned_symbols (outline: class_outline) = 
	let
	    (* grab all the expression types that return values *)
	    val {inputs, iterators, init_exps, intermediate_exps, instance_exps, ...}  = outline

	    (* create an initially empty list of defined symbols.  As we come across symbols that are defined, we'll add them to this list *)
	    val assigned_symbols = SymbolSet.empty

	    (* incrementally add to this list *)
	    val assigned_symbols = SymbolSet.addList(assigned_symbols, SymbolTable.listKeys inputs)
	    val assigned_symbols = SymbolSet.union(assigned_symbols, iterators)
	    val assigned_symbols = SymbolSet.addList(assigned_symbols, SymbolTable.listKeys init_exps)
	    val assigned_symbols = SymbolSet.addList(assigned_symbols, SymbolTable.listKeys intermediate_exps)
	    val assigned_symbols = SymbolSet.addList(assigned_symbols, SymbolTable.listKeys instance_exps)

	in
	    assigned_symbols
	end

    (* class_to_used_symbols - list of all symbols that are used somewhere in the class *)
    fun class_to_used_symbols (outline: class_outline) =
	let
	    (* grab all the expression types that use values *)
	    val {init_exps, intermediate_exps, instance_exps, state_exps, outputs, ...} = outline

	    (* create an initially empty list of used symbols. *)
	    val used_symbols = SymbolSet.empty

	    (* create two helper function to grab the symbols from the tables *)
	    fun dep_table_to_symbols deptable = SymbolSet.flatmap #syms (SymbolTable.listItems deptable)
	    fun inst_dep_table_to_symbols deptable = SymbolSet.flatmap #syms (SymbolTable.listItems deptable)

	    (* incrementally add to this list *)
	    val used_symbols = SymbolSet.union(used_symbols, dep_table_to_symbols init_exps)
	    val used_symbols = SymbolSet.union(used_symbols, dep_table_to_symbols intermediate_exps)
	    val used_symbols = SymbolSet.union(used_symbols, inst_dep_table_to_symbols instance_exps)
	    val used_symbols = SymbolSet.union(used_symbols, dep_table_to_symbols state_exps)
	    val used_symbols = SymbolSet.union(used_symbols, dep_table_to_symbols outputs)
	in
	    used_symbols
	end

    (* class_to_undefined_symbols - checks to make sure every symbol that appears on the rhs of an expression
     * is defined either on the left hand side of an intermediate or initial value equation, as an input, or is returned by an instance *)
    fun class_to_undefined_symbols (outline: class_outline) =
	let
	    (* first go through and find every symbol that is defined *)
	    val assigned_symbols = class_to_assigned_symbols (outline)

	    (* now create a list of all symbols that are used on the rhs *)
	    val used_symbols = class_to_used_symbols (outline)

	    (* see that the difference is *)
	    val difference = SymbolSet.difference(used_symbols, assigned_symbols)

	in
	    difference
	end
	handle e => DynException.checkpoint "DOFOutline.class_to_undefined_symbols" e


    local
	datatype progress = Init | InProgress | Complete
	type inputs = SymbolSet.set (* each entry has a symbol set of input symbols *)
	type intermediate_submodel_paths = (progress * inputs SymbolTable.table) SymbolTable.table
	type submodel_paths = inputs SymbolTable.table SymbolTable.table

	fun intermediate_paths_to_paths (paths : intermediate_submodel_paths) : submodel_paths =
	    SymbolTable.map (fn(_,output_to_inputs)=> output_to_inputs) paths

	fun log_mapping msg (mapping:SymbolSet.set SymbolTable.table) = 
	    let
		open Layout
		fun entryToLayout (lhs_sym, rhs_syms) =
		    seq (map str [Symbol.name lhs_sym, ": ", SymbolSet.toStr rhs_syms])
		val l = heading ("MAPPING " ^ msg, 
				 align (map entryToLayout (SymbolTable.listItemsi mapping)))
	    in
		log (add_newline l)
	    end

	fun log_model_paths msg (model_paths: intermediate_submodel_paths) = 
	    let
		open Layout
		fun progressToStr Init = "Init"
		  | progressToStr InProgress = "InProgress"
		  | progressToStr Complete = "Complete"
		fun outToLayout (out, inputs) =
		    seq (map str [Symbol.name out, ": ", SymbolSet.toStr inputs])
		fun classToLayout (classname, (progress, mappings)) =
		    heading (Symbol.name classname ^ " [" ^ (progressToStr progress) ^ "]",
			     align (map outToLayout (SymbolTable.listItemsi mappings)))
		val l = heading ("MODEL_PATHS " ^ msg, 
				 align (map classToLayout (SymbolTable.listItemsi model_paths)))
	    in
		log (add_newline l)
	    end
		
    in

    (* instance_exp_to_intermediate_exp - this function uses several pieces of information, the input_map, the model_paths, and the
     * output_map structure to create a dependencies entry to make an instance equation look like an intermediate equation.  This 
     * will take only those inputs that are directly linked to the output (as defined in model_paths), ignoring those that are
     * accessible through states. *)
    fun instance_exp_to_intermediate_exp (model_outline, model_paths) (instance_output, {instclass, input_map, output_map, syms, iter_syms}) 
	: (dependencies * intermediate_submodel_paths)= 
	let		
	    val class_output = case SymbolTable.look (output_map, instance_output) of
				   SOME output => output
				 | NONE => (error ("Can't find "^(Symbol.name instance_output)^" in output_map table");
					    Symbol.symbol "undefined")

	    (* now grab the class paths *)
	    val model_paths' = class_to_paths (model_outline, model_paths) instclass
	    val class_paths = case SymbolTable.look (model_paths', instclass) of
				  SOME (_, paths) => paths
				| NONE => error_no_return ("Can't find class " ^ (Symbol.name instclass) ^ " in model_paths")
					      
	    val class_inputs =  case SymbolTable.look (class_paths, class_output) of
				    SOME inputs => inputs
				  | NONE => (error ("Can't find "^(Symbol.name class_output)^" in input symbol table for Model "^(Symbol.name instclass)^" in model_paths");
					     SymbolSet.empty)
					    

	    val instance_inputs = SymbolSet.flatmap 
				      (fn(class_input)=> case SymbolTable.look (input_map, class_input) of
							     SOME instance_inputs => instance_inputs
							   | NONE => SymbolSet.empty)
				      (SymbolSet.listItems class_inputs)
	(*
	 val _ = Util.log ("Creating intermediate exp:")
	 val _ = Util.log ("  Instance Output: " ^ (Symbol.name instance_output))
	 val _ = Util.log ("  Class Output: " ^ (Symbol.name class_output))
	 val _ = Util.log ("  Class Inputs: " ^ (SymbolSet.toStr class_inputs))
	 val _ = Util.log ("  Instance Inputs: " ^ (SymbolSet.toStr instance_inputs))
	 *)
	in
	    ({syms=instance_inputs, iter_syms=SymbolSet.empty}, model_paths')
	end



    (* Take a symbol, find all the algebraic dependencies.  This one searches through instances by relying on 
     * model_paths, a mapping from outputs back to inputs of a submodel. *)
    and symbol_to_algebraic_deps_through_instances (model_outline, model_paths) class_outline previous_dependencies sym = 
	let
	    val {name, iterators, inputs, 
		 init_exps, intermediate_exps, state_exps, instance_exps, 
		 outputs} = class_outline

	    (* This is the starting point for dependencies *)
	    val previous_dependencies = SymbolSet.add (previous_dependencies, sym)

	    (* go through each of the symbols and see what its dependencies are *)
	    fun recurse_helper model_paths syms =
		foldl
		    (fn(sym, (set, model_paths', found_cycle))=>
		       (* if we already found a cycle, don't worry - this is not the time to catch them.  We're doing the more thorough 
			* check below.  So just stop the remaining processing. *)
		       if not found_cycle then
			   (* don't go in an endless loop if it already found the symbol in the set of dependencies *)
			   if not (SymbolSet.member (set, sym)) then
			       let			   
				   (* so here we make the recursive call back.  This can update model_paths, so we keep a new copy
				    * and return that back *)
				   val (deps, model_paths'') = symbol_to_algebraic_deps_through_instances 
								   (model_outline, model_paths') 
								   class_outline 
								   set 
								   sym

				   (* we create a new set that is the union of all that we know are dependencies *)
				   val set' = SymbolSet.add (SymbolSet.union (deps, set), sym)

			       in
				   if SymbolSet.equal (set, set') then
				       (* we found a cycle if we bottom out and produce the identical set *)
				       (set, model_paths'', true)
				   else
				       (* otherwise, we're going to return the new set of dependencies and a possibly updated 
					* model_paths to go on for the next iteration *)
				       (set', model_paths'', false)
			       end
			   else
			       (set, model_paths', found_cycle)
		       else
			   (set, model_paths', true))
		    (previous_dependencies, model_paths, false)
		    (SymbolSet.listItems syms)

	    (* recurse will try to search through each of the found dependencies - we use a wrapper to handle the found_cycle flag *)
	    fun recurse model_paths syms = 
		let
		    val (table, model_paths', found_cycle) = recurse_helper model_paths syms
		in
		    (table, model_paths')
		end

	    (* compute the deps recursively - add all the deps found from the intermediates and outpus.  When you come 
	     * across an instance, convert it into an intermediate by using the model_paths structure, and add those 
	     * symbols *)
	    val (deps, model_paths') = 
		if symbol_is_leaf class_outline sym then
		    (SymbolSet.singleton sym, model_paths)
		else 
		    case SymbolTable.look (#intermediate_exps class_outline, sym) of
			SOME {syms,...} => recurse model_paths syms
		      | NONE => (case SymbolTable.look (#instance_exps class_outline, sym) of
				     SOME (exp_entry as {instclass, syms,...}) => 
				     (* now, we see that the symbol is an output of an instance.  Let's make that instance look 
				      * like an intermediate *)
				     let
					 val ({syms, ...}, model_paths') = instance_exp_to_intermediate_exp (model_outline, model_paths) (sym, exp_entry)
				     in 
					 recurse model_paths' syms
				     end
				   | NONE => (case SymbolTable.look (#outputs class_outline, sym) of 
						  SOME {syms, ...} => recurse model_paths syms
						| NONE => (SymbolSet.empty, model_paths)))

	    (* we need to create a list of symbols to ignore.  Since we're just looking for direct paths, 
	     we would like to filter out the symbols that based on states *)
	    val state_syms = SymbolSet.fromList (SymbolTable.listKeys init_exps)
	    (* and we can also filter out iterators from this list *)
	    val iter_syms = iterators

	    (* subtract the state_syms from the deps since we're only interested in inputs and intermediates at this point *)
	    val resulting_deps = (SymbolSet.difference (SymbolSet.difference (deps, state_syms),
							iter_syms))
	in
	    (resulting_deps, model_paths')
	end
	handle e => DynException.checkpoint "DOFOutline.symbol_to_algebraic_deps_through_instances" e

    (* class_to_path - given a class, create a listing of direct algebraic paths from inputs to outputs. This
     * function returns a new model_paths by updating the existing model paths *)
    and class_to_paths (model_outline, model_paths) classname =
	((*log_model_paths "In class_to_paths" model_paths;*)
	 case SymbolTable.look (model_paths, classname) of
	     (* we use the progress to know if we've already computed the paths.  If we haven't (Init), then we 
	      * do it now.  If we have already started, then that means that we've seen it again as part of an
	      * instance loop.  This is an error and we report that back.  If it's already completed, we just return 
	      * the existing model_paths structure *)
	     SOME (Init, empty_mapping) => 
	     let
		 val (_, class_outline as {outputs, inputs, ...}) = class_to_outline model_outline classname
		 (* take a list of all the outputs *)
		 val output_syms = SymbolTable.listKeys outputs
		 (* and inputs *)
		 val input_set = SymbolSet.fromList (SymbolTable.listKeys inputs)

		 (* update model_paths to be in progress *)
		 val model_paths = SymbolTable.enter (model_paths, classname, (InProgress, empty_mapping))

		 (* get all the algebraic paths *)
		 val (output_input_mapping, model_paths') = 
		     foldl
			 (fn(sym, (table, model_paths'))=>
			    let
				val (deps, model_paths'') = 
				    symbol_to_algebraic_deps_through_instances (model_outline, model_paths') class_outline SymbolSet.empty sym

				(* the only deps we're looking for are based on inputs *)
				val resulting_deps = SymbolSet.intersection (deps, input_set)
			    in
				(SymbolTable.enter (table, sym, resulting_deps), model_paths'')
			    end
			 )
			 (SymbolTable.empty, model_paths)
			 output_syms

	     in
		 SymbolTable.enter (model_paths', classname, (Complete, output_input_mapping))
	     end
	   | SOME (InProgress, _) => (error ("Instance loop detected in model " ^ (Symbol.name classname));
				      model_paths)
	   | SOME (Complete, _) => model_paths
	   | NONE => DynException.stdException (("No class found in model_paths with name " ^ (Symbol.name classname)),
						"DOFOutline.class_to_paths", Logger.INTERNAL))

    (* model_to_paths - create a path structure by iterating over each of the 
     *)
    fun model_to_paths (outline: model_outline) = 
	let
	    val _ = log_model_outline outline

	    (* create an initial data structure *)
	    val empty_paths = SymbolTable.map 
				  (fn(class_outline)=> (Init, SymbolTable.empty))
				  outline

	    (* iterator through each of the classes building up this path list *)
	    val paths = SymbolTable.foldli
			    (fn(classname, _, paths')=> class_to_paths (outline, paths') classname)
			    empty_paths
			    outline
	in
	    (*intermediate_paths_to_paths*) paths
	end
						       
    fun class_to_cycles (model_outline:model_outline, model_paths) (class_outline:class_outline) = 
	let
	    (* create one table to link the lhs and the rhs of all intermediates and 
	     * instances *)
	    (* start with the intermediates and produce a table linking the outputs with the inputs deps *)
	    val mapping : SymbolSet.set SymbolTable.table =
		SymbolTable.foldli
		    (fn(out, {syms, ...}, mapping') => SymbolTable.enter (mapping', out, syms))
		    SymbolTable.empty
		    (#intermediate_exps class_outline)
	    
	    (*val _ = log_mapping "class_to_cycles - intermediates" mapping*)

	    (* now, add each of the instances to this list - we should first make instance look like 
	     * intermediates by performing a transformation *)
	    val (intermediate_instances, model_paths') = 
		SymbolTable.foldli
		    (fn(out, entry, (table, model_paths'))=> 
		       let
			   val (intermediate_exp, model_paths'') = (instance_exp_to_intermediate_exp (model_outline, model_paths')) (out, entry)
		       in
			   (SymbolTable.enter (table, out, intermediate_exp), model_paths'')
		       end)
		    (SymbolTable.empty, model_paths)
		    (#instance_exps class_outline)

	    (* now add the instances *)
	    val mapping : SymbolSet.set SymbolTable.table =
		SymbolTable.foldli
		    (fn(out, {syms, ...}, mapping') => SymbolTable.enter (mapping', out, syms))
		    mapping
		    intermediate_instances

	    (*val _ = log_mapping "class_to_cycles - instances" mapping*)

	    (* and finally the outputs *)
	    fun update_output out = Symbol.symbol ("#output_" ^ (Symbol.name out))
	    val mapping : SymbolSet.set SymbolTable.table =
		SymbolTable.foldli
		    (fn(out, {syms, ...}, mapping') => SymbolTable.enter (mapping', update_output out, syms))
		    mapping
		    (#outputs class_outline)

	    (*val _ = log_mapping "class_to_cycles - outputs" mapping*)

	    (* create a list of leaf symbols that can be removed *)
	    val leaf_symbols = SymbolSet.empty
	    val leaf_symbols = SymbolSet.union (leaf_symbols, #iterators class_outline)
	    val leaf_symbols = SymbolSet.addList (leaf_symbols, SymbolTable.listKeys (#inputs class_outline))
	    val leaf_symbols = SymbolSet.addList (leaf_symbols, SymbolTable.listKeys (#init_exps class_outline))

	    (* map through the mapping, removing the leaf symbols *)
	    val mapping = SymbolTable.map (fn(set)=>SymbolSet.difference (set, leaf_symbols)) mapping
	    (*val _ = log_mapping "class_to_cycles - removed leaves" mapping*)


	    (* iteratively go through the mapping to find cycles *)
	    fun insert_deps mapping out =
		let
		    fun get v = (case SymbolTable.look (mapping, v) of
				     SOME e => e
				   | NONE => (error ("Undefined symbol " ^ (Symbol.name v) ^ " found during cycle detection");
					      SymbolSet.empty))
			handle e => DynException.checkpoint ("DOFOutline.class_to_cycles.insert_deps.get ["^(Symbol.name v)^"]") e

		    fun put (v,e) = SymbolTable.enter (mapping, v, e)
			handle e => DynException.checkpoint ("DOFOutline.class_to_cycles.insert_deps.put ["^(Symbol.name v)^"]") e

		    val syms = get out

		in
		    if SymbolSet.isEmpty syms then
			(NONE, mapping)
		    else
			let
			    val deps = SymbolSet.flatmap 
					    (fn(sym)=> get sym) 
					    (SymbolSet.listItems syms)
			    val syms' = SymbolSet.union(syms, deps)
			in
			    if SymbolSet.member (syms', out) then
				(* found a cycle if the output is a member of it's dependencies *)
				(SOME syms', mapping)
			    else if SymbolSet.equal (syms, syms') then
				(* bottomed out *)
				(NONE, mapping)
			    else
				let
				    val mapping' = put (out, syms')
				in
				    insert_deps mapping' out
				end
			end
		end
		handle e => DynException.checkpoint "DOFOutline.class_to_cycles.insert_deps" e
		
	    (* go through each of the classes to find cycles - aggregate the results into one structure *)
	    fun aggregate_cycles (out, cycles', mapping) =
		case (cycles', insert_deps mapping out) of
		    (SOME {classname, deps}, (SOME deps', mapping')) => (SOME {classname=classname, deps=SymbolSet.union(deps,deps')}, mapping')
		  | (NONE, (SOME deps', mapping')) => (SOME {classname=(#name class_outline), deps=deps'}, mapping')
		  | (_, (NONE, mapping')) => (cycles', mapping')

	    val (cycles,mapping') = 
		foldl
		    (fn(out, (cycles', mapping'))=> (aggregate_cycles (out, cycles', mapping')))
		    (NONE, mapping)
		    (SymbolTable.listKeys mapping)

	    (*val _ = log_mapping ("Final mapping (class="^(Symbol.name (#name class_outline))^")") mapping'*)

	    val cycles = case cycles of
			     SOME {classname, deps} => SOME {classname=classname, deps=SymbolSet.listItems deps}
			   | NONE => NONE
						
	in
	    cycles
	end
	handle e => DynException.checkpoint ("DOFOutline.class_to_cycles [class="^(Symbol.name (#name class_outline))^"]") e

    fun model_to_cycles (outline: model_outline) = 
	let
	    (* compute the direct input to output paths of each class *)
	    val model_paths = model_to_paths outline
			      
	    (*val _ = log_model_paths "Initial generation" model_paths*)

	    (* if errors are generated, better exit now.. *)
	    val _ = DynException.checkToProceed()

	    (* seek out all the classes with cycles and return them to processed at a higher function *)
	    val classes_with_cycles = List.mapPartial (fn(class_outline)=> class_to_cycles (outline, model_paths) class_outline) (SymbolTable.listItems outline)
	in
	    classes_with_cycles
	end
	handle e => DynException.checkpoint "DOFOutline.model_to_cycles" e
    end
end
