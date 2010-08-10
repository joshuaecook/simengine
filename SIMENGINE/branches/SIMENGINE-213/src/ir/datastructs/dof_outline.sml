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
    type instance_dependencies = {instclass: Symbol.symbol, syms: SymbolSet.set, iter_syms: SymbolSet.set}
    type instance_dependency_table = instance_dependencies SymbolTable.table

    (* Each class outline mirrors the original class, but everything is a lot more explicit. 
     * From this structure, it's very easy to determine the states, or the state inits, or 
     * traverse only from outputs through intermediates until reaching states or inputs.  *)
    type class_outline = {name: Symbol.symbol,
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
    val class_to_outline : DOF.class -> class_outline
    val model_to_outline : DOF.model -> model_outline
					
    (* traverse through a class and find all the dependent symbols or iterators *)
    val symbol_to_deps : class_outline -> Symbol.symbol -> SymbolSet.set
    val symbol_to_iters : class_outline -> Symbol.symbol -> SymbolSet.set

end
structure DOFOutline : DOFOUTLINE =
struct

    type dependencies = {syms: SymbolSet.set, iter_syms: SymbolSet.set}
    type dependency_table = dependencies SymbolTable.table

    type instance_dependencies = {instclass: Symbol.symbol, syms: SymbolSet.set, iter_syms: SymbolSet.set}
    type instance_dependency_table = instance_dependencies SymbolTable.table

    type class_outline = {name: Symbol.symbol,
			  inputs: dependency_table,
			  init_exps: dependency_table,
			  intermediate_exps: dependency_table,
			  state_exps: dependency_table,
			  instance_exps: instance_dependency_table,
			  outputs: dependency_table}

    type model_outline = class_outline SymbolTable.table

    fun exp_to_symbols_and_iterators exp =
	let
	    val terms = ExpProcess.exp2termsymbols exp
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
    fun class_to_outline (class:DOF.class) =
	let
	    (* few helper calls *)
	    val empty = SymbolTable.empty
	    val emptyset = SymbolSet.empty
	    val singleton = SymbolSet.singleton

	    (* first, pull out the relevent pieces *)
	    val name = #name class
	    val inputs = !(#inputs class)
	    val exps = !(#exps class)
	    val (init_exps, rest) = List.partition ExpProcess.isInitialConditionEq exps
	    val (instance_exps, rest) = List.partition ExpProcess.isInstanceEq rest
	    val (state_exps, intermediate_exps) = List.partition ExpProcess.isStateEq rest
	    val outputs = !(#outputs class)

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
	    val instance_outline = 
		foldl
		    (fn(inst, table)=>
		       let
			   val {classname, outargs, ...} = ExpProcess.deconstructInst inst
			   val (syms, iter_syms) = exp_to_symbols_and_iterators (ExpProcess.rhs inst)
								    
			   val table' = foldl
					    (fn(term, table')=>
					       let
						   val sym = Term.sym2curname term
						   val iter_syms' = case TermProcess.symbol2temporaliterator term of
									SOME (iter_sym, _) => SymbolSet.add (iter_syms, iter_sym)
								      | NONE => iter_syms
						   val entry' = {instclass=classname, syms=syms, iter_syms=iter_syms'}
					       in
						   SymbolTable.enter (table', sym, entry')
					       end)
					    table
					    outargs
		       in
			   table'
		       end)
		    empty
		    instance_exps

	in
	    {name=name,
	     inputs=inputs_outline,
	     init_exps=init_exps_outline,
	     intermediate_exps=intermediate_exps_outline,
	     state_exps=state_exps_outline,
	     instance_exps=instance_outline,
	     outputs=outputs_outline}
	end
	handle e => DynException.checkpoint "DOFOutline.class_to_outline" e

    fun model_to_outline ((classes, _, _):DOF.model) = 
	foldl
	    (fn(c as {name,...}, table)=>
	       SymbolTable.enter (table, name, class_to_outline c))
	    SymbolTable.empty
	    classes
	handle e => DynException.checkpoint "DOFOutline.model_to_outline" e

    fun inTable (table, sym) = 
	case SymbolTable.look (table, sym) of
	    SOME _ => true
	  | NONE => false

    fun symbol_is_leaf (outline:class_outline) sym =
	inTable (#inputs outline, sym) orelse
	inTable (#init_exps outline, sym)

    fun symbol_is_intermediate (outline:class_outline) sym = 
	inTable (#intermediate_exps outline, sym) orelse
	inTable (#instance_exps outline, sym) orelse
	inTable (#outputs outline, sym)

    (* Take a symbol, find all the dependencies *)
    fun symbol_to_deps (outline:class_outline) sym =
	let
	    val {name, inputs, 
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
	    val {name, inputs, 
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
						       SOME {syms,iter_syms,instclass} => SymbolSet.union (iter_syms, recurse syms)
						     | NONE => (case SymbolTable.look (#outputs outline, sym) of 
								    SOME {syms,iter_syms} => SymbolSet.union (iter_syms, recurse syms)
								  | NONE => SymbolSet.empty))))
	end
	handle e => DynException.checkpoint "DOFOutline.symbol_to_iters" e

end
