signature CPARALLELWRITER =
sig

datatype status =
	 SUCCESS 
       | FAILURE of string

(* one entry point to build a C back-end *)
val buildC : (Symbol.symbol * ShardedModel.shardedModel) -> status

end
structure CParallelWriter : CPARALLELWRITER =
struct

datatype status =
	 SUCCESS 
       | FAILURE of string

fun $ line = Layout.str line
fun SUB text = Layout.indent (Layout.align text, 2)

(* Parenthesises and separates a list of layouts. *)
fun series (start, finish, sep) layouts =
    Layout.seq [Layout.str start, 
		Layout.mayAlign (Layout.separateRight (layouts, sep)), 
		Layout.str finish]

(* Quotes a C string verbatim. *)
fun quote str = 
    Layout.seq [Layout.str "\"", 
		Layout.str (String.toCString str),
		Layout.str "\""]

(* Terminates a C statement with a semicolon. *)
fun stmt layout =
    Layout.seq [layout, Layout.str ";"]

fun statements layouts =
    Layout.align (map stmt layouts)

fun assign (lhs, rhs) =
    stmt (Layout.seq [lhs, Layout.str " = ", rhs])



exception InternalError

val i2s = Util.i2s
val r2s = Util.r2s
val e2s = ExpPrinter.exp2str
val e2ps = ExpPrinter.exp2prettystr

fun cstring str = "\"" ^ (String.toCString str) ^ "\""
fun inc x = 1 + x

(* Indicates whether the class of a given instance satisfies a test.
 * Nb Presumes a CurrentModel context. *)
fun test_instance_class test instance =
    let val {classname, ...} = ExpProcess.deconstructInst instance
	val class = CurrentModel.classname2class classname
    in test class
    end

(* Indicates whether an output contains any term in its condition or contents
 * which satisfies a given predicate. *)
fun output_contains_term test output =
    let val (condition, contents) = (DOF.Output.condition output, DOF.Output.contents output)
	val terms = Util.flatmap ExpProcess.exp2termsymbols (condition :: contents)
    in List.exists test terms
    end

(* Indicates whether a given class or any of its instances 
 * reads any states associated with a given iterator.
 * Nb Presumes a CurrentModel context. *)
fun reads_iterator (_, DOF.UPDATE iter_sym) class =
    let val iter = CurrentModel.itersym2iter iter_sym
    in reads_iterator iter class end

  | reads_iterator iter class =
    let val {exps, outputs, ...} = class
    in List.exists (output_contains_term (term_reads_iterator iter)) (! outputs) orelse
       List.exists (term_reads_iterator iter) (Util.flatmap ExpProcess.exp2termsymbols (! exps)) orelse
       List.exists (test_instance_class (reads_iterator iter)) (List.filter (fn exp => ExpProcess.isInstanceEq exp orelse ExpProcess.isOutputEq exp) (! exps))
    end

and term_reads_iterator iter (Exp.SYMBOL (name, props)) =
    let val (iter_sym, _) = iter
    in case Property.getScope props
	of Property.READSTATE iter_sym' => iter_sym = iter_sym'
	 | Property.READSYSTEMSTATE iter_sym' => iter_sym = iter_sym'
	 | _ => false
    end
  | term_reads_iterator _ _ = false


(* Indicates whether a given class or any of its instances
 * writes any states associated with a given iterator.
 * Nb Presumes a CurrentModel context. *)
fun writes_iterator (_, DOF.UPDATE iter_sym) class =
    let val iter = CurrentModel.itersym2iter iter_sym
    in writes_iterator iter class end

  | writes_iterator iter class =
    let val {exps, name, ...} = class
	val result = List.exists (term_writes_iterator iter) (Util.flatmap ExpProcess.exp2termsymbols (! exps)) orelse
		     List.exists (test_instance_class (writes_iterator iter)) (List.filter (fn exp => ExpProcess.isInstanceEq exp orelse ExpProcess.isOutputEq exp) (! exps))
	(*val _ = Util.log ("Testing writes_iterator for class="^(Symbol.name name)^", iterator=" ^ (Symbol.name (#1 iter)) ^"  -> " ^ (Util.b2s result))*)

    in
	result
    end

and term_writes_iterator iter (Exp.SYMBOL (_, props)) =
    let val (iter_sym, _) = iter
    in case Property.getScope props
	of Property.WRITESTATE iter_sym' => iter_sym = iter_sym'
	 | _ => false
    end
  | term_writes_iterator _ _ = false


(* Indicates whether a given class or any of its instances 
 * reads any states from the system scope.
 * Nb Presumes a CurrentModel context. *)
fun reads_system class =
    let 
	val {exps, outputs, ...} = class
	val p = fn x => Term.isReadSystemState x orelse Term.isReadSystemIterator x
    in List.exists (output_contains_term p) (! outputs) orelse
       List.exists p (Util.flatmap ExpProcess.exp2termsymbols (! exps)) orelse
       List.exists (test_instance_class reads_system) (List.filter (fn exp => ExpProcess.isInstanceEq exp orelse ExpProcess.isOutputEq exp) (! exps))
    end

(* Indicates whether a given class or any of its instances 
 * has states associated with a given iterator.
 * Nb Presumes a CurrentModel context. *)
and has_states iter class = 
    ClassProcess.class2statesizebyiterator iter class > 0
    handle e => DynException.checkpoint ("CParallelWriter.has_states [iter="^(Symbol.name (#1 iter))^"]") e

(* Indicates whether a given class contains any instances 
 * involving states associated with a given iterator.
 * Nb Presumes a CurrentModel context. *)
and has_instance_states iter class = 
    let val (iter_sym, _) = iter
	val instances = ClassProcess.class2instancesbyiterator iter_sym class
    in List.exists (instance_has_states (has_states iter)) instances
    end
    handle e => DynException.checkpoint "CParallelWriter.has_instance_states" e

(* Indicates whether an instance invocation involves states associated with a given iterator.
 * Nb Presumes a CurrentModel context. *)
and instance_has_states test instance =
    let val {classname, ...} = ExpProcess.deconstructInst instance
	val class = CurrentModel.classname2class classname
    in test class
    end

fun searchExpressionsDepthFirst p (class: DOF.class) =
    let
	fun dfs nil = NONE
	  | dfs (exp::exps) =
	    if ExpProcess.isInstanceEq exp then
		let val {classname, ...} = ExpProcess.deconstructInst exp
		    val class' = CurrentModel.classname2class classname
		in
		    case searchExpressionsDepthFirst p class'
		     of SOME exp => SOME exp
		      | NONE => dfs exps
		end
	    else if p exp then
		SOME exp
	    else
		dfs exps
    in
	dfs (! (#exps class))
    end


fun hasInitialValueEquation f class =
    isSome (searchExpressionsDepthFirst
		(fn exp => ExpProcess.isInitialConditionEq exp andalso f exp)
		class)

(* ====================  HEADER  ==================== *)

fun header (class_name) = 
    [$("// C Execution Engine for top-level model: " ^ class_name),
     $("// " ^ Globals.copyright),
     $("")]

(* FIXME the gpu-related code herein is likely not correct. *)
fun init_solver_props top_name shardedModel (iterators_with_solvers, algebraic_iterators) =
    let
        fun copy_states iter_sym =
            let
		val model = ShardedModel.toModel shardedModel iter_sym
                val itername = (Symbol.name iter_sym)
		val num_states = CurrentModel.withModel model (fn _ => ModelProcess.model2statesize model)
            in
	        if 0 < num_states then
                    $("memcpy(&system_states_ext[modelid].states_"^itername^", &system_states_int->states_"^itername^"[modelid], "^(i2s num_states)^"*sizeof(CDATAFORMAT));")
 		else
                    $("")
            end

	fun init_props iter_sym =
	    let
		val model as (_, instance, _) = ShardedModel.toModel shardedModel iter_sym
		val topClassName = 
		    case #name instance 
		     of SOME x => x | NONE => #classname instance		

		val iterator = ShardedModel.toIterator shardedModel iter_sym
		fun progs () =
		    let

			val solverparams = (fn(_,itertype) => case itertype of
								  DOF.CONTINUOUS solver => (Solver.solver2params solver)
								| DOF.DISCRETE {sample_period} => [("timestep", Util.r2s sample_period)]
								| DOF.IMMEDIATE => []
								| _ => [("ERROR", "Bogus iterator")]) iterator
			val solveropts = (fn(_,itertype) => case itertype of
								  DOF.CONTINUOUS solver => (Solver.solver2opts solver)
								| DOF.DISCRETE _ => []
								| DOF.IMMEDIATE => []
								| _ => [("ERROR", "Bogus iterator")]) iterator
			val (itersym, itertype) = iterator
			val itername = Util.removePrefix (Symbol.name itersym)
			val solvername = ((fn(_,itertype) => case itertype of
								 DOF.CONTINUOUS solver => (Solver.solver2name solver)
							       | DOF.DISCRETE _ => "discrete"
							       | DOF.IMMEDIATE => "immediate"
							       | _ => "ERROR Bogus iterator") iterator)
			val solvernameCaps = String.map Char.toUpper solvername
			val num_states = ModelProcess.model2statesize model 

			val num_algebraic_states =
			    let 
				val iters = List.filter (fn it =>
							    case ShardedModel.toIterator shardedModel it
							     of (_, DOF.ALGEBRAIC (_, iter_sym')) => iter_sym' = iter_sym
							      | _ => false)
							algebraic_iterators

				fun numIteratorStates it =
				    let val model = ShardedModel.toModel shardedModel it
				    in CurrentModel.withModel model (fn _ => ModelProcess.model2statesize model)
				    end
			    in
				Util.sum (map numIteratorStates iters)
			    end

			val requiresMatrix = case itertype of
				             DOF.CONTINUOUS (Solver.LINEAR_BACKWARD_EULER _) => true
				           | _ => false
			val c = CurrentModel.classname2class topClassName
			val matrix_exps = ClassProcess.symbol2exps c (Symbol.symbol "#M")
			(* band size is the number of columns in the banded matrix, set to zero when it is a dense matrix *)
			val bandsize = case matrix_exps of (* ignored for dense solver *)
					   [exp] => 
					   if ExpProcess.isMatrixEq exp then
					       let
						   val m = Container.expMatrixToMatrix (ExpProcess.rhs exp)
					       in
						   case !m of
						       Matrix.DENSE _ => 0 (* set to zero when it is dense *)
						     | Matrix.BANDED {ncols,...} => List.length (Matrix.toPaddedBands m)
					       end
					   else
					       0
					 | _ => 0
			(* This makes a huge assumption, that the first iterator in the list will also be the first entry in the statedata structure *)
			val first_algebraic_iterator = if 0 < num_algebraic_states then
							   case (hd (ModelProcess.algebraicIterators itersym)) of
							       (itername,_) => Symbol.name itername
						       else ""
		    in
			(if (List.null solveropts) then
			     nil
			 else
			     [$("{"),
			      $(solvername ^"_opts *opts = ("^solvername^"_opts*)&props[ITERATOR_"^itername^"].opts;"),
			      SUB(map(fn(opt,oval) => $("opts->"^opt^" = "^oval^";")) solveropts),
			      $("}")]) @
			(map (fn(prop,pval) => $("props[ITERATOR_"^itername^"]."^prop^" = "^pval^";")) solverparams) @
			[(* HACK BEGIN *)
			 $("if(global_timestep) props[ITERATOR_"^itername^"].timestep = global_timestep;"),
			 (* HACK END *)
			 $("props[ITERATOR_"^itername^"].starttime = starttime;"),
			 $("props[ITERATOR_"^itername^"].stoptime = stoptime;"),
			 $("props[ITERATOR_"^itername^"].system_states = system_ptrs;"),
			 $("props[ITERATOR_"^itername^"].time = (CDATAFORMAT*)malloc(PARALLEL_MODELS*sizeof(CDATAFORMAT));"),
			 $("props[ITERATOR_"^itername^"].next_time = (CDATAFORMAT*)malloc(PARALLEL_MODELS*sizeof(CDATAFORMAT));"),
			 $("props[ITERATOR_"^itername^"].count = NULL; // Allocated by discrete solver only, must be NULL otherwise"),
			 $("props[ITERATOR_"^itername^"].last_iteration = (int*)malloc(PARALLEL_MODELS*sizeof(int));"),
			 $("memset(props[ITERATOR_"^itername^"].last_iteration, 0, PARALLEL_MODELS * sizeof(int));"),
			 $("// Initial values moved to model_states first time through the exec"),
			 $("props[ITERATOR_"^itername^"].model_states = " ^
			   (if 0 < num_states then
				"(CDATAFORMAT*)(&system_states_int->states_"^(Symbol.name itersym)^");"
			    else if 0 < num_algebraic_states then
				"(CDATAFORMAT*)(&system_states_int->states_"^first_algebraic_iterator^");"
			    else
				"NULL;")),
			 $("props[ITERATOR_"^itername^"].next_states = " ^
			   (if 0 < num_states then
				"(CDATAFORMAT*)(&system_states_next->states_"^(Symbol.name itersym)^");"
			    else if 0 < num_algebraic_states then
				"(CDATAFORMAT*)(&system_states_next->states_"^first_algebraic_iterator^");"
			    else
				"NULL;")),
			 $("props[ITERATOR_"^itername^"].solver = " ^ solvernameCaps ^ ";"),
			 $("props[ITERATOR_"^itername^"].iterator = ITERATOR_" ^ itername ^";")] @
			[$("props[ITERATOR_"^itername^"].inputsize = NUM_INPUTS;"),
			 $("props[ITERATOR_"^itername^"].statesize = " ^ (Util.i2s num_states) ^ ";"),
			 $("props[ITERATOR_"^itername^"].algebraic_statesize = " ^ (Util.i2s num_algebraic_states) ^ ";"),
			 $("props[ITERATOR_"^itername^"].outputsize = outputsize;"),
			 $("props[ITERATOR_"^itername^"].num_models = num_models;"),
			 $("props[ITERATOR_"^itername^"].modelid_offset = modelid_offset;"),
			 $("props[ITERATOR_"^itername^"].od = od;"),
			 $("props[ITERATOR_"^itername^"].running = (int*)malloc(PARALLEL_MODELS*sizeof(int));"),
			 $("")]@ 
			(case itertype of
			     DOF.CONTINUOUS _ =>
			     $("system_ptrs->"^(Symbol.name itersym)^" = props[ITERATOR_"^itername^"].time;")
			   | DOF.DISCRETE _ =>
			     $("system_ptrs->"^(Symbol.name itersym)^" = props[ITERATOR_"^itername^"].count;")
			   | _ =>
			     $("// Ignored "^itername)) ::
			(if 0 < num_states then
			     [$("system_ptrs->states_"^(Symbol.name itersym)^" = system_states_int->states_"^(Symbol.name itersym)^";"),
			      $("system_ptrs->states_"^(Symbol.name itersym)^"_next = system_states_next->states_"^(Symbol.name itersym)^";"),
                              $("#if !defined TARGET_GPU"),
                              $("// Translate structure arrangement from external to internal formatting"),
                              $("for(modelid=0;modelid<num_models;modelid++){"),
                              SUB[$("memcpy(&system_states_int->states_"^(Symbol.name itersym)^"[modelid], " ^
				    "&system_states_ext[modelid].states_"^(Symbol.name itersym)^", " ^
				    "props[ITERATOR_"^itername^"].statesize * sizeof(CDATAFORMAT));")],
                              $("}"),
                              $("#endif")]
			 else
			     nil) @
			(let fun initAlgebraicSystemPointers (iterName, iterType) =
				 let 
				     val (_, instance, _) = ShardedModel.toModel shardedModel iterName
				     val tcn = 
					 case #name instance 
					  of SOME x => x | NONE => #classname instance		
				     val cname = Symbol.name iterName
				 in
				     [$("system_ptrs->states_" ^ cname ^ " = system_states_int->states_" ^ cname ^ ";"),
				      $("system_ptrs->states_" ^ cname ^ "_next = system_states_next->states_" ^ cname ^ ";"),
				      $("#if !defined TARGET_GPU"),
				      $("// Translate structure arrangement from external to internal formatting"),
				      $("for(modelid=0;modelid<num_models;modelid++){"),
				      SUB[$("memcpy(&system_states_int->states_" ^ cname ^ "[modelid], " ^
					    "&system_states_ext[modelid].states_" ^ cname ^ ", " ^
					    "sizeof(statedata_" ^ (Symbol.name tcn) ^ "));")],
				      $("}"),
				      $("#endif")]
				 end
			 in
			     Util.flatmap initAlgebraicSystemPointers (ModelProcess.algebraicIterators itersym)
			 end)
		    end
		    handle e => DynException.checkpoint "CParallelWriter.init_solver_props.init_props.progs" e
	    in
		CurrentModel.withModel model progs
	    end
	    handle e => DynException.checkpoint "CParallelWriter.init_solver_props.init_props" e

	    
	fun gpu_init_constants iter_sym =
	    let
		val (_,iter_type) = ShardedModel.toIterator shardedModel iter_sym
		val iter_name = Symbol.name iter_sym
	    in
		case iter_type
		 of DOF.CONTINUOUS (Solver.LINEAR_BACKWARD_EULER _) =>
		    Layout.align 
			[$ "{",
			 SUB [$ ("linearbackwardeuler_opts *opts = (linearbackwardeuler_opts*)&(props[ITERATOR_"^(Util.removePrefix iter_name)^"].opts);"),
			      $ "switch(opts->lsolver){",
			      $ "case LSOLVER_DENSE:",
			      SUB [$ ("constants_size = props[ITERATOR_"^(Util.removePrefix iter_name)^"].statesize * props[ITERATOR_"^(Util.removePrefix iter_name)^"].statesize * sizeof(CDATAFORMAT);"),
				   $ "break;"],
			      $ "case LSOLVER_BANDED:",
			      SUB [$ ("constants_size = (opts->upperhalfbw + opts->lowerhalfbw + 1) * props[ITERATOR_"^(Util.removePrefix iter_name)^"].statesize * sizeof(CDATAFORMAT);"),
				   $ "break;"],
			      $ "}",
			      $ ("cutilSafeCall(cudaGetSymbolAddress((void **)&g_constants, device_matrix_constants_"^iter_name^"));"),
			      $ ("cutilSafeCall(cudaMemcpy(g_constants, host_matrix_constants_"^iter_name^", constants_size, cudaMemcpyHostToDevice));")],
			 $ "}"]
		  | _ => Layout.empty
	    end

	fun gpu_init_props iter_sym =
	    let val model as (_, instance, _) = ShardedModel.toModel shardedModel iter_sym
		val iterator = ShardedModel.toIterator shardedModel iter_sym

		val topClassName = 
		    case #name instance 
		     of SOME x => x | NONE => #classname instance		
	       
		val topClassBaseName = topClassName

		fun progs () = 
		    let val (_, itertype) = iterator
			val itername = (Symbol.name iter_sym)

			val iterator_value_ptr = 
			    case itertype of
				DOF.CONTINUOUS _ =>
				$("tmp_system->"^itername^" = tmp_props[ITERATOR_"^(Util.removePrefix itername)^"].time;")
			      | DOF.DISCRETE _ =>
				$("tmp_system->"^itername^" = tmp_props[ITERATOR_"^(Util.removePrefix itername)^"].count;")
			      | DOF.IMMEDIATE =>
				$("// no iterator value for 'always'")
			      | _ =>
				$("#error BOGUS ITERATOR NOT FILTERED")

			val iterator_states_ptrs =
			    if 0 < ModelProcess.model2statesize (CurrentModel.getCurrentModel ()) then
				[$("tmp_system->states_"^(itername)^" = (statedata_"^(Symbol.name topClassName)^" * )(tmp_props[ITERATOR_"^(Util.removePrefix itername)^"].model_states);"),
				 $("tmp_system->states_"^(itername)^"_next = (statedata_"^(Symbol.name topClassName)^" * )(tmp_props[ITERATOR_"^(Util.removePrefix itername)^"].next_states);")]
			    else nil

			val my_algebraic_iterators =
			    List.filter (fn it =>
					    case ShardedModel.toIterator shardedModel it
					     of (_, DOF.ALGEBRAIC (_, iter_sym')) => iter_sym' = iter_sym
					      | _ => false)
					algebraic_iterators

			val iterator_algebraic_states_ptrs =
			    let
				fun numIteratorStates it =
				    let val model = ShardedModel.toModel shardedModel it
				    in CurrentModel.withModel model (fn _ => ModelProcess.model2statesize model)
				    end

				fun iteratorStatesPtr it =
				    let val model as (_, instance, _) = ShardedModel.toModel shardedModel it
					val tcn = 
					    case #name instance 
					     of SOME x => x | NONE => #classname instance		
					val tcbn = tcn
				    in
					[$("tmp_system->states_"^(Symbol.name it)^" = (statedata_"^(Symbol.name tcn)^" *)" ^
					  "(tmp_props[ITERATOR_"^(Util.removePrefix itername)^"].model_states + algebraic_offset * PARALLEL_MODELS);"),
					 $("tmp_system->states_"^(Symbol.name it)^"_next = (statedata_"^(Symbol.name tcn)^" *)" ^
					   "(tmp_props[ITERATOR_"^(Util.removePrefix itername)^"].next_states + algebraic_offset * PARALLEL_MODELS);"),
					 $("algebraic_offset += " ^ (Int.toString (numIteratorStates it)) ^ ";")]
				    end
			    in
				Util.flatmap iteratorStatesPtr my_algebraic_iterators
			    end
			    
		    in
			$("algebraic_offset = tmp_props[ITERATOR_"^(Util.removePrefix itername)^"].statesize;") ::
			iterator_value_ptr ::
			iterator_states_ptrs @
			iterator_algebraic_states_ptrs
		    end
	    in
		CurrentModel.withModel model progs
	    end

	    val total_system_states = ShardedModel.statesize shardedModel
    in
	[
	 $("#if defined TARGET_GPU"),
	 $("void gpu_init_system_states_pointers (solver_props *tmp_props, top_systemstatedata *tmp_system) {"),
	 SUB[$("ptrdiff_t algebraic_offset;")],
	 SUB(Util.flatmap gpu_init_props iterators_with_solvers),
	 $("}"),
	 $("void gpu_init_constants (solver_props *props) {"),
	 SUB ([$ "CDATAFORMAT *g_constants;",
	       $ "size_t constants_size;"] @
	      map gpu_init_constants iterators_with_solvers),
	 $("}"),
	 $("#endif"),
	 $(""),
	 $("solver_props *init_solver_props(CDATAFORMAT starttime, CDATAFORMAT stoptime, unsigned int num_models, CDATAFORMAT *model_states, unsigned int modelid_offset, unsigned int gridsize, unsigned int blocksize){"),
	 $("top_systemstatedata *system_ptrs = (top_systemstatedata *)malloc(sizeof(top_systemstatedata));"),
	 SUB((if 0 < total_system_states then
		  [$("systemstatedata_external *system_states_ext = (systemstatedata_external*)model_states;"),
		   $("#if defined TARGET_GPU"),
		   $("systemstatedata_external *system_states_int = (systemstatedata_external*)model_states;"),
		   $("systemstatedata_external *system_states_next = (systemstatedata_external*)malloc(sizeof(systemstatedata_external));"),
		   $("#else"),
		   $("systemstatedata_internal *system_states_int = (systemstatedata_internal*)malloc(sizeof(systemstatedata_internal));"),
		   $("systemstatedata_internal *system_states_next = (systemstatedata_internal*)malloc(sizeof(systemstatedata_internal));"),
		   $("#endif")]

	       else
		   [$("void *system_states_ext = NULL;"),
		    $("void *system_states_int = NULL;"),
		    $("void *system_states_next = NULL;")]) @
	      [$("solver_props *props = (solver_props * )malloc(NUM_ITERATORS*sizeof(solver_props));"),
	      $("#if NUM_OUTPUTS > 0"),
	      $("output_data *od = (output_data*)malloc(PARALLEL_MODELS*sizeof(output_data));"),
	      $("unsigned int outputsize = sizeof(output_data)/sizeof(CDATAFORMAT);"),
	      $("#else"),
	      $("void *od = NULL;"),
	      $("unsigned int outputsize = 0;"),
	      $("#endif"),
	      $("unsigned int i, modelid;")] @
	     (Util.flatmap init_props iterators_with_solvers) @
	     [$(""),
	      $("assert(NUM_ITERATORS);"),
	      $("for(i=0;i<NUM_ITERATORS;i++){"),
	      SUB[$("Iterator iter = ITERATORS[i];"),
		  $("props[iter].gridsize = gridsize;"),
		  $("props[iter].blocksize = blocksize;"),
		  $("// Initialize all time vectors"),
		  SUB[$("for(modelid=0;modelid<num_models;modelid++){"),
		      $("props[iter].time[modelid] = starttime;"),
		      $("props[iter].next_time[modelid] = starttime;")],
		  $("}")],
	      $("}")] @
	     (if 0 < total_system_states then
		   [$("#if defined TARGET_GPU"),
		    $("memcpy(system_states_next, system_states_int, sizeof(systemstatedata_external));"),
		    $("#else"),
		    $("memcpy(system_states_next, system_states_int, sizeof(systemstatedata_internal));"),
		    $("#endif")]
	       else
		   nil) @
	     [$("return props;")]),
	 $("}"),
	 $(""),
	 $("void free_solver_props(solver_props* props, CDATAFORMAT* model_states){"),
	 SUB([$("unsigned int i, modelid;"),
              $("assert(props);"),
              $("")] @
	     (if 0 < total_system_states then
		  [$("systemstatedata_external *system_states_ext = (systemstatedata_external*)model_states;"),
		   $("#ifdef TARGET_GPU"),
		   $("systemstatedata_external *system_states_next = NULL;"),
		   $("for(i=0;i<NUM_ITERATORS;i++){"),
		   SUB[$("if(props[i].statesize + props[i].algebraic_statesize > 0){"),
                       SUB[$("system_states_next = (systemstatedata_external*)props[i].next_states;"),
			   $("break;")],
                       $("}")],
		   $("}"),
		   $("#else"),
		   $("systemstatedata_internal *system_states_int = NULL;"),
		   $("systemstatedata_internal *system_states_next = NULL;"),
		   $("for(i=0;i<NUM_ITERATORS;i++){"),
		   SUB[$("if(props[i].statesize + props[i].algebraic_statesize > 0){"),
                       SUB[$("system_states_int = (systemstatedata_internal*)props[i].model_states;"),
			   $("system_states_next = (systemstatedata_internal*)props[i].next_states;"),
			   $("break;")],
                       $("}")],
		   $("}"),
		   $("#endif"),
		   $(""),
		   $("#if !defined TARGET_GPU && NUM_STATES > 0"),
		   $("// Translate structure arrangement from internal back to external formatting"),
		   $("for(modelid=0;modelid<props->num_models;modelid++){"),
		   SUB(map copy_states iterators_with_solvers),
		   SUB(map copy_states algebraic_iterators),
		   $("}"),
		   $("#endif"),
		   $("#if !defined TARGET_GPU"),
		   $("if (system_states_int) free(system_states_int);"),
		   $("#endif"),
		   $("if (system_states_next) free(system_states_next);"),
		   $("")]
	      else
	      nil) @
	 [$("for(i=0;i<NUM_ITERATORS;i++){"),
	  SUB[$("Iterator iter = ITERATORS[i];"),
	      $("if (props[iter].time) free(props[iter].time);"),
	      $("if (props[iter].next_time) free(props[iter].next_time);"),
	      $("if (props[iter].running) free(props[iter].running);"),
	      $("if (props[iter].last_iteration) free(props[iter].last_iteration);")],
	  $("}"),
	  $("if (props[0].od) free(props[0].od);"),
	  $("if (props[0].system_states) free(props[0].system_states);"),
	  $("free(props);"),
	 $("}"),
	 $("")])]
    end
    handle e => DynException.checkpoint "CParallelWriter.init_solver_props" e

fun simengine_interface class_name (shardedModel as (shards,sysprops) : ShardedModel.shardedModel) outputIterators =
    let
	(*val top_class = CurrentModel.withModel origModel (fn()=>CurrentModel.classname2class (#classname inst))*)
	val iterator_names = List.mapPartial (fn(iter_sym)=>case ShardedModel.toIterator shardedModel iter_sym of
								(_, DOF.ALGEBRAIC _) => NONE
							      | _ => SOME (Util.removePrefix (Symbol.name iter_sym)))
					     outputIterators
	val solver_names = List.mapPartial (fn(iter_sym)=>case ShardedModel.toIterator shardedModel iter_sym of
							      (_,DOF.CONTINUOUS s) => SOME (Solver.solver2name s)
							    | (_,DOF.DISCRETE _) => SOME "discrete"
							    | (_,DOF.IMMEDIATE) => SOME "immediate"
							    | _ => NONE) 
					   outputIterators
	fun init_condition2pair iter_sym basestr exp =
	    let val term = ExpProcess.exp2term (ExpProcess.lhs exp)
		val rhs = ExpProcess.rhs exp
	    in if Term.isInitialValue term iter_sym then
		   SOME ((if "" = basestr then "" else basestr ^ ".") ^ (Term.sym2name term), rhs)
	       else NONE
	    end



	(* State names and initial value pairs are returned in a sorted order,
	 * with the class' own states listed first in the order of the state initial values,
	 * followed by the class' instance's states in order of the instance name. *)
	fun findStatesInitValues iter_sym basestr (class:DOF.class) = 
	    let
		(*val _ = Util.log ("In " ^ (Symbol.name (#name class)))*)
		val exps = #exps class

		val init_conditions = List.filter ExpProcess.isInitialConditionEq (!exps)

		val instances = ClassProcess.class2instances class
		val class_inst_pairs = 
		    let 
			fun sameInstanceName ((c1,i1),(c2,i2)) = i1 = i2
			fun classAndInstanceName eqn =
			    let val {classname, ...} = ExpProcess.deconstructInst eqn
				(*val _ = Util.log (" -> Found classname = " ^ (Symbol.name classname))*)
			    in 
				(classname, ExpProcess.instOrigInstName eqn)
			    end
		    in
			(*Util.uniquify_by_fun sameInstanceName*) (map classAndInstanceName instances)
		    end

		val stateInits = List.mapPartial (init_condition2pair iter_sym basestr) init_conditions
		(*val _ = app (fn(sym,_)=> Util.log (" -> Found state: " ^ (sym))) stateInits*)
		val instanceStateInits = StdFun.flatmap (findInstanceStatesInitValues iter_sym) class_inst_pairs

		val compare = fn ((x,_), (y,_)) => String.compare (x, y)
	    in
		stateInits
		@ ((*Sorting.sorted compare*) instanceStateInits)
	    end
	    handle e => DynException.checkpoint "CParallelWriter.simengine_interface.findStatesInitValues" e

	and findInstanceStatesInitValues iter_sym (classname, instname) =
	    findStatesInitValues iter_sym (Symbol.name instname) (CurrentModel.classname2class classname)


	fun default2c_str (SOME v) = CWriterUtil.exp2c_str v
	  | default2c_str NONE = CWriterUtil.exp2c_str (Exp.TERM Exp.NAN)
	
	val (state_names, state_defaults) = 
	    ListPair.unzip 
		(Util.flatmap 
		     (fn(iter_sym)=>
			let
			    val model as (_,{classname,...},_) = ShardedModel.toModel shardedModel iter_sym
			in
			    CurrentModel.withModel 
				model
				(fn()=>
				   let
				       val class = CurrentModel.classname2class classname
				   in
				       findStatesInitValues iter_sym "" class
				   end)
			end)
		     outputIterators)

	fun is_constant_input input =
	    let
		val name = DOF.Input.name input
	    in
		not (isSome (TermProcess.symbol2temporaliterator name))
	    end

	val (constant_inputs, sampled_inputs) = List.partition is_constant_input (ShardedModel.toInputs shardedModel)
	val (input_names, input_defaults) = ListPair.unzip ((map (fn input => (DOF.Input.name input, DOF.Input.default input)) constant_inputs) @
							   (map (fn input => (DOF.Input.name input, DOF.Input.default input)) sampled_inputs))
	val sampled_exhausted_behaviors = map (fn input => case DOF.Input.behaviour input of
							       DOF.Input.HOLD => "SAMPLED_HOLD"
							     | DOF.Input.HALT => "SAMPLED_HALT"
							     | DOF.Input.CYCLE => "SAMPLED_CYCLE")
					      sampled_inputs

	val sampled_periods = map (fn input =>
				      let
					  val (iter_sym,_) = valOf (TermProcess.symbol2temporaliterator (DOF.Input.name input))
					  val (_,iter_typ) = CurrentModel.itersym2iter iter_sym
				      in
					  case iter_typ
					   of DOF.DISCRETE {sample_period} => sample_period
					    | _ => DynException.stdException(("Unexpected iterator '"^(Symbol.name iter_sym)^"'"), "CParallelWriter.simengine_interface", Logger.INTERNAL)
				      end) sampled_inputs

	val outputs = ShardedModel.toOutputs shardedModel
	val output_periods = map (fn output =>
				     let
					 val (iter_sym,_) = valOf (TermProcess.symbol2temporaliterator (DOF.Output.name output))
					 fun iterSymToDT (iter_sym) =
					     let
						 val (_,iter_typ) = CurrentModel.itersym2iter iter_sym
					     in
						 case iter_typ
						  of DOF.DISCRETE {sample_period} => sample_period
						   | DOF.CONTINUOUS solver => (case solver
										of Solver.FORWARD_EULER {dt} => dt
										 | Solver.EXPONENTIAL_EULER {dt} => dt
										 | Solver.LINEAR_BACKWARD_EULER {dt,...} => dt
										 | Solver.RK4 {dt} => dt
										 | Solver.MIDPOINT {dt} => dt
										 | Solver.HEUN {dt} => dt
										 | Solver.ODE23 {dt,...} => 0.0 (* Change this to dt when ODE23 supports fixed timestep *)
										 | Solver.ODE45 {dt,...} => 0.0 (* Change this to dt when ODE23 supports fixed timestep *)
										 | Solver.CVODE {dt,...} => dt
										 | _ => 0.0) (* Any solver not specified above automatically assumed to be variable timestep *)
						   | DOF.ALGEBRAIC (processtype, symbol) => iterSymToDT(symbol)
						   | DOF.IMMEDIATE => ~1.0 (* Only outputs for first and last iteration *)
						   | _ => DynException.stdException(("Unexpected iterator '"^(Symbol.name iter_sym)^"'"), "CParallelWriter.simengine_interface", Logger.INTERNAL)
					     end
				     in
					 iterSymToDT(iter_sym)
				     end) outputs

	val output_mode = if (List.exists (fn x => Real.== (x, 0.0)) output_periods) then
			      0 (* OUTPUT_RAW_FILES *)
			  else
			      if(List.all (fn x => case (DOF.Output.condition x) of
						       (Exp.TERM (Exp.BOOL v)) => v
						     | _ => false)
					  outputs) then
				  2 (* OUTPUT_PREALLOCATED *)
			      else
				  0 (* OUTPUT_RAW_FILES *)
				  
	fun wrap (f, m) x = CurrentModel.withModel m (fn _ => f x)

	fun name_subsystem_outputs shardedModel iter_sym =
	    let
		val model = ShardedModel.toModel shardedModel iter_sym
		val (_, {classname,...},_) = model
	    in
		CurrentModel.withModel model (fn _ =>
						 let val class = CurrentModel.classname2class classname
						     val {outputs, ...} = class
						 in map (Term.sym2name o DOF.Output.name) (! outputs)
						 end)
	    end

	val output_names = 
	    Util.uniquify (Util.flatmap (name_subsystem_outputs shardedModel) outputIterators)

	fun output_num_quantities (model, output) =
	    CurrentModel.withModel model (fn _ =>
	    let val (name, contents) = (DOF.Output.name output, DOF.Output.contents output)
	    in case TermProcess.symbol2temporaliterator name
		of SOME (iter_sym, _) => inc (List.length contents)
		 | _ => List.length contents
	    end)

	(* Presumes a CurrentModel.withModel context. *)
	fun outputs_from_class (model, class) =
	    let val {outputs, ...} = class
	    in 
		map (fn (output) => (model, output))
		    (! outputs)
	    end

	val outputs_from_top_classes =
	    Util.flatmap (fn(iter_sym) => 
			     let
				 val model = ShardedModel.toModel shardedModel iter_sym
				 val (_,{classname,...},_) = model
			     in
				 CurrentModel.withModel model (fn _ => (outputs_from_class (model, CurrentModel.classname2class classname)))
			     end)
			 outputIterators

	val outputs_from_top_classes =
	    Util.uniquify_by_fun (fn ((_,a),(_,b)) => Term.sym2curname (DOF.Output.name a) = Term.sym2curname (DOF.Output.name b)) outputs_from_top_classes


	val outputs_num_quantities = map output_num_quantities outputs_from_top_classes

	val default_inputs = map default2c_str input_defaults

 	val iters_enumerated = map (fn it => "ITERATOR_"^ it) iterator_names

        val solvers_enumerated = Util.uniquify (map (fn sol => String.map Char.toUpper sol) solver_names)

	local 
	    open JSON
	    val int = int o IntInf.fromInt

	    (* Emits unrepresentable values as NAN *)
	    fun defaultToJSON NONE = null
	      | defaultToJSON (SOME (exp as Exp.TERM t)) =
		(case t
		  of Exp.BOOL b => bool b
		   | Exp.INFINITY => real (1.0 / 0.0)
		   | Exp.INT z => int z
		   | Exp.NAN => real (0.0 / 0.0)
		   | Exp.RATIONAL (n, d) => real (Real.fromInt n / Real.fromInt d)
		   | Exp.REAL r => real r
		   | _ => real (0.0 / 0.0))
	      | defaultToJSON _ = real (0.0 / 0.0)
	in
	val jsonInterface = 
	    object [("name", string class_name),
		    ("inputs", array (map (string o Term.sym2name) input_names)),
		    ("defaultInputs", array (map defaultToJSON input_defaults)),
		    ("states", array (map string state_names)),
		    ("defaultStates", array (map (defaultToJSON o SOME) state_defaults)),
		    ("outputs", array (map string output_names)),
		    ("outputNumQuantities", array (map int outputs_num_quantities)),
		    ("outputMode", int output_mode),
		    ("outputPeriods", array (map real output_periods)),
		    ("precision", string "%d"), (* Place holder for sizeof(CDATAFORMAT) *)
		    ("pointer_size", string "%d"), (* Place holder for sizeof(void* ) *)
		    ("parallel_models", string "%d"), (* Place holder for PARALLEL_MODELS *)
		    ("buffer_length", string "%d"), (* Place holder for BUFFER_LEN *)
		    ("hashcode", string "0000000000000000"),
		    ("version", int 0)]
	end

	local
	    (* Emits non-constant values as NAN *)
	    fun stateDefaultToConstant (exp as Exp.TERM t) =
		(case t 
		  of Exp.BOOL _ => exp
		   | Exp.INFINITY => exp
		   | Exp.INT _ => exp
		   | Exp.NAN => exp
		   | Exp.RATIONAL _ => exp
		   | Exp.REAL _ => exp
		   | _ => Exp.TERM Exp.NAN)
	      | stateDefaultToConstant _ = Exp.TERM Exp.NAN
	in
	val stateDefaultConstants = map stateDefaultToConstant state_defaults
	end

	fun outputToCount output = 
	    let
		val contentsLength = List.length (DOF.Output.contents output)
	    in
		if contentsLength = 0 then
		    1 (* it has to be at least one, even if contents is empty since it'll return events *)
		else
		    contentsLength
	    end
	val total_output_quantities =
	    List.foldr op+ 0 (map outputToCount outputs)
   in
	[$("typedef enum {"),
	 SUB(map (fn(sol) => $((sol ^ ","))) solvers_enumerated),
	 SUB[$("NUM_SOLVERS")],
	 $("} Solver;"),
	 $("const Solver SOLVERS[NUM_SOLVERS] = {" ^ (String.concatWith ", " solvers_enumerated) ^ "};"),
	 $(""),
	 $("typedef enum {"),
	 SUB(map (fn(iter) => $("ITERATOR_"^iter^",")) iterator_names),
	 SUB[$("NUM_ITERATORS")],
	 $("} Iterator;"),
	 $("const Iterator ITERATORS[NUM_ITERATORS] = {" ^ (String.concatWith ", " iters_enumerated) ^ "};"),
	 $(""),
	 $("static const char *input_names[] = {" ^ (String.concatWith ", " (map (cstring o Term.sym2name) input_names)) ^ "};"),
	 $("static const double sampled_input_timesteps[] = {" ^ (String.concatWith ", " (map (CWriterUtil.exp2c_str o Exp.TERM o Exp.REAL) sampled_periods)) ^ "};"),
	 $("static const double output_timesteps[] = {" ^ (String.concatWith ", " (map (CWriterUtil.exp2c_str o Exp.TERM o Exp.REAL) output_periods)) ^ "};"),
	 $("static const sampled_eof_option_t sampled_input_eof_options[] = {" ^ (String.concatWith ", " sampled_exhausted_behaviors) ^ "};"),
	 $("static const char *state_names[] = {" ^ (String.concatWith ", " (map cstring state_names)) ^ "};"),
	 $("static const char *output_names[] = {" ^ (String.concatWith ", " (map cstring output_names)) ^ "};"),
	 $("static const char *iterator_names[] = {" ^ (String.concatWith ", " (map cstring iterator_names)) ^ "};"),
	 $("static const double default_inputs[] = {" ^ (String.concatWith ", " default_inputs) ^ "};"),
	 $("static const double default_states[] = {" ^ (String.concatWith ", " (map CWriterUtil.exp2c_str stateDefaultConstants)) ^ "};"),
	 $("static const unsigned int output_num_quantities[] = {" ^ (String.concatWith ", " (map i2s outputs_num_quantities)) ^ "};"),
	 $("static const char model_name[] = \"" ^ class_name ^ "\";"),
	 $("static const char *solver_names[] = {" ^ (String.concatWith ", " (map cstring solver_names)) ^ "};"),
	 $(""),
	 (* This would be nice but fails in gcc
	 $("static const unsigned int NUM_INPUTS = "^(i2s (List.length input_names)) ^ ";"),
	 $("static const unsigned int NUM_STATES = "^(i2s (List.length state_names)) ^ ";"),
	 $("static const unsigned long long HASHCODE = 0x0000000000000000ULL;"),
	 $("static const unsigned int VERSION = 0;"),
          *)
	 $("#define NUM_CONSTANT_INPUTS "^(i2s (List.length constant_inputs))),
	 $("#define NUM_SAMPLED_INPUTS "^(i2s (List.length sampled_inputs))),
	 $("#define NUM_TIME_VALUE_INPUTS 0"),
	 $("#define NUM_EVENT_INPUTS 0"),
	 $("#define NUM_INPUTS (NUM_CONSTANT_INPUTS + NUM_SAMPLED_INPUTS + NUM_TIME_VALUE_INPUTS + NUM_EVENT_INPUTS)"),
	 $("#define NUM_STATES "^(i2s (List.length state_names))),
	 $("#define OUTPUT_MODE " ^ (i2s output_mode)),
	 $("#define HASHCODE 0x0000000000000000ULL"),
	 $("#define NUM_OUTPUTS "^(i2s (List.length output_names))),
	 $("#define MAX_OUTPUT_SIZE (NUM_OUTPUTS*2*sizeof(int) + (NUM_OUTPUTS+" ^ (i2s total_output_quantities)  ^ ")*sizeof(CDATAFORMAT)) //size in bytes"),
	 $("#define VERSION 0"),
	 $(""),
	 $("static const char *json_interface = " ^
	   (cstring (PrintJSON.toString jsonInterface)) ^
	   ";")]
    end
    handle e => DynException.checkpoint "CParallelWriter.simengine_interface" e


fun class2stateiterators (class: DOF.class) =
    let
	val iterators = map (fn(sym,_)=>sym) (CurrentModel.iterators())
    in
	(iterators, iterators)
    end


local
    fun output2struct (term,sym) =
	$("CDATAFORMAT " ^(Symbol.name sym)^";")
in
fun outputdatastruct_code shardedModel =
    let
	val outputs = ShardedModel.toOutputs shardedModel
    in
	[$(""),
	 $("#if NUM_OUTPUTS > 0"),
	 SUB[$("typedef struct {"),
	     SUB(map output2struct (CWriterUtil.outputs2uniqueoutputsymbols outputs)),
	     $("} output_data;")],
	 $("#endif"),
	 $("")]
    end
end

fun solver_wrappers solvers =
    let
	val methods_params = [("_init", "", ""),
			      ("_eval", ", unsigned int modelid", ", modelid"),
			      ("_free", "", "")]
	fun method_redirect (m, p) s =
	    [$("case " ^ (String.map Char.toUpper s) ^ ":"),
	     SUB[$("return " ^ s ^ m ^ "(props" ^ p ^ ");")]]

	fun create_wrapper (m,pd,p) =
	    [(case m 
	       of "_eval" =>
		  $("__DEVICE__ int solver_eval(solver_props *props" ^ pd ^") {")
		| _ => 
		   $("int solver" ^ m ^ "(solver_props *props" ^ pd ^ "){")),
	     $("assert(NUM_SOLVERS > props->solver);"),
	     SUB($("switch(props->solver){") ::
		 (Util.flatmap (method_redirect (m, p)) solvers) @
		 [$("default:"),
		  SUB[$("return 1;")],
		  $("}")]),
	     $("}"),
	     $("")]
    in
	$("// Wrappers for redirection to correct solver") ::
	Util.flatmap create_wrapper methods_params
    end



fun update_wrapper shardedModel =
    let 
	fun call_update iter_sym =
	    let val model as (_, {classname=top_class,...}, _) = ShardedModel.toModel shardedModel iter_sym
		val iter = ShardedModel.toIterator shardedModel iter_sym
	    in
		CurrentModel.withModel 
		    model 
		    (fn _ =>
			let val (iter_name, iter_typ) = iter
			    val (base_iter_name, base_iter_typ) = 
				case iter_typ 
				 of DOF.UPDATE dep => CurrentModel.itersym2iter dep
				  | _ => 
				    DynException.stdException(("Unexpected iterator '"^(Symbol.name iter_name)^"'"), "CParallelWriter.update_wrapper", Logger.INTERNAL)

			    val class = CurrentModel.classname2class top_class
			    val basename = ClassProcess.class2preshardname class
			    val basename_iter = (Symbol.name basename) ^ "_" ^ (Symbol.name base_iter_name)
			    val (statereads, statewrites, systemstatereads) =
				(if reads_iterator iter class then "("^(*"const "^*)"statedata_" ^ basename_iter ^ " * )props->model_states, " else "",
				 if writes_iterator iter class then "(statedata_" ^ basename_iter ^ " * )props->next_states, " else "",
				 if reads_system class then "props->system_states, " else "")

			in [$("case ITERATOR_" ^ (Util.removePrefix (Symbol.name base_iter_name)) ^ ":"),
			    case base_iter_typ
			     of DOF.CONTINUOUS _ =>
				SUB [$("return flow_" ^ (Symbol.name top_class) ^ "(props->next_time[modelid], " ^
				       statereads ^ statewrites ^ systemstatereads ^
				       "NULL, (CDATAFORMAT * )props->od, 1, modelid);")]
			      | DOF.DISCRETE _ => 
				SUB [$("return flow_" ^ (Symbol.name top_class) ^ "(1 + props->count[modelid], " ^
				       statereads ^ statewrites ^ systemstatereads ^
				       "NULL, (CDATAFORMAT * )props->od, 1, modelid);")]
			      | _ => $("#error BOGUS ITERATOR")]
			end)
	    end

    in [$("__HOST__ __DEVICE__ int update(solver_props *props, unsigned int modelid) {"),
	SUB ($("switch (props->iterator) {") ::
	     (Util.flatmap call_update (ShardedModel.iterators shardedModel)) @
	     [$("default: return 1;"),
	      $("}")]),
	$("}")]
    end

fun preprocess_wrapper shardedModel preprocessIterators =
    let 
	fun call_update iter_sym =
	    let val model as (_, {classname=top_class,...}, _) = ShardedModel.toModel shardedModel iter_sym
		val iter = ShardedModel.toIterator shardedModel iter_sym
	    in	    
		CurrentModel.withModel 
		    model 
		    (fn _ =>
			let val (iter_name, iter_typ) = iter
			    val (base_iter_name, base_iter_typ) = 
				case iter_typ 
				 of DOF.ALGEBRAIC (DOF.PREPROCESS, dep) => CurrentModel.itersym2iter dep
				  | _ => 
				    DynException.stdException(("Unexpected iterator '"^(Symbol.name iter_name)^"'"), "CParallelWriter.update_wrapper", Logger.INTERNAL)

			    val class = CurrentModel.classname2class top_class
			    val (statereads, statewrites, systemstatereads) =
				(if reads_iterator iter class then "props->system_states->states_" ^ (Symbol.name iter_name) ^ ", " else "",
				 if writes_iterator iter class then "props->system_states->states_" ^ (Symbol.name iter_name) ^ "_next, " else "",
				 if reads_system class then "props->system_states, " else "")

			in [$("case ITERATOR_" ^ (Util.removePrefix (Symbol.name base_iter_name)) ^ ":"),
			    case base_iter_typ
			     of DOF.CONTINUOUS _ =>
				SUB [$("return flow_" ^ (Symbol.name top_class) ^ "(props->time[modelid], " ^
				       statereads ^ statewrites ^ systemstatereads ^
				       "NULL, (CDATAFORMAT * )props->od, 1, modelid);")]
			      | DOF.DISCRETE _ => 
				SUB [$("return flow_" ^ (Symbol.name top_class) ^ "(props->count[modelid], " ^
				       statereads ^ statewrites ^ systemstatereads ^
				       "NULL, (CDATAFORMAT * )props->od, 1, modelid);")]
			      | _ => $("#error BOGUS ITERATOR")]
			end)
	    end

    in [$("__HOST__ __DEVICE__ int pre_process(solver_props *props, unsigned int modelid) {"),
	SUB ($("switch (props->iterator) {") ::
	     (Util.flatmap call_update preprocessIterators) @
	     [$("default: return 1;"),
	      $("}")]),
	$("}")]
    end

fun inprocess_wrapper shardedModel inprocessIterators =
    let val _ = ()
	fun call_update iter_sym =
	    let val model as (_, instance, _) = ShardedModel.toModel shardedModel iter_sym
		val topClassName = 
		    case #name instance 
		     of SOME x => x | NONE => #classname instance		

		val iter = ShardedModel.toIterator shardedModel iter_sym
	    in	    
		CurrentModel.withModel 
		    model 
		    (fn _ =>
			let val (iter_name, iter_typ) = iter
			    val (base_iter_name, base_iter_typ) = 
				case iter_typ 
				 of DOF.ALGEBRAIC (DOF.INPROCESS, dep) => CurrentModel.itersym2iter dep
				  | _ => 
				    DynException.stdException(("Unexpected iterator '"^(Symbol.name iter_name)^"'"), "CParallelWriter.update_wrapper", Logger.INTERNAL)

			    val class = CurrentModel.classname2class topClassName
			    val (statereads, statewrites, systemstatereads) =
				(if reads_iterator iter class then "props->system_states->states_" ^ (Symbol.name iter_name) ^ ", " else "",
				 if writes_iterator iter class then "props->system_states->states_" ^ (Symbol.name iter_name) ^ "_next, " else "",
				 if reads_system class then "props->system_states, " else "")

			in
			    [$("case ITERATOR_" ^ (Util.removePrefix (Symbol.name base_iter_name)) ^ ":"),
			     (case base_iter_typ
			       of DOF.CONTINUOUS _ =>
				  SUB [$("return flow_" ^ (Symbol.name topClassName) ^ "(props->time[modelid], " ^
					 statereads ^ statewrites ^ systemstatereads ^
					 "NULL, (CDATAFORMAT * )props->od, 1, modelid);")]
				| DOF.DISCRETE _ => 
				  SUB [$("return flow_" ^ (Symbol.name topClassName) ^ "(props->count[modelid], " ^
					 statereads ^ statewrites ^ systemstatereads ^
					 "NULL, (CDATAFORMAT * )props->od, 1, modelid);")]
				| _ => $("#error BOGUS ITERATOR"))]
			end)
	    end

    in [$("__HOST__ __DEVICE__ int in_process(solver_props *props, unsigned int modelid) {"),
	SUB ($("switch (props->iterator) {") ::
	     (Util.flatmap call_update inprocessIterators) @
	     [$("default: return 1;"),
	      $("}")]),
	$("}")]
    end

fun postprocess_wrapper shardedModel postprocessIterators =
    let val _ = ()
	fun call_update iter_sym =
	    let val model as (_, {classname=top_class,...}, _) = ShardedModel.toModel shardedModel iter_sym
		val iter = ShardedModel.toIterator shardedModel iter_sym
	    in	    
		CurrentModel.withModel 
		    model 
		    (fn _ =>
			let val (iter_name, iter_typ) = iter
			    val (base_iter_name, base_iter_typ) = 
				case iter_typ 
				 of DOF.ALGEBRAIC (DOF.POSTPROCESS, dep) => CurrentModel.itersym2iter dep
				  | _ => 
				    DynException.stdException(("Unexpected iterator '"^(Symbol.name iter_name)^"'"), "CParallelWriter.update_wrapper", Logger.INTERNAL)

			    val class = CurrentModel.classname2class top_class
			    val (statereads, statewrites, systemstatereads) =
				(if reads_iterator iter class then "props->system_states->states_" ^ (Symbol.name iter_name) ^ ", " else "",
				 if writes_iterator iter class then "props->system_states->states_" ^ (Symbol.name iter_name) ^ "_next, " else "",
				 if reads_system class then "props->system_states, " else "")

			in [$("case ITERATOR_" ^ (Util.removePrefix (Symbol.name base_iter_name)) ^ ":"),
			    case base_iter_typ
			     of DOF.CONTINUOUS _ =>
				SUB [$("return flow_" ^ (Symbol.name top_class) ^ "(props->next_time[modelid], " ^
				       statereads ^ statewrites ^ systemstatereads ^
				       "NULL, (CDATAFORMAT * )props->od, 1, modelid);")]
			      | DOF.DISCRETE _ => 
				SUB [$("return flow_" ^ (Symbol.name top_class) ^ "(1 + props->count[modelid], " ^
				       statereads ^ statewrites ^ systemstatereads ^
				       "NULL, (CDATAFORMAT * )props->od, 1, modelid);")]
			      | _ => $("#error BOGUS ITERATOR")]
			end)
	    end

    in [$("__HOST__ __DEVICE__ int post_process(solver_props *props, unsigned int modelid) {"),
	SUB ($("switch (props->iterator) {") ::
	     (Util.flatmap call_update postprocessIterators) @
	     [$("default: return 1;"),
	      $("}")]),
	$("}")]
    end


local
    fun state2member (sym) =
	let
	    val size = (*Term.symbolSpatialSize (ExpProcess.exp2term sym)*)
		ExpProcess.exp2size sym
	    val name = Symbol.name (Term.sym2curname (ExpProcess.exp2term sym))
	in
	    if size = 1 then ("CDATAFORMAT " ^ name ^ "[ARRAY_SIZE];")
	    else ("CDATAFORMAT " ^ name ^ "["^(i2s size)^" * ARRAY_SIZE];")
	end

    fun instanceNamed instname inst =
	ExpProcess.instOrigInstName inst = instname

    fun instance2member instances (classname, instname) =
	let val classTypeName = ClassProcess.classTypeName (CurrentModel.classname2class classname)		  
	    val index = 
		case List.find (instanceNamed instname) instances 
		 of SOME inst' => 
		    let val size = ExpProcess.instSpatialSize inst'
		    in if 1 = size then ";" else "["^(i2s size)^"];"
		    end
		  | NONE => ";"
	in
	    "statedata_" ^ (Symbol.name classTypeName) ^ " " ^ (Symbol.name instname) ^ index
	end
in
fun outputstatestructbyclass_code iterator (class : DOF.class as {exps, ...}) =
    let
	val classname = ClassProcess.class2classname class
	val classTypeName = ClassProcess.classTypeName class

	val init_eqs_symbols = map ExpProcess.lhs (List.filter ExpProcess.isInitialConditionEq (!exps))
						       
	(* can't sort symbols since some processing, especially when matrices are generated, assumes that the state order is fixed.
	  In particular, the linear backward euler solver reorders the initial values in the same order that it is assuming for the
	  matrix. *)
	(*val init_eqs_symbols = 
	    Sorting.sorted (fn (x, y) => String.compare (Symbol.name (Term.sym2curname (ExpProcess.exp2term x)),
							 Symbol.name (Term.sym2curname (ExpProcess.exp2term y))))
			   init_eqs_symbols*)

	val instances = ClassProcess.class2instances class

	fun classAndInstanceName eqn =
	    let val {classname, ...} = ExpProcess.deconstructInst eqn
	    in 
		(classname, ExpProcess.instOrigInstName eqn)
	    end
	val class_inst_pairs =
	    let 
		fun uniq_fun ((c1,i1),(c2,i2)) = i1 = i2
	    in
		Util.uniquify_by_fun uniq_fun (map classAndInstanceName instances)
	    end

	val class_inst_pairs_non_empty = 
	    List.filter (fn (cn,instname) => 
			    let 
				val matching_classes = List.filter
							   (fn(_, instname')=>instname=instname')
							   (map classAndInstanceName instances)
				(*val class = CurrentModel.classname2class cn*)
			    in 
				List.exists (fn(c)=> 
					       reads_iterator iterator c orelse
					       writes_iterator iterator c) 
					    (map (fn(cn,_)=>CurrentModel.classname2class cn) matching_classes)
			    end) 
			class_inst_pairs

	(*val class_inst_pairs_non_empty =
	    Sorting.sorted (fn ((_,x), (_,y)) => (String.compare (Symbol.name x, Symbol.name y)))
			   class_inst_pairs_non_empty*)
    in
	if List.null class_inst_pairs_non_empty andalso List.null init_eqs_symbols then 
	    [$(""),
	     $("// Ignoring class '" ^ (Symbol.name (#name class)) ^ "'")]
	else
	    [$(""),
	     $("// States for class " ^ (Symbol.name (#name class))),
	     $("typedef struct  {"),	 
	     SUB($("// states (count="^(i2s (List.length init_eqs_symbols))^")") ::
		 (map ($ o state2member) init_eqs_symbols) @
		 ($("// instances (count=" ^ (i2s (List.length class_inst_pairs_non_empty)) ^")") ::
		  (map ($ o (instance2member instances)) class_inst_pairs_non_empty))),
	     $("} statedata_" ^ (Symbol.name classTypeName) ^";")]
    end
    handle e => DynException.checkpoint "CParallelWriter.outputstatestructbyclass_code" e       
end

fun iodatastruct_code (shardedModel as (shards,sysprops)) =
    Layout.align 
	(map 
	     (fn (shard as {iter_sym,...}) =>
		 CurrentModel.withModel 
		     (ShardedModel.toModel shardedModel iter_sym)
		     (fn _ => iodatastruct_shard_code shard))
	     shards)

and iodatastruct_shard_code (shard as {classes,...}) =
    Layout.align 
	(map (fn class as {name,...} => 
		 iodatastruct_class_code (name = #classname (CurrentModel.top_inst ())) class)
	     classes)

and iodatastruct_class_code is_top_class class =
    let
	val classname = ClassProcess.class2classname class
    in
	Layout.align
	    [$("// Inputs for class " ^ (Symbol.name classname)),
	     input_datastruct_class_code is_top_class class,
	     $("// States for class " ^ (Symbol.name classname)),
	     state_datastruct_class_code is_top_class class,
	     $("// Outputs for class " ^ (Symbol.name classname)),
	     output_datastruct_class_code is_top_class class]
    end

and input_datastruct_class_code is_top_class class =
    let
	open Layout
	val typename = ClassProcess.classTypeName class
	val inputs_layout =
	    align (ClassProcess.foldInputs
		       (fn (it,lyt) => 
			   $("CDATAFORMAT "^ (Term.sym2name (DOF.Input.name it)) ^ "[VECTOR_WIDTH];") :: 
			   lyt)
		       nil class)
	val instances_layout =
	    align (ClassProcess.foldInstanceEquations
		       (fn (it,lyt) => 
			   let 
			       val {classname,instname,...} = ExpProcess.deconstructInst it
			       val class = CurrentModel.classname2class classname
			       val typename = ClassProcess.classTypeName class
			   in
			       $((Symbol.name typename) ^ "_output " ^ (Symbol.name instname) ^ ";") ::
			       lyt
			   end)
		       nil class)
    in
	if isEmpty inputs_layout andalso isEmpty instances_layout then
	    Layout.empty
	else
	    align
		[$("typedef struct {"),
		 SUB [inputs_layout, instances_layout],
		 $("} "^(Symbol.name typename)^"_input;")]
    end

and state_datastruct_class_code is_top_class class =
    let
	open Layout
	val typename = ClassProcess.classTypeName class
	val states_layout =
	    align (ClassProcess.foldInitialValueEquations
		       (fn (it,lyt) =>
			   let
			       val sym = ExpProcess.lhs it
			       val size = ExpProcess.exp2size sym
			       val name = Symbol.name (Term.sym2curname (ExpProcess.exp2term sym))
			   in
			       $("CDATAFORMAT " ^ name ^ "["^(i2s size)^"*VECTOR_WIDTH];") ::
			       lyt
			   end)
		       nil class)
	val instances_layout =
	    align (ClassProcess.foldInstanceEquations
		       (fn (it,lyt) => 
			   let 
			       val {classname,instname,...} = ExpProcess.deconstructInst it
			       val class = CurrentModel.classname2class classname
			       val typename = ClassProcess.classTypeName class
			   in
			       $((Symbol.name typename) ^ "_state " ^ (Symbol.name instname) ^ ";") ::
			       lyt
			   end)
		       nil class)
    in
	if isEmpty states_layout andalso isEmpty instances_layout then
	    Layout.empty
	else
	    align
		[$("typedef struct {"),
		 SUB [states_layout, instances_layout],
		 $("} "^(Symbol.name typename)^"_state;")]
    end

and output_datastruct_class_code is_top_class class =
    let
	open Layout
	val typename = ClassProcess.classTypeName class
	val outputs_layout =
	    align (ClassProcess.foldOutputs
		       (fn (it,lyt) => 
			   let 
			       val size = List.length (DOF.Output.contents it)
			       val name = Term.sym2name (DOF.Output.name it)
			   in
			       $("CDATAFORMAT "^ name ^ "[" ^ (i2s size) ^ "*VECTOR_WIDTH];") :: 
			       lyt
			   end)
		       nil class)
	val instances_layout =
	    align (ClassProcess.foldInstanceEquations
		       (fn (it,lyt) => 
			   let 
			       val {classname,instname,...} = ExpProcess.deconstructInst it
			       val class = CurrentModel.classname2class classname
			       val typename = ClassProcess.classTypeName class
			   in
			       $((Symbol.name typename) ^ "_output " ^ (Symbol.name instname) ^ ";") ::
			       lyt
			   end)
		       nil class)
    in
	if isEmpty outputs_layout andalso isEmpty instances_layout then
	    Layout.empty
	else
	    align
		[$("typedef struct {"),
		 SUB [outputs_layout, instances_layout],
		 $("} "^(Symbol.name typename)^"_output;")]
    end
    
    

(* Nb depends on a CurrentModel context. *)
fun outputstatestruct_code (iterator: DOF.systemiterator, shard as {classes, ...}) =
    let 
	val () = ()
    in
	Util.flatmap (outputstatestructbyclass_code iterator) classes
    end
    handle e => DynException.checkpoint "CParallelWriter.outputstatestruct_code" e

fun outputsystemstatestruct_code (shardedModel as (shards,_)) statefulIterators =
    let
	val all_classes = Util.flatmap (fn{classes,...}=>classes) shards
	val iterators = map (ShardedModel.toIterator shardedModel) statefulIterators

	fun subsystem_classname_iterator_pair iter_sym =
	    let val model = ShardedModel.toModel shardedModel iter_sym
		val iter = ShardedModel.toIterator shardedModel iter_sym
		val (_, {classname, ...}, _) = model
	    in
		CurrentModel.withModel model (fn _ =>
		let val (iter_sym, iter_typ) = iter
		    val class = CurrentModel.classname2class classname
		in if has_states iter class then
		       SOME (classname, iter_sym, iter_typ)
		   else NONE
		end)
	    end
	    handle e => DynException.checkpoint "CParallelWriter.outputsystemstatestruct_code.subsystem_classname_iterator_pair" e

	val class_names_iterators = 
	    List.mapPartial subsystem_classname_iterator_pair statefulIterators

	val top_sys_state_struct_prog =
	    if List.null class_names_iterators then []
	    else
		[$(""),
		 $("// System State Structure (external ordering)"),
		 $("typedef struct {"),
		 SUB(map (fn(classname, iter_sym, _) => $("statedata_" ^ (Symbol.name classname) ^ " states_" ^ (Symbol.name iter_sym) ^ "[1];")) class_names_iterators),
		 $("} systemstatedata_external;"),
                 $(""),
		 (* Should make the following conditional on whether we are targetting CPU or OPENMP (not GPU) *)
		 $("#ifndef TARGET_GPU"),
		 $("// System State Structure (internal ordering)"),
		 $("typedef struct {"),
		 SUB(map (fn(classname, iter_sym, _) => $("statedata_" ^ (Symbol.name classname) ^ " states_" ^ (Symbol.name iter_sym) ^ "[PARALLEL_MODELS];")) class_names_iterators),
		 $("} systemstatedata_internal;"),
		 $("#endif"),
                 $("")]


	(* Declares a pointer to an iterator value. *)
	val systemPointerStructureIteratorValue =
	    fn (sym, DOF.CONTINUOUS _) => SOME ("CDATAFORMAT * " ^ (Symbol.name sym) ^ ";")
	     | (sym, DOF.DISCRETE _) => SOME ("unsigned int * " ^ (Symbol.name sym) ^ ";")
	     | _ => NONE

	(* Declares pointers to the states of an iterator. *)
	fun systemPointerStructureStates (typename, (iter_sym, iter_typ)) =
	    let val ctype = "statedata_" ^ (Symbol.name typename) ^ " * "
	    in
		case iter_typ
		 of DOF.CONTINUOUS _ => 
		    [ctype ^ "states_" ^ (Symbol.name iter_sym) ^ ";",
		     ctype ^ "states_" ^ (Symbol.name iter_sym) ^ "_next;"]
		  | DOF.DISCRETE _ => 
		    [ctype ^ "states_" ^ (Symbol.name iter_sym) ^ ";",
		     ctype ^ "states_" ^ (Symbol.name iter_sym) ^ "_next;"]
		  | DOF.ALGEBRAIC _ => 
		    [ctype ^ "states_" ^ (Symbol.name iter_sym) ^ ";",
		     ctype ^ "states_" ^ (Symbol.name iter_sym) ^ "_next;"]
		  | _ => nil
	    end

	(* Declares a structured data type representing the system of iterators for a class.
	 * This data type comprises references to the iterator values themselves, followed
	 * by references to each iterator's states. *)
	fun systemPointerStructure {basename, iterators, typedIterators} =
	    if List.null iterators andalso List.null typedIterators then
		[$("// Class " ^ (Symbol.name basename) ^ " is stateless."),
		 $("typedef void * systemstatedata_" ^ (Symbol.name basename) ^ ";"),$("")]
	    else

		[$("// The system of iterators for class " ^ (Symbol.name basename) ^ "."),
		 $("typedef struct {"),
		 SUB (map $
			  (List.mapPartial systemPointerStructureIteratorValue
					   iterators)),
		 SUB (map $
			  (Util.flatmap systemPointerStructureStates typedIterators)),
		 $("} systemstatedata_"^(Symbol.name basename)^";"),$("")]


	val classBaseNames = 
	    let val classes = Util.flatmap #classes shards
	    in Util.uniquify_by_fun (op =) (map ClassProcess.class2preshardname classes)
	    end

	val topClassBaseName =
	    let 
		val model = ShardedModel.toModel shardedModel (hd (ShardedModel.iterators shardedModel))
		val classname = 
		    let 
			val {instance, ...} = hd shards
			val class = CurrentModel.withModel model 
							   (fn _ => CurrentModel.classname2class (#classname instance))
		    in 
			ClassProcess.class2preshardname class
		    end
	    in
		classname
	    end

	val systems =
	    map (fn bn => 
		    let 
			fun findTypeForIterator (sym,_) =
			    let val shard = valOf (ShardedModel.findShard (shardedModel, sym))
				val class = valOf (List.find (fn c => bn = (ClassProcess.class2preshardname c)) (#classes shard))
			    in
				ClassProcess.classTypeName class
			    end
			    handle Option => DynException.stdException(("Option error: " ^ (Symbol.name sym)), "CParallelWriter.outputsystemstatestruct_code.findTypeForIterator", Logger.INTERNAL)
				 | e => DynException.checkpoint "CParallelWriter.outputsystemstatestruct_code.findTypeForIterator" e

			val iterators_with_states = 
			    List.filter (fn(it as (iter_sym,_))=> 
					   let
					       val model as (classes,{classname=top_class,...},_) = ShardedModel.toModel shardedModel iter_sym
					       val classesOfBaseName = List.filter (fn(c)=> ClassProcess.class2preshardname c = bn) classes
					       val hasStates = CurrentModel.withModel 
								   model
								   (fn()=> List.exists (has_states it) classesOfBaseName)
					   in
					       hasStates
					   end)
					iterators
			val typedIterators = map (fn it => (findTypeForIterator it, it)) iterators_with_states
		    in
			systemPointerStructure {basename = bn, iterators=iterators, typedIterators = typedIterators}
		    end)
		classBaseNames


	val per_class_struct_prog = 
	    $("// Per-class system pointer structures") ::
	    (List.concat systems) @
	    [$("typedef systemstatedata_" ^ (Symbol.name topClassBaseName) ^ " top_systemstatedata;")]
    in
	top_sys_state_struct_prog @ 
	per_class_struct_prog
    end
    handle e => DynException.checkpoint "CParallelWriter.outputsystemstatestruct_code" e


fun exp2prog (exp, is_top_class, iter as (iter_sym, iter_type)) =
    if (ExpProcess.isIntermediateEq exp) then
	intermediateeq2prog exp
    else if (ExpProcess.isFirstOrderDifferentialEq exp) then
	firstorderdiffeq2prog exp
    else if (ExpProcess.isDifferenceEq exp) then
	differenceeq2prog exp
    else if (ExpProcess.isUpdateEq exp) then
	differenceeq2prog exp
    else if (ExpProcess.isAlgebraicStateEq exp) then
	differenceeq2prog exp
    else if (ExpProcess.isInstanceEq exp) then
	instanceeq2prog (exp, is_top_class, iter)
    else if (ExpProcess.isOutputEq exp) then
	outputeq2prog (exp, is_top_class, iter)
    else if (ExpProcess.isReadStateEq exp) then
	differenceeq2prog exp
    else
	DynException.stdException(("Unexpected expression '"^(e2s exp)^"'"), "CParallelWriter.class_flow_code.equ_progs", Logger.INTERNAL)

and intermediateeq2prog exp =
    ((if ExpProcess.isMatrixEq exp then
	  let
	      (*val _ = print ("matrix eq -> ")
	       val _ = Util.log (e2s exp)*)
	      val (lhs, rhs) = (ExpProcess.lhs exp, ExpProcess.rhs exp)
	      val var = CWriterUtil.exp2c_str lhs
	      val m = Container.expMatrixToMatrix rhs
	      val m' = case !m of
			   Matrix.DENSE _ => 
			   let
			   (*val _ = print ("intermediate matrix eq -> ")
			    val _ = Matrix.print m*)
			   in
			       m
			   end
			 | Matrix.BANDED _ => 
			   let
			       val bands = Matrix.toPaddedBands m
			       val m' = Matrix.fromRows (Exp.calculus()) bands
			       (*val _ = print ("matrix bands -> ")
				val _ = Matrix.print m'*)
			       val _ = Matrix.transpose m'
			   (*val _ = print ("matrix bands (transposed) -> ")
			    val _ = Matrix.print m'*)

			   in
			       m'
			   end

	      val (rows, cols) = Matrix.size m'
	      fun createIdx (i,j) = "MAT_IDX("^(i2s rows)^","^(i2s cols)^","^(i2s i)^","^(i2s j)^", PARALLEL_MODELS, modelid)"

	      (* Emits only non-constant values *)
	      fun createEntry (i, j, exp as Exp.TERM t) = 
		  (case t 
		    of Exp.BOOL _ => nil
		     | Exp.INFINITY => nil
		     | Exp.INT _ => nil
		     | Exp.NAN => nil
		     | Exp.RATIONAL _ => nil
		     | Exp.REAL _ => nil
		     | _ =>
 		       [$("// " ^ (e2s exp)),
			$(var ^ "[" ^ (createIdx (i,j)) ^ "]" ^ " = " ^ (CWriterUtil.exp2c_str exp) ^ ";")])
		| createEntry (i, j, exp) =
		  [$("// " ^ (e2s exp)),
		   $(var ^ "[" ^ (createIdx (i,j)) ^ "]" ^ " = " ^ (CWriterUtil.exp2c_str exp) ^ ";")]



	  (*  val _ = print ("Matrix written -> ")
	   val _ = Matrix.print m'*)
	  in
	      let open Layout in
		  (align [$ "int i, j, idx;",
			  $ "// constant copy of matrix is always in row-major order",
			  $ ("for (j=0; j<"^(i2s rows)^"; j++) {"),
			  SUB [$ ("for (i=0; i<"^(i2s cols)^"; i++) {"),
			       SUB [assign ($("idx"), $("MAT_IDX("^(i2s rows)^","^(i2s cols)^",j,i,PARALLEL_MODELS,modelid)")),
				    assign ($(var^"[idx]"), $("INTERNAL_C[i+(j*"^(i2s cols)^")]"))],
			       $ "}"],
			  $ "}"]) ::
		  (List.concat (Matrix.mapi createEntry m'))
	      end
	  end
      else if ExpProcess.isArrayEq exp then
	  let
	      val (lhs, rhs) = (ExpProcess.lhs exp, ExpProcess.rhs exp)
	      val size = (Container.arrayToSize o Container.expArrayToArray) rhs
	      val var = CWriterUtil.exp2c_str lhs				  
	      fun createIdx i = "VEC_IDX("^(i2s size)^","^(i2s i)^", PARALLEL_MODELS, modelid)"
	      fun createEntry (exp, i) = [$("//" ^ (e2s exp)),
					  $(var ^ "["^(createIdx i)^"]" ^ " = " ^ (CWriterUtil.exp2c_str exp) ^ ";")]
	  in
	      Util.flatmap createEntry (StdFun.addCount (Container.arrayToList (Container.expArrayToArray rhs)))
	  end
      else
 	  [$("// " ^ (e2s exp)),
	   $("CDATAFORMAT " ^ (CWriterUtil.exp2c_str exp) ^ ";")])
     handle e => DynException.checkpoint "CParallelWriter.class_flow_code.intermediateeq2prog" e)
    

and firstorderdiffeq2prog exp =
    [$((CWriterUtil.exp2c_str exp) ^ ";")]
and differenceeq2prog exp =
    [$((CWriterUtil.exp2c_str exp) ^ ";")]
and outputeq2prog (exp, is_top_class, iter as (iter_sym, iter_type)) =
    let
	val {classname, instname, props, inpargs, outargs} = ExpProcess.deconstructInst exp
	val outname = 
	    case exp 
	     of Exp.FUN (Fun.BUILTIN Fun.ASSIGN, [Exp.TERM (Exp.TUPLE outargs), Exp.FUN (Fun.OUTPUT {classname, instname, outname, props}, inpargs)]) => outname
 	      | _ => DynException.stdException(("Malformed output equation."),
					  "CParallelWriter.outputeq2prog",
					  Logger.INTERNAL)
	val inpassoc = 
	    case inpargs
	     of [Exp.CONTAINER (Exp.ASSOC tab)] => tab
	      | _ =>
		DynException.stdException(("Inputs of output call should be an ASSOC container."),
					  "CParallelWriter.class_flow_code.instaceeq2prog",
					  Logger.INTERNAL)

	(* every iterator except the update iterator uses an iter_name *)
	val iter_name = Symbol.name (case iter_type of
					 DOF.UPDATE v => v
				       | _ => iter_sym)
	val iter_name' = Symbol.name (case iter_type of
					 DOF.UPDATE v => v
				       | DOF.ALGEBRAIC (_,v) => v
				       | _ => iter_sym)

	val instclass = CurrentModel.classname2class classname

	val dereference = if is_top_class then "[STRUCT_IDX]." else "->"

	val systemdata = Unique.unique "subsys_rd"
	val (statereads, systemstatereads) =
	    (if reads_iterator iter instclass then "&rd_" ^ (iter_name) ^ dereference ^ (Symbol.name instname) ^ ", " else "",
	     if reads_system instclass then "&" ^ systemdata ^ ", " else "")


	fun systemstatedata_iterator (iter as (iter_name, _)) =
	    systemdata^"."^(Symbol.name iter_name)^" = sys_rd->"^(Symbol.name iter_name)^";"
	and systemstatedata_states (iter as (iter_name, _)) =
	    [systemdata^"."^"states_"^(Symbol.name iter_name)^" = &sys_rd->states_"^(Symbol.name iter_name)^"[STRUCT_IDX]."^(Symbol.name instname)^";",
	     systemdata^"."^"states_"^(Symbol.name iter_name)^"_next = &sys_rd->states_"^(Symbol.name iter_name)^"_next[STRUCT_IDX]."^(Symbol.name instname)^";"]

	val iters = List.filter (fn (it) => (not (ModelProcess.isImmediateIterator it)) andalso (ClassProcess.requiresIterator it instclass)) (ModelProcess.returnIndependentIterators ())
	val state_iters = List.filter (fn it => reads_iterator it instclass) (ModelProcess.returnStatefulIterators ())

	val sysstates_init = 
	    if reads_system instclass then
		Layout.align 
		    [$("systemstatedata_"^(Symbol.name (ClassProcess.class2preshardname instclass))^" "^systemdata^";"),
		     $("// iterator pointers"),
		     SUB(map ($ o systemstatedata_iterator) iters),
		     $("// state pointers"),
		     SUB(map $ (Util.flatmap systemstatedata_states state_iters))]
	    else
		Layout.empty

	val calling_name = "output_" ^ (Symbol.name classname) ^ "_" ^ (Symbol.name outname)
	val inpvar = if SymbolTable.null inpassoc then "NULL" else Unique.unique "inputdata"
	val outvar = if List.null outargs then "NULL" else Unique.unique "outputdata"

	val (inps_init,num_inps) = 
	    SymbolTable.foldli
		(fn (k,v,(acc,idx)) =>
		    ($(inpvar ^ "[" ^ (i2s idx) ^ "] = " ^ (CWriterUtil.exp2c_str v) ^ "; // " ^ (Symbol.name k)) :: acc,
		     1+idx)
		    ) (nil,0) inpassoc

	val inps_decl = 
	    case num_inps
	     of 0 => Layout.empty
	      | n => $("CDATAFORMAT " ^ inpvar ^ "[" ^ (i2s n) ^ "];")


	val outs_decl = 
	    if List.null outargs then Layout.empty
	    else $("CDATAFORMAT " ^ outvar ^ "["^(i2s (List.length outargs))^"];")

	fun declare_output (sym, idx) =
	    $("CDATAFORMAT " ^ (Symbol.name sym) ^ ";")
	fun assign_output (sym, idx) =
	    $((Symbol.name sym) ^ " = " ^ outvar ^ "[" ^ (i2s idx) ^ "];")

	val output_symbol_pairs =
	    map (fn (out,idx) => (Term.sym2curname out,idx)) (Util.addCount outargs)
    in
	[Layout.align (map declare_output output_symbol_pairs),
	 $("{"),
	 SUB [$("// Calling output " ^ (Symbol.name outname) ^ " of class " ^ (Symbol.name classname)),
	      $("// " ^ (e2s exp)),
	      inps_decl,
	      Layout.align inps_init,
	      sysstates_init,
	      outs_decl,
	      $(calling_name ^ "(" ^ iter_name' ^ "," ^
		statereads ^ systemstatereads ^ inpvar ^ "," ^ outvar ^ ",first_iteration,modelid);"),
	      Layout.align (map assign_output output_symbol_pairs)
	     ],
	 $("}")]
    end
and instanceeq2prog (exp, is_top_class, iter as (iter_sym, iter_type)) =
    let
	val {classname, instname, props, inpargs, outargs} = ExpProcess.deconstructInst exp
	val inpassoc = 
	    case inpargs
	     of [Exp.CONTAINER (Exp.ASSOC tab)] => tab
	      | _ =>
		DynException.stdException(("Inputs of instance call should be an ASSOC container."),
					  "CParallelWriter.class_flow_code.instaceeq2prog",
					  Logger.INTERNAL)


	val orig_instname = instname
	(* every iterator except the update iterator uses an iter_name *)
	val iter_name = Symbol.name (case iter_type of
					 DOF.UPDATE v => v
				       | _ => iter_sym)
	val iter_name' = Symbol.name (case iter_type of
					  DOF.UPDATE v => v
					| DOF.ALGEBRAIC (_,v) => v
					| _ => iter_sym)

	val instclass = CurrentModel.classname2class classname
	val iterators = map (fn(sym, _)=>sym) (CurrentModel.iterators())
	val statereads_top = "&rd_" ^ (iter_name) ^ "[STRUCT_IDX]." ^ (Symbol.name orig_instname)
			     
	val statewrites_top = "&wr_" ^ (iter_name) ^ "[STRUCT_IDX]." ^ (Symbol.name orig_instname)

	val systemdata = Unique.unique "subsys_rd"

	val dereference = if is_top_class then "[STRUCT_IDX]." else "->"

	val (statereads, statewrites, systemstatereads) =
	    (if reads_iterator iter instclass then "&rd_" ^ (iter_name) ^ dereference ^ (Symbol.name orig_instname) ^ ", " else "",
	     if writes_iterator iter instclass then "&wr_" ^ (iter_name) ^ dereference ^ (Symbol.name orig_instname) ^ ", " else "",
	     if reads_system instclass then "&" ^ systemdata ^ ", " else "")
	    

	fun systemstatedata_iterator (iter as (iter_name, _)) =
	    systemdata^"."^(Symbol.name iter_name)^" = sys_rd->"^(Symbol.name iter_name)^";"
	and systemstatedata_states (iter as (iter_name, _)) =
	    [systemdata^"."^"states_"^(Symbol.name iter_name)^" = &sys_rd->states_"^(Symbol.name iter_name)^"[STRUCT_IDX]."^(Symbol.name orig_instname)^";",
	     systemdata^"."^"states_"^(Symbol.name iter_name)^"_next = &sys_rd->states_"^(Symbol.name iter_name)^"_next[STRUCT_IDX]."^(Symbol.name orig_instname)^";"]

	val iters = List.filter (fn (it) => (not (ModelProcess.isImmediateIterator it)) andalso (ClassProcess.requiresIterator it instclass)) (ModelProcess.returnIndependentIterators ())
	val state_iters = List.filter (fn it => reads_iterator it instclass) (ModelProcess.returnStatefulIterators ())

	val sysstates_init = [$("systemstatedata_"^(Symbol.name (ClassProcess.class2preshardname instclass))^" "^systemdata^";"),
			      $("// iterator pointers"),
			      SUB(map ($ o systemstatedata_iterator) iters),
			      $("// state pointers"),
			      SUB(map $ (Util.flatmap systemstatedata_states state_iters))]

	val calling_name = "flow_" ^ (Symbol.name classname)

	val inpvar = if SymbolTable.null inpassoc then "NULL" else Unique.unique "inputdata"
	val outvar = if List.null outargs then "NULL" else Unique.unique "outputdata"
							   

	val (inps_init,num_inps) = 
	    SymbolTable.foldli
		(fn (k,v,(acc,idx)) =>
		    ($(inpvar ^ "[" ^ (i2s idx) ^ "] = " ^ (CWriterUtil.exp2c_str v) ^ "; // " ^ (Symbol.name k)) :: acc,
		     1+idx)
		    ) (nil,0) inpassoc

	val inps = 
	    case num_inps
	     of 0 => nil
	      | n => [$("CDATAFORMAT " ^ inpvar ^ "[" ^ (i2s n) ^ "];")]

	val outs_decl = 
	    if List.null outargs then []
	    else [$("CDATAFORMAT " ^ outvar ^ "["^(i2s (List.length outargs))^"];")]

	fun declare_output ((sym,_),_) = "CDATAFORMAT "^(Symbol.name sym)^";"

	fun assign_output ((sym, output), idx) =
	    (Symbol.name sym) ^ " = " ^ outvar ^ "[" ^ (i2s idx) ^ "];" ^
	    " // Mapped to "^ (Symbol.name classname) ^ ": " ^ (e2s (List.hd (DOF.Output.contents output)))

	(* Output args could contain don't cares *)
	val output_term_pairs = nil
	(*
	 Util.addCount (ListPair.zipEq (outargs, !(#outputs instclass)))
	 handle ListPair.UnequalLengths =>
		DynException.stdException(("Outputs of instance call and instance class are not the same length."),
					  "CParallelWriter.class_flow_code.instaceeq2prog",
					  Logger.INTERNAL)
	 *)
	val output_symbol_pairs =
	    List.mapPartial (fn((outarg,outs),n)=> if Term.isSymbol outarg then 
						       SOME ((Term.sym2curname outarg, outs), n)
						   else 
						       NONE)
			    output_term_pairs
    in
	(map ($ o declare_output) output_symbol_pairs) @
	[$("{"),
	 SUB([$("// Calling instance class " ^ (Symbol.name classname)),
	      $("// " ^ (e2s exp))] @ 
	     inps @
	     inps_init @ 
	     (if reads_system instclass then
		  sysstates_init 
	      else [] ) @
	     outs_decl @
	     [$(calling_name ^ "("^iter_name'^", "^
		statereads ^ statewrites ^ systemstatereads ^ 
		inpvar^", "^outvar^", first_iteration, modelid);")
	     ] @
	     map ($ o assign_output) output_symbol_pairs),
	 $("}"),$("")]
    end
    handle e => DynException.checkpoint "CParallelWriter.class_flow_code.instanceeq2prog" e


fun class_output_code (class, is_top_class, iter as (iter_sym, iter_type)) output =
    if is_top_class then 
	Layout.empty
    else
	let
	    val orig_name = ClassProcess.class2preshardname class
	    (* every iterator except the update iterator uses an iter_name *)
	    val iter_name = Symbol.name (case iter_type of
					     DOF.UPDATE v => v
					   | _ => iter_sym)
	    val iter_name' = Symbol.name (case iter_type of
					      DOF.UPDATE v => v
					    | DOF.ALGEBRAIC (_,v) => v
					    | _ => iter_sym)
			     
	    val (statereadprototype,
		 systemstatereadprototype) =
		(if reads_iterator iter class then
		     (*"const " ^ *)"statedata_" ^ (Symbol.name orig_name) ^ "_" ^ iter_name ^ " *rd_" ^ iter_name ^ ", "
		 else "",
		 if reads_system class then
		     "const systemstatedata_"^(Symbol.name orig_name)^" *sys_rd, "
		 else "")

	    val input_automatic_var =
	     fn (input,i) => 
		$("CDATAFORMAT " ^ (CWriterUtil.exp2c_str (Exp.TERM input)) ^ " = inputs[" ^ (i2s i) ^ "];")

	    val inputs = 
		Util.addCount (!(DOF.Output.inputs output))

	    val read_inputs_progs =
		Layout.align
		    ($("// mapping inputs to variables") ::
		     (map input_automatic_var inputs))

	    val output_symbols = 
		SymbolSet.fromList 
		    (map Term.sym2curname
			 (List.filter (fn term => not ((Term.isReadState term) orelse 
						       (Term.isReadSystemState term) orelse
						       (Term.isReadSystemIterator term) orelse
						       (Term.isIterator term) orelse
						       (ClassProcess.isTermInput class term)))
				      (Util.flatmap ExpProcess.exp2termsymbols (DOF.Output.contents output))))
	    val extra_equations = 
		SymbolSet.foldl
		    (fn (sym,acc) =>
			(exp2prog (ClassProcess.flattenEq class sym,is_top_class,iter)) @ acc
		    ) nil output_symbols

	    val write_outputs_progs =
		Layout.align
		    ($("// writing outputs") ::
		     map (fn (exp,i) =>
			     $("outputs["^(i2s i)^"] = " ^ 
			       (CWriterUtil.exp2c_str exp) ^ 
			       ";")
			 ) (Util.addCount (DOF.Output.contents output)))
	    val classTypeName = ClassProcess.classTypeName class
	    val (inputs_type,outputs_type) = ((Symbol.name classTypeName) ^ "_input", (Symbol.name classTypeName) ^ "_output")
	in
	    Layout.align 
		[$("__HOST__ __DEVICE__ int output_" ^ (Symbol.name (#name class)) ^ "_" ^ (Term.sym2name (DOF.Output.name output)) ^
		   "(CDATAFORMAT "^iter_name'^", " ^ statereadprototype ^ systemstatereadprototype ^
		   " "^inputs_type^" *input, "^outputs_type^" *output, const unsigned int first_iteration, const unsigned int modelid) {"),
		 SUB [read_inputs_progs,
		      Layout.align extra_equations,
		      write_outputs_progs],
		 $("}")]
	end	

fun class_flow_code (class, is_top_class, iter as (iter_sym, iter_type)) =
    let
	(*val _ = Util.log("Generating code for class '"^(Symbol.name (#name class))^"'")
	val _ = DOFPrinter.printClass class*)

	(* we need to add EP indices if this is the top class *)
	val _ = ClassProcess.addEPIndexToClass is_top_class class
	(*val _ = Util.log("After adding EP indices")
	val _ = DOFPrinter.printClass class*)

	(* the original name here refers to the class name with all of the states - if it's a master class, it's that class, otherwise it's the master class 
	   that the slave class points to.  class2orig_name looks at the classtype property to determine what the original name should be.*)
	val orig_name = (*ClassProcess.class2basename class*) ClassProcess.class2preshardname class

	(* val has_states = case iter_type of  *)
	(* 		     DOF.UPDATE _ => true *)
	(* 		   | _ => ClassProcess.class2statesize class > 0 *)


	(* every iterator except the update iterator uses an iter_name *)
	val iter_name = Symbol.name (case iter_type of
					 DOF.UPDATE v => v
				       | _ => iter_sym)
	val iter_name' = Symbol.name (case iter_type of
					 DOF.UPDATE v => v
				       | DOF.ALGEBRAIC (_,v) => v
				       | _ => iter_sym)
			
	val (statereadprototype,
	     statewriteprototype,
	     systemstatereadprototype) =
	    (if reads_iterator iter class then
		 (*"const " ^ *)"statedata_" ^ (Symbol.name orig_name) ^ "_" ^ iter_name ^ " *rd_" ^ iter_name ^ ", "
	     else "",
	     if writes_iterator iter class then
		 "statedata_" ^ (Symbol.name orig_name) ^ "_" ^ iter_name ^ " *wr_" ^ iter_name ^ ", "
	     else "",
	     if reads_system class then
		 "const systemstatedata_"^(Symbol.name orig_name)^" *sys_rd, "
	     else "")


	val useMatrixForm = ModelProcess.requiresMatrixSolution (iter_sym, iter_type)


			    



	val read_memory_progs = []

	val read_states_progs = []

	(* filter out all the unneeded expressions *)
	val (initvalue_exps, rest_exps) = List.partition ExpProcess.isInitialConditionEq (!(#exps class))
	val (valid_exps, rest_exps) = List.partition (fn(exp)=> ExpProcess.isIntermediateEq exp orelse
							        ExpProcess.isInstanceEq exp orelse
								ExpProcess.isOutputEq exp orelse
							        ExpProcess.isStateEq exp orelse
								ExpProcess.isReadStateEq exp) rest_exps
	val _ = if (List.length rest_exps > 0) then
		    (Util.log ("Internal Error: Invalid expressions reached in code writer while writing class " ^ (Symbol.name (ClassProcess.class2orig_name class)));
		     app (fn(exp)=> Util.log ("  Offending expression: " ^ (e2s exp))) rest_exps;
		     DynException.stdException("Invalid expression(s) in code writer", "CParallelWriter.class_flow_code", Logger.INTERNAL))
		else
		    ()


	fun layoutExpressionConstants exp =
	    if ExpProcess.isIntermediateEq exp then
		layoutIntermediateEquationConstants exp
	    else
		Layout.empty

	and layoutIntermediateEquationConstants exp =
	    if ExpProcess.isMatrixEq exp then
		layoutMatrixEquationConstants exp
	    else
		Layout.empty

	and layoutMatrixEquationConstants exp =
	    let
		val m = case Container.expMatrixToMatrix (ExpProcess.rhs exp) of
			    m as ref (Matrix.DENSE _) => 
			    let
			    (*val _ = print ("intermediate matrix eq -> ")
			     val _ = Matrix.print m*)
			    in
				m
			    end
			  | m as ref (Matrix.BANDED _) => 
			    let
				val bands = Matrix.toPaddedBands m
				val m' = Matrix.fromRows (Exp.calculus()) bands
				(*val _ = print ("matrix bands -> ")
				 val _ = Matrix.print m'*)
				val _ = Matrix.transpose m'
			    (*val _ = print ("matrix bands (transposed) -> ")
			     val _ = Matrix.print m'*)

			    in
				m'
			    end

		val (rows,cols) = Matrix.size m

		(* Emits non-constant values as NAN *)
		fun emitCell (exp as Exp.TERM t) =
		    (case t 
		      of Exp.BOOL _ => exp
		       | Exp.INFINITY => exp
		       | Exp.INT _ => exp
		       | Exp.NAN => exp
		       | Exp.RATIONAL _ => exp
		       | Exp.REAL _ => exp
		       | _ => Exp.TERM Exp.NAN)
		  | emitCell _ = Exp.TERM Exp.NAN

		val cells = Matrix.mapi (fn (i, j, exp) => $ (CWriterUtil.exp2c_str (emitCell exp))) m
	    in
		let open Layout in
		    align [$ "#if defined TARGET_GPU",
			   $ ("__constant__ __DEVICE__ CDATAFORMAT device_matrix_constants_" ^ iter_name ^ "["^(i2s cols)^"*"^(i2s rows)^"];"),
			   $ "#endif",
			   assign ($ ("CDATAFORMAT host_matrix_constants_" ^ iter_name ^ "[]"), 
				   series ("{", "}", ",") cells)]
		end
	    end


	val classTypeName = ClassProcess.classTypeName class
	val (inputs_type,outputs_type) = ((Symbol.name classTypeName) ^ "_input", (Symbol.name classTypeName) ^ "_output")

	val header_progs = 
	    (map layoutExpressionConstants valid_exps) @
	    (if useMatrixForm then
		[$("#if defined TARGET_GPU"),
		 $("#define INTERNAL_C device_matrix_constants_"^iter_name'),
		 $("#else"),
		 $("#define INTERNAL_C host_matrix_constants_"^iter_name'),
		 $("#endif"),
		 $("__HOST__ __DEVICE__ int flow_" ^ (Symbol.name (#name class)) ^ 
		   "(CDATAFORMAT "^iter_name'^", " ^ statereadprototype ^ "CDATAFORMAT *INTERNAL_M, CDATAFORMAT *INTERNAL_b, " ^ systemstatereadprototype ^
		   " "^inputs_type^" *input, "^outputs_type^" *output, const unsigned int first_iteration, const unsigned int modelid) {")]
	    else
		[$("__HOST__ __DEVICE__ int flow_" ^ (Symbol.name (#name class)) ^ 
		   "(CDATAFORMAT "^iter_name'^", " ^ statereadprototype ^ statewriteprototype ^ systemstatereadprototype ^
		   " "^inputs_type^" *input, "^outputs_type^" *output, const unsigned int first_iteration, const unsigned int modelid) {")]
	    )


	val input_automatic_var =
	    fn (input,i) =>
	       assign ($("CDATAFORMAT " ^ (CWriterUtil.exp2c_str (Exp.TERM (DOF.Input.name input)))),
		       $("input->" ^ (CWriterUtil.exp2c_str (Exp.TERM (DOF.Input.name input))) ^ "[threadid]"))
	    (* if is_top_class then *)
	    (* 	fn (input,i) =>  *)
	    (* 	   $("CDATAFORMAT " ^ (CWriterUtil.exp2c_str (Exp.TERM (DOF.Input.name input))) ^ " = get_input(" ^ (i2s i) ^ ", modelid);") *)
	    (* else *)
	    (* 	fn (input,i) =>  *)
	    (* 	   $("CDATAFORMAT " ^ (CWriterUtil.exp2c_str (Exp.TERM (DOF.Input.name input))) ^ " = inputs[" ^ (i2s i) ^ "];") *)

	val eqn_symbolset = 
	    SymbolSet.flatmap ExpProcess.exp2symbolset valid_exps

	val inputs = 
	    if is_top_class then
		let
		    (* Impose an ordering on inputs so that we can determine which are constant vs. time-varying. *)
		    fun is_constant_input input =
			let
			    val name = DOF.Input.name input
			in
			    not (isSome (TermProcess.symbol2temporaliterator name))
			end

		    val (constant_inputs, sampled_inputs) = List.partition is_constant_input (!(#inputs class))
		in
		    Util.addCount (constant_inputs @ sampled_inputs)
		end
	    else
		Util.addCount (!(#inputs class))

	val read_inputs_progs =
	    [$(""),
	     $("// mapping inputs to variables")] @ 
	    (map input_automatic_var inputs)


	val equ_progs = 
	    [$(""),
	     $("// writing all intermediate, instance, and differential equation expressions")] @
	    (Util.flatmap (fn(exp)=> (exp2prog (exp,is_top_class,iter))) valid_exps)
	    
	val state_progs = []

        val output_progs = 
            if is_top_class then
                let fun cmp (a, b) = Term.sym2curname a = Term.sym2curname b
                    val outputs_symbols = Util.uniquify_by_fun cmp (ClassProcess.outputsSymbols class)
                    val (iterators_symbols, outputs_symbols) = List.partition Term.isIterator outputs_symbols
                in
                    if List.null outputs_symbols andalso List.null iterators_symbols then
                        [$("// No outputs written")]
                    else
                        [$(""),
                         $("// writing output variables"),
			 $("// FIXME use the appropriate output function instead"),
                         $("#if NUM_OUTPUTS > 0"),
                         $("if (first_iteration) {"),
                         SUB($("output_data *od = (output_data*)outputs;") 
                             (* FIXME this is a temporary hack to catch reads of system iterator values. *)
                             :: (map (fn t => $("od[modelid]." ^ (Symbol.name (Term.sym2curname t)) ^ " = " ^
                                                (if (Symbol.symbol iter_name) = (Term.sym2curname t) then
                                                     (CWriterUtil.exp2c_str (Exp.TERM t))
                                                 else ("sys_rd->" ^ (Symbol.name (Term.sym2curname t)) ^ "[ARRAY_IDX]")) ^ ";"))
                                     iterators_symbols) 
                             @ (map (fn(t)=> $("od[modelid]." ^ ((Symbol.name o Term.processInternalName o Term.sym2curname) t) ^ " = " ^ (CWriterUtil.exp2c_str (Exp.TERM t)) ^ ";"))
                                    outputs_symbols)),
                         $("}"),
                         $("#endif")]
                end
	    else nil


        val mapping_back_progs = []
    in
	header_progs @
	[SUB(read_states_progs @
	     read_inputs_progs @
	     equ_progs @
	     state_progs @
	     output_progs @
	     mapping_back_progs @
	     [$(""),
	      $("return 0;")]),
	 $("}"),
	 $("#undef INTERNAL_C"),
	 $("")]
    end
    handle e => DynException.checkpoint "CParallelWriter.class_flow_code" e

fun state_init_code shardedModel iter_sym =
    let 
	val model as (classes, {classname=top_class,...} ,_) = ShardedModel.toModel shardedModel iter_sym
	val iter as (_,iter_type) = ShardedModel.toIterator shardedModel iter_sym
	val iter_name = Symbol.name (case iter_type of
					 DOF.UPDATE v => v
				       | _ => iter_sym)

	fun stateInitPrototype class =
	    let
		val classname = ClassProcess.class2classname class
		val basename = classname
		val mastername = ClassProcess.class2preshardname class
		val classTypeName = ClassProcess.classTypeName class

		val (inputs, writes) =
		    ((Symbol.name classTypeName) ^ "_input *inputs",
		     (Symbol.name classTypeName) ^ "_state *wr_"^iter_name)
	    in
		"__HOST__ int init_states_" ^ (Symbol.name (#name class)) ^
		"(" ^ inputs ^ ", " ^ writes ^ ", const unsigned int modelid, const unsigned int threadid)"
	    end

	fun stateInitFunction (iter as (iter_sym, iter_typ)) isTopClass class =
	    let
		val iter_name = Symbol.name (case iter_type of
						 DOF.UPDATE v => v
					       | _ => iter_sym)

		val inputs = 
		    if isTopClass then
			let
			    (* Impose an ordering on inputs so that we can determine which are constant vs. time-varying. *)
			    fun is_constant_input input =
				let
				    val name = DOF.Input.name input
				in
				    not (isSome (TermProcess.symbol2temporaliterator name))
				end

			    val (constant_inputs, sampled_inputs) = List.partition is_constant_input (!(#inputs class))
			in
			    Util.addCount (constant_inputs @ sampled_inputs)
			end
		    else
			Util.addCount (!(#inputs class))

		fun inputLocalVar (input,ix) =
		    let
			val name = CWriterUtil.exp2c_str (Exp.TERM (DOF.Input.name input))
		    in
			assign($("CDATAFORMAT " ^ name),
			       $("inputs->"^name^"[threadid]"))
		    end

		val ivqCode = ClassProcess.foldInitialValueEquations
				  (fn (eqn, code) =>
				      if ExpProcess.isEquation eqn then
					  let
					      val eqn' =  ClassProcess.flattenExpressionThroughInstances class eqn
					  in
					      ($((CWriterUtil.exp2c_str eqn') ^ ";")) :: code
					  end
				      else
					  DynException.stdException(("Unexpected non equation present '"^(e2s eqn)^"'"),
								    "CParallelWriter.stateInitCode.stateInitFunction",
								    Logger.INTERNAL)
				  )
				  nil class

		val instCode = ClassProcess.foldInstanceEquations
			 (fn (eqn, code) => 
			     let
				 val {classname, instname, props, inpargs, outargs} = ExpProcess.deconstructInst eqn
				 val instclass = CurrentModel.classname2class classname
				 val orig_instname = instname

				 val inpassoc = 
				     case inpargs
				      of [Exp.CONTAINER (Exp.ASSOC tab)] => tab
				       | _ =>
					 DynException.stdException(("Inputs of output call should be an ASSOC container."),
								   "CParallelWriter.class_flow_code.instaceeq2prog",
								   Logger.INTERNAL)

				 val sysreadsName = Unique.unique "subsys_rd"

				 fun systemstatedata_iterator (iter as (iter_name, _)) =
				     sysreadsName^"."^(Symbol.name iter_name)^" = sys_rd->"^(Symbol.name iter_name)^";"
				 and systemstatedata_states (iter as (iter_name, _)) =
				     [sysreadsName^"."^"states_"^(Symbol.name iter_name)^" = &sys_rd->states_"^(Symbol.name iter_name)^"[STRUCT_IDX]."^(Symbol.name orig_instname)^";",
				      sysreadsName^"."^"states_"^(Symbol.name iter_name)^"_next = &sys_rd->states_"^(Symbol.name iter_name)^"_next[STRUCT_IDX]."^(Symbol.name orig_instname)^";"]

				 val iters = List.filter (fn (it) => (not (ModelProcess.isImmediateIterator it)) andalso (ClassProcess.requiresIterator it instclass)) (ModelProcess.returnIndependentIterators ())
				 val state_iters = List.filter (fn it => reads_iterator it instclass) (ModelProcess.returnStatefulIterators ())

				 val initSysreads =
				     [$("systemstatedata_"^(Symbol.name (ClassProcess.class2preshardname instclass))^" "^sysreadsName^";"),
				      $("// iterator pointers"),
				      SUB(map ($ o systemstatedata_iterator) iters),
				      $("// state pointers"),
				      SUB(map $ (Util.flatmap systemstatedata_states state_iters))]

				 val dereference = if isTopClass then "[STRUCT_IDX]." else "->"
											   
				 (* This may not be a sufficient test.
				  * What if the expression refers to another expression
				  * which, in turn, reads the input? *)
				 fun ivqReadsInput name eqn =
				     let val symset = ExpProcess.exp2symbolset eqn
				     in
					 SymbolSet.member (symset, Term.sym2curname name)
				     end
				     
				 val inpnames: Exp.term list = 
				     rev (ClassProcess.foldInputs (fn (input, names) => (DOF.Input.name input) :: names) nil instclass)

				 val (inputs, writes) = 
				     ("inputs->"^(Symbol.name instname),
				      "wr_mdlvar__t->"^(Symbol.name instname))

				 val inpvar = if SymbolTable.null inpassoc then "NULL" else Unique.unique "inputdata"

				 val (inps_init,num_inps) = 
				     SymbolTable.foldli
					 (fn (k,v,(acc,idx)) =>
					     let
						 val value = 
						     if hasInitialValueEquation (fn exp => Match.exists (Match.asym (Util.sym2codegensym k)) exp) instclass then
							 CWriterUtil.exp2c_str v
						     else "NAN"
					     in
						 ($(inpvar ^ "[" ^ (i2s idx) ^ "] = " ^ value ^ "; // " ^ (Symbol.name k)) :: acc,
						  1+idx)
					     end
					 ) (nil,0) inpassoc

				 val inps = 
				     case num_inps
				      of 0 => nil
				       | n => [$("CDATAFORMAT " ^ inpvar ^ "[" ^ (i2s n) ^ "];")]

			     in
				 if hasInitialValueEquation (fn _ => true) instclass then
				     [$("{ // Initializing instance class " ^ (Symbol.name classname)),
				      SUB([$("// " ^ (ExpPrinter.exp2str eqn)),
					   $("init_states_" ^ (Symbol.name classname) ^ "(" ^
					     inputs ^ ", " ^ writes ^ ", modelid, threadid);")]),
				      $("}")] @
				     code
				 else code
			     end)
			 nil class
	    in
		[$((stateInitPrototype class) ^ "{"),
		 SUB(map inputLocalVar inputs),
		 SUB(ivqCode),
		 SUB(instCode),
		 SUB[$("return 0;")],
		 $("}")]
	    end
    in
	CurrentModel.withModel
	    model
	    (fn _ =>
		let
		    val topclass = CurrentModel.classname2class top_class
		    val ivqClasses = List.filter (hasInitialValueEquation (fn _ => true)) classes
		    val prototypes = map ((fn p => $(p^";")) o stateInitPrototype) ivqClasses
		    val functions = map (fn c => stateInitFunction iter (#name c = #name topclass) c) ivqClasses
		in
		    (prototypes, List.concat functions)
		end)
    end


fun flow_code shardedModel iter_sym = 
    let
	val model as (classes, {classname=top_class,...} ,_) = ShardedModel.toModel shardedModel iter_sym
	val iter as (_,iter_type) = ShardedModel.toIterator shardedModel iter_sym
	(* every iterator except the update iterator uses an iter_name *)
	val iter_name = Symbol.name (case iter_type of
					 DOF.UPDATE v => v
				       | _ => iter_sym)
	val iter_name' = Symbol.name (case iter_type of
					  DOF.UPDATE v => v
					| DOF.ALGEBRAIC (_,v) => v
					| _ => iter_sym)

	val eval_iterators = ModelProcess.returnDependentIterators ()

	fun class_prototypes class =
	    let
		val classname = ClassProcess.class2classname class
		val basename = classname
		val mastername = ClassProcess.class2preshardname class
		val classTypeName = ClassProcess.classTypeName class

		val (statereadprototype,
		     statewriteprototype,
		     systemstatereadprototype) =
		    (if reads_iterator iter class then
			 (*"const " ^ *)"statedata_" ^ (Symbol.name mastername) ^ "_" ^ iter_name ^ " *rd_" ^ iter_name ^ ", "
		     else "",
		     if writes_iterator iter class then
			 "statedata_" ^ (Symbol.name mastername) ^ "_" ^ iter_name ^ " *wr_" ^ iter_name ^ ", "
		     else "",
		     if reads_system class then
			 "const systemstatedata_" ^ (Symbol.name mastername) ^ " *sys_rd, "
		     else "")

		val (inputs_type,outputs_type) = ((Symbol.name classTypeName) ^ "_input", (Symbol.name classTypeName) ^ "_output")

		fun class_flow_prototype class = 
		    let
			val useMatrixForm = ModelProcess.requiresMatrixSolution (iter_sym, iter_type)
		    in
			if useMatrixForm then
			    $("__HOST__ __DEVICE__ int flow_" ^ (Symbol.name (#name class)) ^ 
			      "(CDATAFORMAT "^iter_name'^", " ^ statereadprototype ^ "CDATAFORMAT *INTERNAL_M, CDATAFORMAT *INTERNAL_b, " ^ systemstatereadprototype ^
			      " "^inputs_type^" *input, "^outputs_type^" *output, const unsigned int first_iteration, const unsigned int modelid);")
			else
			    $("__HOST__ __DEVICE__ int flow_" ^ (Symbol.name (#name class)) ^ 
			      "(CDATAFORMAT "^iter_name'^", " ^ statereadprototype ^ statewriteprototype ^ systemstatereadprototype ^
			      " "^inputs_type^" *input, "^outputs_type^" *output, const unsigned int first_iteration, const unsigned int modelid);")
		    end

		and class_output_prototype class output =
		    $("__HOST__ __DEVICE__ int output_" ^ (Symbol.name (#name class)) ^ "_" ^ (Term.sym2name (DOF.Output.name output)) ^
		      "(CDATAFORMAT "^iter_name'^", " ^ statereadprototype ^ systemstatereadprototype ^
		      " "^inputs_type^" *input, "^outputs_type^" *output, const unsigned int first_iteration, const unsigned int modelid);")
	    in
		Layout.align ((class_flow_prototype class) ::
			      (map (class_output_prototype class) (!(#outputs class))))
	    end
	    
    (*val (fun_prototypes, fun_wrappers) = ListPair.unzip (map (fn(iter)=>flow_wrapper (topclass, iter)) eval_iterators)*)
    (*val (fun_prototype, fun_wrapper) = flow_wrapper (topclass, iter)*)

    in
	CurrentModel.withModel model 
			       (fn () =>
				   let
				       val topclass = CurrentModel.classname2class top_class

    				       val fundecl_progs = map class_prototypes classes
							   
				       val flow_progs = Util.flatmap (fn(c)=> class_flow_code (c,#name c = #name topclass, iter)) classes
				       val output_progs = Util.flatmap (fn c => map (class_output_code (c,#name c = #name topclass, iter)) (!(#outputs c))) classes
				   in
				       ([$("// Functions prototypes for flow code of "^(Symbol.name iter_sym))] @ 
					fundecl_progs, 
					flow_progs @ output_progs)
				   end
				   handle e => DynException.checkpoint "CParallelWriter.flow_code.anonymous_fun" e)
    end
    handle e => DynException.checkpoint "CParallelWriter.flow_code" e

fun init_states shardedModel =
    let
	fun subsystem_init_call iter_sym =
	    let val model as (_, {classname=top_class,...}, _) = ShardedModel.toModel shardedModel iter_sym
		val iter as (_,iter_type) = ShardedModel.toIterator shardedModel iter_sym
		val iter_name = Symbol.name (case iter_type of
						 DOF.UPDATE v => v
					       | _ => iter_sym)

		val requiresMatrix = ModelProcess.requiresMatrixSolution iter
	    in CurrentModel.withModel
		   model (fn _ =>
			     let val class = CurrentModel.classname2class top_class
				 val basename = ClassProcess.class2preshardname class
				 val (reads, writes, sysreads) =
				     ("(statedata_" ^ (Symbol.name basename) ^ "_" ^ iter_name ^ " *)(props->system_states->states_" ^ (Symbol.name iter_sym) ^ ")",
				      "(statedata_" ^ (Symbol.name basename) ^ "_" ^ iter_name ^ " *)(props->system_states->states_" ^ (Symbol.name iter_sym) ^ "_next)",
				      if reads_system class then
					  "(const systemstatedata_" ^ (Symbol.name basename) ^ " *)props->system_states"
				      else "NULL")
			     in
				 if hasInitialValueEquation (fn _ => true) class then
				     $("if (0 != init_states_" ^ (Symbol.name top_class) ^ "(" ^ 
				       reads ^ ", " ^ writes ^ ", " ^ sysreads ^ ", " ^ 
				       "NULL, modelid)) { return 1; }")
				 else
				     $("// no initial values for " ^ (Symbol.name top_class))
			     end)
	    end

    in
	[$("__HOST__ int init_states(solver_props *props, const unsigned int modelid){"),
	 SUB(map subsystem_init_call (ShardedModel.iterators shardedModel)),
	 SUB[$("return 0;")],
	 $("}"),
	 $("")]
    end


(* TODO remove the iterval parameter from IMMEDIATE flows. *)
fun model_flows shardedModel = 
    let
	fun subsystem_flow_call iter_sym =
	    let val model as (_, {classname=top_class,...}, _) = ShardedModel.toModel shardedModel iter_sym
		val iter as (_, iter_type) = ShardedModel.toIterator shardedModel iter_sym
		val iter_name = Symbol.name (case iter_type of
						 DOF.UPDATE v => v
					       | _ => iter_sym)

		val requiresMatrix = ModelProcess.requiresMatrixSolution iter
	    in CurrentModel.withModel model (fn _ =>
	       let val class = CurrentModel.classname2class top_class
		   val basename = ClassProcess.class2preshardname class
		   val (statereads, statewrites, systemstatereads) =
		       (if reads_iterator iter class then "("(*^"const "*)^"statedata_" ^ (Symbol.name basename) ^ "_" ^ iter_name ^ "* )y, " else "",
			if requiresMatrix then
			    "(CDATAFORMAT* ) props->mem, dydt, "
			else if writes_iterator iter class then 
			    "(statedata_" ^ (Symbol.name basename) ^ "_" ^ iter_name ^ "* )dydt, " 
			else 
			    "",
			if reads_system class then "(const systemstatedata_" ^ (Symbol.name basename) ^ " *)props->system_states, " else "")
	       in
		SUB[$("case ITERATOR_" ^ (Util.removePrefix (Symbol.name iter_sym)) ^ ":"),
		    $("return flow_" ^ (Symbol.name top_class) ^ 
		      "(iterval, " ^ statereads ^ statewrites ^ systemstatereads ^ "NULL, (CDATAFORMAT *)props->od, first_iteration, modelid);")]
	       end)
	    end

    in
	[$"",
	 $("__HOST__ __DEVICE__ int model_flows(CDATAFORMAT iterval, "(*const *)^"CDATAFORMAT *y, CDATAFORMAT *dydt, solver_props *props, const unsigned int first_iteration, const unsigned int modelid){"),
	 SUB($("switch(props->iterator){") ::
	     (map subsystem_flow_call (ShardedModel.iterators shardedModel)) @
	     [$("default: return 1;"),
	      $("}")]
	    ),
	 $("}"),
	 $("")]
    end
    handle e => DynException.checkpoint "CParallelWriter.model_flows" e


fun output_code (filename, block) =
    let
      val _ = Logger.log_notice (Printer.$("Generating C source file '"^ filename ^"'"))
      val file = TextIO.openOut (filename)
    in
	Printer.printLayout (Layout.align block) file
	
      (* printer.printtexts (file, block, 0) *)
      before TextIO.closeOut file
      handle exn => (TextIO.closeOut file; raise exn)
    end

fun logoutput_code shardedModel =
    let
	val iterators = CurrentModel.iterators()
	fun iter_sym2type sym = 
	    case List.find (fn(iter_sym,_)=>iter_sym=sym) iterators of
		SOME (_,iter_type) => iter_type
	      | NONE => DynException.stdException(("Can't find iterator '"^(Symbol.name sym)^"'"), "CParallelWriter.logoutput_code", Logger.INTERNAL)

	fun isContinuousIterator (iter_sym) = 
	    case iter_sym2type iter_sym of
		DOF.CONTINUOUS _ => true
	      | DOF.DISCRETE _ => false
	      | DOF.UPDATE v => List.exists (fn(iter_sym',iter_type')=> v = iter_sym' andalso (case iter_type' of DOF.CONTINUOUS _ => true | _ => false)) iterators
	      | DOF.ALGEBRAIC (_,v) => List.exists (fn(iter_sym',iter_type')=> v = iter_sym' andalso (case iter_type' of DOF.CONTINUOUS _ => true | _ => false)) iterators
	      | DOF.IMMEDIATE => false
	fun isDiscreteIterator (iter_sym) = 
	    case iter_sym2type iter_sym of
		DOF.CONTINUOUS _ => false
	      | DOF.DISCRETE _ => true
	      | DOF.UPDATE v => List.exists (fn(iter_sym',iter_type')=> v = iter_sym' andalso (case iter_type' of DOF.DISCRETE _ => true | _ => false)) iterators
	      | DOF.ALGEBRAIC (_,v) => List.exists (fn(iter_sym',iter_type')=> v = iter_sym' andalso (case iter_type' of DOF.DISCRETE _ => true | _ => false)) iterators
	      | DOF.IMMEDIATE => false

	(*val orig_name = Symbol.name (ClassProcess.class2basename class)*)
	val outputs = ShardedModel.toOutputs shardedModel
	val dependent_symbols = CWriterUtil.outputs2uniqueoutputsymbols outputs
	val sym_decls = map
			    (fn(term, sym)=> 
			       let
				   val local_scope = case term of
							 Exp.SYMBOL (_, props) => (case Property.getScope props of
										       Property.LOCAL => true
										     | _ => false)
						       | _ => DynException.stdException (("Unexpected non symbol"), "CParallelWriter.logoutput_code", Logger.INTERNAL)
			       in
				   if local_scope then
				       $("CDATAFORMAT " ^ (Symbol.name sym) ^ " = outputsave_" ^ (Symbol.name sym) ^ ";")
				   else
				       $("CDATAFORMAT " ^ (Symbol.name sym) ^ " = " ^ (CWriterUtil.exp2c_str (Exp.TERM term)) ^ ";")
			       end)
			    dependent_symbols


	fun output_prog (output, index) =
	    let val (name, contents, condition) = (DOF.Output.name output, DOF.Output.contents output, DOF.Output.condition output)
		val num_quantities = 
		    case TermProcess.symbol2temporaliterator name
		     of SOME (iter_sym, _) => inc (List.length contents)
		      | _ => List.length contents
		fun iter2baseiter iter_sym =
		    let
			val (iter_sym, iter_type) = CurrentModel.itersym2iter iter_sym
		    in
			case iter_type of
			    DOF.CONTINUOUS _ => iter_sym
			  | DOF.DISCRETE _ => iter_sym
			  | DOF.UPDATE iter_sym' => iter_sym'
			  | DOF.ALGEBRAIC (_,iter_sym') => iter_sym'
			  | DOF.IMMEDIATE => iter_sym
		    end

		val cond = 
		    (case ExpProcess.exp2temporaliterator (Exp.TERM name) 
		      of SOME (iter_sym, _) => "(props->iterator == ITERATOR_" ^ (Util.removePrefix (Symbol.name (iter2baseiter iter_sym))) ^ ")"
		       | _ => "1") ^ " && (" ^
		    (CWriterUtil.exp2c_str (ExpProcess.assignToOutputBuffer condition)) ^ ")"

	    in
		[$("{ // Generating output for symbol " ^ (e2s (Exp.TERM name))),
		 SUB[assign($"cond", $(cond)),
		     $("if (cond) {"),
		     SUB([assign($"outputid", $(i2s index)),
			  assign($"outputsize", $(i2s num_quantities)),
			  $("if (ixob) { // Indexed output element buffering"),
			  SUB([$("buffer_indexed_output(modelid,outputid,outputsize,quantities,ixob,threadid,props->blocksize,cond);")]),
			  $("}"),
			  $("output_buffer_data *buf = (output_buffer_data *)ob->ptr[modelid];"),
			  $("buf->outputid = outputid;"),
			  $("buf->num_quantities = outputsize;"),
			  $("")] @
			 (case (ExpProcess.exp2temporaliterator (Exp.TERM name)) of
			      SOME (iter_sym, _) => 
			      (case CurrentModel.itersym2iter iter_sym of
				   (_, DOF.CONTINUOUS _) =>
				   [$("buf->quantities[0] = props->time[modelid];")]
				 | (_, DOF.DISCRETE _) =>
				   [$("buf->quantities[0] = props->time[modelid];")]
				 | (_, DOF.ALGEBRAIC _) =>
				   [$("buf->quantities[0] = props->time[modelid];")]
				 | (_, DOF.IMMEDIATE) =>
				   [$("buf->quantities[0] = props->time[modelid];")]
				 | _ => [$("#error BOGUS ITERATOR NOT FILTERED")])
			    | NONE => []) @
			 (map (fn (exp, idx) =>
				  $("buf->quantities["^(i2s (1 + idx))^"] = " ^ (CWriterUtil.exp2c_str (ExpProcess.assignToOutputBuffer exp))^";"))
			      (Util.addCount contents)) @
			 [$(""),
			  $("ob->ptr[modelid] = buf->quantities + buf->num_quantities;"),
			  $("ob->count[modelid]++;"),
			  $(""),
			  $("assert((void * )(ob->buffer + (modelid * BUFFER_LEN)) <= ob->ptr[modelid]);"),
			  $("assert(ob->end[modelid] <= (void * )(ob->buffer + ((modelid+1) * BUFFER_LEN)));"),
			  $("assert(ob->ptr[modelid] <= ob->end[modelid]);"),
			  $(""),
			  $("ob->full[modelid] |= (max_output_size >= ((unsigned char * )(ob->end[modelid]) - (unsigned char * )(ob->ptr[modelid])));")]),
		     $("}")],
		 $("}")]
	    end

	(* Presumes a CurrentModel.withModel context. *)
	fun outputs_from_class (model, class) =
	    let val {outputs, ...} = class
	    in 
		map (fn (output) => (model, output))
		    (! outputs)
	    end

	val outputs_from_top_classes =
	    Util.flatmap (fn{iter_sym,...} => 
			     let val model as (_, {classname=top_class,...},_) = ShardedModel.toModel shardedModel iter_sym
			     in
				 CurrentModel.withModel model (fn _ => (outputs_from_class (model, CurrentModel.classname2class top_class)))
			     end)
			 (#1 shardedModel)

	val outputs_from_top_classes =
	    Util.uniquify_by_fun (fn ((_,a),(_,b)) => Term.sym2curname (DOF.Output.name a) = Term.sym2curname (DOF.Output.name b)) outputs_from_top_classes

	val output_exps = 
	    Util.flatmap (fn ((model, output), index) => CurrentModel.withModel model (fn _ => output_prog (output, index)))
			 (Util.addCount outputs_from_top_classes)


	fun outputToCount output = 
	    let
		val contentsLength = List.length (DOF.Output.contents output)
	    in
		if contentsLength = 0 then
		    1 (* it has to be at least one, even if contents is empty since it'll return events *)
		else
		    contentsLength
	    end
	val total_output_quantities =
	    List.foldr op+ 0 (map outputToCount outputs)

    in
        if total_output_quantities > 0 then
	[$(""),
	 $("// Output buffers must have at least this much free space to ensure that an output can be written."),
	 $("static const ptrdiff_t max_output_size = MAX_OUTPUT_SIZE;"),
	 $(""),
	 $("__DEVICE__ void buffer_outputs(solver_props *props, unsigned int modelid, indexed_output_buffer *ixob, unsigned int threadid) {"),
	 SUB([$("int cond;"),
	      $("unsigned int outputid, outputsize;"),
	      $("CDATAFORMAT *quantities = NULL;"),
	      $("#ifdef TARGET_GPU"),
	      $("output_buffer *ob = gpu_ob;"),
	      $("#else"),
	      $("output_buffer *ob = &global_ob[global_ob_idx[modelid]];"),
	      $("#endif"),
	      $("output_data *od = (output_data *)props->od;")] @
	     output_exps),
	 $("}"),
	 $("")]
         else
	 []
    end
    handle e => DynException.checkpoint "CParallelWriter.logoutput_code" e

fun buildC (orig_name, shardedModel) =
    let
	val (shards, sysprops) = shardedModel
		    
	fun itersym2iter itersym =
	    ShardedModel.toIterator shardedModel itersym

	val statefulIterators = map #iter_sym
				    (List.filter (ModelProcess.isStatefulIterator o itersym2iter o #iter_sym) shards)
	val outputIterators =  map #iter_sym
				   (List.filter (ModelProcess.isOutputIterator o itersym2iter o #iter_sym) shards)
	val forkedModelsLessUpdate = (List.filter (fn{iter_sym,...}=> case itersym2iter iter_sym of (_, DOF.UPDATE _) => false | _ => true) shards, 
				      sysprops)

	val iteratorsWithSolvers = map #iter_sym
				       (List.filter (not o ModelProcess.isDependentIterator o itersym2iter o #iter_sym) shards)
	val forkedModelsWithSolvers = (List.filter (not o ModelProcess.isDependentIterator o itersym2iter o #iter_sym) shards,
				       sysprops)

	val updateModels = (List.filter (fn {iter_sym, ...} => case itersym2iter iter_sym of (_, DOF.UPDATE _) => true | _ => false) shards,
			    sysprops)

	val algebraicIterators = map #iter_sym
				     (List.filter (fn {iter_sym,...} => case itersym2iter iter_sym of (_, DOF.ALGEBRAIC _) => true | _ => false) shards)
	val preprocessIterators = map #iter_sym
				      (List.filter (fn {iter_sym,...} => case itersym2iter iter_sym of (_, DOF.ALGEBRAIC (DOF.PREPROCESS, _)) => true | _ => false) shards)
	val inprocessIterators = map #iter_sym
				     (List.filter (fn {iter_sym,...} => case itersym2iter iter_sym of (_, DOF.ALGEBRAIC (DOF.INPROCESS, _)) => true | _ => false) shards)
	val postprocessIterators = map #iter_sym
				       (List.filter (fn {iter_sym,...} => case itersym2iter iter_sym of (_, DOF.ALGEBRAIC (DOF.POSTPROCESS, _)) => true | _ => false) shards)

	val class_name = Symbol.name orig_name

	val iterators = ModelProcess.returnIndependentIterators ()
	val iterator_names = map (Symbol.name o #1) iterators
	(* grab the unique solvers so that we can put the code down for each one *)
	val unique_solvers = Util.uniquify (List.mapPartial (fn(_,itertype)=> case itertype of 
										  DOF.CONTINUOUS solver => SOME (Solver.solver2name solver)
										| DOF.DISCRETE _ => SOME "discrete"
										| DOF.IMMEDIATE => SOME "immediate"
										| _ => NONE) iterators)
	val header_progs = header class_name
	val init_solver_props_c = init_solver_props orig_name shardedModel (iteratorsWithSolvers, algebraicIterators)
	val simengine_interface_progs = simengine_interface class_name shardedModel outputIterators
	(*val iteratordatastruct_progs = iteratordatastruct_code iterators*)
	val iodatastruct_progs = iodatastruct_code shardedModel
	val outputdatastruct_progs = outputdatastruct_code shardedModel
	val outputstatestruct_progs = Util.flatmap 
					  (fn(iter_sym) => 
					     let val shard = valOf (ShardedModel.findShard (shardedModel, iter_sym))
						 val model = ShardedModel.toModel shardedModel iter_sym
						 val iterator = ShardedModel.toIterator shardedModel iter_sym
					     in CurrentModel.withModel model (fn _=> outputstatestruct_code (iterator, shard))
					     end)
					  statefulIterators (* pull out just the shards *)
	val systemstate_progs = outputsystemstatestruct_code shardedModel statefulIterators
	val state_init_data = map (state_init_code shardedModel) (ShardedModel.iterators shardedModel)
	val state_init_prototypes = Util.flatmap #1 state_init_data
	val state_init_functions = Util.flatmap #2 state_init_data

	val flow_data = map (flow_code shardedModel) (ShardedModel.iterators shardedModel)
	val fun_prototypes = Util.flatmap #1 flow_data
	val flow_progs = Util.flatmap #2 flow_data

	val logoutput_progs = logoutput_code forkedModelsLessUpdate
	val simengine_api_h = $(Codegen.getC "simengine/simengine_api.h")
	val precision_h = $(Codegen.getC "simengine/precision.h")
	val memory_layout_h = $(Codegen.getC "simengine/memory_layout.h")

	val target_h = 
	    case sysprops 
	     of	{target=Target.CPU, ...} =>
		$(Codegen.getC "simengine/cpu.h")
	      | {target=Target.OPENMP, ...} =>
		$(Codegen.getC "simengine/openmp.h")
	      | {target=Target.CUDA, ...} =>
		$(Codegen.getC "simengine/gpu.h")

	val solvers_h = $(Codegen.getC "solvers/solvers.h")
	val gpu_util_c = $(Codegen.getC "simengine/gpu_util.c")
	val random_c = $(Codegen.getC "simengine/random.c")
	val solver_gpu_cu = $(Codegen.getC ("solvers/solver_gpu.cu"))
	val solver_c = $(String.concat (map
					    (fn(solv)=> Codegen.getC ("solvers/"^solv^".c"))
					    unique_solvers))
	val solver_wrappers_c = solver_wrappers unique_solvers
	val iterator_wrappers_c = 
	    (update_wrapper updateModels) @ 
	    (preprocess_wrapper shardedModel preprocessIterators) @
	    (inprocess_wrapper shardedModel inprocessIterators) @
	    (postprocess_wrapper shardedModel postprocessIterators)
	val simengine_api_c = $(Codegen.getC "simengine/simengine_api.c")
	val defines_h = $(Codegen.getC "simengine/defines.h")
	val seint_h = $(Codegen.getC "simengine/seint.h")
	val parallel_c = $(Codegen.getC "simengine/parallel.c")
	val output_buffer_h = $(Codegen.getC "simengine/output_buffer.h")
	val output_buffer_c = $(Codegen.getC "simengine/output_buffer.c")
	val init_output_buffer_c = $(Codegen.getC "simengine/init_output_buffer.c")
	val inputs_c = $(Codegen.getC "simengine/inputs.c")
	val log_outputs_c = $(Codegen.getC "simengine/log_outputs.c")

	val exec_c = 
	    case sysprops
	     of {target=Target.CPU, ...} =>
		[$(Codegen.getC "simengine/exec_cpu.c")]
	      | {target=Target.OPENMP, ...} => 
		[$(Codegen.getC "simengine/exec_cpu.c"),
		 $(Codegen.getC "simengine/exec_parallel_cpu.c")]
	      | {target=Target.CUDA, ...} =>
		[$(Codegen.getC "simengine/exec_kernel_gpu.cu"),
		 $(Codegen.getC "simengine/exec_parallel_gpu.cu")]

	val model_flows_c = model_flows forkedModelsWithSolvers
	val init_states_c = init_states 
				(List.filter (fn{iter_sym,...}=> case itersym2iter iter_sym of (_, DOF.IMMEDIATE) => false | _ => true) (#1 forkedModelsLessUpdate), 
				 sysprops)

	val exec_loop_c = $(Codegen.getC "simengine/exec_loop.c")

	(* write the code *)
	val filename = "./" ^ class_name ^ (case sysprops of
						{target=Target.CUDA, ...} => ".cu"
					      | _ => ".c")
	val _ = output_code(filename, (header_progs @
				       [precision_h] @
				       [memory_layout_h] @
				       [target_h] @
				       [simengine_api_h] @
				       simengine_interface_progs @

				       [defines_h] @
				       (case sysprops
					 of {target=Target.CUDA, ...} =>
					    [gpu_util_c]
					  | _ => []) @

				       (* Could be conditional on use of randoms *)
				       [random_c] @
				       [seint_h] @
				       [parallel_c] @
				       [output_buffer_h,output_buffer_c] @
				       [iodatastruct_code shardedModel] @
				       outputdatastruct_progs @
				       outputstatestruct_progs @
				       systemstate_progs @
				       state_init_prototypes @
				       fun_prototypes @
				       [solvers_h] @

				       (case sysprops
					 of {target=Target.CUDA, ...} =>
					    [solver_gpu_cu]
					  | _ => []) @

				       [solver_c] @
				       (*iteratordatastruct_progs @*)
				       solver_wrappers_c @
				       iterator_wrappers_c @
				       [inputs_c] @
				       [init_output_buffer_c] @
				       [simengine_api_c] @
				       logoutput_progs @
				       [log_outputs_c] @
				       exec_c @
				       [$("#define UNIFORM_RANDOM HOST_UNIFORM_RANDOM"),
					$("#define NORMAL_RANDOM HOST_NORMAL_RANDOM")] @
				       state_init_functions @
				       [$("#undef UNIFORM_RANDOM"),
					$("#undef NORMAL_RANDOM")] @
				       init_states_c @
				       [$("#define UNIFORM_RANDOM DEVICE_UNIFORM_RANDOM"),
					$("#define NORMAL_RANDOM DEVICE_NORMAL_RANDOM")] @
				       flow_progs @
				       [$("#undef UNIFORM_RANDOM"),
					$("#undef NORMAL_RANDOM")] @
				       model_flows_c @
				       init_solver_props_c @
				       [exec_loop_c]))
    in
	SUCCESS
    end
    handle e => DynException.checkpoint "CParallelWriter.buildC" e

end
