structure CParallelWriter =
struct

datatype status =
	 SUCCESS 
       | FAILURE of string

open Printer
exception InternalError

val i2s = Util.i2s
val r2s = Util.r2s
val e2s = ExpPrinter.exp2str

fun cstring str = "\"" ^ str ^ "\""
fun inc x = 1 + x

(* Indicates whether the class of a given instance satisfies a test.
 * Nb Presumes a CurrentModel context. *)
fun test_instance_class test instance =
    let val {classname, ...} = ExpProcess.deconstructInst instance
	val class = CurrentModel.classname2class classname
    in test class
    end

(* Indicates whether a given subsystem has states. *)
fun subsystem_has_states subsystem =
    let val {model, iter, top_class} = subsystem
	val (_, {classname, ...}, _) = model
    in CurrentModel.withModel model (fn _ =>
       let val class = CurrentModel.classname2class classname
       in has_states iter class
       end)
    end

(* Indicates whether a given class or any of its instances 
 * has states associated with a given iterator.
 * Nb Presumes a CurrentModel context. *)
and has_states iter class = 
    0 < ClassProcess.class2statesizebyiterator iter class orelse
    has_instance_states iter class

(* Indicates whether a given class contains any instances 
 * involving states associated with a given iterator.
 * Nb Presumes a CurrentModel context. *)
and has_instance_states iter class = 
    let val (iter_sym, _) = iter
	val instances = ClassProcess.class2instancesbyiterator iter_sym class
    in List.exists (instance_has_states (has_states iter)) instances
    end

(* Indicates whether an instance invocation involves states associated with a given iterator.
 * Nb Presumes a CurrentModel context. *)
and instance_has_states test instance =
    let val {classname, ...} = ExpProcess.deconstructInst instance
	val class = CurrentModel.classname2class classname
    in test class
    end

(* Indicates whether an output contains any term in its condition or contents
 * which satisfies a given predicate. *)
fun output_contains_term test output =
    let val {condition, contents, ...} = output
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
       List.exists (test_instance_class (reads_iterator iter)) (List.filter ExpProcess.isInstanceEq (! exps))
    end

and term_reads_iterator iter (Exp.SYMBOL (name, props)) =
    let val (iter_sym, _) = iter
    in case Property.getScope props
	of Property.READSTATE iter_sym' => iter_sym = iter_sym'
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
    let val {exps, ...} = class
    in List.exists (term_writes_iterator iter) (Util.flatmap ExpProcess.exp2termsymbols (! exps)) orelse
       List.exists (test_instance_class (writes_iterator iter)) (List.filter ExpProcess.isInstanceEq (! exps))
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
    let val {exps, outputs, ...} = class
    in List.exists (output_contains_term Term.isReadSystemState) (! outputs) orelse
       List.exists Term.isReadSystemState (Util.flatmap ExpProcess.exp2termsymbols (! exps)) orelse
       List.exists (test_instance_class reads_system) (List.filter ExpProcess.isInstanceEq (! exps))
    end




(* ====================  HEADER  ==================== *)

fun header (class_name, iterator_names, solvers, includes, defpairs) = 
    let val iters_enumerated = map (fn it => "ITERATOR_"^ it) iterator_names
        val solvers_enumerated = map (fn sol => String.map Char.toUpper sol) solvers
    in
	[$("// C Execution Engine for top-level model: " ^ class_name),
	 $("// " ^ Globals.copyright),
	 $("")] @
	(map (fn(inc)=> $("#include "^inc)) includes) @
	[$(""),
	 $("")] @
	(map (fn(name,value)=> $("#define " ^ name ^ " " ^ value)) defpairs)@
	[$(""),
	 $("typedef enum {"),
	 SUB(map (fn(sol) => $((String.map Char.toUpper sol) ^ ",")) solvers),
	 SUB[$("NUM_SOLVERS")],
	 $("} Solver;"),
	 $("const Solver SOLVERS[NUM_SOLVERS] = {" ^ (String.concatWith ", " solvers_enumerated) ^ "};"),
	 $(""),
	 $("typedef enum {"),
	 SUB(map (fn(iter) => $("ITERATOR_"^iter^",")) iterator_names),
	 SUB[$("NUM_ITERATORS")],
	 $("} Iterator;"),
	 $("const Iterator ITERATORS[NUM_ITERATORS] = {" ^ (String.concatWith ", " iters_enumerated) ^ "};"),
	 $("")]
    end

fun init_solver_props top_name forkedclasses =
    let
	val need_systemdata = List.exists subsystem_has_states forkedclasses

        fun free_props {top_class, iter=iterator, model} =
            let
                val (itersym, itertype) = iterator
                val itername = (Symbol.name itersym)
		val num_states = CurrentModel.withModel model (fn _ => ModelProcess.model2statesize model)
            in
	        if 0 < num_states then
                    $("memcpy(&system_states_ext[modelid].states_"^itername^", &system_states_int->states_"^itername^"[modelid], props[ITERATOR_"^itername^"].statesize*sizeof(CDATAFORMAT));")
 		else
                    $("")
            end

	fun init_props {top_class, iter=iterator, model} =
	    let
		fun progs () =
		    let
			val solverparams = (fn(_,itertype) => case itertype of
								  DOF.CONTINUOUS solver => (Solver.solver2params solver)
								| DOF.DISCRETE {sample_period} => [("timestep", Util.r2s sample_period)]
								| DOF.IMMEDIATE => []
								| _ => [("ERROR", "Bogus iterator")]) iterator
			val (itersym, itertype) = iterator
			val itername = (Symbol.name itersym)
			val solvername = String.map Char.toUpper 
						    ((fn(_,itertype) => case itertype of
									    DOF.CONTINUOUS solver => (Solver.solver2name solver)
									  | DOF.DISCRETE _ => "discrete"
									  | DOF.IMMEDIATE => "immediate"
									  | _ => "ERROR Bogus iterator") iterator)
			val num_states = ModelProcess.model2statesize model 
					 
			val requiresBandedMatrix = case itertype of
						       DOF.CONTINUOUS (Solver.LINEAR_BACKWARD_EULER _) => true
						     | _ => false
			val c = CurrentModel.classname2class top_class
			val matrix_exps = ClassProcess.symbol2exps c (Symbol.symbol "#M")
			val bandsize = case matrix_exps of
					   [exp] => 
					   if ExpProcess.isMatrixEq exp then
					       (fn(rows,cols)=>cols) (Container.matrix2size (Container.expmatrix2matrix (ExpProcess.rhs exp)))
					   else
					       0
					 | _ => 0
		    in
			(map (fn(prop,pval) => $("props[ITERATOR_"^itername^"]."^prop^" = "^pval^";")) solverparams) @
			[$("props[ITERATOR_"^itername^"].starttime = starttime;"),
			 $("props[ITERATOR_"^itername^"].stoptime = stoptime;"),
			 $("props[ITERATOR_"^itername^"].system_states = " ^ (if need_systemdata then "system_ptr" else "NULL") ^ ";"),
			 $("props[ITERATOR_"^itername^"].time = (CDATAFORMAT*)malloc(NUM_MODELS*sizeof(CDATAFORMAT));"),
			 $("props[ITERATOR_"^itername^"].next_time = (CDATAFORMAT*)malloc(NUM_MODELS*sizeof(CDATAFORMAT));"),
			 $("props[ITERATOR_"^itername^"].count = NULL; // Allocated by discrete solver only, must be NULL otherwise"),
			 $("// Initial values moved to model_states first time through the exec"),
			 $("props[ITERATOR_"^itername^"].model_states = " ^
			   (if 0 < num_states then
				"(CDATAFORMAT*)(&system_states_int->states_"^itername^");"
			    else
				"NULL;")),
			 $("props[ITERATOR_"^itername^"].next_states = " ^
			   (if 0 < num_states then
				"(CDATAFORMAT*)(&system_states_next->states_"^itername^");"
			    else
				"NULL;")),
			 $("props[ITERATOR_"^itername^"].inputs = inputs;"),
			 $("props[ITERATOR_"^itername^"].outputs = outputs;"),
			 $("props[ITERATOR_"^itername^"].solver = " ^ solvername ^ ";"),
			 $("props[ITERATOR_"^itername^"].iterator = ITERATOR_" ^ itername ^";")] @
			 (if requiresBandedMatrix then
			      [$("props[ITERATOR_"^itername^"].bandsize = " ^ (i2s bandsize) ^ ";")]
			  else
			      []) @
			 [$("props[ITERATOR_"^itername^"].inputsize = NUM_INPUTS;"),
			 $("props[ITERATOR_"^itername^"].statesize = " ^ (Util.i2s num_states) ^ ";"),
			 (*$("props[ITERATOR_"^itername^"].freeme = props[ITERATOR_"^itername^"].next_states;"),*)
			 $("props[ITERATOR_"^itername^"].outputsize = outputsize;"),
			 $("props[ITERATOR_"^itername^"].num_models = NUM_MODELS;"),
			 $("props[ITERATOR_"^itername^"].od = od;"),
			 $("props[ITERATOR_"^itername^"].ob_size = sizeof(output_buffer);"),
			 $("props[ITERATOR_"^itername^"].ob = ob;"),
			 $("props[ITERATOR_"^itername^"].running = (int*)malloc(NUM_MODELS*sizeof(int));"),
			 $("")] @
			(if 0 < num_states then
			     (case itertype of
				  DOF.CONTINUOUS _ =>
				  $("system_ptr->"^itername^" = props[ITERATOR_"^itername^"].time;")
				| DOF.DISCRETE _ =>
				  $("system_ptr->"^itername^" = props[ITERATOR_"^itername^"].count;")
				| _ =>
				  $("#error BOGUS ITERATOR NOT FILTERED")) ::
			     [$("system_ptr->states_"^itername^" = system_states_int->states_"^itername^";"),
			      (if (ModelProcess.hasPostProcessIterator itersym) then
				   $("system_ptr->states_pp_"^itername^" = system_states_int->states_pp_"^itername^";")
			       else
                                   $("")),
                              $("#if !defined TARGET_GPU"),
                              $("// Translate structure arrangement from external to internal formatting"),
                              $("for(modelid=0;modelid<props->num_models;modelid++){"),
                              SUB[$("memcpy(&system_states_int->states_"^itername^"[modelid], &system_states_ext[modelid].states_"^itername^", props[ITERATOR_"^itername^"].statesize*sizeof(CDATAFORMAT));")],
                              $("}"),
                              $("#endif")]
			 else nil)
		    end
		    handle e => DynException.checkpoint "CParallelWriter.init_solver_props.init_props.progs" e
	    in
		CurrentModel.withModel model progs
	    end
	    handle e => DynException.checkpoint "CParallelWriter.init_solver_props.init_props" e

	    
	fun gpu_init_props {top_class, iter=iterator, model} =
	    let fun progs () = 
		    let val (itersym, itertype) = iterator
			val itername = (Symbol.name itersym)

			val iterator_value_ptr = 
			    case itertype of
				DOF.CONTINUOUS _ =>
				$("tmp_system->"^itername^" = tmp_props[ITERATOR_"^itername^"].time;")
			      | DOF.DISCRETE _ =>
				$("tmp_system->"^itername^" = tmp_props[ITERATOR_"^itername^"].count;")
			      | _ =>
				$("#error BOGUS ITERATOR NOT FILTERED")

			val iterator_states_ptr =
			    $("tmp_system->states_"^itername^" = (statedata_"^(Symbol.name top_class)^" * )(tmp_props[ITERATOR_"^itername^"].model_states);")

			val iterator_pp_states_ptr =
			    if ModelProcess.hasPostProcessIterator itersym then
				[$("#error FIXME"),
				 $("system_ptr->states_pp_"^itername^" = &(system_states[modelid].states_pp_"^itername^");")]
			    else nil
			    
		    in
			iterator_value_ptr ::
			iterator_states_ptr ::
			iterator_pp_states_ptr
		    end


		val num_states = CurrentModel.withModel model (fn _ => ModelProcess.model2statesize model)
	    in
		if 0 < num_states then
		    CurrentModel.withModel model progs
		else nil
	    end
    in
	[
	 $("#if defined TARGET_GPU"),
	 $("void gpu_init_system_states_pointers (solver_props *tmp_props, top_systemstatedata *tmp_system) {"),
	 SUB(Util.flatmap gpu_init_props forkedclasses),
	 $("}"),
	 $("#endif"),
	 $(""),
	 $("solver_props *init_solver_props(CDATAFORMAT starttime, CDATAFORMAT stoptime, CDATAFORMAT *inputs, CDATAFORMAT *model_states, simengine_output *outputs){"),
	 SUB((if need_systemdata then
		  [$("systemstatedata_"^(Symbol.name top_name)^" *system_ptr = (systemstatedata_"^(Symbol.name top_name)^" *)malloc(sizeof(systemstatedata_"^(Symbol.name top_name)^" ));"),
		   $("systemstatedata_external *system_states_ext = (systemstatedata_external*)model_states;"),
                   $("#if defined TARGET_GPU"),
                   $("systemstatedata_external *system_states_int = (systemstatedata_external*)model_states;"),
                   $("systemstatedata_external *system_states_next = (systemstatedata_external*)malloc(sizeof(systemstatedata_external));"),
		   $("#else"),
                   $("systemstatedata_internal *system_states_int = (systemstatedata_internal*)malloc(sizeof(systemstatedata_internal));"),
                   $("systemstatedata_internal *system_states_next = (systemstatedata_internal*)malloc(sizeof(systemstatedata_internal));"),
                   $("#endif")]
	      else nil) @
	     [$("solver_props *props = (solver_props * )malloc(NUM_ITERATORS*sizeof(solver_props));"),
	      $("output_buffer *ob = (output_buffer*)malloc(sizeof(output_buffer));"),
	      $("#if NUM_OUTPUTS > 0"),
	      $("output_data *od = (output_data*)malloc(NUM_MODELS*sizeof(output_data));"),
	      $("unsigned int outputsize = sizeof(output_data)/sizeof(CDATAFORMAT);"),
	      $("#else"),
	      $("void *od = NULL;"),
	      $("unsigned int outputsize = 0;"),
	      $("#endif"),
	      $("unsigned int i, modelid;")] @
	     (Util.flatmap init_props forkedclasses) @
	     [$(""),
	      $("// Initialize all time vectors"),
	      $("assert(NUM_ITERATORS);"),
	      $("for(i=0;i<NUM_ITERATORS;i++){"),
	      SUB[$("for(modelid=0;modelid<NUM_MODELS;modelid++){"),
		  SUB[$("Iterator iter = ITERATORS[i];"),
		      $("props[iter].time[modelid] = starttime;"),
		      $("props[iter].next_time[modelid] = starttime;")],
		  $("}")],
	      $("}"),
	      $("memcpy(system_states_next, system_states_int, NUM_MODELS * sizeof(systemstatedata_external));"),
	      $("return props;")]),
	 $("}"),
	 $(""),
	 $("void free_solver_props(solver_props* props, CDATAFORMAT* model_states){"),
	 SUB[$("unsigned int modelid;"),
             $("unsigned int i;"),
             $("assert(props);"),
             $(""),
             $("#if !defined TARGET_GPU"),
             $("systemstatedata_external *system_states_ext = (systemstatedata_external*)model_states;"),
             $("systemstatedata_internal *system_states_int;"),
	     $("for(i=0;i<NUM_ITERATORS;i++){"),
             SUB[$("if(props[i].statesize > 0){"),
                 SUB[$("system_states_int = (systemstatedata_internal*)props[i].model_states;"),
                     $("break;")],
                 $("}")],
             $("}"),
             $(""),
             $("// Translate structure arrangement from internal back to external formatting"),
             $("for(modelid=0;modelid<props->num_models;modelid++){"),
             SUB(map free_props forkedclasses),
             $("}"),
             $("free(system_states_int);"),
             $("#endif"),
             $(""),
	     $("for(i=0;i<NUM_ITERATORS;i++){"),
	     SUB[$("Iterator iter = ITERATORS[i];"),
		 $("if (props[iter].time) free(props[iter].time);"),
		 $("if (props[iter].next_time) free(props[iter].next_time);"),
		 (*$("if (props[iter].freeme) free(props[iter].freeme);"),*)
		 $("if (props[iter].running) free(props[iter].running);")],
	     $("}"),
	     $("if (props[0].ob) free(props[0].ob);"),
	     $("if (props[0].od) free(props[0].od);"),
	     $("free(props);")],
	 $("}"),
	 $("")]
    end
    handle e => DynException.checkpoint "CParallelWriter.init_solver_props" e

fun simengine_interface (*(class_name, class, solver_names, iterator_names)*)(origModel as (classes, inst, props)) forkedModels =
    let
	val top_class = CurrentModel.withModel origModel (fn()=>CurrentModel.classname2class (#classname inst))
	val iterator_names = map (Symbol.name o #1 o #iter) forkedModels
	val solver_names = List.mapPartial (fn{iter,...}=>case iter of
							      (_,DOF.CONTINUOUS s) => SOME (Solver.solver2name s)
							    | (_,DOF.DISCRETE s) => SOME "discrete"
							    | _ => NONE) forkedModels
	val class_name = Symbol.name (#classname inst)

	fun init_condition2pair iter_sym basestr exp =
	    let val term = ExpProcess.exp2term (ExpProcess.lhs exp)
		val rhs = ExpProcess.rhs exp
	    in if Term.isInitialValue term iter_sym then
		   SOME ((if "" = basestr then "" else basestr ^ ".") ^ (Term.sym2name term), CWriterUtil.exp2c_str rhs)
	       else NONE
	    end

	fun findStatesInitValues iter_sym basestr (class:DOF.class) = 
	    let
		val classname = ClassProcess.class2orig_name class
		val exps = #exps class
		(*val state_eqs_symbols = map ExpProcess.lhs (List.filter ExpProcess.isStateEq (!exps))*)
		val init_conditions = List.filter ExpProcess.isInitialConditionEq (!exps)
		fun exp2name exp = 
		    Term.sym2curname (ExpProcess.exp2term exp)
		    handle e => DynException.checkpoint ("CParallelWriter.simengine_interface.findStatesInitValues.exp2name ["^(e2s exp)^"]") e
				      
		val instances = List.filter ExpProcess.isInstanceEq (!exps)
		val class_inst_pairs = ClassProcess.class2instnames class
	    in
		(List.mapPartial (init_condition2pair iter_sym basestr) init_conditions)
		@ (StdFun.flatmap (findInstanceStatesInitValues iter_sym) class_inst_pairs)
	    end
	    handle e => DynException.checkpoint "CParallelWriter.simengine_interface.findStatesInitValues" e

	and findInstanceStatesInitValues iter_sym (classname, instname) =
	    findStatesInitValues iter_sym (Symbol.name instname) (CurrentModel.classname2class classname)


	fun default2c_str (SOME v) = CWriterUtil.exp2c_str v
	  | default2c_str NONE = 
	    DynException.stdException("Unexpected non-default value for input", "CParallelWriter.simEngineInterface", Logger.INTERNAL)
	    
	val (state_names, state_defaults) = 
	    ListPair.unzip 
		(Util.flatmap 
		     (fn{top_class,iter,model}=>
			CurrentModel.withModel 
			    model 
			    (fn()=>
			       let
				   val (iter_sym,_) = iter
				   val class = CurrentModel.classname2class top_class
			       in
				   findStatesInitValues iter_sym "" class
			       end))
		     forkedModels)
	val (input_names, input_defaults) = ListPair.unzip (map (fn{name,default}=>(name,default)) (!(#inputs top_class)))

	fun wrap (f, m) x = CurrentModel.withModel m (fn _ => f x)

	fun name_subsystem_outputs {top_class, model, ...} =
	    CurrentModel.withModel model (fn _ =>
            let val class = CurrentModel.classname2class top_class
		val {outputs, ...} = class
	    in map (Term.sym2name o #name) (! outputs)
	    end)

	val output_names = 
	    Util.uniquify (Util.flatmap name_subsystem_outputs forkedModels)

	fun output_num_quantities (model, output) =
	    CurrentModel.withModel model (fn _ =>
	    let val {name, contents, ...} = output
	    in case TermProcess.symbol2temporaliterator name
		of SOME (iter_sym, _) => inc (List.length contents)
		 | _ => List.length contents
	    end)

	(* Presumes a CurrentModel.withModel context. *)
	fun outputs_from_class (model, class) =
	    let val {outputs, ...} = class
	    in 
		map (fn (output as {name, ...}) => (model, output))
		    (! outputs)
	    end

	val outputs_from_top_classes =
	    Util.flatmap (fn {top_class, model, ...} => CurrentModel.withModel model (fn _ => (outputs_from_class (model, CurrentModel.classname2class top_class))))
			 forkedModels

	val outputs_from_top_classes =
	    Util.uniquify_by_fun (fn ((_,a:DOF.output),(_,b:DOF.output)) => Term.sym2curname (#name a) = Term.sym2curname (#name b)) outputs_from_top_classes


	val outputs_num_quantities = map output_num_quantities outputs_from_top_classes

	val default_inputs = map default2c_str input_defaults
    in
	[$("static const char *input_names[] = {" ^ (String.concatWith ", " (map (cstring o Term.sym2name) input_names)) ^ "};"),
	 $("static const char *state_names[] = {" ^ (String.concatWith ", " (map cstring state_names)) ^ "};"),
	 $("static const char *output_names[] = {" ^ (String.concatWith ", " (map cstring output_names)) ^ "};"),
	 $("static const char *iterator_names[] = {" ^ (String.concatWith ", " (map cstring iterator_names)) ^ "};"),
	 $("static const double default_inputs[] = {" ^ (String.concatWith ", " default_inputs) ^ "};"),
	 $("static const double default_states[] = {" ^ (String.concatWith ", " state_defaults) ^ "};"),
	 $("static const unsigned int output_num_quantities[] = {" ^ (String.concatWith ", " (map i2s outputs_num_quantities)) ^ "};"),
	 $("static const char model_name[] = \"" ^ class_name ^ "\";"),
	 $("static const char *solvers[] = {" ^ (String.concatWith ", " (map cstring solver_names)) ^ "};"),
	 $("#if defined TARGET_CPU"),  (* These #if statements should be converted to sml conditionals based on compiler options *)
	 $("static const char target[] = \"cpu\";"),
	 $("#elif defined TARGET_OPENMP"),
	 $("static const char target[] = \"openmp\";"),
	 $("#elif defined TARGET_GPU"),
	 $("static const char target[] = \"gpu\";"),
	 $("#endif"),
	 $(""),
	 (* This would be nice but fails in gcc
	 $("static const unsigned int NUM_INPUTS = "^(i2s (List.length input_names)) ^ ";"),
	 $("static const unsigned int NUM_STATES = "^(i2s (List.length state_names)) ^ ";"),
	 $("static const unsigned long long HASHCODE = 0x0000000000000000ULL;"),
	 $("static const unsigned int VERSION = 0;"),
         *)
	 $("#define NUM_INPUTS "^(i2s (List.length input_names))),
	 $("#define NUM_STATES "^(i2s (List.length state_names))),
	 $("#define HASHCODE 0x0000000000000000ULL"),
	 (* FIXME get rid of the preprocessor directives that depend on NUM_OUTPUTS being a macro. *)
	 $("#define NUM_OUTPUTS "^(i2s (List.length output_names))),
	 $("#define VERSION 0"),
	 $("")]
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
fun outputdatastruct_code class =
    [$(""),
     $("#if NUM_OUTPUTS > 0"),
     SUB[$("typedef struct {"),
	 SUB(map output2struct (CWriterUtil.class2uniqueoutputsymbols class)),
	 $("} output_data;")],
     $("#endif"),
     $("")]
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

fun update_wrapper subsystems =
    let val _ = ()
	fun call_update {top_class, iter, model} =
	    CurrentModel.withModel model (fn _ =>
	       let val (iter_name, iter_typ) = iter
		   val (base_iter_name, base_iter_typ) = 
		       case iter_typ 
			of DOF.UPDATE dep => CurrentModel.itersym2iter dep
			 | _ => 
			   DynException.stdException(("Unexpected iterator '"^(Symbol.name iter_name)^"'"), "CParallelWriter.update_wrapper", Logger.INTERNAL)

		   val class = CurrentModel.classname2class top_class
		   val basename = ClassProcess.class2basename class
		   val basename_iter = (Symbol.name basename) ^ "_" ^ (Symbol.name base_iter_name)
		   val (statereads, statewrites, systemstatereads) =
		       (if reads_iterator iter class then "(const statedata_" ^ basename_iter ^ " * )props->next_states, " else "",
			if writes_iterator iter class then "(statedata_" ^ basename_iter ^ " * )props->next_states, " else "",
			if reads_system class then "props->system_states, " else "")

	       in [$("case ITERATOR_" ^ (Symbol.name base_iter_name) ^ ":"),
		   case base_iter_typ
		    of DOF.CONTINUOUS _ =>
		       SUB [$("return flow_" ^ (Symbol.name top_class) ^ "(props->next_time[modelid], " ^
			      statereads ^ statewrites ^ systemstatereads ^
			      "props->inputs, (CDATAFORMAT * )props->od, 1, modelid);")]
		     | DOF.DISCRETE _ => 
		       SUB [$("return flow_" ^ (Symbol.name top_class) ^ "(1 + props->count[modelid], " ^
			      statereads ^ statewrites ^ systemstatereads ^
			      "props->inputs, (CDATAFORMAT * )props->od, 1, modelid);")]
		     | _ => $("#error BOGUS ITERATOR")]
	       end)

    in [$("__HOST__ __DEVICE__ int update(solver_props *props, unsigned int modelid) {"),
	SUB ($("switch (props->iterator) {") ::
	     List.concat (map call_update subsystems) @
	     [$("default: return 1;"),
	      $("}")]),
	$("}")]
    end

fun postprocess_wrapper subsystems =
    let val _ = ()
	fun call_update {top_class, iter, model} =
	    CurrentModel.withModel model (fn _ =>
	       let val (iter_name, iter_typ) = iter
		   val (base_iter_name, base_iter_typ) = 
		       case iter_typ 
			of DOF.POSTPROCESS dep => CurrentModel.itersym2iter dep
			 | _ => 
			   DynException.stdException(("Unexpected iterator '"^(Symbol.name iter_name)^"'"), "CParallelWriter.update_wrapper", Logger.INTERNAL)

		   val class = CurrentModel.classname2class top_class
		   val basename = ClassProcess.class2basename class
		   val basename_iter = (Symbol.name basename) ^ "_" ^ (Symbol.name base_iter_name)
		   val (statereads, statewrites, systemstatereads) =
		       (if reads_iterator iter class then "((systemstatedata_" ^ (Symbol.name basename) ^ " * )props->system_states)->states_" ^ (Symbol.name iter_name) ^ ", " else "",
			if writes_iterator iter class then "((systemstatedata_" ^ (Symbol.name basename) ^ " * )props->system_states)->states_" ^ (Symbol.name iter_name) ^ ", " else "",
			if reads_system class then "props->system_states, " else "")

	       in [$("case ITERATOR_" ^ (Symbol.name base_iter_name) ^ ":"),
		   case base_iter_typ
		    of DOF.CONTINUOUS _ =>
		       SUB [$("return flow_" ^ (Symbol.name top_class) ^ "(props->next_time[modelid], " ^
			      statereads ^ statewrites ^ systemstatereads ^
			      "props->inputs, (CDATAFORMAT * )props->od, 1, modelid);")]
		     | DOF.DISCRETE _ => 
		       SUB [$("return flow_" ^ (Symbol.name top_class) ^ "(1 + props->count[modelid], " ^
			      statereads ^ statewrites ^ systemstatereads ^
			      "props->inputs, (CDATAFORMAT * )props->od, 1, modelid);")]
		     | _ => $("#error BOGUS ITERATOR")]
	       end)

    in [$("__HOST__ __DEVICE__ int post_process(solver_props *props, unsigned int modelid) {"),
	SUB ($("switch (props->iterator) {") ::
	     List.concat (map call_update subsystems) @
	     [$("default: return 1;"),
	      $("}")]),
	$("}")]
    end


local
    fun state2member iterators (sym) =
	let
	    val size = (*Term.symbolSpatialSize (ExpProcess.exp2term sym)*)
		ExpProcess.exp2size iterators sym
	    val name = Symbol.name (Term.sym2curname (ExpProcess.exp2term sym))
	in
	    if size = 1 then ("CDATAFORMAT " ^ name ^ "[ARRAY_SIZE];")
	    else ("CDATAFORMAT " ^ name ^ "["^(i2s size)^" * ARRAY_SIZE];")
	end

    fun instanceNamed instname inst =
	ExpProcess.instOrigInstName inst = instname

    fun instance2member instances (classname, instname) =
	let			  
	    val index = 
		case List.find (instanceNamed instname) instances 
		 of SOME inst' => 
		    let val size = ExpProcess.instSpatialSize inst'
		    in if 1 = size then ";" else "["^(i2s size)^"];"
		    end
		  | NONE => ";"
	in
	    "statedata_" ^ (Symbol.name classname) ^ " " ^ (Symbol.name instname) ^ index
	end
in
fun outputstatestructbyclass_code (class : DOF.class as {exps, ...}) =
    let
	val classname = ClassProcess.class2classname class
	val class_iterators = #iterators class
	val init_eqs_symbols = map ExpProcess.lhs (List.filter ExpProcess.isInitialConditionEq (!exps))
	val instances = List.filter ExpProcess.isInstanceEq (!exps)
	(*val _ = Util.log ("in outputstatestructbyclass_code: calling class_inst_pairs for class " ^ (Symbol.name (#name class))^ ", number of instances = " ^ (i2s (List.length instances)))*)
	val class_inst_pairs = ClassProcess.class2instnames class
	(* val _ = Util.log ("Returning from class2instnames: all_classes={"^String.concatWith ", " (map (Symbol.name o #1) class_inst_pairs)^"}") *)

	val class_inst_pairs_non_empty = 
	    List.filter (ClassProcess.hasStates o CurrentModel.classname2class o #1) class_inst_pairs					 

    in
	if List.null class_inst_pairs_non_empty andalso List.null init_eqs_symbols andalso List.null instances then 
	    [$(""),
	     $("// Ignoring class '" ^ (Symbol.name (#name class)) ^ "'")]
	else
	    [$(""),
	     $("// Define state structures"),
	     $("typedef struct  {"),	 
	     SUB($("// states (count="^(i2s (List.length init_eqs_symbols))^")") ::
		 (map ($ o (state2member class_iterators)) init_eqs_symbols) @
		 ($("// instances (count=" ^ (i2s (List.length class_inst_pairs_non_empty)) ^")") ::
		  (map ($ o (instance2member instances)) class_inst_pairs_non_empty))),
	     $("} statedata_" ^ (Symbol.name classname) ^";")]
    end
    handle e => DynException.checkpoint "CParallelWriter.outputstatestructbyclass_code" e       
end

fun outputstatestruct_code (model:DOF.model as (classes,_,_)) =
    let
	fun progs () =
	    let val master_classes = List.filter (fn (c) => ClassProcess.isMaster c andalso (ClassProcess.hasStates c orelse ClassProcess.hasInstances c)) classes
	    in
		List.concat (map outputstatestructbyclass_code master_classes)
	    end
    in
	CurrentModel.withModel model progs
    end 
    handle e => DynException.checkpoint "CParallelWriter.outputstatestruct_code" e

fun outputsystemstatestruct_code forkedModels =
    let
	val master_classes = List.filter (fn (c) => ClassProcess.isMaster c) (CurrentModel.classes ())

	fun subsystem_classname_iterator_pair subsystem =
	    let val {model, iter, ...} = subsystem
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
	    List.mapPartial subsystem_classname_iterator_pair forkedModels

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
		 $("// System State Structure (internal ordering)"),
		 $("typedef struct {"),
		 SUB(map (fn(classname, iter_sym, _) => $("statedata_" ^ (Symbol.name classname) ^ " states_" ^ (Symbol.name iter_sym) ^ "[NUM_MODELS];")) class_names_iterators),
		 $("} systemstatedata_internal;"),
                 $("")]

	fun name_and_iterator class (iter as (iter_sym,_)) = 
	    (Symbol.symbol ((Symbol.name (ClassProcess.class2basename class))^"_"^(Symbol.name iter_sym)), iter)

	fun class_struct_data class =
	    let val iters = List.filter (fn (it) => has_states it class) (CurrentModel.iterators())
		val class_name_iterator_pairs = map (name_and_iterator class) iters
	    in 
		(ClassProcess.class2classname class, class_name_iterator_pairs) 
	    end
	    handle e => DynException.checkpoint "CParallelWriter.outputsystemstatestruct_code.class_struct_data" e

	fun class_struct_declaration (name, iter_pairs) =
	    [$("typedef struct {"),
	     SUB(map ($ o iter_pair_iter_member) iter_pairs),
	     SUB(map ($ o iter_pair_states_member) iter_pairs),
	     $("} systemstatedata_"^(Symbol.name name)^";"),$("")]
	and iter_pair_states_member (classname, iter as (iter_name,iter_typ)) =
	    case iter_typ of
		DOF.UPDATE _ => ""
	      | _ => "statedata_"^(Symbol.name classname)^" *states_"^(Symbol.name iter_name)^";"
	and iter_pair_iter_member (_, (iter_name,DOF.CONTINUOUS _)) =
	    "CDATAFORMAT *"^(Symbol.name iter_name)^";"
	  | iter_pair_iter_member (_, (iter_name,DOF.DISCRETE _)) = 
	    "unsigned int *"^(Symbol.name iter_name)^";"
	  | iter_pair_iter_member _ = 
	    (*"#error BOGUS ITERATOR NOT FILTERED"*) ""
	    
	val per_class_struct_data = 
	    List.filter
		(not o List.null o #2)
		(map class_struct_data master_classes)

	val (top_class_struct_data :: rest_classes_struct_data) = 
	    case per_class_struct_data
	     of nil => 
		DynException.stdException(("No classes to generate state structures."), "CParallelWriter.outputsystemstatestruct_code", Logger.INTERNAL)
	      | _ => per_class_struct_data




	val per_class_struct_prog = 
	    $("// Per-class system pointer structures") ::
	    Util.flatmap class_struct_declaration per_class_struct_data @
	    [$("typedef systemstatedata_"^(Symbol.name (#1 top_class_struct_data))^" top_systemstatedata;"),$("")]
    in
	top_sys_state_struct_prog @ 
	per_class_struct_prog
    end
    handle e => DynException.checkpoint "CParallelWriter.outputsystemstatestruct_code" e

fun class2flow_code (class, is_top_class, iter as (iter_sym, iter_type)) =
    let
	(* we need to add EP indices if this is the top class *)
	val _ = ClassProcess.addEPIndexToClass is_top_class class

	val orig_name = ClassProcess.class2basename class

	(* val has_states = case iter_type of  *)
	(* 		     DOF.UPDATE _ => true *)
	(* 		   | _ => ClassProcess.class2statesize class > 0 *)

	val eval_iterators = List.filter (fn(iter_sym, iter_type)=> case iter_type of
									DOF.UPDATE v => false
								      | DOF.POSTPROCESS v => false
								      | _ => true) (CurrentModel.iterators())

	val (readstates, writestates) = class2stateiterators class
	val iterators = CurrentModel.iterators()
	(*val iteratorprototypes = String.concat (map (fn(sym,_)=> "CDATAFORMAT " ^ (Symbol.name sym) ^ ", ") eval_iterators)*)
	(* val iteratorprototypes = "iteratordata *iter, " *)
	(* every iterator except the update iterator uses an iter_name *)
	val iter_name = Symbol.name (case iter_type of
					 DOF.UPDATE v => v
				       | _ => iter_sym)
	val iter_name' = Symbol.name (case iter_type of
					 DOF.UPDATE v => v
				       | DOF.POSTPROCESS v => v
				       | _ => iter_sym)
			
	val (statereadprototype,
	     statewriteprototype,
	     systemstatereadprototype) =
	    (if reads_iterator iter class then
		 "const statedata_" ^ (Symbol.name orig_name) ^ "_" ^ iter_name ^ " *rd_" ^ iter_name ^ ", "
	     else "",
	     if writes_iterator iter class then
		 "statedata_" ^ (Symbol.name orig_name) ^ "_" ^ iter_name ^ " *wr_" ^ iter_name ^ ", "
	     else "",
	     if reads_system class then
		 "const systemstatedata_"^(Symbol.name orig_name)^" *sys_rd, "
	     else "")


	val useMatrixForm = ModelProcess.requiresMatrixSolution (iter_sym, iter_type)

	val header_progs = 
	    if useMatrixForm then
		[$("__HOST__ __DEVICE__ int flow_" ^ (Symbol.name (#name class)) ^ 
		   "(CDATAFORMAT "^iter_name'^", " ^ statereadprototype ^ "CDATAFORMAT *INTERNAL_M, CDATAFORMAT *INTERNAL_b, " ^ systemstatereadprototype ^
		   " CDATAFORMAT *inputs, CDATAFORMAT *outputs, const unsigned int first_iteration, const unsigned int modelid) {")]
	    else
		[$("__HOST__ __DEVICE__ int flow_" ^ (Symbol.name (#name class)) ^ 
		   "(CDATAFORMAT "^iter_name'^", " ^ statereadprototype ^ statewriteprototype ^ systemstatereadprototype ^
		   " CDATAFORMAT *inputs, CDATAFORMAT *outputs, const unsigned int first_iteration, const unsigned int modelid) {")]
		

	val read_memory_progs = []

	val read_states_progs = []

	(* filter out all the unneeded expressions *)
	val (initvalue_exps, rest_exps) = List.partition ExpProcess.isInitialConditionEq (!(#exps class))
	val (valid_exps, rest_exps) = List.partition (fn(exp)=> ExpProcess.isIntermediateEq exp orelse
							        ExpProcess.isInstanceEq exp orelse
							        ExpProcess.isStateEq exp) rest_exps
	val _ = if (List.length rest_exps > 0) then
		    (Logger.log_error($("Invalid expressions reached in code writer while writing class " ^ (Symbol.name (ClassProcess.class2orig_name class))));
		     app (fn(exp)=> Util.log ("  Offending expression: " ^ (e2s exp))) rest_exps;
		     DynException.setErrored())
		else
		    ()

	val input_automatic_var =
	    if is_top_class then
		fn ({name,default},i) => 
		   $("CDATAFORMAT " ^ (CWriterUtil.exp2c_str (Exp.TERM name)) ^ " = inputs[TARGET_IDX(NUM_INPUTS, NUM_MODELS, " ^ (i2s i) ^ ", modelid)];")
	    else
		fn ({name,default},i) => 
		   $("CDATAFORMAT " ^ (CWriterUtil.exp2c_str (Exp.TERM name)) ^ " = inputs[" ^ (i2s i) ^ "];")

	val eqn_symbolset = 
	    SymbolSet.flatmap ExpProcess.exp2symbolset valid_exps

	(* map only inputs that are actually used within the flow equations *)
	(*
	fun input_appears_in_eqns ({name=Exp.SYMBOL (insym,_),...},_) =
	    SymbolSet.exists (fn (sym) => sym = insym) eqn_symbolset
	  (* TODO what happens for non-symbol inputs? *)
	  | input_appears_in_eqns _ = true
		   
	 *)
	val inputs = (* List.filter input_appears_in_eqns *) (Util.addCount (!(#inputs class)))

	val read_inputs_progs =
	    [$(""),
	     $("// mapping inputs to variables")] @ 
	    (map input_automatic_var inputs)


	local
	    fun exp2prog exp =
		if (ExpProcess.isIntermediateEq exp) then
		    intermediateeq2prog exp
		else if (ExpProcess.isFirstOrderDifferentialEq exp) then
		    firstorderdiffeq2prog exp
		else if (ExpProcess.isDifferenceEq exp) then
		    differenceeq2prog exp
		else if (ExpProcess.isUpdateEq exp) then
		    differenceeq2prog exp
		else if (ExpProcess.isPPEq exp) then
		    differenceeq2prog exp
		else if (ExpProcess.isInstanceEq exp) then
		    instanceeq2prog exp
		else
		    DynException.stdException(("Unexpected expression '"^(e2s exp)^"'"), "CParallelWriter.class2flow_code.equ_progs", Logger.INTERNAL)

	    and intermediateeq2prog exp =
		((if ExpProcess.isMatrixEq exp then
		      let
			  val (lhs, rhs) = (ExpProcess.lhs exp, ExpProcess.rhs exp)
			  val (rows, cols) = (Container.matrix2size o Container.expmatrix2matrix) rhs
			  val var = CWriterUtil.exp2c_str lhs
			  fun createIdx (i,j) = "MATIDX("^(i2s rows)^","^(i2s cols)^","^(i2s i)^","^(i2s j)^", NUM_MODELS, modelid)"
			  fun createEntry (exp, i, j) = [$("// " ^ (e2s exp)),
							 $(var ^ "[" ^ (createIdx (i,j)) ^ "]" ^ " = " ^ (CWriterUtil.exp2c_str exp) ^ ";")]
		      in
			  List.concat (Container.matrixmap createEntry (Container.expmatrix2matrix rhs))
		      end
		  else if ExpProcess.isArrayEq exp then
		      let
			  val (lhs, rhs) = (ExpProcess.lhs exp, ExpProcess.rhs exp)
			  val size = (Container.array2size o Container.exparray2array) rhs
			  val var = CWriterUtil.exp2c_str lhs				  
			  fun createIdx i = "VECIDX("^(i2s size)^","^(i2s i)^", NUM_MODELS, modelid)"
			  fun createEntry (exp, i) = [$("//" ^ (e2s exp)),
						      $(var ^ "["^(createIdx i)^"]" ^ " = " ^ (CWriterUtil.exp2c_str exp) ^ ";")]
		      in
			  List.concat (map createEntry (StdFun.addCount (Container.array2list (Container.exparray2array rhs))))
		      end
		  else
 		      [$("// " ^ (e2s exp)),
		       $("CDATAFORMAT " ^ (CWriterUtil.exp2c_str exp) ^ ";")])
		 handle e => DynException.checkpoint "CParallelWriter.class2flow_code.intermediateeq2prog" e)
		

	    and firstorderdiffeq2prog exp =
 		[$((CWriterUtil.exp2c_str exp) ^ ";")]
	    and differenceeq2prog exp =
 		[$((CWriterUtil.exp2c_str exp) ^ ";")]
	    and instanceeq2prog exp =
		let
		    val {classname, instname, props, inpargs, outargs} = ExpProcess.deconstructInst exp
		    val orig_instname = case InstProps.getRealInstName props of
					    SOME v => v
					  | NONE => instname

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
			systemdata^"."^"states_"^(Symbol.name iter_name)^" = &sys_rd->states_"^(Symbol.name iter_name)^"->"^(Symbol.name orig_instname)^";"

		    val iters = List.filter (fn (it) => (not (ModelProcess.isImmediateIterator it)) andalso (ClassProcess.requiresIterator it instclass)) (ModelProcess.returnIndependentIterators ())
		    val state_iters = List.filter (fn it => ClassProcess.requiresIterator it instclass) (ModelProcess.returnStatefulIterators ())

		    val sysstates_init = [$("systemstatedata_"^(Symbol.name (ClassProcess.class2basename instclass))^" "^systemdata^";"),
					  SUB(map ($ o systemstatedata_iterator) iters),
					  SUB(map ($ o systemstatedata_states) state_iters)]

		    val calling_name = "flow_" ^ (Symbol.name classname)

		    val inpvar = if List.null inpargs then "NULL" else Unique.unique "inputdata"
		    val outvar = Unique.unique "outputdata"
				 

		    val inps = 
			if List.null inpargs then []
			else [$("CDATAFORMAT " ^ inpvar ^ "[" ^ (i2s (List.length inpargs)) ^ "];")]

		    val inps_init = map ( fn(inparg, idx) => $(inpvar ^ "[" ^ (i2s idx) ^ "] = " ^ CWriterUtil.exp2c_str inparg ^ ";")) (Util.addCount inpargs)
		    val outs_decl = "CDATAFORMAT " ^ outvar ^ "["^(i2s (List.length outargs))^"];"

		    fun declare_output ((sym,_),_) = "CDATAFORMAT "^(Symbol.name sym)^";"

		    fun assign_output ((sym, {name, contents, condition}), idx) =
			(Symbol.name sym) ^ " = " ^ outvar ^ "[" ^ (i2s idx) ^ "];" ^
			" // Mapped to "^ (Symbol.name classname) ^ ": " ^ (e2s (List.hd (contents)))

		    (* removing below line since the output args could contain don't cares *)
		    (*val output_symbol_pairs = 
			Util.addCount (ListPair.zip (map Term.sym2curname outargs, !(#outputs instclass)))*)
		    val output_term_pairs =
			Util.addCount (ListPair.zip (outargs, !(#outputs instclass)))
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
			  $("// " ^ (CWriterUtil.exp2c_str exp))] @ 
			 inps @
			 inps_init @ 
			 (if reads_system instclass then
			      sysstates_init 
			  else [] ) @
			 [$(outs_decl),
			  $(calling_name ^ "("^iter_name'^", "^
			    statereads ^ statewrites ^ systemstatereads ^ 
			    inpvar^", "^outvar^", first_iteration, modelid);")
			 ] @
			 map ($ o assign_output) output_symbol_pairs),
		     $("}"),$("")]
		end
		handle e => DynException.checkpoint "CParallelWriter.class2flow_code.instanceeq2prog" e



	in
	val equ_progs = 
	    [$(""),
	     $("// writing all intermediate, instance, and differential equation expressions")] @
	    (Util.flatmap exp2prog valid_exps)
	end
	    
	val state_progs = []



	val output_progs = 
	    if is_top_class then
		let fun cmp (a, b) = Term.sym2curname a = Term.sym2curname b
		    val outputs_symbols = Util.uniquify_by_fun cmp (ClassProcess.outputsSymbols class)
		in
		    [$(""),
		     $("// writing output variables"),
                     $("#if NUM_OUTPUTS > 0"),
		     $("if (first_iteration) {"),
		     SUB($("output_data *od = (output_data*)outputs;") ::
			 (map (fn(t)=> $("od[modelid]." ^ (Symbol.name (Term.sym2curname t)) ^ " = " ^ (CWriterUtil.exp2c_str (Exp.TERM t)) ^ ";"))
			      outputs_symbols)),
		     $("}"),
                     $("#endif")]
		end
	    else
		[$(""),
		 $("// writing output data "),
		 SUB(map 
			 (fn({name,contents,condition},i)=> 
			    let
				val _ = if length contents = 1 then
					    ()
					else
					    DynException.stdException (("Output '"^(e2s (Exp.TERM name))^"' in class '"^(Symbol.name (#name class))^"' can not be a grouping of {"^(String.concatWith ", " (map e2s contents))^"} when used as a submodel"), "CParallelWriter.class2flow_code", Logger.INTERNAL)
					    
				val valid_condition = case condition 
						       of (Exp.TERM (Exp.BOOL v)) => v
							| _ => false
				val _ = if valid_condition then
					    ()
					else
					    DynException.stdException (("Output '"^(e2s (Exp.TERM name))^"' in class '"^(Symbol.name (#name class))^"' can not have a condition '"^(e2s condition)^"' when used as a submodel"), "CParallelWriter.class2flow_code", Logger.INTERNAL)
					    
			    in
				case contents of
				    [content] =>
				    $("outputs["^(i2s i)^"] = " ^ (CWriterUtil.exp2c_str (content)) ^ ";")
				  | _ => 
				    DynException.stdException (("Output '"^(e2s (Exp.TERM name))^"' in class '"^(Symbol.name (#name class))^"' can not be a grouping of {"^(String.concatWith ", " (map e2s contents))^"} when used as a submodel"), 
							       "CParallelWriter.class2flow_code", 
							       Logger.INTERNAL)
			    end) (Util.addCount (!(#outputs class))))]

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
	 $("")]
    end
    handle e => DynException.checkpoint "CParallelWriter.class2flow_code" e

fun flow_code {model as (classes,_,_), iter as (iter_sym, iter_type), top_class} : text list * text list = 
    let
	val iter_name = Symbol.name (case iter_type of
					 DOF.UPDATE v => v
				       | DOF.POSTPROCESS v => v
				       | _ => iter_sym)

	val eval_iterators = ModelProcess.returnDependentIterators ()

	fun class_flow_prototype class = 
	    let
		val orig_name = ClassProcess.class2basename class
	    (* val class_has_states = case iter_type of *)
	    (* 			   DOF.UPDATE _ => true *)
	    (* 			 | _ => ClassProcess.class2statesize class > 0 *)
	    in
		if ClassProcess.isInline class then
		    $("CDATAFORMAT "^(Symbol.name (#name class))^"("^
		      (String.concatWith 
			   ", " 
			   (map 
				(fn{name,...}=> "CDATAFORMAT " ^ (CWriterUtil.exp2c_str (Exp.TERM name))) 
				(!(#inputs class))))^");")
		else
		    let
			(* every iterator except the update iterator uses an iter_name *)
			val iter_name = Symbol.name (case iter_type of
							 DOF.UPDATE v => v
						       | _ => iter_sym)
			val iter_name' = Symbol.name (case iter_type of
							  DOF.UPDATE v => v
							| DOF.POSTPROCESS v => v
							| _ => iter_sym)
			val (statereadprototype,
			     statewriteprototype,
			     systemstatereadprototype) =
			    (if reads_iterator iter class then
				 "const statedata_" ^ (Symbol.name orig_name) ^ "_" ^ iter_name ^ " *rd_" ^ iter_name ^ ", "
			     else "",
			     if writes_iterator iter class then
				 "statedata_" ^ (Symbol.name orig_name) ^ "_" ^ iter_name ^ " *wr_" ^ iter_name ^ ", "
			     else "",
			     if reads_system class then
				 "const systemstatedata_"^(Symbol.name orig_name)^" *sys_rd, "
			     else "")

			val useMatrixForm = ModelProcess.requiresMatrixSolution (iter_sym, iter_type)
		    in
			if useMatrixForm then
			    $("__HOST__ __DEVICE__ int flow_" ^ (Symbol.name (#name class)) ^ 
			      "(CDATAFORMAT "^iter_name'^", " ^ statereadprototype ^ "CDATAFORMAT *INTERNAL_M, CDATAFORMAT *INTERNAL_b, " ^ systemstatereadprototype ^
			      " CDATAFORMAT *inputs, CDATAFORMAT *outputs, const unsigned int first_iteration, const unsigned int modelid);")
			else
			    $("__HOST__ __DEVICE__ int flow_" ^ (Symbol.name (#name class)) ^ 
			      "(CDATAFORMAT "^iter_name'^", " ^ statereadprototype ^ statewriteprototype ^ systemstatereadprototype ^
			      " CDATAFORMAT *inputs, CDATAFORMAT *outputs, const unsigned int first_iteration, const unsigned int modelid);")
		    end
	    end


    (*val (fun_prototypes, fun_wrappers) = ListPair.unzip (map (fn(iter)=>flow_wrapper (topclass, iter)) eval_iterators)*)
    (*val (fun_prototype, fun_wrapper) = flow_wrapper (topclass, iter)*)

    in
	CurrentModel.withModel model 
			       (fn () =>
				   let
				       val topclass = CurrentModel.classname2class top_class

    				       val fundecl_progs = map class_flow_prototype classes
							   
				       val flow_progs = List.concat (map (fn(c)=>
									    if ClassProcess.isInline c then
										(Logger.log_error ($("Functional classes like '"^(Symbol.name (#name c))^"' are not supported"));
										 DynException.setErrored();
										 [])
									    else
										class2flow_code (c,#name c = #name topclass, iter)) classes)
				   in
				       ([$("// Functions prototypes for flow code")] @ fundecl_progs, flow_progs)
				   end
				   handle e => DynException.checkpoint "CParallelWriter.flow_code.anonymous_fun" e)
    end
    handle e => DynException.checkpoint "CParallelWriter.flow_code" e


(* TODO remove the iterval parameter from IMMEDIATE flows. *)
fun model_flows forkedModels = 
    let
	fun subsystem_flow_call subsystem =
	    let val {top_class, model, iter} = subsystem
		val (iter_sym, _) = iter
		val requiresMatrix = ModelProcess.requiresMatrixSolution iter
	    in CurrentModel.withModel model (fn _ =>
	       let val class = CurrentModel.classname2class top_class
		   val basename = ClassProcess.class2basename class
		   val (statereads, statewrites, systemstatereads) =
		       (if reads_iterator iter class then "(const statedata_" ^ (Symbol.name top_class) ^ "* )y, " else "",
			if writes_iterator iter class then 
			    "(statedata_" ^ (Symbol.name top_class) ^ "* )dydt, " 
			else if requiresMatrix then
			    "(CDATAFORMAT* ) props->mem, dydt, "
			else
			    "",
			if reads_system class then "(const systemstatedata_" ^ (Symbol.name basename) ^ " *)props->system_states, " else "")
	       in
		SUB[$("case ITERATOR_" ^ (Symbol.name iter_sym) ^ ":"),
		    $("return flow_" ^ (Symbol.name top_class) ^ 
		      "(iterval, " ^ statereads ^ statewrites ^ systemstatereads ^ "props->inputs, (CDATAFORMAT *)props->od, first_iteration, modelid);")]
	       end)
	    end

    in
	[$"",
	 $("__HOST__ __DEVICE__ int model_flows(CDATAFORMAT iterval, const CDATAFORMAT *y, CDATAFORMAT *dydt, solver_props *props, const unsigned int first_iteration, const unsigned int modelid){"),
	 SUB($("switch(props->iterator){") ::
	     (map subsystem_flow_call forkedModels) @
	     [$("default: return 1;"),
	      $("}")]
	    ),
	 $("}"),
	 $("")]
    end
    handle e => DynException.checkpoint "CParallelWriter.model_flows" e


fun output_code (name, location, block) =
    let
      val filename = location ^ "/" ^ name ^ "_parallel.c"
      val _ = Logger.log_notice ($("Generating C source file '"^ filename ^"'"))
      val file = TextIO.openOut (filename)
    in
      Printer.printtexts (file, block, 0)
      before TextIO.closeOut file
    end

fun logoutput_code class forkedModels =
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
	      | DOF.POSTPROCESS v => List.exists (fn(iter_sym',iter_type')=> v = iter_sym' andalso (case iter_type' of DOF.CONTINUOUS _ => true | _ => false)) iterators
	      | DOF.IMMEDIATE => false
	fun isDiscreteIterator (iter_sym) = 
	    case iter_sym2type iter_sym of
		DOF.CONTINUOUS _ => false
	      | DOF.DISCRETE _ => true
	      | DOF.UPDATE v => List.exists (fn(iter_sym',iter_type')=> v = iter_sym' andalso (case iter_type' of DOF.DISCRETE _ => true | _ => false)) iterators
	      | DOF.POSTPROCESS v => List.exists (fn(iter_sym',iter_type')=> v = iter_sym' andalso (case iter_type' of DOF.DISCRETE _ => true | _ => false)) iterators
	      | DOF.IMMEDIATE => false

	val orig_name = Symbol.name (ClassProcess.class2basename class)
	val dependent_symbols = CWriterUtil.class2uniqueoutputsymbols class
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
	    let val {name, contents, condition} = output
		val num_quantities = 
		    case TermProcess.symbol2temporaliterator name
		     of SOME (iter_sym, _) => inc (List.length contents)
		      | _ => List.length contents
		val cond = 
		    (case ExpProcess.exp2temporaliterator (Exp.TERM name) 
		      of SOME (iter_sym, _) => "(props->iterator == ITERATOR_" ^ (Symbol.name iter_sym) ^ ")"
		       | _ => "1") ^ " && (" ^
		    (CWriterUtil.exp2c_str (ExpProcess.assignToOutputBuffer condition)) ^ ")"

	    in
		[$("{ // Generating output for symbol " ^ (e2s (Exp.TERM name))),
		 SUB[$("int cond = " ^ cond ^ ";"),
		     $("if (cond) {"),
		     SUB([$("output_buffer_data *buf = (output_buffer_data *)ob->ptr[modelid];"),
			  $("buf->outputid = " ^ (i2s index) ^ ";"),
			  $("buf->num_quantities = " ^ (i2s num_quantities) ^ ";"),
			  $("")] @
			 (case (ExpProcess.exp2temporaliterator (Exp.TERM name)) of
			      SOME (iter_sym, _) => 
			      (case CurrentModel.itersym2iter iter_sym of
				   (_, DOF.CONTINUOUS _) =>
				   [$("buf->quantities[0] = props->time[modelid];")]
				 | (_, DOF.DISCRETE _) =>
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
			  $("ob->full[modelid] |= (MAX_OUTPUT_SIZE >= ((unsigned char * )(ob->end[modelid]) - (unsigned char * )(ob->ptr[modelid])));")]),
		     $("}")],
		 $("}")]
	    end

	(* Presumes a CurrentModel.withModel context. *)
	fun outputs_from_class (model, class) =
	    let val {outputs, ...} = class
	    in 
		map (fn (output as {name, ...}) => (model, output))
		    (! outputs)
	    end

	val outputs_from_top_classes =
	    Util.flatmap (fn {top_class, model, ...} => CurrentModel.withModel model (fn _ => (outputs_from_class (model, CurrentModel.classname2class top_class))))
			 forkedModels

	val outputs_from_top_classes =
	    Util.uniquify_by_fun (fn ((_,a:DOF.output),(_,b:DOF.output)) => Term.sym2curname (#name a) = Term.sym2curname (#name b)) outputs_from_top_classes

	val output_exps = 
	    Util.flatmap (fn ((model, output), index) => CurrentModel.withModel model (fn _ => output_prog (output, index)))
			 (Util.addCount outputs_from_top_classes)


	val total_output_quantities =
	    List.foldr op+ 0 (map (List.length o #contents) (!(#outputs class)))

    in
        if total_output_quantities > 0 then
	[$(""),
	 $("// Output buffers must have at least this much free space to ensure that an output can be written."),
	 $("static const ptrdiff_t MAX_OUTPUT_SIZE = NUM_OUTPUTS*2*sizeof(int) + (NUM_OUTPUTS+" ^ (i2s total_output_quantities)  ^ ")*sizeof(CDATAFORMAT); //size in bytes"),
	 $(""),
	 $("__DEVICE__ void buffer_outputs(solver_props *props, unsigned int modelid) {"),
	 SUB([$("output_buffer *ob = props->ob;"),
	      $("output_data *od = (output_data *)props->od;")] @
	     output_exps),
	 $("}"),
	 $("")]
         else
	 []
    end
    handle e => DynException.checkpoint "CParallelWriter.logoutput_code" e

fun buildC (combinedModel as (classes, inst, props), forkedModels) =
    let
(*	val () = CurrentModel.setCurrentModel combinedModel

	val forkedModels = ModelProcess.createIteratorForkedModels model *)

	val () = 
	    let val model = CurrentModel.getCurrentModel ()
		val filename = "dof-system.json"
                fun subsystem_to_json {top_class, iter, model} =
                    let val (iter_name, iter_typ) = iter
                    in mlJS.js_object [("top_class", mlJS.js_string (Symbol.name top_class)),
                                       ("iterator", mlJS.js_string (Symbol.name iter_name)),
                                       ("model", ModelProcess.to_json model)]
                    end
		fun output outstream = 
		    mlJS.output (outstream, mlJS.js_array (map subsystem_to_json forkedModels))
	    in if ModelProcess.isDebugging model then
		   Printer.withOpenOut filename output
	       else ()
	    end
			   

	val forkedModelsLessUpdate = List.filter (fn{iter=(iter_sym, iter_type),...}=> case iter_type of DOF.UPDATE _ => false | _ => true) forkedModels
	val forkedModelsWithSolvers = List.filter (not o ModelProcess.isDependentIterator o #iter) forkedModels

	val updateModels = List.filter (fn {iter=(_, iter_typ), ...} => case iter_typ of DOF.UPDATE _ => true | _ => false) forkedModels
	val postprocessModels = List.filter (fn {iter=(_, iter_typ), ...} => case iter_typ of DOF.POSTPROCESS _ => true | _ => false) forkedModels

	val {name=inst_name, classname=class_name} = inst
	val inst_class = CurrentModel.classname2class class_name
	val orig_name = #name inst_class
	val class_name = Symbol.name orig_name

	val statespace = ClassProcess.class2statesize inst_class

	val {precision,...} = props

	val c_data_format = case precision 
			     of DOF.SINGLE => "float" 
			      | DOF.DOUBLE => "double"

	val iterators = ModelProcess.returnIndependentIterators ()
	val iterator_names = map (Symbol.name o #1) iterators
	(* grab the unique solvers so that we can put the code down for each one *)
	val unique_solvers = Util.uniquify (List.mapPartial (fn(_,itertype)=> case itertype of 
										  DOF.CONTINUOUS solver => SOME (Solver.solver2name solver)
										| DOF.DISCRETE _ => SOME "discrete"
										| DOF.IMMEDIATE => SOME "immediate"
										| _ => NONE) iterators)
	val header_progs = (header (class_name, iterator_names, unique_solvers,
				    [], (* no additional includes *)
				    []))

	val init_solver_props_c = init_solver_props orig_name forkedModelsWithSolvers		   
	val simengine_interface_progs = simengine_interface combinedModel forkedModelsLessUpdate
	(*val iteratordatastruct_progs = iteratordatastruct_code iterators*)
	val outputdatastruct_progs = outputdatastruct_code inst_class
	val outputstatestruct_progs = Util.flatmap (fn{model,...} => CurrentModel.withModel model (fn _=> outputstatestruct_code model)) forkedModelsLessUpdate
	val systemstate_progs = outputsystemstatestruct_code forkedModelsLessUpdate
	val flow_data = map flow_code forkedModels
	val fun_prototypes = List.concat (map #1 flow_data)
	val flow_progs = List.concat (map #2 flow_data)
	val logoutput_progs = logoutput_code inst_class forkedModelsLessUpdate
	val simengine_target_h = $(Archive.getC "simengine/simengine_target.h")
	val simengine_api_h = $(Archive.getC "simengine/simengine_api.h")
	val solvers_h = $(Archive.getC "solvers/solvers.h")
	val gpu_util_c = $(Archive.getC "simengine/gpu_util.c")
	val solver_gpu_cu = $(Archive.getC ("solvers/solver_gpu.cu"))
	val solver_c = $(String.concat (map
					    (fn(solv)=> Archive.getC ("solvers/"^solv^".c"))
					    unique_solvers))
	val solver_wrappers_c = solver_wrappers unique_solvers
	val iterator_wrappers_c = (update_wrapper updateModels) @ 
				  (postprocess_wrapper postprocessModels)
	val simengine_api_c = $(Archive.getC "simengine/simengine_api.c")
	val defines_h = $(Archive.getC "simengine/defines.h")
	val semeta_seint_h = $(Archive.getC "simengine/semeta_seint.h")
	val output_buffer_h = $(Archive.getC "simengine/output_buffer.h")
	val init_output_buffer_c = $(Archive.getC "simengine/init_output_buffer.c")
	val log_outputs_c = $(Archive.getC "simengine/log_outputs.c")
	val exec_cpu_c = $(Archive.getC "simengine/exec_cpu.c")
	val exec_parallel_cpu_c = $(Archive.getC "simengine/exec_parallel_cpu.c")
	val exec_serial_cpu_c = $(Archive.getC "simengine/exec_serial_cpu.c")
	val exec_kernel_gpu_cu = $(Archive.getC "simengine/exec_kernel_gpu.cu") (* Make conditional on GPU target *)
	val exec_parallel_gpu_cu = $(Archive.getC "simengine/exec_parallel_gpu.cu") (* Make conditional on GPU target *)
	val model_flows_c = model_flows forkedModelsWithSolvers
	val exec_loop_c = $(Archive.getC "simengine/exec_loop.c")

	(* write the code *)
	val _ = output_code(class_name, ".", (header_progs @
					      [simengine_target_h] @
					      (*[gpu_util_c] @ *)(* Make conditional on GPU target *)
					      simengine_interface_progs @

					      [simengine_api_h] @
					      [defines_h] @
					      [semeta_seint_h] @
					      [output_buffer_h] @
					      outputdatastruct_progs @
					      outputstatestruct_progs @
					      systemstate_progs @
					      fun_prototypes @
					      [solvers_h] @
					      (*[solver_gpu_cu] @ *)(* Make conditional on GPU target *)
					      [solver_c] @
					      (*iteratordatastruct_progs @*)
					      solver_wrappers_c @
					      iterator_wrappers_c @
					      [init_output_buffer_c] @
					      [simengine_api_c] @
					      init_solver_props_c @
					      logoutput_progs @
					      [log_outputs_c] @
					      [exec_cpu_c] @
					      [exec_parallel_cpu_c] @
					      [exec_serial_cpu_c] @ 
					      (*[exec_kernel_gpu_cu] @
					      [exec_parallel_gpu_cu] @ *)
					      flow_progs @
					      model_flows_c @
					      [exec_loop_c]))
    in
	SUCCESS
    end
    handle e => DynException.checkpoint "CParallelWriter.buildC" e

end
