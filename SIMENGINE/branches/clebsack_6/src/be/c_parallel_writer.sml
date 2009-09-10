structure CParallelWriter =
struct

datatype status =
	 SUCCESS 
       | FAILURE of string

open Printer
exception InternalError

val i2s = Util.i2s
val r2s = Util.r2s

fun cstring str = "\"" ^ str ^ "\""
fun inc x = 1 + x

(* ====================  HEADER  ==================== *)

fun header (class_name, includes, defpairs) = 
    [$("// C Execution Engine for top-level model: " ^ class_name),
     $("// " ^ Globals.copyright),
     $(""),
     $("#if defined TARGET_GPU"),
     $("#define BLOCK_SIZE 192"),
     $("#endif"),
     $(""),
     $("#include <simengine.h>"),
     $("#include <solvers.h>")] @
    (map (fn(inc)=> $("#include "^inc)) includes) @
    [$(""),
     $("")] @
    (map (fn(name,value)=> $("#define " ^ name ^ " " ^ value)) defpairs)


fun simengine_interface (class_name, class, solver_name) =
    let
	val time = Symbol.symbol "t"

	fun init_condition2pair basestr exp =
	    let val term = ExpProcess.exp2term (ExpProcess.lhs exp)
		val rhs = ExpProcess.rhs exp
	    in if Term.isInitialValue term time then
		   SOME ((if "" = basestr then "" else basestr ^ ".") ^ (Term.sym2name term), CWriterUtil.exp2c_str rhs)
	       else NONE
	    end

	fun findStatesInitValues basestr (class:DOF.class) = 
	    let
		val classname = ClassProcess.class2orig_name class
		val exps = #exps class
		val diff_eqs_symbols = map ExpProcess.lhs (List.filter ExpProcess.isFirstOrderDifferentialEq (!exps))
		val init_conditions = List.filter ExpProcess.isInitialConditionEq (!exps)
		fun exp2name exp = 
		    Term.sym2curname (ExpProcess.exp2term exp)
		    handle e => DynException.checkpoint ("CParallelWriter.simengine_interface.findStatesInitValues.exp2name ["^(ExpProcess.exp2str exp)^"]") e
				      
		val instances = List.filter ExpProcess.isInstanceEq (!exps)
		val class_inst_pairs = ClassProcess.class2instnames class
	    in
		(List.mapPartial (init_condition2pair basestr) init_conditions)
		@ (StdFun.flatmap findInstanceStatesInitValues class_inst_pairs)
	    end
	    handle e => DynException.checkpoint "CParallelWriter.simengine_interface.findStatesInitValues" e

	and findInstanceStatesInitValues (classname, instname) =
	    findStatesInitValues (Symbol.name instname) (CurrentModel.classname2class classname)


	fun default2c_str (SOME v) = CWriterUtil.exp2c_str v
	  | default2c_str NONE = 
	    DynException.stdException("Unexpected non-default value for input", "CParallelWriter.simEngineInterface", Logger.INTERNAL)
	    
	val (state_names, state_defaults)  = ListPair.unzip (findStatesInitValues "" class)
	val (input_names, input_defaults) = ListPair.unzip (map (fn{name,default}=>(name,default)) (!(#inputs class)))
	val output_names = map #name (!(#outputs class))
	val output_num_quantities = map (i2s o inc o List.length o #contents) (!(#outputs class))
	val default_inputs = map default2c_str input_defaults
    in
	[$("const char *input_names[] = {" ^ (String.concatWith ", " (map (cstring o Term.sym2name) input_names)) ^ "};"),
	 $("const char *state_names[] = {" ^ (String.concatWith ", " (map cstring state_names)) ^ "};"),
	 $("const char *output_names[] = {" ^ (String.concatWith ", " (map (cstring o Term.sym2name) output_names)) ^ "};"),
	 $("const double default_inputs[] = {" ^ (String.concatWith ", " default_inputs) ^ "};"),
	 $("const double default_states[] = {" ^ (String.concatWith ", " state_defaults) ^ "};"),
	 $("const unsigned int output_num_quantities[] = {" ^ (String.concatWith ", " output_num_quantities) ^ "};"),
	 $("const char model_name[] = \"" ^ class_name ^ "\";"),
	 $("const char solver[] = \"" ^ solver_name ^ "\";"),
	 $("#if defined TARGET_CPU"),
	 $("const char target[] = \"cpu\";"),
	 $("#elif defined TARGET_OPENMP"),
	 $("const char target[] = \"openmp\";"),
	 $("#elif defined TARGET_GPU"),
	 $("const char target[] = \"gpu\";"),
	 $("#endif"),
	 $(""),
	 $("const simengine_metadata semeta = {"),
	 SUB[$("0x0000000000000000ULL, // hashcode"),
	     $("NUM_MODELS,"),
	     $("solver,"),
	     $("target,"),
	     $("sizeof(CDATAFORMAT)")
	    ],
	 $("};"),
	 $(""),
	 $("#define NUM_INPUTS "^(i2s (List.length input_names))),
	 $("#define NUM_STATES "^(i2s (List.length state_names))),
	 $("#define NUM_OUTPUTS "^(i2s (List.length output_names))),
	 $(""),
	 $("const simengine_interface seint = {"),
	 SUB[$("0, // Version,"),
	     $("NUM_INPUTS, // Number of inputs"),
	     $("NUM_STATES, // Number of states"),
	     $("NUM_OUTPUTS, // Number of outputs"),
	     $("input_names,"),
	     $("state_names,"),
	     $("output_names,"),
	     $("default_inputs,"),
	     $("default_states,"),
	     $("output_num_quantities,"),
	     $("model_name,"),
	     $("&semeta")],
	 $("};"),
	 $(""),
	 $("simengine_alloc se_alloc = { malloc, realloc, free };"),
	 $("")]
    end

local
    fun output2struct (term,sym) =
	$("CDATAFORMAT " ^(Symbol.name sym)^";")
in
fun outputdatastruct_code class =
    [$("typedef struct {"),
     SUB(map output2struct (CWriterUtil.class2uniqueoutputsymbols class)),
     $("} output_data;"),
     $(""),
     $("output_data OD[NUM_MODELS];")
    ]
end

fun initbyclass_code (class as {exps, ...}) =
    let
	val classname = (*ClassProcess.class2orig_name class*) ClassProcess.class2classname class
	val exps = #exps class
	val diff_eqs_symbols = map ExpProcess.lhs (List.filter ExpProcess.isFirstOrderDifferentialEq (!exps))
	val instances = List.filter ExpProcess.isInstanceEq (!exps)
	val class_inst_pairs = ClassProcess.class2instnames class
    in
	[$(""),
	 $("// define state structures"),
	 $("struct statedata_" ^ (Symbol.name classname) ^ " {"),	 
	 SUB($("// states (count="^(i2s (List.length diff_eqs_symbols))^")")::
	     (map (fn(sym)=>
		     let
			 val size = Term.symbolSpatialSize (ExpProcess.exp2term sym)
			 val name = Symbol.name (Term.sym2curname (ExpProcess.exp2term sym))
		     in
			 if size = 1 then
			     $("CDATAFORMAT " ^ name ^ "[ARRAY_SIZE];")
			 else
			     $("CDATAFORMAT " ^ name ^ "["^(i2s size)^"*ARRAY_SIZE];")
		     end) diff_eqs_symbols) @
	     ($("// instances (count=" ^ (i2s (List.length class_inst_pairs)) ^")")::
	      (map 
		   (fn(classname, instname)=>
		      let			  
			  val size = 
			      case List.find (fn(inst)=> ExpProcess.instOrigInstName inst = instname) instances 
			       of SOME inst' => ExpProcess.instSpatialSize inst'
				| NONE => 1
		      in
			  if size = 1 then
			      $("struct statedata_" ^ (Symbol.name classname) ^ " "^(Symbol.name instname)^";")
			  else
			      $("struct statedata_" ^ (Symbol.name classname) ^ " "^(Symbol.name instname)^"["^(i2s size)^"];")
		      end)
		   class_inst_pairs))),
	 $("};")]
    end

local
    fun state2member (sym) =
	let
	    val size = Term.symbolSpatialSize (ExpProcess.exp2term sym)
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
	    "struct statedata_" ^ (Symbol.name classname) ^ " " ^ (Symbol.name instname) ^ index
	end
in
fun outputstatestructbyclass_code (class : DOF.class as {exps, ...}) =
    let
	val classname = (*ClassProcess.class2orig_name class*) ClassProcess.class2classname class
	val diff_eqs_symbols = map ExpProcess.lhs (List.filter ExpProcess.isFirstOrderDifferentialEq (!exps))
	val instances = List.filter ExpProcess.isInstanceEq (!exps)
	val class_inst_pairs = ClassProcess.class2instnames class
    in
	[$(""),
	 $("// define state structures"),
	 $("struct statedata_" ^ (Symbol.name classname) ^ " {"),	 
	 SUB($("// states (count="^(i2s (List.length diff_eqs_symbols))^")") ::
	     (map ($ o state2member) diff_eqs_symbols) @
	     ($("// instances (count=" ^ (i2s (List.length class_inst_pairs)) ^")") ::
	      (map ($ o instance2member instances) class_inst_pairs))),
	 $("};")]
    end
end

fun outputstatestruct_code classes =
    let
	fun isMasterClass {properties={classtype,...},...} =
	    case classtype of
		DOF.MASTER _ => true
	      | _ => false
	val master_classes = List.filter isMasterClass classes

	val predeclare_statements = 
	    map
		(fn(class)=> $("struct statedata_" ^ (Symbol.name ((*ClassProcess.class2orig_name*) ClassProcess.class2classname class)) ^ ";"))
		master_classes

    in
	($("")::($("// Pre-declare state structures"))::predeclare_statements) @
	List.concat (map outputstatestructbyclass_code master_classes)
    end

fun class2flow_code (class, top_class) =
    let
	val orig_name = ClassProcess.class2orig_name class

	val header_progs = 
	    [$(""),
	     $("__HOST__ __DEVICE__ int flow_" ^ (Symbol.name (#name class)) 
	       ^ "(CDATAFORMAT t, const struct statedata_"^(Symbol.name orig_name)^" *y, struct statedata_"^(Symbol.name orig_name)^" *dydt, CDATAFORMAT *inputs, CDATAFORMAT *outputs, unsigned int first_iteration, unsigned int modelid) {")]

	val read_memory_progs = []

	val read_states_progs = []
	    
	val read_inputs_progs =
	    [$(""),
(*	     $("const struct statedata_"^(Symbol.name orig_name)^" *y = &py[modelid];"),
	     $("struct statedata_"^(Symbol.name orig_name)^" *dydt = &pdydt[modelid];"),
*)
	     $("// mapping inputs to variables")] @ 
	    (map
		 (if top_class then
		     (fn({name,default},i)=> $("CDATAFORMAT " ^ (CWriterUtil.exp2c_str (Exp.TERM name)) ^ " = inputs[TARGET_IDX(NUM_INPUTS, NUM_MODELS, " ^ (i2s i) ^ ", modelid)];"))
		 else
		     (fn({name,default},i)=> $("CDATAFORMAT " ^ (CWriterUtil.exp2c_str (Exp.TERM name)) ^ " = inputs[" ^ (i2s i) ^ "];")))
		 (Util.addCount (!(#inputs class))))


	(* filter out all the unneeded expressions *)
	val (initvalue_exps, rest_exps) = List.partition ExpProcess.isInitialConditionEq (!(#exps class))
	val (valid_exps, rest_exps) = List.partition (fn(exp)=>(ExpProcess.isIntermediateEq exp) orelse
							     (ExpProcess.isInstanceEq exp) orelse
							     (ExpProcess.isFirstOrderDifferentialEq exp)) rest_exps
	val _ = if (List.length rest_exps > 0) then
		    (Logger.log_error($("Invalid expressions reached in code writer while writing class " ^ (Symbol.name (ClassProcess.class2orig_name class))));
		     app (fn(exp)=> Util.log ("  Offending expression: " ^ (ExpProcess.exp2str exp))) rest_exps;
		     DynException.setErrored())
		else
		    ()

	val equ_progs = 
	    [$(""),
	     $("// writing all intermediate, instance, and differential equation expressions")] @
	    (let
		 val progs =
		     Util.flatmap
			 (fn(exp)=>
			    if (ExpProcess.isIntermediateEq exp) then
 				[$("CDATAFORMAT " ^ (CWriterUtil.exp2c_str exp) ^ ";")]
			    else if (ExpProcess.isFirstOrderDifferentialEq exp) then
 				[$((CWriterUtil.exp2c_str exp) ^ ";")]
			    else if (ExpProcess.isInstanceEq exp) then
				let
				    val {classname, instname, props, inpargs, outargs} = ExpProcess.deconstructInst exp
				    val orig_instname = case Fun.getRealInstName props of
							     SOME v => v
							   | NONE => instname

				    val class = CurrentModel.classname2class classname

				    val calling_name = "flow_" ^ (Symbol.name classname)

				    val inpvar = Unique.unique "inputdata"
				    val outvar = Unique.unique "outputdata"

(*				    val inps = "CDATAFORMAT " ^ inpvar ^ "[] = {" ^ (String.concatWith ", " (map CWriterUtil.exp2c_str inpargs)) ^ "};"*)
				    val inps = "CDATAFORMAT " ^ inpvar ^ "[" ^ (i2s (List.length inpargs)) ^ "];"
				    val inps_init = map ( fn(inparg, idx) => $(inpvar ^ "[" ^ (i2s idx) ^ "] = " ^ CWriterUtil.exp2c_str inparg ^ ";"))(Util.addCount inpargs)
				    val outs_decl = "CDATAFORMAT " ^ outvar ^ "["^(i2s (List.length outargs))^"];"

				    val symbols = map Term.sym2curname outargs

				    fun inst_output ((sym, {name, contents, condition}), idx) =
					"CDATAFORMAT " ^ (Symbol.name sym) ^ " = " ^ outvar ^ "[" ^ (i2s idx) ^ "];" ^
					" // Mapped to "^ (Symbol.name classname) ^ ": " ^ (ExpProcess.exp2str (List.hd (contents)))

				in
				    [SUB([$("// Calling instance class " ^ (Symbol.name classname)),
					  $("// " ^ (CWriterUtil.exp2c_str exp)),
					  $(inps)] @ inps_init @ [$(outs_decl),
					  if top_class then
					      $(calling_name ^ "(t, &y[STRUCT_IDX]."^(Symbol.name orig_instname)^", &dydt[STRUCT_IDX]."^(Symbol.name orig_instname)^", "^inpvar^", "^outvar^", first_iteration, modelid);")
					  else
					      $(calling_name ^ "(t, &y->"^(Symbol.name orig_instname)^", &dydt->"^(Symbol.name orig_instname)^", "^inpvar^", "^outvar^", first_iteration, modelid);")
					 ] @
					 map ($ o inst_output)
					     (Util.addCount (ListPair.zip (symbols, !(#outputs class)))))

				    ]
				end
			    else
				DynException.stdException(("Unexpected expression '"^(ExpProcess.exp2str exp)^"'"), "CParallelWriter.class2flow_code.equ_progs", Logger.INTERNAL)
			 )
			 valid_exps
	     in
		 progs
	     end)
	    
	val state_progs = []

	val output_progs = 
	    if top_class then
		[$(""),
		 $("// writing output variables"),
		 $("if (first_iteration) {"),
		 SUB($("output_data *od = (output_data*)outputs;")::
		     (map
			  (fn(t,s)=> $("od[modelid]." ^ (Symbol.name s) ^ " = " ^ (CWriterUtil.exp2c_str (Exp.TERM t)) ^ ";"))
			  (CWriterUtil.class2uniqueoutputsymbols class))),
		 $("}")]
	    else
		[$(""),
		 $("// writing output data "),
		 SUB(map 
			 (fn({name,contents,condition},i)=> 
			    let
				val _ = if length contents = 1 then
					    ()
					else
					    DynException.stdException (("Output '"^(ExpProcess.exp2str (Exp.TERM name))^"' in class '"^(Symbol.name (#name class))^"' can not be a grouping of {"^(String.concatWith ", " (map ExpProcess.exp2str contents))^"} when used as a submodel"), "CParallelWriter.class2flow_code", Logger.INTERNAL)
					    
				val valid_condition = case condition 
						       of (Exp.TERM (Exp.BOOL v)) => v
							| _ => false
				val _ = if valid_condition then
					    ()
					else
					    DynException.stdException (("Output '"^(ExpProcess.exp2str (Exp.TERM name))^"' in class '"^(Symbol.name (#name class))^"' can not have a condition '"^(ExpProcess.exp2str condition)^"' when used as a submodel"), "CParallelWriter.class2flow_code", Logger.INTERNAL)
					    
			    in
				case contents of
				    [content] =>
				    $("outputs["^(i2s i)^"] = " ^ (CWriterUtil.exp2c_str (content)) ^ ";")
				  | _ => 
				    DynException.stdException (("Output '"^(ExpProcess.exp2str (Exp.TERM name))^"' in class '"^(Symbol.name (#name class))^"' can not be a grouping of {"^(String.concatWith ", " (map ExpProcess.exp2str contents))^"} when used as a submodel"), 
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

fun flow_code (classes: DOF.class list, topclass: DOF.class) = 
    let
	fun isInline (class: DOF.class) =
	    let
		val {properties={classform,...},...} = class
	    in
		case classform of 
		    DOF.FUNCTIONAL => true
		  | _ => false
	    end

	val fundecl_progs = map
				(fn(class) => 
				   let
				       val orig_name = ClassProcess.class2orig_name class
				   in
				       if isInline class then
					   $("CDATAFORMAT "^(Symbol.name (#name class))^"("^(String.concatWith ", " (map (fn{name,...}=> "CDATAFORMAT " ^ (CWriterUtil.exp2c_str (Exp.TERM name))) (!(#inputs class))))^");")
				       else
					   $("__HOST__ __DEVICE__ int flow_" ^ (Symbol.name (#name class)) ^ "(CDATAFORMAT t, const struct statedata_"^(Symbol.name orig_name)^" *y, struct statedata_"^(Symbol.name orig_name)^" *dydt, CDATAFORMAT *inputs, CDATAFORMAT *outputs, unsigned int first_iteration, unsigned int modelid);")
				   end)
				classes
	val iterators = CurrentModel.iterators()
				
	val flow_progs = List.concat (map (fn(c)=>
					     if isInline c then
						 (Logger.log_error ($("Functional classes like '"^(Symbol.name (#name c))^"' are not supported"));
						  DynException.setErrored();
						  [])
					     else
						 class2flow_code (c,#name c = #name topclass)) classes)

	val top_level_flow_progs = 
	    [$"",
	     $("__HOST__ __DEVICE__ int model_flows(CDATAFORMAT t, const CDATAFORMAT *y, CDATAFORMAT *dydt, CDATAFORMAT *inputs, CDATAFORMAT *outputs, unsigned int first_iteration, unsigned int modelid){"),
	     SUB[$("return flow_" ^ (Symbol.name (#name topclass)) ^ "(t, (const struct statedata_"^(Symbol.name (ClassProcess.class2orig_name topclass))^"*)y, (struct statedata_"^(Symbol.name (ClassProcess.class2orig_name topclass))^"*)dydt, inputs, outputs, first_iteration, modelid);")],
	     $("}"),
	     $(""),
	     $("EXTERN_C"),
	     $("int simengine_evalflow(double t, double *y, double *dydt, double *inputs) {"),
	     SUB[$("CDATAFORMAT *outputs; // should never be written to here since first_iteration is zero"),
		 $("int first_iteration = 0;"),
		 $("int modelid = 0;"),
		 $("return flow_" ^ (Symbol.name (#name topclass)) ^ "(t, (const struct statedata_"^(Symbol.name (ClassProcess.class2orig_name topclass))^"*)y, (struct statedata_"^(Symbol.name (ClassProcess.class2orig_name topclass))^"*)dydt, (CDATAFORMAT*) inputs, outputs, first_iteration, modelid);")],
	     $("}")]
    in
	[$("// Flow code function declarations")] @
	fundecl_progs @
	flow_progs @
	top_level_flow_progs
    end


fun output_code (name, location, block) =
    let
      val filename = location ^ "/" ^ name ^ "_parallel.c"
      val _ = Logger.log_notice ($("Generating C source file '"^ filename ^"'"))
      val file = TextIO.openOut (filename)
    in
      Printer.printtexts (file, block, 0)
      before TextIO.closeOut file
    end

fun props2solver props =
    let
    	val iterators = #iterators props
	val solver = case List.find (fn(sym, itertype) => 
				       (case itertype of
					    DOF.CONTINUOUS solver => true
					  | DOF.DISCRETE => false)) iterators of
			 SOME (sym, DOF.CONTINUOUS solver) => solver
		       | _ => DynException.stdException ("Requiring at least one differential equation", "CParallelWriter.buildC", Logger.INTERNAL)
    in
	solver
    end

fun exec_code (class:DOF.class, props, statespace) =
    let
	val orig_name = Symbol.name (ClassProcess.class2orig_name class)
    in
	[$(""),
	 $("//__DEVICE__ unsigned int Stop;"),
	 $(""),
	 $("//__GLOBAL__ void clearStop(void) {"),
	 SUB[$("//Stop = 0;")],
	 $("//}"),
	 $(""),
	 $("#if defined TARGET_GPU"),
	 $("__GLOBAL__ void exec_kernel_gpu(INTEGRATION_MEM *mem){"),
	 SUB[$("const unsigned int modelid = blockIdx.x * blockDim.x + threadIdx.x;"),
	     $(""),
	     $("unsigned int num_iterations;"),
	     $(""),
	     $("if (modelid < NUM_MODELS) {"),
	     $(""),
	     $("init_output_buffer((output_buffer*)(mem->props->ob), modelid);"),
	     $(""),
	     $("for(num_iterations = 0; num_iterations < MAX_ITERATIONS; num_iterations++){"),
	     SUB[$("// Check if simulation is complete"),
		 $("if(((output_buffer*)(mem->props->ob))->finished[modelid]){"),
		 SUB[$("break;")],
		 $("}"),
		 $("if(mem->props->time[modelid] >= mem->props->stoptime){"),
		 SUB[$("((output_buffer*)(mem->props->ob))->finished[modelid] = 1;"),
		     $("atomicDec(&((output_buffer*)(mem->props->ob))->active_models, mem->props->num_models);"),
		     $("break;")],
		 $("}"),
		 $(""),
		 $("CDATAFORMAT prev_time = mem->props->time[modelid];"),
		 $("SOLVER(INTEGRATION_METHOD, eval, TARGET, SIMENGINE_STORAGE, mem, modelid);"),
		 $("if (mem->props->time[modelid] > prev_time) {"),
		 SUB[$("buffer_outputs(mem->props->time[modelid], (output_data*)mem->props->outputs, (output_buffer*)mem->props->ob, modelid);")],
		 $("}"),
		 $("else { break; }"),
		 $(""),
		 $("// Check if buffer is full - may not be needed"),
		 $("//atomicOr(&Stop, ((output_buffer*)(mem->props->ob))->full[modelid]);"),
		 $("if(((output_buffer*)(mem->props->ob))->full[modelid]) { break; }"),
		 $("// Break if any buffer is full"),
		 $("//if(Stop) { break; }")],
	     $("}"),
	     $(""),
	     $("}")],
	 $("}"),
	 $("#endif"),
	 $(""),
	 $("#if defined TARGET_OPENMP"),
	 $("void exec_kernel_openmp(INTEGRATION_MEM *mem, unsigned int modelid){"),
	 SUB[$("unsigned int num_iterations;"),
	     $(""),
	     $("init_output_buffer((output_buffer*)(mem->props->ob), modelid);"),
	     $(""),
	     $("for(num_iterations = 0; num_iterations < MAX_ITERATIONS; num_iterations++){"),
	     SUB[$("// Check if simulation is complete"),
		 $("if(((output_buffer*)(mem->props->ob))->finished[modelid]){"),
		 SUB[$("break;")],
		 $("}"),
		 $("if(mem->props->time[modelid] >= mem->props->stoptime){"),
		 SUB[$("((output_buffer*)(mem->props->ob))->finished[modelid] = 1;"),
		     $("#pragma omp critical"),
		     SUB[$("--((output_buffer*)(mem->props->ob))->active_models;")],
		     $("break;")],
		 $("}"),
		 $(""),
		 $("CDATAFORMAT prev_time = mem->props->time[modelid];"),
		 $("SOLVER(INTEGRATION_METHOD, eval, TARGET, SIMENGINE_STORAGE, mem, modelid);"),
		 $("if (mem->props->time[modelid] > prev_time) {"),
		 SUB[$("buffer_outputs(mem->props->time[modelid], ((output_data*)mem->props->outputs), ((output_buffer*)mem->props->ob), modelid);")],
		 $("}"),
		 $("else { break; }"),
		 $(""),
		 $("if (((output_buffer *)(mem->props->ob))->full[modelid])"),
		 SUB[$("{ break; }")]],
	     $("}")],
	 $("}"),
	 $("#endif"),
	 $(""),
	 $("int exec_loop(CDATAFORMAT *t, CDATAFORMAT t1, CDATAFORMAT *inputs, CDATAFORMAT *model_states, simengine_output *outputs) {"),
	 SUB[$("unsigned int modelid = 0;"),
	     $("unsigned int status = SUCCESS;"),
	     $("solver_props props;"),
	     $("props.timestep = DT;"),
	     $("props.abstol = ABSTOL;"),
	     $("props.reltol = RELTOL;"),
	     $("props.starttime = *t;"),
	     $("props.stoptime = t1;"),
	     $("props.time = t;"),
	     $("props.model_states = model_states;"),
	     $("props.inputs = inputs;"),
	     $("props.outputs = (CDATAFORMAT*)OD;"),
	     $("props.first_iteration = TRUE;"),
	     $("props.statesize = seint.num_states;"),
	     $("props.inputsize = seint.num_inputs;"),
	     $("props.outputsize = sizeof(output_data)/sizeof(CDATAFORMAT); // This is the size of the information needed to produce outputs, not just the number of named outputs"),
	     $("props.num_models = semeta.num_models;"),
	     $("props.ob_size = sizeof(output_buffer);"),
	     $("props.ob = &OB; // FIXME - switch to dynamic allocation don't forget to free!"),
	     $(""),
	     $("for(modelid=0;modelid<semeta.num_models;modelid++){"),
	     SUB[$("OB.finished[modelid] = 0;")],
	     $("}"),
	     $("OB.active_models = NUM_MODELS;"),
	     $(""),
	     $("INTEGRATION_MEM *mem = SOLVER(INTEGRATION_METHOD, init, TARGET, SIMENGINE_STORAGE, &props);"),
	     $(""),
	     $(""),
	     $("while (OB.active_models) {"),
	     $("#if defined TARGET_OPENMP"),
	     $("omp_set_num_threads(omp_get_num_procs());"),
	     $("#pragma omp parallel"),
	     $("{"),
	     SUB[$("unsigned int mid;"),
		 $("unsigned int thread_num = omp_get_thread_num();"),
		 $("unsigned int num_threads = omp_get_num_threads();"),
		 $(""),
		 $("unsigned int models_per_thread = NUM_MODELS/num_threads;"),
		 $("unsigned int extra_models = NUM_MODELS%num_threads;"),
		 $(""),
		 $("// Run models in batches equal to the number of cores"),
		 $("for(mid = thread_num*models_per_thread; mid < (thread_num+1)*models_per_thread; mid++){"),
		 SUB[$("exec_kernel_openmp(mem, mid);"),
		     $("if(0 != log_outputs(&OB, outputs, mid))"),
		     SUB[$("{ status = ERRMEM; }")]],
		 $("}"),
		 $("// If the number of models is not an even multiple of the number of cores"),
		 $("// there will be one additional batch of models that is less than the number of cores"),
		 $("if(thread_num < extra_models){"),
		 SUB[$("mid = num_threads * models_per_thread + thread_num;"),
		     $("exec_kernel_openmp(mem, mid);"),
		     $("if(0 != log_outputs(&OB, outputs, mid))"),
		     SUB[$("{ status = ERRMEM; }")]],
		 $("}")],
	     $("}"),
	     $("if(status != SUCCESS) break;"),
	     $("#elif defined TARGET_GPU"),
	     $("unsigned int nthreads, nblocks;"),
	     $("nthreads = BLOCK_SIZE < NUM_MODELS ? BLOCK_SIZE : NUM_MODELS;"),
	     $("nblocks = (NUM_MODELS + BLOCK_SIZE - 1) / BLOCK_SIZE;"),
	     $(""),
	     $("//clearStop<<<1,1>>>();"),
	     $("exec_kernel_gpu<<<nblocks, nthreads>>>(mem);"),
	     $("cutilSafeCall(cudaMemcpy(&OB, props.ob, props.ob_size, cudaMemcpyDeviceToHost));"),
	     $("for (modelid = 0; modelid < semeta.num_models; ++modelid) {"),
	     SUB[$("if (0 != log_outputs(&OB, outputs, modelid))"),
		 SUB[$("{ return ERRMEM; }")]],
	     $("}"),
	     $("#else"),
	     $("for(modelid=0;modelid<NUM_MODELS;modelid++){"),
	     SUB[$("init_output_buffer(&OB, modelid);"),
		 $("while (t[modelid] < t1) {"),
		 SUB[$("CDATAFORMAT prev_time = mem->props->time[modelid];"),
		     $("int status = SOLVER(INTEGRATION_METHOD, eval, TARGET, SIMENGINE_STORAGE, mem, modelid);"),
		     $("if (mem->props->time[modelid] > prev_time) {"),
		     SUB[$("buffer_outputs(mem->props->time[modelid], OD, ((output_buffer*)mem->props->ob), modelid);")],
		     $("}"),
		     $("else {return ERRCOMP;}"),
		     $("if (status != 0) {"),
		     SUB[$("return ERRCOMP;")],
		     $("}"),
		     $("if (OB.full[modelid]){"),
		     SUB[$("if(0 != log_outputs(&OB, outputs, modelid))"),
			 SUB[$("{ return ERRMEM; }")],
			 $("init_output_buffer(&OB, modelid);")
			],
		     $("}")],
		 $("}"),
		 $("OB.active_models--;"),
		 $("if(0 != log_outputs(&OB, outputs, modelid))"),
		 SUB[$("{ return ERRMEM; }")]],
	     $("}"),
	     $("#endif"),
	     $("}"),
	     $(""),
	     $("#ifdef TARGET_GPU"),
	     $("// Copy back final times and states from GPU"),
	     $("cutilSafeCall(cudaMemcpy(t, props.time, props.num_models*sizeof(CDATAFORMAT), cudaMemcpyDeviceToHost));"),
	     $("cutilSafeCall(cudaMemcpy(model_states, props.model_states, props.statesize*props.num_models*sizeof(CDATAFORMAT), cudaMemcpyDeviceToHost));"),
	     $("#endif"),
	     $(""),
	     $("SOLVER(INTEGRATION_METHOD, free, TARGET, SIMENGINE_STORAGE, mem);")],
	 $("return status;"),
	 $("}")]
    end

fun logoutput_code class =
    let
	val orig_name = Symbol.name (ClassProcess.class2orig_name class)
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
	val output_exps =Util.flatmap
			      (fn(out as ({condition, contents, name}, output_index))=> 
				 [$("{ // Generating output for symbol " ^ (ExpProcess.exp2str (Exp.TERM name))),
				  SUB[$("int cond = " ^ (CWriterUtil.exp2c_str (ExpProcess.assignToOutputBuffer condition)) ^ ";"),
				      $("if (cond) {"),
				      SUB([$("((unsigned int*)(ob->ptr[modelid]))[0] = " ^ (i2s output_index) ^ ";"),
					   $("((unsigned int*)(ob->ptr[modelid]))[1] = " ^ (i2s (inc (List.length contents))) ^ ";"),
					   $("ob->ptr[modelid] = &((unsigned int*)(ob->ptr[modelid]))[2];"),
					   $("*((CDATAFORMAT*)(ob->ptr[modelid])) = t;"),
					   $("ob->ptr[modelid] = &((CDATAFORMAT*)(ob->ptr[modelid]))[1];")] @
					  (Util.flatmap (fn (exp) =>
							    [$("*((CDATAFORMAT*)(ob->ptr[modelid])) = "^(CWriterUtil.exp2c_str (ExpProcess.assignToOutputBuffer exp))^";"),
							      $("ob->ptr[modelid] = &((CDATAFORMAT*)(ob->ptr[modelid]))[1];")])
							contents) @
					  [$("ob->count[modelid]++;"),
					   $("ob->full[modelid] |= MAX_OUTPUT_SIZE >= ((unsigned long long)(ob->end[modelid]) - (unsigned long long)(ob->ptr[modelid]));")]),
				      $("}")],
				  $("}")]
			      )
			      (Util.addCount(!(#outputs class)))

	val total_output_quantities =
	    List.foldr op+ 0 (map (List.length o #contents) (!(#outputs class)))

    in
	[$(""),
	 $("#define MAX_OUTPUT_SIZE (NUM_OUTPUTS*2*sizeof(int) + (NUM_OUTPUTS+" ^ (i2s total_output_quantities)  ^ ")*sizeof(CDATAFORMAT)) //size in bytes"),
	 $(""),
	 $("/* An internal data structure that maintains a buffer of output data."),
	 $(" *"),
	 $(" * The 'count' array tracks the number of data produced for each model."),
	 $(" *"),
	 $(" * The 'buffer' array comprises a list of tagged output data for each"),
	 $(" * model having the format:"),
	 $(" *     {tag, count, quantities[count]}"),
	 $(" * where 'tag' is an integer identifying a model output, 'count' is a"),
	 $(" * counter specifying the number of data quantities, and 'quantities'"),
	 $(" * is an array of actual data points."),
	 $(" *"),
	 $(" * The 'ptr' and 'end' pointers are references to positions within 'buffer.'"),
	 $(" */"),
	 $("typedef struct{"),
	 SUB[$("unsigned int active_models;"),
	     $("unsigned int finished[NUM_MODELS];"),
	     $("unsigned int full[NUM_MODELS];"),
	     $("unsigned int count[NUM_MODELS];"),
	     $("void *ptr[NUM_MODELS];"),
	     $("void *end[NUM_MODELS];"),
	     $("CDATAFORMAT buffer[BUFFER_LEN*NUM_MODELS];")],
	 $("} output_buffer;"),
	 $(""),
	 $("output_buffer OB;"),
	 $(""),
	 $("int log_outputs(output_buffer *, simengine_output *, unsigned int);"),
	 $(""),
	 $("__DEVICE__ void init_output_buffer(output_buffer *ob, unsigned int modelid){"),
	 SUB[$("ob->full[modelid] = 0;"),
	     $("ob->count[modelid] = 0;"),
	     $("ob->ptr[modelid] = &ob->buffer[modelid*BUFFER_LEN];"),
	     $("ob->end[modelid] = &ob->buffer[(modelid+1)*BUFFER_LEN];")],
	 $("}"),
	 $(""),
	 $("__DEVICE__ void buffer_outputs(double t, output_data *od, output_buffer *ob, unsigned int modelid) {"),
	 SUB([$("")] @
	     output_exps @
	     [$("")]
	    ),
	 $("}")]
    end


fun main_code class =
    let
	val name = Symbol.name (#name class)
	val orig_name = Symbol.name (ClassProcess.class2orig_name class)
    in
	[$("/* Transmutes the internal data buffer into the structured output"),
	 $(" * which may be retured to the client."),
	 $(" */"),
	 $("int log_outputs(output_buffer *ob, simengine_output *outputs, unsigned int modelid) {"),
	 SUB[$("unsigned int outputid, nquantities, dataid, quantityid;"),
	     $("simengine_output *output;"),
	     $("double *odata;"),
	     $(""),
	     $("unsigned int ndata = ob->count[modelid];"),
	     $("void *data = &(ob->buffer[modelid * BUFFER_LEN]);"),
	     $(""),
	     $("for (dataid = 0; dataid < ndata; ++dataid) {"),
	     SUB[$("outputid = ((unsigned int *)data)[0];"),
		 $("nquantities = ((unsigned int *)data)[1];"),
		 $("data = &((unsigned int*)data)[2];"),
		 $(""),
		 $("// TODO an error code for invalid data?"),
		 $("if (outputid > seint.num_outputs) { return 1; }"),
		 $("if (seint.output_num_quantities[outputid] != nquantities) { return 1; }"),
		 $(""),
		 $("output = &outputs[AS_IDX(seint.num_outputs,semeta.num_models,outputid,modelid)];"),
		 $(""),
		 $("if (output->num_samples == output->alloc) {"),
		 SUB[$("output->alloc *= 2;"),
		     $("#pragma omp critical"),
		     $("{"),
		     SUB[$("output->data = (double*)se_alloc.realloc(output->data, output->num_quantities * output->alloc * sizeof(double));")],
		     $("}"),
		     $("if (!output->data)"),
			 SUB[$("{ return 1; }")]],
		 $("}"),
		 $(""),
		 $("odata = &output->data[AS_IDX(nquantities, output->num_samples, 0, output->num_samples)];"),
		 $(""),
		 $("for (quantityid = 0; quantityid < nquantities; ++quantityid) {"),
		 SUB[$("odata[quantityid] = *((CDATAFORMAT*)data);"),
		     $("data = &((CDATAFORMAT*)data)[1];")],
		 $("}"),
		 $(""),
		 $("++output->num_samples;")],
	     $("}"),
	     $(""),
	     $("return 0;")],
	 $("}"),
	 $(""),
	 $("EXTERN_C"),
	 $("const simengine_interface *simengine_getinterface(){"),
	 SUB[$("return &seint;")],
	 $("}"),
	 $(""),
	 $("EXTERN_C"),
	 $("simengine_result *simengine_runmodel(double start_time, double stop_time, unsigned int num_models, double *inputs, double *states, simengine_alloc *alloc){"),
	 SUB[$("CDATAFORMAT model_states[NUM_MODELS * NUM_STATES];"),
	     $("CDATAFORMAT parameters[NUM_MODELS * NUM_INPUTS];"),
	     $("CDATAFORMAT t[NUM_MODELS];"),
	     $("CDATAFORMAT t1 = stop_time;"),
	     $("unsigned int stateid;"),
	     $("unsigned int modelid;"),
	     $("unsigned int inputid;"),
	     $("unsigned int outputid;"),
	     $(""),
	     $("// Set up allocation functions"),
	     $("if(alloc){"),
	     SUB[$("se_alloc.malloc = alloc->malloc;"),
		 $("se_alloc.realloc = alloc->realloc;"),
		 $("se_alloc.free = alloc->free;")],
	     $("}"),
	     (*
	     $("else{"),
	     SUB[$("se_alloc.malloc = malloc;"),
		 $("se_alloc.realloc = realloc;"),
		 $("se_alloc.free = free;")],
	     $(" }"),
	     *)
	     $(""),
	     $("// Create result structure"),
	     $("simengine_result *seresult = (simengine_result*)se_alloc.malloc(sizeof(simengine_result));"),
	     $(""),
	     $("// Couldn't allocate return structure, return NULL"),
	     $("if(!seresult) return NULL;"),
	     $(""),
	     $("// Check that the number of models matches"),
	     $("if(num_models != semeta.num_models){"),
	     SUB[$("seresult->status = ERRNUMMDL;"),
		 $("seresult->status_message = simengine_errors[ERRNUMMDL];"),
		 $("seresult->outputs = NULL;"),
		 $("seresult->final_states = NULL;"),
		 $("seresult->final_time = NULL;"),
		 $("return seresult;")],
	     $("}"),
	     $(""),
	     $("// Allocate return structures"),
	     $("seresult->outputs = (simengine_output*)se_alloc.malloc(semeta.num_models * seint.num_outputs * sizeof(simengine_output));"),
	     $("seresult->final_states = (double*)se_alloc.malloc(semeta.num_models * seint.num_states * sizeof(double));"),
	     $("seresult->final_time = (double*)se_alloc.malloc(semeta.num_models * sizeof(double));"),
	     $("if(!seresult->outputs || !seresult->final_states ||!seresult->final_time){"),
	     SUB[$("seresult->status = ERRMEM;"),
		 $("seresult->status_message = simengine_errors[ERRMEM];"),
		 $("seresult->outputs = NULL;"),
		 $("seresult->final_states = NULL;"),
		 $("seresult->final_time = NULL;"),
		 $("return seresult;")],
	     $("}"),
	     $(""),
	     $("// Copy inputs and state initial values to internal representation"),
	     $("for(modelid=0; modelid<semeta.num_models; modelid++){"),
	     SUB[$("t[modelid] = start_time;"),
		 $("for(stateid=0;stateid<seint.num_states;stateid++){"),
		 SUB[$("model_states[TARGET_IDX(seint.num_states, semeta.num_models, stateid, modelid)] = states[AS_IDX(seint.num_states, semeta.num_models, stateid, modelid)];")],
		 $("}"),
		 $("for(inputid=0;inputid<seint.num_inputs;inputid++){"),
		 SUB[$("parameters[TARGET_IDX(seint.num_inputs, semeta.num_models, inputid, modelid)] = inputs[AS_IDX(seint.num_inputs, semeta.num_models, inputid, modelid)];")],
		 $("}")],
	     $("}"),
	     $(""),
	     $("// Initialization of output structures"),
	     $("for (modelid = 0; modelid < semeta.num_models; ++modelid) {"),
	     SUB[$("for (outputid = 0; outputid < seint.num_outputs; ++outputid) {"),
		 SUB[$("seresult->outputs[AS_IDX(seint.num_outputs, semeta.num_models, outputid, modelid)].alloc = START_SIZE;"),
		     $("seresult->outputs[AS_IDX(seint.num_outputs, semeta.num_models, outputid, modelid)].num_quantities = seint.output_num_quantities[outputid];"),
		     $("seresult->outputs[AS_IDX(seint.num_outputs, semeta.num_models, outputid, modelid)].num_samples = 0;"),
		     $("seresult->outputs[AS_IDX(seint.num_outputs, semeta.num_models, outputid, modelid)].data = (double*)se_alloc.malloc(START_SIZE*seint.output_num_quantities[outputid]*sizeof(double));")],
		 $("}")],
	     $("}"),
	     $(""),
	     $("// Run the model"),
	     $("seresult->status = exec_loop(t, t1, parameters, model_states, seresult->outputs);"),
	     $("seresult->status_message = simengine_errors[seresult->status];"),
	     $(""),
	     $("// Copy state values back to state initial value structure"),
	     $("for(modelid=0; modelid<semeta.num_models; modelid++){"),
	     SUB[$("seresult->final_time[modelid] = t[modelid];"),
		 $("for(stateid=0;stateid<seint.num_states;stateid++){"),
		 SUB[$("seresult->final_states[AS_IDX(seint.num_states, semeta.num_models, stateid, modelid)] = model_states[TARGET_IDX(seint.num_states, semeta.num_models, stateid, modelid)];")],
		 $("}")],
	     $("}"),
	     $("return seresult;")
	    ],
	 $("}")]
    end

fun buildC (model: DOF.model as (classes, inst, props)) =
    let
	val {name=inst_name, classname=class_name} = inst
	val inst_class = CurrentModel.classname2class class_name
	val class_name = Symbol.name (#name inst_class)

	val statespace = ClassProcess.class2statesize inst_class

	val {iterators,precision,...} = props
	val solver = props2solver props
	val solver_name = Solver.solver2name solver

	val c_data_format = case precision 
			     of DOF.SINGLE => "float" 
			      | DOF.DOUBLE => "double"

	val header_progs = (header (class_name, 
				    [], (* no additional includes *)
				    ("ITERSPACE", i2s (length iterators))::
				    (*				   ("STATESPACE", i2s statespace)::
				     ("INPUTSPACE", i2s (length (!(#inputs inst_class))))::
				     ("OUTPUTSPACE", i2s (length (!(#outputs inst_class)))):: *)
				    ("INTEGRATION_METHOD", solver_name)::
				    ("INTEGRATION_MEM", solver_name ^ "_mem")::
				    ("START_SIZE", "1000")::
				    ("BUFFER_LEN", "8000")::
				    ("MAX_ALLOC_SIZE", "65536000")::
				    ("MAX_ITERATIONS", "1000")::
				    (Solver.solver2params solver))) @
			   [$("#ifdef TARGET_GPU"),
			    $("#include \"solvers/"^solver_name^".cu\""),
			    $("#endif")]

	val simengine_interface_progs = simengine_interface (class_name, inst_class, solver_name)
(*	val input_progs = input_code inst_class*)
	val outputdatastruct_progs = outputdatastruct_code inst_class
	val outputstatestruct_progs = outputstatestruct_code classes
(*	val outputinit_progs = outputinit_code inst_class *)
(*	val init_progs = init_code classes *)
	val flow_progs = flow_code (classes, inst_class)
	val exec_progs = exec_code (inst_class, props, statespace)
	val main_progs = main_code inst_class
	val logoutput_progs = logoutput_code inst_class

	(* write the code *)
	val _ = output_code(class_name, ".", (header_progs @ 
					      simengine_interface_progs @
					      outputdatastruct_progs @
					      outputstatestruct_progs @
					      logoutput_progs @
(*					      outputinit_progs @
					      input_progs @
					      init_progs @ *)
					      flow_progs @ 
					      exec_progs @
					      main_progs))
    in
	SUCCESS
    end
    handle e => DynException.checkpoint "CParallelWriter.buildC" e

end
