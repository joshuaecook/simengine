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
     $("#include<simengine_api.h>"),
     $("#include<solvers.h>"),
     $("")] @
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
    [$("#if NUM_OUTPUTS > 0"),
     $("typedef struct {"),
     SUB(map output2struct (CWriterUtil.class2uniqueoutputsymbols class)),
     $("} output_data;"),
     $("#endif"),
     $("")]
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
	val class_inst_pairs_non_empty = 
	    List.filter
		(fn(classname,instname)=>
		   ClassProcess.class2statesize (CurrentModel.classname2class classname) > 0
		)
		class_inst_pairs
					 
    in
	[$(""),
	 $("// define state structures"),
	 $("struct statedata_" ^ (Symbol.name classname) ^ " {"),	 
	 SUB($("// states (count="^(i2s (List.length diff_eqs_symbols))^")") ::
	     (map ($ o state2member) diff_eqs_symbols) @
	     ($("// instances (count=" ^ (i2s (List.length class_inst_pairs)) ^")") ::
	      (map ($ o instance2member instances) class_inst_pairs_non_empty))),
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

	val has_states = ClassProcess.class2statesize class > 0

	val header_progs = 
	    if has_states then
		[$(""),
		 $("__HOST__ __DEVICE__ int flow_" ^ (Symbol.name (#name class)) 
		   ^ "(CDATAFORMAT t, const struct statedata_"^(Symbol.name orig_name)^" *y, struct statedata_"^(Symbol.name orig_name)^" *dydt, CDATAFORMAT *inputs, output_data *outputs, unsigned int first_iteration, unsigned int modelid) {")]
	    else
		[$(""),
		 $("__HOST__ __DEVICE__ int flow_" ^ (Symbol.name (#name class)) 
		   ^ "(CDATAFORMAT t, CDATAFORMAT *inputs, output_data *outputs, unsigned int first_iteration, unsigned int modelid) {")]


	val read_memory_progs = []

	val read_states_progs = []
	    
	val read_inputs_progs =
	    [$(""),
	     $("// mapping inputs to variables")] @ 
	    (map
		 (if top_class then
		     (fn({name,default},i)=> $("CDATAFORMAT " ^ (CWriterUtil.exp2c_str (Exp.TERM name)) ^ " = inputs[TARGET_IDX(NUM_INPUTS, NUM_MODELS, " ^ (i2s i) ^ ", modelid)];"))
		 else
		     (fn({name,default},i)=> $("CDATAFORMAT " ^ (CWriterUtil.exp2c_str (Exp.TERM name)) ^ " = inputs[" ^ (i2s i) ^ "];")))
		 (Util.addCount (!(#inputs class))))


	(* filter out all the unneeded expressions *)
	val (initvalue_exps, rest_exps) = List.partition ExpProcess.isInitialConditionEq (!(#exps class))
	val (valid_exps, rest_exps) = List.partition (fn(exp)=> ExpProcess.isIntermediateEq exp orelse
							        ExpProcess.isInstanceEq exp orelse
							        ExpProcess.isFirstOrderDifferentialEq exp) rest_exps
	val _ = if (List.length rest_exps > 0) then
		    (Logger.log_error($("Invalid expressions reached in code writer while writing class " ^ (Symbol.name (ClassProcess.class2orig_name class))));
		     app (fn(exp)=> Util.log ("  Offending expression: " ^ (ExpProcess.exp2str exp))) rest_exps;
		     DynException.setErrored())
		else
		    ()

	local
	    fun exp2prog exp =
		if (ExpProcess.isIntermediateEq exp) then
		    intermediateeq2prog exp
		else if (ExpProcess.isFirstOrderDifferentialEq exp) then
		    firstorderdiffeq2prog exp
		else if (ExpProcess.isInstanceEq exp) then
		    instanceeq2prog exp
		else
		    DynException.stdException(("Unexpected expression '"^(ExpProcess.exp2str exp)^"'"), "CParallelWriter.class2flow_code.equ_progs", Logger.INTERNAL)

	    and intermediateeq2prog exp =
 		[$("CDATAFORMAT " ^ (CWriterUtil.exp2c_str exp) ^ ";")]
	    and firstorderdiffeq2prog exp =
 		[$((CWriterUtil.exp2c_str exp) ^ ";")]
	    and instanceeq2prog exp =
		let
		    val {classname, instname, props, inpargs, outargs} = ExpProcess.deconstructInst exp
		    val orig_instname = case Fun.getRealInstName props of
					    SOME v => v
					  | NONE => instname

		    val class = CurrentModel.classname2class classname

		    val class_has_states = ClassProcess.class2statesize class > 0

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
						      if class_has_states then
							  $(calling_name ^ "(t, &y[STRUCT_IDX]."^(Symbol.name orig_instname)^", &dydt[STRUCT_IDX]."^(Symbol.name orig_instname)^", "^inpvar^", "^outvar^", first_iteration, modelid);")
						      else
							  $(calling_name ^ "(t, "^inpvar^", "^outvar^", first_iteration, modelid);")						  
						  else
						      if class_has_states then
							  $(calling_name ^ "(t, &y->"^(Symbol.name orig_instname)^", &dydt->"^(Symbol.name orig_instname)^", "^inpvar^", "^outvar^", first_iteration, modelid);")
						      else
							  $(calling_name ^ "(t, "^inpvar^", "^outvar^", first_iteration, modelid);")
						 ] @
			 map ($ o inst_output)
			     (Util.addCount (ListPair.zip (symbols, !(#outputs class)))))

		    ]
		end



	in
	val equ_progs = 
	    [$(""),
	     $("// writing all intermediate, instance, and differential equation expressions")] @
	    (Util.flatmap exp2prog valid_exps)
	end
	    
	val state_progs = []

	val output_progs = 
	    if top_class then
		[$(""),
		 $("// writing output variables"),
                 $("#if NUM_OUTPUTS > 0"),
		 $("if (first_iteration) {"),
		 SUB(map (fn(t,s)=> $("outputs[modelid]." ^ (Symbol.name s) ^ " = " ^ (CWriterUtil.exp2c_str (Exp.TERM t)) ^ ";"))
			 (CWriterUtil.class2uniqueoutputsymbols class)),
		 $("}"),
                 $("#endif")]
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
    handle e => DynException.checkpoint "CParallelWriter.class2flow_code" e

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
				       val class_has_states = ClassProcess.class2statesize class > 0
				   in
				       if isInline class then
					   $("CDATAFORMAT "^(Symbol.name (#name class))^"("^(String.concatWith ", " (map (fn{name,...}=> "CDATAFORMAT " ^ (CWriterUtil.exp2c_str (Exp.TERM name))) (!(#inputs class))))^");")
				       else
					   if class_has_states then
					       $("__HOST__ __DEVICE__ int flow_" ^ (Symbol.name (#name class)) ^ "(CDATAFORMAT t, const struct statedata_"^(Symbol.name orig_name)^" *y, struct statedata_"^(Symbol.name orig_name)^" *dydt, CDATAFORMAT *inputs, output_data *outputs, unsigned int first_iteration, unsigned int modelid);")
					   else
					       $("__HOST__ __DEVICE__ int flow_" ^ (Symbol.name (#name class)) ^ "(CDATAFORMAT t, CDATAFORMAT *inputs, output_data *outputs, unsigned int first_iteration, unsigned int modelid);")					       
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
             SUB[if ClassProcess.class2statesize topclass > 0 then
	       $("return flow_" ^ (Symbol.name (#name topclass)) ^ "(t, (const struct statedata_"^(Symbol.name (ClassProcess.class2orig_name topclass))^"*)y, (struct statedata_"^(Symbol.name (ClassProcess.class2orig_name topclass))^"*)dydt, inputs, (output_data*)outputs, first_iteration, modelid);")
             else
	       $("return flow_" ^ (Symbol.name (#name topclass)) ^ "(t, inputs, (output_data*)outputs, first_iteration, modelid);")
             ],
	     $("}"),
	     $("")]
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
	  handle e => DynException.stdException ("Unable to open file "^filename^" for writing", "CParallelWriter.output_code", Logger.INTERNAL)
    in
      Printer.printtexts (file, block, 0)
      before TextIO.closeOut file
    end
    handle e => DynException.checkpoint "CParallelWriter.output_code" e

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

fun logoutput_code class =
    let
	val orig_name = Symbol.name (ClassProcess.class2orig_name class)
	val dependent_symbols = CWriterUtil.class2uniqueoutputsymbols class
	val outputs = #outputs class
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

	fun buffer_output (exp, ix) =
	    $("obd->data["^(i2s (inc ix))^"] = " ^ (CWriterUtil.exp2c_str (ExpProcess.assignToOutputBuffer exp)) ^ ";")

	val output_exps =Util.flatmap
			      (fn(out as ({condition, contents, name}, output_index))=> 
				 [$("{ // Generating output for symbol " ^ (ExpProcess.exp2str (Exp.TERM name))),
				  SUB[$("int cond = " ^ (CWriterUtil.exp2c_str (ExpProcess.assignToOutputBuffer condition)) ^ ";"),
				      $("if (cond) {"),
				      SUB([$("ob_data *obd = (ob_data *)(ob->ptr[modelid]);"),
					   $("obd->outputid = " ^ (i2s output_index) ^ ";"),
					   $("obd->nquantities = " ^ (i2s (inc (List.length contents))) ^ ";"),
					   $("obd->data[0] = t;")] @
					  (map buffer_output (Util.addCount contents)) @
					  [$("ob->ptr[modelid] = &obd->data["^(i2s (inc (List.length contents)))^"];"),
					   $("ob->count[modelid]++;"),
					   $("ob->full[modelid] |= (MAX_OUTPUT_SIZE > ((unsigned long long)(ob->end[modelid]) - (unsigned long long)(ob->ptr[modelid])));")]),
				      $("}")],
				  $("}")]
			      )
			      (Util.addCount(!outputs))

	val total_output_quantities =
	    List.foldr op+ 0 (map (List.length o #contents) (!outputs))

    in
        if total_output_quantities > 0 then
	[$(""),
	 $("#define MAX_OUTPUT_SIZE (NUM_OUTPUTS*2*sizeof(unsigned int) + (NUM_OUTPUTS+" ^ (i2s total_output_quantities)  ^ ")*sizeof(CDATAFORMAT)) //size in bytes"),
	 $(""),
	 $("__DEVICE__ void buffer_outputs(double t, output_data *od, output_buffer *ob, unsigned int modelid) {"),
	 SUB(output_exps),
	 $("}"),
	 $(""),
	 $("#include<simengine.c>")]
         else
	 [$("#include<simengine.c>")]
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
				    ("INTEGRATION_METHOD", solver_name)::
				    ("INTEGRATION_MEM", solver_name ^ "_mem")::
				    (Solver.solver2params solver))) @
			   [$("#ifdef TARGET_GPU"),
			    $("#include \"solvers/"^solver_name^".cu\""),
			    $("#endif")]

	val simengine_interface_progs = simengine_interface (class_name, inst_class, solver_name)
	val outputdatastruct_progs = outputdatastruct_code inst_class
	val outputstatestruct_progs = outputstatestruct_code classes
	val flow_progs = flow_code (classes, inst_class)
	val logoutput_progs = logoutput_code inst_class

	(* write the code *)
	val _ = output_code(class_name, ".", (header_progs @ 
					      simengine_interface_progs @
					      outputdatastruct_progs @
					      outputstatestruct_progs @
					      logoutput_progs @
					      flow_progs))
    in
	SUCCESS
    end
    handle e => DynException.checkpoint "CParallelWriter.buildC" e

end
