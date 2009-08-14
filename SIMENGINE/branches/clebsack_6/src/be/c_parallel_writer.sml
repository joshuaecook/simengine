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
	 $(""),
	 $("const simengine_metadata semeta = {"),
	 SUB[$("0x0000000000000000ULL, // hashcode"),
	     $("NUM_MODELS,"),
	     $("solver,"),
	     $("sizeof(CDATAFORMAT)")
	    ],
	 $("};"),
	 $(""),
	 $("const simengine_interface seint = {"),
	 SUB[$("0, // Version,"),
	     $("INPUTSPACE, // Number of inputs"),
	     $("STATESPACE, // Number of states"),
	     $("OUTPUTSPACE, // Number of outputs"),
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
	 $("simengine_alloc se_alloc;"),
	 $("")]
    end

local
    fun output2struct (out as {name, contents, condition}) = 

	(map ((fn (sym) =>
		  $("CDATAFORMAT " ^ (Symbol.name sym) ^ ";"))
	      o Term.sym2curname)
	     (Util.flatmap ExpProcess.exp2termsymbols contents))



in
fun outputdatastruct_code {outputs, ...} =
    [$("typedef struct {"),
     SUB(List.concat (map output2struct (!outputs))),
     $("} output_data;")]
end

fun initbyclass_code (class as {exps, ...}) =
    let
	val classname = ClassProcess.class2orig_name class
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
			     $("CDATAFORMAT " ^ name ^ "[NUM_MODELS];")
			 else
			     $("CDATAFORMAT " ^ name ^ "["^(i2s size)^"*NUM_MODELS];")
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
	    if size = 1 then ("CDATAFORMAT " ^ name ^ "[NUM_MODELS];")
	    else ("CDATAFORMAT " ^ name ^ "["^(i2s size)^" * NUM_MODLS];")
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
	val classname = ClassProcess.class2orig_name class
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
		(fn(class)=> $("struct " ^ (Symbol.name (ClassProcess.class2orig_name class)) ^ ";"))
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
	     $("__DEVICE__ int flow_" ^ (Symbol.name (#name class)) 
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
		 (fn({name,default},i)=> $("CDATAFORMAT " ^ (CWriterUtil.exp2c_str (Exp.TERM name)) ^ " = inputs[" ^ (i2s i) ^ "];"))
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

				    val inps = "CDATAFORMAT " ^ inpvar ^ "[] = {" ^ (String.concatWith ", " (map CWriterUtil.exp2c_str inpargs)) ^ "};"
				    val outs_decl = "CDATAFORMAT " ^ outvar ^ "["^(i2s (List.length outargs))^"];"

				    val symbols = map Term.sym2curname outargs

				    fun inst_output ((sym, {name, contents, condition}), idx) =
					"CDATAFORMAT " ^ (Symbol.name sym) ^ " = " ^ outvar ^ "[" ^ (i2s idx) ^ "];" ^
					" // Mapped to "^ (Symbol.name classname) ^ ": " ^ (ExpProcess.exp2str (List.hd (contents)))

				in
				    [SUB([$("// Calling instance class " ^ (Symbol.name classname)),
					  $("// " ^ (CWriterUtil.exp2c_str exp)),
					  $(inps), $(outs_decl),
					  $(calling_name ^ "(t, &y[STRUCT_IDX]."^(Symbol.name orig_instname)^", &dydt[STRUCT_IDX]."^(Symbol.name orig_instname)^", "^inpvar^", "^outvar^", first_iteration, 0);")] @
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
		 $("output_data od;"),
		 SUB(map
			 (fn(t,s)=> $("od." ^ (Symbol.name s) ^ " = " ^ (CWriterUtil.exp2c_str (Exp.TERM t)) ^ ";"))
			 (CWriterUtil.class2uniqueoutputsymbols class)),
		 $("buffer_outputs(t, &od, modelid);"),
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
					   $("__DEVICE__ int flow_" ^ (Symbol.name (#name class)) ^ "(CDATAFORMAT t, const struct statedata_"^(Symbol.name orig_name)^" *y, struct statedata_"^(Symbol.name orig_name)^" *dydt, CDATAFORMAT *inputs, CDATAFORMAT *outputs, unsigned int first_iteration, unsigned int modelid);")
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

	val top_level_flow_progs = [$"",
				    $("__DEVICE__ int model_flows(CDATAFORMAT t, const CDATAFORMAT *y, CDATAFORMAT *dydt, CDATAFORMAT *inputs, CDATAFORMAT *outputs, unsigned int first_iteration, unsigned int modelid){"),
				    SUB[$("return flow_" ^ (Symbol.name (#name topclass)) ^ "(t, (const struct statedata_"^(Symbol.name (ClassProcess.class2orig_name topclass))^"*)y, (struct statedata_"^(Symbol.name (ClassProcess.class2orig_name topclass))^"*)dydt, inputs, outputs, first_iteration, modelid);")],
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
	 $("int exec_loop(CDATAFORMAT *t, CDATAFORMAT t1, unsigned int num_models, CDATAFORMAT *inputs, CDATAFORMAT *model_states, simengine_output *outputs) {"),
	 SUB[$("unsigned int modelid = 0;"),
	     $("solver_props props;"),
	     $("props.timestep = DT;"),
	     $("props.abstol = ABSTOL;"),
	     $("props.reltol = RELTOL;"),
	     $("props.starttime = *t;"),
	     $("props.stoptime = t1;"),
	     $("props.time = t;"),
	     $("props.model_states = model_states;"),
	     $("props.inputs = inputs;"),
	     $("props.outputs = NULL;"),
	     $("props.first_iteration = TRUE;"),
	     $("props.statesize = seint.num_states;"),
	     $("props.inputsize = seint.num_inputs;"),
	     $("props.num_models = num_models;"),
	     $(""),
	     $("INTEGRATION_MEM *mem = SOLVER(INTEGRATION_METHOD, init, TARGET, SIMENGINE_STORAGE, &props);"),
	     $("for(modelid=0; modelid<num_models; modelid++){"),
	     SUB[$("init_output_buffer(modelid);")],
	     $("}"),
	     $("for(modelid=0; modelid<num_models; modelid++){"),
	     SUB[$("while (t[modelid] < t1) {"),
		 SUB[$("int status = SOLVER(INTEGRATION_METHOD, eval, TARGET, SIMENGINE_STORAGE, mem, modelid);"),
		     $("if (status != 0) {"),
		     SUB[$("return ERRCOMP;")],
		     $("}"),
		     $("if (OB.full){"),
		     SUB[$("if(0 != log_outputs(ob, outputs, modelid))"),
			 SUB[$("{ return ERRMEM; }")],
			 $("init_output_buffer(modelid);")
			],
		     $("}")],
		 $("}")],
	     $("if(0 != log_outputs(ob, outputs, modelid))"),
	     SUB[$("{ return ERRMEM; }")],
	     $("}"),
	     $("SOLVER(INTEGRATION_METHOD, free, TARGET, SIMENGINE_STORAGE, mem);")],
	 $("return SUCCESS;"),
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
				  SUB[$("int cond = " ^ (CWriterUtil.exp2c_str condition) ^ ";"),
				      $("if (cond) {"),
				      SUB([$("((unsigned int*)ob->ptr[modelid])[0] = " ^ (i2s output_index) ^ ";"),
					   $("((unsigned int*)ob->ptr[modelid])[1] = " ^ (i2s (inc (List.length contents))) ^ ";"),
					   $("ob->ptr[modelid] += 2*sizeof(unsigned int);"),
					   $("*((CDATAFORMAT*)ob->ptr[modelid]) = t;"),
					   $("ob->ptr[modelid] += sizeof(CDATAFORMAT);")] @
					  (Util.flatmap ((fn (sym) =>
							     [$("*((CDATAFORMAT*)ob->ptr[modelid]) = od->"^(Symbol.name sym)^";"),
							      $("ob->ptr[modelid] += sizeof(CDATAFORMAT);")])
		  					 o Term.sym2curname)
							(Util.flatmap ExpProcess.exp2termsymbols contents)) @
					  [$("ob->count[modelid]++;"),
					   $("ob->full = MAX_OUTPUT_SIZE > (ob->end[modelid] - ob->ptr[modelid]);")]),
				      $("}")],
				  $("}")]
			      )
			      (Util.addCount(!(#outputs class)))
    in
	[$(""),
	 $("#define MAX_OUTPUT_SIZE (2*sizeof(int) + " ^ (i2s (inc (List.foldl Int.max 0 (map (List.length o #contents) (!(#outputs class))))))  ^ "*sizeof(CDATAFORMAT)) //size in bytes"),
	 $(""),
	 $("/* An internal data structure that maintains a buffer of output data."),
	 $("*"),
	 $("* The 'count' array tracks the number of data produced for each model."),
	 $("*"),
	 $("* The 'buffer' array comprises a list of tagged output data for each"),
	 $("* model having the format:"),
	 $("*     {tag, count, quantities[count]}"),
	 $("* where 'tag' is an integer identifying a model output, 'count' is a"),
	 $("* counter specifying the number of data quantities, and 'quantities'"),
	 $("* is an array of actual data points."),
	 $("*"),
	 $("* The 'ptr' and 'end' pointers are references to positions within 'buffer.'"),
	 $("*/"),
	 $("typedef struct{"),
	 SUB[$("unsigned int full;"),
	     $("unsigned int count[NUM_MODELS];"),
	     $("CDATAFORMAT buffer[BUFFER_LEN*NUM_MODELS];"),
	     $("void *ptr[NUM_MODELS];"),
	     $("void *end[NUM_MODELS];")],
	 $("} output_buffer;"),
	 $(""),
	 $("output_buffer OB;"),
	 $("output_buffer *ob = &OB;"),
	 $(""),
	 $("void init_output_buffer(unsigned int modelid){"),
	 SUB[$("if(0 == modelid) ob->full = 0;"),
	     $("ob->count[modelid] = 0;"),
	     $("ob->ptr[modelid] = &ob->buffer[modelid*BUFFER_LEN];"),
	     $("ob->end[modelid] = &ob->buffer[(modelid+1)*BUFFER_LEN];")],
	 $("}"),
	 $(""),
	 $("void buffer_outputs(double t, output_data *od, unsigned int modelid) {"),
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
	 SUB[$("unsigned int tag, nquantities, dataid, quantityid;"),
	     $("simengine_output *output;"),
	     $("double *odata;"),
	     $(""),
	     $("unsigned int ndata = ob->count[modelid];"),
	     $("void *data = &(ob->buffer[modelid]);"),
	     $(""),
	     $("for (dataid = 0; dataid < ndata; ++dataid) {"),
	     SUB[$("tag = ((unsigned int *)data)[0];"),
		 $("nquantities = ((unsigned int *)data)[1];"),
		 $("data += 2 * sizeof(unsigned int);"),
		 $(""),
		 $("output = &outputs[modelid * OUTPUTSPACE + tag];"),
		 $(""),
		 $("if (output->num_samples == output->alloc) {"),
		 SUB[$("output->alloc *= 2;"),
		     $("if (!(output->data = se_alloc.realloc(output->data, outputs->alloc * sizeof(CDATAFORMAT))))"),
		     SUB[$("{ return 1; }")]],
		 $("}"),
		 $(""),
		 $("odata = &output->data[nquantities * output->num_samples];"),
		 $(""),
		 $("for (quantityid = 0; quantityid < nquantities; ++quantityid) {"),
		 SUB[$("odata[quantityid] = *((double *)data);"),
		     $("data += sizeof(CDATAFORMAT);")],
		 $("}"),
		 $(""),
		 $("++output->num_samples;")],
	     $("}"),
	     $(""),
	     $("return 0;")],
	 $("}"),
	 $(""),
	 $("const simengine_interface *simengine_getinterface(){"),
	 SUB[$("return &seint;")],
	 $("}"),
	 $(""),
	 $("simengine_result *simengine_runmodel(double start_time, double stop_time, unsigned int num_models, double *inputs, double *states, simengine_alloc *alloc){"),
	 SUB[$("CDATAFORMAT model_states[semeta.num_models*seint.num_states];"),
	     $("CDATAFORMAT parameters[semeta.num_models*seint.num_inputs];"),
	     $("CDATAFORMAT t[semeta.num_models];"),
	     $("CDATAFORMAT t1 = stop_time;"),
	     $("unsigned int stateid;"),
	     $("unsigned int modelid;"),
	     $("unsigned int inputid;"),
	     $("unsigned int i;"),
	     $(""),
	     $("// Set up allocation functions"),
	     $("if(alloc){"),
	     SUB[$("se_alloc.malloc = alloc->malloc;"),
		 $("se_alloc.realloc = alloc->realloc;"),
		 $("se_alloc.free = alloc->free;")],
	     $("}"),
	     $("else{"),
	     SUB[$("se_alloc.malloc = malloc;"),
		 $("se_alloc.realloc = realloc;"),
		 $("se_alloc.free = free;")],
	     $(" }"),
	     $(""),
	     $("// Create result structure"),
	     $("simengine_result *seresult = (se_alloc.malloc)(sizeof(simengine_result));"),
	     $(""),
	     $("// Couldn't allocate return structure, return NULL"),
	     $("if(!seresult) return NULL;"),
	     $(""),
	     $("// Check that the number of models matches"),
	     $("if(num_models != semeta.num_models){"),
	     SUB[$("seresult->status = ERRNUMMDL;"),
		 $("seresult->status_message = simengine_errors[ERRNUMMDL];"),
		 $("seresult->outputs = NULL;"),
		 $("return seresult;")],
	     $("}"),
	     $(""),
	     $("// Allocate return structures"),
	     $("seresult->outputs = (se_alloc.malloc)(semeta.num_models * seint.num_outputs * sizeof(simengine_output));"),
	     $("if(!seresult->outputs){"),
	     SUB[$("seresult->status = ERRMEM;"),
		 $("seresult->status_message = simengine_errors[ERRMEM];"),
		 $("seresult->outputs = NULL;"),
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
	     $("for(i = 0; i<semeta.num_models*seint.num_outputs; i++){"),
	     SUB[$("seresult->outputs[i].alloc = START_SIZE;"),
		 $("seresult->outputs[i].num_quantities = seint.output_num_quantities[i];"),
		 $("seresult->outputs[i].num_samples = 0;"),
		 $("seresult->outputs[i].data = (se_alloc.malloc)(START_SIZE*seint.output_num_quantities[i]*sizeof(CDATAFORMAT));")],
	     $("}"),
	     $(""),
	     $("// Run the model"),
	     $("seresult->status = exec_loop(t, t1, semeta.num_models, parameters, model_states, seresult->outputs);"),
	     $("seresult->status_message = simengine_errors[seresult->status];"),
	     $(""),
	     $("// Copy state values back to state initial value structure"),
	     $("for(modelid=0; modelid<semeta.num_models; modelid++){"),
	     SUB[
		 $("for(stateid=0;stateid<seint.num_states;stateid++){"),
		 SUB[$("states[AS_IDX(seint.num_states, semeta.num_models, stateid, modelid)] = model_states[TARGET_IDX(seint.num_states, semeta.num_models, stateid, modelid)];")],
		 $("}")],
	     $("}"),
	     $("return seresult;")
	    ],
	 $("}")
	]
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

	val header_progs = header (class_name, 
				   [], (* no additional includes *)
				   ("ITERSPACE", i2s (length iterators))::
				   ("STATESPACE", i2s statespace)::
				   ("INPUTSPACE", i2s (length (!(#inputs inst_class))))::
				   ("OUTPUTSPACE", i2s (length (!(#outputs inst_class))))::
				   ("INTEGRATION_METHOD", solver_name)::
				   ("INTEGRATION_MEM", solver_name ^ "_mem")::
				   ("START_SIZE", "1000")::
				   ("BUFFER_LEN", "8000")::
				   ("MAX_ALLOC_SIZE", "65536000")::
				   (Solver.solver2params solver))

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
