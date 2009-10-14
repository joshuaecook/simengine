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

(* ====================  HEADER  ==================== *)

fun header (class_name, includes, defpairs) = 
    [$("// C Execution Engine for top-level model: " ^ class_name),
     $("// " ^ Globals.copyright),
     $("")] @
    (map (fn(inc)=> $("#include "^inc)) includes) @
    [$(""),
     $("")] @
    (map (fn(name,value)=> $("#define " ^ name ^ " " ^ value)) defpairs)

fun simengine_interface (class_name, class, solver_name) =
    let
	val time = Symbol.symbol "t"
	val iter_syms = map (fn(sym, _)=> sym) (CurrentModel.iterators())

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
		    handle e => DynException.checkpoint ("CParallelWriter.simengine_interface.findStatesInitValues.exp2name ["^(e2s exp)^"]") e
				      
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
	 $("const char *iterator_names[] = {" ^ (String.concatWith ", " (map (cstring o Symbol.name) iter_syms)) ^ "};"),
	 $("const double default_inputs[] = {" ^ (String.concatWith ", " default_inputs) ^ "};"),
	 $("const double default_states[] = {" ^ (String.concatWith ", " state_defaults) ^ "};"),
	 $("const unsigned int output_num_quantities[] = {" ^ (String.concatWith ", " output_num_quantities) ^ "};"),
	 $("const char model_name[] = \"" ^ class_name ^ "\";"),
	 $("const char solver[] = \"" ^ solver_name ^ "\";"),
	 $("#if defined TARGET_CPU"),  (* These #if statements should be converted to sml conditionals based on compiler options *)
	 $("const char target[] = \"cpu\";"),
	 $("#elif defined TARGET_OPENMP"),
	 $("const char target[] = \"openmp\";"),
	 $("#elif defined TARGET_GPU"),
	 $("const char target[] = \"gpu\";"),
	 $("#endif"),
	 $(""),
	 $(""),
	 $("#define NUM_INPUTS "^(i2s (List.length input_names))),
	 $("#define NUM_STATES "^(i2s (List.length state_names))),
	 $("#define NUM_OUTPUTS "^(i2s (List.length output_names))),
	 $("#define HASHCODE (0x0000000000000000ULL)"),
	 $("#define VERSION (0)"),
	 $("")]
    end

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

fun iteratordatastruct_code iters =
    let
	val passed_iters = List.mapPartial (fn(iter_sym, iter_type)=>
					      case iter_type of
						  DOF.CONTINUOUS _ => SOME (iter_sym, "CDATAFORMAT")
						| DOF.DISCRETE _ => SOME (iter_sym, "CDATAFORMAT") (* TODO: change this to int if desired... *)
						| _ => NONE) iters
    in
	[$(""),
	 $("// Iterator data structure"),
	 $("struct iteratordata {"),
	 SUB(map (fn(iter_sym,format)=> $(format ^ " " ^ (Symbol.name iter_sym) ^ ";")) passed_iters),
	 $("};")]
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
	    "struct statedata_" ^ (Symbol.name classname)  ^ " " ^ (Symbol.name instname) ^ index
	end
in
fun outputstatestructbyclass_code (class : DOF.class as {exps, ...}) =
    let
	val classname = (*ClassProcess.class2orig_name class*) ClassProcess.class2classname class
	val class_iterators = #iterators class
	val state_eqs_symbols = map ExpProcess.lhs (List.filter ExpProcess.isStateEq (!exps))
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
	 $("// Define state structures"),
	 $("struct statedata_" ^ (Symbol.name classname) ^ " {"),	 
	 SUB($("// states (count="^(i2s (List.length state_eqs_symbols))^")") ::
	     (map ($ o (state2member class_iterators)) state_eqs_symbols) @
	     ($("// instances (count=" ^ (i2s (List.length class_inst_pairs_non_empty)) ^")") ::
	      (map ($ o (instance2member instances)) class_inst_pairs_non_empty))),
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
	(*Util.flatmap (fn(iter as (name, itertype))=>
			(($(""))::
			 ($("// Pre-declare state structures (iterator '"^(Symbol.name name)^"')"))::
			 (map (fn(str)=> $(str ^ "_" ^ (Symbol.name name) ^ ";")) predeclare_statements)) @
			List.concat (map (outputstatestructbyclass_code iter) master_classes)) iterators*)
	
	($("")::
	 ($("// Pre-declare state structures"))::predeclare_statements) @
	List.concat (map outputstatestructbyclass_code master_classes)
    end

fun outputsystemstatestruct_code forkedModels =
    [$(""),
     $("// System State Structure"),
     $("struct systemstatedata {"),
     SUB(map (fn{model=(_,{classname,...},_),iter=(iter_sym,_),...} => $("struct statedata_" ^ (Symbol.name classname) ^ " " ^ (Symbol.name iter_sym) ^ ";")) forkedModels),
     $("};"),
     $("")]

fun class2flow_code (class, top_class, iter as (iter_sym, iter_type)) =
    let
	val orig_name = ClassProcess.class2orig_name class

	val has_states = case iter_type of 
			     DOF.UPDATE _ => true
			   | _ => ClassProcess.class2statesize class > 0

	val eval_iterators = List.filter (fn(iter_sym, iter_type)=> case iter_type of
									DOF.UPDATE v => false
								      | DOF.POSTPROCESS v => false
								      | _ => true) (CurrentModel.iterators())

	val (readstates, writestates) = class2stateiterators class
	val iterators = CurrentModel.iterators()
	(*val iteratorprototypes = String.concat (map (fn(sym,_)=> "CDATAFORMAT " ^ (Symbol.name sym) ^ ", ") eval_iterators)*)
	val iteratorprototypes = "struct iteratordata *iter, "
	(* every iterator except the update iterator uses an iter_name *)
	val iter_name = Symbol.name (case iter_type of
					 DOF.UPDATE v => v
				       | _ => iter_sym)
			
	val statereadprototype = "const struct statedata_" ^ (Symbol.name orig_name) ^ "_" ^ iter_name ^ " *rd_" ^ iter_name
	val statewriteprototype = "struct statedata_" ^ (Symbol.name orig_name) ^ "_" ^ iter_name ^ " *wr_" ^ iter_name
	val systemstatereadprototype = "const struct systemstatedata *sys_rd"
				 
(*	val stateprototypes = 
	    (String.concat (map
				(fn(sym)=> "const struct statedata_" ^ (Symbol.name orig_name) ^ "_" ^ (Symbol.name sym) ^ " *rd_" ^ (Symbol.name sym) ^ ", ")
				readstates)) ^
	    (String.concat (map
				(fn(sym)=> "struct statedata_" ^ (Symbol.name orig_name) ^ "_" ^ (Symbol.name sym) ^ " *wr_" ^ (Symbol.name sym) ^ ", ")
				writestates))*)

	val header_progs = 
	    (*[$(""),
	       $("__HOST__ __DEVICE__ int flow_" ^ (Symbol.name (#name class)) 
	       ^ "("^iteratorprototypes^stateprototypes^"CDATAFORMAT *inputs, CDATAFORMAT *outputs, unsigned int first_iteration, unsigned int modelid) {")]*)
	    
	    if has_states then
		[$(""),
		 $("__HOST__ __DEVICE__ int flow_" ^ (Symbol.name (#name class)) 
		   ^ "("^iteratorprototypes^statereadprototype^", "^statewriteprototype^", "^systemstatereadprototype^", CDATAFORMAT *inputs, CDATAFORMAT *outputs, unsigned int first_iteration, unsigned int modelid) {")]
	    else
		[$(""),
		 $("__HOST__ __DEVICE__ int flow_" ^ (Symbol.name (#name class)) 
		   ^ "("^iteratorprototypes^"CDATAFORMAT *inputs, CDATAFORMAT *outputs, unsigned int first_iteration, unsigned int modelid) {")]	    

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
							        ExpProcess.isStateEq exp) rest_exps
	val _ = if (List.length rest_exps > 0) then
		    (Logger.log_error($("Invalid expressions reached in code writer while writing class " ^ (Symbol.name (ClassProcess.class2orig_name class))));
		     app (fn(exp)=> Util.log ("  Offending expression: " ^ (e2s exp))) rest_exps;
		     DynException.setErrored())
		else
		    ()

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
 		[$("CDATAFORMAT " ^ (CWriterUtil.exp2c_str exp) ^ ";")]
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

		    val class = CurrentModel.classname2class classname
		    val iterators = map (fn(sym, _)=>sym) (CurrentModel.iterators())
		    val statereads_top = map
					 (fn(sym)=> "&rd_" ^ (Symbol.name sym) ^ "[STRUCT_IDX]." ^ (Symbol.name orig_instname) ^ ", ")
					 iterators
					 
		    val statewrites_top = map
					  (fn(sym)=> "&wr_" ^ (Symbol.name sym) ^ "[STRUCT_IDX]." ^ (Symbol.name orig_instname) ^ ", ")
					  iterators

		    val statereads = map
					 (fn(sym)=> "&rd_" ^ (Symbol.name sym) ^ "->" ^ (Symbol.name orig_instname) ^ ", ")
					 iterators
					 
		    val statewrites = map
					  (fn(sym)=> "&wr_" ^ (Symbol.name sym) ^ "->" ^ (Symbol.name orig_instname) ^ ", ")
					  iterators

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
			" // Mapped to "^ (Symbol.name classname) ^ ": " ^ (e2s (List.hd (contents)))

		in
		    [SUB([$("// Calling instance class " ^ (Symbol.name classname)),
			  $("// " ^ (CWriterUtil.exp2c_str exp)),
			  $(inps)] @ inps_init @ [$(outs_decl),
						  if top_class then
						      $(calling_name ^ "("^(String.concat (map (fn(sym)=>Symbol.name sym ^ ", ") iterators))^(String.concat statereads_top) ^ (String.concat statewrites_top) ^ inpvar^", "^outvar^", first_iteration, modelid);")
						      (*
						      if class_has_states then
							  $(calling_name ^ "(t, &y[STRUCT_IDX]."^(Symbol.name orig_instname)^", &dydt[STRUCT_IDX]."^(Symbol.name orig_instname)^", "^inpvar^", "^outvar^", first_iteration, modelid);")
						      else
							  $(calling_name ^ "(t, "^inpvar^", "^outvar^", first_iteration, modelid);")						  
						       *)
						  else
						      $(calling_name ^ "("^(String.concat (map (fn(sym)=>Symbol.name sym ^ ", ") iterators))^(String.concat statereads) ^ (String.concat statewrites) ^ inpvar^", "^outvar^", first_iteration, modelid);")
						      (*
						      if class_has_states then
							  $(calling_name ^ "(t, &y->"^(Symbol.name orig_instname)^", &dydt->"^(Symbol.name orig_instname)^", "^inpvar^", "^outvar^", first_iteration, modelid);")
						      else
							  $(calling_name ^ "(t, "^inpvar^", "^outvar^", first_iteration, modelid);")
						       *)
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
		 SUB($("output_data *od = (output_data*)outputs;")::
		     (map
			  (fn(t,s)=> $("od[modelid]." ^ (Symbol.name s) ^ " = " ^ (CWriterUtil.exp2c_str (Exp.TERM t)) ^ ";"))
			  (CWriterUtil.class2uniqueoutputsymbols class))),
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

fun flow_wrapper (class, (iter_sym, _)) =
    let
	val orig_name = Symbol.name (ClassProcess.class2orig_name class)
	val iter_name = Symbol.name iter_sym

	val (readstates, writestates) = class2stateiterators class
	val iterators = CurrentModel.iterators()
	(*val iteratorprototypes = String.concat (map (fn(sym,_)=> "CDATAFORMAT " ^ (Symbol.name sym) ^ ", ") iterators)*)
	val iteratorprototypes = "struct iteratordata *iter, "
				 
	val stateprototypes = 
	    (String.concat (map
				(fn(sym)=> "const struct statedata_" ^ (orig_name) ^ "_" ^ (Symbol.name sym) ^ " *rd_" ^ (Symbol.name sym) ^ ", ")
				readstates)) ^
	    (String.concat (map
				(fn(sym)=> "struct statedata_" ^ (orig_name) ^ "_" ^ (Symbol.name sym) ^ " *wr_" ^ (Symbol.name sym) ^ ", ")
				writestates))
	val fun_prototype = "__HOST__ __DEVICE__ int flow_"^iter_name^"(CDATAFORMAT "^iter_name^", const CDATAFORMAT *rd_"^iter_name^", CDATAFORMAT *wr_"^iter_name^", CDATAFORMAT *inputs, CDATAFORMAT *outputs, unsigned int first_iteration, unsigned int modelid)"

    in
	($(fun_prototype ^ ";"),
  	 [(*$(""),
	  $("// wrapper for flow function over iterator '"^iter_name^"'"),
	  $(fun_prototype ^ " {"),
	  SUB[$("return flow_"^orig_name^"("^(String.concat (map (fn(sym,_)=> Symbol.name sym ^ ", ") iterators))^(String.concat (map (fn(sym)=>  "(const struct statedata_"^orig_name^"_"^(Symbol.name sym)^" * ) rd_" ^ (Symbol.name sym) ^ ", ") readstates))^(String.concat (map (fn(sym)=> "(struct statedata_"^orig_name^"_"^(Symbol.name sym)^"* ) wr_" ^ (Symbol.name sym) ^ ", ") writestates))^"inputs, outputs, first_iteration, modelid);")],
	  $("}")*)])
    end



fun flow_code (*(classes: DOF.class list, topclass: DOF.class)*){model as (classes,_,_), iter as (iter_sym, iter_type), top_class} = 
    let
	val prevModel = CurrentModel.getCurrentModel()
	val _ = CurrentModel.setCurrentModel(model)

	val topclass = CurrentModel.classname2class top_class

	val iter_name = Symbol.name (case iter_type of
					 DOF.UPDATE v => v
				       | DOF.POSTPROCESS v => v
				       | _ => iter_sym)

	fun isInline (class: DOF.class) =
	    let
		val {properties={classform,...},...} = class
	    in
		case classform of 
		    DOF.FUNCTIONAL => true
		  | _ => false
	    end

	val eval_iterators = List.filter (fn(iter_sym, iter_type)=> case iter_type of
									DOF.UPDATE v => false
								      | DOF.POSTPROCESS v => false
								      | _ => true) (CurrentModel.iterators())
	val fundecl_progs = map
				(fn(class) => 
				   let
				       val orig_name = ClassProcess.class2orig_name class
				       val class_has_states = case iter_type of
								  DOF.UPDATE _ => true
								| _ => ClassProcess.class2statesize class > 0
				   in
				       if isInline class then
					   $("CDATAFORMAT "^(Symbol.name (#name class))^"("^(String.concatWith ", " (map (fn{name,...}=> "CDATAFORMAT " ^ (CWriterUtil.exp2c_str (Exp.TERM name))) (!(#inputs class))))^");")
				       else
					   let
					       (*val (readstates, writestates) = class2stateiterators class*)

					       (*val iteratorprototypes = String.concat (map (fn(sym,_)=> "CDATAFORMAT " ^ (Symbol.name sym) ^ ", ") eval_iterators)*)
					       val iteratorprototypes = "struct iteratordata *iter, "
									
					       (* every iterator except the update iterator uses an iter_name *)
					       val iter_name = Symbol.name (case iter_type of
										DOF.UPDATE v => v
									      | _ => iter_sym)

					       val statereadprototype = "const struct statedata_" ^ (Symbol.name orig_name) ^ "_" ^ iter_name ^ " *rd_" ^ iter_name
					       val statewriteprototype = "struct statedata_" ^ (Symbol.name orig_name) ^ "_" ^ iter_name ^ " *wr_" ^ iter_name
					       val systemstatereadprototype = "const struct systemstatedata *sys_rd"

					       (*val stateprototypes = 
						   (String.concat (map
								       (fn(sym)=> "const struct statedata_" ^ (Symbol.name orig_name) ^ "_" ^ (Symbol.name sym) ^ " *rd_" ^ (Symbol.name sym) ^ ", ")
								       readstates)) ^
						   (String.concat (map
								       (fn(sym)=> "struct statedata_" ^ (Symbol.name orig_name) ^ "_" ^ (Symbol.name sym) ^ " *wr_" ^ (Symbol.name sym) ^ ", ")
								       writestates))*)

					   in
					       (*$("__HOST__ __DEVICE__ int flow_" ^ (Symbol.name (#name class)) ^ "("^iteratorprototypes^stateprototypes^"CDATAFORMAT *inputs, CDATAFORMAT *outputs, unsigned int first_iteration, unsigned int modelid);")*)
					       
					       if class_has_states then
						   $("__HOST__ __DEVICE__ int flow_" ^ (Symbol.name (#name class)) ^ "("^iteratorprototypes^statereadprototype^", "^statewriteprototype^", "^systemstatereadprototype^", CDATAFORMAT *inputs, CDATAFORMAT *outputs, unsigned int first_iteration, unsigned int modelid);")
					       else
						   $("__HOST__ __DEVICE__ int flow_" ^ (Symbol.name (#name class)) ^ "("^iteratorprototypes^"CDATAFORMAT *inputs, CDATAFORMAT *outputs, unsigned int first_iteration, unsigned int modelid);")
					   end
				   end)
				classes
				
	val flow_progs = List.concat (map (fn(c)=>
					     if isInline c then
						 (Logger.log_error ($("Functional classes like '"^(Symbol.name (#name c))^"' are not supported"));
						  DynException.setErrored();
						  [])
					     else
						 class2flow_code (c,#name c = #name topclass, iter)) classes)

	val top_level_flow_progs = 
	    [$"",
	     $("__HOST__ __DEVICE__ int model_flows(C DATAFORMAT t, const CDATAFORMAT *y, CDATAFORMAT *dydt, CDATAFORMAT *inputs, CDATAFORMAT *outputs, unsigned int first_iteration, unsigned int modelid){"),
             SUB[if ClassProcess.class2statesize topclass > 0 then
	       $("return flow_" ^ (Symbol.name (#name topclass)) ^ "(t, (const struct statedata_"^(Symbol.name (ClassProcess.class2orig_name topclass))^"* )y, (struct statedata_"^(Symbol.name (ClassProcess.class2orig_name topclass))^"* )dydt, inputs, outputs, first_iteration, modelid);")
             else
	       $("return flow_" ^ (Symbol.name (#name topclass)) ^ "(t, inputs, outputs, first_iteration, modelid);")
             ],
	     $("}"),
	     $("")]

	(*val (fun_prototypes, fun_wrappers) = ListPair.unzip (map (fn(iter)=>flow_wrapper (topclass, iter)) eval_iterators)*)
	val (fun_prototype, fun_wrapper) = flow_wrapper (topclass, iter)

    in
	([$("// Functions prototypes for flow code wrappers"),
	  fun_prototype],
	 $("// Flow code function declarations")::
	 fundecl_progs @
	 flow_progs @
	 fun_wrapper) before (CurrentModel.setCurrentModel(prevModel))

    (*(Util.flatmap (fn(iter)=>flow_wrapper (topclass, iter)) iterators)*)
	(*top_level_flow_progs*)
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
					  | DOF.DISCRETE {sample_period} => false
					  | DOF.POSTPROCESS itersym => false
					  | DOF.UPDATE itersym => false)) iterators of
			 SOME (sym, DOF.CONTINUOUS solver) => solver
		       | _ => DynException.stdException ("Requiring at least one differential equation", "CParallelWriter.buildC", Logger.INTERNAL)
    in
	solver
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
				 [$("{ // Generating output for symbol " ^ (e2s (Exp.TERM name))),
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
					   $("ob->full[modelid] |= (MAX_OUTPUT_SIZE >= ((unsigned long long)(ob->end[modelid]) - (unsigned long long)(ob->ptr[modelid])));")]),
				      $("}")],
				  $("}")]
			      )
			      (Util.addCount(!(#outputs class)))

	val total_output_quantities =
	    List.foldr op+ 0 (map (List.length o #contents) (!(#outputs class)))

    in
        if total_output_quantities > 0 then
	[$(""),
	 $("#define MAX_OUTPUT_SIZE (NUM_OUTPUTS*2*sizeof(int) + (NUM_OUTPUTS+" ^ (i2s total_output_quantities)  ^ ")*sizeof(CDATAFORMAT)) //size in bytes"),
	 $(""),
	 $("__DEVICE__ void buffer_outputs(double t, output_data *od, output_buffer *ob, unsigned int modelid) {"),
	 SUB(output_exps),
	 $("}"),
	 $("")]
         else
	 []
    end


fun buildC (model: DOF.model as (classes, inst, props)) =
    let
	val forkedModels = ModelProcess.createIteratorForkedModels model
	val forkedModelsLessUpdate = List.filter (fn{iter=(iter_sym, iter_type),...}=> case iter_type of DOF.UPDATE _ => false | _ => true) forkedModels

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
				    (Solver.solver2params solver)))

	val simengine_interface_progs = simengine_interface (class_name, inst_class, solver_name)
	val iteratordatastruct_progs = iteratordatastruct_code iterators
	val outputdatastruct_progs = outputdatastruct_code inst_class
	val outputstatestruct_progs = Util.flatmap (fn{model=(classes',_,_),...} => outputstatestruct_code classes') forkedModelsLessUpdate
	val systemstate_progs = outputsystemstatestruct_code forkedModelsLessUpdate
	val flow_data = map flow_code forkedModels
	val fun_prototypes = List.concat (map #1 flow_data)
	val flow_progs = List.concat (map #2 flow_data)
	val logoutput_progs = logoutput_code inst_class
	val simengine_target_h = $(Archive.getC "simengine/simengine_target.h")
	val simengine_api_h = $(Archive.getC "simengine/simengine_api.h")
	val solvers_h = $(Archive.getC "solvers/solvers.h")
	val solver_gpu_cu = $(Archive.getC ("solvers/solver_gpu.cu"))

	(* grab the unique solvers so that we can put the code down for each one *)
	val unique_solvers = Util.uniquify (List.mapPartial (fn(_,itertype)=> case itertype of 
										  DOF.CONTINUOUS solver => SOME (Solver.solver2name solver)
										| _ => NONE) iterators)
	val solver_c = $(String.concat (map
					    (fn(solv)=> Archive.getC ("solvers/"^solv^".c"))
					    unique_solvers))

	val simengine_api_c = $(Archive.getC "simengine/simengine_api.c")
	val defines_h = $(Archive.getC "simengine/defines.h")
	val semeta_seint_h = $(Archive.getC "simengine/semeta_seint.h")
	val output_buffer_h = $(Archive.getC "simengine/output_buffer.h")
	val init_output_buffer_c = $(Archive.getC "simengine/init_output_buffer.c")
	val log_outputs_c = $(Archive.getC "simengine/log_outputs.c")
	val exec_cpu_c = $(Archive.getC "simengine/exec_cpu.c")
	val exec_parallel_cpu_c = $(Archive.getC "simengine/exec_parallel_cpu.c")
	val exec_serial_cpu_c = $(Archive.getC "simengine/exec_serial_cpu.c")
	val exec_kernel_gpu_cu = $(Archive.getC "simengine/exec_kernel_gpu.cu")
	val exec_parallel_gpu_cu = $(Archive.getC "simengine/exec_parallel_gpu.cu")
	val exec_loop_c = $(Archive.getC "simengine/exec_loop.c")

	(* write the code *)
	val _ = output_code(class_name, ".", (header_progs @
					      [simengine_target_h] @
					      fun_prototypes @
					      [simengine_api_h] @
					      [solvers_h] @
					      [defines_h] @
					      [solver_gpu_cu] @
					      [solver_c] @
					      simengine_interface_progs @
					      [semeta_seint_h] @
					      iteratordatastruct_progs @
					      outputdatastruct_progs @
					      outputstatestruct_progs @
					      systemstate_progs @
					      [output_buffer_h] @
					      [init_output_buffer_c] @
					      logoutput_progs @
					      [simengine_api_c] @
					      [log_outputs_c] @
					      [exec_cpu_c] @
					      [exec_parallel_cpu_c] @
					      [exec_serial_cpu_c] @
					      [exec_kernel_gpu_cu] @
					      [exec_parallel_gpu_cu] @
					      flow_progs @
					      [exec_loop_c]))
    in
	SUCCESS
    end
    handle e => DynException.checkpoint "CParallelWriter.buildC" e

end
