structure ODEMexWriter =
struct

open Printer

val i2s = Util.i2s
val r2s = Util.r2s

fun outputstruct_code (class: DOF.class) =
    let
	val {outputs, ...} = class
	val output_names = map (fn{name,...}=> CWriterUtil.exp2c_str (Exp.TERM name)) (!outputs)
    in
	[$(""),
	 $("void generate_struct(mxArray **structptr) {"),
	 SUB([$("const char *field_names[] = {" ^ (String.concatWith ", " (map (fn(out)=>"\"" ^ out ^ "\"") output_names)) ^ "};"),
	      $("double *memptr;"),
	      $("*structptr = mxCreateStructMatrix(1, 1, "^(i2s (length (!outputs)))^", field_names);")] @
	     (Util.flatmap
		  (fn({name=term, contents, ...},i)=>
		     let
			 val name = CWriterUtil.exp2c_str (Exp.TERM term)
			 val var = "outputdata_" ^ name
			 val count = length contents + 1 (* add one for time *)
		     in
			 [$("if ("^var^".length > 0) {"),
			  SUB[$("memptr = MALLOCFUN("^var^".length*"^(i2s count)^"*sizeof(double));"),
			      $("if (NULL == memptr) {"),
			      SUB[$("ERRORFUN(Simatra:outOfMemory, \"Ran out of memory allocating output buffer of %d bytes\", "^var^".length*"^(i2s count)^"*sizeof(double));"),
				  $("return;")],
			      $("}"),
			      $("else {"),
			      SUB($("memcpy(memptr, "^var^".time, "^var^".length*sizeof(CDATAFORMAT));")::
				  (map
				      (fn(c, j)=> $("memcpy(memptr+("^(i2s (j+1))^"*"^var^".length), "^var^".vals"^(i2s j)^", "^var^".length*sizeof(CDATAFORMAT));"))
				      (Util.addCount contents))),
			      $("}"),
			      $("mwSize dims[2];"),
			      $("dims[0] = "^var^".length; dims[1] = "^(i2s count)^";"),
			      $("mxArray *"^name^"_array = mxCreateNumericArray(2, dims, mxDOUBLE_CLASS, mxREAL);"),
			      $("mxSetData("^name^"_array, memptr);"),
			      $("mxSetFieldByNumber(*structptr, 0, "^(i2s i)^", "^name^"_array);")
			     ],			  
			  $("}")]
		     end)
		  (Util.addCount (!outputs)))
	    ),	 
	 $("}")]
    end

fun inputstruct_code (class: DOF.class) =
    let
	val {inputs, ...} = class
	val input_names = map (fn{name,...}=> ExpProcess.exp2str (Exp.TERM name)) (!inputs)
    in
	[$(""),
	 $("int parseInputs(const mxArray *inpstruct, CDATAFORMAT *inputs) {"),
	 SUB[$("if (mxIsStruct(inpstruct)) {"),
	     SUB[$("int numfields = mxGetNumberOfFields(inpstruct);"),
		 $("int i;"),
		 $("const char *fieldname;"),
		 $("mxArray *field;"),
		 $("for (i=0;i<numfields;i++) {"),
		 SUB([$("fieldname = mxGetFieldNameByNumber(inpstruct, i);"),
		      $("field = mxGetFieldByNumber(inpstruct, 0, i);"),
		      $("if (1 != mxGetM(field) || 1 != mxGetN(field)) {"),
		      SUB[$("ERRORFUN(Simatra:inputTypeError, \"The value for field %s must be a scalar\", fieldname);"),
			  $("return 1;")],
		      $("}")] @
		     (foldl 
			  (fn((name, i),progs)=>[$("if (0 == strcmp(fieldname, \""^name^"\")) {"),
						 SUB[$("inputs["^(i2s i)^"] = mxGetScalar(field);")],
						 $("}"),
						 $("else {"),
						 SUB(progs),
						 $("}")])
			  ([$("ERRORFUN(Simatra:undefinedInputError, \"The input %s specified does not exist\", fieldname);"),
			    $("return 1;")])
			  (Util.addCount input_names))),
		 $("}")],
	     $("}"),
	     $("else {"),
	     SUB[$("ERRORFUN(Simatra:argumentError, \"The second argument must be a parameter structure\");"),
		 $("return 1;")],
	 $("}"),
	     $(""),
	     $("return 0;")],
	 $("}")]
    end


fun main_code inst_class =
    let
	val name = Symbol.name (#name inst_class)
	val orig_name = Symbol.name (ClassProcess.class2orig_name inst_class)
    in
	[$(""),
	 $("void mexFunction(int nlhs, mxArray *plhs[ ],int nrhs, const mxArray *prhs[ ]) {"),
	 SUB[$("CDATAFORMAT t = 0;"),
	     $(""),
	     $("double *y;"),
	     $("// Parse right-hand side arguments"),	     
	     $("if (nrhs != 2) {"),
	     SUB[$("ERRORFUN(Simatra:argumentError, \"Must have two arguments passed in the right side.  Type 'help "^name^"' for more information.\");"),
		 $("return;")],
	     $("}"),
	     $("if (nrhs >= 1) {"),
	     SUB[$("t = mxGetScalar(prhs[0]);")],
	     $("}"),
	     $("if (nrhs >= 2) {"),
	     SUB[$("if (STATESPACE != mxGetM(prhs[1])) {"),
		 SUB[$("ERRORFUN(Simatra:initialValueError, \"The input vector must have a length of %d\", STATESPACE);"),
		     $("return;")],
		 $("}"),
		 $("y = (double *)mxGetData(prhs[1]);")],
	     $("}"),
	     $(""),
	     $("CDATAFORMAT inputs[INPUTSPACE];"),
	     $(""),
	     $("init_inputs(inputs);"),
	     $(""),
	     $("double *dydt;"),
	     $("dydt = MALLOCFUN(STATESPACE*sizeof(double));"),
	     $("CDATAFORMAT *outputs;"),
	     $("flow_"^name^"(t, (struct statedata_"^orig_name^"*) y, (struct statedata_"^orig_name^"*) dydt, inputs, outputs, FALSE);"),
	     $(""),
	     $("plhs[0] = mxCreateNumericMatrix(STATESPACE,1, mxDOUBLE_CLASS, mxREAL);"),
	     $("mxSetData(plhs[0], dydt);"),
	     $("")],
	 $("}")]
    end	 

fun buildMexHelp name = 
    let
	fun write_help (filename, block) =
	    let
		val _ = Logger.log_notice ($("Generating Matlab MEX help file '"^ filename ^"'"))
		val file = TextIO.openOut (filename)
	    in
		Printer.printtexts (file, block, 0)
		before TextIO.closeOut file
	    end

	val upper = StdFun.toUpper name

	val progs = 
	    [$("%"^upper^" is a functional form of the "^upper^" model that "),
	     $("%is compatible with existing Matlab ode solvers"),
	     $("% "),
	     $("% [T,Y] = ode45(@"^upper^",[STARTTIME STOPTIME], Y0) utilizes the ode45"),
	     $("% solver to compute the numerical solution of "^upper^". The initial"),
	     $("% values of the states can be found in the return structure generated by"),
	     $("% buildEngine.  In this mode, inputs are limited to only their default"),
	     $("% values."),
	     $("% "),
	     $("%   m = buildEngine('myModel.dsl');"),
	     $("%   out = ode15s(@myModel, [0 100], m.state_inits);"),
	     $("% ")
	    ]
    in
	write_help(name ^ "_ode.m", progs)
    end




fun buildODEMex (model: DOF.model as (classes, inst, props)) =
    let
	val {name=inst_name, classname=class_name} = inst
	val inst_class = CurrentModel.classname2class class_name
	val class_name = Symbol.name (#name inst_class)

	val statespace = EqUtil.class2statesize inst_class

	val {iterators,time=(min_time, max_time),precision} = props
	val solver = CWriter.props2solver props

	val c_data_format = case precision 
			     of DOF.SINGLE => "float" 
			      | DOF.DOUBLE => "double"

	val header_progs = CWriter.header (class_name, 
					   ["<mex.h>"],
					   ("ITERSPACE", i2s (length iterators))::			   
					   ("STATESPACE", i2s statespace)::
					   ("CDATAFORMAT", c_data_format)::
					   ("INPUTSPACE", i2s (length (!(#inputs inst_class))))::
					   ("START_SIZE", "1000")::
					   ("MAX_ALLOC_SIZE", "65536000")::
					   ("MALLOCFUN", "mxMalloc")::
					   ("REALLOCFUN", "mxRealloc")::
					   ("PRINTFUN", "//")::
					   ("FPRINTFUN", "fprintf")::
					   (*("ERRORFUN(id,txt)", "(mexErrMsgIdAndText(#id, txt))")*)
					   ("ERRORFUN(ID, MESSAGE, ...)", "(mexErrMsgIdAndTxt(#ID, MESSAGE, ## __VA_ARGS__))")::
					   (Solver.solver2params solver))

(*
#define ERRORFUN(ID, MESSAGE, ARGS...) (fprintf(stderr, "Error (%s): " message "\n", #ID, ARGS...))
#define ERRORFUN(ID, MESSAGE, ARGS...) (mexErrMsgIdAndText(#ID, MESSAGE, ARGS...))
*)

	val input_progs = CWriter.input_code inst_class
	val outputdatastruct_progs = CWriter.outputdatastruct_code inst_class
	val outputstatestruct_progs = CWriter.outputstatestruct_code classes
	val outputinit_progs = CWriter.outputinit_code inst_class
	val init_progs = CWriter.init_code classes
	val flow_progs = CWriter.flow_code (classes, inst_class)
(*	val exec_progs = CWriter.exec_code (inst_class, props, statespace)
	val outputstruct_progs = outputstruct_code inst_class
	val inputstruct_progs = inputstruct_code inst_class*)
	val main_progs = main_code inst_class
(*	val logoutput_progs = CWriter.logoutput_code inst_class*)

	(* write the code *)
	val _ = CWriter.output_code(class_name ^ "_odemex", ".", (header_progs @ 
								  outputdatastruct_progs @
								  outputstatestruct_progs @
								  outputinit_progs @ 
								  input_progs @ 
								  init_progs @ 
								  flow_progs @ 
								  (*logoutput_progs @
								  exec_progs @
								  outputstruct_progs @
								  inputstruct_progs @*)
								  main_progs))

	val _ = buildMexHelp class_name
		

    in
	System.SUCCESS
    end



end
