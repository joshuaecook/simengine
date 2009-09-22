structure CWriter =
struct

datatype status =
	 SUCCESS 
       | FAILURE of string

open Printer
exception InternalError

val i2s = Util.i2s
val r2s = Util.r2s
val l2s = Util.l2s

(* ====================  HEADER  ==================== *)

fun header ((model: DOF.model), includes, defpairs, iterators) = 
    let
	val (_, inst as {classname,...}, _) = model
	val inst_class = CurrentModel.classname2class classname
	val class_name = Symbol.name (#name inst_class)
	val iterators = CurrentModel.iterators()
    in
	[$("// C Execution Engine for top-level model: " ^ class_name),
	 $("// " ^ Globals.copyright),
	 $(""),
	 $("#include <stdio.h>"),
	 $("#include <stdlib.h>"),
	 $("#include <math.h>"),
	 $("#include <string.h>"),
	 $("#define CDATAFORMAT double"),
	 $("#include <solvers.h>")] @
	(map (fn(inc)=> $("#include "^inc)) includes) @
	[$(""),
	 $("int fun_invocations = 0, steps = 0;"),
	 $("")] @
	(map (fn(name,value)=> $("#define " ^ name ^ " " ^ value)) defpairs) @
	[$("")] @
	(Util.flatmap (fn(sym,_)=> 
			 let
			     val statesize = ModelProcess.model2statesizebyiterator sym model
			     (*val _ = Util.log ("Computing statesize to be " ^ (i2s statesize))*)
			 in
			     [$("static CDATAFORMAT model_states_"^(Symbol.name sym)^"["^(i2s statesize)^"];"),
			      $("static CDATAFORMAT model_states_wr_"^(Symbol.name sym)^"["^(i2s statesize)^"];")]
			 end)
		      iterators) @
	[$("")]
    end

fun class2stateiterators (class: DOF.class) =
    let
	val iterators = map (fn(sym,_)=>sym) (CurrentModel.iterators())
    in
	(iterators, iterators)
    end
(*    let
	val {properties={classform,...},...} = class
    in
	case classform of 
	    DOF.FUNCTIONAL => ([],[])
	  | DOF.INSTANTIATION {readstates,writestates} => (readstates, writestates)
    end*)

fun class2iterators (class: DOF.class) =
    let
	val (readstates, writestates) = class2stateiterators class
    in
	(Util.intersection (readstates, writestates))
    end

fun outputdatastruct_code class =
    let
	val outputs = #outputs class
	fun output2struct (out as {name, contents, condition}) = 
	    let
		val {prefix,identifier,iterators} = CWriterUtil.expsym2parts class (Exp.TERM name)
		val struct_name = "output_" ^ identifier
		val struct_inst = "outputdata_" ^ identifier
		val iter = TermProcess.symbol2temporaliterator name
	    in
		[$(""),
		 $("struct " ^ struct_name ^ " {"),
		 SUB[$("int length;"),
		     $("int alloc_size;"),
		     (case iter of 
			  SOME (sym, _) => $("CDATAFORMAT *time; // equivalent to iterator " ^ (Symbol.name sym))
			| NONE => $("// no temporal iterator")),
		     $("// output data: "),
		     SUB(map (fn(exp,i)=> $("CDATAFORMAT *vals" ^ (i2s i) ^ "; // " ^ (ExpProcess.exp2str exp))) (Util.addCount contents))],
		 $("};"),
		 $("struct " ^ struct_name ^ " " ^ struct_inst ^ ";")]
	    end
    in
	List.concat (map output2struct (!outputs))
    end

fun outputstatestructbyclass_code iter (class : DOF.class) =
    let
	val classname = ClassProcess.class2orig_name class
	val exps = #exps class
	val state_eqs_symbols = map ExpProcess.lhs (List.filter (ExpProcess.isStateEqOfIter iter) (!exps))
	val instances = List.filter ExpProcess.isInstanceEq (!exps)
	val class_inst_pairs = ClassProcess.class2instnames class
	val iter_name = Symbol.name (#1 iter)
    in
	[$(""),
	 $("// define state structures"),
	 $("struct statedata_" ^ (Symbol.name classname) ^ "_" ^ iter_name ^ " {"),	 
	 SUB($("// states (count="^(i2s (List.length state_eqs_symbols))^")")::
	     (map (fn(exp)=>
		     let
			 (* TODO - size must be a list of sizes *)
			 val size = (*Term.symbolSpatialSize (ExpProcess.exp2term sym)*) ExpProcess.exp2size (#iterators class) exp
			 val name = Symbol.name (Term.sym2curname (ExpProcess.exp2term exp))
		     in
			 if size = 1 then
			     $("CDATAFORMAT " ^ name ^ ";")
			 else
			     $("CDATAFORMAT " ^ name ^ "["^(i2s size)^"];")
		     end) state_eqs_symbols) @
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
			      $("struct statedata_" ^ (Symbol.name classname) ^ "_" ^ iter_name ^ " "^(Symbol.name instname)^";")
			  else
			      $("struct statedata_" ^ (Symbol.name classname) ^ "_" ^ iter_name ^ " "^(Symbol.name instname)^"["^(i2s size)^"];")
		      end)
		   class_inst_pairs))),
	 $("};")]
    end

fun outputstatestruct_code iterators classes =
    let
	fun isMasterClass {properties={classtype,...},...} =
	    case classtype of
		DOF.MASTER _ => true
	      | _ => false
	val master_classes = List.filter isMasterClass classes

	val predeclare_statements = 
	    map
		(fn(class)=> "struct statedata_" ^ (Symbol.name (ClassProcess.class2orig_name class)))
		master_classes

    in
	Util.flatmap (fn(iter as (name, itertype))=>
			 ($("")::($("// Pre-declare state structures (iterator '"^(Symbol.name name)^"')"))::(map (fn(str)=> $(str ^ "_" ^ (Symbol.name name) ^ ";")) predeclare_statements)) @
			 List.concat (map (outputstatestructbyclass_code iter) master_classes)) iterators
    end

fun outputinit_code class =
    let 
	val outputs = #outputs class
	fun output2progs (out as {name, contents, condition}) = 
	    let
		val {prefix,identifier,iterators} = CWriterUtil.expsym2parts class (Exp.TERM name)
		val var = "outputdata_" ^ identifier
		val iter = TermProcess.symbol2temporaliterator name
		val spatial_iterators = TermProcess.symbol2spatialiterators name

		fun iter2size iter = 
		    let
			val {name,high,low,step} = CWriterUtil.iter2range class iter
		    in
			Real.ceil((high-low)/step) + 1
		    end
		    
		val total_size = ExpProcess.exp2size (#iterators class) (Exp.TERM name)
	    in
		[$(var ^ ".length = 0;"),
		 $(var ^ ".alloc_size = START_SIZE;")] @
		(case iter of
		     SOME (sym, _) => [$(var ^ ".time = MALLOCFUN(START_SIZE*sizeof(CDATAFORMAT)); // iterator '"^(Symbol.name sym)^"'")]
		   | NONE => []) @
		(map (fn(c,i)=> $(var ^ ".vals" ^ (i2s i) ^ " = MALLOCFUN(START_SIZE*"^(i2s total_size)^"*sizeof(CDATAFORMAT));")) (Util.addCount contents))
	    end

	val dependent_symbols = CWriterUtil.class2uniqueoutputsymbols class
	val sym_decls = map
			    (fn(term, sym)=> 
			       if (ExpProcess.exp2size (#iterators class) (Exp.TERM term)) > 1 then
				   $("CDATAFORMAT *outputsave_" ^ (Symbol.name sym) ^ ";")
			       else
				   $("CDATAFORMAT outputsave_" ^ (Symbol.name sym) ^ ";")
			    )
			    dependent_symbols

	val init_outputsaves = List.mapPartial 
				   (fn(term, sym)=> 
				      let
					  val size = ExpProcess.exp2size (#iterators class) (Exp.TERM term)
				      in
					  if size > 1 then
					      SOME ($("outputsave_" ^ (Symbol.name sym) ^ " = MALLOCFUN("^(i2s size)^" * sizeof(CDATAFORMAT));"))
					  else
					      NONE
				      end
				   ) dependent_symbols

    in
	[$(""),
	 $("void output_init() {"),
	 SUB(List.concat (map output2progs (!outputs))),
	 $("}"),
	 $(""),
	 $("// declaring variables to store for computing outputs")] @
	 sym_decls @ 
	[$(""),
	 $("void outputsave_init() {"),
	 SUB(init_outputsaves),
	 $("}")]

    end

fun initbyclass_code iter class =
    let
	val classname = ClassProcess.class2orig_name class
	val exps = #exps class
	val init_eqs = (List.filter ExpProcess.isInitialConditionEq (!exps))
	(* init_eqs' - matches only those with the correct iterator *)
	val init_eqs' = List.filter (fn(exp)=> case ExpProcess.lhs exp of
						   Exp.TERM (Exp.SYMBOL (_, props))=> 
						   (case Property.getIterator props of
							SOME ((sym,_)::rest)=> sym = (#1 iter)
						      | _ => false)
						 | _ => false)
				    init_eqs
	val instances = List.filter ExpProcess.isInstanceEq (!exps)
	val class_inst_pairs = ClassProcess.class2instnames class
	val iter_name = Symbol.name (#1 iter)
    in
	[$(""),
	 $("// define state initialization functions"),
	 $("void init_" ^ (Symbol.name classname) ^ "_" ^ iter_name ^ "(struct statedata_"^(Symbol.name classname)^"_"^iter_name^" *states) {"),	 
	 SUB($("// states (count="^(i2s (List.length init_eqs))^")")::
	     (Util.flatmap (fn(exp)=>
		     let
			 val size = (*Term.symbolSpatialSize (ExpProcess.exp2term (ExpProcess.lhs sym))*)
			     ExpProcess.exp2size (#iterators class) exp
			 val {prefix,identifier,iterators} = CWriterUtil.expsym2parts class (ExpProcess.lhs exp)
			 (*val name = Symbol.name (Term.sym2curname (ExpProcess.exp2term (ExpProcess.lhs exp)))*)
			 val assigned_value = CWriterUtil.exp2c_str (ExpProcess.rhs exp)
		     in
			 if size = 1 then
			     [$("states->" ^ identifier ^ " = " ^ assigned_value ^ ";")]
			 else (* might have to do something special here or in c_writer_util - add the foreach code...*)
			     CWriterUtil.expandprogs2parallelfor 
				 class 
				 (exp, [$("states->" ^ identifier ^ iterators ^" = " ^ assigned_value ^ ";")])
		     end) init_eqs') @
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
			      $("init_" ^ (Symbol.name classname) ^ "_" ^ iter_name ^ "(&states->"^(Symbol.name instname)^");")
			  else (* not sure what to do here *)
			      $("init_" ^ (Symbol.name classname) ^ "(&states->"^(Symbol.name instname)^");")
		      end)
		   class_inst_pairs))),
	 $("};")]
    end
    

fun init_code (classes, inst_class, iterators) =
    let
	fun isMasterClass {properties={classtype,...},...} =
	    case classtype of
		DOF.MASTER _ => true
	      | _ => false
	val master_classes = List.filter isMasterClass classes
	fun orig_name class = Symbol.name (ClassProcess.class2orig_name class)
	fun iter_name iter = Symbol.name (#1 iter)

	fun predeclare_statements itername = 
	    map
		(fn(class)=> $("void init_" ^ (orig_name class) ^ "_" ^ itername ^ "(struct statedata_"^ (orig_name class) ^"_" ^ itername ^ " *states);"))
		master_classes

	val init_states_code = 
	    [$(""),
	     $("// Call the individual init_??? routines for each iterator"),
	     $("void init_states() {"),
	     SUB(map (fn(iter)=> $("init_" ^ (orig_name inst_class) ^ "_" ^ (iter_name iter) ^ "((struct statedata_"^(orig_name inst_class)^"_"^(iter_name iter)^"*) model_states_"^(iter_name iter)^");")) iterators),
	     $("}")]

    in
	(Util.flatmap (fn(iter as (name,itertype))=> ($("")::($("// Pre-declare state initialization functions"))::(predeclare_statements (Symbol.name name))) @
						     List.concat (map (initbyclass_code iter) master_classes))
		      iterators) @
	init_states_code
    end

fun class2flow_code (class, top_class) =
    let
	val orig_name = ClassProcess.class2orig_name class

	val (readstates, writestates) = class2stateiterators class
	val iterators = CurrentModel.iterators()
	val iteratorprototypes = String.concat (map (fn(sym,_)=> "CDATAFORMAT " ^ (Symbol.name sym) ^ ", ") iterators)
				 
	val stateprototypes = 
	    (String.concat (map
				(fn(sym)=> "const struct statedata_" ^ (Symbol.name orig_name) ^ "_" ^ (Symbol.name sym) ^ " *rd_" ^ (Symbol.name sym) ^ ", ")
				readstates)) ^
	    (String.concat (map
				(fn(sym)=> "struct statedata_" ^ (Symbol.name orig_name) ^ "_" ^ (Symbol.name sym) ^ " *wr_" ^ (Symbol.name sym) ^ ", ")
				writestates))

	val header_progs = 
	    [$(""),
	     $("int flow_" ^ (Symbol.name (#name class)) 
	       ^ "("^iteratorprototypes^stateprototypes^"CDATAFORMAT inputs[], CDATAFORMAT outputs[], int first_iteration) {")]

	val read_memory_progs = []

	val read_states_progs = []
	    
	val read_inputs_progs =
	    [$(""),
	     $("// mapping inputs to variables")] @ 
	    (map
		 (fn({name,default},i)=> $("CDATAFORMAT " ^ (CWriterUtil.exp2c_str (Exp.TERM name)) ^ " = inputs[" ^ (i2s i) ^ "];"))
		 (Util.addCount (!(#inputs class))))


	(* filter out all the unneeded expressions *)
	val (initvalue_exps, rest_exps) = List.partition ExpProcess.isInitialConditionEq (!(#exps class))
	val (valid_exps, rest_exps) = List.partition (fn(exp)=>(ExpProcess.isIntermediateEq exp) orelse
							     (ExpProcess.isInstanceEq exp) orelse
							     (ExpProcess.isStateEq exp)) rest_exps
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
			    let
				val size = ExpProcess.exp2size (#iterators class) exp
				val spatial_iterators = ExpProcess.exp2spatialiterators exp
				fun exp2lhssymname exp = Symbol.name (Term.sym2curname (ExpProcess.getLHSSymbol exp))
			    in
				if (ExpProcess.isIntermediateEq exp) then
				    if size > 1 then					
					if (ExpProcess.isTerm (ExpProcess.rhs exp)) andalso 
					   (Term.isNumeric (ExpProcess.exp2term (ExpProcess.rhs exp))) then
					    if (Term.isScalar (ExpProcess.exp2term (ExpProcess.rhs exp))) then
						(* duplicate each element as it is assigned *)
						[$("CDATAFORMAT "^(exp2lhssymname exp)^"["^(i2s size)^"] = "^
						   "{"^(String.concatWith ", " (List.tabulate (size, fn(x)=>(CWriterUtil.exp2c_str (ExpProcess.rhs exp)))))^"};")]
					    else
						(* assign as a vector*)
						[$("CDATAFORMAT "^(exp2lhssymname exp)^"["^(i2s size)^"] = "^
						   (CWriterUtil.exp2c_str (ExpProcess.rhs exp))^"};")]
					else
 					    [$("CDATAFORMAT " ^ (exp2lhssymname exp) ^ "["^(i2s size)^"];")] @ 
					    (CWriterUtil.exp2parallelfor class exp)
				    else
 					[$("CDATAFORMAT " ^ (CWriterUtil.exp2c_str exp) ^ ";")]
				else if (ExpProcess.isStateEq exp) then
				    if size > 1 then
					CWriterUtil.exp2parallelfor class exp
				    else
 					[$((CWriterUtil.exp2c_str exp) ^ ";")]
				else if (ExpProcess.isInstanceEq exp) then
				    let
					val {classname, instname, props, inpargs, outargs} = ExpProcess.deconstructInst exp
					val orig_instname = case InstProps.getRealInstName props of
								SOME v => v
							      | NONE => instname

					val class = CurrentModel.classname2class classname
					val {properties={classform,...},...} = class

					val iterators = map (fn(sym, _)=>sym) (CurrentModel.iterators())
					val statereads = map
							     (fn(sym)=> "&rd_" ^ (Symbol.name sym) ^ "->" ^ (Symbol.name orig_instname) ^ ", ")
							     iterators
							     
					val statewrites = map
							      (fn(sym)=> "&wr_" ^ (Symbol.name sym) ^ "->" ^ (Symbol.name orig_instname) ^ ", ")
							      iterators

					val calling_name = "flow_" ^ (Symbol.name classname)

					val inpvar = Unique.unique "inputdata"
					val outvar = Unique.unique "outputdata"

					val inps = "CDATAFORMAT " ^ inpvar ^ "[] = {" ^ (String.concatWith ", " (map CWriterUtil.exp2c_str inpargs)) ^ "};"
					val outs_decl = "CDATAFORMAT " ^ outvar ^ "["^(i2s (List.length outargs))^"];"
				    in
					[SUB([$("// Calling instance class " ^ (Symbol.name classname)),
					      $("// " ^ (CWriterUtil.exp2c_str exp)),
					      $(inps), $(outs_decl),
					      $(calling_name ^ "("^(String.concat (map (fn(sym)=>Symbol.name sym ^ ", ") iterators))^(String.concat statereads) ^ (String.concat statewrites) ^ inpvar^", "^outvar^", first_iteration);")] @
					     let
						 val symbols = map
								   (fn(outsym) => Term.sym2curname outsym)
								   outargs
					     in
						 map
						     (fn((sym, {name, contents, condition}),i')=> 
							$("CDATAFORMAT " ^ (Symbol.name sym) ^ " = " ^ outvar ^
							  "["^(i2s i')^"]; // Mapped to "^(Symbol.name classname)^": "^(ExpProcess.exp2str (List.hd (contents)))))
						     (Util.addCount (ListPair.zip (symbols, !(#outputs class))))
					     end)

					]
				    end
				else
				    DynException.stdException(("Unexpected expression '"^(ExpProcess.exp2str exp)^"'"), "CWriter.class2flow_code.equ_progs", Logger.INTERNAL)
			    end)
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
		 SUB(map
			 (fn(t,s)=> 
			    let
				val size = ExpProcess.exp2size (#iterators class) (Exp.TERM t)
				val {prefix,identifier,iterators} = CWriterUtil.expsym2parts class (Exp.TERM t)
			    in
				if size > 1 then
				    $("memcpy(outputsave_"^(Symbol.name s)^", &("^prefix^"->"^identifier^"), "^(i2s size)^" * sizeof(CDATAFORMAT));")
				else
				    $("outputsave_" ^ (Symbol.name s) ^ " = " ^ (CWriterUtil.exp2c_str (Exp.TERM t)) ^ ";")
			    end
			 )
			 (CWriterUtil.class2uniqueoutputsymbols class)),
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
					    DynException.stdException (("Output '"^(ExpProcess.exp2str (Exp.TERM name))^"' in class '"^(Symbol.name (#name class))^"' can not be a grouping of {"^(String.concatWith ", " (map ExpProcess.exp2str contents))^"} when used as a submodel"), "CWriter.class2flow_code", Logger.INTERNAL)
					    
				val valid_condition = case condition 
						       of (Exp.TERM (Exp.BOOL v)) => v
							| _ => false
				val _ = if valid_condition then
					    ()
					else
					    DynException.stdException (("Output '"^(ExpProcess.exp2str (Exp.TERM name))^"' in class '"^(Symbol.name (#name class))^"' can not have a condition '"^(ExpProcess.exp2str condition)^"' when used as a submodel"), "CWriter.class2flow_code", Logger.INTERNAL)
					    
			    in
				case contents of
				    [content] =>
				    $("outputs["^(i2s i)^"] = " ^ (CWriterUtil.exp2c_str (content)) ^ ";")
				  | _ => 
				    DynException.stdException (("Output '"^(ExpProcess.exp2str (Exp.TERM name))^"' in class '"^(Symbol.name (#name class))^"' can not be a grouping of {"^(String.concatWith ", " (map ExpProcess.exp2str contents))^"} when used as a submodel"), 
							       "CWriter.class2flow_code", 
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
	val iteratorprototypes = String.concat (map (fn(sym,_)=> "CDATAFORMAT " ^ (Symbol.name sym) ^ ", ") iterators)
				 
	val stateprototypes = 
	    (String.concat (map
				(fn(sym)=> "const struct statedata_" ^ (orig_name) ^ "_" ^ (Symbol.name sym) ^ " *rd_" ^ (Symbol.name sym) ^ ", ")
				readstates)) ^
	    (String.concat (map
				(fn(sym)=> "struct statedata_" ^ (orig_name) ^ "_" ^ (Symbol.name sym) ^ " *wr_" ^ (Symbol.name sym) ^ ", ")
				writestates))
    in
  	[$(""),
	 $("// wrapper for flow function over iterator '"^iter_name^"'"),
	 $("int flow_"^iter_name^"(CDATAFORMAT "^iter_name^", const CDATAFORMAT *rd_"^iter_name^", CDATAFORMAT *wr_"^iter_name^", CDATAFORMAT inputs[], CDATAFORMAT outputs[], int first_iteration) {"),
	 SUB[$("return flow_"^orig_name^"("^(String.concat (map (fn(sym,_)=> Symbol.name sym ^ ", ") iterators))^(String.concat (map (fn(sym)=>  "(const struct statedata_"^orig_name^"_"^(Symbol.name sym)^" *) rd_" ^ (Symbol.name sym) ^ ", ") readstates))^(String.concat (map (fn(sym)=> "(struct statedata_"^orig_name^"_"^(Symbol.name sym)^"*) wr_" ^ (Symbol.name sym) ^ ", ") writestates))^"inputs, outputs, first_iteration);")],
	 $("}")]
    end


fun flow_code (model: DOF.model) = 
    let
	val (classes, inst, props) = model	
	val {name=inst_name, classname=class_name} = inst
	val inst_class = CurrentModel.classname2class class_name

	fun isInline (class: DOF.class) =
	    let
		val {properties={classform,...},...} = class
	    in
		case classform of 
		    DOF.FUNCTIONAL => true
		  | _ => false
	    end
	val iterators = CurrentModel.iterators()

	val fundecl_progs = map
				(fn(class) => 
				   let
				       val orig_name = ClassProcess.class2orig_name class
				   in
				       if isInline class then
					   $("CDATAFORMAT "^(Symbol.name (#name class))^"("^(String.concatWith ", " (map (fn{name,...}=> "CDATAFORMAT " ^ (CWriterUtil.exp2c_str (Exp.TERM name))) (!(#inputs class))))^");")
				       else
					   let
					       val (readstates, writestates) = class2stateiterators class

					       val iteratorprototypes = String.concat (map (fn(sym,_)=> "CDATAFORMAT " ^ (Symbol.name sym) ^ ", ") iterators)
									
					       val stateprototypes = 
						   (String.concat (map
								       (fn(sym)=> "const struct statedata_" ^ (Symbol.name orig_name) ^ "_" ^ (Symbol.name sym) ^ " *rd_" ^ (Symbol.name sym) ^ ", ")
								       readstates)) ^
						   (String.concat (map
								       (fn(sym)=> "struct statedata_" ^ (Symbol.name orig_name) ^ "_" ^ (Symbol.name sym) ^ " *wr_" ^ (Symbol.name sym) ^ ", ")
								       writestates))

					   in
					       $("int flow_" ^ (Symbol.name (#name class)) ^ "("^iteratorprototypes^stateprototypes^"CDATAFORMAT inputs[], CDATAFORMAT outputs[], int first_iteration);")
					   end
				   end)
				classes
				
	val flow_progs = List.concat (map (fn(c)=>
					     if isInline c then
						 (Logger.log_error ($("Functional classes like '"^(Symbol.name (#name c))^"' are not supported"));
						  DynException.setErrored();
						  [])
					     else
						 class2flow_code (c,#name c = #name inst_class)) classes)

	val top_level_flow_progs = [$"",
				    $("int model_flows(CDATAFORMAT t, const void *y, void *dydt, CDATAFORMAT inputs[], CDATAFORMAT outputs[], int first_iteration){"),
				    SUB[$("return flow_" ^ (Symbol.name (#name topclass)) ^ "(t, y, dydt, inputs, outputs,first_iteration);")],
				    $("}")]
    in
	[$("// Flow code function declarations")] @
	fundecl_progs @
	flow_progs @
	(Util.flatmap (fn(iter)=>flow_wrapper (inst_class, iter)) iterators)
    end

fun input_code (class: DOF.class) =
    [$(""),
     $("void init_inputs(double *inputs) {"),
     SUB(map 
	     (fn({name,default},i)=> $("inputs["^(i2s i)^"] = " ^(case default 
								of SOME t => CWriterUtil.exp2c_str t
								 | NONE => "(0.0/0.0)")^ "; // " ^ (ExpProcess.exp2str (Exp.TERM name))))
	     (Util.addCount (!(#inputs class)))),
     $("}")]

fun output_code (name, location, block) =
    let
      val filename = location ^ "/" ^ name ^ ".c"
      val _ = Logger.log_notice ($("Generating C source file '"^ filename ^"'"))
      val file = TextIO.openOut (filename)
    in
      Printer.printtexts (file, block, 0)
      before TextIO.closeOut file
    end

fun props2solvers props = 
    let
	val iterators = #iterators props
	(* val _ = Util.log("In props2solvers: found iterators: " ^ (l2s (map (fn(sym,_)=>Symbol.name sym) iterators))) *)
	val continuous_iterators = List.filter (fn(sym, itertype)=>case itertype of
					       DOF.CONTINUOUS _ => true
					     | DOF.DISCRETE _ => false) iterators
        (* val _ = Util.log("In props2solvers: found continuous iterators: " ^ (l2s (map (fn(sym,_)=>Symbol.name sym) continuous_iterators))) *)
    in
	List.mapPartial (* just to skip out on the exception that is not needed *)
	    (fn(sym, itertype)=> case itertype of
				     DOF.CONTINUOUS solver => SOME (sym, solver)
				   | _ => NONE (* won't reach this condition *))
	    continuous_iterators
    end

fun props2discretes props =
    let
	val iterators = #iterators props
	val discrete_iterators = List.filter (fn(sym, itertype)=>case itertype of
					       DOF.CONTINUOUS _ => false
					     | DOF.DISCRETE _ => true) iterators
    in
	List.mapPartial (* just to skip out on the exception that is not needed *)
	    (fn(sym, itertype)=> case itertype of
				     DOF.DISCRETE {fs} => SOME (sym, fs)
				   | _ => NONE (* won't reach this condition *))
	    discrete_iterators
    end


fun exec_code (class:DOF.class, props, statespace) =
    let
	val iter_solver_list = props2solvers props
	val iter_fs_list = props2discretes props
	val orig_name = Symbol.name (ClassProcess.class2orig_name class)
	val iterators = CurrentModel.iterators()
			
	val itervars = String.concat (map (fn(sym,_)=> "CDATAFORMAT *" ^ (Symbol.name sym) ^ ", ") iterators)

	val time_comparison = String.concatWith " || " (map (fn(sym, itertype)=> 
							    case itertype of
								DOF.CONTINUOUS _ => "*" ^ (Symbol.name sym) ^ " < t1"
							      | DOF.DISCRETE {fs} => "(*" ^ (Symbol.name sym) ^ "/" ^ (r2s fs) ^ ") < t1") iterators)
    in
	[$(""),
	 $("void exec_loop(CDATAFORMAT *t, CDATAFORMAT t1, CDATAFORMAT *inputs) {"),	 
	 SUB((StdFun.flatmap 
		  (fn(sym, itertype)=> 
		     case itertype of
			 DOF.CONTINUOUS _ => 
			 [$("CDATAFORMAT "^(Symbol.name sym)^"_ptr = *t...;"),
			  $("CDATAFORMAT *"^(Symbol.name sym)^" = &"^(Symbol.name sym)^"_ptr;")]
		       | DOF.DISCRETE {fs} => 
			 [$("CDATAFORMAT "^(Symbol.name sym)^"_ptr = *t * "^(r2s fs)^";"),
			  $("CDATAFORMAT *"^(Symbol.name sym)^" = &"^(Symbol.name sym)^"_ptr;")])
		  (List.filter (fn(sym,_)=> sym <> (Symbol.symbol "t")) iterators)) @
	     (Util.flatmap (fn(sym, solver)=>
			      let
				  val iter_name = Symbol.name sym
				  val {dt, abstol, reltol} = Solver.solver2options solver
				  val statesize = ModelProcess.model2statesizebyiterator sym (CurrentModel.getCurrentModel())
			      in
				  [$("solver_props props_"^iter_name^";"),
				   $("props_"^iter_name^".timestep = "^(r2s dt)^";"),
				   $("props_"^iter_name^".abstol = "^(r2s abstol)^";"),
				   $("props_"^iter_name^".reltol = "^(r2s reltol)^";"),
				   $("props_"^iter_name^".starttime = *"^iter_name^";"),
				   $("props_"^iter_name^".stoptime = t1;"),
				   $("props_"^iter_name^".time = t;"),
				   $("props_"^iter_name^".model_states = model_states_"^iter_name^";"),
				   $("props_"^iter_name^".inputs = inputs;"),
				   $("props_"^iter_name^".outputs = NULL;"),
				   $("props_"^iter_name^".first_iteration = TRUE;"),
				   $("props_"^iter_name^".statesize = "^(i2s statesize)^";"),
				   $("props_"^iter_name^".fun = &flow_"^(iter_name)^";"),
				   $("INTEGRATION_METHOD_"^iter_name^"(mem) *mem_"^iter_name^" = INTEGRATION_METHOD_"^iter_name^"(init)(&props_"^iter_name^");"),
				   $("")]
			      end)
			   iter_solver_list) @
	     [$("while ("^time_comparison^") {"),
	      SUB((Util.flatmap (fn(sym, solver)=> 
				   [$("double prev_"^(Symbol.name sym)^" = *"^(Symbol.name sym)^";"),
				    $("if (*"^(Symbol.name sym)^" < t1) {"),
				    SUB[$("int status = INTEGRATION_METHOD_"^(Symbol.name sym)^"(eval)(mem_"^(Symbol.name sym)^");"),
					$("if (status != 0) {"),
					SUB[(*$("sprintf(str, \"Flow calculated failed at time=%g\", *t);")*)
					    $("ERRORFUN(Simatra:flowError, \"Flow calculation failed at time=%g for iterator "^(Symbol.name sym)^"\", *t);"),
					    $("break;")],
					$("}")],
				    $("}"),
				    $("")]) iter_solver_list) @
		  (Util.flatmap
		       (fn(sym, fs)=> 
			  [$("double prev_"^(Symbol.name sym)^" = *"^(Symbol.name sym)^";"),
			   $("if ((*"^(Symbol.name sym)^"/"^(r2s fs)^") < t1) {"),
			   SUB[$("int status = flow_"^(Symbol.name sym)^
				 "(*"^(Symbol.name sym)^", model_states_"^
				 (Symbol.name sym)^", model_states_wr_"^(Symbol.name sym)^", inputs, NULL, 1);"),
			       $("if (status != 0) {"),
			       SUB[(*$("sprintf(str, \"Flow calculated failed at time=%g\", *t);")*)
				   $("ERRORFUN(Simatra:flowError, \"Flow calculation failed at time=%g for iterator "^(Symbol.name sym)^"\", *t);"),
				   $("break;")],
			       $("}"),
			       $("(*"^(Symbol.name sym)^")++;")],
			   $("}")
			  ]
		       )
		       iter_fs_list) @
		  [$("if (log_outputs("^(String.concat (map (fn(sym,_)=>"prev_" ^ (Symbol.name sym) ^ ", ") iterators))^
		     (String.concatWith ", " (map (fn(sym,_)=> "(struct statedata_"^orig_name^"_"^(Symbol.name sym)^"*) model_states_"^(Symbol.name sym)) iterators))^") != 0) {"),
		   SUB[$("ERRORFUN(Simatra:outOfMemory, \"Exceeded available memory\");"),
		       $("break;")],		   
		   $("}")] @	
		  (* here, do the memory copy back *)
		  (map (fn(sym,_)=>
			  $("memcpy(model_states_"^(Symbol.name sym)^", model_states_wr_"^(Symbol.name sym)^", sizeof(struct statedata_"^orig_name^"_"^(Symbol.name sym)^"));"))
		       iter_fs_list) @
		   [$("steps++;")](*,*)
		 (*  $("PRINTFUN(\"%g,"^(String.concatWith "," (List.tabulate (statespace, fn(i)=>"%g")))^"\\n\", t, "^
		       (String.concatWith ", " (List.tabulate (statespace, fn(i)=>"model_states["^(i2s i)^"]")))^");")*)
		 ),
	      $("}")] @
	      (Util.flatmap 
		   (fn(sym, solver)=> [$("INTEGRATION_METHOD_"^(Symbol.name sym)^"(free)(mem_"^(Symbol.name sym)^");")])
		   iter_solver_list)
	    ),
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
						       | _ => DynException.stdException (("Unexpected non symbol"), "CWriter.logoutput_code", Logger.INTERNAL)
				   val size = ExpProcess.exp2size (#iterators class) (Exp.TERM term)
			       in
				   (*if local_scope then*)
				   if size > 1 then
				       $("CDATAFORMAT *" ^ (Symbol.name sym) ^ " = outputsave_" ^ (Symbol.name sym) ^ ";")
				   else
				       $("CDATAFORMAT " ^ (Symbol.name sym) ^ " = outputsave_" ^ (Symbol.name sym) ^ ";")
				  (* else
				       $("CDATAFORMAT " ^ (Symbol.name sym) ^ " = " ^ (CWriterUtil.exp2c_str (Exp.TERM term)) ^ ";")*)
			       end)
			    dependent_symbols
	val output_exps =Util.flatmap
			      (fn(out as {condition, contents, name})=> 
				 let
				     val {prefix,identifier,iterators} = CWriterUtil.expsym2parts class (Exp.TERM name)
				     val size = ExpProcess.exp2size (#iterators class) (Exp.TERM name)
				     val var = "outputdata_" ^ identifier
				     val iter = TermProcess.symbol2temporaliterator name
				 in
				     [$("{ // Generating output for symbol " ^ (ExpProcess.exp2str (Exp.TERM name))),
				      SUB[$("int cond = " ^ (CWriterUtil.exp2c_str condition) ^ ";"),
					  $("if (cond) {"),
					  SUB([$("if ("^var^".length == "^var^".alloc_size) {"),
					       SUB((case iter of 
							SOME (sym, _) => [$("CDATAFORMAT *new_ptr = REALLOCFUN("^var^".time, "^var^".alloc_size*2*sizeof(CDATAFORMAT));"),
									  $("if (NULL == new_ptr) return 1;"),
									  $(var ^ ".time = new_ptr;")]
						      | NONE => [$("CDATAFORMAT *new_ptr;")]) @
						   (Util.flatmap 
							(fn(_, i)=> [$("new_ptr = REALLOCFUN("^var^".vals"^(i2s i)^", "^var^".alloc_size*2*"^(i2s size)^"*sizeof(CDATAFORMAT));"),
								     $("if (NULL == new_ptr) return 1;"),
								     $(var^".vals"^(i2s i)^" = new_ptr;")])
							(Util.addCount contents)) @
						   [$(var^".alloc_size *= 2;")]),
					       $("}")] @					      
					      (case iter of
						   SOME (sym,_)=>  [$(var^".time["^var^".length] = "^(Symbol.name sym)^";"),
								    $("PRINTFUN(\"%g \", "^(Symbol.name sym)^");")]
						 | NONE => []) @
					      (Util.flatmap
						   (fn(exp,i)=> 
						      let
							  val {prefix,identifier,iterators} = CWriterUtil.expsym2parts class exp
						      in
							  CWriterUtil.expandprogs2parallelfor
							      class
							      (exp, [$(var^".vals"^(i2s i)^"["^var^".length] = "^identifier^iterators^";"), 
								     $("PRINTFUN(\"%g \", "^var^".vals"^(i2s i)^"["^var^".length]);")])
						      end)
						   (Util.addCount contents)) @
					      [$(var^".length++;")]),
					  $("}")
					 ],
				      $("}")]
				 end)
			      (!(#outputs class))

	val (readstates, writestates) = class2stateiterators class
	val iterators = CurrentModel.iterators()
	val itervars = String.concat (map (fn(sym,_)=> "CDATAFORMAT " ^ (Symbol.name sym) ^ ", ") iterators)
	val stateprototypes = 
	    (String.concatWith ", " (map
					 (fn(sym)=> "const struct statedata_" ^ (orig_name) ^ "_" ^ (Symbol.name sym) ^ " *rd_" ^ (Symbol.name sym))
				readstates))

    in
	[$(""),
	 $("int log_outputs("^itervars^stateprototypes^") {"),
	 SUB(sym_decls @
	     [$("")] @
	     output_exps @
	     [$(""),
	      $("PRINTFUN(\"\\n\");"),
	      $("return 0;")]
	    ),
	 $("}")]
    end

fun main_code class =
    let
	val name = Symbol.name (#name class)
	val orig_name = Symbol.name (ClassProcess.class2orig_name class)
    in
	[$("int main(int argc, char **argv) {"),
	 SUB[(*$("FPRINTFUN(stderr,\"Running '"^name^"' model ...\\n\");"),*)
	     $(""),
	     $("// Get the simulation time t from the command line"),
	     $("double t = 0;"),
	     $("double t1 = atof(argv[1]);"),
	     $("output_init(); // initialize the outputs"),
	     $("outputsave_init(); // initialize temporary memory used for outputs"),
	     $("init_states(); // initialize the states"),
	     $("CDATAFORMAT inputs[INPUTSPACE];"),
	     $(""),
	     $("init_inputs(inputs);"),
	     $(""),
	     $("exec_loop(&t, t1, inputs);"),
	     $(""),
	     $("return 0;")],
	 $("}")]
    end

fun buildC (model: DOF.model as (classes, inst, props)) =
    let
	val {name=inst_name, classname=class_name} = inst
	val inst_class = CurrentModel.classname2class class_name
	val class_name = Symbol.name (#name inst_class)

	val statespace = ClassProcess.class2statesize inst_class

	val {iterators,precision,...} = props
	val iter_solver_list = props2solvers props

	val c_data_format = case precision 
			     of DOF.SINGLE => "float" 
			      | DOF.DOUBLE => "double"

	val header_progs = header (model,
				   [], (* no additional includes *)
				   ("ITERSPACE", i2s (length iterators))::
				   ("STATESPACE", i2s statespace)::
				   ("CDATAFORMAT", c_data_format)::
				   ("INPUTSPACE", i2s (length (!(#inputs inst_class))))::nil @ 
				   (map (fn(sym, solver)=>("INTEGRATION_METHOD_"^(Symbol.name sym)^"(m)", (Solver.solver2name solver) ^ "_ ## m")) iter_solver_list) @
				   (("START_SIZE", "1000")::
				    ("MAX_ALLOC_SIZE", "65536000")::
				    ("MALLOCFUN", "malloc")::
				    ("REALLOCFUN", "realloc")::
				    ("PRINTFUN", "printf")::
				    (*("ERRORFUN(id,txt)", "(fprintf(stderr, \"Error (%s): %s\\n\", #id, txt))")*)
				    ("ERRORFUN(ID, MESSAGE, ...)", "(fprintf(stderr, \"Error (%s): \" MESSAGE \"\\n\", #ID, ## __VA_ARGS__))")::
				    nil),				 
				   iterators)

	val input_progs = input_code inst_class
	val outputdatastruct_progs = outputdatastruct_code inst_class
	val outputstatestruct_progs = outputstatestruct_code iterators classes
	val outputinit_progs = outputinit_code inst_class
	val init_progs = init_code (classes, inst_class, iterators)
	val flow_progs = flow_code model
	val exec_progs = exec_code (inst_class, props, statespace)
	val main_progs = main_code inst_class
	val logoutput_progs = logoutput_code inst_class

	(* write the code *)
	val _ = output_code(class_name, ".", (header_progs @ 
					      outputdatastruct_progs @ 
					      outputstatestruct_progs @
					      outputinit_progs @ 
					      input_progs @ 
					      init_progs @ 
					      flow_progs @ 
					      logoutput_progs @
					      exec_progs @
					      main_progs))
    in
	SUCCESS
    end
    handle e => DynException.checkpoint "CWriter.buildC" e

end
