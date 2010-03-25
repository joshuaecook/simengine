structure CompilerLib =
struct

val TypeMismatch = DynException.TypeMismatch
and IncorrectNumberOfArguments = DynException.IncorrectNumberOfArguments

exception Aborted

datatype phase = TRANSLATION | COMPILATION | CODEGENERATION
exception CompilationError of phase
exception CompilationFailure of phase

fun log str = if DynamoOptions.isFlagSet "logdof" then 
		  Util.log str
	      else
		  Logger.log_notice (Printer.$ str)

fun std_compile exec args =
    (case args of
	 [object] => 
	 ((let
	       val _ = Profile.mark()

	       (* Translation Phase *)
	       val forest as (_,{classname=name,...},_) = 
		   case Compile.dslObjectToDOF (exec, object) of
		       (f, Compile.SUCCESS) => f
		     | (_, Compile.USERERROR) => raise (CompilationError TRANSLATION)
		     | (_, Compile.EXCEPTION) => raise (CompilationFailure TRANSLATION)
	       val _ = Profile.mark()

	      (* Compilation Phase *)
	      val forkedModels = 
		  case Compile.DOFToShardedModel forest of
		       (f, Compile.SUCCESS) => f
		     | (_, Compile.USERERROR) => raise (CompilationError COMPILATION)
		     | (_, Compile.EXCEPTION) => raise (CompilationFailure COMPILATION)
	       val _ = Profile.mark()

	      (* Code Generation Phase *)
	      val () = case Compile.ShardedModelToCodeGen (name, forkedModels) of
			   Compile.SUCCESS => ()
			 | Compile.USERERROR => raise (CompilationError CODEGENERATION)
			 | Compile.EXCEPTION => raise (CompilationFailure CODEGENERATION)
	       val _ = Profile.mark()

	  in 
	       error_code 0
	  end)
	  handle CompilationError TRANSLATION => error_code 1
	       | CompilationError COMPILATION => error_code 2
	       | CompilationError CODEGENERATION => error_code 3
	       | CompilationFailure TRANSLATION => error_code 4
	       | CompilationFailure COMPILATION => error_code 5
	       | CompilationFailure CODEGENERATION => error_code 6
	       | DynException.InternalFailure => error_code 7)        
       | _ => raise IncorrectNumberOfArguments {expected=1, actual=(length args)})

and error_code code = KEC.LITERAL(KEC.CONSTREAL (Real.fromInt code))

val std_compile = Profile.timeTwoCurryArgs "Model Compiling" std_compile


fun std_transExp exec args =
    (case args of
	 [object] => valOf(ModelTranslate.reverseExp (exec, valOf (ModelTranslate.translateExp(exec, object))))

       | _ => raise IncorrectNumberOfArguments {expected=1, actual=(length args)})
    handle e => DynException.checkpoint "CompilerLib.std_transExp" e

fun std_addRules exec args =
    (case args of
	 [categoryname, rules] => 
	 (case ModelTranslate.rules2rewriterules (exec, rules) of
	      SOME rules =>
	      (Rules.addRules (ModelTranslate.exp2str categoryname, rules);
	       KEC.UNIT)
	    | NONE => KEC.UNIT)
       | _ => raise IncorrectNumberOfArguments {expected=2, actual=(length args)})
    handle e => DynException.checkpoint "CompilerLib.std_addRules" e


fun std_expcost exec args =
    (case args of
	 [exp] => 
	 let
	     val exp = valOf (ModelTranslate.translateExp(exec, exp))
	 in
	     KEC.LITERAL(KEC.CONSTREAL (Real.fromInt (Cost.exp2cost exp)))
	 end
       | _ => raise IncorrectNumberOfArguments {expected=1, actual=(length args)})
    handle e => DynException.checkpoint "CompilerLib.std_expcost" e

fun std_applyRewriteExp exec args =
    (case args of
	 [rewrite, exp] => valOf(ModelTranslate.reverseExp (exec, Match.applyRewriteExp (valOf (ModelTranslate.rule2rewriterule (exec, rewrite)))
											(valOf (ModelTranslate.translateExp(exec, exp)))))

       | _ => raise IncorrectNumberOfArguments {expected=2, actual=(length args)})
    handle e => DynException.checkpoint "CompilerLib.std_applyRewriteExp" e

fun std_applyRewritesExp exec args =
    (case args of
	 [KEC.LITERAL (KEC.CONSTSTR rulecategory), exp] =>
	 let
	     val _ = print ("in applyRewritesExp\n")
	     val rules = Rules.getRules rulecategory
	     val _ = print ("got rules\n")
	     val exp = valOf (ModelTranslate.translateExp(exec, exp))
	     val _ = print ("got exp\n")
	 in
	     valOf(ModelTranslate.reverseExp (exec, Match.applyRewritesExp rules exp))
	     before print ("done\n")
	 end
       | [rewrite, exp] => valOf(ModelTranslate.reverseExp (exec, Match.applyRewritesExp (valOf (ModelTranslate.rules2rewriterules (exec, rewrite)))
											 (valOf (ModelTranslate.translateExp(exec, exp)))))

       | _ => raise IncorrectNumberOfArguments {expected=2, actual=(length args)})
    handle e => DynException.checkpoint "CompilerLib.std_applyRewritesExp" e

fun std_repeatApplyRewriteExp exec args =
    (case args of
	 [rewrite, exp] => valOf(ModelTranslate.reverseExp (exec, (Match.repeatApplyRewriteExp (valOf (ModelTranslate.rule2rewriterule (exec, rewrite)))
											       (valOf (ModelTranslate.translateExp(exec, exp))))))

       | _ => raise IncorrectNumberOfArguments {expected=2, actual=(length args)})
    handle e => DynException.checkpoint "CompilerLib.std_repeatApplyRewriteExp" e

fun std_repeatApplyRewritesExp exec args =
    (case args of
	 [KEC.LITERAL (KEC.CONSTSTR rulecategory), exp] =>
	 let
	     val rules = Rules.getRules rulecategory
	     val exp = valOf (ModelTranslate.translateExp(exec, exp))
	 in
	     valOf(ModelTranslate.reverseExp (exec, Match.repeatApplyRewritesExp rules exp))
	 end
       | [rewrite, exp] => valOf(ModelTranslate.reverseExp (exec, Match.repeatApplyRewritesExp (valOf (ModelTranslate.rules2rewriterules (exec, rewrite)))
											 (valOf (ModelTranslate.translateExp(exec, exp)))))

       | _ => raise IncorrectNumberOfArguments {expected=2, actual=(length args)})
    handle e => DynException.checkpoint "CompilerLib.std_repeatApplyRewritesExp" e

fun std_collect exec args =
    (case args of
	 [KEC.LITERAL (KEC.CONSTSTR name), exp] =>
	 let
	     val exp = valOf (ModelTranslate.translateExp(exec, exp))
	 in
	     valOf(ModelTranslate.reverseExp (exec, ExpProcess.collect (ExpBuild.var name, exp)))
	 end
       | _ => raise IncorrectNumberOfArguments {expected=2, actual=(length args)})
    handle e => DynException.checkpoint "CompilerLib.std_repeatApplyRewritesExp" e

fun std_exp2str exec args =
    (case args of
	 [object] => KEC.LITERAL(KEC.CONSTSTR (ExpPrinter.exp2str (valOf (ModelTranslate.translateExp(exec, object)))))

       | _ => raise IncorrectNumberOfArguments {expected=1, actual=(length args)})
    handle e => DynException.checkpoint "CompilerLib.std_exp2str" e 

fun std_profile (exec:KEC.exp->KEC.exp) (args:KEC.exp list) : KEC.exp =
    (case args of
	 [KEC.LITERAL (KEC.CONSTSTR name), exp, arg] => 
	 let
	     val exec' = Profile.time name exec
	 in
	     exec' (KEC.APPLY {func = exp,
			      args = case arg of 
					 KEC.TUPLE entries => arg
				       | KEC.UNIT => KEC.TUPLE []
				       | _ => KEC.TUPLE [arg]})
	 end
       | [a, _, _] => raise TypeMismatch ("expected a string but received " ^ (PrettyPrint.kecexp2nickname a))
       | _ => raise IncorrectNumberOfArguments {expected=3, actual=(length args)})

val imports: string list ref = ref nil

fun toVector object =
    KEC.APPLY {func = KEC.SEND {message = Symbol.symbol "tovector",
				object = object},
	       args = KEC.UNIT}

fun getModelImports exec _ = exec (toVector (KEC.TUPLE (map (KEC.LITERAL o KEC.CONSTSTR) (List.rev (! imports)))))


fun loadModel exec args =
    case args
     of [KEC.LITERAL (KEC.CONSTSTR path)] =>
	let
	    val _ = imports := nil
	    fun importing path = imports := path :: (! imports)

	    val object = (KEC.SYMBOL o Symbol.symbol o OS.Path.base o OS.Path.file) path

	    val model = ImportHook.withImportHook importing (fn _ => 
			exec (KEC.STMS [KEC.ACTION (KEC.IMPORT path, PosLog.NOPOS),
					KEC.ACTION (KEC.ASSIGN (KEC.SEND {message = Symbol.symbol "imports",
	    								  object = KEC.SEND {message = Symbol.symbol "template", object = object}},
								KEC.LIBFUN (Symbol.symbol "getModelImports", KEC.UNIT)),
	    		    			    PosLog.NOPOS),
					KEC.ACTION (KEC.EXP object, PosLog.NOPOS)])
							    ) 
	in
	    model
	end
      | [a] => raise TypeMismatch ("expected a string but received " ^ (PrettyPrint.kecexp2nickname a))
      | args => raise IncorrectNumberOfArguments {expected=1, actual=(length args)}

val loadModel = Profile.timeTwoCurryArgs "Model Loading" loadModel

fun simfileSettings exec args =
    case args
     of [KEC.LITERAL (KEC.CONSTSTR path)] =>
	Simex.withSimengine path (fn simengine =>
	let val api = Simex.api simengine
	    val newTable = KEC.SEND {object = KEC.SYMBOL (Symbol.symbol "Table"), 
				     message = Symbol.symbol "new"}

	    val keys = ["target", "precision", "parallel_models", "version"]
	    val values = 
		[KEC.LITERAL (KEC.CONSTSTR (Simex.API.target api)),
		 KEC.LITERAL (KEC.CONSTSTR (case Simex.API.precision api
					     of Simex.API.Double => "double"
					      | Simex.API.Single => "float")),
		 KEC.LITERAL (KEC.CONSTREAL (Real.fromInt (Simex.API.parallelModels api))),
		 KEC.LITERAL (KEC.CONSTREAL (Real.fromInt (Simex.API.version api)))]

	    val entries = KEC.list2kecvector
			      (ListPair.map (fn (k,v) => KEC.TUPLE [KEC.LITERAL (KEC.CONSTSTR k), v]) (keys, values))
	in
	    exec (KEC.APPLY {func = newTable,
			     args = KEC.TUPLE [entries]})
	end)
      | [a] => raise TypeMismatch ("expected a string but received " ^ (PrettyPrint.kecexp2nickname a))
      | args => raise IncorrectNumberOfArguments {expected=1, actual=(length args)}

fun settingsHelp exec args =
    case args 
     of nil => KEC.LITERAL (KEC.CONSTSTR (DynamoOptions.optionsdescription "simEngine"))
      | args => raise IncorrectNumberOfArguments {expected=0, actual=(length args)}

fun logSettings exec args =
    case args 
     of nil => (DynamoOptions.logSettings();
		KEC.UNIT)
      | args => raise IncorrectNumberOfArguments {expected=0, actual=(length args)}


val library = [{name="compile", operation=std_compile},
	       {name="loadModel", operation=loadModel},
	       {name="profileTime", operation=std_profile},
	       {name="logSettings", operation=logSettings},
	       {name="getModelImports", operation=getModelImports},
	       {name="simfileSettings", operation=simfileSettings},
	       {name="settingsHelp", operation=settingsHelp},
	       {name="transexp", operation=std_transExp},
	       {name="exp2str", operation=std_exp2str},
	       {name="addRules", operation=std_addRules},
	       {name="expcost", operation=std_expcost},
	       {name="applyRewriteExp", operation=std_applyRewriteExp},
	       {name="applyRewritesExp", operation=std_applyRewritesExp},
	       {name="repeatApplyRewriteExp", operation=std_repeatApplyRewriteExp},
	       {name="repeatApplyRewritesExp", operation=std_repeatApplyRewritesExp},
	       {name="collect", operation=std_collect}]

end
