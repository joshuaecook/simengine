structure CompilerLib =
struct

val TypeMismatch = DynException.TypeMismatch
and IncorrectNumberOfArguments = DynException.IncorrectNumberOfArguments

exception Aborted

fun log str = if DynamoOptions.isFlagSet "logdof" then 
		  Util.log str
	      else
		  Logger.log_notice (Printer.$ str)

(* FIXME this is really ugly and shouldn't be in this file. *)
(* Ensures that classes within a shard model are in dependency order. *)
fun orderShard (model, shard as {classes, instance as {classname,...}, iter_sym}) =
    let val _ = ()
	val topClass = CurrentModel.withModel model (fn _ => CurrentModel.classname2class classname)
	val unordered = List.filter (fn c => classname <> ClassProcess.class2classname c) classes

	(* TODO need to filter through uniq? *)
	val orderedClasses = orderClasses (model, topClass, unordered)
    in
	{classes = rev orderedClasses, instance = instance, iter_sym = iter_sym}
    end

and orderClasses (model, topClass, nil) = [topClass]
  | orderClasses (model, topClass, unordered) =
    let val (instanceClassNames,_) = ListPair.unzip (ClassProcess.class2instnames' topClass)
	val (instanceClasses, unordered') = 
	    List.partition (fn c => List.exists (fn cn => ClassProcess.class2classname c = cn) instanceClassNames) unordered
    in
	topClass :: (List.concat (map (fn c => orderClasses (model, c, unordered')) instanceClasses))
    end



fun std_compile exec args =
    (case args of
	 [object] => 
	 (let
	      val dslname = exec (KEC.SEND {message = Symbol.symbol "name",
					    object = KEC.SEND {message = Symbol.symbol "modeltemplate",
							       object = object}})
	      val name = case dslname
			  of KEC.LITERAL (KEC.CONSTSTR str) => str
			   | _ => raise Aborted

(*
	      val _ = 
		  (let val sim = Simex.new (name ^ ".sim")
		       val api = Simex.api sim
		   in
		       print ("Found compiled sim named " ^ (Simex.API.name api) ^ "\n")
		   end)
		  handle Fail why => print ("Unable to reuse " ^ (name ^ ".sim") ^ ": " ^ why ^ "\n")
*)
			  


	      val _ = if DynException.isErrored() then
			  raise Aborted
		      else
			  ()

	      val _ = log "Translating model ..."
	      val forest = case ModelTranslate.translate(exec, object) of
				       SOME f => f
				     | NONE => raise Aborted
						  
	      val _ = DynException.checkToProceed()

	      val (classes, {classname,...}, _) = forest

	      val _ = DOFPrinter.printModel forest

	      val _ = CurrentModel.setCurrentModel forest

	      val () = 
		  if ModelProcess.isDebugging (CurrentModel.getCurrentModel ()) then
		      PrintJSON.printFile ("dof.json", ModelSyntax.toJSON (CurrentModel.getCurrentModel ()))
		  else ()

	      val _ = if DynamoOptions.isFlagSet "optimize" then
			  (log ("Optimizing model ...");
			   ModelProcess.optimizeModel (CurrentModel.getCurrentModel());
			   DOFPrinter.printModel(CurrentModel.getCurrentModel()))
(*			  handle e => (app (fn(s) => print("    " ^ s ^ "\n")) (MLton.Exn.history e))*)
		      else
			  ()

	      val _ = log("Normalizing model ...")
	      val _ = ModelProcess.normalizeModel (CurrentModel.getCurrentModel())

	      val _ = log("Normalizing parallel model ...")
	      val forkedModels = ShardedModel.forkModel (CurrentModel.getCurrentModel())

	      val _ = if DynamoOptions.isFlagSet "optimize" then
			  let
			      val (shards, sysprops) = forkedModels
			      fun toModel {classes, instance, ...} = (classes, instance, sysprops)
			  in
			      (log ("Optimizing model ...");
			       app 
				   (fn(shard) => 
				      (CurrentModel.withModel (toModel shard)
							      (fn() => ModelProcess.optimizeModel (toModel shard)))) 
				   shards)
			  end
		      else
			  ()

	      val _ = log ("Ordering model classes ...")
	      val forkedModels =
		  let 
		      val (shards, sysprops) = forkedModels
		      val shards' = map (fn (shard as {classes,instance,...}) => 
                          orderShard ((classes,instance,sysprops),shard)) shards
		  in 
		      (shards', sysprops) 
		  end

(*	      val _ = log("Ready to build the following DOF ...")*)
	      val _ = log("Ready to build ...")
(*	      val _ = DOFPrinter.printModel (CurrentModel.getCurrentModel())*)

	      val () = 
		  if ModelProcess.isDebugging (CurrentModel.getCurrentModel()) then
		      PrintJSON.printFile ("dof-final.json", ModelSyntax.toJSON (CurrentModel.getCurrentModel ()))
		  else ()


	      local 
		  open JSON open JSONExtensions
		  fun JSONSymbol (sym) =
		      object [("$symbol", string (Symbol.name sym))]

		  fun shardToJSON {classes, instance as {name, classname}, iter_sym} =
		      object [("classes", array (map ClassSyntax.toJSON classes)),
			      ("instance", object [("classname", JSONSymbol classname),
						   ("name", JSONOption (JSONSymbol, name))]),
			      ("iterator", JSONSymbol iter_sym)]
		  val (shards, sysprops) = forkedModels
	      in
	      val () =
		  if ModelProcess.isDebugging (CurrentModel.getCurrentModel()) then
		      PrintJSON.printFile ("dof-system.json",
					   object [("classname", JSONSymbol classname),
						   ("properties", ModelSyntax.propertiesToJSON sysprops),
						   ("shards", array (map shardToJSON shards))])
		  else ()		  
	      end

	      val code = CParallelWriter.buildC (classname, forkedModels)
(*	      val code = CWriter.buildC(CurrentModel.getCurrentModel())*)

	      val _ = DynException.checkToProceed()
	  in 
	      case code of
		  CParallelWriter.SUCCESS => KEC.LITERAL(KEC.CONSTSTR "\nCompilation Finished Successfully\n")
		| CParallelWriter.FAILURE f => KEC.LITERAL(KEC.CONSTSTR ("\nFailure: " ^ f ^ "\n"))
	  end 
	  handle Aborted => KEC.LITERAL(KEC.CONSTSTR ("\nFailure: Compilation stopped due to errors\n"))
	       | TooManyErrors => KEC.LITERAL(KEC.CONSTSTR ("\nFailure: Compilation stopped due to too many errors\n")))
       | _ => raise IncorrectNumberOfArguments {expected=1, actual=(length args)})
    handle e => DynException.checkpoint "CompilerLib.std_compile" e

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

fun loadModel exec args =
    case args
     of [KEC.LITERAL (KEC.CONSTSTR path)] =>
	exec (KEC.STMS [KEC.ACTION (KEC.IMPORT path, PosLog.NOPOS),
			KEC.ACTION ((KEC.EXP o KEC.SYMBOL o Symbol.symbol o OS.Path.base o OS.Path.file) path, PosLog.NOPOS)])
      | [a] => raise TypeMismatch ("expected a string but received " ^ (PrettyPrint.kecexp2nickname a))
      | args => raise IncorrectNumberOfArguments {expected=1, actual=(length args)}

fun simfileSettings exec args =
    case args
     of [KEC.LITERAL (KEC.CONSTSTR path)] =>
	Simex.withSimengine path (fn simengine =>
	let val api = Simex.api simengine
	    val newTable = KEC.SEND {object = KEC.SYMBOL (Symbol.symbol "Table"), 
				     message = Symbol.symbol "new"}

	    val keys = ["target", "precision", "num_models", "version"]
	    val values = 
		[KEC.LITERAL (KEC.CONSTSTR (Simex.API.target api)),
		 KEC.LITERAL (KEC.CONSTSTR (case Simex.API.precision api
					     of Simex.API.Double => "double"
					      | Simex.API.Single => "float")),
		 KEC.LITERAL (KEC.CONSTREAL (Real.fromInt (Simex.API.numModels api))),
		 KEC.LITERAL (KEC.CONSTREAL (Real.fromInt (Simex.API.version api)))]

	    val entries = KEC.list2kecvector
			      (ListPair.map (fn (k,v) => KEC.TUPLE [KEC.LITERAL (KEC.CONSTSTR k), v]) (keys, values))
	in
	    exec (KEC.APPLY {func = newTable,
			     args = KEC.TUPLE [entries]})
	end)
      | [a] => raise TypeMismatch ("expected a string but received " ^ (PrettyPrint.kecexp2nickname a))
      | args => raise IncorrectNumberOfArguments {expected=1, actual=(length args)}

val library = [{name="compile", operation=std_compile},
	       {name="loadModel", operation=loadModel},
	       {name="simfileSettings", operation=simfileSettings},
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
