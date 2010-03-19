signature COMPILE =
sig

    datatype status = SUCCESS | USERERROR | EXCEPTION

    val dslObjectToDOF : ((KEC.exp -> KEC.exp) * KEC.exp) -> (DOF.model * status)
    val DOFToShardedModel : DOF.model -> (ShardedModel.shardedModel * status)
    val ShardedModelToCodeGen : (Symbol.symbol * ShardedModel.shardedModel) -> status

end
structure Compile : COMPILE =
struct

datatype status = SUCCESS | USERERROR | EXCEPTION

(* logger function *)
fun log str = if DynamoOptions.isFlagSet "logdof" then 
		  Util.log str
	      else
		  Logger.log_notice (Printer.$(str))


(* dslObjectToDOF: convert the DSL object representation into a DOF representation *)
fun dslObjectToDOF (exec, object) =
    let
	val (forest, stat) = case ModelTranslate.translate (exec, object) of
				 SOME f => (f, SUCCESS)
			       | NONE => (CurrentModel.empty_model, USERERROR)
	val _ = DynException.checkToProceed()
    in 
	(forest, stat)
    end
    handle DynException.TooManyErrors => (CurrentModel.empty_model, USERERROR)
	 | DynException.InternalError _ => (CurrentModel.empty_model, EXCEPTION)

val dslObjectToDOF = Profile.time "Translation" dslObjectToDOF


(* DOFToShardedModel: compile the DOF representation, perform optimizations, create a sharded model representation *)
fun DOFToShardedModel forest = 
    let
	(* Perform a debugging step *)
	val _ = DOFPrinter.printModel forest

	(* always grab the latest model *)
	fun model() = CurrentModel.getCurrentModel()

	(* initially set the current model global variable *)
	val _ = CurrentModel.setCurrentModel forest

	(* here, we can validate the model to catch issues that can't be found elsewhere *)
	val _ = ModelValidate.validate forest
	val _ = DynException.checkToProceed()

	(* if the optimize flag is set, spend some time trying to reduce the equations *)
	val _ = if DynamoOptions.isFlagSet "optimize" then
		    (log ("Optimizing model ...");
		     ModelProcess.optimizeModel (model());
		     DOFPrinter.printModel(model()))
		else
		    ()

	val _ = log("Normalizing model ...")
	val _ = ModelProcess.normalizeModel (model())

	(* check license after all the processing is performed to make sure that the model is acceptable *)
	val _ = ModelValidate.validateLicensing (model())

	val _ = log("Normalizing parallel model ...")
	val forkedModels = ShardedModel.forkModel (model())

	val forkedModels = if DynamoOptions.isFlagSet "aggregate" then
			       let
				   val _ = log("Aggregating iterators ...")
				   val forkedModels' = ShardedModel.combineDiscreteShards forkedModels
			       in
				   forkedModels'
			       end
			   else
			       forkedModels

	(* perform another pass of optimizations *)
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
				      ShardedModel.orderShard ((classes,instance,sysprops),shard)) shards
	    in 
		(shards', sysprops) 
	    end

    in
	(forkedModels, SUCCESS)
    end
    handle DynException.TooManyErrors => (ShardedModel.empty_shardedModel, USERERROR)
	 | DynException.InternalError _ => (ShardedModel.empty_shardedModel, EXCEPTION)

val DOFToShardedModel = Profile.time "Compilation" DOFToShardedModel

fun ShardedModelToCodeGen (name, shardedModel) =
    let
	val stat = case CParallelWriter.buildC (name, shardedModel) of
	 CParallelWriter.SUCCESS => SUCCESS
       | CParallelWriter.FAILURE _ => EXCEPTION (* should never occur *)

	val _ = DynException.checkToProceed()
    in
	stat
    end
    handle DynException.TooManyErrors => USERERROR
	 | DynException.InternalError _ => EXCEPTION

val ShardedModelToCodeGen = Profile.time "Code Generation" ShardedModelToCodeGen

end
