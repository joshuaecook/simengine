(*
Copyright (C) 2011 by Simatra Modeling Technologies

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.

*)

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
	val _ = Profile.time "Validating model ..." ModelValidate.validate forest
	val _ = DynException.checkToProceed()
	val _ = Profile.mark()

	(* if the optimize flag is set, spend some time trying to reduce the equations *)
	val _ = if DynamoOptions.isFlagSet "optimize" then
		    (log ("Optimizing model ...");
		     ModelProcess.optimizeModel false (model()); (* don't perform ordering *)
		     DOFPrinter.printModel(model()))
		else
		    ()
	val _ = Profile.mark()

	val _ = log("Normalizing model ...")
	val _ = ModelProcess.normalizeModel (model())
	val _ = Profile.mark()

	(* if profiling, put some additional debugging information prior to shard generation - this will show 
	 * whether or not we were able to reduce the cost of the evaluation *)
	val _ = if DynamoOptions.isFlagSet "verbose" then
		    Cost.logModelCosts (model())
		else
		    ()

	(* check license after all the processing is performed to make sure that the model is acceptable *)
	val _ = ModelValidate.validateLicensing (model())

	val _ = log("Normalizing parallel model ...")
	val forkedModels = Profile.time "Forking model" ShardedModel.forkModel (model())
	val _ = Profile.mark()

	val forkedModels = if DynamoOptions.isFlagSet "aggregate" then
			       let
				   val _ = log("Aggregating iterators ...")
				   val forkedModels' = Profile.time "Aggregating iterators" ShardedModel.combineDiscreteShards forkedModels
			       in
				   forkedModels'
			       end
			   else
			       forkedModels

	(* perform another pass of optimizations *)
	val forkedModels = 
	    if DynamoOptions.isFlagSet "optimize" then
		let
		    val (shards, sysprops) = forkedModels
		    fun toModel {classes, instance, ...} = (classes, instance, sysprops)
		    val _ = log ("Optimizing model ...")
		    val _ = app 
				(fn(shard) => 
				   (CurrentModel.withModel (toModel shard)
							   (fn() => Profile.timeTwoCurryArgs 
									"Optimizing shard" 
									ModelProcess.optimizeModel true (toModel shard)))) (* order after *)
				shards
		    val _ = ShardedModel.printShardedModel "After optimization" forkedModels
		    val _ = log ("Refreshing system properties ...")
		    val forkedModels' = ShardedModel.refreshSysProps forkedModels
		    val _ = ShardedModel.printShardedModel "After refreshing sys props" forkedModels'
		in
		    forkedModels'
		end
	    else
		forkedModels
	val _ = Profile.mark()

	val _ = log ("Ordering model classes ...")
	val forkedModels =
	    let 
		val (shards, sysprops) = forkedModels
		val shards' = Profile.time "Ordering shard"
					   (fn()=>map (fn (shard as {classes,instance,...}) => 
							  ShardedModel.orderShard ((classes,instance,sysprops),shard)) shards) ()
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
