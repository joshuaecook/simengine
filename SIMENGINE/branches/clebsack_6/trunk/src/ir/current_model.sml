(* CurrentModel: Stores one DOF.model definition in memory *)
signature CURRENTMODEL =
sig

    (* Default empty model *)
    val empty_model : DOF.model

   (* Primary accessor and modifier *) 
    val getCurrentModel: unit -> DOF.model
    val setCurrentModel: DOF.model -> unit

    (* Temporarily holds the given model as current, 
     * invokes the given thunk and returns its result
     * after restoring the previous current model. 
     * Any exception raised by the thunk will be reraised 
     * after the previous model is restored. *)
    val withModel : DOF.model -> (unit -> 'a) -> 'a

    (* Additional useful accessors *)
    val classes: unit -> DOF.class list
    val iterators: unit -> DOF.systemiterator list (* returns temporal iterators of the model *)
    val top_inst: unit -> DOF.instance

    (* Useful functions *)
    val classname2class: Symbol.symbol -> DOF.class (* Searches for a class with the given name in the current model *)
    val itersym2iter: Symbol.symbol -> DOF.systemiterator (* Searches for the matching iterator *)

end
structure CurrentModel : CURRENTMODEL =
struct

(* current model *)
val empty_model:DOF.model
  = ([], 
     {name=NONE,classname=Symbol.symbol "empty"}, 
     {iterators=[(Symbol.symbol "t", DOF.CONTINUOUS Solver.default),
		 (Symbol.symbol "n", DOF.DISCRETE {sample_period=1.0})],
      precision=DOF.DOUBLE,
      target=Target.CPU,
      parallel_models=1,
      debug=false,
      profile=false})

val current_model = (ref empty_model: DOF.model ref)

(* accessors/modifiers for the current model *)
fun getCurrentModel() = (!current_model)
fun setCurrentModel(model: DOF.model) = 
    current_model := model

fun withModel model f =
    let val old = getCurrentModel () before setCurrentModel model
    in 
	f () before setCurrentModel old
	handle e => (setCurrentModel old; raise e)
    end

(* accessors for the top level fields *)
fun classes() = 
    let
	val (classes, instance, props) = (!current_model)
    in
	classes
    end

fun top_inst() = 
    let
	val (classes, instance, props) = (!current_model)
    in
	instance
    end

fun top_name() =
    let
	val {name, classname} = top_inst()
    in
	case name of
	    SOME v => Symbol.name v
	  | NONE => "UNKNOWN"
    end

fun classname2class(sym) : DOF.class = 
    let
	val myclasses = classes ()
	(* First look for a class with a matching name. *)
	val found = List.find (fn c => sym = #name c) myclasses
    in
	if isSome found then valOf found
	else
		DynException.stdException(("Can't find class with name '"^(Symbol.name sym)^"': ClassList=" ^(Util.symlist2s (map #name myclasses))),
					  ("CurrentModel.classname2class"), Logger.INTERNAL)
    end


fun iterators() =
    let 
	val (_,_,{iterators,...}) = !current_model
    in
	iterators
    end
				
fun itersym2iter iter_sym =
    case List.find (fn(iter_sym',_)=>iter_sym=iter_sym') (iterators()) of
	SOME iter => iter
      | NONE => DynException.stdException(("Can't find iterator with name '"^(Symbol.name iter_sym)^"'"),
					  ("CurrentModel.itersym2iter"), Logger.INTERNAL)

end
