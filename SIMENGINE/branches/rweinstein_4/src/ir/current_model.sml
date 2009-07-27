structure CurrentModel =
struct

(* current model *)
val empty_model:DOF.model
  = ([], 
     {name=NONE,classname=Symbol.symbol "empty"}, 
     {iterators=nil,time=(0.0,0.0),precision=DOF.DOUBLE})

val current_model = (ref empty_model: DOF.model ref)

(* accessors/modifiers for the current model *)
fun getCurrentModel() = (!current_model)
fun setCurrentModel(model: DOF.model) = 
    current_model := model

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
    case List.find (fn(class)=> #name class = sym) (classes())
     of SOME v => v
      | NONE => DynException.stdException(("Can't find class with name '"^(Symbol.name sym)^"'"),
					  ("CurrentModel.classname2class"), Logger.INTERNAL)
fun iterators() =
    let 
	val (_,_,{iterators,...}) = !current_model
    in
	iterators
    end
				

end
