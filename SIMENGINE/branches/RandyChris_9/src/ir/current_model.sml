(* CurrentModel: Stores one DOF.model definition in memory *)
signature CURRENTMODEL =
sig

   (* Primary accessor and modifier *) 
    val getCurrentModel: unit -> DOF.model
    val setCurrentModel: DOF.model -> unit

    (* Additional useful accessors *)
    val classes: unit -> DOF.class list
    val iterators: unit -> DOF.systemiterator list (* returns temporal iterators of the model *)
    val top_inst: unit -> DOF.instance

    (* Useful functions *)
    val classname2class: Symbol.symbol -> DOF.class (* Searches for a class with the given name in the current model *)

end
structure CurrentModel : CURRENTMODEL =
struct

(* current model *)
val empty_model:DOF.model
  = ([], 
     {name=NONE,classname=Symbol.symbol "empty"}, 
     {iterators=[(Symbol.symbol "t", DOF.CONTINUOUS Solver.default),
		 (Symbol.symbol "n", DOF.DISCRETE {fs=1.0})],time=(0.0,0.0),precision=DOF.DOUBLE})

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
    case List.find (fn(class)=> 
		      #name class = sym (* this is the first option *) 
		      orelse
		      (case #classtype (#properties class) of
			   DOF.MASTER sym' => sym = sym' (* these are accepted since they just had to be renamed *)
			 | DOF.SLAVE _ => false (* we are not matching slave classes *))		      
		   ) (classes())
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
