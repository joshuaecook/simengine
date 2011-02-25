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
    val top_class : unit -> DOF.class

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

(* Exceptions *)
val NoClassFound = DynException.NoClassFound
val NoIteratorFound = DynException.NoIteratorFound

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
	    raise NoClassFound (sym, (map #name myclasses))
(*DynException.stdException(("Can't find class with name '"^(Symbol.name sym)^"': ClassList=" ^(Util.symlist2s (map #name myclasses))),
					  ("CurrentModel.classname2class"), Logger.INTERNAL)*)
    end


fun top_class() = 
    let
	val {classname, ...} = top_inst()
    in
	classname2class(classname)
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
      | NONE => 
	raise NoIteratorFound (iter_sym)
(*DynException.stdException(("Can't find iterator with name '"^(Symbol.name iter_sym)^"'"),
					  ("CurrentModel.itersym2iter"), Logger.INTERNAL)*)

end
