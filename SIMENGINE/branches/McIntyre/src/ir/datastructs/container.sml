signature CONTAINER =
sig

val vector2list : Exp.exp Vector.vector -> Exp.exp list
val list2vector : Exp.exp list -> Exp.exp Vector.vector
val matrix2vectors : Exp.exp Array2.array -> Exp.exp Vector.vector list
val vectors2matrix : Exp.exp Vector.vector list -> Exp.exp Array2.array
val listexp2listvector : Exp.exp list -> Exp.exp Vector.vector list

end
structure Container:CONTAINER =
struct

fun vector2list v = map 
			(fn(i)=>Vector.sub (v,i)) 
			(List.tabulate (Vector.length v, (fn(x)=>x)))
fun list2vector l = Vector.fromList l

fun matrix2vectors m = map
			   (fn(i)=>Array2.row (m, i))
			   (List.tabulate (Array2.nRows m, (fn(x)=>x)))
fun vectors2matrix vectors = 
    let
	val lists = map vector2list vectors
    in
	Array2.fromList lists
    end

fun listexp2listvector l =
    let
	fun exp2vector (Exp.CONTAINER (Exp.VECTOR v)) = v
	  | exp2vector _ = 
	    DynException.stdException("Unexpected expression",
				      "Container.listexp2listvectors.exp2vector",
				      Logger.INTERNAL)
    in
	map exp2vector l
    end

end
