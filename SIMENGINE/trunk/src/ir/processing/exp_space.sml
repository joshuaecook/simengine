signature EXPSPACE =
sig
    
    (* given an exp, return the space *)
    val expToSpace : Exp.exp -> Space.space

end
structure ExpSpace : EXPSPACE =
struct

local
    open Space

    fun termToReal (Exp.REAL v) = v
      | termToReal (Exp.INT v) = Real.fromInt v
      | termToReal t = (Logger.log_error (Printer.$("Can't convert term "^(ExpPrinter.exp2str (Exp.TERM t))^" to real"));
			DynException.setErrored();
			0.0)

    val head = ExpTraverse.head
    val level = ExpTraverse.level

in
fun expToSpace exp =
    case exp of
	Exp.TERM t => (case t of
			   Exp.RATIONAL _ => scalar
			 | Exp.INT _ => scalar
			 | Exp.REAL _ => scalar
			 | Exp.BOOL _ => scalar
			 | Exp.COMPLEX _ => scalar
			 | Exp.TUPLE tuple => tensor [length tuple]
			 | Exp.RANGE {low, high, step} => 
			   let
			       val numElements = ((termToReal high)-(termToReal low))/(termToReal step)+1.0
			   in
			       tensor [Real.ceil numElements]
			   end
			 | Exp.RANDOM (_, space) => space
			 | Exp.SYMBOL (_, props) => Property.getSpace props
			 | Exp.DONTCARE => scalar (* this is kind of unknown *)
			 | Exp.INFINITY => scalar
			 | Exp.NAN => scalar
			 | Exp.PATTERN _ => scalar (* let's just support scalars here *)
			 | Exp.STRING _ => scalar)
      | Exp.FUN (f,_) => 
	let
	    val codomain = #codomain (FunProcess.fun2props f)
	in
	    codomain (map expToSpace (level exp))
	end
      | Exp.CONTAINER c => (case c of
				Exp.MATRIX m => fromMatrixDims (Matrix.size m)
			      | Exp.ARRAY a => fromVectorDim (Array.length a)
			      | Exp.ASSOC table => collection (map expToSpace (SymbolTable.listItems table))
			      | Exp.EXPLIST exps => collection (map expToSpace exps))
      | Exp.SUBREF (exp', subspace) => sub (expToSpace exp') subspace
      | Exp.META _ => scalar (* have no idea what to do here... *)

end

end
