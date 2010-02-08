signature NORMALIZE = 
sig
val normalize: Exp.exp -> Exp.exp (* flatten sequences and normalize lambdas with applications *)

end

structure Normalize : NORMALIZE =
struct

fun normalize_with_env table exp =
    let
	fun flatten exps =
	    let
		fun combine (Exp.META(Exp.SEQUENCE s), exps) =
		    exps @ s
		  | combine (exp, exps) =
		    exps @ [exp]
	    in
		foldl combine nil exps
	    end
    
    in
	case exp of
	    Exp.FUN (f, args) => 
	    Exp.FUN (f, flatten(map (normalize_with_env table) args))
	  | Exp.TERM t => 
	    (case t of 
		 Exp.SYMBOL (s, _) =>
		 (case SymbolTable.look (table, s) of
		      NONE => exp
		    | SOME e => e)
	       | _ => exp)
	  | Exp.CONTAINER c =>
	    let
		fun normalize_array a =
		    let
			val l = Container.arrayToList a
			val l' = map (normalize_with_env table) l
		    in
			Container.listToArray l'
		    end
		fun normalize_matrix m =
		    let
			val arrays = Matrix.toRows m
			val arrays' = map normalize_array arrays
		    in
			Matrix.fromRows (Exp.calculus ()) arrays'
		    end
	    in
	    Exp.CONTAINER 
	    (case c of
		 Exp.EXPLIST l => Exp.EXPLIST (flatten (map (normalize_with_env table) l))
	       | Exp.ARRAY a => Exp.ARRAY (normalize_array a)
	       | Exp.MATRIX m => Exp.MATRIX (normalize_matrix m))
	    end
	  | Exp.META m =>
	    let
		fun run argsym exp arg =
		    let
			val table' = SymbolTable.enter (table, argsym, arg)
		    in 
			normalize_with_env table' exp
		    end
	    in
		(case m of
		     Exp.APPLY {func, arg} =>
		     (case normalize_with_env table func of
			  Exp.META(Exp.LAMBDA {arg=argsym, body}) 
			  => 
			  run argsym body arg 
			| _ => exp)

		   | Exp.MAP {func, args} =>
		     (case normalize_with_env table func of
			  Exp.META(Exp.LAMBDA {arg=argsym, body}) 
			  => 
			  (case normalize_with_env table args of
			       Exp.META(Exp.SEQUENCE exps) =>
			       Exp.META(Exp.SEQUENCE (map (run argsym body) exps))
			     | arg => run argsym body arg)
			| _ => exp)				
     
		   | Exp.LAMBDA _ =>
		     exp
		   | Exp.SEQUENCE exps => Exp.META(Exp.SEQUENCE(flatten(map (normalize_with_env table) exps))))
	    end
    end

fun normalize exp = normalize_with_env SymbolTable.empty exp


end
