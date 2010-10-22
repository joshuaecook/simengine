signature NORMALIZE = 
sig
val normalize: Exp.exp -> Exp.exp (* flatten sequences and normalize lambdas with applications *)

end

structure Normalize : NORMALIZE =
struct

val head = ExpTraverse.head
val level = ExpTraverse.level

fun normalize_with_env table exp =
    let
	fun expFlatMap f exps =
	    Util.flatmap 
		((fn Exp.META (Exp.SEQUENCE xs) => xs | x => [x]) o f)
		exps
    in
	case exp of
	    Exp.FUN (f, args) => 
	    Exp.FUN (f, expFlatMap (normalize_with_env table) args)
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
	    in
	    Exp.CONTAINER 
	    (case c of
		 Exp.EXPLIST l => Exp.EXPLIST (expFlatMap (normalize_with_env table) l)
	       | Exp.ARRAY a => Exp.ARRAY (normalize_array a)
	       | Exp.ASSOC t => Exp.ASSOC (SymbolTable.map (normalize_with_env table) t)
	       | Exp.MATRIX m => Exp.MATRIX 
				     (Container.expMatrixToMatrix 
					  ((head exp) (map (normalize_with_env table) (level exp))))
	    )
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
		   | Exp.SEQUENCE exps => Exp.META(Exp.SEQUENCE(expFlatMap (normalize_with_env table) exps)))
	    end
	  | Exp.SUBREF (exp', subspace) =>
	    Exp.SUBREF (normalize_with_env table exp', subspace)
    end

fun normalize exp = normalize_with_env SymbolTable.empty exp


val normalize = Profile.wrap (normalize, Profile.alloc "Normalize.normalize")

end
