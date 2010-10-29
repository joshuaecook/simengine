signature EXPTRAVERSE =
sig
    
    (* offer the head and level operators to traverse through expressions
     * These utility functions are similar to Mathematica's Head[] and Level[] operators.  These
     * can be combined together to quickly traverse through an expression - notice that:
     * exp == (head exp)(level exp) 
     *)

    val head : Exp.exp -> (Exp.exp list -> Exp.exp) (* returns a function that will reapply the top 
						     * function of the expression *)
    val level : Exp.exp -> Exp.exp list (* returns the argument of the function *)

end
structure ExpTraverse : EXPTRAVERSE =
struct

(* utility function *)
val e2s = ExpPrinter.exp2str
fun exp2term (Exp.TERM t) = t
  | exp2term exp = DynException.stdException(("Unexpected non-term: " ^ (e2s exp)),"ExpTraverse.exp2term", Logger.INTERNAL)

(* common term rewriting commands *)
(* level - grab the next level of arguments *)
fun level (exp) =
    (case exp of 
	 Exp.FUN (_,args) => args
       | Exp.TERM (Exp.TUPLE termlist) => map Exp.TERM termlist
       | Exp.TERM (Exp.COMPLEX (a, b)) => map Exp.TERM [a,b]
       | Exp.TERM (Exp.RANGE {low, step, high}) => map Exp.TERM [low, step, high]
       | Exp.CONTAINER (Exp.EXPLIST l) => l
       | Exp.CONTAINER (Exp.ARRAY a) => Container.arrayToList a
       | Exp.CONTAINER (Exp.ASSOC tab) => SymbolTable.listItems tab
       | Exp.CONTAINER (Exp.MATRIX m) =>
	 (case !m of
	      Matrix.DENSE {data,...} => 	     
	      map 
		  (Exp.CONTAINER o Exp.ARRAY)
		  (Matrix.toRows m)
	   | Matrix.BANDED {data,calculus,...} =>
	     (#zero calculus)::(map Container.arrayToExpArray data))
       | Exp.SUBREF (exp, space) => [exp]
      | _ => [])
    handle e => DynException.checkpoint ("ExpTraverse.level ["^(e2s exp)^"]") e

(* this will return a function to rebuild the head *)
fun head (exp) =
    (case exp of
	Exp.FUN (funtype, args) => (fn(args')=> Exp.FUN (funtype, args'))
      | Exp.TERM (Exp.TUPLE (termlist)) => (fn(args') => Exp.TERM (Exp.TUPLE (map exp2term args')))
      | Exp.TERM (Exp.COMPLEX (a, b)) => (fn(args') => Exp.TERM (Exp.COMPLEX (exp2term (List.nth (args', 0)), exp2term (List.nth (args', 1)))))
      | Exp.TERM (Exp.RANGE {low, step, high}) => (fn(args') => Exp.TERM (Exp.RANGE {low=exp2term (List.nth (args', 0)),
										     step=exp2term (List.nth (args', 1)),
										     high=exp2term (List.nth (args', 2))}))
      | Exp.CONTAINER (Exp.EXPLIST l) => (fn(args') => Exp.CONTAINER (Exp.EXPLIST args'))
      | Exp.CONTAINER (Exp.ARRAY a) => (fn(args') => Exp.CONTAINER (Exp.ARRAY (Container.listToArray args')))
      | Exp.CONTAINER (Exp.ASSOC tab) =>
	let
	    val keys = SymbolTable.listKeys tab
	    val zero = SymbolTable.empty
	    val cons = Exp.CONTAINER o Exp.ASSOC
	in
	 fn items =>
	    cons (ListPair.foldlEq (fn (k,v,tab) => SymbolTable.enter (tab,k,v)) zero (keys, items))
	end
	    
      | Exp.CONTAINER (Exp.MATRIX m) => 
	(case !m of
	     Matrix.DENSE {data, calculus} => 
	     (fn(args') => Exp.CONTAINER (Exp.MATRIX (Matrix.fromRows (calculus) (Container.expListToArrayList args'))))
	   | Matrix.BANDED {data, calculus, nrows, ncols, upperbw, lowerbw} => 
	     (fn(all_args) => 
		case all_args of
		    zero::args' => 
		    if (#isZero calculus) zero then
			let
			    val data' = map Container.expArrayToArray args'
			in
			    Exp.CONTAINER (Exp.MATRIX 
					       (ref (Matrix.BANDED {data=data',
								    calculus=calculus, 
								    nrows=nrows,
								    ncols=ncols,
								    upperbw=upperbw,
								    lowerbw=lowerbw})))
			end
		    else (* the zero element has changed, so it's no longer a banded matrix *)
			let
			    val args'' = map Container.expArrayToArray args'
			    val data' = Array2.array (nrows, ncols, zero)
			    fun updateBand (a, num) =
				let
				    val indices = if num = 0 then (* on diagonal *)
						      List.tabulate (Array.length a, fn(i)=>(i,i))
						  else if num < 0 then (* lower band *)
						      List.tabulate (Array.length a, fn(i)=>(i-num, i))
						  else (* upper band *)
						      List.tabulate (Array.length a, fn(i)=>(i, i+num))
				in
				    app 
					(fn(exp, (i,j))=> Array2.update (data', i, j, exp)) 
					(ListPair.zip (Container.arrayToList a, indices))
				end
			    val band_numbers = List.tabulate (length args'', fn(x)=>x-lowerbw)
			    val _ = app updateBand (ListPair.zip (args'', band_numbers))
			in
			    Exp.CONTAINER (Exp.MATRIX (ref (Matrix.DENSE {data=data', calculus=calculus})))
			end
		  | _ => DynException.stdException("Unexpected number of arguments", "ExpTraverse.head [Banded Matrix]", Logger.INTERNAL)))
      | Exp.SUBREF (exp, space) => (fn(all_args) => case all_args of
							[onearg] => Exp.SUBREF (exp, space)
						      | _ => DynException.stdException("Unexpected number of arguments", "ExpTraverse.head [SUBREF]", Logger.INTERNAL))
      | _ => (fn(args') => exp))
    handle e => DynException.checkpoint ("ExpTraverse.head ["^(e2s exp)^"]") e




end
