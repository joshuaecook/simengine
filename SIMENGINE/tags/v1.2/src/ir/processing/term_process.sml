signature TERMPROCESS = 
sig

    (* iterator related term functions *)
    val symbol2temporaliterator : Exp.term -> Iterator.iterator option
    val symbol2spatialiterators : Exp.term -> Iterator.iterator list

end
structure TermProcess : TERMPROCESS =
struct

fun symbol2temporaliterator term = 
    let
	val iterators = CurrentModel.iterators()
(*	val _ = Util.log("Global iterators = " ^ (Util.symlist2s (map #1 iterators)))*)
    in
	case term of
	    Exp.SYMBOL (sym, props) => 
	    (case Property.getIterator props of
		 SOME iters => 
		 List.find 
		     (fn (sym, _) => List.exists (fn(sym',_)=> sym = sym') iterators) 
		     iters
	       | NONE => NONE)
	  | _ => NONE
    end

fun symbol2spatialiterators term = 
    case symbol2temporaliterator term of
	SOME (itersym,_) => 
	 (case term of
	      Exp.SYMBOL (_, props) => 
	      (case Property.getIterator props of
		   SOME iters => List.filter (fn(sym', _)=> not (itersym = sym')) iters
		 | NONE => [])
	    | _ => [])
      | NONE => (case term of
		     Exp.SYMBOL (_, props) => 
		     (case Property.getIterator props of
			  SOME iters => iters
			| NONE => [])
		   | _ => [])


end
