signature EXPEQUALITY =
sig

(* these are all the assigned patterns that have already been matched [(Symbol.symbol 'a', Expression (x+y*z))] *)
type patterns_matched = Exp.exp SymbolTable.table list 

(* matching functions *)
val equiv : (Exp.exp * Exp.exp) -> bool (* are two expressions equivalent? *)
val termEquiv : (Exp.term * Exp.term) -> bool (* are two terminals equivalent? *)
(*val exp_equivalent : patterns_matched -> (Exp.exp * Exp.exp) -> (patterns_matched * bool) (* same as above, but add in the matched patterns *)*)
val findMatches : (Exp.exp * Exp.exp) -> patterns_matched (* find matched patterns between two expressions *)

end
structure ExpEquality : EXPEQUALITY =
struct

fun print (s) = ()


type patterns_matched = Exp.exp SymbolTable.table list
val b2s = Util.b2s
val e2s = ExpPrinter.exp2str
fun explist2str explist = 
    "{" ^ (String.concatWith ", " (map e2s explist)) ^ "}"

(* this function is used to kill all match candidates based upon a flag *)
(*   this is useful if some condition would mean that nothing can match regardless of prior matched symbols *)
fun checkAndKillMatches matchCandidates false = nil
  | checkAndKillMatches matchCandidates true = matchCandidates

(* helper function for All *)
fun allEquiv comparefun matchCandidates (nil, nil) = matchCandidates
  | allEquiv comparefun (matchCandidates: patterns_matched) (term1::rest1, term2::rest2) =
    let
	val matchCandidates' = comparefun matchCandidates (term1, term2)
    in
	allEquiv comparefun matchCandidates' (rest1, rest2) 
    end 
  | allEquiv comparefun matchCandidates _ = nil


(* Check if terms are equivalent *)
fun terms_equivalent (matchCandidates: patterns_matched) (term1, term2) = 
    case (term1, term2) of 

	(* Start off on the simple cases *)
	(Exp.RATIONAL (a1,b1), Exp.RATIONAL (a2,b2)) => 
	checkAndKillMatches matchCandidates 
			    ((a1=a2 andalso b1=b2) (* general case *)
			     orelse (* otherwise, if they are multiples*)
			     (Real.?=((Real.fromInt(a1))/(Real.fromInt(a2)),(Real.fromInt(a1))/(Real.fromInt(a2)))))
      | (Exp.INT a1, Exp.INT a2) => 
	checkAndKillMatches matchCandidates 
			    (a1 = a2)
      | (Exp.REAL a1, Exp.REAL a2) => 
	checkAndKillMatches matchCandidates 
			    (Real.?=(a1, a2))

      | (Exp.BOOL a1, Exp.BOOL a2) => 
	checkAndKillMatches matchCandidates 
			    ((a1 andalso a2) orelse (not a1 andalso not a2))
      | (Exp.COMPLEX (r1, i1), Exp.COMPLEX (r2, i2)) => 

	allEquiv terms_equivalent matchCandidates ([r1, i1], [r2, i2])
      | (Exp.LIST (l1, d1), Exp.LIST (l2, d2)) => 
	let
	    val matchCandidates' =
		checkAndKillMatches matchCandidates 
				    
				    (* first check the list sizes *)
				    ((List.length l1) = (List.length l2)
				     andalso 
				     (* check the number of dimensions *)
				     (List.length d1) = (List.length l2)
				     andalso
				     (* check that the dimensions are organized in the same way *)
				     (List.all (fn(s1, s2)=> s1=s2) (ListPair.zip (d1, d2))))
				    (* check that all the terms are the same *)
	in
	    (allEquiv terms_equivalent matchCandidates' (l1, l2))
	end
      | (Exp.TUPLE l1, Exp.TUPLE l2) =>
	let
	    val matchCandidates' =
		(* first check the list sizes *)
		checkAndKillMatches matchCandidates 
				    ((List.length l1) = (List.length l2) )
	in
	    (* check that all the terms are the same *)
	    allEquiv terms_equivalent matchCandidates' (l1, l2)
	end
      | (Exp.SYMBOL (s1, p1), Exp.SYMBOL (s2, p2)) =>
	checkAndKillMatches matchCandidates 
			    (* symbol names must be the same *)
			    (s1 = s2
			    andalso
			    (* check the properties are the same *)
			    (* start with diff *)
			    (case (Property.getDerivative p1, Property.getDerivative p2)
			      of (NONE, NONE) => true
			       | (SOME (o1, l1), SOME (o2, l2)) => 
				 (* check the order *)
				 o1 = o2
				 andalso 
				 (* check the list of iterators *)
				 (* check the size *)
				 length l1 = length l2
				 andalso
				 List.all (fn(a,b)=>a=b) (ListPair.zip (l1, l2))
		 
			       | _ => false)
			     andalso
			     (* check the iterator lists *)
			     (case (Property.getIterator p1, Property.getIterator p2)
			       of (NONE, NONE) => true
				| (SOME l1, SOME l2) => 
				  (* check the size *)
				  length l1 = length l2
				  andalso
				  List.all Iterator.iter_equiv (ListPair.zip (l1, l2))

				| _ => false))

      | (Exp.INFINITY, Exp.INFINITY) => matchCandidates
      | (Exp.NAN, Exp.NAN) => matchCandidates
      | (Exp.DONTCARE, _) => matchCandidates
      | (_, Exp.DONTCARE) => matchCandidates
      (* now handle some of the other cases *)
      | (t1, Exp.PATTERN p2) => pattern_equivalent matchCandidates (p2, Exp.TERM t1)
      | (Exp.PATTERN p1, t2) => pattern_equivalent matchCandidates (p1, Exp.TERM t2)
      | _ => (*if (isNumeric term1) andalso (isNumeric term2) then
		   if (termCount term1) = (termCount term2) then*)
	nil

(* verifies that a pattern is equivalent to a single expression *)
(*   note: that expression is expected to not be another pattern *)
(*   note: this is to NOT be used for expressions in a list, since patcount will NOT be used *)
and pattern_equivalent matchCandidates (pat as (sym, (predname,pred), patcount), (exp: Exp.exp)) =
    let
	(* cache the predicate result *)
	val predMatches = ref NONE

	fun equivUsingCandidate candidate =
	    case SymbolTable.look(candidate, sym) of
		SOME (e) => 
		(exp_equivalent [candidate] (exp, e)) (* will return [candidate] if equivalent, [] if not (thus pruning the match candidate) *)
	      | NONE => 
		let
		    val candidate' = SymbolTable.enter (candidate, sym, exp)
		in
		    case !predMatches of
			SOME result => if result then [candidate'] else nil
		      | NONE => 
			let 
			    val result = pred exp
			    val _ = predMatches := SOME result
			in
			    if result then [candidate'] else nil
			end
		end
    in
	foldl (op @) nil (map equivUsingCandidate matchCandidates)
    end

(*
and pattern_list_equivalent matchCandidates (pat as (sym, (_, pred), patcount), explist) =
    if PatternProcess.patcount_compatible patcount (List.length explist) then
	foldl
	    (fn(exp,(matchCandidates', ret'))=> if ret' then
						      pattern_equivalent matchCandidates' (pat, exp)
						  else (* something previously failed *)
						      (matchCandidates', false))
	    (matchCandidates, false)
	    explist
    else
	(matchCandidates, false)
*)

and patternListMatch (candidate: Exp.exp SymbolTable.table) (pattern: Exp.exp list) elements =
    let
	(* the pattern state refers to a state machine we execute over the pattern *)
	(*   Every time we hit a pattern which could match in multiple (ie 1-5 arg count),  *)
	(*   we fork the state to contain an entry for each possible path.  We remove entries *)
	(*   as they are found to be invalid.  We execute the entire state in parallel.*)
	val initialPatternState = [(candidate, NONE, pattern)]	

	datatype partialMatch = NEWMATCH of Symbol.symbol
			      | PRIORMATCH of Symbol.symbol * int

(*	fun matchPattern nil (candidate, openMatch, nil) = [(candidate, NONE, nil)]
	  | matchPattern _ (candidate, openMatch, nil) = nil
	  | matchPattern nil (candidate, openMatch, patterns) =
	    (* check patterns to be all empty *)
	    let
		fun isZeroCompatible Pattern.ONE = false
		  | isZeroCompatible Pattern.ONE_OR_MORE = false
		  | isZeroCompatible Pattern.ZERO_OR_MORE = true
		  | isZeroCompatible Pattern.SPECIFIC_COUNT c = c = 0
		  | isZeroCompatible Pattern.SPECIFIC_RANGE (low, high) = low <= 0
	    in
		if List.all isZeroCompatible patterns then
		    [(candidate, NONE, nil)]
		else
		    []
	    end*)
	fun matchPattern (element: Exp.exp) (candidate, openMatch, nil: Exp.exp list) = nil
	  | matchPattern (element: Exp.exp) (candidate, openMatch, nextMatch::pattern') =
	    case nextMatch of
		Exp.TERM(Exp.PATTERN (sym, (predname,pred), patcount)) =>
		let
		    fun rebuildPattern patcount' =
			Exp.TERM(Exp.PATTERN (sym, (predname,pred), patcount'))
		in
		    (case patcount of
			 Pattern.ONE => 
			 (case SymbolTable.look(candidate, sym) of
			      SOME e => (* match against a single, previously matched item *)
			      map (fn(c) => 
				     (c,
				      NONE,
				      pattern'))
				  (exp_equivalent [candidate] (element, e))
			    | NONE => (* new single match *)
			      if pred element then  (* check if it satisfies the predicates *)
				  [(SymbolTable.enter (candidate, sym, element),
				    NONE,
				    pattern')]
			      else
				  nil)
		       | Pattern.ONE_OR_MORE => (* 1+ *)
			 (case SymbolTable.look(candidate, sym) of
			      SOME (Exp.META(Exp.SEQUENCE exps)) =>  (* previously matched sequence found *)
			      (case openMatch of 
				   NONE =>  (* this is the beginning of a 1+ match against a sequence*)
				   if (length exps) > 0 andalso not (null (exp_equivalent [candidate] (element, List.nth(exps, 0)))) then
				       [(candidate, 
					 SOME (PRIORMATCH (sym, 1)),
					 (rebuildPattern Pattern.ZERO_OR_MORE) :: pattern')]
				   else
				       []
				 | SOME (NEWMATCH s) => [] (* this isn't a valid case, since we cannot have a previous match on record AND be a new match of indeterminate length *)
				 | SOME (PRIORMATCH (s, i)) => [](* this isn't a valid case, since we turn 1+ into 0+ after the first element *)
			      )
 			    | SOME e => (* previously matched single item found *)
			      (case openMatch of 
				   NONE =>  (* this is the beginning of a 1+ match against a single item, so it's equivalent to a ONE *)
				   matchPattern element (candidate, NONE, (rebuildPattern Pattern.ONE) :: pattern')
				 | SOME (NEWMATCH s) => [] (* this isn't a valid case, since we cannot have a previous match on record AND be a new match of indeterminate length *)
				 | SOME (PRIORMATCH (s, i)) => [](* this isn't a valid case, since the previous matched single item isn't a sequence *)
			      )
			    | NONE => (* no previous matches found *)
			      if pred element then
				  [(SymbolTable.enter (candidate, sym, element),
				    SOME (NEWMATCH sym),
				    (rebuildPattern Pattern.ZERO_OR_MORE) :: pattern')]
			      else
				  nil)
		       | Pattern.ZERO_OR_MORE =>
			 (case SymbolTable.look(candidate, sym) of
			      SOME (Exp.META(Exp.SEQUENCE exps)) =>  (* previously matched sequence found *)
			      (case openMatch of 
				   NONE =>  (* this is the beginning of a 0+ match against a sequence*)
				   if (length exps) = 0 then
				       (matchPattern element (candidate, NONE, pattern')) (* we've matched previously, and it is supposed to be empty *)
				   else if (length exps) > 0 andalso not (null (exp_equivalent [candidate] (element, List.nth(exps, 0)))) then
				       [(candidate, 
					 SOME (PRIORMATCH (sym, 1)),
					 (rebuildPattern Pattern.ZERO_OR_MORE) :: pattern')] (* we've matched previously, and it is a sequence we're going to start going through  *)
				   else
				       []
				 | SOME (NEWMATCH s) =>  (* we have matched some number of elements and are trying to match more *)
				   (if pred element then
					[(SymbolTable.enter (candidate, sym, Exp.META(Exp.SEQUENCE(exps @ [element]))),
					  SOME (NEWMATCH sym),
					  (rebuildPattern Pattern.ZERO_OR_MORE) :: pattern')]
				    else
					[]) @
				   (matchPattern element (candidate,
							  NONE,
							  pattern'))
				 | SOME (PRIORMATCH (s, i)) => (* We verify our element against the existing match *)
				   if (length exps) <= i then
				       [(candidate,
					 NONE,
					 pattern')]
				   else if not (null (exp_equivalent [candidate] (element, List.nth(exps, i)))) then
				       [(candidate, 
					 SOME (PRIORMATCH (sym, i+1)),
					 (rebuildPattern Pattern.ZERO_OR_MORE) :: pattern')] (* we've matched previously, and it is a sequence we're going through  *)
				   else
				       []
			      )
 			    | SOME e => (* previously matched single item found *)
			      (case openMatch of 
				   NONE =>  (* this is the beginning of a 0+ match against a single item, equivalent to a ONE*)
				   matchPattern element (candidate, NONE, (rebuildPattern Pattern.ONE) :: pattern')
				 | SOME (NEWMATCH s) =>  (* we have matched 1 element and are trying to match more *)
				   (if pred element then
					[(SymbolTable.enter (candidate, sym, Exp.META(Exp.SEQUENCE(e :: element :: nil))),
					  SOME (NEWMATCH sym),
					  (rebuildPattern Pattern.ZERO_OR_MORE) :: pattern')] 
				    else
					[]) @
				   (matchPattern element (candidate,
							  NONE, 
							  pattern'))
				    
				 | SOME (PRIORMATCH (s, i)) => [](* this isn't a valid case, since the previous matched single item isn't a sequence *)
			      )
			    | NONE => (* no previous matches found *)
			      (if pred element then (* if the element matches the pred, try starting a new sequence *)
				   [(SymbolTable.enter (candidate, sym, element),
				     SOME (NEWMATCH sym),
				     (rebuildPattern Pattern.ZERO_OR_MORE) :: pattern')]
			       else
				   nil)
			      @ (matchPattern element (SymbolTable.enter (candidate, sym, Exp.META(Exp.SEQUENCE [])), NONE, pattern')) (* handle the case that we should match NOTHING and continue *)
			 )

		       (* TODO: fill in*)
		       | Pattern.SPECIFIC_COUNT c =>
			 (case SymbolTable.look(candidate, sym) of
			      SOME (Exp.META(Exp.SEQUENCE exps)) => nil  (* previously matched sequence found *)
 			    | SOME e => nil(* previously matched single item found *)
			    | NONE => nil(* no previous matches found *)
			 )
		       | Pattern.SPECIFIC_RANGE (low, high) =>
			 (case SymbolTable.look(candidate, sym) of
			      SOME (Exp.META(Exp.SEQUENCE exps)) => nil (* previously matched sequence found *)
 			    | SOME e => nil (* previously matched single item found *)
			    | NONE => nil (* no previous matches found *)
			 )
		    )
		end		    
	      | nonpattern => 
		map (fn(c) => (c,
			       NONE,
			       pattern'))
		    (exp_equivalent [candidate] (element, nonpattern))
		

	fun updatePatternState (element, patternState) =
	    Util.flatmap (matchPattern element) patternState


	fun printPatternState (cm, pm, patterns) =
	    (print("  new pattern state: \n");
	     print("    candidateMatch = " ^ (String.concatWith ", " (map (fn(k) => (Symbol.name k) ^ "=" ^ (e2s(valOf(SymbolTable.look (cm,k))))) (SymbolTable.listKeys cm))) ^ "\n");
	     print("    prior match: ");
	     case pm of
		 NONE => print ("NONE\n")
	       | SOME(NEWMATCH s) => print ("NEWMATCH of " ^ (Symbol.name s) ^ "\n")
	       | SOME (PRIORMATCH (s,i)) => print ("PRIORMATCH of " ^ (Symbol.name s) ^ " @ " ^ (Int.toString i) ^ "\n");
	     print("    patterns: " ^ (String.concatWith ", " (map e2s (patterns))) ^ "\n"))
		 
	    
	fun printPatternStates (element, patternstates) =
	    (print ("=====================\n");
	     app printPatternState patternstates;
	     print ("  element we're about to process: " ^ (e2s element) ^ "\n"))

	val _ = print ("Matching: " ^ (String.concatWith ", " (map e2s elements)) ^ "\n")

	val patternState' = foldl (fn(e,ps) => (printPatternStates(e, ps); updatePatternState (e, ps))) initialPatternState elements

	val _ = print ("  ==============Final")
	val _ = app printPatternState patternState'

	fun isZeroable (Exp.TERM(Exp.PATTERN (sym, (predname,pred), patcount))) =
	    (case patcount of
		 Pattern.ZERO_OR_MORE => true
	       | Pattern.SPECIFIC_COUNT 0 => true
	       | Pattern.SPECIFIC_RANGE (0, _) => true
	       | _ => false)
	  | isZeroable _ = false

	fun nullable (NONE) nil = true
	  | nullable (NONE) (patt::rest) = 
	    isZeroable patt andalso nullable NONE rest
	  | nullable (SOME (NEWMATCH _)) patt =
	    nullable NONE patt
	  | nullable (SOME (PRIORMATCH (_, i))) patts =
	    nullable NONE patts andalso (*i = (length blablabla) TODO: FIXME *) true

	val matches = List.filter (fn(_, openmatch, pattern) => nullable openmatch pattern) 
				  patternState'

	(* if any trailing patterns were left blank, we iterate through and fill them in *)
	fun pattern2sym (Exp.TERM(Exp.PATTERN (sym, (predname,pred), patcount))) =
	    sym
	  | pattern2sym _ = DynException.stdException ("Invalid pattern expression", "ExpEquality.patternlistmatch.pattern2sym", Logger.INTERNAL)

	fun addBlank (cm, _, pattern) =
	    foldl (fn(p,cm) => 
		     let 
			 val sym = pattern2sym p 
		     in
			 case SymbolTable.look(cm, sym) of
			     NONE => SymbolTable.enter(cm, sym, Exp.META(Exp.SEQUENCE[]))
			   | SOME _ => cm
		     end) 
	    cm 
	    pattern

	val matches' = map addBlank matches
		
    in
	matches'
    end
    

and list_equivalent (matchCandidates: patterns_matched) (explist1: Exp.exp list, explist2: Exp.exp list) =
let
	val _ = print ("calling list_equivalent\n")
(*	val _ = print ("  assigned patterns = " ^ (String.concatWith ", " (map (fn(sym, repl_exp) => (Symbol.name sym) ^ "=" ^ (e2s repl_exp)) matchCandidates)))	*)
	val _ = print ("  matching = " ^(explist2str explist1)^"' and '"^(explist2str explist2)^"\n")		

	fun isPattern (Exp.TERM(Exp.PATTERN _)) = true
	  | isPattern _ = false

	fun containsPatterns alist = 
	    List.exists isPattern alist

	fun pairwiseMatch (list1, list2) =
	    if length list1 <> length list2 then
		matchCandidates
	    else
		allEquiv exp_equivalent matchCandidates (list1, list2)
				   
in
    if (containsPatterns explist1) andalso (containsPatterns explist2) then
	pairwiseMatch (explist1, explist2)
    else
	if (containsPatterns explist1) then
	    Util.flatmap (fn(candidate) => patternListMatch candidate explist1 explist2) 
			 matchCandidates
	else
	    Util.flatmap (fn(candidate) => patternListMatch candidate explist2 explist1) 
			 matchCandidates
end

(* Check if two expressions are equivalent *)
and exp_equivalent (matchCandidates: patterns_matched) (exp1, exp2) = 
    let
(*	val _ = Util.log ("in exp_equivalent checking: " ^ (e2s exp1) ^ " and " ^ (e2s exp2))*)
(*	val _ = Util.log ("  with matchCandidates = " ^ (String.concatWith ", " (map (fn(sym, repl_exp) => (Symbol.name sym) ^ "=" ^ (e2s repl_exp)) matchCandidates)))*)

	val matchCandidates' = 
	    case (exp1, exp2) of
		(Exp.TERM t1, Exp.TERM t2) => 
		terms_equivalent matchCandidates (t1, t2)

	      | (Exp.FUN (Fun.BUILTIN fun1, args1), 
		 Exp.FUN (Fun.BUILTIN fun2, args2)) => 
		if fun1 = fun2 then
		    case #operands (FunProps.op2props fun1) of
			FunProps.VARIABLE _ => 
			list_equivalent matchCandidates (args1, args2)

		      | FunProps.FIXED _ => 
			allEquiv exp_equivalent matchCandidates (args1, args2)
		else
		    nil
	      | (Exp.FUN (Fun.INST {classname=classname1,...},args1),
		 Exp.FUN (Fun.INST {classname=classname2,...},args2)) =>
		if classname1=classname2 then
		    allEquiv exp_equivalent matchCandidates (args1, args2)
		else
		    nil
	      | (Exp.FUN _, Exp.FUN _) => nil
	      (* need to handle patterns *)

	      (* simply, we can start with just using the allEquiv - maybe we can
	         eventually use list_equivalent *)
	      | (Exp.CONTAINER (Exp.EXPLIST l1), Exp.CONTAINER (Exp.EXPLIST l2)) =>
		list_equivalent matchCandidates (l1, l2)

	      | (Exp.CONTAINER (c1 as (Exp.ARRAY a1)), Exp.CONTAINER (c2 as (Exp.ARRAY a2))) =>
		allEquiv exp_equivalent matchCandidates 
			 (Container.container2elements c1, 
			  Container.container2elements c2)
		
	      | (Exp.CONTAINER (c1 as (Exp.MATRIX m1)), Exp.CONTAINER (c2 as (Exp.MATRIX m2))) =>
		if (Container.matrix2size m1) = (Container.matrix2size m2) then
		    allEquiv exp_equivalent matchCandidates 
			     (Container.container2elements c1, 
			      Container.container2elements c2)
		else
		    nil
		
	      | (exp1, Exp.TERM (Exp.PATTERN p)) => 
		pattern_equivalent matchCandidates (p, exp1) 

	      | (Exp.TERM (Exp.PATTERN p), exp2) => 
		pattern_equivalent matchCandidates (p, exp2) 

	      | (Exp.META(Exp.SEQUENCE s1), Exp.META(Exp.SEQUENCE s2))
		=> allEquiv exp_equivalent matchCandidates (s1, s2)

	      | _ => nil

    in
	matchCandidates'
    end

(* Perform equivalency check on expressions *)
fun termEquiv (term1, term2) = 
    let 
	val matchCandidates = terms_equivalent [SymbolTable.empty] (term1, term2)
    in
	not (null matchCandidates)
    end

fun findMatches (exp1, exp2) =
    let
	val matchCandidates = exp_equivalent [SymbolTable.empty] (exp1, exp2)
    in
	matchCandidates
    end

fun equiv (exp1, exp2) = 
    let
	val matchCandidates = findMatches (exp1, exp2)
    in
	not (null matchCandidates)
    end


end
