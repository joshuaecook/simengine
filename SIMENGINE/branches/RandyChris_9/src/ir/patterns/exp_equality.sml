structure ExpEquality =
struct

(* two helper functions that operate the same way as andalso and orelse, 
 just with an extra output argument *)
fun andcond ((pat1, bool1), (pat2, bool2)) =
    if bool1 then
	(pat2, bool2)
    else
	(pat1, bool1)
infix andcond

fun orcond ((pat1, bool1), (pat2, bool2)) =
    if bool1 then
	(pat1, bool1)
    else
	(pat2, bool2)
infix orcond

(* helper function for All *)
fun allEquiv comparefun assigned_patterns (nil, nil) = (assigned_patterns, true)
  | allEquiv comparefun assigned_patterns (term1::rest1, term2::rest2) =
    let
	val (assigned_patterns', result) = comparefun assigned_patterns (term1, term2)
    in
	if result then
	    allEquiv comparefun assigned_patterns' (rest1, rest2)
	else
	    (assigned_patterns, result)
    end
  | allEquiv comparefun assigned_patterns _ = (assigned_patterns, false)


(* Check if terms are equivalent *)
fun terms_equivalent assigned_patterns (term1, term2) = 
    case (term1, term2) of 

	(* Start off on the simple cases *)
	(Exp.RATIONAL (a1,b1), Exp.RATIONAL (a2,b2)) => 
	(assigned_patterns, 
	 (a1=a2 andalso b1=b2) (* general case *)
	 orelse (* otherwise, if they are multiples*)
	 (Real.==((Real.fromInt(a1))/(Real.fromInt(a2)),(Real.fromInt(a1))/(Real.fromInt(a2))))
	)
      | (Exp.INT a1, Exp.INT a2) => 
	(assigned_patterns,
	 a1 = a2
	)
      | (Exp.REAL a1, Exp.REAL a2) => 
	(assigned_patterns,
	 Real.==(a1, a2)
	)
      | (Exp.COMPLEX (r1, i1), Exp.COMPLEX (r2, i2)) => 

	allEquiv terms_equivalent assigned_patterns ([r1, i1], [r2, i2])
      | (Exp.LIST (l1, d1), Exp.LIST (l2, d2)) => 
	(* first check the list sizes *)
	(assigned_patterns, 
	 (List.length l1) = (List.length l2) 
	)
	    andcond (* check the number of dimensions *)
	    (assigned_patterns,
	     (List.length d1) = (List.length l2)
	    )
	    andcond (* check that the dimensions are organized in the same way *)
	    (assigned_patterns, 
	     (List.all (fn(s1, s2)=> s1=s2) (ListPair.zip (d1, d2)))
	    )
	    andcond (* check that all the terms are the same *)
	    (allEquiv terms_equivalent assigned_patterns (l1, l2))
      | (Exp.TUPLE l1, Exp.TUPLE l2) =>
	(* first check the list sizes *)
	(assigned_patterns, 
	 (List.length l1) = (List.length l2) 
	)
	    andcond (* check that all the terms are the same *)
	    (allEquiv terms_equivalent assigned_patterns (l1, l2))
      | (Exp.SYMBOL (s1, p1), Exp.SYMBOL (s2, p2)) =>
	(* symbol names must be the same *)
	(assigned_patterns,
	 s1 = s2
	)
	    andcond (* check the properties are the same *)
	    (* start with diff *)
	    (case (Property.getDerivative p1, Property.getDerivative p2)
	      of (NONE, NONE) => (assigned_patterns, true)
	       | (SOME (o1, l1), SOME (o2, l2)) => 
		 (* check the order *)
		 (assigned_patterns, 
		  o1 = o2
		 )
		     andcond (* check the list of iterators *)
		     (* check the size *)
		     (assigned_patterns,
		      length l1 = length l2)
		     andcond
		     (assigned_patterns,
		      List.all (fn(a,b)=>a=b) (ListPair.zip (l1, l2)))
		 
	       | _ => (assigned_patterns, false))
	    andcond
	    (* check the iterator lists *)
	    (case (Property.getIterator p1, Property.getIterator p2)
	      of (NONE, NONE) => (assigned_patterns, true)
	       | (SOME l1, SOME l2) => 
		 (* check the size *)
		 (assigned_patterns,
		  length l1 = length l2
		 )
		     andcond
		     (assigned_patterns,
		      List.all Iterator.iter_equiv (ListPair.zip (l1, l2)))
	       | _ => (assigned_patterns, false))
      | (Exp.INFINITY, Exp.INFINITY) => (assigned_patterns, true)
      | (Exp.NAN, Exp.NAN) => (assigned_patterns, true)
      | (Exp.DONTCARE, _) => (assigned_patterns, true)
      | (_, Exp.DONTCARE) => (assigned_patterns, true)
      (* now handle some of the other cases *)
      | (t1, Exp.PATTERN p2) => pattern_equivalent assigned_patterns (p2, Exp.TERM t1)
      | (Exp.PATTERN p1, t2) => pattern_equivalent assigned_patterns (p1, Exp.TERM t2)
      | _ => (*if (isNumeric term1) andalso (isNumeric term2) then
		   if (termCount term1) = (termCount term2) then*)
	(assigned_patterns, false)

(* verifies that a pattern is equivalent to a single expression *)
and pattern_equivalent assigned_patterns (pat as (sym, (_,pred), patcount), exp) =
    case List.find (fn(s, e)=> s=sym) assigned_patterns of
	SOME (s, e) => (exp_equivalent assigned_patterns (exp, e))
      | NONE => if pred exp then 
		  ((sym, exp)::assigned_patterns, true)
		else
		    (assigned_patterns, false)

(*
and pattern_list_equivalent assigned_patterns (pat as (sym, (_, pred), patcount), explist) =
    if PatternProcess.patcount_compatible patcount (List.length explist) then
	foldl
	    (fn(exp,(assigned_patterns', ret'))=> if ret' then
						      pattern_equivalent assigned_patterns' (pat, exp)
						  else (* something previously failed *)
						      (assigned_patterns', false))
	    (assigned_patterns, false)
	    explist
    else
	(assigned_patterns, false)
*)

and list_equivalent assigned_patterns (explist1, explist2) =
    let
	fun isExpZeroCompatible ap exp = 
	    case exp
	     of Exp.TERM (Exp.NAN) => (ap, true)
	      | Exp.TERM (Exp.LIST (l, d)) => (ap, length l = 0)
	      | Exp.TERM (Exp.TUPLE l) => (ap, length l = 0)
	      | Exp.TERM (Exp.PATTERN (sym, _, patcount)) => 
		if PatternProcess.patcount_compatible patcount 0 then
		    (((sym,Exp.TERM Exp.NAN)::ap), true)
		else
		    (ap, false)
	      | Exp.FUN (Fun.BUILTIN Fun.GROUP, []) => (ap, true)
	      | _ => (ap, false)

	(* list_equiv_helper - recursive routine that will use a greedy algorithm to best match two lists of expressions *)
	fun list_equiv_helper assigned_patterns (nil, nil) = (assigned_patterns, true)
	  | list_equiv_helper assigned_patterns (nil, explist2) = 
	    foldl (fn(exp, r as (ap, _)) => r andcond isExpZeroCompatible ap exp) (assigned_patterns, true) explist2
	  | list_equiv_helper assigned_patterns (explist1, nil) = 
	     foldl (fn(exp, r as (ap, _)) => r andcond isExpZeroCompatible ap exp) (assigned_patterns, true) explist1
	  | list_equiv_helper assigned_patterns (exp1::nil, exp2::nil) = 
	    (* shortcut pattern *)
	    exp_equivalent assigned_patterns (exp1, exp2)
	  | list_equiv_helper assigned_patterns (exp1::rest1, explist2) =
	    let
		(* remove duplicates in the assigned patterns structure *)
		fun removeAPduplicates assigned_patterns =
		    let
			val symbols = map (fn(sym,_)=>sym) assigned_patterns
		    in
			map 
			    (fn(sym)=>
			       let
				   val (matching, unmatching) = List.partition (fn(sym', _)=>sym=sym') assigned_patterns
			       in
				   if length matching = 1 then
				       Util.hd matching
				   else
				       let
					   val exps = map (fn(_, exp)=>exp) matching
				       in
					   (sym, Exp.FUN (Fun.BUILTIN Fun.GROUP, exps))
				       end
			       end
			    )
			    (Util.uniquify symbols)
		    end
		    
		(* gobble takes as many of the explist argument as possible to match with exp *)
		fun gobble assigned_patterns (exp, nil) = ((assigned_patterns, true), [])
		  | gobble assigned_patterns (exp1, exp2::rest) = 
		    let			
			val (assigned_patterns', ret) = exp_equivalent assigned_patterns (exp1, exp2)
		    in
			if ret then
			    let
				val ((assigned_patterns'',ret'),explist) = gobble assigned_patterns (exp1, rest)
			    in
				if ret' then
				    ((assigned_patterns'',ret'),explist)
				else
				    ((assigned_patterns', ret),rest)
			    end
			else
			    ((assigned_patterns, false), exp2::rest)
		    end


		(* gobble_helper - what this does is run gobble repeatedly, but it needs to also run list_equiv_helper *)
		fun gobble_helper assigned_patterns max_count (exp1::rest1, explist2) = 
		    (case exp1 of
			 Exp.TERM (Exp.PATTERN (_,_,patcount)) => 
			 let
			     val min_num = PatternProcess.min_patcount patcount
			 in
			     if min_num > max_count then
				 (* return what we have ... *)
				 (assigned_patterns, false, explist2)
			     else
				 let
				     val ((assigned_patterns', ret'), rest) = gobble assigned_patterns (exp1, explist2)
				 in
				     if ret' then (* passing this means that it was able to successfully gobble something in explist *)
					 let
					     val (assigned_patterns'', ret'') = list_equiv_helper assigned_patterns' (rest1, rest)
					 in
					     if ret'' then (* this means that the rest matched*)
						 (assigned_patterns'', ret'', [])
					     else (* the rest didn't match *)
						 if max_count > min_num then
						     gobble_helper assigned_patterns (max_count-1) (exp1::rest1, explist2) (* start again, with one less gobbled *)
						 else (* there's not a lot we can do ... it doesn't match *)
						     (assigned_patterns, false, explist2)
					 end
				     else (* we couldn't gobble, meaning that it didn't work *)
					 (assigned_patterns, false, explist2)
				 end
			 end
		       | _ => DynException.stdException ("Wouldn't expect anything but expressions here", "ExpEquality.list_equivalent.list_equiv_helper.gobble_helper", Logger.INTERNAL))
		  | gobble_helper _ _ _ = DynException.stdException ("Unexpected nil expression list", "ExpEquality.list_equivalent.list_equiv_helper.gobble_helper", Logger.INTERNAL)

		(* all the matching should be done... if it doesn't work at this point, it won't work.. *)
		val ((assigned_patterns', ret, list1), list2) = 
		    case exp1 of
			Exp.TERM (Exp.PATTERN (_,_,patcount)) =>
			(case PatternProcess.max_patcount patcount of
			     SOME i => if length explist2 > i then
					   (gobble_helper assigned_patterns i (exp1::rest1, Util.take (explist2, i)), Util.drop (explist2, i))
				       else
					   (gobble_helper assigned_patterns (length explist2) (exp1::rest1, explist2), [])
			   | NONE => (gobble_helper assigned_patterns (length explist2) (exp1::rest1, explist2), []))
		      | _=> 
			let
			    val (assigned_patterns', ret') = exp_equivalent assigned_patterns (exp1, Util.hd explist2)
			in
			    ((assigned_patterns', ret', []), (List.tl explist2))
			end

		val remaining_exps = list1 @ list2
		val matched_exps = Util.take (explist2, length explist2 - (length remaining_exps))
	    in
		(assigned_patterns', ret)
(*

		if ret then
		    let
			(* we gobbled up as much as we can, now we have to run it on the remaining expressions.  It's possible that we gobbled too much, so if it failed, we'll try it with one less pattern *)
			val (assigned_patterns', ret') = list_equiv_helper (removeAPduplicates assigned_patterns') (rest1, remaining_exps)
		    in
			if ret' then (* so we're good ... *)
			    (assigned_patterns', ret')
			else (* can we back track a bit - only if we can reduce the match *)
			    case exp1 of
				Exp.TERM (Exp.PATTERN (_,_,patcount)) => 
				let
				    val num_matched = length matched_exps
				    val min_num = PatternProcess.min_patcount patcount
				in
				    if num_matched = min_num then (* we did all we can do, won't match *)
					(assigned_patterns', ret')
				    else (* lets reduce the number of matched by one *)
					let
					    val ((assigned_patterns'', ret''), _) = gobble assigned_patterns (exp1, Util.take(explist2, num_matched-1))
					in
					    
					end
				end
		    end
		else (* we can't do anything here ... *)
		    (assigned_patterns, false)*)
	    end
    in
	list_equiv_helper assigned_patterns (explist1, explist2)
    end

(* Check if two expressions are equivalent *)
and exp_equivalent assigned_patterns (exp1, exp2) = 
    case (exp1, exp2) of
	(Exp.TERM t1, Exp.TERM t2) => terms_equivalent assigned_patterns (t1, t2)
      | (Exp.FUN (Fun.BUILTIN fun1, args1), Exp.FUN (Fun.BUILTIN fun2, args2)) => 
	if fun1 = fun2 then
	    case #operands (Fun.builtin2props fun1) of
		Fun.VARIABLE _ => list_equivalent assigned_patterns (args1, args2)
	      | Fun.FIXED _ => allEquiv exp_equivalent assigned_patterns (args1, args2)
	else
	    (assigned_patterns, false)
      | (Exp.FUN (Fun.INST {classname=classname1,...},args1),
	 Exp.FUN (Fun.INST {classname=classname2,...},args2)) =>
	if classname1=classname2 then
	    allEquiv exp_equivalent assigned_patterns (args1, args2)
	else
	    (assigned_patterns, false)
      | (Exp.FUN _, Exp.FUN _) => (assigned_patterns, false)
      (* need to handle patterns *)
      | (exp1, Exp.TERM (Exp.PATTERN p)) => pattern_equivalent assigned_patterns (p, exp1)
      | (Exp.TERM (Exp.PATTERN p), exp2) => pattern_equivalent assigned_patterns (p, exp2)
      | _ => (assigned_patterns, false)

(* Perform equivalency check on expressions *)
and equiv (exp1, exp2) = 
    let
	val (assigned_patterns, result) = exp_equivalent [] (exp1, exp2)
    in
	result
    end

fun findMatches (exp1, exp2) =
    let
	val (assigned_patterns, result) = exp_equivalent [] (exp1, exp2)
    in
	assigned_patterns
    end

end
