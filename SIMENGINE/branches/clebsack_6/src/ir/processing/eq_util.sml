structure EqUtil = 
struct

val log = Util.log

fun eq2exp (eq:DOF.eq) : Exp.exp = 
    let
	val {eq_type, sourcepos, lhs, rhs} = eq
    in
	ExpBuild.equals (Exp.TERM lhs, rhs)
    end

fun isInstance (eq: DOF.eq) = case #eq_type eq of DOF.INSTANCE _ => true | _ => false
fun isDifference (eq: DOF.eq) = case #eq_type eq of DOF.DIFFERENCE_EQ _ => true | _ => false
fun isDerivative (eq: DOF.eq) = case #eq_type eq of DOF.DERIVATIVE_EQ _ => true | _ => false
fun isIntermediate (eq: DOF.eq) = case #eq_type eq of DOF.INTERMEDIATE_EQ => true | _ => false
fun isInitialValue (eq: DOF.eq) = case #eq_type eq of DOF.INITIAL_VALUE _ => true | _ => false

fun getInstances (eqs:DOF.eq list) = List.filter isInstance eqs
fun getDifferenceEqs (eqs:DOF.eq list) = List.filter isDifference eqs
fun getDerivativeEqs (eqs:DOF.eq list) = List.filter isDerivative eqs
fun getIntermediateEqs (eqs:DOF.eq list) = List.filter isIntermediate eqs
fun getInitialValueEqs (eqs:DOF.eq list) = List.filter isInitialValue eqs

fun eq2statesize (eq:DOF.eq as {eq_type, lhs,...}) = 
    case (eq_type, lhs)
     of (DOF.INSTANCE {name,classname, offset}, lhs) => 
	class2statesize (CurrentModel.classname2class classname)
      | (_, lhs) => Term.termSize lhs

and class2statesize class =
    let
	val size = Util.sum (map eq2statesize (!(#eqs class)))
	(*val _ = log ("State size for class '"^(Symbol.name (#name class))^"' calculated to be " ^ (Util.i2s size))*)
    in
	size
    end

fun eq2statesizeByIterator iter (eq:DOF.eq as {eq_type, lhs,...}) = 
    case (eq_type, lhs)
     of (DOF.INSTANCE {name,classname, offset}, lhs) => 
	class2statesizeByIterator iter (CurrentModel.classname2class classname)
      | (_, lhs) => Term.termSizeByIterator iter lhs

and class2statesizeByIterator iter class =
    let
	val size = Util.sum (map (eq2statesizeByIterator iter) (!(#eqs class)))
	(*val _ = log ("State size for class '"^(Symbol.name (#name class))^"' calculated to be " ^ (Util.i2s size))*)
    in
	size
    end


fun order_eqs (eqs: DOF.eq list) : DOF.eq list =
    let
	(* filter equations and order them *)
	val init_eqs = getInitialValueEqs eqs
	val state_eqs = (getDerivativeEqs eqs) @ (getDifferenceEqs eqs)
	val filtered_eqs = (getInstances eqs) @ (getIntermediateEqs eqs)
			   
	fun eq2depends (eq as {lhs, rhs, ...}) = ExpProcess.exp2symbols rhs
	fun eq2writes (eq as {lhs, rhs,...}) = ExpProcess.exp2symbols (Exp.TERM lhs)
	val symbol_map = map (fn(eq, i)=>(i, eq2writes eq, eq2depends eq)) (Util.addCount filtered_eqs)

	fun symlist2str symlist = 
	    "[" ^ (String.concatWith ", " (map Symbol.name symlist)) ^ "]"

	fun log_map symmap = 	    
	    (log "Symbol Maps";
	     app (fn(i, lhs, rhs)=> log ((Util.i2s i) ^ ": " ^ (symlist2str lhs)  ^ " -> " ^ (symlist2str rhs))) symmap;
	     log "")

	val symbols_to_check = Util.intersection (Util.flatmap eq2writes filtered_eqs,
						  Util.flatmap eq2depends filtered_eqs)

	fun positions symmap sym = 
	    let
		val lhs = case (List.find (fn((_, lhs, _),_)=> List.exists (fn(s)=>s=sym) lhs) (Util.addCount symmap)) of
			      SOME (_, i) => SOME i
			    | NONE => NONE
		val rhs = case (List.find (fn((_, _, rhs),_)=> List.exists (fn(s)=>s=sym) rhs) (Util.addCount symmap)) of
			      SOME (_, i) => SOME i
			    | NONE => NONE
	    in
		(lhs, rhs)
	    end
	    
	fun check_symmap symmap =
	    List.exists (fn(sym)=> case positions symmap sym of 
				       (SOME i1, SOME i2) => i2 < i1
				     | _ => false) symbols_to_check

	fun moveRow symmap (r1, r2) =
	    let
		val extracted_row = Util.nth (symmap, r1)
		val remaining_rows = if r1 = 0 then
					 Util.tl (symmap)
				     else if r1 = length symmap - 1 then
					 Util.take (symmap, r1)
				     else 
					 (Util.take (symmap, r1)) @
					 (Util.drop (symmap, r1+1))
	    in
		Util.take (remaining_rows, r2) @ [extracted_row] @ (Util.drop (remaining_rows, r2))
	    end
	    
	fun reorder_symmap symbol_map = 
	    foldl
		(fn(sym, map')=>
		   case positions map' sym of
		       (SOME i1, SOME i2) => 
		       if i2 < i1 then
			   moveRow map' (i1, i2)
		       else
			   map'
		     | _ => map')
		symbol_map
		symbols_to_check
		
	(*val _ = log_map symbol_map
	val _ = log_map (reorder_symmap symbol_map)*)
			      
	fun reorder_eqs symmap =
	    map (fn(i,_,_) => List.nth (filtered_eqs, i))symmap

	fun align_orders (eqs1:DOF.eq list, eqs2:DOF.eq list) : (DOF.eq list * DOF.eq list) = 
	    let
		fun error str = DynException.stdException(("Found state init and state equation discrepancy: " ^ str), "EqUtil.order_eqs.align_orders", Logger.INTERNAL)

		val _ = if (length eqs1) = (length eqs2) then
			    ()
			else
			    error "lengths not matching"

		fun eq2symbol {lhs,...} = List.hd (ExpProcess.exp2symbols (Exp.TERM lhs))

		val listpair = 
		    map 
			(fn(eq1)=> case (List.find (fn(eq2)=> (eq2symbol eq2) = (eq2symbol eq1)) eqs2) of
				       SOME v => (eq1, v)
				     | NONE => error ("Initial value eq '"^(ExpProcess.exp2str (eq2exp eq1))^"' does not have a matching state equation")) 
			eqs1
	    in
		ListPair.unzip listpair
	    end

	val (init_eqs', state_eqs') = align_orders (init_eqs, state_eqs)

    in
	init_eqs' @ 
	(if check_symmap symbol_map then
	     let 
		 val map' = reorder_symmap symbol_map
	     in
		 if check_symmap map' then
		     DynException.stdException("Can't handle possible circular dependency", "EqUtil.order_eqs", Logger.INTERNAL)
		 else
		     reorder_eqs map'
	     end
	 else
	     filtered_eqs) @
	state_eqs'
    end
    
fun renameSym (orig_sym, new_sym) eq =
    let
	val {eq_type, sourcepos, lhs, rhs} = eq
    in
	{eq_type=eq_type,
	 sourcepos=sourcepos,
	 lhs=ExpProcess.exp2term (ExpProcess.renameSym (orig_sym, new_sym) (Exp.TERM lhs)),
	 rhs=ExpProcess.renameSym (orig_sym, new_sym) rhs}
    end
    
fun hasInstanceIter offset_list itersym = 
    List.exists (fn(sym,_)=> sym=itersym) offset_list

fun getInstanceIterOffset offset_list itersym =
    case List.find (fn(sym, offset)=> sym=itersym) offset_list of
	SOME (_, offset) => offset
      | NONE => DynException.stdException(("Expected iterator '"^(Symbol.name itersym)^"' to exist"),
					  "EqUtil.getInstanceIterOffset", Logger.INTERNAL)

(*
fun log_eqs (header, eqs) = 
    (log "";
     log header;
     log ("--------------------------------------");
     (app (fn(e)=>log (DOFPrinter.eq2str e)) eqs);
     log ("--------------------------------------"))
*)
end
