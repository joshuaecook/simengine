signature EXPSPACE =
sig
    
    (* given an exp, return the space *)
    val expToSpace : Exp.exp -> Space.space
    (* same as above but logs a user error if a space exception occurs *)
    val expToSpace_UserError: Exp.exp -> Space.space
    (* same as above but returns a space option *)
    val expToSpaceOption: Exp.exp -> Space.space option

    (* Space exception when space propagation fails *)
    datatype space_exception_type = 
	     FunException of {spaces: Space.space list, exp: Exp.exp}
	   | TermException of Exp.exp
	   | SubRefException of {subspace: SubSpace.subspace, exp: Exp.exp}
	   | ReshapeException of {orig_space: Space.space, new_space: Space.space, exp: Exp.exp}
	   | SubspaceException of SubSpace.subspace
	   | UnknownException of Exp.exp
    exception SpaceException of space_exception_type

end
structure ExpSpace : EXPSPACE =
struct

(* Space exception when space propagation fails *)
datatype space_exception_type = 
	 FunException of {spaces: Space.space list, exp: Exp.exp}
       | TermException of Exp.exp
       | SubRefException of {subspace: SubSpace.subspace, exp: Exp.exp}
       | ReshapeException of {orig_space: Space.space, new_space: Space.space, exp: Exp.exp}
       | SubspaceException of SubSpace.subspace
       | UnknownException of Exp.exp
exception SpaceException of space_exception_type


local
    open Space
    open SpaceProcess

    fun termToReal (Exp.REAL v) = v
      | termToReal (Exp.INT v) = Real.fromInt v
      | termToReal t = (Logger.log_error (Printer.$("Can't convert term "^(ExpPrinter.exp2str (Exp.TERM t))^" to real"));
			DynException.setErrored();
			0.0)

    val head = ExpTraverse.head
    val level = ExpTraverse.level

    val e2s = ExpPrinter.exp2str
    val s2s = Space.toString
    val ss2s = SubSpace.toString
in
fun expToSpace exp =
    (case exp of
	 Exp.TERM t => ((case t of
			    Exp.RATIONAL _ => scalar
			  | Exp.INT _ => scalar
			  | Exp.REAL _ => scalar
			  | Exp.BOOL _ => scalar
			  | Exp.COMPLEX _ => scalar
			  | Exp.FILEREF (fe, space) => space
			  | Exp.TUPLE tuple => collection (map (expToSpace o Exp.TERM) tuple)
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
			handle _ => raise SpaceException (TermException exp))
       | Exp.FUN (f,_) => 
	 let
	     val codomain = #codomain (FunProcess.fun2props f)
	     val spaces = map expToSpace (level exp)
	 in
	     (codomain spaces)
	     handle SpaceException e => raise SpaceException e
		  | _ => raise (SpaceException (FunException {exp=exp, spaces=spaces}))
	 end
       | Exp.CONTAINER c => (case c of
				 Exp.MATRIX m => fromMatrixDims (Matrix.size m)
			       | Exp.ARRAY a => fromVectorDim (Array.length a)
			       | Exp.ASSOC table => collection (map expToSpace (SymbolTable.listItems table))
			       | Exp.EXPLIST exps => collection (map expToSpace exps))
       | Exp.CONVERSION c => (case c of
				  Exp.SUBREF (exp', subspace) => 
				  (let
				       val space' = expToSpace exp'
				       (*val _ = Util.log ("space': " ^ (s2s space'))*)
				       val space'' = sub space' subspace
				   (*val _ = Util.log ("space'': " ^ (s2s space''))*)
				   in
				       space''
				   end
				   handle SpaceException e => raise (SpaceException e)
					| e => raise (SpaceException (SubRefException {subspace=subspace,
										       exp=exp'})))
				| Exp.RESHAPE (exp', space) =>
				  (let
				       val space' = expToSpace exp'
				       val _ = if (Space.size space) = (Space.size space') then
						   ()
					       else
						   raise (SpaceException (ReshapeException {exp=exp', 
											    orig_space=space', 
											    new_space=space}))
				   in
				       space
				   end
				   handle SpaceException e => raise (SpaceException e))
				| Exp.SUBSPACE subspace => raise (SpaceException (SubspaceException subspace))
			     )
       | Exp.META _ => scalar (* have no idea what to do here... *))
    handle SpaceException e => raise SpaceException e
	 | _ => raise (SpaceException (UnknownException exp))


val _ = Inst.expToSpace := expToSpace

(* adaption of expToSpace to throw user errors instead of exceptions*)
fun expToSpace_UserError exp =
    expToSpace exp
    handle SpaceException e =>
	   ((case e of
		TermException exp => 
		Logger.log_error (Printer.$("Invalid dimensions present in term: " ^ (e2s exp)))
	      | FunException {exp, spaces} => 
		Logger.log_error (Printer.SUB[Printer.$("Invalid dimensions present in arguments to expression"),
					      Printer.SUB[Printer.$("Exp: " ^ (e2s exp)),
							  Printer.$("Dimensions: " ^ (Util.list2str s2s spaces))]])
	      | SubRefException {exp, subspace} => 
		Logger.log_error (Printer.$("Invalid dimension when subreferencing "^(e2s exp)^" by " ^ (ss2s subspace)))
	      | ReshapeException {exp, orig_space, new_space} => 
		Logger.log_error (Printer.$("Can not reshape "^(e2s exp)^" with dimension "^(s2s orig_space)^
					    " to " ^ (s2s new_space)))
	      | SubspaceException subspace =>
		Logger.log_error (Layout.str "Unexpected subspace found as an expression type")
	      | UnknownException exp => 
		Logger.log_error (Printer.$("Invalid dimensions present in exp: " ^ (e2s exp)))
	    );
	    DynException.setErrored();
	    Space.emptyCollection)
	 | e => DynException.checkpoint ("ExpSpace.expToSpace ["^(e2s exp)^"]") e

(* return NONE when an error occurs*)
fun expToSpaceOption exp = 
    SOME (expToSpace exp)
    handle _ => NONE

end

(* Assign the method to SpaceProcess *)
val _ = SpaceProcess.setExpToSpace expToSpace

end
