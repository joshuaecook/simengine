structure ExpBuild =
struct

fun svar sym = Exp.TERM (Exp.SYMBOL (sym, Property.default_symbolproperty))
fun var str = svar (Symbol.symbol str)

fun pvar str = 
        Exp.TERM (Exp.SYMBOL (Symbol.symbol (str), 
                              Property.setIsRewriteSymbol Property.default_symbolproperty true))

fun tvar str = Exp.TERM 
		   (Exp.SYMBOL (Symbol.symbol str, 
				Property.setIterator Property.default_symbolproperty 
						     (Symbol.symbol "t",Iterator.RELATIVE 0)))

fun avar str temporal_iterator = Exp.TERM 
		   (Exp.SYMBOL (Symbol.symbol str, 
				Property.setIterator Property.default_symbolproperty 
						     (Symbol.symbol temporal_iterator, Iterator.RELATIVE 0)))


fun ivar str iterator = Exp.TERM 
			 (Exp.SYMBOL (Symbol.symbol str, 
				      Property.setIterator Property.default_symbolproperty iterator))

fun tvar_from_state str = Exp.TERM 
			      (Exp.SYMBOL (Symbol.symbol str, 
					   Property.setScope 
					       (Property.setIterator Property.default_symbolproperty 
								     (Symbol.symbol "t",Iterator.RELATIVE 0))
					       (Property.READSTATE (Symbol.symbol "rd_t"))))

fun event str = Exp.TERM (Exp.SYMBOL (Symbol.symbol str, Property.setIsEvent Property.default_symbolproperty true))

fun diff str = Exp.TERM (Exp.SYMBOL (Symbol.symbol str, 
				     Property.setScope
					 (Property.setDerivative 
					      (Property.setIterator 
						   Property.default_symbolproperty 
						   (Symbol.symbol "t",Iterator.RELATIVE 0))
					      (1, [Symbol.symbol "t"])
					 )					
					 (Property.WRITESTATE (Symbol.symbol "wr_t"))
				    )
			)


fun initvar str = Exp.TERM (Exp.SYMBOL (Symbol.symbol str, 
					 (Property.setIterator 
					      Property.default_symbolproperty 
					      (Symbol.symbol "t",Iterator.ABSOLUTE 0))
					)
			   )

fun initnvar str = Exp.TERM (Exp.SYMBOL (Symbol.symbol str, 
					 (Property.setIterator 
					      Property.default_symbolproperty 
					      (Symbol.symbol "n",Iterator.ABSOLUTE 0))
					)
			   )

(*
fun initavar (str, temporal_iterator, spatial_iterators) = 
    Exp.TERM (Exp.SYMBOL (Symbol.symbol str, 
			  (Property.setIterator 
			       Property.default_symbolproperty 
			       ((Symbol.symbol temporal_iterator,Iterator.ABSOLUTE 0)::
				(map (fn(iter)=>(Symbol.symbol iter, Iterator.RELATIVE 0)) spatial_iterators)))
			 )
	     )
*)
fun state_init_var (sym, iter_sym) = 
    Exp.TERM (Exp.SYMBOL (sym,
			 (Property.setIterator
			      Property.default_symbolproperty
			      (iter_sym, Iterator.ABSOLUTE 0))))

fun state_next_var (sym, iter_sym) = 
    Exp.TERM (Exp.SYMBOL (sym,
			 (Property.setIterator
			      Property.default_symbolproperty
			      (iter_sym, Iterator.RELATIVE 1))))


fun diff_state_var (sym, iter_sym) =
    Exp.TERM (Exp.SYMBOL (sym,
			  Property.setScope
			      (Property.setDerivative 
				   (Property.setIterator 
					Property.default_symbolproperty 
					(iter_sym,Iterator.RELATIVE 0))
				   (1, [iter_sym])
			      )					
			      (Property.WRITESTATE (Symbol.symbol ("wr_" ^ (Symbol.name iter_sym))))
			 )
	     )

fun var_with_iter (sym, iter) =
    Exp.TERM (Exp.SYMBOL (sym,
			  Property.setIterator
			      Property.default_symbolproperty
			      iter))

fun spacevar_with_iter (sym, iter, space) =
    Exp.TERM (Exp.SYMBOL (sym,
			  Property.setSpace
			      (Property.setIterator
				   Property.default_symbolproperty
				   iter)
			      space))

fun spacevar (sym, space) =
    Exp.TERM (Exp.SYMBOL (sym,
			  Property.setSpace
			      Property.default_symbolproperty
			      space))


fun itersvar sym = Exp.TERM (Exp.SYMBOL (sym,
					 Property.setScope Property.default_symbolproperty Property.ITERATOR))
fun itervar str = itersvar (Symbol.symbol str)

fun relitervar (str,ind) = Exp.TERM (Exp.SYMBOL (Symbol.symbol str,
					   Property.setScope 
					       (Property.setIterator 
						    Property.default_symbolproperty 
						    (Symbol.symbol str, Iterator.RELATIVE ind))
					       Property.ITERATOR))

fun nextvar str = Exp.TERM (Exp.SYMBOL (Symbol.symbol str, 
					Property.setScope
					    (Property.setIterator 
						 Property.default_symbolproperty 
						 (Symbol.symbol "n",Iterator.RELATIVE 1))
					    (Property.WRITESTATE (Symbol.symbol "n"))
				       )
			   )

fun nextavar str temporal_iterator = Exp.TERM (Exp.SYMBOL (Symbol.symbol str, 
							   (Property.setIterator 
								Property.default_symbolproperty 
								(temporal_iterator,Iterator.RELATIVE 1))
							  )
					      )
				     
fun curvar str = Exp.TERM (Exp.SYMBOL (Symbol.symbol str, 
				       Property.setScope
					   (Property.setIterator 
						Property.default_symbolproperty 
						(Symbol.symbol "n",Iterator.RELATIVE 0))
					   (Property.READSTATE (Symbol.symbol "n"))
				      )
			  )

fun prevvar str = Exp.TERM (Exp.SYMBOL (Symbol.symbol str, 
				       (Property.setIterator 
					    Property.default_symbolproperty 
					    (Symbol.symbol "n",Iterator.RELATIVE ~1))
				      )
			  )

fun relvar (sym, itersym, offset) =
    Exp.TERM (Exp.SYMBOL (sym, 
			  (Property.setIterator 
			       Property.default_symbolproperty 
			       (itersym, Iterator.RELATIVE offset))
			 )
	     )

fun int i = Exp.TERM (Exp.INT i);
fun real r = Exp.TERM (Exp.REAL r);
fun bool b = Exp.TERM (Exp.BOOL b);
fun str s = Exp.TERM (Exp.STRING s);
fun uniform_rand () = Exp.TERM (Exp.RANDOM (Exp.UNIFORM, Space.scalar));
fun normal_rand () = Exp.TERM (Exp.RANDOM (Exp.NORMAL, Space.scalar));
fun frac (n,d) = Exp.TERM (Exp.RATIONAL (n, d))
fun plus l = Exp.FUN (Fun.BUILTIN Fun.ADD, l);
fun sub (a,b) = Exp.FUN (Fun.BUILTIN Fun.SUB, [a, b]);
fun neg v = Exp.FUN (Fun.BUILTIN Fun.NEG, [v]) (*times [int ~1, v]*)
fun divide (a,b) = Exp.FUN (Fun.BUILTIN Fun.DIVIDE, [a, b]);
fun times l = Exp.FUN (Fun.BUILTIN Fun.MUL, l);
fun power (a,b) = Exp.FUN (Fun.BUILTIN Fun.POW, [a, b]);
fun recip a = power (a, int (~1))
fun sqrt a = Exp.FUN (Fun.BUILTIN Fun.SQRT, [a])
fun floor a = Exp.FUN (Fun.BUILTIN Fun.FLOOR, [a])
fun ceiling a = Exp.FUN (Fun.BUILTIN Fun.CEILING, [a])
fun round a = Exp.FUN (Fun.BUILTIN Fun.ROUND, [a])
fun square a = power (a, int 2)
fun norm l = sqrt (plus (map square l))
fun exp v = (*power (var "e", v)*) Exp.FUN (Fun.BUILTIN Fun.EXP, [v])
fun equals (a,b) = Exp.FUN (Fun.BUILTIN Fun.ASSIGN, [a, b]);
infix equals;
fun cond (p, q, r) = Exp.FUN (Fun.BUILTIN Fun.IF, [p, q, r]);
fun atan2 (a,b) = Exp.FUN (Fun.BUILTIN Fun.ATAN2, [a,b])
fun re z = Exp.FUN (Fun.BUILTIN Fun.RE, [z])
fun im z = Exp.FUN (Fun.BUILTIN Fun.IM, [z])
fun land l = Exp.FUN (Fun.BUILTIN Fun.AND, l)
fun lor l = Exp.FUN (Fun.BUILTIN Fun.OR, l)
fun not v = Exp.FUN (Fun.BUILTIN Fun.NOT, [v])
fun arg z = atan2 (re z, im z)
fun cos x = Exp.FUN (Fun.BUILTIN Fun.COS, [x])
fun sin x = Exp.FUN (Fun.BUILTIN Fun.SIN, [x])
fun log x = Exp.FUN (Fun.BUILTIN Fun.LOG, [x])
fun logn (b, x) = Exp.FUN (Fun.BUILTIN Fun.LOG, [b, x])
fun complex (a,b) = Exp.TERM (Exp.COMPLEX (a,b))
fun complex_fun (a,b) = Exp.FUN (Fun.BUILTIN Fun.COMPLEX, [a,b])
fun complex_logn (b, z) = 
    let
	val r = re z
	val i = im z
    in
	complex_fun 
	    (plus [divide (times [arg b, arg z],
			   plus [arg (square b), times [frac(1,4), square (log (square b))]]), 
		   divide (times [log (square b), log (plus [square i, square r])], 
			   times [int 4, plus [square (arg b), times [frac(1,4), square (log (square b))]]])],
	     sub (divide (times [arg z, log (square b)],
			  times [int 2, plus [square (arg b), times [frac(1,4), square (log (square b))]]]),
		  divide (times [arg b, log (plus [square i, square r])],
			  times [int 2, plus [square (arg b), times [frac(1,4), square (log (square b))]]])))
    end

fun tuple l = Exp.TERM (Exp.TUPLE l)
fun lambda (arg, body) = Exp.META(Exp.LAMBDA{arg=Symbol.symbol arg, body=body})
fun sequence (exps) = Exp.META(Exp.SEQUENCE exps)
fun apply (func, arg) = Exp.META(Exp.APPLY{func=func, arg=arg})
fun map (func, args) = Exp.META(Exp.MAP{func=func, args=args})
fun explist (args) = Exp.CONTAINER(Exp.EXPLIST args)

fun error msg = (Logger.log_error (Printer.$ msg); DynException.setErrored)
fun exp2term (Exp.TERM t) = t
  | exp2term _ = (error "Invalid expression used where terminal was required";
		  Exp.NAN)
fun range [low, high] = Exp.TERM (Exp.RANGE {low=exp2term low, high=exp2term high, step=exp2term (int 1)})
  | range [low, step, high] = Exp.TERM (Exp.RANGE {low=exp2term low, high=exp2term high, step=exp2term step})
  | range _ = (error "Range expects either two arguments (low, high) or three (low, step, high)";
	       Exp.TERM (Exp.NAN))
fun subref (exp, subspace) = Exp.CONVERSION (Exp.SUBREF (exp, subspace))

end
