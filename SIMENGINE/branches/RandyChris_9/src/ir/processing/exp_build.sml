structure ExpBuild =
struct

fun var str = Exp.TERM (Exp.SYMBOL (Symbol.symbol str, Property.default_symbolproperty))
fun tvar str = Exp.TERM 
		   (Exp.SYMBOL (Symbol.symbol str, 
				Property.setIterator Property.default_symbolproperty 
						     [(Symbol.symbol "t",Iterator.RELATIVE 0)]))

fun tvar_from_state str = Exp.TERM 
			      (Exp.SYMBOL (Symbol.symbol str, 
					   Property.setScope 
					       (Property.setIterator Property.default_symbolproperty 
								     [(Symbol.symbol "t",Iterator.RELATIVE 0)])
					       (Property.READSTATE (Symbol.symbol "rd_t"))))



fun diff str = Exp.TERM (Exp.SYMBOL (Symbol.symbol str, 
				     Property.setScope
					 (Property.setDerivative 
					      (Property.setIterator 
						   Property.default_symbolproperty 
						   [(Symbol.symbol "t",Iterator.RELATIVE 0)])
					      (1, [Symbol.symbol "t"])
					 )					
					 (Property.WRITESTATE (Symbol.symbol "wr_t"))
				    )
			)


fun initvar str = Exp.TERM (Exp.SYMBOL (Symbol.symbol str, 
					 (Property.setIterator 
					      Property.default_symbolproperty 
					      [(Symbol.symbol "t",Iterator.ABSOLUTE 0)])
					)
			   )

fun initnvar str = Exp.TERM (Exp.SYMBOL (Symbol.symbol str, 
					 (Property.setIterator 
					      Property.default_symbolproperty 
					      [(Symbol.symbol "n",Iterator.ABSOLUTE 0)])
					)
			   )

fun initavar (str, temporal_iterator, spatial_iterators) = 
    Exp.TERM (Exp.SYMBOL (Symbol.symbol str, 
			  (Property.setIterator 
			       Property.default_symbolproperty 
			       ((Symbol.symbol temporal_iterator,Iterator.ABSOLUTE 0)::
				(map (fn(iter)=>(Symbol.symbol iter, Iterator.RELATIVE 0)) spatial_iterators)))
			 )
	     )

fun nextvar str = Exp.TERM (Exp.SYMBOL (Symbol.symbol str, 
					Property.setScope
					    (Property.setIterator 
						 Property.default_symbolproperty 
						 [(Symbol.symbol "n",Iterator.RELATIVE 1)])
					    (Property.WRITESTATE (Symbol.symbol "wr_n"))
				       )
			   )

fun curvar str = Exp.TERM (Exp.SYMBOL (Symbol.symbol str, 
				       Property.setScope
					   (Property.setIterator 
						Property.default_symbolproperty 
						[(Symbol.symbol "n",Iterator.RELATIVE 0)])
					   (Property.READSTATE (Symbol.symbol "rd_n"))
				      )
			  )

fun prevvar str = Exp.TERM (Exp.SYMBOL (Symbol.symbol str, 
				       (Property.setIterator 
					    Property.default_symbolproperty 
					    [(Symbol.symbol "n",Iterator.RELATIVE ~1)])
				      )
			  )

fun relvar (sym, itersym, offset) =
    Exp.TERM (Exp.SYMBOL (sym, 
			  (Property.setIterator 
			       Property.default_symbolproperty 
			       [(itersym, Iterator.RELATIVE offset)])
			 )
	     )

fun int i = Exp.TERM (Exp.INT i);
fun real r = Exp.TERM (Exp.REAL r);
fun bool b = Exp.TERM (Exp.BOOL b);
fun plus l = Exp.FUN (Fun.BUILTIN Fun.ADD, l);
fun sub (a,b) = Exp.FUN (Fun.BUILTIN Fun.SUB, [a, b]);
fun neg v = Exp.FUN (Fun.BUILTIN Fun.NEG, [v]) (*times [int ~1, v]*)
fun divide (a,b) = Exp.FUN (Fun.BUILTIN Fun.DIVIDE, [a, b]);
fun times l = Exp.FUN (Fun.BUILTIN Fun.MUL, l);
fun power (a,b) = Exp.FUN (Fun.BUILTIN Fun.POW, [a, b]);
fun exp v = power (var "e", v)
fun equals (a,b) = Exp.FUN (Fun.BUILTIN Fun.ASSIGN, [a, b]);
infix equals;
fun group l = Exp.FUN (Fun.BUILTIN Fun.GROUP, l)

end
