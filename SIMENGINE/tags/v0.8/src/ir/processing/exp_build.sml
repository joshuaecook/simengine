structure ExpBuild =
struct

fun var str = Exp.TERM (Exp.SYMBOL (Symbol.symbol str, Property.default_symbolproperty))
fun tvar str = Exp.TERM 
		   (Exp.SYMBOL (Symbol.symbol str, 
				Property.setIterator Property.default_symbolproperty 
						     [(Symbol.symbol "t",Iterator.RELATIVE 0)]))

fun diff str = Exp.TERM (Exp.SYMBOL (Symbol.symbol str, 
				     Property.setDerivative 
					 (Property.setIterator 
					      Property.default_symbolproperty 
					      [(Symbol.symbol "t",Iterator.RELATIVE 0)])
					 (1, [Symbol.symbol "t"])
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

fun initavar (str, iter) = 
    Exp.TERM (Exp.SYMBOL (Symbol.symbol str, 
			  (Property.setIterator 
			       Property.default_symbolproperty 
			       [(Symbol.symbol iter,Iterator.ABSOLUTE 0)])
			 )
	     )

fun nextvar str = Exp.TERM (Exp.SYMBOL (Symbol.symbol str, 
					 (Property.setIterator 
					      Property.default_symbolproperty 
					      [(Symbol.symbol "n",Iterator.RELATIVE 1)])
					)
			   )

fun curvar str = Exp.TERM (Exp.SYMBOL (Symbol.symbol str, 
				       (Property.setIterator 
					    Property.default_symbolproperty 
					    [(Symbol.symbol "n",Iterator.RELATIVE 0)])
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
fun plus l = Exp.FUN (Symbol.symbol "add", l);
fun times l = Exp.FUN (Symbol.symbol "mul", l);
fun power (a,b) = Exp.FUN (Symbol.symbol "pow", [a, b]);
fun exp v = power (var "e", v)
fun equals (a,b) = Exp.FUN (Symbol.symbol "assign", [a, b]);
fun neg v = Exp.FUN (Symbol.symbol "mul", [int ~1, v])
infix equals;

end
