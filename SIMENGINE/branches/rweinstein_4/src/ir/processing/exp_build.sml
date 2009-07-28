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
fun plus l = Exp.FUN (Fun.BUILTIN Fun.ADD, l);
fun times l = Exp.FUN (Fun.BUILTIN Fun.MUL, l);
fun power (a,b) = Exp.FUN (Fun.BUILTIN Fun.POW, [a, b]);
fun exp v = power (var "e", v)
fun equals (a,b) = Exp.FUN (Fun.BUILTIN Fun.ASSIGN, [a, b]);
fun neg v = times [int ~1, v]
infix equals;

end
