signature PATTERN =
sig

    (* print a pattern *)
    val toString : Exp.pattern -> string

end
structure Pattern =
struct

val i2s = Util.i2s

fun toString ((symbol, (predicate_name, predicate_fun), patcount):
		 (Symbol.symbol * Exp.predicate * Exp.patterncount)) =
    let
	val name = Symbol.name symbol
	val typestr = "{"^predicate_name^"}"

	val countstr = case patcount 
			of Exp.ONE => "_"
			 | Exp.ONE_OR_MORE => "__"
			 | Exp.ZERO_OR_MORE  => "___"
			 | Exp.SPECIFIC_COUNT i => "_{"^(i2s i)^"}"
			 | Exp.SPECIFIC_RANGE (i1, i2) => "_{"^(i2s i1)^":"^(i2s i2)^"}"
    in
	name ^ typestr ^ countstr
    end


end
