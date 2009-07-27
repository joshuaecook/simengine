structure Fun =
struct

type iteratorname = Symbol.symbol
type instname = Symbol.symbol

datatype instform = FUNCTIONAL
		  | FUNCTIONAL_BY_REF
		  | STATELY of {reads: (iteratorname * instname) list,
				writes: (iteratorname * instname) list}
type dimlist = int list

type instproperties =
     {dim: dimlist option,
      sourcepos: PosLog.pos option,
      realname: Symbol.symbol option,
      form: instform option}

datatype funtype = BUILTIN of Symbol.symbol
		 | INST of {classname:Symbol.symbol, 
			    instname:Symbol.symbol, 
			    props:instproperties}



datatype fix = INFIX | PREFIX | POSTFIX | MATCH


(* Precedence Table (based on C++)
   1: Module scope
   2: Parenthesis, post-increment/decrement
   3: Logical negation, complement, pre-increment/decrement
   4: Power
   5: Multiplication, Division, Modulus
   6: Addition, Subtraction
   7: Bit shift
   8: Aritmetic Comparisons (LE, GE, LT, GT)
   9: Logical Comparisons (EQ, NE)
   10: Bitwise AND
   11: Bitwise XOR
   12: Bitwise OR
   13: Logical AND
   14: Logical OR
   15: IF
   16: Assignment
   17: Comma *)

datatype operands = FIXED of int (* the required number of arguments *)
		  | VARIABLE of int (* the default value *)

type op_props = {name: string,
		 operands: operands,
		 precedence: int,
		 commutative: bool,
		 associative: bool,
		 text: (string * fix),
		 C: (string * fix)}


(* Create new Symbol Table *)

fun add2optable (opTable, name, props) = 
    SymbolTable.enter (opTable, Symbol.symbol name, props)

val opTable = 
    let
	val opTable = SymbolTable.empty

	val op_entries = [
	     {name="add",
	      operands=VARIABLE 0,
	      precedence=6,
	      commutative=true,
	      associative=true,
	      text=("+",INFIX),
	      C=("+",INFIX)},
	     {name="sub",
	      operands=FIXED 2,
	      precedence=6,
	      commutative=false,
	      associative=false,
	      text=("-",INFIX),
	      C=("-",INFIX)},
	     {name="neg",
	      operands=FIXED 1,
	      precedence=6,
	      commutative=false,
	      associative=false,
	      text=("-",PREFIX),
	      C=("-",PREFIX)},
	     {name="not",
	      operands=FIXED 1,
	      precedence=3,
	      commutative=false,
	      associative=false,
	      text=("!",PREFIX),
	      C=("!",PREFIX)},
	     {name="divide",
	      operands=FIXED 2,
	      precedence=5,
	      commutative=false,
	      associative=false,
	      text=("/",INFIX),
	      C=("/",INFIX)},
	     {name="modulus",
	      operands=FIXED 2,
	      precedence=5,
	      commutative=false,
	      associative=false,
	      text=("%",INFIX),
	      C=("%",INFIX)},
	     {name="deriv",
	      operands=FIXED 2,
	      precedence=1,
	      commutative=false,
	      associative=false,
	      text=("D",PREFIX),
	      C=("Derivative",PREFIX)},
	     {name="logn",
	      operands=FIXED 2,
	      precedence=1,
	      commutative=false,
	      associative=false,
	      text=("log_$1($2)",MATCH),
	      C=("(log($1)/log($2))",MATCH)},
	     {name="atan2",
	      operands=FIXED 2,
	      precedence=1,
	      commutative=false,
	      associative=false,
	      text=("atan2",PREFIX),
	      C=("atan2",PREFIX)},
	     {name="gt",
	      operands=FIXED 2,
	      precedence=8,
	      commutative=false,
	      associative=false,
	      text=(">",INFIX),
	      C=(">",INFIX)},
	     {name="lt",
	      operands=FIXED 2,
	      precedence=8,
	      commutative=false,
	      associative=false,
	      text=("<",INFIX),
	      C=("<",INFIX)},
	     {name="ge",
	      operands=FIXED 2,
	      precedence=8,
	      commutative=false,
	      associative=false,
	      text=(">=",INFIX),
	      C=(">=",INFIX)},
	     {name="le",
	      operands=FIXED 2,
	      precedence=8,
	      commutative=false,
	      associative=false,
	      text=(">=",INFIX),
	      C=(">=",INFIX)},
	     {name="eq",
	      operands=FIXED 2,
	      precedence=9,
	      commutative=false,
	      associative=false,
	      text=("==",INFIX),
	      C=("==",INFIX)},
	     {name="neq",
	      operands=FIXED 2,
	      precedence=9,
	      commutative=false,
	      associative=false,
	      text=("<>",INFIX),
	      C=("!=",INFIX)},
	     {name="if",
	      operands=FIXED 3,
	      precedence=15,
	      commutative=false,
	      associative=false,
	      text=("If $1 then $2 else $3", MATCH),
	      C=("$1 ? $2 : $3",MATCH)},
	     {name="and",
	      operands=VARIABLE 1,
	      precedence=13,
	      commutative=false,
	      associative=false,
	      text=("&&",INFIX),
	      C=("&&",INFIX)},
	     {name="or",
	      operands=VARIABLE 0,
	      precedence=14,
	      commutative=false,
	      associative=false,
	      text=("||",INFIX),
	      C=("||",INFIX)},
	     {name="mul",
	      operands=VARIABLE 1,
	      precedence=5,
	      commutative=true,
	      associative=true,
	      text=("*",INFIX),
	      C=("*",INFIX)},
	     {name="log", (* natural log *)
	      operands=FIXED 1,
	      precedence=1,
	      commutative=false,
	      associative=false,
	      text=("log",PREFIX),
	      C=("log",PREFIX)},
	     {name="pow",
	      operands=FIXED 2,
	      precedence=4,
	      commutative=false,
	      associative=false,
	      text=("^",INFIX),
	      C=("pow($1,$2)",MATCH)},
	     {name="assign",
	      operands=FIXED 2,
	      precedence=16,
	      commutative=false,
	      associative=false,
	      text=(" = ",INFIX),
	      C=(" = ",INFIX)},
	     {name="group",
	      operands=FIXED 1,
	      precedence=1,
	      commutative=false,
	      associative=false,
	      text=("",PREFIX),
	      C=("",PREFIX)}
	]

	val op_functions = ["abs", "exp", "sqrt", "log", "log10",
			    "deg2rad", "rad2deg", "sin", "cos",
			    "tan", "sinh", "cosh", "tanh", "asin",
			    "acos", "atan", "asinh", "acosh", "atanh",
			    "csc", "sec", "cot", "csch", "sech",
			    "coth", "ascs", "asec", "acot", "acsch",
			    "asech", "acoth"]

	val optable_with_entries = 
	    foldl (fn(entry, opTable)=> 
		 let
		     val {name, ...} = entry
		 in
		     add2optable (opTable, name, entry)
		 end)
		  opTable (* blank opTable *)
		  op_entries (* all the pre-defined operations *)

	val optable_with_unary_functions =
	    foldl (fn(name, opTable)=>
		     let
			 val entry = {name=name,
				      operands=FIXED 1,
				      precedence=1,
				      commutative=false,
				      associative=false,
				      text=(name, PREFIX),
				      C=(name, PREFIX)}
		     in
			 add2optable (opTable, name, entry)
		     end)
		  optable_with_entries
		  op_functions
    in
	optable_with_unary_functions
    end
    

fun builtin2props f : op_props = 
    case SymbolTable.look (opTable, f)
     of SOME v => v
      | NONE => (print ("Can't handle operation '" ^ (Symbol.name f) ^ "'\n");
		 DynException.stdException(("No builtin function with name '"^(Symbol.name f)^"' defined"), "Fun.fun2props", Logger.INTERNAL))


fun fun2textstrnotation f =
    case f 
	 of BUILTIN v => 
	    let
		val {text as (str, notation),...} = builtin2props v
	    in
		(str, notation)
	    end
	  | INST {classname,...} => (Symbol.name classname, PREFIX)

fun fun2cstrnotation f =
    case f 
	 of BUILTIN v => 
	    let
		val {C as (str, notation),...} = builtin2props v
	    in
		(str, notation)
	    end
	  | INST {classname,...} => (Symbol.name classname, PREFIX)


(*	
    case (Symbol.name f) of
	"PLUS" => ("+", INFIX)
      | "TIMES" => ("*", INFIX)
      | "POWER" => ("^", INFIX)
      | "LOG" => ("Log", PREFIX)
      | "GROUP" => ("", PREFIX) (* just a grouping operator *)
      | "EQUALS" => ("==", INFIX)
      | v => (v, PREFIX)
*)




end
