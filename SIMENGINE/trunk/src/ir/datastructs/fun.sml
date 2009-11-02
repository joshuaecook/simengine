structure Fun =
struct

type iteratorname = Symbol.symbol
type instname = Symbol.symbol

(*datatype instform = FUNCTIONAL
		  | FUNCTIONAL_BY_REF
		  | STATELY of {reads: (iteratorname * instname) list,
				writes: (iteratorname * instname) list}*)
type dimlist = int list

type instproperties =
     {dim: dimlist option,
      sourcepos: PosLog.pos option,
      realclassname: Symbol.symbol option,
      realinstname: Symbol.symbol option,
      inline: bool(*,
      form: instform option*)}

(* handle instance properties *)
val emptyinstprops = {dim=NONE,
		      sourcepos=NONE,
		      realclassname=NONE,
		      realinstname=NONE,
		      inline=false}

fun getDim (props : instproperties) = #dim props
fun getSourcePos (props : instproperties)= #sourcepos props
fun getRealClassName (props : instproperties)= #realclassname props
fun getRealInstName (props : instproperties)= #realinstname props
fun isInline (props : instproperties)= #inline props

fun setDim (props as {dim, sourcepos, realclassname, realinstname, inline} : instproperties) sym : instproperties = 
    {dim=SOME sym,
     sourcepos=sourcepos,
     realclassname=realclassname,
     realinstname=realinstname,
     inline=inline}
															 
fun setSourcePos (props as {dim, sourcepos, realclassname, realinstname, inline} : instproperties) sym : instproperties = 
    {dim=dim,
     sourcepos=SOME sym,
     realclassname=realclassname,
     realinstname=realinstname,
     inline=inline}
															 
fun setRealClassName (props as {dim, sourcepos, realclassname, realinstname, inline} : instproperties) sym : instproperties = 
    {dim=dim,
     sourcepos=sourcepos,
     realclassname=SOME sym,
     realinstname=realinstname,
     inline=inline}
															 
fun setRealInstName (props as {dim, sourcepos, realclassname, realinstname, inline} : instproperties) sym : instproperties = 
    {dim=dim,
     sourcepos=sourcepos,
     realclassname=realclassname,
     realinstname=SOME sym,
     inline=inline}

fun setInline (props as {dim, sourcepos, realclassname, realinstname, inline} : instproperties) sym : instproperties = 
    {dim=dim,
     sourcepos=sourcepos,
     realclassname=realclassname,
     realinstname=realinstname,
     inline=sym}
														 

(* operation list *)
datatype operation = 
	 (* arithmetic operations *)
	 ADD | SUB | NEG | MUL | DIVIDE | MODULUS | POW | 
	 (* unary arithmetic operations *)
	 ABS | SQRT | DEG2RAD | RAD2DEG | 
	 (* logorithmic functions *)
	 LOGN | EXP | LOG | LOG10 | 
	 (* trigonometric functions *)
	 SIN | COS | TAN | CSC | SEC | COT |
	 ASIN | ACOS | ATAN | ATAN2 | ACSC | ASEC | ACOT |
	 SINH | COSH | TANH | CSCH | SECH | COTH |
	 ASINH | ACOSH | ATANH | ACSCH | ASECH | ACOTH |
	 (* logical operations *)
	 NOT | AND | OR | 
	 (* comparison operations *)
	 GT | LT | GE | LE | EQ | NEQ |
	 (* reduction operations *)
	 RADD | RMUL | RAND | ROR | 
	 (* special purpose operations *)
	 DERIV | IF | ASSIGN | GROUP | NULL

(* create a full op list *)
val op_list = 
    [(* arithmetic operations *)
     ADD, SUB, NEG, MUL, DIVIDE, MODULUS, POW, 
     (* unary arithmetic operations *)
     ABS, SQRT, DEG2RAD, RAD2DEG, 
     (* logorithmic functions *)
     LOGN, EXP, LOG, LOG10, 
     (* trigonometric functions *)
     SIN, COS, TAN, CSC, SEC, COT,
     ASIN, ACOS, ATAN, ATAN2, ACSC, ASEC, ACOT,
     SINH, COSH, TANH, CSCH, SECH, COTH,
     ASINH, ACOSH, ATANH, ACSCH, ASECH, ACOTH,
     (* logical operations *)
     NOT, AND, OR, 
     (* comparison operations *)
     GT, LT, GE, LE, EQ, NEQ,
     (* reduction operations *)
     RADD, RMUL, RAND, ROR, 
     (* special purpose operations *)
     DERIV, IF, ASSIGN, GROUP]

    
datatype funtype = BUILTIN of operation
		 | INST of {classname:Symbol.symbol, 
			    instname:Symbol.symbol, 
			    props:instproperties}


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

datatype fix = INFIX | PREFIX | POSTFIX | MATCH
datatype operands = FIXED of int (* the required number of arguments *)
		  | VARIABLE of int (* the default value *)

type op_props = {name: string,
		 operands: operands,
		 precedence: int,
		 commutative: bool,
		 associative: bool,
		 text: (string * fix),
		 C: (string * fix)}


fun unaryfun2props name : op_props =
    {name=name,
     operands=FIXED 1,
     precedence=1,
     commutative=false,
     associative=false,
     text=(name, PREFIX),
     C=(name, PREFIX)}


fun op2props optype = 
    case optype of
	ADD => {name="add",
		operands=VARIABLE 0,
		precedence=6,
		commutative=true,
		associative=true,
		text=("+",INFIX),
		C=("+",INFIX)}
      | SUB => {name="sub",
		operands=FIXED 2,
		precedence=6,
		commutative=false,
		associative=false,
		text=("-",INFIX),
		C=("-",INFIX)}
      | NEG => {name="neg",
		operands=FIXED 1,
		precedence=6,
		commutative=false,
		associative=false,
		text=("-",PREFIX),
		C=("-",PREFIX)}
      | MUL => {name="mul",
		operands=VARIABLE 1,
		precedence=5,
		commutative=true,
		associative=true,
		text=("*",INFIX),
		C=("*",INFIX)}
      | DIVIDE => {name="divide",
		   operands=FIXED 2,
		   precedence=5,
		   commutative=false,
		   associative=false,
		   text=("/",INFIX),
		   C=("/",INFIX)}
      | MODULUS => {name="modulus",
		    operands=FIXED 2,
		    precedence=5,
		    commutative=false,
		    associative=false,
		    text=("%",INFIX),
		    C=("fmod($1,$2)",MATCH)}
      | POW => {name="pow",
		operands=FIXED 2,
		precedence=4,
		commutative=false,
		associative=false,
		text=("^",INFIX),
		C=("pow($1,$2)",MATCH)}
      | ABS => unaryfun2props "abs"
      | SQRT => unaryfun2props "sqrt"
      | DEG2RAD => unaryfun2props "deg2rad"
      | RAD2DEG => unaryfun2props "rad2deg"
      | LOGN => {name="logn",
		 operands=FIXED 2,
		 precedence=1,
		 commutative=false,
		 associative=false,
		 text=("log_$1($2)",MATCH),
		 C=("(log($1)/log($2))",MATCH)}
      | EXP => unaryfun2props "exp"
      | LOG => unaryfun2props "log"
      | LOG10 => unaryfun2props "log10"
      | SIN => unaryfun2props "sin"
      | COS => unaryfun2props "cos"
      | TAN => unaryfun2props "tan"
      | CSC => {name="csc",
		 operands=FIXED 1,
		 precedence=1,
		 commutative=false,
		 associative=false,
		 text=("csc",PREFIX),
		 C=("(1/sin($1))",MATCH)}
      | SEC => {name="sec",
		 operands=FIXED 1,
		 precedence=1,
		 commutative=false,
		 associative=false,
		 text=("sec",PREFIX),
		 C=("(1/cos($1))",MATCH)}
      | COT => {name="cot",
		 operands=FIXED 1,
		 precedence=1,
		 commutative=false,
		 associative=false,
		 text=("cot",PREFIX),
		 C=("(1/tan($1))",MATCH)}
      | ASIN => unaryfun2props "asin"
      | ACOS => unaryfun2props "acos"
      | ATAN => unaryfun2props "atan"
      | ATAN2 => {name="atan2",
		  operands=FIXED 2,
		  precedence=1,
		  commutative=false,
		  associative=false,
		  text=("atan2",PREFIX),
		  C=("atan2",PREFIX)}
      | ACSC => {name="acsc",
		 operands=FIXED 1,
		 precedence=1,
		 commutative=false,
		 associative=false,
		 text=("acsc",PREFIX),
		 C=("asin(1/$1)",MATCH)}
      | ASEC => {name="asec",
		 operands=FIXED 1,
		 precedence=1,
		 commutative=false,
		 associative=false,
		 text=("asec",PREFIX),
		 C=("acos(1/$1)",MATCH)}
      | ACOT => {name="acot",
		 operands=FIXED 1,
		 precedence=1,
		 commutative=false,
		 associative=false,
		 text=("acot",PREFIX),
		 C=("atan(1/$1)",MATCH)}
      | SINH => unaryfun2props "sinh"
      | COSH => unaryfun2props "cosh"
      | TANH => unaryfun2props "tanh"
      | CSCH => {name="csch",
		 operands=FIXED 1,
		 precedence=1,
		 commutative=false,
		 associative=false,
		 text=("csch",PREFIX),
		 C=("(1/sinh($1))",MATCH)}
      | SECH => {name="sech",
		 operands=FIXED 1,
		 precedence=1,
		 commutative=false,
		 associative=false,
		 text=("sech",PREFIX),
		 C=("(1/cosh($1))",MATCH)}
      | COTH => {name="coth",
		 operands=FIXED 1,
		 precedence=1,
		 commutative=false,
		 associative=false,
		 text=("coth",PREFIX),
		 C=("(1/tanh($1))",MATCH)}
      | ASINH => {name="asinh",
		 operands=FIXED 1,
		 precedence=1,
		 commutative=false,
		 associative=false,
		 text=("asinh",PREFIX),
		 C=("log($1 + sqrt($1*$1+1))",MATCH)}
      | ACOSH => {name="acosh",
		 operands=FIXED 1,
		 precedence=1,
		 commutative=false,
		 associative=false,
		 text=("acosh",PREFIX),
		 C=("log($1 + sqrt($1*$1-1))",MATCH)}
      | ATANH => {name="atanh",
		 operands=FIXED 1,
		 precedence=1,
		 commutative=false,
		 associative=false,
		 text=("atanh",PREFIX),
		 C=("(log((1+$1)/(1-$1))/2)",MATCH)}
      | ACSCH => {name="acsch",
		 operands=FIXED 1,
		 precedence=1,
		 commutative=false,
		 associative=false,
		 text=("acsch",PREFIX),
		 C=("log(1/$1 + sqrt($1*$1+1)/fabs($1))",MATCH)}
      | ASECH => {name="asech",
		 operands=FIXED 1,
		 precedence=1,
		 commutative=false,
		 associative=false,
		 text=("asech",PREFIX),
		 C=("log((1 + sqrt(1-$1*$1))/$1)",MATCH)}
      | ACOTH => {name="acoth",
		 operands=FIXED 1,
		 precedence=1,
		 commutative=false,
		 associative=false,
		 text=("acoth",PREFIX),
		 C=("(log(($1+1)/($1-1))/2)",MATCH)}
      | NOT => {name="not",
		operands=FIXED 1,
		precedence=3,
		commutative=false,
		associative=false,
		text=("!",PREFIX),
		C=("!",PREFIX)}
      | AND => {name="and",
		operands=VARIABLE 1,
		precedence=13,
		commutative=false,
		associative=false,
		text=("&&",INFIX),
		C=("&&",INFIX)}
      | OR => {name="or",
	       operands=VARIABLE 0,
	       precedence=14,
	       commutative=false,
	       associative=false,
	       text=("||",INFIX),
	       C=("||",INFIX)}
      | GT => {name="gt",
	       operands=FIXED 2,
	       precedence=8,
	       commutative=false,
	       associative=false,
	       text=(">",INFIX),
	       C=(">",INFIX)}
      | LT => {name="lt",
	      operands=FIXED 2,
	      precedence=8,
	      commutative=false,
	      associative=false,
	      text=("<",INFIX),
	      C=("<",INFIX)}
      | GE => {name="ge",
	       operands=FIXED 2,
	       precedence=8,
	       commutative=false,
	       associative=false,
	       text=(">=",INFIX),
	       C=(">=",INFIX)}
      | LE => {name="le",
	       operands=FIXED 2,
	       precedence=8,
	       commutative=false,
	       associative=false,
	      text=("<=",INFIX),
	       C=("<=",INFIX)}
      | EQ => {name="eq",
	      operands=FIXED 2,
	      precedence=9,
	      commutative=false,
	      associative=false,
	      text=("==",INFIX),
	      C=("==",INFIX)}
      | NEQ => {name="neq",
		operands=FIXED 2,
		precedence=9,
		commutative=false,
		associative=false,
		text=("<>",INFIX),
		C=("!=",INFIX)}
      | DERIV => {name="deriv",
		  operands=FIXED 2,
		  precedence=1,
		  commutative=false,
		  associative=false,
		  text=("D",PREFIX),
		  C=("Derivative",PREFIX)}
      | IF => {name="if",
	       operands=FIXED 3,
	       precedence=15,
	       commutative=false,
	       associative=false,
	       text=("If $1 then $2 else $3", MATCH),
	       C=("$1 ? $2 : $3",MATCH)}
      | ASSIGN => {name="assign",
		   operands=FIXED 2,
		   precedence=16,
		   commutative=false,
		   associative=false,
		   text=(" = ",INFIX),
		   C=(" = ",INFIX)}
      | GROUP => {name="group",
		  operands=FIXED 1,
		  precedence=1,
		  commutative=false,
		  associative=false,
		  text=("",PREFIX),
		  C=("",PREFIX)}
      | NULL => {name="nullfun",
		  operands=FIXED 0,
		  precedence=1,
		  commutative=false,
		  associative=false,
		  text=("NULL",PREFIX),
		  C=("",PREFIX)}
      | RADD => {name="reduction_add",
		 operands=FIXED 1,
		 precedence=6,
		 commutative=true,
		 associative=true,
		 text=("radd",INFIX),
		 C=("simEngine_library_radd",INFIX)}
      | RMUL => {name="reduction_mul",
		 operands=FIXED 1,
		 precedence=5,
		 commutative=true,
		 associative=true,
		 text=("rmul",INFIX),
		 C=("simEngine_library_rmul",INFIX)}
      | RAND => {name="reduction_add",
		 operands=FIXED 1,
		 precedence=13,
		 commutative=true,
		 associative=true,
		 text=("rand",INFIX),
		 C=("simEngine_library_rand",INFIX)}
      | ROR => {name="reduction_add",
		 operands=FIXED 1,
		 precedence=14,
		 commutative=true,
		 associative=true,
		 text=("ror",INFIX),
		 C=("simEngine_library_ror",INFIX)}



(* Create new Symbol Table *)

fun add2optable (opTable, name, opsym) = 
    SymbolTable.enter (opTable, Symbol.symbol name, opsym)

val opTable = 
    let
	val opTable = SymbolTable.empty

	val optable_with_entries = 
	    foldl (fn(operation, opTable)=> 
		 let
		     val {name, ...} = op2props operation
		 in
		     add2optable (opTable, name, operation)
		 end)
		  opTable (* blank opTable *)
		  op_list (* all the pre-defined operations *)

    in
	optable_with_entries
    end
    
fun name2op sym =
    case SymbolTable.look (opTable, sym) of
	SOME oper => oper
      | NONE => (Logger.log_error(Printer.$("No such operation with name '"^(Symbol.name sym)^"' defined in the system"));
		 DynException.setErrored();
		 NULL)

fun builtin2props f : op_props = 
    op2props f

fun fun2textstrnotation f =
    case f 
	 of BUILTIN v => 
	    let
		val {text as (str, notation),...} = builtin2props v
	    in
		(str, notation)
	    end
	  | INST {classname,props,...} => 
	    case getRealClassName props of
		SOME sym => ((Symbol.name sym) ^ "<"^(Symbol.name classname)^">", PREFIX)
	      | NONE => (Symbol.name classname, PREFIX)

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
