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
      iterators: Symbol.symbol list,
      inline: bool(*,
      form: instform option*)}

(* handle instance properties *)
val emptyinstprops = {dim=NONE,
		      sourcepos=NONE,
		      realclassname=NONE,
		      realinstname=NONE,
		      iterators=nil,
		      inline=false}

fun getDim (props : instproperties) = #dim props
fun getSourcePos (props : instproperties)= #sourcepos props
fun getRealClassName (props : instproperties)= #realclassname props
fun getRealInstName (props : instproperties)= #realinstname props
fun isInline (props : instproperties)= #inline props
fun getIterators (props: instproperties) = #iterators props

fun setDim (props as {dim, sourcepos, realclassname, realinstname, inline, iterators} : instproperties) sym : instproperties = 
    {dim=SOME sym,
     sourcepos=sourcepos,
     realclassname=realclassname,
     realinstname=realinstname,
     iterators=iterators,
     inline=inline}
															 
fun setSourcePos (props as {dim, sourcepos, realclassname, realinstname, inline, iterators} : instproperties) sym : instproperties = 
    {dim=dim,
     sourcepos=SOME sym,
     realclassname=realclassname,
     realinstname=realinstname,
     iterators=iterators,
     inline=inline}
															 
fun setRealClassName (props as {dim, sourcepos, realclassname, realinstname, inline, iterators} : instproperties) sym : instproperties = 
    {dim=dim,
     sourcepos=sourcepos,
     realclassname=SOME sym,
     realinstname=realinstname,
     iterators=iterators,
     inline=inline}
															 
fun setRealInstName (props as {dim, sourcepos, realclassname, realinstname, inline, iterators} : instproperties) sym : instproperties = 
    {dim=dim,
     sourcepos=sourcepos,
     realclassname=realclassname,
     realinstname=SOME sym,
     iterators=iterators,
     inline=inline}

fun setInline (props as {dim, sourcepos, realclassname, realinstname, inline, iterators} : instproperties) sym : instproperties = 
    {dim=dim,
     sourcepos=sourcepos,
     realclassname=realclassname,
     realinstname=realinstname,
     iterators=iterators,
     inline=sym}

fun setIterators (props as {dim, sourcepos, realclassname, realinstname, inline, iterators} : instproperties) newiterators : instproperties = 
    {dim=dim,
     sourcepos=sourcepos,
     realclassname=realclassname,
     realinstname=realinstname,
     iterators=newiterators,
     inline=inline}
														 

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
		 C: (string * fix),
		 codomain: int list list -> int list}


fun vectorizedCodomain (nil: int list list) : int list = nil
  | vectorizedCodomain (first::rest) =
    let
	fun combineSizes (size1, size2) = 
	    if (size1 = size2) then size1
	    else if (size1 = 1) then size2
	    else if (size2 = 1) then size1
	    else
		(Logger.log_internalerror (Printer.$("Arguments have mismatched sizes ("^(Int.toString size1)^","^(Int.toString size2)^")"));
		 DynException.setErrored(); 
		 1)

    in
	foldl (fn(a,b) => map combineSizes
			      (ListPair.zip(a,b))) 
	      first 
	      rest
    end

fun safeTail nil = nil
  | safeTail (a::rest) = rest

fun codomainReduction (nil: int list list) : int list = nil
  | codomainReduction args =
    safeTail(vectorizedCodomain(args))


fun unaryfun2props name : op_props =
    {name=name,
     operands=FIXED 1,
     precedence=1,
     commutative=false,
     associative=false,
     text=(name, PREFIX),
     C=(name, PREFIX),
     codomain = vectorizedCodomain}

fun op2props optype = 
    case optype of
	ADD => {name="add",
		operands=VARIABLE 0,
		precedence=6,
		commutative=true,
		associative=true,
		text=("+",INFIX),
		C=("+",INFIX),
		codomain= vectorizedCodomain}
      | SUB => {name="sub",
		operands=FIXED 2,
		precedence=6,
		commutative=false,
		associative=false,
		text=("-",INFIX),
		C=("-",INFIX),
		codomain= vectorizedCodomain}
      | NEG => {name="neg",
		operands=FIXED 1,
		precedence=6,
		commutative=false,
		associative=false,
		text=("-",PREFIX),
		C=("-",PREFIX),
		codomain= vectorizedCodomain}
      | MUL => {name="mul",
		operands=VARIABLE 1,
		precedence=5,
		commutative=true,
		associative=true,
		text=("*",INFIX),
		C=("*",INFIX),
		codomain= vectorizedCodomain}
      | DIVIDE => {name="divide",
		   operands=FIXED 2,
		   precedence=5,
		   commutative=false,
		   associative=false,
		   text=("/",INFIX),
		   C=("/",INFIX),
		   codomain= vectorizedCodomain}
      | MODULUS => {name="modulus",
		    operands=FIXED 2,
		    precedence=5,
		    commutative=false,
		    associative=false,
		    text=("%",INFIX),
		    C=("%",INFIX),
		    codomain= vectorizedCodomain}
      | POW => {name="pow",
		operands=FIXED 2,
		precedence=4,
		commutative=false,
		associative=false,
		text=("^",INFIX),
		C=("pow($1,$2)",MATCH),
		codomain= vectorizedCodomain}
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
		 C=("(log($1)/log($2))",MATCH),
		 codomain= vectorizedCodomain}
      | EXP => unaryfun2props "exp"
      | LOG => unaryfun2props "log"
      | LOG10 => unaryfun2props "log10"
      | SIN => unaryfun2props "sin"
      | COS => unaryfun2props "cos"
      | TAN => unaryfun2props "tan"
      | CSC => unaryfun2props "csc"
      | SEC => unaryfun2props "sec"
      | COT => unaryfun2props "cot"
      | ASIN => unaryfun2props "asinh"
      | ACOS => unaryfun2props "acosh"
      | ATAN => unaryfun2props "atanh"
      | ATAN2 => {name="atan2",
		  operands=FIXED 2,
		  precedence=1,
		  commutative=false,
		  associative=false,
		  text=("atan2",PREFIX),
		  C=("atan2",PREFIX),
		  codomain= vectorizedCodomain}
      | ACSC => unaryfun2props "acsch"
      | ASEC => unaryfun2props "asech"
      | ACOT => unaryfun2props "acoth"
      | SINH => unaryfun2props "sinh"
      | COSH => unaryfun2props "cosh"
      | TANH => unaryfun2props "tanh"
      | CSCH => unaryfun2props "csch"
      | SECH => unaryfun2props "sech"
      | COTH => unaryfun2props "coth"
      | ASINH => unaryfun2props "asinh"
      | ACOSH => unaryfun2props "acosh"
      | ATANH => unaryfun2props "atanh"
      | ACSCH => unaryfun2props "acsch"
      | ASECH => unaryfun2props "asech"
      | ACOTH => unaryfun2props "acoth"
      | NOT => {name="not",
		operands=FIXED 1,
		precedence=3,
		commutative=false,
		associative=false,
		text=("!",PREFIX),
		C=("!",PREFIX),
		codomain= vectorizedCodomain}
      | AND => {name="and",
		operands=VARIABLE 1,
		precedence=13,
		commutative=false,
		associative=false,
		text=("&&",INFIX),
		C=("&&",INFIX),
		codomain= vectorizedCodomain}
      | OR => {name="or",
	       operands=VARIABLE 0,
	       precedence=14,
	       commutative=false,
	       associative=false,
	       text=("||",INFIX),
	       C=("||",INFIX),
	       codomain= vectorizedCodomain}
      | GT => {name="gt",
	       operands=FIXED 2,
	       precedence=8,
	       commutative=false,
	       associative=false,
	       text=(">",INFIX),
	       C=(">",INFIX),
	       codomain= vectorizedCodomain}
      | LT => {name="lt",
	       operands=FIXED 2,
	       precedence=8,
	       commutative=false,
	       associative=false,
	       text=("<",INFIX),
	       C=("<",INFIX),
	       codomain= vectorizedCodomain}
      | GE => {name="ge",
	       operands=FIXED 2,
	       precedence=8,
	       commutative=false,
	       associative=false,
	       text=(">=",INFIX),
	       C=(">=",INFIX),
	       codomain= vectorizedCodomain}
      | LE => {name="le",
	       operands=FIXED 2,
	       precedence=8,
	       commutative=false,
	       associative=false,
	       text=(">=",INFIX),
	       C=(">=",INFIX),
	       codomain= vectorizedCodomain}
      | EQ => {name="eq",
	       operands=FIXED 2,
	       precedence=9,
	       commutative=false,
	       associative=false,
	       text=("==",INFIX),
	       C=("==",INFIX),
	       codomain= vectorizedCodomain}
      | NEQ => {name="neq",
		operands=FIXED 2,
		precedence=9,
		commutative=false,
		associative=false,
		text=("<>",INFIX),
		C=("!=",INFIX),
		codomain= vectorizedCodomain}
      | DERIV => {name="deriv",
		  operands=FIXED 2,
		  precedence=1,
		  commutative=false,
		  associative=false,
		  text=("D",PREFIX),
		  C=("Derivative",PREFIX),
		  codomain= vectorizedCodomain}
      | IF => {name="if",
	       operands=FIXED 3,
	       precedence=15,
	       commutative=false,
	       associative=false,
	       text=("If $1 then $2 else $3", MATCH),
	       C=("$1 ? $2 : $3",MATCH),
	       codomain= vectorizedCodomain}
      | ASSIGN => {name="assign",
		   operands=FIXED 2,
		   precedence=16,
		   commutative=false,
		   associative=false,
		   text=(" = ",INFIX),
		   C=(" = ",INFIX),
		   codomain= vectorizedCodomain}
      | GROUP => {name="group",
		  operands=VARIABLE 0,
		  precedence=1,
		  commutative=false,
		  associative=false,
		  text=("",PREFIX),
		  C=("",PREFIX),
		  codomain= vectorizedCodomain}
      | NULL => {name="nullfun",
		 operands=FIXED 0,
		 precedence=1,
		 commutative=false,
		 associative=false,
		 text=("NULL",PREFIX),
		 C=("",PREFIX),
		 codomain= vectorizedCodomain}
      | RADD => {name="reduction_add",
		 operands=FIXED 1,
		 precedence=6,
		 commutative=true,
		 associative=true,
		 text=("radd",PREFIX),
		 C=("simEngine_library_radd",PREFIX),
		 codomain=codomainReduction}
      | RMUL => {name="reduction_mul",
		 operands=FIXED 1,
		 precedence=5,
		 commutative=true,
		 associative=true,
		 text=("rmul",PREFIX),
		 C=("simEngine_library_rmul",PREFIX),
		 codomain=codomainReduction}
      | RAND => {name="reduction_and",
		 operands=FIXED 1,
		 precedence=13,
		 commutative=true,
		 associative=true,
		 text=("rand",PREFIX),
		 C=("simEngine_library_rand",PREFIX),
		 codomain=codomainReduction}
      | ROR => {name="reduction_or",
		operands=FIXED 1,
		precedence=14,
		commutative=true,
		associative=true,
		text=("ror",PREFIX),
		C=("simEngine_library_ror",PREFIX),
		codomain=codomainReduction}



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

fun op2name (f: funtype) = 
    case f
     of BUILTIN v => #name (op2props v)
      | INST {classname,...} => Symbol.name classname

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

fun hasVariableArguments f =
    case f 
     of BUILTIN v => 
	(case #operands (builtin2props v) of
	     VARIABLE _ => true
	   | FIXED _ => false)
      | INST _ => false (* not allowing variable arguments for instances *)


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
