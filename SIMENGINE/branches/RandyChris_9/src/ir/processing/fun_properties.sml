structure FunProps =
struct

open Fun

datatype fix = INFIX | PREFIX | POSTFIX | MATCH
datatype operands = FIXED of int (* the required number of arguments *)
		  | VARIABLE of Exp.term (* the default value *)


datatype computationtype = UNARY of {bool: (bool -> Exp.exp) option,
				     int: (int -> Exp.exp) option,
				     real: (real -> Exp.exp) option,
				     complex: ((Exp.term * Exp.term) -> Exp.exp) option,
				     (* NONE in collection means that it is parallelizable *)
				     collection: (Exp.term -> Exp.exp) option, (* cumsum, diff, uniquify, concat, intersection, etc. *)
				     rational: ((int * int) -> Exp.exp) option}
			 | BINARY of {bool: (bool * bool -> Exp.exp) option,
				      int: (int * int -> Exp.exp) option,
				      real: (real * real -> Exp.exp) option,
				      complex: ((Exp.term * Exp.term) * (Exp.term * Exp.term) -> Exp.exp) option,
				      collection: (Exp.term * Exp.term -> Exp.exp) option,
				      rational: ((int * int) * (int * int) -> Exp.exp) option}
			 | IF_FUN of {bool: (bool * bool * bool -> Exp.exp) option,
				      int: (bool * int * int -> Exp.exp) option,
				      real: (bool * real * real -> Exp.exp) option,
				      complex: (bool * (Exp.term * Exp.term) * (Exp.term * Exp.term) -> Exp.exp) option,
				      collection: (bool * Exp.term * Exp.term -> Exp.exp) option,
				      rational: (bool * (int * int) * (int * int) -> Exp.exp) option}
			 | INSTANCE

val empty_evals = {bool=NONE, int=NONE, real=NONE, complex=NONE, rational=NONE, collection=NONE}
val empty_unary = UNARY empty_evals
val empty_binary = BINARY empty_evals
val empty_if_fun = IF_FUN empty_evals
val empty_instance = INSTANCE

type op_props = {name: string,
		 operands: operands,
		 precedence: int,
		 commutative: bool,
		 associative: bool,
		 eval: computationtype,
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


fun unaryfun2props (name, eval) : op_props =
    {name=name,
     operands=FIXED 1,
     precedence=1,
     commutative=false,
     associative=false,
     eval=eval,
     text=(name, PREFIX),
     C=(name, PREFIX),
     codomain = vectorizedCodomain}

exception UnsupportedType of Exp.term
val i2r = Real.fromInt
val r2i = Real.floor
val i2s = Util.i2s
val b2s = Util.b2s
val r2s = Util.r2s
val e2s = (*ExpPrinter.exp2str*) fn(x)=>"can't print here"
val int = ExpBuild.int
val real = ExpBuild.real
val bool = ExpBuild.bool
val frac = ExpBuild.frac
val complex = ExpBuild.complex_fun

fun op2props optype = 
    case optype of
	ADD => {name="add",
		operands=VARIABLE (Exp.INT 0),
		precedence=6,
		commutative=true,
		associative=true,
		eval=BINARY {bool=NONE,
			     int=SOME (fn(i1,i2)=> int (i1+i2)),
			     real=SOME (fn(r1,r2)=> real (r1+r2)),
			     complex=SOME (fn((r1,i1),(r2,i2)) => eval (complex (ExpBuild.plus [Exp.TERM r1, Exp.TERM r2],
										 ExpBuild.plus [Exp.TERM i1, Exp.TERM i2]))),
			     collection=NONE,
			     rational=SOME (fn((n1,d1),(n2,d2))=>if d1=d2 then 
								     frac (n1+n2,d1) 
								 else
								     frac (n1*d2 + n2*d1, d1*d2))},
		text=("+",INFIX),
		C=("+",INFIX),
		codomain= vectorizedCodomain}
      | SUB => {name="sub",
		operands=FIXED 2,
		precedence=6,
		commutative=false,
		associative=false,
		eval=empty_binary,
		text=("-",INFIX),
		C=("-",INFIX),
		codomain= vectorizedCodomain}
      | NEG => {name="neg",
		operands=FIXED 1,
		precedence=6,
		commutative=false,
		associative=false,
		eval=empty_unary,
		text=("-",PREFIX),
		C=("-",PREFIX),
		codomain= vectorizedCodomain}
      | MUL => {name="mul",
		operands=VARIABLE (Exp.INT 1),
		precedence=5,
		commutative=true,
		associative=true,
		eval=empty_binary,
		text=("*",INFIX),
		C=("*",INFIX),
		codomain= vectorizedCodomain}
      | DIVIDE => {name="divide",
		   operands=FIXED 2,
		   precedence=5,
		   commutative=false,
		   associative=false,
		   eval=empty_binary,
		   text=("/",INFIX),
		   C=("/",INFIX),
		   codomain= vectorizedCodomain}
      | MODULUS => {name="modulus",
		    operands=FIXED 2,
		    precedence=5,
		    commutative=false,
		    associative=false,
		    eval=empty_binary,
		    text=("%",INFIX),
		    C=("%",INFIX),
		    codomain= vectorizedCodomain}
      | POW => {name="pow",
		operands=FIXED 2,
		precedence=4,
		commutative=false,
		associative=false,
		eval=empty_binary,
		text=("^",INFIX),
		C=("pow($1,$2)",MATCH),
		codomain= vectorizedCodomain}
      | COMPLEX => {name="complex",
		    operands=FIXED 2,
		    precedence=4,
		    commutative=false,
		    associative=false,
		    eval=empty_binary,
		    text=("complex",INFIX),
		    C=("complex",PREFIX),
		    codomain= vectorizedCodomain}
      | RE => unaryfun2props ("re", UNARY {bool=NONE,
					   int=NONE,
					   real=NONE,
					   complex=SOME (fn(r, i)=> Exp.TERM r),
					   rational=NONE,
					   collection=NONE})
      | IM => unaryfun2props ("im", UNARY {bool=NONE,
					   int=NONE,
					   real=NONE,
					   complex=SOME (fn(r, i)=> Exp.TERM i),
					   rational=NONE,
					   collection=NONE})
      | ABS => unaryfun2props ("abs", UNARY {bool=NONE,
					     int=SOME (int o r2i o abs o i2r),
					     real=SOME (real o abs),
					     complex=SOME (fn(r,i)=>complex (ExpBuild.norm [Exp.TERM r, Exp.TERM i],
									     int 0)),
					     rational=SOME (fn(n,d)=>frac (r2i (abs (i2r n)),
									   r2i (abs (i2r d)))),
					     collection=NONE})
      | ARG => unaryfun2props ("arg", UNARY {bool=NONE,
					     int=SOME (fn(r)=> int 0),
					     real=SOME (fn(r)=> real 0.0),
					     complex=SOME (fn(r, i)=> ExpBuild.atan2(Exp.TERM r,Exp.TERM i)),
					     rational=NONE,
					     collection=NONE})
      | CONJ => {name="conj",
		 operands=FIXED 1,
		 precedence=1,
		 commutative=false,
		 associative=false,
		 eval=UNARY {bool=NONE,
			     int=NONE,
			     real=NONE,
			     complex=SOME (fn(r,i)=> eval (complex (Exp.TERM r,
								    ExpBuild.neg (Exp.TERM i)))),
			     rational=NONE,
			     collection=NONE},
		 text=("log_$1($2)",MATCH),
		 C=("(log($1)/log($2))",MATCH),
		 codomain= vectorizedCodomain}
      | SQRT => unaryfun2props ("sqrt", 
				UNARY {bool=NONE,
				       int=NONE,
				       real=SOME
						(fn(r)=> if r < 0.0 then
							     eval (ExpBuild.sqrt (complex (real r,int 0)))
							 else
							     real (Math.sqrt r)),
				       complex=SOME (fn(r,i)=>
						       let
							   val z = Exp.COMPLEX (r,i)
							   val factor = ExpBuild.power (ExpBuild.plus [ExpBuild.square (Exp.TERM r),
												       ExpBuild.square (Exp.TERM i)], 
											ExpBuild.frac (1,4))
							   val inner = ExpBuild.times [ExpBuild.frac (1,2), ExpBuild.arg (Exp.TERM z)]
						       in
							   complex (ExpBuild.times [factor, ExpBuild.cos inner],
								    ExpBuild.times [factor, ExpBuild.sin inner])
						       end),
				       rational=NONE,
				       collection=NONE})
      | DEG2RAD => unaryfun2props ("deg2rad",
				   UNARY {bool=NONE,
					  int=NONE,
					  real=SOME (fn(r)=> real (r * Math.pi / 180.0)),
					  complex=NONE,
					  rational=NONE,
					  collection=NONE})
      | RAD2DEG => unaryfun2props ("rad2deg",
				   UNARY {bool=NONE,
					  int=NONE,
					  real=SOME (fn(r)=> real (r * 180.0 / Math.pi)),
					  complex=NONE,
					  rational=NONE,
					  collection=NONE})
      | LOGN => {name="logn",
		 operands=FIXED 2,
		 precedence=1,
		 commutative=false,
		 associative=false,
		 eval=BINARY {bool=NONE,
			      int=NONE,
			      real=SOME (fn(b, r) => 
					   if r < 0.0 orelse b < 0.0 then
					       eval (ExpBuild.complex_logn (complex (real b, int 0), 
									    complex (real r, int 0)))
					   else
					       real ((Math.ln b)/(Math.ln r))),
			      complex=SOME (fn(b:(Exp.term * Exp.term), z)=> eval (ExpBuild.complex_logn (ExpBuild.complex b,
										    ExpBuild.complex z))),
			      rational=NONE,
			      collection=NONE},
		 text=("log_$1($2)",MATCH),
		 C=("(log($1)/log($2))",MATCH),
		 codomain= vectorizedCodomain}
      | EXP => unaryfun2props ("exp", empty_unary)
      | LOG => unaryfun2props ("log", empty_unary)
      | LOG10 => unaryfun2props ("log10", empty_unary)
      | SIN => unaryfun2props ("sin", empty_unary)
      | COS => unaryfun2props ("cos", empty_unary)
      | TAN => unaryfun2props ("tan", empty_unary)
      | CSC => unaryfun2props ("csc", empty_unary)
      | SEC => unaryfun2props ("sec", empty_unary)
      | COT => unaryfun2props ("cot", empty_unary)
      | ASIN => unaryfun2props ("asin", empty_unary)
      | ACOS => unaryfun2props ("acos", empty_unary)
      | ATAN => unaryfun2props ("atan", empty_unary)
      | ATAN2 => {name="atan2",
		  operands=FIXED 2,
		  precedence=1,
		  commutative=false,
		  associative=false,
		  eval=empty_binary,
		  text=("atan2",PREFIX),
		  C=("atan2",PREFIX),
		  codomain= vectorizedCodomain}
      | ACSC => unaryfun2props ("acsch", empty_unary)
      | ASEC => unaryfun2props ("asech", empty_unary)
      | ACOT => unaryfun2props ("acoth", empty_unary)
      | SINH => unaryfun2props ("sinh", empty_unary)
      | COSH => unaryfun2props ("cosh", empty_unary)
      | TANH => unaryfun2props ("tanh", empty_unary)
      | CSCH => unaryfun2props ("csch", empty_unary)
      | SECH => unaryfun2props ("sech", empty_unary)
      | COTH => unaryfun2props ("coth", empty_unary)
      | ASINH => unaryfun2props ("asinh", empty_unary)
      | ACOSH => unaryfun2props ("acosh", empty_unary)
      | ATANH => unaryfun2props ("atanh", empty_unary)
      | ACSCH => unaryfun2props ("acsch", empty_unary)
      | ASECH => unaryfun2props ("asech", empty_unary)
      | ACOTH => unaryfun2props ("acoth", empty_unary)
      | NOT => {name="not",
		operands=FIXED 1,
		precedence=3,
		commutative=false,
		associative=false,
		eval=empty_unary,
		text=("!",PREFIX),
		C=("!",PREFIX),
		codomain= vectorizedCodomain}
      | AND => {name="and",
		operands=VARIABLE (Exp.BOOL true),
		precedence=13,
		commutative=false,
		associative=false,
		  eval=empty_binary,
		text=("&&",INFIX),
		C=("&&",INFIX),
		codomain= vectorizedCodomain}
      | OR => {name="or",
	       operands=VARIABLE (Exp.BOOL false),
	       precedence=14,
	       commutative=false,
	       associative=false,
		  eval=empty_binary,
	       text=("||",INFIX),
	       C=("||",INFIX),
	       codomain= vectorizedCodomain}
      | GT => {name="gt",
	       operands=FIXED 2,
	       precedence=8,
	       commutative=false,
	       associative=false,
		  eval=empty_binary,
	       text=(">",INFIX),
	       C=(">",INFIX),
	       codomain= vectorizedCodomain}
      | LT => {name="lt",
	       operands=FIXED 2,
	       precedence=8,
	       commutative=false,
	       associative=false,
		  eval=empty_binary,
	       text=("<",INFIX),
	       C=("<",INFIX),
	       codomain= vectorizedCodomain}
      | GE => {name="ge",
	       operands=FIXED 2,
	       precedence=8,
	       commutative=false,
	       associative=false,
		  eval=empty_binary,
	       text=(">=",INFIX),
	       C=(">=",INFIX),
	       codomain= vectorizedCodomain}
      | LE => {name="le",
	       operands=FIXED 2,
	       precedence=8,
	       commutative=false,
	       associative=false,
		  eval=empty_binary,
	       text=(">=",INFIX),
	       C=(">=",INFIX),
	       codomain= vectorizedCodomain}
      | EQ => {name="eq",
	       operands=FIXED 2,
	       precedence=9,
	       commutative=false,
	       associative=false,
		  eval=empty_binary,
	       text=("==",INFIX),
	       C=("==",INFIX),
	       codomain= vectorizedCodomain}
      | NEQ => {name="neq",
		operands=FIXED 2,
		precedence=9,
		commutative=false,
		associative=false,
		 eval=empty_unary,
		text=("<>",INFIX),
		C=("!=",INFIX),
		codomain= vectorizedCodomain}
      | DERIV => {name="deriv",
		  operands=FIXED 2,
		  precedence=1,
		  commutative=false,
		  associative=false,
		  eval=empty_binary,
		  text=("D",PREFIX),
		  C=("Derivative",PREFIX),
		  codomain= vectorizedCodomain}
      | IF => {name="if",
	       operands=FIXED 3,
	       precedence=15,
	       commutative=false,
	       associative=false,
		  eval=empty_if_fun,
	       text=("If $1 then $2 else $3", MATCH),
	       C=("$1 ? $2 : $3",MATCH),
	       codomain= vectorizedCodomain}
      | ASSIGN => {name="assign",
		   operands=FIXED 2,
		   precedence=16,
		   commutative=false,
		   associative=false,
		  eval=empty_binary,
		   text=(" = ",INFIX),
		   C=(" = ",INFIX),
		   codomain= vectorizedCodomain}
      | GROUP => {name="group",
		  operands=VARIABLE (Exp.LIST ([],[])),
		  precedence=1,
		  commutative=false,
		  associative=false,
		  eval=empty_binary,
		  text=("",PREFIX),
		  C=("",PREFIX),
		  codomain= vectorizedCodomain}
      | NULL => {name="nullfun",
		 operands=FIXED 0,
		 precedence=1,
		 commutative=false,
		 associative=false,
		  eval=empty_binary,
		 text=("NULL",PREFIX),
		 C=("",PREFIX),
		 codomain= vectorizedCodomain}
      | RADD => {name="reduction_add",
		 operands=FIXED 1,
		 precedence=6,
		 commutative=true,
		 associative=true,
		 eval=empty_unary,
		 text=("radd",PREFIX),
		 C=("simEngine_library_radd",PREFIX),
		 codomain=codomainReduction}
      | RMUL => {name="reduction_mul",
		 operands=FIXED 1,
		 precedence=5,
		 commutative=true,
		 associative=true,
		 eval=empty_unary,
		 text=("rmul",PREFIX),
		 C=("simEngine_library_rmul",PREFIX),
		 codomain=codomainReduction}
      | RAND => {name="reduction_and",
		 operands=FIXED 1,
		 precedence=13,
		 commutative=true,
		 associative=true,
		 eval=empty_unary,
		 text=("rand",PREFIX),
		 C=("simEngine_library_rand",PREFIX),
		 codomain=codomainReduction}
      | ROR => {name="reduction_or",
		operands=FIXED 1,
		precedence=14,
		commutative=true,
		associative=true,
		 eval=empty_unary,
		text=("ror",PREFIX),
		C=("simEngine_library_ror",PREFIX),
		codomain=codomainReduction}

and eval exp = exp
    handle UnsupportedType e => DynException.stdException(("Could not evaluate '"^(e2s e)^"' in expression '"^(e2s exp)^"'"),
							"FunProps.eval", Logger.INTERNAL)
	 | Overflow => DynException.stdException(("Overflow detected when evaluating expression '"^(e2s exp)^"'"),
						 "FunProps.eval", Logger.INTERNAL)
	 | e => (Logger.log_error (Printer.$("Unknown error detected when evaluating expression '"^(e2s exp)^"'"));
 		 DynException.checkpoint "FunProps.eval" e)

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
		  Fun.op_list (* all the pre-defined operations *)

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
	case InstProps.getRealClassName props of
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
