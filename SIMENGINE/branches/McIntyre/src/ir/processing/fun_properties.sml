signature FUNPROPS =
sig

(* Duplicate the data types *)
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

(* Every operation and instance will have a data structure with each of these properties *)
type op_props = {name: string,
		 operands: operands,
		 precedence: int,
		 commutative: bool,
		 associative: bool,
		 eval: computationtype,
		 text: (string * fix),
		 C: (string * fix),
		 mathematica: (string * fix),
		 expcost: int,
		 codomain: int list list -> int list}


(* standard operation functions *)
val op2props : Fun.operation -> op_props (* returns the operation properties for a given function *)
val eval : Exp.exp -> Exp.exp (* will simplify and evaluate expressions - not fully implemented yet *)

(* return information about operations *)
val op2name : Fun.funtype -> string (* return the name of an operation as a string *)
val name2op : Symbol.symbol -> Fun.operation (* Given a symbol, return the operation *)
val fun2textstrnotation : Fun.funtype -> (string * fix) (* accessor to determine how to display op as text *)
val fun2cstrnotation : Fun.funtype -> (string * fix) (* accessor to determine how to display op as C code *)
val fun2mathematicastrnotation : Fun.funtype -> (string * fix) (* accessor to determine how to display op as Mathematica code *)
val hasVariableArguments : Fun.funtype -> bool (* operations like ADD and MUL can allow arbitrary numbers of operands *)

end

structure FunProps : FUNPROPS =
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
		 mathematica: (string * fix),
		 expcost: int,
		 codomain: int list list -> int list}


(* define default expression costs *)
val basicOpCost = 1
val transcendentalOpCost = 20

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

fun unaryfun2props (name, eval, cost) : op_props =
    {name=name,
     operands=FIXED 1,
     precedence=1,
     commutative=false,
     associative=false,
     eval=eval,
     text=(name, PREFIX),
     C=(name, PREFIX),
     mathematica=(Util.firstCap name, PREFIX),
     expcost=case cost of SOME v => v | NONE => transcendentalOpCost,
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
val plus = ExpBuild.plus
val sub = ExpBuild.sub
val neg = ExpBuild.neg
val times = ExpBuild.times
val divide = ExpBuild.divide
val power = ExpBuild.power
val t2e = Exp.TERM

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
			     complex=SOME (fn((r1,i1),(r2,i2)) => eval (complex (plus [t2e r1, t2e r2],
										 plus [t2e i1, t2e i2]))),
			     collection=NONE,
			     rational=SOME (fn((n1,d1),(n2,d2))=>if d1=d2 then 
								     frac (n1+n2,d1) 
								 else
								     frac (n1*d2 + n2*d1, d1*d2))},
		text=("+",INFIX),
		C=("+",INFIX),
		mathematica=("+",INFIX),
		expcost=basicOpCost,
		codomain= vectorizedCodomain}
      | SUB => {name="sub",
		operands=FIXED 2,
		precedence=6,
		commutative=false,
		associative=false,
		eval=BINARY {bool=NONE,
			     int=SOME (fn(i1,i2)=> int (i1-i2)),
			     real=SOME (fn(r1,r2)=> real (r1-r2)),
			     complex=SOME (fn((r1,i1),(r2,i2)) => eval (complex (sub (t2e r1, t2e r2),
										 sub (t2e i1, t2e i2)))),
			     collection=NONE,
			     rational=SOME (fn((n1,d1),(n2,d2))=>if d1=d2 then 
								     frac (n1-n2,d1) 
								 else
								     frac (n1*d2 - n2*d1, d1*d2))},
		text=("-",INFIX),
		C=("-",INFIX),
		mathematica=("-",INFIX),
		expcost=basicOpCost,
		codomain= vectorizedCodomain}
      | NEG => {name="neg",
		operands=FIXED 1,
		precedence=6,
		commutative=false,
		associative=false,
		eval=UNARY {bool=NONE,
			    int=SOME (fn(i)=> int (~i)),
			    real=SOME (fn(r)=> real (~r)),
			    complex=SOME (fn(r,i)=> eval( complex (neg (t2e r),
								   neg (t2e i)))),
			    rational=SOME (fn(n,d)=>frac (~n, d)),
			    collection=NONE},
		text=("-",PREFIX),
		C=("-",PREFIX),
		mathematica=("-($1)",MATCH),
		expcost=basicOpCost,
		codomain= vectorizedCodomain}
      | MUL => {name="mul",
		operands=VARIABLE (Exp.INT 1),
		precedence=5,
		commutative=true,
		associative=true,
		eval=BINARY {bool=NONE,
			     int=SOME (fn(i1,i2)=> int (i1*i2)),
			     real=SOME (fn(r1,r2)=> real (r1*r2)),
			     complex=SOME (fn((r1,i1),(r2,i2)) => 
					     let
						 val r1 = t2e r1
						 val i1 = t2e i1
						 val r2 = t2e r2
						 val i2 = t2e i2	
					     in
						 eval (complex (sub (times [r1, r2], times [i1, i2]),
								plus [times [r1, i2], times [r2, i1]]))
					     end),
			     collection=NONE,
			     rational=SOME (fn((n1,d1),(n2,d2))=>frac (n1*n2, d1*d2))},
		text=("*",INFIX),
		C=("*",INFIX),
		mathematica=(" ",INFIX), (* space *)
		expcost=basicOpCost,
		codomain= vectorizedCodomain}
      | DIVIDE => {name="divide",
		   operands=FIXED 2,
		   precedence=5,
		   commutative=false,
		   associative=false,
		   eval=BINARY {bool=NONE,
				int=SOME (fn(i1,i2)=> 
					    let 
						val int_div = Int.div (i1, i2)
						val real_div = (Real.fromInt i1)/
							       (Real.fromInt i2)
					    in
						if int_div = (Real.floor real_div) then
						    int int_div
						else
						    real real_div
					    end),		 
				real=SOME (fn(r1,r2)=> real (r1/r2)),
				complex=
				(* this can be more complicated - ComplexExpand[(r1 + i1 I)/(r2 + i2 I)] in Mathematica *)
				SOME (fn((r1,i1),(r2,i2)) => divide (complex (t2e r1, t2e i1),
									     complex (t2e r2, t2e i2))),
				collection=NONE,
				rational=SOME (fn((n1,d1),(n2,d2))=> frac (n1*d2, n2*d1))},
		   text=("/",INFIX),
		   C=("/",INFIX),
		   mathematica=("/",INFIX),
		   expcost=basicOpCost * 2,
		   codomain= vectorizedCodomain}
      | MODULUS => {name="modulus",
		    operands=FIXED 2,
		    precedence=5,
		    commutative=false,
		    associative=false,
		    eval=BINARY {bool=NONE,
				 int=SOME (fn(i1,i2)=> int (Int.mod (i1, i2))),
				 real=SOME (fn(r1,r2)=> real (r1-r2*Real.fromInt (Real.floor(r1/r2)))),
				 complex=NONE,
				 collection=NONE,
				 rational=NONE},
		    text=("%",INFIX),
		    C=("fmod($1,$2)",MATCH),
		    mathematica=("Mod",PREFIX),
		    expcost=basicOpCost * 2,
		    codomain= vectorizedCodomain}
      | POW => {name="pow",
		operands=FIXED 2,
		precedence=4,
		commutative=false,
		associative=false,
		eval=BINARY {bool=NONE,
			     int=SOME (fn(i1,i2)=>
					 if i2 = 0 then
					     int 1
					 else if i2 < 0 then
					     eval (ExpBuild.recip (power (int i1, int (~i2))))
					 else
					     int (foldl (fn(a,b)=> a*b) i1 (List.tabulate (i2, fn(x)=>i2)))),
			     real=SOME (fn(r1,r2)=>
					  if Real.?=(r2, 0.0) then
					      int 1
					  else 
					      real (Math.pow (r1, r2))),
			     complex=NONE, (* this can be more complicated - ComplexExpand[(r1 + i1 I)^(r2 + i2 I)] in Mathematica *)
			     collection=NONE,
			     rational=SOME (fn((n1,d1),(n2,d2))=> 
						  if d2 = 1 then (* this is the only one that we can really handle *)
						      eval (divide (power(int n1, int n2),
								    power(int d1, int n2)))
						  else (* can't do a whole lot, return the original... *)
						      power (frac(n1, d1),
							     frac(n2, d2)))},
		text=("^",INFIX),
		C=("pow($1,$2)",MATCH),
		mathematica=("^",INFIX),
		expcost=basicOpCost * 4,
		codomain= vectorizedCodomain}
      | COMPLEX => {name="complex",
		    operands=FIXED 2,
		    precedence=4,
		    commutative=false,
		    associative=false,
		    eval=empty_binary,
		    text=("complex",INFIX),
		    C=("complex",PREFIX),
		    mathematica=("Complex",PREFIX),
		    expcost=0,
		    codomain= vectorizedCodomain}
      | RE => unaryfun2props ("re", UNARY {bool=NONE,
					   int=NONE,
					   real=NONE,
					   complex=SOME (fn(r, i)=> t2e r),
					   rational=NONE,
					   collection=NONE}, SOME 0)
      | IM => unaryfun2props ("im", UNARY {bool=NONE,
					   int=NONE,
					   real=NONE,
					   complex=SOME (fn(r, i)=> t2e i),
					   rational=NONE,
					   collection=NONE}, SOME 0)
      | ABS => unaryfun2props ("abs", UNARY {bool=NONE,
					     int=SOME (int o r2i o abs o i2r),
					     real=SOME (real o abs),
					     complex=SOME (fn(r,i)=>complex (ExpBuild.norm [t2e r, t2e i],
									     int 0)),
					     rational=SOME (fn(n,d)=>frac (r2i (abs (i2r n)),
									   r2i (abs (i2r d)))),
					     collection=NONE}, SOME basicOpCost)
      | ARG => unaryfun2props ("arg", UNARY {bool=NONE,
					     int=SOME (fn(r)=> int 0),
					     real=SOME (fn(r)=> real 0.0),
					     complex=SOME (fn(r, i)=> ExpBuild.atan2(t2e r,t2e i)),
					     rational=NONE,
					     collection=NONE}, SOME transcendentalOpCost)
      | CONJ => {name="conj",
		 operands=FIXED 1,
		 precedence=1,
		 commutative=false,
		 associative=false,
		 eval=UNARY {bool=NONE,
			     int=NONE,
			     real=NONE,
			     complex=SOME (fn(r,i)=> eval (complex (t2e r,
								    neg (t2e i)))),
			     rational=NONE,
			     collection=NONE},
		 text=("conj",PREFIX),
		 C=("conj",PREFIX),
		 mathematica=("Conjugate",PREFIX),
		 expcost=basicOpCost,
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
							   val factor = ExpBuild.power (plus [ExpBuild.square (t2e r),
												       ExpBuild.square (t2e i)], 
											frac (1,4))
							   val inner = times [frac (1,2), ExpBuild.arg (t2e z)]
						       in
							   complex (times [factor, ExpBuild.cos inner],
								    times [factor, ExpBuild.sin inner])
						       end),
				       rational=NONE,
				       collection=NONE}, NONE)
      | DEG2RAD => unaryfun2props ("deg2rad",
				   UNARY {bool=NONE,
					  int=NONE,
					  real=SOME (fn(r)=> real (r * Math.pi / 180.0)),
					  complex=NONE,
					  rational=NONE,
					  collection=NONE}, SOME (basicOpCost * 3))
      | RAD2DEG => unaryfun2props ("rad2deg",
				   UNARY {bool=NONE,
					  int=NONE,
					  real=SOME (fn(r)=> real (r * 180.0 / Math.pi)),
					  complex=NONE,
					  rational=NONE,
					  collection=NONE}, SOME (basicOpCost * 3))
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
		 mathematica=("Log",PREFIX), (* supports two argument logs *)
		 expcost=transcendentalOpCost,
		 codomain= vectorizedCodomain}
      | EXP => unaryfun2props ("exp", empty_unary, NONE)
      | LOG => unaryfun2props ("log", empty_unary, NONE)
      | LOG10 => unaryfun2props ("log10", empty_unary, NONE)
      | SIN => unaryfun2props ("sin", empty_unary, NONE)
      | COS => unaryfun2props ("cos", empty_unary, NONE)
      | TAN => unaryfun2props ("tan", empty_unary, NONE)
      | CSC => {name="csc",
		operands=FIXED 1,
		precedence=1,
		commutative=false,
		associative=false,
		eval=empty_unary,
		text=("csc",PREFIX),
		C=("(1/sin($1))",MATCH),
		mathematica=("Csc",PREFIX),
		expcost=transcendentalOpCost,
		codomain= vectorizedCodomain}
      | SEC => {name="sec",
		operands=FIXED 1,
		precedence=1,
		commutative=false,
		associative=false,
		eval=empty_unary,
		text=("sec",PREFIX),
		C=("(1/cos($1))",MATCH),
		mathematica=("Sec",PREFIX),
		expcost=transcendentalOpCost,
		codomain= vectorizedCodomain}
      | COT => {name="cot",
		operands=FIXED 1,
		precedence=1,
		commutative=false,
		associative=false,
		eval=empty_unary,
		text=("cot",PREFIX),
		C=("(1/tan($1))",MATCH),
		mathematica=("Cot",PREFIX),
		expcost=transcendentalOpCost,
		codomain= vectorizedCodomain}
      | ASIN => unaryfun2props ("asin", empty_unary, NONE)
      | ACOS => unaryfun2props ("acos", empty_unary, NONE)
      | ATAN => unaryfun2props ("atan", empty_unary, NONE)
      | ATAN2 => {name="atan2",
		  operands=FIXED 2,
		  precedence=1,
		  commutative=false,
		  associative=false,
		  eval=empty_binary,
		  text=("atan2",PREFIX),
		  C=("atan2",PREFIX),
		  mathematica=("ArcTan",PREFIX),
		  expcost=transcendentalOpCost,
		  codomain= vectorizedCodomain}
      | ACSC => {name="acsc",
		 operands=FIXED 1,
		 precedence=1,
		 commutative=false,
		 associative=false,
		 eval=empty_unary,
		 text=("acsc",MATCH),
		 C=("asin(1/$1)",MATCH),
		 mathematica=("ArcCsc",PREFIX),
		 expcost=transcendentalOpCost,
		 codomain= vectorizedCodomain}
      | ASEC => {name="asec",
		 operands=FIXED 1,
		 precedence=1,
		 commutative=false,
		 associative=false,
		eval=empty_unary,
		 text=("asec",MATCH),
		 C=("acos(1/$1)",MATCH),
		mathematica=("ArcSec",PREFIX),
		expcost=transcendentalOpCost,
		codomain= vectorizedCodomain}
      | ACOT => {name="acot",
		 operands=FIXED 1,
		 precedence=1,
		 commutative=false,
		 associative=false,
		eval=empty_unary,
		 text=("acot",MATCH),
		 C=("atan(1/$1)",MATCH),
		mathematica=("ArcCot",PREFIX),
		expcost=transcendentalOpCost,
		codomain= vectorizedCodomain}
      | SINH => unaryfun2props ("sinh", empty_unary, NONE)
      | COSH => unaryfun2props ("cosh", empty_unary, NONE)
      | TANH => unaryfun2props ("tanh", empty_unary, NONE)
      | CSCH => {name="csch",
		 operands=FIXED 1,
		 precedence=1,
		 commutative=false,
		 associative=false,
		eval=empty_unary,
		 text=("csch",PREFIX),
		 C=("(1/sinh($1))",MATCH),
		mathematica=("Csch",PREFIX),
		expcost=transcendentalOpCost,
		codomain= vectorizedCodomain}
      | SECH => {name="sech",
		 operands=FIXED 1,
		 precedence=1,
		 commutative=false,
		 associative=false,
		eval=empty_unary,
		 text=("sech",PREFIX),
		 C=("(1/cosh($1))",MATCH),
		mathematica=("Sech",PREFIX),
		expcost=transcendentalOpCost,
		codomain= vectorizedCodomain}
      | COTH => {name="coth",
		 operands=FIXED 1,
		 precedence=1,
		 commutative=false,
		 associative=false,
		eval=empty_unary,
		 text=("coth",PREFIX),
		 C=("(1/tanh($1))",MATCH),
		mathematica=("Coth",PREFIX),
		expcost=transcendentalOpCost,
		codomain= vectorizedCodomain}
      | ASINH => {name="asinh",
		 operands=FIXED 1,
		 precedence=1,
		 commutative=false,
		 associative=false,
		eval=empty_unary,
		 text=("asinh",PREFIX),
		 C=("log($1 + sqrt($1*$1+1))",MATCH),
		mathematica=("ArcSinh",PREFIX),
		expcost=transcendentalOpCost,
		codomain= vectorizedCodomain}
      | ACOSH => {name="acosh",
		 operands=FIXED 1,
		 precedence=1,
		 commutative=false,
		 associative=false,
		eval=empty_unary,
		 text=("acosh",PREFIX),
		 C=("log($1 + sqrt($1*$1-1))",MATCH),
		mathematica=("ArcCosh",PREFIX),
		expcost=transcendentalOpCost,
		codomain= vectorizedCodomain}
      | ATANH => {name="atanh",
		 operands=FIXED 1,
		 precedence=1,
		 commutative=false,
		 associative=false,
		eval=empty_unary,
		 text=("atanh",PREFIX),
		 C=("(log((1+$1)/(1-$1))/2)",MATCH),
		mathematica=("ArcTanh",PREFIX),
		expcost=transcendentalOpCost,
		codomain= vectorizedCodomain}
      | ACSCH => {name="acsch",
		 operands=FIXED 1,
		 precedence=1,
		 commutative=false,
		 associative=false,
		eval=empty_unary,
		 text=("acsch",PREFIX),
		 C=("log(1/$1 + sqrt($1*$1+1)/fabs($1))",MATCH),
		mathematica=("ArcCsch",PREFIX),
		expcost=transcendentalOpCost,
		codomain= vectorizedCodomain}
      | ASECH => {name="asech",
		 operands=FIXED 1,
		 precedence=1,
		 commutative=false,
		 associative=false,
		eval=empty_unary,
		 text=("asech",PREFIX),
		 C=("log((1 + sqrt(1-$1*$1))/$1)",MATCH),
		mathematica=("ArcSech",PREFIX),
		expcost=transcendentalOpCost,
		codomain= vectorizedCodomain}
      | ACOTH => {name="acoth",
		 operands=FIXED 1,
		 precedence=1,
		 commutative=false,
		 associative=false,
		eval=empty_unary,
		 text=("acoth",PREFIX),
		 C=("(log(($1+1)/($1-1))/2)",MATCH),
		mathematica=("ArcCoth",PREFIX),
		expcost=transcendentalOpCost,
		codomain= vectorizedCodomain}
      | NOT => {name="not",
		operands=FIXED 1,
		precedence=3,
		commutative=false,
		associative=false,
		eval=empty_unary,
		text=("!",PREFIX),
		C=("!",PREFIX),
		mathematica=("!",PREFIX),
		expcost=basicOpCost,
		codomain= vectorizedCodomain}
      | AND => {name="and",
		operands=VARIABLE (Exp.BOOL true),
		precedence=13,
		commutative=false,
		associative=false,
		  eval=empty_binary,
		text=("&&",INFIX),
		C=("&&",INFIX),
		mathematica=("&&",INFIX),
		expcost=basicOpCost,
		codomain= vectorizedCodomain}
      | OR => {name="or",
	       operands=VARIABLE (Exp.BOOL false),
	       precedence=14,
	       commutative=false,
	       associative=false,
		  eval=empty_binary,
	       text=("||",INFIX),
	       C=("||",INFIX),
		mathematica=("||",INFIX),
		expcost=basicOpCost,
	       codomain= vectorizedCodomain}
      | GT => {name="gt",
	       operands=FIXED 2,
	       precedence=8,
	       commutative=false,
	       associative=false,
		  eval=empty_binary,
	       text=(">",INFIX),
	       C=(">",INFIX),
		mathematica=(">",INFIX),
		expcost=basicOpCost,
	       codomain= vectorizedCodomain}
      | LT => {name="lt",
	       operands=FIXED 2,
	       precedence=8,
	       commutative=false,
	       associative=false,
		  eval=empty_binary,
	       text=("<",INFIX),
	       C=("<",INFIX),
		mathematica=("<",INFIX),
		expcost=basicOpCost,
	       codomain= vectorizedCodomain}
      | GE => {name="ge",
	       operands=FIXED 2,
	       precedence=8,
	       commutative=false,
	       associative=false,
		  eval=empty_binary,
	       text=(">=",INFIX),
	       C=(">=",INFIX),
		mathematica=(">=",INFIX),
		expcost=basicOpCost,
	       codomain= vectorizedCodomain}
      | LE => {name="le",
	       operands=FIXED 2,
	       precedence=8,
	       commutative=false,
	       associative=false,
	       eval=empty_binary,
	       text=("<=",INFIX),
	       C=("<=",INFIX),
		mathematica=("<=",INFIX),
	       expcost=basicOpCost,
	       codomain= vectorizedCodomain}
      | EQ => {name="eq",
	       operands=FIXED 2,
	       precedence=9,
	       commutative=false,
	       associative=false,
		  eval=empty_binary,
	       text=("==",INFIX),
	       C=("==",INFIX),
		mathematica=("==",INFIX),
		expcost=basicOpCost,
	       codomain= vectorizedCodomain}
      | NEQ => {name="neq",
		operands=FIXED 2,
		precedence=9,
		commutative=false,
		associative=false,
		 eval=empty_unary,
		text=("<>",INFIX),
		C=("!=",INFIX),
		mathematica=("!=",INFIX),
		expcost=basicOpCost,
		codomain= vectorizedCodomain}
      | DERIV => {name="deriv",
		  operands=FIXED 2,
		  precedence=1,
		  commutative=false,
		  associative=false,
		  eval=empty_binary,
		  text=("D",PREFIX),
		  C=("Derivative",PREFIX),
		  mathematica=("D",PREFIX),
		  expcost=0,
		  codomain= vectorizedCodomain}
      | IF => {name="if",
	       operands=FIXED 3,
	       precedence=15,
	       commutative=false,
	       associative=false,
	       eval=empty_if_fun,
	       text=("If $1 then $2 else $3", MATCH),
	       C=("$1 ? $2 : $3",MATCH),
	       mathematica=("If",PREFIX),
	       expcost=basicOpCost,
	       codomain= vectorizedCodomain}
      | ASSIGN => {name="assign",
		   operands=FIXED 2,
		   precedence=16,
		   commutative=false,
		   associative=false,
		   eval=empty_binary,
		   text=("=",INFIX),
		   C=("=",INFIX),
		   mathematica=("==",INFIX),
		   expcost=basicOpCost,
		   codomain= vectorizedCodomain}
      | GROUP => {name="group",
		  operands=VARIABLE (Exp.LIST ([],[])),
		  precedence=1,
		  commutative=false,
		  associative=false,
		  eval=empty_binary,
		  text=("",PREFIX),
		  C=("",PREFIX),
		  mathematica=("",PREFIX),
		  expcost=0,
		  codomain= vectorizedCodomain}
      | NULL => {name="nullfun",
		 operands=FIXED 0,
		 precedence=1,
		 commutative=false,
		 associative=false,
		 eval=empty_binary,
		 text=("NULL",PREFIX),
		 C=("",PREFIX),
		 mathematica=("Null",PREFIX),
		 expcost=0,
		 codomain= vectorizedCodomain}
      | RADD => {name="reduction_add",
		 operands=FIXED 1,
		 precedence=6,
		 commutative=true,
		 associative=true,
		 eval=empty_unary,
		 text=("radd",PREFIX),
		 C=("simEngine_library_radd",PREFIX),
		mathematica=("Plus @@ $1",MATCH),
		 expcost=basicOpCost,
		 codomain=codomainReduction}
      | RMUL => {name="reduction_mul",
		 operands=FIXED 1,
		 precedence=5,
		 commutative=true,
		 associative=true,
		 eval=empty_unary,
		 text=("rmul",PREFIX),
		 C=("simEngine_library_rmul",PREFIX),
		mathematica=("Times @@ $1",MATCH),
		 expcost=basicOpCost,
		 codomain=codomainReduction}
      | RAND => {name="reduction_and",
		 operands=FIXED 1,
		 precedence=13,
		 commutative=true,
		 associative=true,
		 eval=empty_unary,
		 text=("rand",PREFIX),
		 C=("simEngine_library_rand",PREFIX),
		mathematica=("And @@ $1",MATCH),
		 expcost=basicOpCost,
		 codomain=codomainReduction}
      | ROR => {name="reduction_or",
		operands=FIXED 1,
		precedence=14,
		commutative=true,
		associative=true,
		 eval=empty_unary,
		text=("ror",PREFIX),
		C=("simEngine_library_ror",PREFIX),
		mathematica=("Or @@ $1",MATCH),
		expcost=basicOpCost,
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

fun op2name (f: funtype) = 
    case f
     of BUILTIN v => #name (op2props v)
      | INST {classname,...} => Symbol.name classname

fun fun2textstrnotation f =
    case f 
     of BUILTIN v => 
	let
	    val {text as (str, notation),...} = op2props v
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
	    val {C as (str, notation),...} = op2props v
	in
	    (str, notation)
	end
      | INST {classname,...} => (Symbol.name classname, PREFIX)

fun fun2mathematicastrnotation f =
    case f 
     of BUILTIN v => 
	let
	    val {mathematica as (str, notation),...} = op2props v
	in
	    (str, notation)
	end
      | INST {classname,...} => (Symbol.name classname, PREFIX)

fun hasVariableArguments f =
    case f 
     of BUILTIN v => 
	(case #operands (op2props v) of
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
