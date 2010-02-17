structure Fun =
struct

														 

(* operation list *)
datatype operation = 
	 (* arithmetic operations *)
	 ADD | SUB | NEG | MUL | DIVIDE | MODULUS | POW | 
	 (* unary arithmetic operations *)
	 ABS | SQRT | DEG2RAD | RAD2DEG | 
	 (* complex functions *)
	 RE | IM | ARG | CONJ | COMPLEX |
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
	 DERIV | IF | ASSIGN | NULL

(* create a full op list *)
val op_list = 
    [(* arithmetic operations *)
     ADD, SUB, NEG, MUL, DIVIDE, MODULUS, POW, 
     (* unary arithmetic operations *)
     ABS, SQRT, DEG2RAD, RAD2DEG, 
     (* complex functions *)
     RE, IM, ARG, CONJ, COMPLEX, 
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
     DERIV, IF, ASSIGN]

    
datatype funtype = BUILTIN of operation
		 | INST of {classname:Symbol.symbol, 
			    instname:Symbol.symbol, 
			    props:InstProps.instproperties}


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


end
