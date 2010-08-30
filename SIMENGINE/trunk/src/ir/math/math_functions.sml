structure MathFunctions =
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
	 (* rounding operations *)
	 FLOOR | CEILING | ROUND | 
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
     (* rounding operations *)
     FLOOR, CEILING, ROUND,
     (* reduction operations *)
     RADD, RMUL, RAND, ROR, 
     (* special purpose operations *)
     DERIV, IF, ASSIGN]

    


end
