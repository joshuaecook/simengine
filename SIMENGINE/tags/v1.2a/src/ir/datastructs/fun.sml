structure Fun =
struct

open MathFunctions

type operation = MathFunctions.operation

datatype funtype 
  (* A primitive operation as listed above. *)
  = BUILTIN of operation
  (* A call to a model instance; returns no value and has side-effects on the instance's state. *)
  | INST of {classname: Symbol.symbol, 
	     instname: Symbol.symbol, 
	     props: InstProps.instproperties}
  (* A call to obtain an output value from a model instance. *)
  | OUTPUT of {classname: Symbol.symbol,
	       instname: Symbol.symbol,
	       outname: Symbol.symbol,
	       props: InstProps.instproperties}


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
