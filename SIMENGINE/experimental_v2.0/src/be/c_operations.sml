signature COPERS =
sig

type op_t
type type_t
type target_t
type syntax_t

type atom_t
type code_result

val operToUsage : op_t -> type_t list -> target_t -> ((type_t list * type_t list) * syntax_t)
val operToC : op_t -> type_t list -> target_t -> (atom_t list * atom_t list) -> code_result

end
structure C_Operations : COPERS =
struct

datatype op_t = datatype MathFunctions.operation
datatype type_t = datatype CType.t
datatype target_t = datatype Target.target
datatype layout_t = datatype Layout.t

type arg = layout_t
type destructive_t = bool (* indicated whether or not the input argument is changed by calling this procedure *)
datatype passing_t = ByValue | ByReference
datatype syntax_t = 
	 PassThrough
       | InfixFunction of string
       | PrefixFunction of string
       | PrefixProcedure of {name: string,
			     input_arguments: (destructive_t * passing_t) list, (* list of input types *)
			     output_arguments: passing_t list, (* list of output types - in almost all cases, this has to be
								* ByReference, except maybe if the function operators on a 
								* pointer *)
			     side_effect: bool, (* creates a side effect, so the ordering of this operation could be important *)
			     return_status: bool (* does it return a status or a void *)
			    }

fun invalid (msg) = DynException.stdException(("Invalid operation: "^msg), "COperations", Logger.INTERNAL)
fun noargs (oper) = DynException.stdException(("Operation "^oper^" must have at least one argument"), "COperations", Logger.INTERNAL)
fun binop (oper) = DynException.stdException(("Operation "^oper^" is supported as a binary operation only"), "COperations", Logger.INTERNAL)
fun unsupporteddatatype (oper) = DynException.stdException(("Operation "^oper^" was accessed with unsupported data types"), "COperations", Logger.INTERNAL)
fun unsupportedcontainer (oper) = DynException.stdException(("Operation "^oper^" was accessed with unsupported data types"), "COperations", Logger.INTERNAL)

local
    open CType
    structure F = MathFunctionProperties
    fun binary_infix (t1, t2) oper = 
	let
	    val (t1', t2') = commensuratePair (t1, t2)
	    val b = typeToBase t1'
	    val s = typeToSize t1'
	    val _ = if directCSupport (b, s) then
			()
		    else
			unsupporteddatatype oper
	    val (ot, func) = case (typeToContainer t1', typeToContainer t2') of
				 (Scalar, Scalar) => (toType {container=Scalar, base=b, size=s},
						      InfixFunction oper)
			       | _ => unsupportedcontainer oper
	in
	    (([t1', t2'], [ot]), func)
	end
in
fun operToUsage F.ADD type_list target =
    (case type_list of
	 [] => noargs "+"
       | [onetype] => (([onetype],[onetype]), PassThrough)
       | [t1 as (Scalar, _, _), t2 as (Scalar, _, _)] => binary_infix (t1, t2) "+"
       | [t1, t2] => unsupportedcontainer "+"
       | _ => binop "+")
  | operToUsage F.SUB type_list target =
    (case type_list of
	 [] => noargs "-"
       | [onetype] => binop "-"
       | [t1 as (Scalar, _, _), t2 as (Scalar, _, _)] => binary_infix (t1, t2) "-"
       | [t1, t2] => unsupportedcontainer "-"
       | _ => binop "-")
  | operToUsage F.MUL type_list target =
    (case type_list of
	 [] => noargs "*"
       | [onetype] => (([onetype],[onetype]), PassThrough)
       | [t1 as (Scalar, _, _), t2 as (Scalar, _, _)] => binary_infix (t1, t2) "*"
       | [t1, t2] => unsupportedcontainer "*"
       | _ => binop "*")
  | operToUsage F.DIVIDE type_list target =
    (case type_list of
	 [] => noargs "/"
       | [onetype] => binop "/"
       | [t1 as (Scalar, _, _), t2 as (Scalar, _, _)] => binary_infix (t1, t2) "/"
       | [t1, t2] => unsupportedcontainer "/"
       | _ => binop "/")
  | operToUsage F.POW type_list target =
    (case type_list of
	 [] => noargs "power"
       | [onetype] => binop "power"
       | [t1 as (Scalar, b1, s1), t2 as (Scalar, b2, s2)] => 
	 let
	     (*      double pow (      double base,      double exponent ); *)
	     (* long double pow ( long double base, long double exponent ); *)
	     (*       float pow (       float base,       float exponent ); *)
	     (*      double pow (      double base,         int exponent ); *)
	     (* long double pow ( long double base,         int exponent ); *)
	     val t1' as (_, b1', s1') =
		 if directCSupport (b1, s1) then
		     case (b1, s1) of
			 (Integer, _) => (Scalar, Real, s1)
		       | _ => t1
		 else
		     unsupporteddatatype "power"
	     val t2' as (_, b2', s2') =
		 if directCSupport (b2, s2) then
		     case (s1', (b2, s2)) of
			 (bits64, (Real, _)) => (Scalar, Real, bits64)
		       | (bits64, (Integer, bits32)) => (Scalar, Integer, bits32)
		       | (bits64, _) => unsupporteddatatype "power"
		       | (bits32, (_, bits32)) => (Scalar, Real, bits32) (* convert integers to float *)
		       | (_, _) => unsupporteddatatype "power"					
		 else
		     unsupporteddatatype "power"
	     val ot = t1'
	 in
	     (([t1',t2'], [ot]), PrefixFunction "pow")
	 end
       | [t1, t2] => unsupportedcontainer "power"
       | _ => binop "power")
  | operToUsage oper type_list target = invalid (MathFunctionProperties.op2name oper)
end

(*fun operToSyntax ADD _ _ = InfixFunction "+"
  | operToSyntax MATMUL [banded_matrix (4,5), matrix (5, 6)] GPU = "MatMult(c, a, b, 4, 5, 6)"
  | operToSyntax MATMUL [matrix (4,5), matrix (5, 6)] CPU = "LaPackMatMult(c, a, b, 4, 5, 5, 6)"
  | operToSyntax LINSOLVE [matrix (a,b), vector (c)] _ = ()
  | operToSyntax MATRIXINVERT [matrix upper_triangular (a)] CPU = *)


local
    open Spil
in
datatype atom_t = datatype Atom.t
datatype code_result
  = Expression of Expression.t
  | StatementList of Statement.t list
fun operToC oper type_list target = (fn(inputs, outputs)=> Expression (Expression.Value Atom.Null))
end




end
