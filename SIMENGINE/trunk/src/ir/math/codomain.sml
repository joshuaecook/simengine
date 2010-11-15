signature CODOMAIN =
sig

    type transform = Space.space list -> Space.space

    (* various codomain transforms *)
    val vectorizedCodomain : transform
    val codomainReduction : transform
    val transposeCodomain : transform
    val matrixMulCodomain : transform
    val linearSolveCodomain : transform

end
structure Codomain : CODOMAIN = 
struct

type transform = Space.space list -> Space.space

val i2s = Util.i2s

fun vectorizedCodomain nil = DynException.stdException (("must have arguments for a vectorized codomain"), "MathFunctionProperties.vectorizedCodomain'.combineSizes", Logger.INTERNAL)
  | vectorizedCodomain (first::rest) =
    let
	fun combineSpaces (space1, space2) = 
	    if Space.equal (space1, space2) then space1
	    else if Space.isScalar space1 then space2
	    else if Space.isScalar space2 then space1
	    else if Space.isEmpty space1 andalso Space.isEmpty space2 then
		Space.emptyCollection
	    else
		(DynException.stdException (("Arguments have mismatched spaces ("^(Space.toString space1)^","^(Space.toString space2)^")"), "MathFunctionProperties.vectorizedCodomain.combineSizes", Logger.INTERNAL))
    in
	foldl combineSpaces first rest
    end

fun codomainReduction [arg] = 
    Space.reduceCodomain arg
  | codomainReduction args = DynException.stdException (("There should be only one argument for reduction operations, instead found " ^ (i2s (List.length args))), "MathFunctionProperties.codomainReduction", Logger.INTERNAL)

fun transposeCodomain [space] =
    if Space.isEmpty space orelse Space.isScalar space then
	space
    else if Space.isVector space then
	Space.fromMatrixDims (1, Space.toVectorDim space)
    else if Space.isMatrix space then
	let
	    val (i,j) = Space.toMatrixDims space
	in 
	    (* let's see how this works - by doing this check, transpose(transpose(v)) where v is a vector
	     * is reversible. *)
	    if i = 1 then
		Space.fromVectorDim (j)
	    else
		Space.fromMatrixDims (j, i)
	end
    else
	DynException.stdException (("Transpose does not accept arguments of type " ^ (Space.toString space)), "MathFunctionProperties.transposeCodomain", Logger.INTERNAL)
  | transposeCodomain args = DynException.stdException (("There should be only one argument for the transpose operation, instead found " ^ (i2s (List.length args))), "MathFunctionProperties.transposeCodomain", Logger.INTERNAL)

fun matrixMulCodomain [space1, space2] =
    let
	fun toDims space = 
	    if Space.isEmpty space then
		(0, 0)
	    else if Space.isScalar space then
		(1, 1)
	    else if Space.isVector space then
		(Space.toVectorDim space, 1)
	    else if Space.isMatrix space then
		Space.toMatrixDims space
	    else
		DynException.stdException(("Matrix multiplication does not accept arguments of type " ^ (Space.toString space)), "MathFunctionProperties.matrixMulCodomain", Logger.INTERNAL)
	val (a_i, a_j) = toDims space1
	val (b_i, b_j) = toDims space2
	val c = if b_j = 1 then
		    Space.fromVectorDim a_i
		else
		    Space.fromMatrixDims (a_i, b_j)
    in
	if a_j = b_i then
	    c
	else
	    DynException.stdException(("Matrix multiplcation expects inner dimensions to be equal, instead ("^(Space.toString space1)^"*"^(Space.toString space1)^")"),
				      "MathFunctionProperties.matrixMulCodomain", Logger.INTERNAL)
    end
  | matrixMulCodomain args = DynException.stdException (("There should be only two arguments for the matrix_mul operation, instead found " ^ (i2s (List.length args))), "MathFunctionProperties.matrixMulCodomain", Logger.INTERNAL)

fun linearSolveCodomain [space1, space2] = 
    let
	fun isSquareMatrix space = 
	    Space.isMatrix space andalso
	    let	val (i, j) = Space.toMatrixDims space
	    in i = j
	    end
    in
	if Space.isScalar space1 andalso Space.isScalar space2 then
	    Space.scalar
	else if isSquareMatrix space1 andalso Space.isVector space2 then
	    let 
		val (i, _) = Space.toMatrixDims space1
		val v = Space.toVectorDim space2
	    in
		if i = v then
		    space2
		else	  
		    DynException.stdException(("Linear solver expects matrix to have the same size as vector, instead ("^(Space.toString space1)^"*"^(Space.toString space1)^")"),
					      "MathFunctionProperties.linearSolveCodomain", Logger.INTERNAL)
	    end
	else
	    DynException.stdException(("Linear solver expects first argument to be a square matrix and the second argument to be a vector, instead ("^(Space.toString space1)^"*"^(Space.toString space1)^")"),
				      "MathFunctionProperties.linearSolveCodomain", Logger.INTERNAL)
    end
  | linearSolveCodomain args = DynException.stdException (("There should be only two arguments for the linear_solve operation, instead found " ^ (i2s (List.length args))), "MathFunctionProperties.linearSolveCodomain", Logger.INTERNAL)



end
