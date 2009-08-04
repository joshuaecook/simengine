(* This module ties in the options.sml compiler settings with the interactive shell *)

signature SHELLOPTIONS =
sig

val getOptionsInit: unit -> KEC.stm list

end

structure ShellOptions : SHELLOPTIONS =
struct

fun getOptionsInit () =
    let
	fun bool2exp value =
	    HLEC.LITERAL (HLEC.CONSTBOOL value)

datatype dynvalue = INTEGER of int
		  | REAL of real
		  | STRING of string
		  | INTEGER_VEC of int list
		  | REAL_VEC of real list
		  | STRING_VEC of string list

	fun value2exp (DynamoOptions.INTEGER i) = HLEC.LITERAL (HLEC.CONSTREAL (Real.fromInt i))
	  | value2exp (DynamoOptions.REAL r) = HLEC.LITERAL (HLEC.CONSTREAL r)
	  | value2exp (DynamoOptions.STRING s) = HLEC.LITERAL (HLEC.CONSTSTR s)
	  | value2exp (DynamoOptions.INTEGER_VEC iv) = HLEC.VECTOR (map (fn(i) => HLEC.LITERAL (HLEC.CONSTREAL (Real.fromInt i))) iv)
	  | value2exp (DynamoOptions.REAL_VEC rv) = HLEC.VECTOR (map (fn(r) => HLEC.LITERAL (HLEC.CONSTREAL (r))) rv)
	  | value2exp (DynamoOptions.STRING_VEC sv) = HLEC.VECTOR (map (fn(s) => HLEC.LITERAL (HLEC.CONSTSTR (s))) sv)

	fun build_setting (DynamoOptions.FLAG (name, value)) =
	    (HLEC.PUBLIC,
	     HLEC.DEFINITION(HLEC.DEFLOCAL (Symbol.symbol name, 
					    HLEC.TYPE (Symbol.symbol "Setting"), 
					    HLEC.APPLY{func=HLEC.SEND{message=Symbol.symbol "new", object=HLEC.SYMBOL (Symbol.symbol "Setting")},
						       args=HLEC.TUPLE [HLEC.LITERAL (HLEC.CONSTSTR name),
									HLEC.LITERAL (HLEC.CONSTBOOL value),
									HLEC.TYPEEXP (HLEC.TYPE (Symbol.symbol "Boolean")),
									HLEC.VECTOR (map (fn(s) => HLEC.LITERAL (HLEC.CONSTSTR s)) (DynamoOptions.getDescriptionForSetting name))]}), 
			     PosLog.NOPOS))
	  (*	    HLEC.DEFINITION(HLEC.DEFLOCAL (name, HLEC.TYPE "Boolean", bool2exp value), PosLog.NOPOS)*)

	  | build_setting (DynamoOptions.SETTING (name, value)) =
	    let 
		fun value2exp (DynamoOptions.INTEGER i) = 
		    (HLEC.LITERAL (HLEC.CONSTREAL (Real.fromInt i)),
		     HLEC.TYPEEXP (HLEC.TYPE (Symbol.symbol "Number")))
		  | value2exp (DynamoOptions.REAL r) = 
		    (HLEC.LITERAL (HLEC.CONSTREAL r),
		     HLEC.TYPEEXP (HLEC.TYPE (Symbol.symbol "Number")))
		  | value2exp (DynamoOptions.STRING s) = 
		    (HLEC.LITERAL (HLEC.CONSTSTR s),
		     HLEC.TYPEEXP (HLEC.TYPE (Symbol.symbol "String")))
		  | value2exp (DynamoOptions.INTEGER_VEC iv) = 
		    (HLEC.VECTOR (map (fn(i) => HLEC.LITERAL (HLEC.CONSTREAL (Real.fromInt i))) iv),
		     HLEC.TYPEEXP (HLEC.COMPOUNDTYPE (Symbol.symbol "Vector", HLEC.TYPE (Symbol.symbol "Number"))))
		  | value2exp (DynamoOptions.REAL_VEC rv) = 
		    (HLEC.VECTOR (map (fn(r) => HLEC.LITERAL (HLEC.CONSTREAL (r))) rv),
		     HLEC.TYPEEXP (HLEC.COMPOUNDTYPE (Symbol.symbol "Vector", HLEC.TYPE (Symbol.symbol "Number"))))
		  | value2exp (DynamoOptions.STRING_VEC sv) = 
		    (HLEC.VECTOR (map (fn(s) => HLEC.LITERAL (HLEC.CONSTSTR (s))) sv),
		     HLEC.TYPEEXP (HLEC.COMPOUNDTYPE (Symbol.symbol "Vector", HLEC.TYPE (Symbol.symbol "String"))))

		val (value, valType) = value2exp value
	    in
		(HLEC.PUBLIC,
		 HLEC.DEFINITION(HLEC.DEFLOCAL (Symbol.symbol name, 
						HLEC.TYPE (Symbol.symbol "Setting"), 
						HLEC.APPLY{func=HLEC.SEND{message=Symbol.symbol "new", object=HLEC.SYMBOL (Symbol.symbol "Setting")},
							   args=HLEC.TUPLE [HLEC.LITERAL (HLEC.CONSTSTR name),
									    value,
									    valType,
									    HLEC.VECTOR (map (fn(s) => HLEC.LITERAL (HLEC.CONSTSTR s)) (DynamoOptions.getDescriptionForSetting name))]}), 
				 PosLog.NOPOS))
	    end
(*	    HLEC.DEFINITION(HLEC.DEFLOCAL (name, HLEC.DONTCARE, value2exp value), PosLog.NOPOS)*)
	    

(*	val stms = map build_setting (DynamoOptions.getSettingsList())*)

	fun build_group {group, tag, ...} =
	    (HLEC.PUBLIC,
	     HLEC.DEFINITION(HLEC.DEFNAMESPACE {name=Symbol.symbol tag,
						stms=map build_setting (DynamoOptions.getSettingsForGroup group)},
			     PosLog.NOPOS))

	val stms = map build_group (DynamoOptions.getGroupsList())
		   
    in			
	Desugar.hlec2kec ([HLEC.DEFINITION (HLEC.DEFNAMESPACE {name=Symbol.symbol "settings",
							       stms=stms},
					    PosLog.NOPOS)])
    end



end
