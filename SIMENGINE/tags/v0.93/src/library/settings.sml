structure SettingsLib =
struct

val TypeMismatch = DynException.TypeMismatch
and IncorrectNumberOfArguments = DynException.IncorrectNumberOfArguments

fun error msg =
    Logger.log_error (Printer.$ msg)

exception Aborted

fun std_setsetting exec args =
    (case args of
	 [KEC.LITERAL (KEC.CONSTSTR name), value] => 
	 (let
	      exception SkipSetting

	      (*check that the value is the correct type.  If so, convert it and assign in *)
	      val _ =
		  (case (DynamoOptions.getTypeForSetting name, value) of
		       (DynamoOptions.FLAG_T, KEC.LITERAL (KEC.CONSTBOOL b)) 
		       => DynamoOptions.setSetting(DynamoOptions.FLAG (name, b))
		     | (DynamoOptions.INTEGER_T, KEC.LITERAL (KEC.CONSTREAL r))
		       => DynamoOptions.setSetting(DynamoOptions.SETTING(name, DynamoOptions.INTEGER (Real.floor r)))
		     | (DynamoOptions.REAL_T, KEC.LITERAL (KEC.CONSTREAL r))
		       => DynamoOptions.setSetting(DynamoOptions.SETTING(name, DynamoOptions.REAL (r)))
		     | (DynamoOptions.STRING_T, KEC.LITERAL (KEC.CONSTSTR s))
		       => DynamoOptions.setSetting(DynamoOptions.SETTING(name, DynamoOptions.STRING s))
		     | (DynamoOptions.INTEGER_VECTOR_T, KEC.VECTOR v)
		       => 
		       let
			   fun entry2int (KEC.LITERAL(KEC.CONSTREAL r)) =
			       Real.floor r
			     | entry2int _ =
			       raise SkipSetting before error "Invalid types on setting arguments"
			       
			   val iv = map entry2int (KEC.kecvector2list v)
		       in
			   DynamoOptions.setSetting(DynamoOptions.SETTING(name, DynamoOptions.INTEGER_VEC iv))
		       end
		     | (DynamoOptions.REAL_VECTOR_T, KEC.VECTOR v)
		       =>
		       let
			   fun entry2real (KEC.LITERAL(KEC.CONSTREAL r)) =
			       r
			     | entry2real _ =
			       raise SkipSetting before error "Invalid types on setting arguments"

			   val rv = map entry2real (KEC.kecvector2list v)
		       in
			   DynamoOptions.setSetting(DynamoOptions.SETTING(name, DynamoOptions.REAL_VEC rv))
		       end
		     | (DynamoOptions.STRING_VECTOR_T, KEC.VECTOR v)
		       =>
		       let
			   fun entry2str (KEC.LITERAL(KEC.CONSTSTR s)) =
			       s
			     | entry2str _ =
			       raise SkipSetting before error "Invalid types on setting arguments"

			   val sv = map entry2str (KEC.kecvector2list v)
		       in
			   DynamoOptions.setSetting(DynamoOptions.SETTING(name, DynamoOptions.STRING_VEC sv))
		       end
		     | _ =>
		       raise SkipSetting before error "Invalid types on setting arguments"
		  ) handle SkipSetting => ()  
	 in 
	     KEC.UNIT
	 end	 
	 handle Aborted => KEC.UNIT)
       | _ => raise IncorrectNumberOfArguments {expected=2, actual=(length args)})


val library = [{name="setSetting", operation=std_setsetting}]

end
