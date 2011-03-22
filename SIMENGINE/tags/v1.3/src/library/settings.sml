(*
Copyright (C) 2011 by Simatra Modeling Technologies

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.

*)

structure SettingsLib =
struct

open LibraryUtil

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
		       (OptionsList.FLAG_T, KEC.LITERAL (KEC.CONSTBOOL b)) 
		       => DynamoOptions.setSetting(DynamoOptions.FLAG (name, b))
		     | (OptionsList.INTEGER_T, KEC.LITERAL (KEC.CONSTREAL r))
		       => DynamoOptions.setSetting(DynamoOptions.SETTING(name, DynamoOptions.INTEGER (Real.floor r)))
		     | (OptionsList.REAL_T, KEC.LITERAL (KEC.CONSTREAL r))
		       => DynamoOptions.setSetting(DynamoOptions.SETTING(name, DynamoOptions.REAL (r)))
		     | (OptionsList.STRING_T, KEC.LITERAL (KEC.CONSTSTR s))
		       => DynamoOptions.setSetting(DynamoOptions.SETTING(name, DynamoOptions.STRING s))
		     | (OptionsList.INTEGER_VECTOR_T, KEC.VECTOR v)
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
		     | (OptionsList.REAL_VECTOR_T, KEC.VECTOR v)
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
		     | (OptionsList.STRING_VECTOR_T, KEC.VECTOR v)
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


val library = [{name="setSetting", operation=std_setsetting},
	       {name="settingsToJSON",
		operation = fn exec => unitToStringFun (PrintJSON.toString o DynamoOptions.allSettingsToJSON)}
	      ]

end
