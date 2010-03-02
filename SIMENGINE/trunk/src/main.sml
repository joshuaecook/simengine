structure Main = struct

exception Usage


fun log_stack e () =
    "  Exception stack trace:" ::
    (map (fn(s) => "    " ^ s ^ "\n") (MLton.Exn.history e))


fun rep_loop isInteractive textstream env =
    if !Globals.eof_encountered then 
	(KEC.UNIT, env)
    else 
	let
	    val _ = if isInteractive then 
			print ("SIM:" ^ (Int.toString (!ParserSettings.lineCount)) ^ "> ")
		    else
			()

	    val stms  = OOLCParse.parse textstream
	    val _ = Semant.check stms
	    val hlec = ASTTrans.ast2hlec stms
	    val kec = Desugar.hlec2kec hlec

	    fun exec_exp (exp: KEC.exp) = 
		let
		    val (_, exp') = Exec.run (rep_loop false) env [KEC.ACTION (KEC.EXP exp, PosLog.NOPOS)]
		in
		    exp'
		end

	    (*val _ = Logger.log_hash()*)
	    val (env', kec') = Exec.run (rep_loop false) env kec

	    val _ = if isInteractive then 
			ShellPrint.showResult exec_exp kec'
		    else
			()
	in
	    rep_loop isInteractive textstream env'
	end
	handle err as OOLCParse.ParserError => 
	       (Logger.log_error (Printer.$"Parse errors encountered");
		   (*DynException.log "Main" err;	     
		   if DynamoOptions.isFlagSet("debugexceptions") then
		       app print (log_stack err ())
		   else
		       ();*)
		   rep_loop isInteractive textstream env)
	     | DynException.RestartRepl => 
	       if isInteractive then 
		      (DynException.resetErrored()
		     ; rep_loop isInteractive textstream env) 
		  else (KEC.UNIT, env)
	     | e as DynException.TooManyErrors =>
	       if isInteractive then
		   (DynException.resetErrored()
		  ; rep_loop isInteractive textstream env)
	       else
		   raise e
	       
		  
	

val defaultOptions = [Logger.LIBRARY]



(* This is essentially the same as MLton.World.load
 * except we keep the list of command line arguments. *)
fun resume () =
    let val sew = getSIMENGINESEW ()
    in
	if OS.FileSys.access (sew, [OS.FileSys.A_READ]) then
	    let val c = CommandLine.name ()
	    in
		Posix.Process.exec (c, [c, "@MLton", "load-world", sew, "--"] @ (CommandLine.arguments ()))
	    end
	else raise Fail ("Cannot load SEW file " ^ sew)
    end


fun main () =
    let
	val log = Logger.log_stdout (Logger.WARNINGS, defaultOptions)

	val _ = DynamoOptions.importRegistryFile (getSIMENGINEDOL ())
		before DynException.checkToProceed ()

	val env = PopulatedEnv.new (rep_loop false)

	(* Save/restore the world. *)
	val _ = 
	    if not MLton.Profile.isOn then
		ignore (MLton.World.save (getSIMENGINESEW ()))
	    else ()

	(* Verify the license file *)
	val _ = License.verifyNotRestricted () (* check to make sure that there the user/hostid/network/site specification is correct *)

	(* now verify that the version is correct *)
	val _ = License.isValidVersion (BuildOptions.majorVersion, BuildOptions.minorVersion)

	(* verify that it is not expired *)
	val _ = License.verifyExpired ()

	val argv = CommandLine.arguments ()

	val _ = DynamoOptions.importRegistryFile (getSIMENGINEDOL ())
		before DynException.checkToProceed ()

	(* Execute the startup file. *)
	val (env, _) = Exec.run (rep_loop false) env
				[KEC.ACTION 
				     (KEC.EXP (KEC.APPLY {func=KEC.SYMBOL (Symbol.symbol "startup"),
							  args=KEC.UNIT}),
				      PosLog.NOPOS)]

	val userLog = Logger.log_add (getSIMENGINELOG (), Logger.ALL, defaultOptions)

	val log = if DynamoOptions.isFlagSet "verbose" then
		      Logger.log_stdout (Logger.ALL, defaultOptions)
		  else
		      Logger.log_stdout (Logger.WARNINGS, defaultOptions)

	val dir = OS.FileSys.fullPath (OS.Path.currentArc)

	fun indexOf list =
	    let val tab = ListPair.zipEq (List.tabulate (length list, fn n => n), list)
	    in
	     fn f => 
		case List.find (fn (_, x) => f x) tab
		 of SOME (n, _) => SOME n
		  | NONE => NONE
	    end

	fun strEquals x y =
	    case String.compare (x, y) of EQUAL => true | _ => false
    in
	case indexOf argv (strEquals "-simex")
	 of SOME n => 
	    (* Noninteractive operating on a model definition. *)
	    (KEC.UNIT, env)
	  | NONE => 
	    case indexOf argv (strEquals "-batch")
	     of SOME n =>
		let val filename = if length argv > n + 1 then List.nth (argv, 1 + n) else "-"
		in 
		    if "-" <> filename orelse 0 < String.size filename andalso #"-" <> String.sub (filename, 0) then
			(* Noninteractive reading from a file. *)
			let
			    val filename = OS.FileSys.fullPath filename
					   handle _ => (print ("Unable to locate " ^ filename ^ "\n")
						      ; raise Usage)
			    val {dir, file} = OS.Path.splitDirFile filename
			    val stream = TextIO.openIn filename
					   handle _ => (print ("Unable to read " ^ filename ^ "\n")
						      ; raise Usage)
			in
			    ParserSettings.setSettings (false, file, dir)
			  ; rep_loop false stream env
			end
		    else
			(* Noninteractive reading from STDIN. *)
			(ParserSettings.setSettings (true, "STDIN", dir)
		       ; print (Globals.startupMessage() ^ "\n")
		       ; rep_loop false TextIO.stdIn env)
		end
	      | NONE =>
		(* Interactive reading from STDIN. *)
		(ParserSettings.setSettings (true, "STDIN", dir)
	       ; print (Globals.startupMessage() ^ "\n")
	       ; rep_loop true TextIO.stdIn env)
      ; Logger.log_remove userLog
      ; Logger.log_remove log
      ; GeneralUtil.SUCCESS
    end
    handle DynException.TooManyErrors => 
	   GeneralUtil.FAILURE "Too many errors encountered"
	 | DynException.RestartRepl => 
	   GeneralUtil.FAILURE "Error occurred in standard library"
	 | OOLCParse.ParserError => 
	   GeneralUtil.FAILURE "Error found when parsing source code"
	 | Usage => 
	   GeneralUtil.FAILURE ("usage: " ^ (CommandLine.name ()) ^ " -batch [file]\n" ^
				"       " ^ (CommandLine.name ()) ^ " [option] -simex model")
	 | e => 
	   (DynException.log "Main" e
	  ; GeneralUtil.FAILURE "An exception was encountered")



end

val _ =
    case (if MLton.Profile.isOn then
	      Main.main ()
	  else
	      (Main.resume () handle _ => Main.main ()))
     of GeneralUtil.SUCCESS => ()
      | GeneralUtil.FAILURE message => 
	(print (message ^ "\n")
       ; OS.Process.exit OS.Process.failure)
