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



fun main (name, argv) =
    let
	val log = Logger.log_stdout (Logger.WARNINGS, defaultOptions)

	val _ = DynamoOptions.importRegistryFile (getSIMENGINEDOL ())
		before DynException.checkToProceed ()

	val env = PopulatedEnv.new (rep_loop false)

	(* Save/restore the world. *)
	val _ = if not MLton.Profile.isOn then
		    ignore (MLton.World.save (getSIMENGINESEW ()))
		else ()

	val _ = DynamoOptions.importRegistryFile (getSIMENGINEDOL ())
		before DynException.checkToProceed ()

	(* Execute the startup file. *)
	val (env, _) = Exec.run (rep_loop false) env
				[KEC.ACTION ((KEC.IMPORT "startup.dsl"), PosLog.NOPOS)]

	val userLog = Logger.log_add (getSIMENGINELOG (), Logger.ALL, defaultOptions)

	val log = if DynamoOptions.isFlagSet "verbose" then
		      Logger.log_stdout (Logger.ALL, defaultOptions)
		  else
		      Logger.log_stdout (Logger.WARNINGS, defaultOptions)

	(* Show the startup message *)
	val _ = print (Globals.startupMessage() ^ "\n")

	(* Verify the license file *)
	val _ = License.verifyNotRestricted () (* check to make sure that there the user/hostid/network/site specification is correct *)

	(* now verify that the version is correct *)
	val _ = License.isValidVersion (BuildOptions.majorVersion, BuildOptions.minorVersion)

	(* verify that it is not expired *)
	val _ = License.verifyExpired ()

    in
	case argv
	 of [] =>
	    let
		val dir = OS.FileSys.fullPath (OS.Path.currentArc)
	    in
		ParserSettings.setSettings (true, "STDIN", dir)
	      ; rep_loop true TextIO.stdIn env
	    end
	  | ["-batch"] => 
	    let
		val dir = OS.FileSys.fullPath (OS.Path.currentArc)
	    in
		ParserSettings.setSettings (true, "STDIN", dir)
	      ; rep_loop false TextIO.stdIn env
	    end
	  | [filename] => 
	    let
		val filename = OS.FileSys.fullPath filename
		val {dir, file} = OS.Path.splitDirFile filename
		val stream = TextIO.openIn filename
	    in
		ParserSettings.setSettings (false, file, dir)
	      ; rep_loop false stream env
	    end
	  | _ =>
	    (print ("Usage: " ^ name ^ " [optional filename]\n\n")
	   ; raise Usage)
      ; Logger.log_remove userLog
      ; Logger.log_remove log
      ; GeneralUtil.SUCCESS
    end
    handle DynException.TooManyErrors => 
	   GeneralUtil.FAILURE "Too many errors encountered"
	 | OOLCParse.ParserError => 
	   GeneralUtil.FAILURE "Error found when parsing source code"
	 | Usage => GeneralUtil.SUCCESS
	 | e => 
	   (DynException.log "Main" e
	  ; GeneralUtil.FAILURE "An exception was encountered")



end



val _ = 
    if MLton.Profile.isOn then
	Main.main (CommandLine.name (), CommandLine.arguments ())
    else
	MLton.World.load (getSIMENGINESEW ())
    handle _ => Main.main (CommandLine.name (), CommandLine.arguments ())
