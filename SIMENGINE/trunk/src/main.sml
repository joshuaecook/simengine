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


(* this function will handle reading from the dol files and the command line arguments
 * to update the internal settings registry. *)
fun processSettings () =
    let
	val argv = CommandLine.arguments ()

	(* Read the global registry file in the installation directory *)
	val _ = (if not MLton.Profile.isOn then 
		     (DynamoOptions.importRegistryFile (getSIMENGINEDOL ());
		      case getSIMENGINELOCALDOL() of
			  SOME dol => DynamoOptions.importRegistryFile dol
			| NONE => ();
		      case getUPDATEDOL() of
			  SOME dol => DynamoOptions.importRegistryFile dol
			| NONE => ())
		 else
		     ())
		before DynException.checkToProceed ()

	(* read in command line arguments to DynamoOptions *)
	val _ = Logger.log_notice (Printer.$("Arguments to simEngine: " ^ (Util.l2s argv)))
	val dsl_files = DynamoOptions.importCommandLineArgs argv
    in
	DynException.checkToProceed()
    end
    handle DynException.TooManyErrors => raise DynException.SettingsError

(* Execute the startup file - startup.dsl *)
fun startup env = 
    let
	val (env, _) = Exec.run (rep_loop false) env
					[KEC.ACTION 
					     (KEC.EXP (KEC.APPLY {func=KEC.SYMBOL (Symbol.symbol "startup"),
								  args=KEC.UNIT}),
					      PosLog.NOPOS)]
    in
	env
    end


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

	val dol = if MLton.Profile.isOn then
		      getSIMENGINEPROFILEDOL () (* this should be ../datafiles/profile.dol *)
		  else
		      getSIMENGINELOCALDOL () (* in this case, it's going to be the system one *)

	val _ = case dol of
		    SOME file => DynamoOptions.importRegistryFile file
		  | NONE => (Logger.log_failure(Printer.$("Can't read system registry file"));
			     DynException.setErrored())
		    before DynException.checkToProceed ()

	(* read in command line arguments to DynamoOptions *)
	(*val _ = Util.log ("Args: " ^ (Util.l2s argv))*)
	val argv = CommandLine.arguments ()
	val _ = Logger.log_notice (Printer.$("Arguments to simEngine: " ^ (Util.l2s argv)))
	val _ = DynamoOptions.importCommandLineArgs argv

	val env = PopulatedEnv.new (rep_loop false)

	(* Save/restore the world. *)
	val _ = 
	    if not MLton.Profile.isOn then
		ignore (MLton.World.save (getSIMENGINESEW ()))
	    else ()

	(* set up a user log in /tmp *)
	val userLog = Logger.log_add (getSIMENGINELOG (), Logger.ALL, defaultOptions)

	(* Read the DOL files and the commmand line arguments *)
	val _ = processSettings()
	val _ = Profile.mark()

	(* Verify the license file *)
	val _ = CurrentLicense.findAndVerify()

	(* Update the logging based on the verbose flag which may have been passed in as a command line argument *)
	val log = if DynamoOptions.isFlagSet "verbose" then
		       Logger.log_stdout (Logger.ALL, defaultOptions)
		  else
		       Logger.log_stdout (Logger.WARNINGS, defaultOptions)

	(* initialize the exec *)
	val env = PopulatedEnv.importSettings (rep_loop false) env
	val _ = Exec.execInit()
	val _ = Profile.mark()

	(* Execute startup.dsl - this will execute the compiler and the simulator if requested, 
	 * or exit out to interactive mode below *)
	val env = Profile.time "simEngine" startup env
	val _ = Profile.mark()

	val _ = if DynamoOptions.isFlagSet "startupmessage" then
		    Logger.log_notice (Printer.$("Starting up simEngine ..."))
		else
		    ()

	val dir = OS.FileSys.fullPath (OS.Path.currentArc)

	local
	    fun isDefined setting =
		DynamoOptions.getStringSetting setting <> ""
	in
	fun nonInteractiveOption () =
	    isDefined "simex" orelse
	    isDefined "compile" orelse
	    isDefined "simulate" orelse
	    DynamoOptions.isFlagSet "help"
	end
	val batchFile = DynamoOptions.getStringSetting "batch"
	val _ = Profile.displayTimes()

    in
	if nonInteractiveOption () then
	    ((*Util.log "non interactive";*)
	    (* Noninteractive operating on a model definition. *)
	    (KEC.UNIT, env))
	else if batchFile <> "" then
	    ((*Util.log "batch mode ...";*)
	    if "-" <> batchFile orelse 0 < String.size batchFile andalso #"-" <> String.sub (batchFile, 0) then
		(* Noninteractive reading from a file. *)
		let
		    val filename = OS.FileSys.fullPath batchFile
			handle _ => (print ("Unable to locate " ^ batchFile ^ "\n")
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
	       (*; print (Globals.startupMessage() ^ "\n")*)
	       ; rep_loop false TextIO.stdIn env))
	else
	    (* Interactive reading from STDIN. *)
	    ((*Util.log("interactive ...");*)ParserSettings.setSettings (true, "STDIN", dir)
	   (*; print (Globals.startupMessage() ^ "\n")*)
	   ; rep_loop true TextIO.stdIn env)
      ; Logger.log_remove userLog
      ; Logger.log_remove log
      ; GeneralUtil.SUCCESS
    end
    handle DynException.RestartRepl => GeneralUtil.IGNORE
	 | DynException.SettingsError => GeneralUtil.USAGE (* needs to know how it is used, in matlab mode, it should return the matlab usage, otherwise we should just return the standard simEngine usage information *)
	 | Usage => GeneralUtil.USAGE
	 | DynException.InternalFailure => GeneralUtil.FAILURE NONE
	 | e => 
	   (DynException.log "Main" e
	  ; GeneralUtil.FAILURE (SOME "An exception was encountered"))



end

val _ =
    case (if MLton.Profile.isOn then
	      Main.main ()
	  else
	      (Main.resume () handle _ => Main.main ()))
     of GeneralUtil.SUCCESS => ()
      | GeneralUtil.USAGE => 
	if DynamoOptions.isFlagSet "inferiorMode" then
	    Posix.Process.exit 0w129
	else
	    Util.log ("usage: " ^ (CommandLine.name ()) ^ " -batch [file]\n" ^
		      "       " ^ (CommandLine.name ()) ^ " [option] -simex model\n" ^
		      "       " ^ (CommandLine.name ()) ^ " [option] -simex model [-start startime] -stop stoptime\n")
      (* normally will return exit status 0, but if there's a user error, 
       * it will return exit status 128, otherwise exit status 1 for a failure *)
      | GeneralUtil.IGNORE => (Posix.Process.exit 0w128) (* something had to happen to cause mlton to exit, 
							  * generally a user error *)
      | GeneralUtil.FAILURE NONE => (Posix.Process.exit 0w1) (* internal failure occurred, likely a call to
							      * std_failure from DSL *)
      | GeneralUtil.FAILURE (SOME message) => 
	(print (message ^ "\n")
       ; Posix.Process.exit 0w1)
	
