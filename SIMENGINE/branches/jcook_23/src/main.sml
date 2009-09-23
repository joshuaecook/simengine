(* exception FAIL *)
(* fun handler _ = *)
(*     (print "hello world\n"; *)
(*      TextIO.flushOut(TextIO.stdOut); *)
(*      raise FAIL) *)

(* MLton.Signal.setHandler(Posix.Signal.int, handler) *)

(* WARNING: If you do not know what you are doing, DO NOT REMOVE OR CHANGE THIS LINE *)
(*val _ = ModelCompileLauncher.compile_hook := ModelCompile.compile*)


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
	    val (env', kec') = Exec.run (rep_loop false) env kec

	    val _ = if isInteractive then 
			ShellPrint.showResult exec_exp kec'
		    else
			()
	in
	    rep_loop isInteractive textstream env'
	end
	handle err as OOLCParse.ParserError
	       => (Logger.log_error (Printer.$"Parse errors encountered");
		   DynException.log "Main" err;	     
		   if DynamoOptions.isFlagSet("debugexceptions") then
		       app print (log_stack err ())
		   else
		       ();
		   rep_loop isInteractive textstream env)
	     | DynException.RestartRepl
	       => if isInteractive then (DynException.resetErrored(); rep_loop isInteractive textstream env) else (KEC.UNIT, env)
	     | e as DynException.TooManyErrors =>
	       if isInteractive then
		   (DynException.resetErrored();
		    rep_loop isInteractive textstream env)
	       else
		   raise e
		  
	
exception Usage

val defaultOptions = [Logger.LIBRARY]

val _ = Logger.log_stdout (Logger.WARNINGS, 
			   defaultOptions(* @ (DynamoParameters.options2groups options)*))
	
			    
	
val registryfile = case OS.Process.getEnv("SIMENGINEDOL") of
		       SOME reg => reg
		     | NONE => (case OS.Process.getEnv("SIMENGINE") of
				    SOME reg => reg ^ "/data/default.dol"
				  | NONE => (print ("\nError: Environment variable SIMENGINEDOL or SIMENGINE must be set to determine the location of the Diesel options file (dol)\n");
					     DynException.setErrored();
					     ""))
val _ = DynException.checkToProceed()
	
	
(* read in registry settings - this will overwrite the command line args imported above *)
val _ = DynamoOptions.importRegistryFile (registryfile)
	before DynException.checkToProceed()
	

	
val logfiledir = "/tmp" (*TODO: change this*)
val username = case OS.Process.getEnv("USER") of
		   SOME user => user
		 | NONE => ""
val logfilename = logfiledir ^ "/simEngine-" ^ username ^ ".log"
val log_id = Logger.log_add (logfilename,
			     Logger.ALL, 
			     defaultOptions(* @ (DynamoParameters.options2groups options)*))

val _ = (if List.length (CommandLine.arguments()) = 0 then
	     let
		 val env = PopulatedEnv.new (rep_loop false)
		 val _ = ParserSettings.setSettings (true, "STDIN", ".")
		 val _ = print (Globals.startupMessage ^ "\n")
	     in
		 rep_loop true TextIO.stdIn env
	     end
	 else
	     if List.length (CommandLine.arguments()) = 1 then
		 if hd(CommandLine.arguments()) = "-batch" then
		     let
			 val env = PopulatedEnv.new (rep_loop false)
			 val _ = ParserSettings.setSettings (true, "STDIN", ".")
			 val _ = print (Globals.startupMessage ^ "\n")
		     in
			 rep_loop false TextIO.stdIn env
		     end
		 else
		     let
			 val filename = (hd (CommandLine.arguments()))
			 val file = TextIO.openIn filename
			 val (name, path) = GeneralUtil.filepath_split filename
			 val env = PopulatedEnv.new (rep_loop false)
			 val _ = ParserSettings.setSettings (false, name, path)
		     in
			 rep_loop false file env
		     end
	     else
		 (print ("Usage: " ^ CommandLine.name() ^ " [optional filename]\n\n");
		  raise Usage);
	GeneralUtil.SUCCESS)
	before (Logger.log_remove log_id)
    handle DynException.TooManyErrors => 
	   GeneralUtil.FAILURE "Too many errors encountered"
	 | Usage => GeneralUtil.SUCCESS
	 |  e => 
	    (DynException.log "Main" e;
	     GeneralUtil.FAILURE "An exception was encountered")

