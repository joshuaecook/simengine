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
				    SOME path => OS.Path.concat (path, OS.Path.fromUnixPath "data/default.dol")
				  | NONE => (print ("\nError: Environment variable SIMENGINEDOL or SIMENGINE must be set to determine the location of the Diesel options file (dol)\n");
					     DynException.setErrored();
					     ""))
val _ = DynException.checkToProceed()

local
    structure W = MLton.World
in
val worldfile = case (OS.Process.getEnv("SIMENGINESEW"), OS.Process.getEnv("SIMENGINE"))
		 of (SOME world, _) => world
		  | (NONE, SOME path) => OS.Path.concat (path, OS.Path.fromUnixPath "data/default.sew")
		  | (NONE, NONE) => (print ("\nError: Environment variable SIMENGINESEW or SIMENGINE must be set to determine the location of the simEngine world file (sew)\n");
				     DynException.setErrored();
				     "")
val _ = DynException.checkToProceed()
(* Attempts to restore the world from a previously-saved file.
 * It's ok to fail; that means the world hasn't existed before so we just continue building it and save it later. 
 * If this succeeds, the world will be replaced by the one being restored, jumping to the state of execution in which it was saved.
 *)
val _ = (ignore (W.load worldfile)
	 handle Fail _ => ())
end

	
	
(* read in registry settings - this will overwrite the command line args imported above *)
val _ = DynamoOptions.importRegistryFile (registryfile)
	before DynException.checkToProceed()


	

local
structure W = MLton.World
fun main () =
    let val logfile = OS.Path.joinDirFile {dir = OS.Path.fromUnixPath "/tmp",
					   file = case OS.Process.getEnv "USER"
						   of SOME user => "simEngine-" ^ user ^ ".log"
						    | NONE => "simEngine.log"}
	val log_id = Logger.log_add (logfile, Logger.ALL, defaultOptions)
	val env = PopulatedEnv.new (rep_loop false)
		  
	(* Saves/restores the world. 
	 * Open file descriptors are not retained; reopens the log file in the restored world.
	 * TODO should the old log file be closed before saving?
	 *)
	val log_id = case W.save worldfile
		      of W.Original => log_id
		       | W.Clone => 
			 let val logfile = OS.Path.joinDirFile {dir = OS.Path.fromUnixPath "/tmp",
								file = case OS.Process.getEnv "USER"
									of SOME user => "simEngine-" ^ user ^ ".log"
									 | NONE => "simEngine.log"}
			 in 
			     Logger.log_add (logfile, Logger.ALL, defaultOptions)
			 end
    in
	(case CommandLine.arguments ()
	 of nil =>
	    let val _ = ParserSettings.setSettings (true, "STDIN", ".")
		val _ = print (Globals.startupMessage ^ "\n")
	    in
		rep_loop true TextIO.stdIn env
	    end

	  | ["-batch"] => 
	    let val _ = ParserSettings.setSettings (true, "STDIN", ".")
	    in
		rep_loop false TextIO.stdIn env
	    end

	  | [filename] => 
	    let val input = TextIO.openIn filename
		val {file, dir} = OS.Path.splitDirFile filename
		val _ = ParserSettings.setSettings (false, file, dir)
	    in
		rep_loop false input env
	    end

	  | _ => 
	    (print ("Usage: " ^ CommandLine.name() ^ " [optional filename]\n\n");
	     raise Usage);

	 Logger.log_remove log_id;
	 GeneralUtil.SUCCESS)
    end
in

val _ = main ()
handle DynException.TooManyErrors => GeneralUtil.FAILURE "Too many errors encountered"
     | Usage => GeneralUtil.SUCCESS
     | e => (DynException.log "Main" e; GeneralUtil.FAILURE "An exception was encountered")
	    
end


	     (*

val _ = (if List.length (CommandLine.arguments()) = 0 then
	     let
		 val env = PopulatedEnv.new (rep_loop false)
		 val _ = ParserSettings.setSettings (true, "STDIN", ".")
		 val _ = print (Globals.startupMessage ^ "\n")
			 
		 val _ = case W.save worldfile
			  of W.Original => ()
			   | W.Clone => print "Need to reopen log file"
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

	     *)
