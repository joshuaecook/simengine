signature PROFILE =
sig

    val time: string -> ('a -> 'b) -> 'a -> 'b
    val timeTwoCurryArgs : string -> ('a -> 'b -> 'c) -> 'a -> 'b -> 'c
    val mark: unit -> unit
    val displayTimes: unit -> unit

end
structure Profile: PROFILE =
struct

val stack = ref 0
val genTimings = ref NONE
val timingData = ref NONE
val markTimer = ref (Timer.startRealTimer())
val baseTime = ref 0.0

fun generateSpaces stackcount =
    String.concat (List.tabulate (stackcount, fn(x)=>"|-> "))

fun time message fcn arg =
    if DynamoOptions.isFlagSet "profile" then
	let
	    val spaces = generateSpaces (!stack)
	    val _ = Util.log (spaces ^ "Beginning " ^ message)
	    val _ = stack := (!stack + 1)
	    val cputime = Timer.startCPUTimer()
	    val realtime = Timer.startRealTimer()
	    val t1 = Time.now()
	    val result = fcn arg
	    val {gc={sys=gc_sys, usr=gc_usr}, nongc={sys=nongc_sys,usr=nongc_usr}} = Timer.checkCPUTimes cputime
	    val tdiff = Time.toReal (Timer.checkRealTimer realtime)
	    val _ = Util.log(spaces ^ ">> " ^ message ^ ": " ^ (GeneralUtil.real2str tdiff) ^ " s")
	    val _ = stack := (!stack - 1)
	in
	    result
	end
	(*handle e => DynException.checkpoint "Profile.time" e*)
    else
	fcn arg

fun timeTwoCurryArgs message fcn arg1 arg2 = 
    let
	fun fcn' (arg1, arg2) = time message (fcn arg1) arg2
	fun fcn'' arg1 arg2 = fcn' (arg1, arg2)
    in
	fcn'' arg1 arg2
    end

(* store the current progress file stream *)
val progressFileStream = ref NONE
fun write_progress_file r =
    let
	val dir = DynamoOptions.getStringSetting("outputdir")
	val _ = if Directory.isDir dir then 
		    case (!progressFileStream) of
			SOME s => 
			let
			    val pos = TextIO.getPosOut s
			    val _ = TextIO.output (s, ("Compiling: " ^ (Util.i2s (Real.floor(r * 100.0))) ^ "%"))
			    val _ = TextIO.setPosOut (s, pos)
			in
			    ()
			end
		      | NONE => (progressFileStream := SOME (TextIO.openOut (dir ^ "/compilation_progress"));
				 write_progress_file r)
		else
		    () (* can't do anything until the directory is created *)
    in
	()
    end
    handle IO.Io {name, function, cause} => (Logger.log_warning (Printer.$("Returned an IO error on '"^name^"' with function '"^function^"'"));
					     case cause of
						 OS.SysErr (message, SOME syserror) => Logger.log_warning (Printer.$("Encountered SysErr exception with message '"^(message)^"', error name '"^(OS.errorName syserror)^"'"))
					       | OS.SysErr (message, NONE) => Logger.log_warning (Printer.$("Encountered SysErr exception with message '"^(message)^"'"))
					       | IO.BlockingNotSupported => Logger.log_warning (Printer.$("Encountered BlockingNotSupported exception"))
					       | IO.NonblockingNotSupported => Logger.log_warning (Printer.$("Encountered NonblockingNotSupported exception"))
					       (*| IO.TerminatedStream => Logger.log_warning (Printer.$("Encountered TerminatedStream exception"))*)
					       | IO.ClosedStream => Logger.log_warning (Printer.$("Encountered ClosedStream exception"))
					       | Subscript => Logger.log_warning (Printer.$("Encountered SubScript exception"))
					       | _ => Logger.log_warning (Printer.$("Encountered unknown exception")))
	 | e => DynException.checkpoint "Profile.write_progress_file" e

fun mark () =
    case (!genTimings, !timingData) of
	(NONE, NONE) => 
	let
	    val gen = DynamoOptions.isFlagSet("regenerateTimings")
	in
	    (genTimings := SOME gen;
	     timingData := SOME (if gen then
				     (markTimer := (Timer.startRealTimer());
				      [])
				 else
				     DynamoOptions.getRealVectorSetting("compilerTimingData")))
	end
      | (SOME gen, SOME data) => (if gen then
				      timingData := SOME ((Time.toReal (Timer.checkRealTimer (!markTimer)))::data)
				  else
				      let
					  val curTime = Time.toReal (Timer.checkRealTimer (!markTimer))
					  val _ = case data of
						      expTime::restTime => 
						      ((*Util.log ("Percent done: " ^ (Real.toString (100.0*expTime)) ^ "%");*)
						       write_progress_file expTime;
						       timingData := SOME restTime)
						    | nil => Logger.log_warning (Printer.$("Unexpected shortage of timing samples.  Rerun the simulation with the -regenerateTimings option and save the resulting value to your dol file."))
				      in
					  ()
				      end)
      | _ => DynException.stdException("Unexpected internal state for genTimings and timingData", "Profile.mark", Logger.INTERNAL)

local
    fun maxData data = 
	foldl (fn(a,b)=>if a > b then a else b) (~1.0) data
    fun normalizeData data =
	let val max = maxData data
	in map (fn(d)=> d/max) data	    
	end
in
fun displayTimes () =
    case (!genTimings, !timingData) of
	(SOME true, SOME data) => Util.log("<compilerTimingData = "^(Util.list2str Util.r2s (List.rev (normalizeData data)))^">")
      | _ => ()
end

end
