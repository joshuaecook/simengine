signature PROFILE =
sig

    val time: string -> ('a -> 'b) -> 'a -> 'b
    val timeTwoCurryArgs : string -> ('a -> 'b -> 'c) -> 'a -> 'b -> 'c
    val mark: unit -> unit
    val displayTimes: unit -> unit

    (* write a status to the display *)
    val write_status: string -> unit
    val map: string -> ('a -> 'b) -> 'a list -> 'b list
    val app: string -> ('a -> unit) -> 'a list -> unit
    val foldl: string -> ('a * 'b -> 'b) -> 'b -> 'a list -> 'b
    val foldr: string -> ('a * 'b -> 'b) -> 'b -> 'a list -> 'b

    val clearProgressFile : unit -> unit
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
val characters_written = ref 0 
fun clearProgressFile() = (progressFileStream := NONE;
			   characters_written := 0)
fun write_spaces (fid, num) =
    let
	val str = String.implode (List.tabulate (num, fn(x) => #" "))
	val pos = TextIO.getPosOut fid
	val _ = TextIO.output (fid, str)
	val _ = TextIO.flushOut fid
	val _ = TextIO.setPosOut (fid, pos)
    in
	()
    end

fun write_progress_file txt =
    let
	(* should be already created at this time by simex *)
	val dir = DynamoOptions.getStringSetting("outputdir")

	val _ = if Directory.isDir dir then
		    case (!progressFileStream) of
			SOME s => 
			let
			    val _ = write_spaces (s, !characters_written)
			    val pos = TextIO.getPosOut s
			    val _ = TextIO.output (s, txt)
			    (*val _ = Util.log ("Writing Status '"^(txt)^"'")*)
			    val _ = characters_written := (String.size txt)
			    val _ = TextIO.flushOut s
			    val _ = TextIO.setPosOut (s, pos)
			in
			    ()
			end
		      | NONE => (progressFileStream := SOME (TextIO.openOut (dir ^ "/compilation_progress"));
				 write_progress_file txt)
		else 
		    () (* don't worry about it if there is no output directory - probably running in batch mode *)
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
					       | _ => Logger.log_warning (Printer.$("Encountered unknown exception"));
					     clearProgressFile())
	 | e => DynException.checkpoint "Profile.write_progress_file" e

(* print out a percentage *)
fun write_percentage txt r = 
    write_progress_file (txt ^ ": " ^ (Util.i2s (Real.floor(r * 100.0))) ^ "%")

(* write out a status message *)
val write_status = write_progress_file

(* override list operations *)
fun profileMap txt fcn l = 
    let
	val status = write_percentage txt
	val num = Real.fromInt (List.length l)
	fun toPercent i = 
	    Real.fromInt i / num
	fun fcn' (x, i) = (status (toPercent i);
			   fcn x)
    in
	map fcn' (Util.addCount l)
    end
fun profileApp txt fcn l = 
    let
	val status = write_percentage txt
	val num = Real.fromInt (List.length l)
	fun toPercent i = 
	    Real.fromInt i / num
	fun fcn' (x, i) = (status (toPercent i);
			   fcn x)
    in
	app fcn' (Util.addCount l)
    end
fun profileFoldl txt fcn init l = 
    let
	val status = write_percentage txt
	val num = Real.fromInt (List.length l)
	fun toPercent i = 
	    Real.fromInt i / num
	fun fcn' ((a, i), b) = (status (toPercent i);
				fcn (a, b))
    in
	foldl fcn' init (Util.addCount l)
    end
fun profileFoldr txt fcn init l = 
    let
	val status = write_percentage txt
	val num = Real.fromInt (List.length l)
	fun toPercent i = 
	    Real.fromInt i / num
	fun fcn' ((a, i), b) = (status (toPercent i);
				fcn (a, b))
    in
	foldr fcn' init (Util.addCount l)
    end

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
						       write_percentage "Compiling" expTime;
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

(* always keep these on the bottom of this file so it doesn't interfere with the rest of the functions *)
val map = profileMap
val app = profileApp
val foldl = profileFoldl
val foldr = profileFoldr

end
