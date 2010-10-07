structure StatusReporter =
struct

open Printer

val showStatus = ref true
val outstream = ref TextIO.stdOut
val barWidth = ref 35
val completedChar = ref #"="
val incompleteChar = ref #" "

val expected = ref 1
val actual = ref 0
val numchars = ref 0
val percentChars = ref 0
val start = ref 50

fun for low high dofun =
    if low >= high then
	()
    else
	(dofun();
	 for (low+1) high dofun)

fun wipeDisplay() =
    for 0 (!barWidth+2+(!percentChars)) (fn() => TextIO.output(!outstream, ("\b \b")))

fun wipePercent() =
    for 0 (!percentChars) (fn() => TextIO.output(!outstream, ("\b \b")))

fun percent() = 
    let
	val workdone = Real.fromInt (!actual)
	val workoverall = Real.fromInt (!expected)
	(*val percentStr = " " ^ (GeneralUtil.int2str (Real.toInt IEEEReal.TO_NEAREST (100.0 * (Real.fromInt v)/(Real.fromInt (!barWidth))))) ^ "%"*)
	val percentStr = " " ^ (GeneralUtil.int2str (if workoverall > 0.0 then Real.toInt IEEEReal.TO_NEAREST (100.0 * (workdone)/(workoverall)) else 100)) ^ "%"
    in
	(percentChars := (String.size percentStr);
	 percentStr)
    end

(*fun printBar() =
    (TextIO.output(!outstream, "[");
     for 0 (!numchars) (fn() => TextIO.output(!outstream, Char.toString (!completedChar)));
     for (!numchars) (!barWidth) (fn() => TextIO.output(!outstream, Char.toString (!incompleteChar)));
     TextIO.output(!outstream, "]");
     if (!numchars) = (!barWidth) then
	 TextIO.output (!outstream, " Done")
     else
	 TextIO.output(!outstream, percent (!numchars)))*)
fun printBar() =
     if (!numchars) = (!barWidth) then
	 TextIO.output (!outstream, "Done")
     else
	 (TextIO.output(!outstream, "[");
	  for 0 (!numchars) (fn() => TextIO.output(!outstream, Char.toString (!completedChar)));
	  for (!numchars) (!barWidth) (fn() => TextIO.output(!outstream, Char.toString (!incompleteChar)));
	  TextIO.output(!outstream, "]");
	  TextIO.output(!outstream, percent()))

fun printPercent() = 
    if (!numchars) = (!barWidth) then
	TextIO.output (!outstream, "Done")
    else
	TextIO.output (!outstream, percent())

fun updateDisplay () =
    let
	val workdone = Real.fromInt (!actual)
	val workoverall = Real.fromInt (!expected)
	val width = Real.fromInt (!barWidth)
	val progress = if workoverall * width <= 0.0 then 
			   100
		       else
			   Real.floor(workdone / workoverall * width)
    in
	if progress > (!numchars) then
	    (numchars := progress;
	     wipeDisplay();
	     printBar();
	     TextIO.flushOut(TextIO.stdOut))
	else (* even if you don't update the progress bar, update the percent which should change more often *)
	    (wipePercent();
	     printPercent();
             TextIO.flushOut(TextIO.stdOut))
    end
(* TODO - Remove this function, not used *)
fun showHeader (inputfiles, registryfile) =
    (TextIO.output(!outstream, "\n\nBeginning Dynamo Compilation\n");
     TextIO.output(!outstream, "  Settings file: " ^ registryfile ^ "\n");
     TextIO.output(!outstream, "  Input files: " ^ (String.concatWith ", " inputfiles) ^ "\n");
     TextIO.output(!outstream, "  Output directory: " ^ (StdFun.name2work_dir (StdFun.hd inputfiles))))

(*fun beginPhase (phase) =
    (TextIO.output(!outstream, "\n===== " ^ phase ^ " =====\n");
     Logger.log_notice ("starting phase " ^ phase))*)
fun beginPhase (phase) =
    (TextIO.output(!outstream, "\n\n-+ " ^ phase);
     Logger.log_notice ($("Starting phase '" ^ phase ^ "'")))
    
    
(*fun beginProcess (processname, workexpected) =
    (TextIO.output(!outstream, "\n== " ^ processname ^ " ==\n");
     expected := workexpected;
     actual := 0;
     numchars := 0;
     updateDisplay();
     Logger.log_notice ("starting process " ^ processname))*)
fun beginProcess (processname, workexpected) =
    let
	val displayStr = "\n |--> " ^ processname ^ " "
	val extraChars = !start - (String.size displayStr) - 1
    in
	(TextIO.output(!outstream, displayStr);
	 expected := workexpected;
	 actual := 0;
	 for 0 extraChars (fn() => TextIO.output(!outstream, "."));
	 TextIO.output(!outstream, " ");
	 numchars := 0;
	 printBar();
	 (*updateDisplay();*)
	 Logger.log_notice ($("Starting process '" ^ processname ^ "'")))
    end
    
fun reportWork (workPerformed) =
    (actual := !actual + workPerformed;
     updateDisplay())

fun map processname f list =
    (beginProcess(processname, length list);
     List.map (fn(x) => (let 
			     val v = f(x)
			     val _ = reportWork 1
			 in
			     v
			 end)) list)

(* mapticks will run a map over some set of data but report work a set number of times *)
(* this is useful when a process has multiple map commands that take a considerable amount of time *)
(* by selectively adding more ticks to different map operations, the status bar can be made to move *)
(* at a fairly constant rate *)
fun mapticks ticks f list = 
    let
	val ticker = ref 1.0
	val len = List.length list
	val step = (Real.fromInt len)/(Real.fromInt ticks)
	val result = List.map (fn(item, n)=>
				 (if ((!ticker * step) < (Real.fromInt n)) then
				      (ticker := !ticker + 1.0;
				       reportWork 1)
				  else
				      ();
				  f item))
			 (StdFun.addCount list)
	(* make sure there are the right amount of ticks *)
	val _ = reportWork ((ticks+1) - (Real.floor (!ticker)))
    in
	result
    end

fun app processname f list =
    (beginProcess(processname, length list);
     List.app (fn(x) => (let 
			     val v = f(x)
			     val _ = reportWork 1
			 in
			     v
			 end)) list)

fun foldl processname f init list =
    (beginProcess(processname, length list);
     List.foldl (fn(x) => (let 
			       val v = f(x)
			       val _ = reportWork 1
			   in
			       v
			   end)) init list)

fun foldr processname f init list =
    (beginProcess(processname, length list);
     List.foldr (fn(x) => (let 
			       val v = f(x)
			       val _ = reportWork 1
			   in
			       v
			   end)) init list)
end

