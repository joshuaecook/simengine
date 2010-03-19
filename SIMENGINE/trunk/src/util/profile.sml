signature PROFILE =
sig

    val time: string -> ('a -> 'b) -> 'a -> 'b
    val timeTwoCurryArgs : string -> ('a -> 'b -> 'c) -> 'a -> 'b -> 'c

end
structure Profile: PROFILE =
struct

val stack = ref 0

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
	handle e => DynException.checkpoint "Profile.time" e
    else
	fcn arg

fun timeTwoCurryArgs message fcn arg1 arg2 = 
    let
	fun fcn' (arg1, arg2) = time message (fcn arg1) arg2
	fun fcn'' arg1 arg2 = fcn' (arg1, arg2)
    in
	fcn'' arg1 arg2
    end

end
