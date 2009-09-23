structure TestUtils =
struct

exception InternalAssertionFailure of string

fun assert (cond, msg) =
    if cond then
	()
    else
	raise InternalAssertionFailure msg

end
