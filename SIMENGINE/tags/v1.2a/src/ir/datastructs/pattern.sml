structure Pattern =
struct

datatype patterncount = ONE
		      | ONE_OR_MORE
		      | ZERO_OR_MORE
		      | SPECIFIC_COUNT of int
		      | SPECIFIC_RANGE of (int * int)


end
