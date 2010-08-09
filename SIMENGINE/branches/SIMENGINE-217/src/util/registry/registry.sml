structure Registry =
struct

datatype value = REG_NUMBER of real
	       | REG_STRING of string
	       | REG_NUMBER_VECTOR of real list

datatype entry = REG_FLAG of string * bool
	       | REG_SETTING of string * value

end
