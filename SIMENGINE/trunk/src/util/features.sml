(*
Copyright (C) 2011 by Simatra Modeling Technologies

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.

*)

signature FEATURES =
sig

    datatype feature = NUM_STATES of int
		     | NUM_ITERATORS of int
		     | GPU
		     | MULTI_CORE
		     | OPTIMIZATION of int
		     | BACKWARD_EULER
		     | EXPONENTIAL_EULER

    (* tests for individual features *)
    val isEnabled : feature -> bool
    val verifyEnabled : feature -> unit
    val defaultTarget : unit -> string

end
structure Features : FEATURES =
struct

datatype feature = NUM_STATES of int
		 | NUM_ITERATORS of int
		 | GPU
		 | MULTI_CORE
		 | OPTIMIZATION of int
		 | BACKWARD_EULER
		 | EXPONENTIAL_EULER

(* Magic numbers *)
val MAX_NUMBER_OF_STATES_IN_BASIC_MODE = 100
val MAX_NUMBER_OF_ITERATORS_IN_BASIC_MODE = 1

(* toString method for feature *)
val i2s = Util.i2s
fun toString (NUM_STATES i) = 
    if CurrentLicense.isBasic() then
	"Unlimited States (maximum="^(i2s MAX_NUMBER_OF_STATES_IN_BASIC_MODE)^")"
    else
	"Unlimited States"
  | toString (NUM_ITERATORS i) = 
    if CurrentLicense.isBasic() then
	"Multiple Iterators (maximum="^(i2s MAX_NUMBER_OF_ITERATORS_IN_BASIC_MODE)^")"
    else
	"Multiple Iterators"
  | toString (GPU) = "GPU"
  | toString (MULTI_CORE) = "MultiCore"
  | toString (OPTIMIZATION i) = if CurrentLicense.isBasic() then
				    "Optimization (Max Level=1)"
				else if CurrentLicense.isStandard() then
				    "Optimization (Max Level=2)"
				else
				    "Full Optimization"
  | toString (BACKWARD_EULER) = "SpecializedSolvers"
  | toString (EXPONENTIAL_EULER) = "SpecializedSolvers"

(* check to see if a feature is enabled *)
fun isEnabled feature = 
    case feature of
	NUM_STATES requested => if CurrentLicense.isBasic() then
				    requested <= MAX_NUMBER_OF_STATES_IN_BASIC_MODE
				else
				    true
      | NUM_ITERATORS requested => if CurrentLicense.isBasic() then
				       requested = MAX_NUMBER_OF_ITERATORS_IN_BASIC_MODE
				   else
				       true
      | GPU => CurrentLicense.isTrial() orelse CurrentLicense.isProfessional() orelse CurrentLicense.isDevelopment()
      | MULTI_CORE => CurrentLicense.isTrial() orelse CurrentLicense.isProfessional() orelse CurrentLicense.isDevelopment()
      | OPTIMIZATION requested => if CurrentLicense.isBasic() then
				      requested <= 1
				  else if CurrentLicense.isStandard() then
				      requested <= 2
				  else
				      true
      | BACKWARD_EULER => not (CurrentLicense.isBasic())
      | EXPONENTIAL_EULER => not (CurrentLicense.isBasic())

(* return an error if the feature is not available *)
fun verifyEnabled feature =
    if isEnabled feature then
	()
    else
	(Logger.log_error(Printer.$("Feature '"^(toString feature)^"' is not available under a '"^(CurrentLicense.versionToString())^"' license"));
	 DynException.setErrored())

fun defaultTarget () = 
    if CurrentLicense.isTrial() orelse CurrentLicense.isProfessional() orelse CurrentLicense.isDevelopment() then
	"parallelcpu"
    else
	"cpu"

end
