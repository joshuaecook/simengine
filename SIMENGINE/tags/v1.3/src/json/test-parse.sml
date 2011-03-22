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

signature TEST_PARSE_JSON_STRUCTS = sig
structure JS: JSON
structure Parse: JSON_PARSE
sharing JS = Parse.JS
end

functor TestParseJSON(S: TEST_PARSE_JSON_STRUCTS) = struct
open S
open Test
structure P = Parse
structure JS = JS

val value = P.parseFile "build-options.json"

val _ = if JS.isObject value then pass ()
	else bug ("Expected value to be a JSON object")

val _ = case JS.member (value, "buildRevision")
	 of SOME rev => 
	    (case JS.toInt rev
	      of SOME 769 => pass ()
	       | SOME z => bug ("Expected buildRevision to be 769 but got " ^ (IntInf.toString z))
	       | NONE => bug ("Expected buildRevision to be a string"))
	  | NONE => bug ("Expected build options to have a buildRevision")

val _ = case JS.member (value, "version")
	 of SOME ver =>
	    (case JS.toString ver
	      of SOME "0.92" => pass ()
	       | SOME s => bug ("Expected version to be 0.92 but got " ^ s)
	       | NONE => bug ("Expected version to be a string"))
	  | NONE => bug ("Expected build options to have a version")

val _ = case JS.member (value, "backends")
	 of SOME bes =>
	    (case JS.elements bes
	      of SOME nil => bug ("Expected backends to be nonempty")
	       | SOME elems =>
		 (case (List.length elems, JS.toString (List.hd elems))
		   of (3, SOME "software") => pass ()
		    | _ => bug ("Unexpected value in backends"))
	       | NONE => bug ("Expected backends to be an array"))
	  | NONE => bug ("Expected build options to have a list of backends")


val _ = case JS.toBool (P.parseString "true")
	 of SOME true => pass ()
	  | SOME _ => bug ("Expected parsed string to be true")
	  | NONE => bug ("Expected parse string to be a boolean")

val _ = case JS.elements (P.parseString "[]")
	 of SOME nil => pass ()
	  | SOME _ => bug ("Expected parsed string to be an empty list")
	  | NONE => bug ("Expected parsing string to succeed")

structure Print = PrintJSON(structure JS = JS)
val _ = Print.print (TextIO.stdOut, value)

end

