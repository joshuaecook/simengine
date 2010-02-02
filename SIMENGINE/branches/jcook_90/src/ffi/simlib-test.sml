(* Copyright (C) 2010 by Simatra Modeling Technologies, L.L.C. *)

functor SimlibTest (S: SIMLIB) = struct
val pass = ignore
fun bug why = raise Fail why

val _ = case S.makeObjectFromFile {filename = "simlib-test.sml",
				   objectName = "simlib-test.sml"}
	 of "simlib_test_sml.o" => pass ()
	  | s => bug ("Expected object filename to be simlib_test_sml.o but got " ^ s)


val lipsum = "Lorum ipsum dolor sit amet."
val _ = case S.makeObjectFromContents {objectName = "lipsum.txt",
				       data = lipsum}
	 of "lipsum_txt.o" => pass ()
	  | s => bug ("Expected object filename to be lipsum_txt.o but got " ^ s)

val _ = case S.getContentsFromArchive {archive = "test.sim",
				       objectName = "lipsum.txt"}
	 of s => if lipsum = s then pass ()
		 else bug ("Expected contents to be \"" ^ lipsum ^ "\" but got \"" ^ s ^ "\"")

val _ = S.getFileFromArchive {archive = "test.sim",
			      objectName = "simlib-test.sml",
			      filename = "my-simlib-test.sml"}

end

structure Test = SimlibTest(Simlib)
