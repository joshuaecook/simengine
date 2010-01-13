(* Copyright (C) 2010 by Simatra Modeling Technologies, L.L.C. *)

(* The default structures for parsing and printing JSON data. *)
structure ParseJSON = ParseJSON(structure JS = JSON structure Token = JSONToken structure Lex = LexJSON)
structure PrintJSON = PrintJSON(structure JS = JSON)


