signature JSON_PARSE_STRUCTS = sig
    structure JS: JSON

    structure Token: sig
	datatype token = EOF | COMMA | COLON
		       | NULL | TRUE | FALSE
		       | LARRAY | RARRAY 
		       | LOBJECT | ROBJECT
		       | INT of IntInf.int
		       | REAL of LargeReal.real
		       | STRING of string
	val toString: token -> string
    end

    structure Lex: sig
	val makeLexer: (int -> string) -> (unit -> Token.token)
    end
end

signature JSON_PARSE = sig
    include JSON_PARSE_STRUCTS

    val parse: TextIO.instream -> JS.json
    val parseFile: string -> JS.json
    val parseString: string -> JS.json
end
