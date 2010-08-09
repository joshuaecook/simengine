structure AllTests = struct

structure T = struct
    structure Test = Test
    structure JS = JSON
    structure Token = JSONToken
    structure Lex = LexJSON
end

structure Parse =
ParseJSON(open T)

structure TestParseJSON = 
TestParseJSON(open T structure Parse = Parse)


end
