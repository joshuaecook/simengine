structure SpilContext = ParserContext();

structure SpilGrammar = MakeSpilGrammar(structure Token = LrParser.Token structure Syntax = Spil structure Context = SpilContext)

structure SpilLexer = MakeSpilLexer(structure Tokens = SpilGrammar.Tokens structure Syntax = Spil structure Context = SpilContext)

structure SpilParser = JoinWithArg(structure ParserData = SpilGrammar.ParserData structure Lex = SpilLexer structure LrParser = LrParser)

structure SpilReader = MakeSpilReader(structure Syntax = Spil structure Context = SpilContext structure Parser = SpilParser structure Tokens = SpilGrammar.Tokens)

structure SpilPrinter = MakeSpilPrinter(structure Syntax = Spil)
