val fragment = SpilReader.readStdin () 
val layout = SpilPrinter.layout fragment
val _ = Layout.print (layout, TextIO.print)
val _ = TextIO.print "\n"
