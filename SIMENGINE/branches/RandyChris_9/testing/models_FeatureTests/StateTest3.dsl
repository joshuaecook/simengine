model (sum,diff) = StateTest3

   constant A = 1
   constant B = 2
   constant C = 3

   // newlines are ignored after operators
   equation sum = A +
                  B + // comments may appear here
                  C

   // newlines are ignored within parentheses and other symmetric delimiters
   equation diff = (A
                 -  B // comments my appear here also
                 -  C)
end
