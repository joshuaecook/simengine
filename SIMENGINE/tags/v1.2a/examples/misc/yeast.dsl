// Anaerobic Fermentation Pathway in Saccharomyces Cerevisiae (Yeast)
// Voit, E., Computational Analyses of Biochemical Systems, 2000, pp. 277
//
// Adapted for use with simEngine
// Copyright 2009 Simatra Modeling Technologies, L.L.C.
model (X) = yeast

state X1 = .0345209
state X2 = 1.01408
state X3 = 9.182335
state X4 = 0.00955136
state X5 = 1.1278
state X6 = 19.7
state X7 = 68.5
state X8 = 31.7
state X9 = 49.9
state X10 = 3440
state X11 = 14.31
state X12 = 203
state X13 = 25.1
state X14 = 0.042

equations
	X1' = 0.8122*X2^(-0.2344)*X6 - 2.8632*X1^0.7464*X5^0.0243*X7
	X2' = 2.8632*X1^0.7464*X5^0.0243*X7 - 0.5232*X2^0.7318*X5^(-0.3941)*X8 - 0.0009*X2^8.6107*X11
	X3' = 0.5232*X2^0.7318*X5^(-0.3941)*X8 - 0.011*X3^0.6159*X5^0.1308*X9*X14^(-0.6088) - 0.04725*X3^0.05*X4^0.533*X5^(-0.0822)*X12
	X4' = 0.022*X3^0.6159*X5^0.1308*X9*X14^(-0.6088) - 0.0945*X3^0.05*X4^0.533*X5^(-0.0822)*X10
	X5' = 0.022*X3^0.6159*X5^0.1308*X9*X14^(-0.6088) + 0.0945*X3^0.05*X4^0.533*X5^(-0.0822)*X10 - 2.8632*X1^0.7464*X5^0.0243*X7 - 0.0009*X2^8.6107*X11 - 0.5232*X2^0.7318*X5^(-0.3941)*X8 - X5*X13 
end

output X = (X1, X2, X3, X4, X5)

solver=forwardeuler{dt=0.001}

end
