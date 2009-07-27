import "stg.dsl"

model (Vm) = stgpop
//  input ave_I with {default=2}

  submodel stg stg0 
  submodel stg stg1 
  submodel stg stg2 
  submodel stg stg3 
  submodel stg stg4 
  submodel stg stg5 
  submodel stg stg6 
  submodel stg stg7 
  submodel stg stg8 
  submodel stg stg9 


  submodel stg stg10 
  submodel stg stg11 
  submodel stg stg12 
  submodel stg stg13 
  submodel stg stg14 
  submodel stg stg15 
  submodel stg stg16 
  submodel stg stg17 
  submodel stg stg18 
  submodel stg stg19 


  submodel stg stg20 
  submodel stg stg21 
  submodel stg stg22 
  submodel stg stg23 
  submodel stg stg24 
  submodel stg stg25 
  submodel stg stg26 
  submodel stg stg27 
  submodel stg stg28 
  submodel stg stg29 

  submodel stg stg30 
  submodel stg stg31 
  submodel stg stg32 
  submodel stg stg33 
  submodel stg stg34 
  submodel stg stg35 
  submodel stg stg36 
  submodel stg stg37 
  submodel stg stg38 
  submodel stg stg39 


  submodel stg stg40 
  submodel stg stg41 
  submodel stg stg42 
  submodel stg stg43 
  submodel stg stg44 
  submodel stg stg45 
  submodel stg stg46 
  submodel stg stg47 
  submodel stg stg48 
  submodel stg stg49 


  submodel stg stga0 
  submodel stg stga1 
  submodel stg stga2 
  submodel stg stga3 
  submodel stg stga4 
  submodel stg stga5 
  submodel stg stga6 
  submodel stg stga7 
  submodel stg stga8 
  submodel stg stga9 


  submodel stg stga10 
  submodel stg stga11 
  submodel stg stga12 
  submodel stg stga13 
  submodel stg stga14 
  submodel stg stga15 
  submodel stg stga16 
  submodel stg stga17 
  submodel stg stga18 
  submodel stg stga19 


  submodel stg stga20 
  submodel stg stga21 
  submodel stg stga22 
  submodel stg stga23 
  submodel stg stga24 
  submodel stg stga25 
  submodel stg stga26 
  submodel stg stga27 
  submodel stg stga28 
  submodel stg stga29 

  submodel stg stga30 
  submodel stg stga31 
  submodel stg stga32 
  submodel stg stga33 
  submodel stg stga34 
  submodel stg stga35 
  submodel stg stga36 
  submodel stg stga37 
  submodel stg stga38 
  submodel stg stga39 


  submodel stg stga40 
  submodel stg stga41 
  submodel stg stga42 
  submodel stg stga43 
  submodel stg stga44 
  submodel stg stga45 
  submodel stg stga46 
  submodel stg stga47 
  submodel stg stga48 
  submodel stg stga49 



//  output u = (fn1.u, fn2.u, fn3.u)
//  output w = (fn1.w, fn2.w, fn3.w)
  output Vm = (stg1.Vm, stg2.Vm, stg3.Vm)
end

compile(stgpop)