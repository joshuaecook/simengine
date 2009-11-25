// Hodgkin & Huxley Giant Squid Axon Model (J Physiol, 1952)
//
// Adapted for use with simEngine
// Copyright 2009 Simatra Modeling Technologies, L.L.C.
//
// model definition
import "hh_split.dsl"
model (Vms) = hh_compartment(I_begin, I_middle, I_end) 
    

    iterator t_exp with {continuous, solver=forwardeuler{dt=0.01}}
    iterator t_imp with {continuous, solver=linearbackwardeuler{dt=0.01}}

    input I_begin with {default=0}
    input I_middle with {default=0}
    input I_end with {default=0}    

    constant R = 1

    submodel hh_split hh1
    submodel hh_split hh2
    submodel hh_split hh3
    submodel hh_split hh4
    submodel hh_split hh5
    submodel hh_split hh6
    submodel hh_split hh7
    submodel hh_split hh8
    submodel hh_split hh9
    submodel hh_split hh10

    hh1.I_app = (hh2.Vm - hh1.Vm)/R + I_begin
    hh2.I_app = (hh3.Vm - hh2.Vm)/R + (hh1.Vm - hh2.Vm)/R + I_middle
    hh3.I_app = (hh4.Vm - hh3.Vm)/R + (hh2.Vm - hh3.Vm)/R + I_middle
    hh4.I_app = (hh5.Vm - hh4.Vm)/R + (hh3.Vm - hh4.Vm)/R + I_middle
    hh5.I_app = (hh6.Vm - hh5.Vm)/R + (hh4.Vm - hh5.Vm)/R + I_middle
    hh6.I_app = (hh7.Vm - hh6.Vm)/R + (hh5.Vm - hh6.Vm)/R + I_middle
    hh7.I_app = (hh8.Vm - hh7.Vm)/R + (hh6.Vm - hh7.Vm)/R + I_middle
    hh8.I_app = (hh9.Vm - hh8.Vm)/R + (hh7.Vm - hh8.Vm)/R + I_middle
    hh9.I_app = (hh10.Vm - hh9.Vm)/R + (hh8.Vm - hh9.Vm)/R + I_middle
    hh10.I_app = (hh9.Vm - hh10.Vm)/R + I_end
    
    equation Vm_begin = hh1.Vm
    equation Vm_middle = hh5.Vm
    equation Vm_end = hh10.Vm
    output Vms = (hh1.Vm, hh2.Vm, hh3.Vm, hh4.Vm, hh5.Vm, hh6.Vm, hh7.Vm, hh8.Vm, hh9.Vm, hh10.Vm)


end
