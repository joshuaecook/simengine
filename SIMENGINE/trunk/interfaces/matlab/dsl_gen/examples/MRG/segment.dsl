/*
 * segment (single axon) definition for the MRG model
 * adapted for use with simEngine
 *
 * Copyright 2009 Simatra Modeling Technologies, L.L.C.
 */

import "node.dsl"
import "mysa.dsl"
import "flut.dsl"
import "stin.dsl"

function join1(c1, c2)
  //function to join node to another section
  c1.Iaxonal = (c1.V - c2.V)/((c1.Raxonal + c2.Raxonal)/2)
  c1.Iperiaxonal = (c1.Vp - c2.Vp)/((c1.Rperiaxonal + c2.Rperiaxonal)/2)
end

function join2(c1, c2, c3)
  //function to join a section to 2 other sections
  c1.Iaxonal = (c1.V - c2.V)/((c1.Raxonal + c2.Raxonal)/2) + (c1.V - c3.V)/((c1.Raxonal + c3.Raxonal)/2)
  c1.Iperiaxonal = (c1.Vp - c2.Vp)/((c1.Rperiaxonal + c2.Rperiaxonal)/2)+(c1.Vp-c3.Vp)/((c1.Rperiaxonal + c3.Rperiaxonal)/2)
end

model (VmAxonal_L, VmAxonal_R, VmPeriaxonal_R,
       Raxonal_L, Raxonal_R, Rperiaxonal_R, Rperiaxonal_L) = 
       segment(Istim, Isegmental_L, Isegmental_R, Iperiaxonal_R, VextLeft, VextRight)
  
  input Istim with {default = 0}
  input Isegmental_L with {default = 0}
  input Isegmental_R with {default = 0}
  input Iperiaxonal_R with {default = 0}
  input VextLeft with {default = 0}
  input VextRight with {default = 0}

  submodel node nodeA with {diameter = 1.9, length = 1, Isegmental = Isegmental_L, Vext = VextLeft}
  submodel mysa mysaA with {diameter = 1.9, length=3, Vext = VextLeft + (VextRight - VextLeft)*3/497}
  submodel flut flutA with {diameter = 3.4, length=35, Vext = VextLeft + (VextRight - VextLeft)*38/497}
  submodel stin stinA with {diameter = 3.4, length=70, Vext = VextLeft + (VextRight - VextLeft)*108/497}
  submodel stin stinB with {diameter = 3.4, length=70, Vext = VextLeft + (VextRight - VextLeft)*178/497}
  submodel stin stinC with {diameter = 3.4, length=70, Vext = VextLeft + (VextRight - VextLeft)*248/497}
  submodel stin stinD with {diameter = 3.4, length=70, Vext = VextLeft + (VextRight - VextLeft)*318/497}
  submodel stin stinE with {diameter = 3.4, length=70, Vext = VextLeft + (VextRight - VextLeft)*388/497}
  submodel stin stinF with {diameter = 3.4, length=70, Vext = VextLeft + (VextRight - VextLeft)*458/497}
  submodel flut flutB with {diameter = 3.4, length=35, Vext = VextLeft + (VextRight - VextLeft)*493/497}
  submodel mysa mysaB with {diameter = 1.9, length=3, Isegmental_axonal = Isegmental_R,
                            Isegmental_periaxonal = Iperiaxonal_R, Vext = VextLeft + (VextRight - VextLeft)*496/497}

  nodeA.Istim = Istim

  join1(nodeA, mysaA)
  join2(mysaA, nodeA, flutA)
  join2(flutA, mysaA, stinA)
  join2(stinA, flutA, stinB)
  join2(stinB, stinA, stinC)
  join2(stinC, stinB, stinD)
  join2(stinD, stinC, stinE)
  join2(stinE, stinD, stinF)
  join2(stinF, stinE, flutB)
  join2(flutB, stinF, mysaB)
  join1(mysaB, flutB)

  output VmAxonal_L = nodeA.V
  output VmAxonal_R = mysaB.V
  output VmPeriaxonal_R = mysaB.Vp

  output Raxonal_L = nodeA.Raxonal
  output Raxonal_R = mysaB.Raxonal
  output Rperiaxonal_L = nodeA.Rperiaxonal  
  output Rperiaxonal_R = mysaB.Rperiaxonal
  
end
