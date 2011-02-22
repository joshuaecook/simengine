model (Vm, s, spiketime) = canPBC(gnap, gcan, Isyn, Iext, el1, gt, v6, k6, v7, c1, v1, v2, v3, k3, c2)

input gnap with {default = 2.8}
input gcan with {default = 0}
input gt with {default = 10}
input el1 with {default = -59}
input v6 with {default = 0.24}
input k6 with {default = 0.25}
input v7 with {default = 0.1}
input c1 with {default = 0.185}
input v1 with {default = 6.5}
input v2 with {default = 0.066}
input v3 with {default = 0.9}
input k3 with {default = 0.1}
input c2 with {default = 0.19}
input Isyn with {default = 0}
input Iext with {default = 0}
println ("after inputs")

constant factor_time = 8.5e-3

//Electrophysiological constant definitions
constant Cm=21

constant gna=28.0 
constant gk=11.2 
constant gl=2.8 

constant ena=50.0 
constant eca=40.0
constant ek=-85.0 
constant eh=-40.0
constant Kcyt=1.5

//state variable and default initial values:
state Vm = -62.7
state h1 = 0
state n1 = 0
state ca = 0.3
state ip3 = 0.7
state w = 0.1
state mt = 0.001
state ht = 0.562
state s = 0

println "deffed some states"

//decalration of other model parameters
constant namhalf=-45.1 
constant namden=-5.0 
constant nahhalf=-53.0 
constant nahden=6.0 
constant nahscale=0.0001
constant mhalf=-34.0 
constant mden=-5.0
constant nhalf=-29.0 
constant nden=-4.0 
constant nscale=0.1 
constant mhhalf=-65 
constant mhden=8.0  
constant taumh=100.0

constant d1 = 0.13 
constant d2 = 1.05
constant d3 = 0.943
constant d5 = 0.0823

constant a2 = 0.2

constant c0 = 2
//constant c1 = 0.185 

//constant v1 = 7.2 
//constant v2 = 0.066 
//constant v3 = 0.9 

//constant k3 = 0.1 

constant kt = 0.0005

constant Bcyt = 150
constant tau = 0.01 

//input v6 with {default = 0.21} 
//constant k6 = 0.18 
//constant v7 = 0.1

constant kr=1 
constant taus=5
constant shalf=-10.0 
constant sden=-0.5

println "oh cool, we finished constants"

equations
		it = gt*mt*ht*(Vm-eca)
		caer = (1.20-ca)/0.185
		beta = (1+Kcyt*Bcyt/((Kcyt+ca)^2))^-1
		Jerin = c1*(v1*(w*ca/(d5+ca))^3+v2)*(caer-ca)-kt*it
		Jerout = v3*(ca^2)/(k3^2+ca^2)
		vact = 1/(1+exp((Vm+40)/-5))
		Jextout = vact*0.26*(ca^2)/(0.1+ca^2)

		alpha = (ip3+d1)/(ip3+d3)
		winf = ip3*d2/(ip3+d3)/(alpha*d2+ca)
		tauca = (a2*(alpha*d2+ca))^-1
		
		mtinf = (1+exp(-(Vm+38)/5))^-1
		taumt = 2+5/(exp((Vm+28)/25)+exp(-(Vm+28)/70))
		htinf = (1+exp((Vm+70.1)/7))^-1
		tauht = 1+20/(exp((Vm+70)/65)+exp(-(Vm+70)/65))

		namv1 = (Vm-namhalf)/namden 
		namss1   = 1.0 / (1.0+exp(namv1))
		nahv1 = (Vm-nahhalf)/nahden
		nahss1   = 1.0 / (1.0+exp(nahv1))
		nahitau1 = nahscale*cosh(nahv1/2.0)
end
println "did some equations"
equations

		//H current
		mhss1 = 1.0/(1.0+exp((Vm-mhhalf)/mhden))

		//AP current
		mv1    = (Vm-mhalf)/mden
		nv1    = (Vm-nhalf)/nden
		mss1    = 1.0 / (1.0+exp(mv1))
		nss1    = 1.0 / (1.0+exp(nv1))
		nitau1  = nscale*cosh(nv1/2.0)

		f = 1/(1+(0.8/ca)^1)


		sterm = (Vm-shalf)/sden
		sinf = 1/(1+exp(sterm))

		ican = gcan*f*(Vm-ena)
		ina1 = gna*mss1*mss1*mss1*(1.0-n1)*(Vm-ena)
		ik1  = gk*n1*n1*n1*n1*(Vm-ek)
		inap1 = gnap*namss1*h1*(Vm-ena)
		
		il1 = gl*(Vm-el1)

		itot1 = inap1+il1+ina1+ik1+ican+it+Isyn+Iext+(Jextout/kt)

end
println "did some more"
equations

		Vm' = -itot1/Cm
		h1' =  (nahss1-h1)*nahitau1
		n1' = (nss1-n1)*nitau1
            	w' = factor_time*(winf-w)/tauca
		ip3' = factor_time*(v6*ca/(k6+ca)-v7*ip3)
		ca' = factor_time*(beta/tau)*(Jerin-Jerout-Jextout)
		mt' = (mtinf-mt)/taumt
		ht' = (htinf-ht)/tauht
		s' = (sinf*(1-s)-kr*s)/taus
end

println "big block done"

state spiketime = 0
equations
  spiketime = {t when (Vm < Vm[t[-1]] and Vm[t[-1]] > Vm[t[-2]]),
                     spiketime otherwise}
  output_data = (Vm < -40 and Vm[t[-1]] >= -40 and t > 10000)
end

output Vm with {condition=t>30000}
output spiketime with {condition = output_data}
output s with {condition=t>30000}

println "just the solver to do"

//output metrics[t] = (last_spike_time) with {condition=output_data}
solver = ode23
solver.dt = .01

println "and we're done"
end

