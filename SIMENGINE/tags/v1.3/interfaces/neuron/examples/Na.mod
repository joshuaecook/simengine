TITLE  Sodium channel fourth version
: sodium channel based on Robert Lee's modification of Kuo and Bean
: same as second version with different name and Range variables


NEURON {
  SUFFIX Nachan
  USEION na READ ena WRITE ina
  RANGE g, gbar,mVs,aTau,gTau,Con,Coff,Oon,Ooff,m1Vh,m2Vh
  GLOBAL tfac
  RANGE h1Tau,h1ss,h2Tau,h2ss,m,h

}

DEFINE nstates 6

UNITS { 
	(mV) = (millivolt)
	(S) = (siemens)
}


PARAMETER {
	gbar = .015			(S/cm2)

	: kinetic parameters
	mVs = 12			(mV)		: voltage sensitivity of activation
	aTau = 0.05			(ms)		: activation time constant
	gTau = 0.02			(ms)		: time constant for closed-open transition
	Con = 0.004			(/ms)		: rate constant for active closed to inactive closed
	Coff = 4.5			(/ms)		: rate constant for inactive closed to active closed
	Oon = 3.936			(/ms)		: rate constant for active open to inactive open
	Ooff = 0.07208			(/ms)		: rate constant for inactive open to active open
	m1Vh=-26			(mV)
	m2Vh=-96			(mV)
	tfac=4.7						: multiplication factor for temp for 22 to 37 C
}

ASSIGNED {
	 h1Tau		(ms)
         h1ss 
         h2Tau		(ms)
         h2ss		

	 :functional parameters calculated in performance variables procedure

   	Oonp  		 (/ms)  		 
   	Ooffp  		 (/ms)  		 
   	Conp  		 (/ms)  		 
   	Coffp  		 (/ms)  		 
	aVs		 (mV)
	bVs		 (mV)
	gVs		 (mV)
	dVs		 (mV)
	bTau  		(ms)
	dTau  		(ms)
	a[9] 	
	invTau[nstates]	(/ms)	
	hfrac[nstates]	
	af1		(/mV)
	bf1		(/mV)
	gf1		(/mV)
	df1 		(/mV)
	af2		(/ms)
	bf2		(/ms)
	gf2		(/ms)
	df2		(/ms)
	abf1		(/mV)
	gdf1		(/mV)
	abf2		
	gdf2		
			:parameters calculated in CalcParams
	temp[nstates]
	Scale
	m
	s
	h
	RatioAB
	RatioGD
	AStatesSS[nstates]
	IStatesSS[nstates]
			:parameters used in states
	tot
	minf
			:output and independent variables
	dt			    		(ms)
	v					(mV)
 	ena					(mV)
	ina 					(milliamp/cm2)
	g					(S/cm2)
}

STATE {
	Astates[nstates]   FROM 0 TO 1
	Istates[nstates]   FROM 0 TO 1
}

BREAKPOINT {
 SOLVE states
 g = gbar * Astates[5]
 ina = g * (v - ena)
}

INITIAL { 
	newparams()
	performance_variables()
	FROM i=0 TO nstates-1{
	Astates[i]=1./12
	Istates[i]=1./12}
	SOLVE states  
}

PROCEDURE states ()
{
	calcparams(v)
	FROM i=0 TO nstates-1 {
	tot=(Astates[i] + Istates[i])
	minf = tot/(1+hfrac[i])
	Astates[i]=minf-(minf-Astates[i])*exp(-dt*invTau[i])
	Istates[i]=tot-Astates[i]
	}
	
}


PROCEDURE newparams()
{
	 h1Tau =1/(Con+Coff)/tfac
         h1ss =log(Coff/Con)
         h2Tau=1/(Oon+Ooff)/tfac
         h2ss=log(Ooff/Oon)
}

PROCEDURE performance_variables()
{
         Oonp=1/h2Tau/(exp(h2ss)+1)        
         Ooffp=1/h2Tau-Oonp
         Conp=1/h1Tau/(exp(h1ss)+1)
         Coffp=1/h1Tau-Conp
         aVs=2*mVs
         bVs=2*mVs
         gVs=5*mVs
         dVs=5*mVs/4
         bTau=aTau*exp(-m1Vh/mVs)
         dTau=gTau*exp(-m2Vh/mVs)            
         a[0]=1
         a[1]=exp(log((Coffp/Conp)/(Ooffp/Oonp))/8)
 
       FROM i=2 TO 8 {
                  a[i]=a[i-1]*a[1]
	}

        FROM i=0 TO 4{
                  invTau[i]=(Coffp/a[i]+Conp*a[i])
                  hfrac[i]=Conp/Coffp*a[2*i]
        }
         invTau[5]=(Ooffp+Oonp)
         hfrac[5]=Oonp/Ooffp

         :performance variable values
         af1=1/aVs
         bf1=-1/bVs
         gf1=1/gVs
         df1=-1/dVs
         af2=1/aTau
         bf2=1/bTau
         gf2=1/gTau
         df2=1/dTau
         abf1=af1-bf1
         gdf1=gf1-df1
         abf2=af2/bf2
         gdf2=gf2/df2
}

PROCEDURE  calcparams(v(mV)){
         RatioAB=exp(v*abf1)*abf2
         RatioGD=exp(v*gdf1)*gdf2

        :solve active side

         s=Astates[0]+Astates[1]+Astates[2]+Astates[3]+Astates[4]+Astates[5]
         temp[0]=1
         temp[1]=4*RatioAB
         temp[2]=temp[1]*1.5*RatioAB
         temp[3]=temp[2]*0.666666666*RatioAB
         temp[4]=temp[3]*0.25*RatioAB
         temp[5]=temp[4]*RatioGD
         Scale=1/(temp[0]+temp[1]+temp[2]+temp[3]+temp[4]+temp[5])
         m=Scale*temp[5]
         Scale=Scale*s
         AStatesSS[0]=temp[0]*Scale
         AStatesSS[1]=temp[1]*Scale
         AStatesSS[2]=temp[2]*Scale
         AStatesSS[3]=temp[3]*Scale
         AStatesSS[4]=temp[4]*Scale
         AStatesSS[5]=temp[5]*Scale

        :solve inactive side

         s=Istates[0]+Istates[1]+Istates[2]+Istates[3]+Istates[4]+Istates[5]
         temp[1]=temp[1]*a[2]
         temp[2]=temp[2]*a[4]
         temp[3]=temp[3]*a[6]
         temp[4]=temp[4]*a[8]
         temp[5]=temp[4]*RatioGD
         Scale=s/(temp[0]+temp[1]+temp[2]+temp[3]+temp[4]+temp[5])
         IStatesSS[0]=temp[0]*Scale
         IStatesSS[1]=temp[1]*Scale
         IStatesSS[2]=temp[2]*Scale
         IStatesSS[3]=temp[3]*Scale
         IStatesSS[4]=temp[4]*Scale
         IStatesSS[5]=temp[5]*Scale
	 FROM i = 0 TO nstates-1{
         Astates[i]=AStatesSS[i]
	 Istates[i]=IStatesSS[i]
	 }
         h=1-(Istates[0]+Istates[1]+Istates[2]+Istates[3]+Istates[4]+Istates[5])
}