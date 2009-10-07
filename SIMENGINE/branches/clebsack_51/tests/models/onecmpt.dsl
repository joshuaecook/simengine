/*
1-compartment motoneuron model with Na, Kdr, KCa, CaN, CaL.
Can be imported into larger model with abitrary number of compartments if modified
to include axial currents to adjacent compartments
Modified from node and axon models provided by Simatra. 
*/
//**********************************************************************************
//**************************Standard One Compartment Definition******************
//**********************************************************************************
function cube(x) = x*x*x
function sqr(x) = x*x
function xinf(a, b, V) = 1/(1 + exp(-(V - a)/b))

model cmprt
   //Geometric parameters
   constant pi = 3.14157
   parameter length (1 to 2000 by 0.5) = 400   // um   
   parameter diameter (1 to 2000 by 0.1) = 400 //um
   
   equation SA = pi*diameter*length
   
   //Electrical parameters
   constant Cn = 1     // uF/cm^2
   constant rho = 70e4 // Ohm*um

   //Conductances, specific by unit area
   parameter GdensKdr (0 to 2 by 1e-4) = 0.015        // S/cm^2
   parameter Gdensleak (0 to 0.01 by 1e-6)= 0.000125  //S/cm^2
   parameter GdensNa (0 to 10 by 1e-4) = 0.01         // S/cm^2
   parameter GdensCaN (0 to 0.02 by 1e-4) = 0.0001    //S/cm^2
   parameter GdensCaL (0 to 0.014 by 1e-4) = 0.0003   //S/cm^2
   parameter GdensKCa (0 to 0.1 by 1e-4) = 0.002      //S/cm^2
   
   //Reversal Potentials
   constant ENa = 50     //mV
   constant EK = -85     //mV
   constant Eleak = -70  //mV
   constant ECa = 80     //mV

   // Maximal Conductances (uS)
   equations
      GNa = SA*GdensNa*0.01
      GKdr = SA*GdensKdr*0.01
      GCaN = SA*GdensCaN*0.01
      GCaL = SA*GdensCaL*0.01
      GKCa = SA*GdensKCa*0.01
      Gleak = SA*Gdensleak*0.01
   end
   
   // Membrane Capacitance (nF)
   equation Cmem = SA*Cn*1e-5

   //Voltage and Current Clamp Controls
   parameter Vclamp (0 to 1 by 1) = 0
   parameter Vclamp_cmd (-90 to 60 by 0.1) = -60
   parameter Iapp (0 to 100 by 0.1) = 0

   //State Variables
   state Vm (-90 to 60 by 0.001) = -70
   state nKdr (0 to 1 by 1e-4) = 0
   state NaC (0 to 1 by 1e-4)=0.7
   state NaO (0 to 1 by 1e-4)=0.0
   state NaI1 (0 to 1 by 1e-4)=0.3
   state NaI2 (0 to 1 by 1e-4)=0.0
   state NaIS1 (0 to 1 by 1e-4)=0.0
   state mNCa (0 to 1 by 1e-4) = 0
   state hNCa (0 to 1 by 1e-4) = 1
   state mLCa (0 to 1 by 1e-4) = 0
   state mKCa (0 to 1 by 1e-4) = 0
   state CaLi (0 to 100 by 0.0001)=0.05    //[Ca] near KCa channels from CaL
   state CaNi (0 to 100 by 0.0001)=0.05    //[Ca] near KCa channels from CaN

   //Delayed rectifier potassium current parameters

   constant nVh = -20    // (mV)
   constant nslp = 18    // (mV)
   constant tVh = -39    // (mV)			
   constant tslp = 5.5   // (mV)			
   constant tmin = 0.8   // (ms)		
   constant taumax = 10  // (ms)

   //Delayed rectifier rate equations

   equations
      b = exp((Vm - tVh)/tslp)
      f = sqr(1 + b)
      ninf = xinf(nVh,nslp,Vm)
      ntau = tmin + taumax*b/f
   end

   //Na parameters
   constant Con = 0.02        //(/ms)		C -> I1 transition
   constant Coff = 0.048      //(/ms)		I1 -> C transition
   constant Oon = 0.84        //(/ms)		O -> I2 transition
   constant Ooff = 0.02       //(/ms)		I2 -> O transition
   constant alpha = 50        //(/ms)		activation
   constant beta = 50         //(/ms)		deactivation
   constant alphaS1 = 0.0005  //(/ms)  	        O -> IS1 transition
   constant alphaS2 = 0.0001  //(/ms) 	        I2 -> IS2 transition
   constant betaS = 0.00024   //(/ms)	        IS2 -> I2 and IS1-> O
   constant aVs = -7.00	      //(/mV)		Vslp of activation (alpha)
   constant bVs = 4.00        //(mV)		Vslp of deactivation (beta)
   constant aVh = 5.00	      //(mV)		alpha shift
   constant bVh = 50.00	      //(mV)		beta shift
   constant tfac =3           //		multiplication factor for temp

   //Na rate equations
   equations
      afac = ((Coff/Con)/(Ooff/Oon))
      fCO = alpha * tfac /(1+exp((Vm+aVh)/aVs)) //modified from original to avoid high rates
      bCO =  beta * tfac/(1+ exp((Vm+bVh)/bVs)) //modified from original to avoid high rates
      fis1= tfac*alphaS1
      bis1=tfac*betaS
      fis2= tfac*alphaS2
      bis2=tfac*betaS
      fI1I2 = alpha * tfac* afac/(1+exp((Vm+aVh)/aVs))
      bI1I2 = beta * (tfac/afac)/(1+ exp((Vm+bVh)/bVs))
   end

   //N-type Ca current parameters
   constant tmNCa = 2	    //(ms)
   constant thNCa = 40	    //(ms)
   constant mVhNCa  = -20   //(mV)
   constant mslpNCa = 5     //(mV)
   constant hVhNCa = -45    //(mV)
   constant hslpNCa = -5    //(mV)

   //N-type Ca current rate equations
   equations
      minfNCa = xinf(mVhNCa,mslpNCa,Vm)
      hinfNCa = xinf(hVhNCa,hslpNCa,Vm)
   end

   //L-type Ca current parameters
   constant tmLCa = 50	  //(ms)
   constant mVhLCa  = -40 //(mV)
   constant mslpLCa = 5   //(mV)

   //L-type Ca current rate equations
   equations
      minfLCa = xinf(mVhLCa,mslpLCa,Vm)
   end

   //KCa channel parameters
   constant  cainf=0.1                            //resting Ca concentration in uM
   parameter KpN (0 to 100000 by 100) = 40000     //(uM/ms/nA) 
   parameter RCaN (0.001 to 1 by 0.001) = 0.05	  //(/ms)	
   parameter KpL (0 to 1000 by 10) = 50           //(uM/ms/nA) 
   parameter RCaL (0.001 to 1 by 0.001) = 0.002	  //(/ms)	
   parameter RKCaon (0.001 to 1 by 0.001) = 0.1	  //(/ms) KCa activation
   parameter RKCaoff (0.001 to 1 by 0.001) = 0.1  //(/ms) Ca deactivation

   //KCa channel rate equations
   equations
      aKCa = RKCaon * sqr(CaNi+CaLi-cainf)        //rate constant depends on [Ca]^2 above rest
      mtauKCa_inv = (aKCa+RKCaoff)
      mtauKCa = 1/(aKCa+RKCaoff)
      minfKCa = aKCa*mtauKCa
   end

   //Derived Currents (nA)
   equations
      IKdr = GKdr*cube(nKdr)*nKdr*(Vm-EK)
      Ileak = Gleak*(Vm-Eleak)
      INCa = GCaN*sqr(mNCa)*hNCa*(Vm-ECa)
      ILCa = GCaL*mLCa*(Vm-ECa)
      IKCa = GKCa * mKCa*(Vm-EK)
      INa = GNa*NaO*(Vm - ENa)
      IChantot=IKdr+INCa+ILCa+IKCa+INa
   end

   //Derived State
   equation NaIS2 = 1-NaC-NaO-NaI1-NaI2-NaIS1

   // Differential Equations
   equations
      nKdr' = (ninf-nKdr)/ntau
      NaC' = -fCO*NaC+bCO*NaO-Con*tfac*NaC+Coff*tfac*NaI1
      NaO' = -bCO*NaO+fCO*NaC-Oon*tfac*NaO+Ooff*tfac*NaI2-fis1*NaO+bis1*NaIS1
      NaI1' = -fI1I2*NaI1+bI1I2*NaI2+Con*tfac*NaC-Coff*tfac*NaI1
      NaI2' = fI1I2*NaI1-bI1I2*NaI2-Ooff*tfac*NaI2+Oon*tfac*NaO-fis2*NaI2+bis2*NaIS2
      NaIS1' = -bis1*NaIS1+fis1*NaO
      mNCa' = (minfNCa-mNCa)/tmNCa
      hNCa' = (hinfNCa-hNCa)/thNCa
      mLCa' = (minfLCa-mLCa)/tmLCa
      CaNi' = -KpN*INCa/SA - RCaN*CaNi
      CaLi' = -KpL*ILCa/SA - RCaL*CaLi
      //mKCa' = (minfKCa-mKCa)/mtauKCa
      mKCa' = (minfKCa-mKCa)*mtauKCa_inv
      //Vm' = IF Vclamp THEN ((Vclamp_cmd-Vm)/dt) ELSE -(1/Cmem)*(Ileak+IChantot-Iapp)
      Vm' = -(1/Cmem)*(Ileak+IChantot-Iapp)
   end
 
   solver = forwardeuler(0.0001)
   t.setPrecision (Range.new(0, 1000, 0.0001))

   setVisible |Vm|
   setVisible |I*|
   setVisible |CaNi|
   setVisible |CaLi|
end

