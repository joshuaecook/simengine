/*
1-compartment motoneuron model with Na, Kdr, KCa, CaN, CaL.
Can be imported into larger model with abitrary number of compartments if modified
to include axial currents to adjacent compartments
Modified from node and axon models provided by Simatra. 
*/
//**********************************************************************************
function cube(x) = x*x*x
function sqr(x) = x*x
function xinf(a, b, V) = 1/(1 + exp(-(V - a)/b))

model (Vm, Raxonal) = compartment (I1, I2)
//***********************************************************************************
//Special Functions Definition
//***********************************************************************************
//    function sum(v: Vector of _)
//      var total = 0  
//      foreach i in v do
//        total = total + i
//      end
//      total
//    end

//    var compartmentalCurrents = []
//    equation Icompartmental = 0

//    function connect(c)
//        compartmentalCurrents.push_back((self.Vm - c.Vm)/((self.Raxonal + c.Raxonal)/2))
//        equation Icompartmental = sum(compartmentalCurrents) //nA
//    end

//***********************************************************************************
//Geometric parameters and constants
//***********************************************************************************

   constant pi = 3.14157
   constant length = 400     //um   
   constant diameter = 400   //um
   equation SA = pi*diameter*length              //um^2
   
//***********************************************************************************
//Electrophysiological parameters and constants
//***********************************************************************************

   //Electrical parameters
   constant Cn = 1     // uF/cm^2
   constant rho = 70e4 // Ohm*um
   
   //Conductances, specific by unit area
   constant GdensKdr = 0.08          //S/cm^2  0.015 in 1-compartment
   constant Gdensleak = 0.00005   //S/cm^2  0.000125 in 1-compartment
   constant GdensNa = 0.015         //S/cm^2  0.01 in 1-compartment
   constant GdensCaN = 0.01      //S/cm^2  0.0001 in 1-compartment
   constant GdensCaL = 0.007    //S/cm^2  0.0003 in 1-compartment
   constant GdensKCa = 0.05       //S/cm^2  0.002 in 1-compartment
   //parameter Gaxonal (0.001 to 1000 by 0.001) = 5      //unkown units

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
      Gaxonal = 1e6*(3.1415926 * diameter*diameter)/(4*length*rho) //uS
      Raxonal = 1/Gaxonal
   end
   
   // Membrane Capacitance (nF)
   equation Cmem = SA*Cn*1e-5

   //Voltage and Current Clamp Controls
   //parameter Vclamp (0 to 1 by 1) = 0
   //parameter Vclamp_cmd (-90 to 60 by 0.1) = -60
   //parameter Iapp (0 to 100 by 0.1) = 0  //nA
   equation Iapp = I1 + I2
   //constant Iapp = 0

//***********************************************************************************
//State variable definition
//***********************************************************************************
   //State Variables
   state Vm = -70       //mV
   state nKdr = 0       //unitless
   state NaC = 0.7      //unitless
   state NaO = 0.0      //unitless
   state NaI1 = 0.3     //unitless
   state NaI2 = 0.0     //unitless
   state NaIS1 = 0.0    //unitless
   state mNCa = 0       //unitless
   state hNCa = 1       //unitless
   state mLCa = 0       //unitless
   state mKCa = 0       //unitless
   state CaLi = 0.05    //[Ca] near KCa channels from CaL
   state CaNi = 0.05    //[Ca] near KCa channels from CaN

//***********************************************************************************
//Delayed rectifier potassium current parameters and equations
//***********************************************************************************
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

//***********************************************************************************
//Na parameters
//***********************************************************************************
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

//***********************************************************************************
//N-type Ca current parameters and equations
//***********************************************************************************
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

//***********************************************************************************
//L-type Ca current parameters and equations
//***********************************************************************************
   constant tmLCa = 50	  //(ms)
   constant mVhLCa  = -40 //(mV)
   constant mslpLCa = 5   //(mV)

   //L-type Ca current rate equations
   equations
      minfLCa = xinf(mVhLCa,mslpLCa,Vm)
   end

//***********************************************************************************
//KCa channel parameters and equations
//***********************************************************************************
   constant  cainf=0.1                            //resting Ca concentration in uM
   constant KpN = 40000     //(uM/ms/nA) 
   constant RCaN = 0.05	  //(/ms)	
   constant KpL = 50           //(uM/ms/nA) 
   constant RCaL = 0.002	  //(/ms)	
   constant RKCaon = 0.1	  //(/ms) KCa activation
   constant RKCaoff = 0.1  //(/ms) Ca deactivation

   //KCa channel rate equations
   //state temp_Ca (0 to 10000 by 0.001) = 0.05
  
   equations
      temp_Ca = sqr(CaNi+CaLi-cainf)
      aKCa = RKCaon * temp_Ca        //rate constant depends on [Ca]^2 above rest
      mtauKCa = 1/(aKCa+RKCaoff)
      minfKCa = aKCa*mtauKCa
   end

//***********************************************************************************
//Derived Currents (nA)
//***********************************************************************************
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

//***********************************************************************************
//Model Differential Equations
//***********************************************************************************
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
      mKCa' = (minfKCa-mKCa)/mtauKCa
      Vm' = -(1/Cmem)*(Ileak+IChantot/*+Icompartmental*/+Iapp)
    end
 

   solver = rk4
   solver.dt = (0.0001)

//    setVisible |Vm|
    //setVisible |I*|
//    setVisible |CaNi|
//    setVisible |CaLi|
end

//compile(compartment)
