
// Purine with definable HGPRT (GMA-System)
// Converted from PurineGHGPRT.plc
model (X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X14, X15, X16, X17, X18) = purine(D)

    // D = deficiency factor in HGPRT (between 0 (no deficiency) and 1 (total deficiency))
    input D with {default=0.99}

    state X1 = 7.13
    state X2 = 114
    state X3 = 0.1477
    state X4 = 2077
    state X5 = 3.85
    state X6 = .62
    state X7 = 33.8
    state X8 = 132
    state X9 = 5.61
    state X10 = 2.54
    state X11 = 24533
    state X12 = 4755
    state X13 = 10
    state X14 = 22.6
    state X15 = 9.7
    state X16 = 145.6
    state X17 = 18
    state X18 = 1400
    
    // Metabolite Flows
    equations
	vprpps=0.9*X1^-.03*X4^-.45*X8^-0.04*X17^.65*X18^.7
	vpyr=1.2951*X1^1.27
	vaprt=233.8*X1^.5*X4^-0.8*X6^.75
	vgprt=361.69*(1-D)*X1^1.2*X8^-1.2*X15^.42
	vhprt=12.569*(1-D)*X1^1.1*X2^-.89*X13^.48
	vden=5.2728*X1^2*X2^-0.06*X4^-0.25*X8^-0.2*X18^-0.08
	vade=0.01*X6^0.55
	vpolyam=0.29*X5^.9
	vtrans=8.8539*X5^.33
	vmat=7.2067*X4^.2*X5^-0.6
	vasuc=3.5932*X2^.4*X4^-0.24*X8^0.2*X18^-0.05
	vasli=66544.7*X3^.99*X4^-0.95
	vimpd=1.2823*X2^.15*X7^-0.09*X8^-0.03
	vgmps=0.3738*X4^0.12*X7^0.16
	vgmpr=0.3005*X2^-.15*X4^-.07*X7^-0.76*X8^.7
	vinuc=0.9135*X2^.8*X18^-0.36
	vampd=0.02688*X4^.8*X8^-0.03*X18^-0.1
	vrnag=0.04615*X11
	vrnaa=0.06923*X11
	vgrna=409.6*X4^0.05*X8^.13
	varna=614.5*X4^0.05*X8^0.13
	vgnuc=0.2511*X8^0.9*X18^-0.34
	vgdrnr=.1199*X8^.4*X9^-1.2*X10^-.39 
	vadrnr=0.0602*X4^.1*X9^-.3*X10^0.87
	vada=0.001062*X4^.97
	vdnag=.001318*X12
	vdnaa=.001938*X12
	vgdna=2.2296*X9^.42*X10^.33
	vadna=3.2789*X9^.42*X10^.33
	vdgnuc=0.03333*X10
	vdada=0.03333*X9
	vgua=0.4919*X15^.5
	vhxd=0.2754*X13^.65
	vxd=.949*X14^.55
	vx=0.0012*X14^2
	vhx=0.003793*X13^1.12
	vua=.00008744*X16^2.21
    end

    // State equations
    equations
	// X1: PRPP
	X1' = vprpps - vden - vaprt - vgprt - vhprt - vpyr
	// X2: IMP
	X2' = vden + vhprt + vampd + vgmpr - vasuc - vimpd - vinuc
	// X3: S-AMP
	X3' = vasuc - vasli
	// X4: Ado, AMP, ADP, ATP
	X4' = vasli + vaprt + vrnaa + vtrans - vmat - vampd - varna - vada - vadrnr
	// X5: SAM
	X5' = vmat - vtrans - vpolyam
	// X6: Ade
	X6' = vpolyam - vaprt - vade
	// X7: XMP
	X7' = vimpd - vgmps
	// X8: GMP, GDP, GTP
	X8' = vgmps + vgprt + vrnag - vgmpr - vgrna - vgnuc - vgdrnr
	// X9: dAdo, dAMP, dADP, dATP    
	X9' = vadrnr + vdnaa - vadna - vdada
	// X10: dGMP, dGDP, dGTP
	X10' = vgdrnr + vdnag - vgdna - vdgnuc
	// X11: RNA
	X11' = varna + vgrna - vrnaa - vrnag
	// X12: DNA
	X12' = vadna + vgdna - vdnag - vdnaa
	// X13: HX, Ino, dIno
	X13' = vada + vdada + vinuc - vhprt - vhx - vhxd
	// X14: Xa
	X14' = vhxd + vgua - vxd - vx
	// X15: Gua, Guo, dGuo
	X15' = vgnuc + vdgnuc - vgprt - vgua
	// X16: UA
	X16' = vxd - vua
	// X17: R5P
	// X18: Pi
    end

    solver=cvode


end
