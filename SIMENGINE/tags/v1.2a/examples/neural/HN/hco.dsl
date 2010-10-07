settings.debug.logdof.setValue(true)
import "hn.dsl"

model (VmR, VmL) = hco(Istim)
	input Istim with {default = 0}

	submodel hn hnR with {Istim = Istim}
	submodel hn hnL

	hnR.Vpre = hnL.VmA
	hnL.Vpre = hnR.VmA

	output VmR = hnR.VmS
	output VmL = hnL.VmS

	solver = cvode//forwardeuler
	solver.dt = 1e-5
end
