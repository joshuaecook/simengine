class Solver
  var name
  var dt
  var abstol
  var reltol

  // Linear Backward Euler Specific Options
  var lbe_solv
  var lbe_upperhalfbw
  var lbe_lowerhalfbw

  // CVODE Specific Options
  var cv_lmm
  var cv_iter
  var cv_solv
  var cv_upperhalfbw
  var cv_lowerhalfbw
  var cv_maxorder

  constructor(name, dt, abs_tolerance, rel_tolerance)
    self.name = name
    self.dt = dt
    self.abstol = abs_tolerance
    self.reltol = rel_tolerance

    // Linear Backward Euler Specific Options
    self.lbe_solv = "LSOLVER_DENSE" // Currently LSOLVER_BANDED or LSOLVER_DENSE
    self.lbe_upperhalfbw = 0 // Only relevant for banded solver
    self.lbe_lowerhalfbw = 0 // Only relevant for banded solver

    // CVODE Specific Options
    self.cv_lmm = "CV_BDF" // lmm = linear multistep method (can be CV_BDF or CV_ADAMS)
    self.cv_iter = "CV_NEWTON" // iter = nunlinear solver iteration (can be CV_NEWTON or CV_FUNCTIONAL)
    self.cv_solv = "CVDENSE" // solv = specify the type of solver and how they compute the Jacobian
                             // (can be CVDENSE, CVBAND, CVDIAG, CVSPGMR, CVSPBCG, CVSPTFQMR) 
    self.cv_upperhalfbw = 1 // upper and lower half bandwidths for use only with CVBAND 
    self.cv_lowerhalfbw = 1
    self.cv_maxorder = 5
  end

  

end

class Integrators
    property forwardeuler
      get = Solver.new ("forwardeuler", 0.1, 0, 0)
    end
    property exponentialeuler
      get = Solver.new ("exponentialeuler", 0.1, 0, 0)
    end
    property linearbackwardeuler
      get = Solver.new ("linearbackwardeuler", 0.1, 0, 0)
    end
    property rk4
      get = Solver.new("rk4", 0.1, 0, 0)
    end
      //function midpoint(dt)
    property midpoint
      get = Solver.new("midpoint", dt, 0, 0)
    end
      //function heun(dt)
    property heun
      get = Solver.new("heun", dt, 0, 0)
    end
    property ode23
      get = Solver.new("ode23", 0.1, 1e-6, 1e-3)
    end
    property ode45
      get = Solver.new("ode45", 0.1, 1e-6, 1e-3)
    end
    property cvode
      get = Solver.new("cvode", 0, 1e-6, 1e-6)
    end
    property cvode_stiff
      get = Solver.new("cvode", 0, 1e-6, 1e-6) {cv_lmm = "CV_BDF", cv_iter = "CV_NEWTON", cv_maxorder = 5}
    end
    property cvode_diag
      get = Solver.new("cvode", 0, 1e-6, 1e-6) {cv_lmm = "CV_BDF", cv_iter = "CV_NEWTON", cv_solv="CVDIAG", cv_maxorder = 5}
    end
    property cvode_tridiag
      get = Solver.new("cvode", 0, 1e-6, 1e-6) {cv_lmm = "CV_BDF", cv_iter = "CV_NEWTON", cv_solv="CVBAND", cv_upperhalfbw=1, cv_lowerhalfbw=1, cv_maxorder = 5}
    end
    property cvode_nonstiff
      get = Solver.new("cvode", 0, 1e-6, 1e-6) {cv_lmm = "CV_ADAMS", cv_iter = "CV_FUNCTIONAL", cv_maxorder = 12}
    end
//    property mycvode
//      get = Solver.new (yada) {cvode_var1 = blabla}
//    end
end
open (Integrators.new())
//end

//open Integration
