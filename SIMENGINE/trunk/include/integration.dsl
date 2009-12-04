// namespace Integration

// namespace Solvers
// class IntegrationMethod
//   var dt // to be filled in by the specifics of the solver

//   hidden function isDifferentEq (eq) = false
//   hidden overload function isDifferentEq (eq:DifferentialEquation) = true
  
//   hidden function isNotDifferentEq (eq) = not (isDifferentEq eq)


//   function transformEqs (eqs: Vector of Equation)/*: Vector of Equation*/
//     // default is to leave them unchanged
//   end

//   function transform (m: Model)
//     var eqs = m.getEquations()
//     var quants = m.getVisibleQuantities()
//     var tvis = exists q in quants suchthat q.getName() == "t"
//     var nvis = exists q in quants suchthat q.getName() == "n"

//     if istype(type Iterator, m.t) then
//       if not (exists eq in eqs suchthat isDifferentEq(eq)) then
//         if tvis then m.t.setIsVisible(false) end
// 	if not nvis then m.n.setIsVisible(true) end
//       else
//         if not tvis then m.t.setIsVisible(true) end
// 	if nvis then m.n.setIsVisible(false) end
//       end
//     end

//     transformEqs(eqs)
//   end

//   function initialize(m: Model)
//   end
// end

// class InvalidIntegrationMethod extends IntegrationMethod
//   function transformEqs(eqs)
//     if exists eq in eqs suchthat isDifferentEq eq then //exists(isDifferentEq, eqs) then
//       error "No integration method was defined at the top-most model.  One must be set in 'solver'."
//     end
//   end
// end

// class FixedStepIntegrationMethod extends IntegrationMethod
//   var maxduration

//   function initialize(m: Model)
//     var tvis = exists q in m.getVisibleQuantities() suchthat q.getName() == "t"
    
//     if istype(type Iterator, m.t) then
//       m.t.setStep(dt)
//       if isdefined(maxduration) then
//         m.t.setMaxDuration(maxduration)
//       end
//       if not tvis then m.t.setIsVisible(true) end
//     end

//     if istype(type Iterator, m.n) then
//       m.n.setStep(1)
//       if isdefined maxduration then
//         m.n.setMaxDuration(1/dt * maxduration)
//       else
//         m.n.setMaxDuration(2^31-1)
//         //TODO: de-hardcode
//         //error "solver.maxduration must be set to the maximal time value" //TODO: make this more general for iterators
//       end
//       //m.n.setIsVisible(true)
//     end

//   end

//   constructor (dt)
//     var newdt = 2^(logn(dt,2).floor())
//     if (dt <> newdt) then
//       warning ("Selecting " + newdt + " for dt instead of specified value " + dt)
//     end
//     self.dt = newdt
//   end
// end

// class ForwardEuler extends FixedStepIntegrationMethod
//   function add_fwd_euler (eq: Equation)
//     //eq
//   end
  
//   overload function add_fwd_euler (eq: DifferentialEquation)
//     eq.assigned_state.setEquation(Equation.new (eq.assigned_state, 
//                    	  	                eq.assigned_state + dt * eq.expression))
//   end

//   function transformEqs (eqs: Vector of Equation)/*: Vector of Equation*/
//     eqs.map add_fwd_euler
//   end

//   constructor (dt)
//     super(dt)
//   end
// end


// namespace ModelRewriter

//   function initialize (eq: Equation)
//     var s = eq.getState()
//     s.addVar("rewriter_value")
//     s.rewriter_value = s
//     s.addVar("rewriter_storage")
//     s.rewriter_storage = s
//   end

//   overload function initialize(eqs: Vector of Equation)
//     foreach eq in eqs do 
//       initialize(eq)
//     end
//   end

//   function reset (eqs: Vector of Equation)
//     foreach eq in eqs do
//       eq.getState().rewriter_value = eq.getState()
//     end
//   end


//   function regenerate (eq: Equation)
//     function regenerateExp(x: Number) = x
//     overload function regenerateExp (x: Binary) = x
//     overload function regenerateExp (x: Boolean) = x
//     overload function regenerateExp (x: SimQuantity) = x
//     overload function regenerateExp (x: State) = x.rewriter_value
//     overload function regenerateExp (x: ModelOperation) = 
//         LF apply (x.execFun, x.args.map(regenerateExp).totuple())

//     regenerateExp(eq.getExp())
//   end
// end

// /*
// 3)  Heun method (predictor-corrector type Euler)

// y[n+1]* = y[n] + h*f(y[n])
// y[n+1] = y[n] + (h/2)*(f(y[n]) + f(y[n+1]*))

// If t (or whatever variable the DEQ is in respect to) is part of the
// equation, i.e. dy/dt = f(y,t), the equations would be (note, we
// probably need to consider implementing all methods for the general
// case dy/dt = f(y, t), if we can):

// y[n+1]* = y[n] + h*f(y[n], t[n])
// y[n+1] = y[n] + (h/2)*(f(y[n], t[n]) + f(y[n+1]*, t[n+1]))

// So, you're estimating y[n+1] using Euler, then calculating what the
// derivative would be using that estimate, and using an average of the
// two derivatives to calculate the "actual" y[n+1]
// */
// class Heun extends FixedStepIntegrationMethod
//   function transformEqs(eqs: Vector of Equation)
//     constant diffEqs = eqs.filter isDifferentEq
//     constant restEqs = eqs.filter isNotDifferentEq

//     ModelRewriter.initialize(diffEqs)

//     // We break the equations into the following parts:
//     // y.base = f(y[n],t[n])
//     // y.predict = f(y[n] + h * y.base)
//     // y.correct = y[n] + (h/2) * (y.base + y.predict)

//     //generate half of 
//     foreach eq in diffEqs do
//       eq.assigned_state.addVar "base"
//       eq.assigned_state.base = ModelRewriter.regenerate(eq)
//     end    

//     // generate predictor term
//     //   - setup rewriter term
//     foreach eq in diffEqs do
//       eq.assigned_state.rewriter_value = eq.assigned_state + dt * eq.assigned_state.base
//     end
//     //   - create predict value
//     foreach eq in diffEqs do
//       eq.assigned_state.addVar "predict"
//       eq.assigned_state.predict = ModelRewriter.regenerate(eq)
//     end    
 
//     // generate correct term and put into result
//     foreach eq in diffEqs do
//       eq.assigned_state.setEquation(Equation.new(eq.assigned_state, 
// 				                 eq.assigned_state + (dt/2) * (eq.assigned_state.base + eq.assigned_state.predict)))
				      
//     end
//   end
//   constructor (dt)
//     super(dt)
//   end

// end


// /*
// 4) Midpoint method

// y[n+1] = y[n] + h*f(y[n] + (h/2)*f(y[n]))

// essentially, you're estimating the derivative at the midpoint t[n] +
// (h/2) and using that as the derivative at y[n] to calculate y[n+1].
// Considered more accurate than Euler, but I"m not sure about stability.
// */
// class Midpoint extends FixedStepIntegrationMethod
//   function transformEqs (eqs: Vector of Equation)/*: Vector of Equation*/
//     constant diffEqs = eqs.filter isDifferentEq
//     constant restEqs = eqs.filter isNotDifferentEq

//     ModelRewriter.initialize(diffEqs)

//     //y[n] + (h/2)*f(y[n])
//     foreach eq in diffEqs do
//       eq.assigned_state.rewriter_storage = eq.assigned_state + dt / 2 * (ModelRewriter.regenerate eq)
//     end
    
//     foreach eq in diffEqs do
//       eq.assigned_state.rewriter_value = eq.assigned_state.rewriter_storage
//     end

//     //result
//     foreach eq in diffEqs do
//       eq.assigned_state.setEquation(Equation.new(eq.assigned_state,
// 	                                         eq.assigned_state + dt * (ModelRewriter.regenerate eq)))
//     end

//   end  
//   constructor (dt)
//     super(dt)
//   end
// end


// class RK4 extends FixedStepIntegrationMethod
//   function transformEqs (eqs: Vector of Equation)/*: Vector of Equation*/
//     constant diffEqs = eqs.filter isDifferentEq
//     constant restEqs = eqs.filter isNotDifferentEq
  
//     ModelRewriter.initialize(diffEqs)
   
//     //k1
//     foreach eq in diffEqs do
//       eq.assigned_state.addVar("k1")
//       eq.assigned_state.k1 = operator_noop(dt * ModelRewriter.regenerate(eq))
//     end

//     //k2
//     foreach eq in diffEqs do
//       eq.assigned_state.rewriter_value = eq.assigned_state + 0.5 * eq.assigned_state.k1
//     end
    
//     foreach eq in diffEqs do
//       eq.assigned_state.addVar("k2")
//       eq.assigned_state.k2 = operator_noop(dt * ModelRewriter.regenerate(eq))
//     end
    

//     //k3
//     foreach eq in diffEqs do
//       eq.assigned_state.rewriter_value = eq.assigned_state + 0.5 * eq.assigned_state.k2
//     end
    
//     foreach eq in diffEqs do
//       eq.assigned_state.addVar("k3")
//       eq.assigned_state.k3 = operator_noop(dt * ModelRewriter.regenerate(eq))
//     end

//     //k4
//     foreach eq in diffEqs do
//       eq.assigned_state.rewriter_value = eq.assigned_state + eq.assigned_state.k3
//     end
    
//     foreach eq in diffEqs do
//       eq.assigned_state.addVar("k4")
//       eq.assigned_state.k4 = operator_noop(dt * ModelRewriter.regenerate(eq))
//     end

//     //result
//     foreach eq in diffEqs do
//       constant k1 = eq.assigned_state.k1
//       constant k2 = eq.assigned_state.k2
//       constant k3 = eq.assigned_state.k3
//       constant k4 = eq.assigned_state.k4

//       eq.assigned_state.setEquation(Equation.new(eq.assigned_state,
// 				                 eq.assigned_state + 1/6 * (k1 + 2*k2 + 2*k3 + k4)))
//     end
//   end

//   constructor (dt)
//     super(dt)
//   end
 
// end

// end


// function forwardeuler(dt) = Solvers.ForwardEuler.new(dt)
// function rk4(dt) = Solvers.RK4.new(dt)
// function midpoint(dt) = Solvers.Midpoint.new(dt)
// function heun(dt) = Solvers.Heun.new(dt)
// function invalid_method() = Solvers.InvalidIntegrationMethod.new()

class Solver
  var name
  var dt
  var abstol
  var reltol
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
      get = Solver.new("cvode", -1, 1e-6, 1e-6)
    end
    property cvode_stiff
      get = Solver.new("cvode", -1, 1e-6, 1e-6) {cv_lmm = "CV_BDF", cv_iter = "CV_NEWTON", cv_maxorder = 5}
    end
    property cvode_diag
      get = Solver.new("cvode", -1, 1e-6, 1e-6) {cv_lmm = "CV_BDF", cv_iter = "CV_NEWTON", cv_solv="CVDIAG", cv_maxorder = 5}
    end
    property cvode_tridiag
      get = Solver.new("cvode", -1, 1e-6, 1e-6) {cv_lmm = "CV_BDF", cv_iter = "CV_NEWTON", cv_solv="CVBAND", cv_upperhalfbw=1, cv_lowerhalfbw=1, cv_maxorder = 5}
    end
    property cvode_nonstiff
      get = Solver.new("cvode", -1, 1e-6, 1e-6) {cv_lmm = "CV_ADAMS", cv_iter = "CV_FUNCTIONAL", cv_maxorder = 12}
    end
//    property mycvode
//      get = Solver.new (yada) {cvode_var1 = blabla}
//    end
end
open (Integrators.new())
//end

//open Integration
