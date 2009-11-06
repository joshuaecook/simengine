namespace Translate

  //TODO: fill in
  function fixName (name)
    // make "][" into ", "
    // make ".[" into "["
    // make "]" into "]." unless it is at end

    name
  end

  multifunction 
    nameQuantities (m: Model, namePrefix)
      foreach q in m.getLocalQuantities() do
        q.addVar("dslname", fixName(namePrefix + (q.getName())))	
      end

      foreach s in m.submodels do
        nameQuantities (s(2), namePrefix + (s(1)) + ".")
      end
    end

    nameQuantities (m: Vector of _, namePrefix)
      var index = 1

      foreach elem in m do        
        nameQuantities (elem, namePrefix + "[" + index + "]")

        index = index + 1
      end
    end
  end

  /* Finds derivatives within equation expressions and resolves them to differential equations. */
  function resolveDifferentialReferences (m: Model)
    multifunction
      isDerivative (exp) = false
      isDerivative (exp: ModelOperation) = exp.name == "deriv"
    end

    multifunction
      resolveDifferentialInExpression (exp) = exp
 
      resolveDifferentialInExpression (exp: ModelOperation)
        if isDerivative(exp) then
          var degree = exp.args 1
          var quant = exp.args 2
          var eq = quant.getEquation()
          var newexp

          if not(istype(type DifferentialEquation,eq)) then
            // use backward difference method of estimation
            //newexp = (quant[m.n] - quant[m.n - 1]) / m.solver.dt
	    error "Don't know how to handle derivatives for quantity "+exp.args(2).tostring()+" with no differential equation defined."
          elseif degree > eq.degree then 
	    error "Don't know how to handle derivatives for quantity "+exp.args(2).tostring()+" of a degree greater than one already defined."
          elseif degree < eq.degree then 
	    error "Don't know how to handle derivatives for quantity "+exp.args(2).tostring()+" of a degree less than one already defined."
          else
            newexp = eq.getExp()
	  end

          newexp
        else
          exp.args = exp.args.map(resolveDifferentialInExpression)
          exp
        end
      end
    end

    foreach eq in m.getEquations() do
      eq.expression = resolveDifferentialInExpression(eq.getExp())
    end
  end

  /* Assigns a visibility condition to the iterator quantities of a given model based on the visibility conditions of all other quantities. */
  function initializeIteratorVisibility (m: Model)
    /* FIXME: this won't work---the iterator visibility needs to be dependent not only on the visibility conditions of other states,
       but also on their enabled condition, which is not known until simulation run time. */
    var itervis = false

    multifunction 
      isIterator (q) = false
      isIterator (q: SimIterator) = true
    end

    foreach quant in [q foreach q in m.getVisibleQuantities() when not(isIterator(q))] do
      itervis = itervis or quant.getIsVisible()
    end

    foreach quant in [q foreach q in m.getLocalQuantities() when isIterator(q)] do
      quant.setIsVisible(itervis)
    end
  end

  /* Adds intermediate equations to the given model and its submodels for any conditionally visible quantities. */
  function initializeVisibilityEquations (m: Model)
    var name

    function isConditionallyVisible (q)
      multifunction
        is (v) = false
        is (v: ModelOperation) = true
        is (v: SimQuantity) = true
      end
      is(q.getIsVisible())
    end

    multifunction 
      isIterator (q) = false
      isIterator (q: SimIterator) = true
    end      
/*
    foreach subm in flatten [sm(2) foreach sm in m.getSubModels()] do
      foreach quant in [q foreach q in subm.getLocalQuantities() when not(isIterator(q)) and isConditionallyVisible(q)] do
        addIntermediateEquation(subm,(quant.getName() + ".isVisible"),quant.getIsVisible())
      end
    end

    foreach quant in [q foreach q in m.getLocalQuantities() when isConditionallyVisible(q)] do
      addIntermediateEquation(m,(quant.getName() + ".isVisible"),quant.getIsVisible())
    end
    */
  end

  /* Resolves references to iterators within equations in submodels to point to the top-level iterator of the same name. */
  function resolveIteratorReferences (m: Model)
    multifunction 
      resolveIteratorInExpression (it) = it

      // FIXME: check to ensure that the quantity returned here is actually the one which is overriding the iterator
      resolveIteratorInExpression (it: SimIterator) = {m.getMember(it.name) when it.isOverridden(),
                                                    it otherwise}

      resolveIteratorInExpression (it: ModelOperation)
        var newArgs = it.args.map(resolveIteratorInExpression)
        it.args = newArgs
	it
      end
    end

    foreach subm in flatten [sm(2) foreach sm in m.getSubModels()] do
      foreach eq in [eq foreach eq in subm.getLocalEquations()] do
        resolveIteratorInExpression(eq.expression)
      end
    end
  end


  multifunction 
    adjustForTemporalIndices(exp)      
    end

    adjustForTemporalIndices(exp: ModelOperation)
      foreach arg in exp.args do
        adjustForTemporalIndices arg
      end
    end

    adjustForTemporalIndices(exp: TemporalReference)
      var temporalIndex = exp.step / exp.simIterator.getStep()
/*      exp.internalState.historyDepth = {temporalIndex when temporalIndex > exp.internalState.historyDepth,
                                        exp.internalState.historyDepth otherwise}*/
      if temporalIndex < exp.internalState.historyDepth then
        exp.internalState.historyDepth = temporalIndex
      end

    end

  end

  function initializeDownsamplers (m: Model)
    // TODO: revisit this if we need to support submodels having differing time deltas or solvers
    var known_downsamplers = []
    var downsampler

    function equivalent_downsamplers (a, b) = a.class == b.class and a.frequency == b.frequency

    function find_known_downsampler (known, candidate)
      var looking_at
      if 1 == known.length() then
        looking_at = known.first()
        if equivalent_downsamplers(looking_at,candidate) then
          looking_at
        else
          undefined
        end
      elseif 1 < known.length() then
        looking_at = known.first()
        if equivalent_downsamplers(looking_at,candidate) then
          looking_at
        else
          find_known_downsampler (known.rest(), candidate)
        end
      else
        undefined
      end
    end

    foreach subm in (flatten [sub 2 foreach sub in m.getSubModels()]).push_front(m) do
      foreach quant in [q foreach q in subm.getLocalQuantities() when isdefined(q.downsampling)] do
        downsampler = find_known_downsampler(known_downsamplers, quant.downsampling)
        if isdefined(downsampler) then
          quant.downsampling = downsampler
        else
          known_downsamplers.push_back(quant.downsampling)
        end

        quant.downsampling.initialize(m.solver.dt, subm, quant)
      end
    end
  end


  function model2forest (m)//: Model)

/*    // clone the model so that changes we make do not affect it.
    var m = LF deepclone m

    initializeDownsamplers(m)

    // ensure that the iterator is visible when any other output is visible
    initializeIteratorVisibility(m)

    // create intermediate equations for visibility conditions
    initializeVisibilityEquations(m)

    // ensure that references to "t" in submodels point to the "t" in the top-level model
    resolveIteratorReferences(m)

    resolveDifferentialReferences(m)

    // initialize the solver (this may create more quantities)
    m.solver.initialize(m)

    // transform differential equations into difference equations
    m.solver.transform(m)

    // name all quantities with string names
    nameQuantities(m, "")
  
    // de-vector
    // add additional equations for time indexing.
    foreach eq in m.getEquations() do
      eq.assigned_state.addVar("historyDepth", 0)
    end
    foreach eq in m.getEquations() do
      adjustForTemporalIndices(eq.getExp())
    end

    // construct a Forest object.
*/
    m
  end

end
