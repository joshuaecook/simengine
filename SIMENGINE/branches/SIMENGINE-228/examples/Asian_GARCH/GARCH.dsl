//GARCH simulation of Asset Price path used for option pricing
model (assetPrice) = GARCH(S0, r, lambda, alpha0, alpha1, beta1) 
   
   iterator period with {discrete, sample_period=1}
   
   input S0 with {default = 100}
   input r with {default = .05/365}
   input lambda with {default = .01}
   input alpha0 with {default = .00001}
   input alpha1 with {default = .2}
   input beta1  with {default = .7}

   //equation initialVariance = alpha0/(1 - alpha1 - beta1)
   state assetPrice = S0 with {iter = period}
   state variance = alpha0/(1-alpha1-beta1) with {iter = period}

   //constant randomNumber = 1
   random randomNumber with {normal, mean=0, stddev=1, iter=period}

   equations
     variance[period + 1] = alpha0+alpha1*variance[period]*(randomNumber - lambda)^2 + beta1*variance[period]
     assetPrice[period + 1] = assetPrice[period]*exp(r - 0.5*variance[period] + randomNumber*sqrt(variance[period]))
   end
   
end


