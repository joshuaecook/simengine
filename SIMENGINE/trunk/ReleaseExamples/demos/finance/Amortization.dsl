/*
 * periodPayment - amortization function that takes in a principal amount, an interest rate, and the number of periods, 
 * and calculates the payment each period that would be required to pay off the loan fully within the number of periods
 * specified.
 *
 * Copyright 2010 Simatra Modeling Technologies
 */

// periodPayment - amortization formula
function periodPayment(P, i, n) = (P*i)/(1-(1+i)^(-n))

// Define interest rate functions
function fixedRate(period) = 5
function adjustableRate(period) = {3.75 when period < 60,
				   4.75 when period < 72,
				   5.75 when period < 84,
				   6.75 when period < 96,
				   7.75 when period < 108,
				   8 otherwise}

// Define a model to compute an amortization period
model (payments, totalPayments, principal) = Amortization(loan, years, mortgage_type)

    // Two options for adjustable, fixed rate and adjustable rate
    input mortgage_type with {default=0}

    // Define an iterator period which will counts months
    iterator period with {discrete, sample_period=1}

    // Create two states, one to track total payments, the other to track the principal remaining
    state totalPayments = 0 with {iter=period}
    state principal = 0 /* set this to the loan value externally */ with {iter=period}

    // Each of the equations to define the model
    equations
	// Compute the annual and periodic rates based on the type of mortgage
	annualRate = {adjustableRate(period) when mortgage_type,
		      fixedRate(period) otherwise}
	periodicRate = annualRate/100/12  // periodic rate is computed monthly

	// Compute the payment this period using the amortization formula
	payments = periodPayment(principal, periodicRate, (years*12)-period)

	// Incremement the totalPayments and principal as if they were difference equations
	totalPayments[period+1] = totalPayments[period] + payments
	interestPayment = principal * periodicRate
	principalPayment = payments-interestPayment
	principal[period+1] = principal[period] - principalPayment
    end

    // group together the monthly payments split into principal payment and interest payment
    output payments[period] = (payments, principalPayment, interestPayment)

end
