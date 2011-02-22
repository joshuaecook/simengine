To run the program, type "GARCH_simulation(100,10000,111.11,0.1)". The first argument "100" is number of steps. Since we take one step per day, this also means the time to maturity is 100 days. The second argument "10000" means 10000 sample path. "111.11" is the strike price. "0.1" is annual risk-free interest rate.

In this program, we price Asian option with GARCH process. We mainly referred to Duan's paper "The GARCH option pricing model" and utilized the equation (2.3) in page 17. Regarding the parameters alpha_0, alpha_1, beta_1 and lambda, we used those in page 1228 of the paper "Empirical Martingale Simulation for Asset Prices". In Driver.m, we did the same calculation as what the author did in table 5 of the later paper, and got very close answers as those in the table. Hence, our scheme is correct.

GARCH.m: generate the whole evolutions for volatilities and stock prices
simulation.m: do a bunch of simulations and calculate the option price
Driver.m: calculates mean and standard deviation from 500 repetitions.