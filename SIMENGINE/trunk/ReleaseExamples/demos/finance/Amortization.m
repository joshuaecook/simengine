% AMORTIZATION - produces a comparison between a fixed rate mortgage and
% adjustable rate mortgage using simEngine by Simatra
%
% Copyright 2010 Simatra Modeling Technologies
function Amortization

% Compute amortizations and return two tables of data
table = ComputeAmortization(250000, 30);

% Create plots to visualize each mortgage option
figure(1);
subplot(2,2,1);
simplot(table(1).payments);
title('Mortgage Payment Schedule (Fixed Rate)')
legend('Payment', 'Principal Payment', 'Interest Payment');
axis([0 360 0 1800]);
xlabel('Period (months)');
ylabel('Amount (dollars)');
subplot(2,2,3);
title('Equity Schedule')
simplot(table(1).principal, table(1).totalPayments);
legend('Principal', 'Total Payments', 'Location', 'NorthWest')
axis([0 360 0 600000]);
xlabel('Period (months)');
ylabel('Amount (dollars)');

subplot(2,2,2);
simplot(table(2).payments);
title('Mortgage Payment Schedule (Adjustable Rate)')
legend('Payment', 'Principal Payment', 'Interest Payment');
axis([0 360 0 1800]);
xlabel('Period (months)');
ylabel('Amount (dollars)');
subplot(2,2,4);
title('Equity Schedule')
simplot(table(2).principal, table(2).totalPayments);
legend('Principal', 'Total Payments', 'Location', 'NorthWest')
axis([0 360 0 600000]);
xlabel('Period (months)');
ylabel('Amount (dollars)');

end


% COMPUTEAMORTIZATION - takes in a loan amount and the number of years of
% the mortgage to produce an amortization table
function table = ComputeAmortization(loan, years)

% Create two different rate structures
fixedRate = @(period)(5);
adjustableRate = @(period)(piecewise(3.75, period < 60, ...
                                     4.75, period < 72, ...
                                     5.75, period < 84, ...
                                     6.75, period < 96, ...
                                     7.75, period < 108, ...
                                     8.75));

% Run simulations with the given loan data
loan_information = struct('loan', loan, 'years', years);

% Execute two simulations, passing in the different rate functions
table = simex(create_amortization(fixedRate), years*12, loan_information);
table(2) = simex(create_amortization(adjustableRate), years*12, loan_information);

disp(' ');
disp('Loan Information')
disp(sprintf('Loan Amount:\t%9.2f',loan));
disp(sprintf('Mortage Term:\t%0.0f', years));
disp(' ');
disp('Comparison between a fixed 5% rate vs. a 3.75% Hybrid 5-1 ARM:');

% Compute some statistics gathered from the simulation
disp(' ')
disp(sprintf('                     Year       Fixed        Adjustable'));
years = [5 10 30];
for i=1:length(years)
  periods = years(i)*12+1;
  disp(sprintf('Total Payments        %2d     $%9.2f      $%9.2f', ...
               years(i), table(1).totalPayments(periods,2), table(2).totalPayments(periods,2)));
end
disp(' ');
for i=1:length(years)
  periods = years(i)*12+1;
  disp(sprintf('Remaining Principal   %2d     $%9.2f      $%9.2f', ...
               years(i), round(round(table(1).principal(periods,2)* ...
                                     100))/100, round(round(table(2).principal(periods,2)*100))/100));
end
disp(' ');

end


% CREATE_AMORTIZATION - returns a simEngine Model object for computing the
% amortization
% 
function m = create_amortization(rate_function)

% Define the iterator for the system - we're going to iterator over periods
% which is defined as months
period = Iterator('discrete', 'sample_period', 1);

% Create a Model object called Amortization
m = Model('Amortization', period);

% Define the inputs to the model
loan = m.input('loan');
years = m.input('years');

% Define two states of the system, one to track total payments, the other
% to track the principal
totalPayments = m.state(0);
principal = m.state(loan);

% Compute the annual rate as a function of the period
annualRate = rate_function(Exp(period));
periodicRate = annualRate/100/12; % periodic rate is computed monthly

% Compute the payment this period using the amortization formula
periodPayment = @(P, i, n)((P*i)/(1-(1+i)^(-n)));
payments = periodPayment(principal, periodicRate, (years*12)-period);

% Incrememnt the total payments and principal as if they were difference
% equations
m.recurrenceequ(totalPayments, totalPayments + payments);
interestPayment =  principal * periodicRate;
principalPayment = payments - interestPayment;
m.recurrenceequ(principal, principal - principalPayment);

% Group the outputs together
m.output('payments', payments, principalPayment, interestPayment);
m.output(totalPayments);
m.output(principal);
end