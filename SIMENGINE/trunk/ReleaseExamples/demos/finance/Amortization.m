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

% Run two simulations in parallel by setting mortgage type to 0 and 1
loan_information = struct('loan', loan, 'years', years, 'mortgage_type', [0 1]);

% Execute the parallel simulation
table = simex('Amortization.dsl', years*12, loan_information);

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