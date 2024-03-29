%  SIMPLOT Plots simulation structure or matrices generated by simex
%     Usage:
%     SIMPLOT(XDATA, YDATA, ...)
%     SIMPLOT(XDATA, YDATA, LINEFORMAT, ...)
%     SIMPLOT(MATRIX, ...)
%     SIMPLOT(MATRIX, LINEFORMAT, ...)
%     SIMPLOT(SIMEXOUTPUT, ...)
%     SIMPLOT(SIMEXOUTPUT, LINEFORMAT, ...)
% 
%     Description:
%     SIMPLOT plots simulation output structures generated by SIMEX
%     in the same manner that the PLOT command will plot X-Y data
%     pairs.  SIMPLOT supports both grouped outputs and multiple
%     outputs per structure.
%
%       XDATA is a 1-D vector of time data
%
%       YDATA is a 1-D vector of value data
%
%       LINEFORMAT is the string format used by PLOT to specify
%       line color and line style
%
%       MATRIX is an N by M matrix where the first column is the
%       time and the remaining M-1 are value vectors plotted
%       against time
%
%       SIMEXOUTPUT is an output structure generated by SIMEX that
%       includes fields that are the same type as MATRIX
%
% Copyright 2009,2010 Simatra Modeling Technologies, L.L.C.
% For more information, please visit http://www.simatratechnologies.com
% For additional help, please email support@simatratechnologies.com
%
