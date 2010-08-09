function varargout = CompareCPUGPU(varargin)
% COMPARECPUGPU M-file for CompareCPUGPU.fig
%      COMPARECPUGPU, by itself, creates a new COMPARECPUGPU or raises the existing
%      singleton*.
%
%      H = COMPARECPUGPU returns the handle to a new COMPARECPUGPU or the handle to
%      the existing singleton*.
%
%      COMPARECPUGPU('CALLBACK',hObject,eventData,handles,...) calls the local
%      function named CALLBACK in COMPARECPUGPU.M with the given input arguments.
%
%      COMPARECPUGPU('Property','Value',...) creates a new COMPARECPUGPU or raises the
%      existing singleton*.  Starting from the left, property value pairs are
%      applied to the GUI before CompareCPUGPU_OpeningFcn gets called.  An
%      unrecognized property name or invalid value makes property application
%      stop.  All inputs are passed to CompareCPUGPU_OpeningFcn via varargin.
%
%      *See GUI Options on GUIDE's Tools menu.  Choose "GUI allows only one
%      instance to run (singleton)".
%
% See also: GUIDE, GUIDATA, GUIHANDLES

% Edit the above text to modify the response to help CompareCPUGPU

% Last Modified by GUIDE v2.5 16-Oct-2009 16:25:58

% Begin initialization code - DO NOT EDIT
gui_Singleton = 1;
gui_State = struct('gui_Name',       mfilename, ...
                   'gui_Singleton',  gui_Singleton, ...
                   'gui_OpeningFcn', @CompareCPUGPU_OpeningFcn, ...
                   'gui_OutputFcn',  @CompareCPUGPU_OutputFcn, ...
                   'gui_LayoutFcn',  [] , ...
                   'gui_Callback',   []);
if nargin && ischar(varargin{1})
    gui_State.gui_Callback = str2func(varargin{1});
end

if nargout
    [varargout{1:nargout}] = gui_mainfcn(gui_State, varargin{:});
else
    gui_mainfcn(gui_State, varargin{:});
end
% End initialization code - DO NOT EDIT


% --- Executes just before CompareCPUGPU is made visible.
function CompareCPUGPU_OpeningFcn(hObject, eventdata, handles, varargin)
% This function has no output args, see OutputFcn.
% hObject    handle to figure
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
% varargin   command line arguments to CompareCPUGPU (see VARARGIN)

% Choose default command line output for CompareCPUGPU
handles.output = hObject;

% Update handles structure
guidata(hObject, handles);

image(imread('TimingNetworkDiagram.jpg'), 'Parent', ...
      handles.Axes_Legend);

image(imread('logo.jpg'), 'Parent', handles.Axes_Logo);

axis(handles.Axes_Legend,'off');
set(gca, 'xtick',[], 'ytick',[]);

axis(handles.Axes_Logo,'off');
set(gca, 'xtick',[], 'ytick',[]);


% uiwait makes CompareCPUGPU wait for user response (see UIRESUME)
% uiwait(handles.figure1);

paramdata = cell(1,2);
paramdata{1,1} = 0;
paramdata{1,2} = 100;
paramdata{2,1} = 11;
paramdata{2,2} = 11;
paramdata{3,1} = -62.5;
paramdata{3,2} = -62.5;
paramdata{4,1} = 4;
paramdata{4,2} = 4;
paramdata{5,1} = 5.25;
paramdata{5,2} = 5.25;
set(handles.ParameterTable, 'Data', paramdata);



% --- Outputs from this function are returned to the command line.
function varargout = CompareCPUGPU_OutputFcn(hObject, eventdata, handles) 
% varargout  cell array for returning output args (see VARARGOUT);
% hObject    handle to figure
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Get default command line output from handles structure
varargout{1} = handles.output;


% --- Executes on button press in RunCPUButton.
function RunCPUButton_Callback(hObject, eventdata, handles)
% hObject    handle to RunCPUButton (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
dim = str2double(get(handles.CPUStepsEdit, 'String'));

paramdata = get(handles.ParameterTable,'Data');


load('states');
cd('CPU');

m = simex('../HN/timingNetwork.dsl','-dontrecompile');
inputs.stimR4 = [paramdata{1,1} paramdata{1,2}];
inputs.gleak3 = [paramdata{2,1} paramdata{2,2}];
inputs.Eleak3 = [paramdata{3,1} paramdata{3,2}];
inputs.gh3 = [paramdata{4,1} paramdata{4,2}];
inputs.tauhCaS3 = [paramdata{5,1} paramdata{5,2}];

params = createInputSpace(m, dim, inputs);

tic;
  [out y1 t1] = simex('../HN/timingNetwork.dsl',15,params,states,'-dontrecompile','-cpu','-float','-decimation=4','-debug');
t=toc;

cd('..');

set(handles.CPUSimulationTime, 'String', sprintf('%.2f s',t));
set(handles.CPUTimePerModel, 'String', sprintf('%.2f s',t/dim));

handles.CPUTimePerModelValue = t/dim;

[x, y, M] = getDataMatrix(0,15,out,2);
surf(y, x, M, 'EdgeAlpha', 0, 'Parent', handles.Axes_HNL3);
set(handles.Axes_HNL3, 'YLimMode', 'manual', 'YLim', [0 dim], 'Visible', 'on');
[x, y, M] = getDataMatrix(0,15,out,3);
surf(y, x, M, 'EdgeAlpha', 0, 'Parent', handles.Axes_HNR3);
set(handles.Axes_HNR3, 'YLimMode', 'manual', 'YLim', [0 dim], 'Visible', 'on');
[x, y, M] = getDataMatrix(0,15,out,4);
surf(y, x, M, 'EdgeAlpha', 0, 'Parent', handles.Axes_HNL4);
set(handles.Axes_HNL4, 'YLimMode', 'manual', 'YLim', [0 dim], 'Visible', 'on');
[x, y, M] = getDataMatrix(0,15,out,5);
surf(y, x, M, 'EdgeAlpha', 0, 'Parent', handles.Axes_HNR4);
set(handles.Axes_HNR4, 'YLimMode', 'manual', 'YLim', [0 dim], 'Visible', 'on');
[x, y, M] = getDataMatrix(0,15,out,6);
surf(y, x, M, 'EdgeAlpha', 0, 'Parent', handles.Axes_HNL1);
set(handles.Axes_HNL1, 'YLimMode', 'manual', 'YLim', [0 dim], 'Visible', 'on');
[x, y, M] = getDataMatrix(0,15,out,7);
surf(y, x, M, 'EdgeAlpha', 0, 'Parent', handles.Axes_HNR1);
set(handles.Axes_HNR1, 'YLimMode', 'manual', 'YLim', [0 dim], 'Visible', 'on');
%simplot(out(1),'Parent',handles.CPUAxes);

set(handles.RunGPUButton, 'Enable', 'on');

assignin('base','handles',handles);

guidata(hObject, handles);


% --- Executes on button press in RunGPUButton.
function RunGPUButton_Callback(hObject, eventdata, handles)
% hObject    handle to RunGPUButton (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
dim = str2double(get(handles.GPUStepsEdit, 'String'));

paramdata = get(handles.ParameterTable,'Data');

load('states');
cd('GPU');

m = simex('../HN/timingNetwork.dsl','-dontrecompile');
inputs.stimR4 = [paramdata{1,1} paramdata{1,2}];
inputs.gleak3 = [paramdata{2,1} paramdata{2,2}];
inputs.Eleak3 = [paramdata{3,1} paramdata{3,2}];
inputs.gh3 = [paramdata{4,1} paramdata{4,2}];
inputs.tauhCaS3 = [paramdata{5,1} paramdata{5,2}];

params = createInputSpace(m, dim, inputs);

tic;
  [out y1 t1] = simex('../HN/timingNetwork.dsl',15,params,states,'-dontrecompile','-gpu','-float','-decimation=4','-debug');
t=toc;
cd('..');
  
set(handles.GPUSimulationTime, 'String', sprintf('%.2f s', t));
set(handles.GPUTimePerModel, 'String', sprintf('%.2f s', t/dim));
ratio = handles.CPUTimePerModelValue/ (t/dim);
set(handles.GPUSpeedUp, 'String', sprintf('%.2fx', ratio));

[x, y, M] = getDataMatrix(0,15,out,2);
surf(y, x, M, 'EdgeAlpha', 0, 'Parent', handles.Axes_HNL3);
set(handles.Axes_HNL3, 'YLimMode', 'manual', 'YLim', [0 dim], 'Visible', 'on');
[x, y, M] = getDataMatrix(0,15,out,3);
surf(y, x, M, 'EdgeAlpha', 0, 'Parent', handles.Axes_HNR3);
set(handles.Axes_HNR3, 'YLimMode', 'manual', 'YLim', [0 dim], 'Visible', 'on');
[x, y, M] = getDataMatrix(0,15,out,4);
surf(y, x, M, 'EdgeAlpha', 0, 'Parent', handles.Axes_HNL4);
set(handles.Axes_HNL4, 'YLimMode', 'manual', 'YLim', [0 dim], 'Visible', 'on');
[x, y, M] = getDataMatrix(0,15,out,5);
surf(y, x, M, 'EdgeAlpha', 0, 'Parent', handles.Axes_HNR4);
set(handles.Axes_HNR4, 'YLimMode', 'manual', 'YLim', [0 dim], 'Visible', 'on');
[x, y, M] = getDataMatrix(0,15,out,6);
surf(y, x, M, 'EdgeAlpha', 0, 'Parent', handles.Axes_HNL1);
set(handles.Axes_HNL1, 'YLimMode', 'manual', 'YLim', [0 dim], 'Visible', 'on');
[x, y, M] = getDataMatrix(0,15,out,7);
surf(y, x, M, 'EdgeAlpha', 0, 'Parent', handles.Axes_HNR1);
set(handles.Axes_HNR1, 'YLimMode', 'manual', 'YLim', [0 dim], 'Visible', 'on');
% 
% simplot(out(1),'Parent',handles.GPUAxes);

guidata(hObject, handles);



function CPUStepsEdit_Callback(hObject, eventdata, handles)
% hObject    handle to CPUStepsEdit (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of CPUStepsEdit as text
%        str2double(get(hObject,'String')) returns contents of CPUStepsEdit as a double


% --- Executes during object creation, after setting all properties.
function CPUStepsEdit_CreateFcn(hObject, eventdata, handles)
% hObject    handle to CPUStepsEdit (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function GPUStepsEdit_Callback(hObject, eventdata, handles)
% hObject    handle to GPUStepsEdit (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of GPUStepsEdit as text
%        str2double(get(hObject,'String')) returns contents of GPUStepsEdit as a double


% --- Executes during object creation, after setting all properties.
function GPUStepsEdit_CreateFcn(hObject, eventdata, handles)
% hObject    handle to GPUStepsEdit (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --- Executes during object creation, after setting all properties.
function CPUSimulationTime_CreateFcn(hObject, eventdata, handles)
% hObject    handle to CPUSimulationTime (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called


% --- getDataMatrix
function [x, y, M] = getDataMatrix(starttime, stoptime, out, idx)

num_points = 1000;
steps = length(out);
x = 1:steps;
M = zeros(steps,num_points);
y = linspace(starttime, stoptime, num_points);
for i=1:steps
  d = out(i).Vm;
  try
    M(i,:) = interp1(d(:,1),d(:,idx),y,'nearest');
  catch me
    me
    disp('Interpolation failed');
  end
end



function Status_Callback(hObject, eventdata, handles)
% hObject    handle to Status (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of Status as text
%        str2double(get(hObject,'String')) returns contents of Status as a double


% --- Executes during object creation, after setting all properties.
function Status_CreateFcn(hObject, eventdata, handles)
% hObject    handle to Status (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end

%% createInputSpace - creates a linear input space structure
function input_data = createInputSpace(m, steps, inputs)

input_names = m.input_names;
input_data = m.default_inputs;
fields = fieldnames(inputs);
for i=1:length(fields)
  if any(strcmpi(input_names, fields{i}))
    val = inputs.(fields{i});
    if length(val) == 2
      input_data.(fields{i}) = linspace(val(1), val(2), steps);
    else
      input_data.(fields{i}) = val;
    end
  else
    warning('Simatra:argumentError', ['Invalid input with '...
                        'name ' fields{i}]);
  end
end
