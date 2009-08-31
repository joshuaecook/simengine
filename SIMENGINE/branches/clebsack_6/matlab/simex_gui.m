function varargout = simex_gui(varargin)
% SIMEX_GUI M-file for simex_gui.fig
%      SIMEX_GUI, by itself, creates a new SIMEX_GUI or raises the existing
%      singleton*.
%
%      H = SIMEX_GUI returns the handle to a new SIMEX_GUI or the handle to
%      the existing singleton*.
%
%      SIMEX_GUI('CALLBACK',hObject,eventData,handles,...) calls the local
%      function named CALLBACK in SIMEX_GUI.M with the given input arguments.
%
%      SIMEX_GUI('Property','Value',...) creates a new SIMEX_GUI or raises the
%      existing singleton*.  Starting from the left, property value pairs are
%      applied to the GUI before simex_gui_OpeningFcn gets called.  An
%      unrecognized property name or invalid value makes property application
%      stop.  All inputs are passed to simex_gui_OpeningFcn via varargin.
%
%      *See GUI Options on GUIDE's Tools menu.  Choose "GUI allows only one
%      instance to run (singleton)".
%
% See also: GUIDE, GUIDATA, GUIHANDLES

% Edit the above text to modify the response to help simex_gui

% Last Modified by GUIDE v2.5 31-Aug-2009 13:46:51

% Begin initialization code - DO NOT EDIT
gui_Singleton = 1;
gui_State = struct('gui_Name',       mfilename, ...
                   'gui_Singleton',  gui_Singleton, ...
                   'gui_OpeningFcn', @simex_gui_OpeningFcn, ...
                   'gui_OutputFcn',  @simex_gui_OutputFcn, ...
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


% --- Executes just before simex_gui is made visible.
function simex_gui_OpeningFcn(hObject, eventdata, handles, varargin)
% This function has no output args, see OutputFcn.
% hObject    handle to figure
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
% varargin   command line arguments to simex_gui (see VARARGIN)

% Choose default command line output for simex_gui
handles.output = hObject;

% Update handles structure
guidata(hObject, handles);

% UIWAIT makes simex_gui wait for user response (see UIRESUME)
% uiwait(handles.figure1);


% --- Outputs from this function are returned to the command line.
function varargout = simex_gui_OutputFcn(hObject, eventdata, handles) 
% varargout  cell array for returning output args (see VARARGOUT);
% hObject    handle to figure
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Get default command line output from handles structure
varargout{1} = handles.output;



function FileEdit_Callback(hObject, eventdata, handles)
% hObject    handle to FileEdit (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of FileEdit as text
%        str2double(get(hObject,'String')) returns contents of FileEdit as a double

setStatus(handles, 'Compiling ...');
filename = get(hObject, 'String');
if not(exist(filename, 'file'))
  setStatus(handles, ['No such file with the name ' filename])
  return;
end
  
m = simex(filename);
handles.m = m;
set(handles.OutputMenu, 'Value', 1);
set(handles.OutputMenu, 'String', m.output_names);
% Update Input Table
set(handles.InputTable, 'RowName', m.input_names);
data = cell(length(m.input_names),2);
for i=1:length(m.input_names)
    input = m.input_names{i};
    val = m.default_inputs.(input);
    data{i,1} = val;
    data{i,2} = val;
end
set(handles.InputTable, 'Data', data);

% Update State Init Table
set(handles.StateTable, 'RowName', m.state_names);
data = cell(length(m.state_names), 2);
for i=1:length(m.state_names)
  val = m.default_states(i);
  data{i,1} = val;
  data{i,2} = val;
end
set(handles.StateTable, 'Data', data);

% Update Solver
set(handles.ODESolver, 'Value', 1);
solver_choices = get(handles.ODESolver, 'String');
solver_choices{1} = ['Model Specified (' m.metadata.solver ')'];
set(handles.ODESolver, 'String', solver_choices);

setStatus(handles, ['Loaded model <' m.name '>']);


%starttime = str2double(get(handles.StartTimeEdit, 'String'));
%stoptime = str2double(get(handles.StopTimeEdit, 'String'));
%o = simex(get(hObject, 'String'), [starttime stoptime]);
%handles.o = o;
%figure(handles.figure1);
%outputnames = get(handles.OutputMenu, 'String');
%outputname = outputnames{get(handles.OutputMenu, 'Value')};
%simplot(o.(outputname));

% save the date back
guidata(hObject, handles);


% --- Executes during object creation, after setting all properties.
function FileEdit_CreateFcn(hObject, eventdata, handles)
% hObject    handle to FileEdit (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --- Executes on button press in FileBrowseButton.
function FileBrowseButton_Callback(hObject, eventdata, handles)
% hObject    handle to FileBrowseButton (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

[filename, pathname] = uigetfile({'*.dsl','Diesel Files (*.dsl)'}, 'Choose DSL Model File');
file = fullfile(pathname, filename);
if exist(file,'file')
    set(handles.FileEdit, 'String', file);
    FileEdit_Callback(handles.FileEdit, eventdata, handles);
end


function StartTimeEdit_Callback(hObject, eventdata, handles)
% hObject    handle to StartTimeEdit (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of StartTimeEdit as text
%        str2double(get(hObject,'String')) returns contents of StartTimeEdit as a double




% --- Executes during object creation, after setting all properties.
function StartTimeEdit_CreateFcn(hObject, eventdata, handles)
% hObject    handle to StartTimeEdit (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function StopTimeEdit_Callback(hObject, eventdata, handles)
% hObject    handle to StopTimeEdit (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of StopTimeEdit as text
%        str2double(get(hObject,'String')) returns contents of StopTimeEdit as a double



% --- Executes during object creation, after setting all properties.
function StopTimeEdit_CreateFcn(hObject, eventdata, handles)
% hObject    handle to StopTimeEdit (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --- Executes on selection change in OutputMenu.
function OutputMenu_Callback(hObject, eventdata, handles)
% hObject    handle to OutputMenu (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: contents = get(hObject,'String') returns OutputMenu contents as cell array
%        contents{get(hObject,'Value')} returns selected item from OutputMenu

outputnames = get(hObject, 'String');
outputname = outputnames{get(hObject, 'Value')};



% --- Executes during object creation, after setting all properties.
function OutputMenu_CreateFcn(hObject, eventdata, handles)
% hObject    handle to OutputMenu (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: popupmenu controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --- Executes on button press in RunButton.
function RunButton_Callback(hObject, eventdata, handles)
% hObject    handle to RunButton (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

setStatus(handles, 'Running ...');

target = '-cpu';

precisionnum = get(handles.DoubleButton, 'Value');
switch precisionnum
    case 1
        precision = '-double';
    case 0
        precision = '-single';
end

starttime = str2double(get(handles.StartTimeEdit, 'String'));
stoptime = str2double(get(handles.StopTimeEdit, 'String'));

inputs = handles.m.input_names;
input_data = get(handles.InputTable, 'Data');
state_data = get(handles.StateTable, 'Data');

new_inputs = struct();
for i=1:length(inputs)
   new_inputs.(inputs{i}) = input_data{i,2};
end
new_states = zeros(1,length(handles.m.default_states));
for i=1:length(new_states);
  new_states(i) = state_data{i,2};
end

solver_num=get(handles.ODESolver, 'Value');

if solver_num == 1
  start = tic;
    [o, yf, tf] = simex(get(handles.FileEdit, 'String'), [starttime stoptime], ...
                        target, precision, new_inputs, new_states);
    stop = toc(start);
    if isfield(handles, 't')
      set(handles.OutputMenu, 'Value', 1);
      handles=rmfield(handles, 't');
    end
    if isfield(handles, 'y')
      handles=rmfield(handles, 'y');
    end
    handles.o = o;
    set(handles.OutputMenu, 'String', handles.m.output_names);
else
  all_solvers = get(handles.ODESolver, 'String');
  cur_solver = all_solvers{solver_num};  
  start = tic;
    [t, y] = simex(get(handles.FileEdit, 'String'), [starttime stoptime], ...
                   target, precision, new_inputs, new_states, ['-solver=' ...
                        cur_solver]);
    stop = toc(start);    
    if isfield(handles, 'o')
      set(handles.OutputMenu, 'Value', 1);
      handles=rmfield(handles, 'o');            
    end
    handles.t = t;
    handles.y = y;
    set(handles.OutputMenu, 'String', handles.m.state_names);
end
setStatus(handles, sprintf('Finished simulation in %g seconds', stop));
guidata(hObject, handles);


function StepsEdit_Callback(hObject, eventdata, handles)
% hObject    handle to StepsEdit (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of StepsEdit as text
%        str2double(get(hObject,'String')) returns contents of StepsEdit as a double


% --- Executes during object creation, after setting all properties.
function StepsEdit_CreateFcn(hObject, eventdata, handles)
% hObject    handle to StepsEdit (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --- Executes on button press in PlotButton.
function PlotButton_Callback(hObject, eventdata, handles)
% hObject    handle to PlotButton (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

handles

if isfield(handles, 'o')
  figure;
  outputnames = get(handles.OutputMenu, 'String');
  output = outputnames{get(handles.OutputMenu, 'Value')};
  simplot(handles.o.(output));
  title(['Plot of trace ' output ' in model ' handles.m.name]);
elseif isfield(handles, 't')
  figure;
  statenames = get(handles.OutputMenu, 'String');
  statenum = get(handles.OutputMenu, 'Value');
  state = statenames{statenum};
  plot(handles.t, handles.y(:,statenum));
  title(['Plot of trace ' state ' in model ' handles.m.name]);
end

% --- Set status
function setStatus(handles, str)

set(handles.StatusBar, 'String', ['Status: ' str]);


% --- Executes on selection change in TargetMenu.
function TargetMenu_Callback(hObject, eventdata, handles)
% hObject    handle to TargetMenu (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: contents = get(hObject,'String') returns TargetMenu contents as cell array
%        contents{get(hObject,'Value')} returns selected item from TargetMenu


% --- Executes during object creation, after setting all properties.
function TargetMenu_CreateFcn(hObject, eventdata, handles)
% hObject    handle to TargetMenu (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: popupmenu controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --- Executes on button press in PrecisionButton.
function PrecisionButton_Callback(hObject, eventdata, handles)
% hObject    handle to PrecisionButton (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of PrecisionButton


% --- Executes on button press in DoubleButton.
function DoubleButton_Callback(hObject, eventdata, handles)
% hObject    handle to DoubleButton (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of DoubleButton



function OutputNameEdit_Callback(hObject, eventdata, handles)
% hObject    handle to OutputNameEdit (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of OutputNameEdit as text
%        str2double(get(hObject,'String')) returns contents of OutputNameEdit as a double


% --- Executes during object creation, after setting all properties.
function OutputNameEdit_CreateFcn(hObject, eventdata, handles)
% hObject    handle to OutputNameEdit (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --- Executes on button press in SaveWSButton.
function SaveWSButton_Callback(hObject, eventdata, handles)
% hObject    handle to SaveWSButton (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

if isfield(handles, 'o')
  variable = get(handles.OutputNameEdit, 'String');
  assignin('base', variable, handles.o);
  setStatus(handles, ['Wrote output data to ''' variable '''']);
elseif isfield(handles, 'y')
  variable = get(handles.OutputNameEdit, 'String');
  odeoutput.t = handles.t;
  odeoutput.y = handles.y;
  assignin('base', variable, odeoutput);
  setStatus(handles, ['Wrote output data to ''' variable '''']);
else
  setStatus(handles, ['Can''t plot because there is no simulation ' ...
                      'data available']);
end

% --- Executes on selection change in ODESolver.
function ODESolver_Callback(hObject, eventdata, handles)
% hObject    handle to ODESolver (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: contents = get(hObject,'String') returns ODESolver contents as cell array
%        contents{get(hObject,'Value')} returns selected item from ODESolver


% --- Executes during object creation, after setting all properties.
function ODESolver_CreateFcn(hObject, eventdata, handles)
% hObject    handle to ODESolver (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: popupmenu controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --- Executes on button press in AddToPlotBox.
function AddToPlotBox_Callback(hObject, eventdata, handles)
% hObject    handle to AddToPlotBox (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of AddToPlotBox
