function varargout = simview(varargin)
% SIMVIEW M-file for simview.fig
%      SIMVIEW, by itself, creates a new SIMVIEW or raises the existing
%      singleton*.
%
%      H = SIMVIEW returns the handle to a new SIMVIEW or the handle to
%      the existing singleton*.
%
%      SIMVIEW('CALLBACK',hObject,eventData,handles,...) calls the local
%      function named CALLBACK in SIMVIEW.M with the given input arguments.
%
%      SIMVIEW('Property','Value',...) creates a new SIMVIEW or raises the
%      existing singleton*.  Starting from the left, property value pairs are
%      applied to the GUI before simview_OpeningFcn gets called.  An
%      unrecognized property name or invalid value makes property application
%      stop.  All inputs are passed to simview_OpeningFcn via varargin.
%
%      *See GUI Options on GUIDE's Tools menu.  Choose "GUI allows only one
%      instance to run (singleton)".
%
% See also: GUIDE, GUIDATA, GUIHANDLES

% Edit the above text to modify the response to help simview

% Last Modified by GUIDE v2.5 25-Aug-2009 18:18:52

% Begin initialization code - DO NOT EDIT
gui_Singleton = 1;
gui_State = struct('gui_Name',       mfilename, ...
                   'gui_Singleton',  gui_Singleton, ...
                   'gui_OpeningFcn', @simview_OpeningFcn, ...
                   'gui_OutputFcn',  @simview_OutputFcn, ...
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


% --- Executes just before simview is made visible.
function simview_OpeningFcn(hObject, eventdata, handles, varargin)
% This function has no output args, see OutputFcn.
% hObject    handle to figure
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
% varargin   command line arguments to simview (see VARARGIN)

% Choose default command line output for simview
handles.output = hObject;

% Update handles structure
guidata(hObject, handles);

% UIWAIT makes simview wait for user response (see UIRESUME)
% uiwait(handles.figure1);


% --- Outputs from this function are returned to the command line.
function varargout = simview_OutputFcn(hObject, eventdata, handles) 
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
m = simex(get(hObject, 'String'))
handles.m = m;
set(handles.OutputMenu, 'String', m.output_names);
set(handles.InputTable, 'RowName', m.input_names);
data = cell(length(m.input_names),3);
for i=1:length(m.input_names)
    input = m.input_names{i};
    val = m.default_inputs.(input);
    data{i,1} = val;
    data{i,2} = val;
    data{i,3} = val;
end
set(handles.InputTable, 'Data', data);
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

if isfield(handles, 'o')
   if isfield(handles.o, outputname) 
       figure(handles.figure1);
       simplot(handles.o.(outputname));
   end
end


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

targetnum = get(handles.TargetMenu, 'Value');
switch targetnum
    case 1
        target = '-cpu';
    case 2
        target = '-openmp';
    case 3
        target = '-gpu';
end

precisionnum = get(handles.DoubleButton, 'Value');
switch precisionnum
    case 1
        precision = '-double';
    case 0
        precision = '-single';
end

starttime = str2double(get(handles.StartTimeEdit, 'String'));
stoptime = str2double(get(handles.StopTimeEdit, 'String'));
steps = str2double(get(handles.StepsEdit, 'String'));
inputs = handles.m.input_names;
input_data = get(handles.InputTable, 'Data');
new_inputs = struct();
for i=1:length(inputs)
   low = input_data{i,2};
   high = input_data{i,3};
   new_inputs.(inputs{i}) = linspace(low, high, steps);
end
t = tic;
o = simex(get(handles.FileEdit, 'String'), [starttime stoptime], target, precision, new_inputs);
stoptime = toc(t);
setStatus(handles, sprintf('Finished simulation in %g seconds', stoptime));
handles.o = o;
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


% --- Executes on button press in SurfaceButton.
function SurfaceButton_Callback(hObject, eventdata, handles)
% hObject    handle to SurfaceButton (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

num_points = 1000;
if isfield(handles, 'o')
    figure;
    steps = length(handles.o);
    s = zeros(steps, num_points);
    outputnames = get(handles.OutputMenu, 'String');
    output = outputnames{get(handles.OutputMenu, 'Value')};
    starttime = str2double(get(handles.StartTimeEdit, 'String'));
    stoptime = str2double(get(handles.StopTimeEdit, 'String'));
    x1 = linspace(starttime, stoptime, num_points);
    for i=1:steps
        d = handles.o(i).(output);
        s(i, :) = spline(d(:,1), d(:,2), x1);
    end
    surf(s, 'EdgeAlpha', 0);
end


% --- Executes on button press in CycleButton.
function CycleButton_Callback(hObject, eventdata, handles)
% hObject    handle to CycleButton (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

num_points = 1000;
if isfield(handles, 'o')
    figure;
    steps = length(handles.o);
    s = zeros(steps, num_points);
    outputnames = get(handles.OutputMenu, 'String');
    output = outputnames{get(handles.OutputMenu, 'Value')};
    starttime = str2double(get(handles.StartTimeEdit, 'String'));
    stoptime = str2double(get(handles.StopTimeEdit, 'String'));
    x1 = linspace(starttime, stoptime, num_points);
    for i=1:steps
        d = handles.o(i).(output);
        s(i, :) = spline(d(:,1), d(:,2), x1);
    end
    maxy = max(max(s));
    miny = min(min(s));
    range = maxy-miny;
    plotmin = miny - (range * 0.1);
    plotmax = maxy + (range * 0.1);
    for i=1:steps
        d = handles.o(i).(output);
        s(i, :) = spline(d(:,1), d(:,2), x1);
        plot(x1, s(i,:));
        axis([starttime stoptime plotmin plotmax]);
        pause(0.05);
    end

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
