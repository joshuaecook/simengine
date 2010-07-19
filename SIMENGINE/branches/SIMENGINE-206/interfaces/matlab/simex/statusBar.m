function [messageLength] = statusBar(message, previousLength)
try
  dt = javaMethod('getInstance', 'com.mathworks.mde.desk.MLDesktop');
  if dt.hasMainFrame
    dt.setStatusText(message);
  else
    textStatusBar(message, previousLength);
  end
catch it
  textStatusBar(message, previousLength);
end
messageLength = length(message);
end

function textStatusBar(message, previousLength)
    global SIMEX_NO_TEXT_STATUS_BAR
    if(~isempty(SIMEX_NO_TEXT_STATUS_BAR))
      return;
    end

    % Update less frequently than graphical status bar to avoid
    % flickering on a terminal
    pause(0.25);
    
    % Backup over previous message
    for i = 1:previousLength
        fprintf('\b');
    end
    % Wipe the previous message with spaces
    for i = 1:previousLength
        fprintf(' ');
    end
    % Backup over spaces
    for i = 1:previousLength
        fprintf('\b');
    end
    % Print a new message if available
    if ~isempty(message)
      fprintf('%s', message);
    end
end
