unit killer;

interface

procedure SetKillHandler;
procedure UnsetKillHandler;

const
  fQuit: boolean = false;

implementation
uses
  Windows,
  dnini, commands, startup, dnexec
  ;

function CtrlRoutine(dwCtrlType: DWORD): boolean;
  label
    lCtrlBreak;
  begin
  CtrlRoutine := false;
  case dwCtrlType of
    CTRL_CLOSE_EVENT:
      begin
      if SmartWindowsBoxClose = 3 then
        goto lCtrlBreak;
      if SmartWindowsBoxClose = 2 then
        Confirms := Confirms and not cfExitConfirm;
      if SmartWindowsBoxClose >= 1 then
        begin
        fQuit := true;
        CtrlRoutine := true;
        end;
      end;
    CTRL_LOGOFF_EVENT, CTRL_SHUTDOWN_EVENT:
      Halt(-1);
    CTRL_BREAK_EVENT:
      begin
      if CtrlBreakKill and not fExec then
lCtrlBreak: Halt(-1);
      CtrlRoutine := true;
      end;
  end {case};
  end;


procedure SetKillHandler;
  var
    ierr: longint;
  begin
  if not SetConsoleCtrlHandler(
      @CtrlRoutine,  // address of handler function
      true     // handler to add or remove
             )
  then ierr := GetLastError;
  end;

procedure UnsetKillHandler;
  var
    ierr: longint;
  begin
  if not SetConsoleCtrlHandler(
      @CtrlRoutine,  // address of handler function
      false     // handler to add or remove
             )
  then ierr := GetLastError;
  end;
end.
