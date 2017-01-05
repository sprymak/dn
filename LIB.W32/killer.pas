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
  , VPSysLow, VideoMan, DNApp
  ;

function CtrlRoutine(dwCtrlType: DWORD): boolean;
  var
    RC: longint;
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
      if fExec then
        begin
{AK155 �� ��砩, ����� DN �� �㬥� �ᯮ�����, �� �맢����� ������
�ணࠬ�� �믮����� � �⤥�쭮� ����, � �襫 � �������� � ��� �࠭��,
������� ����������� "ࠧ�㤨��" DN �� ����� Ctrl-Break. ��࠭� ������
�� ᮡ�⨥, ��᪮��� ��� �ᯮ������� � � Win9x. }
        FreeConsole;
        AllocConsole;
        SetKillHandler;
        TerminateProcess(hExtCmd, RC);
        DoneVideo;
        InitVideo;
        InitialiseKeyboardHandler;
        Application^.Redraw;
        end
      else if CtrlBreakKill then
lCtrlBreak:
        Halt(-1);
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
