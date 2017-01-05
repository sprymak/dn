unit killer;

interface
uses
  Os2Def, Os2Base;

procedure SetKillHandler(var RegRec: ExceptionRegistrationRecord);
procedure UnsetKillHandler(var RegRec: ExceptionRegistrationRecord);

const
  fQuit: boolean = false;

implementation
uses
  Drivers
  , dnini, commands, startup, dnexec
  ;

{$S-}
function CtrlRoutine(
    p1: PExceptionReportRecord; p2: PExceptionRegistrationRecord;
    p3: PContextRecord; pv: Pointer): ULong; cdecl;
  var
    rc : ApiRet;
  label
    lCtrlBreak;
  begin
  CtrlRoutine := xcpt_Continue_Search;
  case p1^.ExceptionNum of
    xcpt_Signal:
      begin
      Case p1^.ExceptionInfo[0] of
        xcpt_Signal_Intr:
          CtrlRoutine := xcpt_Continue_Execution;
        xcpt_Signal_KillProc:
          begin
          if SmartWindowsBoxClose = 3 then
            goto lCtrlBreak;
          if SmartWindowsBoxClose = 2 then
            Confirms := Confirms and not cfExitConfirm;
          if SmartWindowsBoxClose >= 1 then
            begin
            fQuit := true;
            CtrlRoutine := xcpt_Continue_Execution;
            end;
          end;
        xcpt_Signal_Break:
          begin
          CtrlRoutine := xcpt_Continue_Execution;
          if CtrlBreakKill and not fExec then
            begin
lCtrlBreak:
            rc := DosUnsetExceptionHandler(p2^);
            if rc = No_Error then
              halt(-1);
            end;
          end;
      end {case}
      end;
  end {case};
  end;

procedure SetKillHandler(var RegRec: ExceptionRegistrationRecord);
  var
    rc : ApiRet;
  begin
  FillChar(RegRec, SizeOf(RegRec), 0);
  RegRec.ExceptionHandler := CtrlRoutine;
  rc := DosSetExceptionHandler(RegRec);
  if rc <> No_Error then
    Writeln('DosSetExceptionHandler error: return code = ',rc);
  end;

procedure UnsetKillHandler(var RegRec: ExceptionRegistrationRecord);
  var
    rc : ApiRet;
  begin
  rc := DosUnsetExceptionHandler(RegRec);
  end;

end.
