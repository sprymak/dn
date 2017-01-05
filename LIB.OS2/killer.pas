unit Killer;

interface
uses
  Os2Def, Os2Base;

procedure SetKillHandler(var RegRec: ExceptionRegistrationRecord);
procedure UnsetKillHandler(var RegRec: ExceptionRegistrationRecord);

const
  fQuit: boolean = False;

implementation
uses
  Drivers
  , DnIni, Commands, Startup, DnExec
  ;

{$S-}
function CtrlRoutine(
  P1: PExceptionReportRecord; P2: PExceptionRegistrationRecord;
  p3: PContextRecord; PV: Pointer): ULong;
  cdecl;
  var
    rc: apiret;
  label
    lCtrlBreak;
  begin
    CtrlRoutine := xcpt_Continue_Search;
    case P1^.ExceptionNum of
      xcpt_Signal:
        begin
          case P1^.ExceptionInfo[0] of
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
                    fQuit := True;
                    CtrlRoutine := xcpt_Continue_Execution;
                  end;
              end;
            xcpt_Signal_Break:
              begin
                CtrlRoutine := xcpt_Continue_Execution;
                if CtrlBreakKill and not fExec then
                  begin
lCtrlBreak:
                    rc := DosUnsetExceptionHandler(P2^);
                    if rc = NO_ERROR then
                      Halt(-1);
                  end;
              end;
          end {case}
        end;
    end {case};
  end { CtrlRoutine };

procedure SetKillHandler(var RegRec: ExceptionRegistrationRecord);
  var
    rc: apiret;
  begin
    FillChar(RegRec, SizeOf(RegRec), 0);
    RegRec.ExceptionHandler := CtrlRoutine;
    rc := DosSetExceptionHandler(RegRec);
    if rc <> NO_ERROR then
      Writeln('DosSetExceptionHandler error: return code = ', rc);
  end;

procedure UnsetKillHandler(var RegRec: ExceptionRegistrationRecord);
  var
    rc: apiret;
  begin
    rc := DosUnsetExceptionHandler(RegRec);
  end;

end.
