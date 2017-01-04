{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source 1.51.09
//  Based on Dos Navigator (C) 1991-99 RIT Research Labs
//
//  This programs is free for commercial and non-commercial use as long as
//  the following conditions are aheared to.
//
//  Copyright remains RIT Research Labs, and as such any Copyright notices
//  in the code are not to be removed. If this package is used in a
//  product, RIT Research Labs should be given attribution as the RIT Research
//  Labs of the parts of the library used. This can be in the form of a textual
//  message at program startup or in documentation (online or textual)
//  provided with the package.
//
//  Redistribution and use in source and binary forms, with or without
//  modification, are permitted provided that the following conditions are
//  met:
//
//  1. Redistributions of source code must retain the copyright
//     notice, this list of conditions and the following disclaimer.
//  2. Redistributions in binary form must reproduce the above copyright
//     notice, this list of conditions and the following disclaimer in the
//     documentation and/or other materials provided with the distribution.
//  3. All advertising materials mentioning features or use of this software
//     must display the following acknowledgement:
//     "Based on Dos Navigator by RIT Research Labs."
//
//  THIS SOFTWARE IS PROVIDED BY RIT RESEARCH LABS "AS IS" AND ANY EXPRESS
//  OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
//  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
//  DISCLAIMED. IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE FOR
//  ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
//  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
//  GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
//  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
//  IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
//  OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
//  ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//
//  The licence and distribution terms for any publically available
//  version or derivative of this code cannot be changed. i.e. this code
//  cannot simply be copied and put under another distribution licence
//  (including the GNU Public Licence).
//
//////////////////////////////////////////////////////////////////////////}
{$I STDEFINE.INC}

Unit U_MyApp;

interface
uses DnUtil, DNApp, Drivers, Gauges, Advance, Advance3, Views, Commands,
     UserMenu, Messages, Startup, xTime, FlPanelX, Macro, Objects;

type
   MyApp = object (TDNApplication)
      procedure HandleEvent(var Event: TEvent); virtual;
      procedure GetEvent(var Event: TEvent); virtual;
      procedure Idle; virtual;
   end;

var
  MyApplication: MyApp;

implementation
uses Dn1;

(*{$I  runcmd.inc}*)

{$IFDEF OS_DOS}
     {Gimly}
procedure PostQuitMessage;
var Event: TEvent;
begin
 w95locked := TRUE;
 Event.What := evCommand; Event.Command := cmQuit;
 PDNApplication(Application)^.HandleCommand(Event);
 if w95locked then
   MyApplication.HandleEvent(Event);
end;
{$ENDIF}

procedure MyApp.GetEvent;
 var W: Word;
     WW: Word;
     PM: PKeyMacros;

const
     MacroPlaying: Boolean = False;
     MacroKey: Integer = 0;
     CurrentMacro: PKeyMacros = nil;

begin
 {$IFDEF OS_DOS}
 if (not w95locked) and w95QuitCheck then PostQuitMessage; {Gimly}
 {$ENDIF}

 inherited GetEvent(Event);
 if MacroPlaying and ((Event.What = evKeyDown) or (Event.What = evNothing)) then
                      begin
                        Event.What := evKeyDown;
                        Event.KeyCode := CurrentMacro^.Keys^[MacroKey];
                        Inc(MacroKey);
                        MacroPlaying := MacroKey < CurrentMacro^.Count;
                      end;
 case Event.What of
  evNothing: if (NeedLocated > 0) and (GetSTime - NeedLocated > 30) then
               begin
                 NeedLocated := 0;
                 Message(Desktop, evCommand, cmDoSendLocated, nil);
               end;
  evKeyDown:
       begin
{          if (Event.KeyCode = kbAltQ) and Desktop^.GetState(sfFocused) then begin OpenSmartpad; ClearEvent(Event) end;}
           if Event.KeyCode = kbNoKey then begin Event.What := evNothing; Exit end else
             if (not MacroPlaying) and (Event.KeyCode = kbShiftIns) and (ShiftState and kbAltShift <> 0)
                and (ShiftState and kbCtrlShift = 0) then
                 begin Event.What := evNothing; ScreenGrabber(Off); Exit end;
           if (Event.ScanCode >= Hi(kbCtrlF1)) and (Event.ScanCode <= Hi(kbCtrlF10))
             and (Pointer(Current) = Pointer(Desktop)) and (ShiftState and 3 <> 0) then
             begin
               if QuickExecExternal(Event.ScanCode - Hi(kbCtrlF1) + 1) then
                begin
                  Event.What := evCommand;
                  Event.Command := cmExecString;
                  Event.InfoPtr := @QuickExecExternalStr;
                end else Event.What := evNothing;
               Exit;
             end;
           if (ShiftState and 7 <> 0) and ((ShiftState and 4 = 0) or (ShiftState and 3 = 0)) and
              (Event.ScanCode >= kbAlt1 shr 8) and (Event.ScanCode <= kbAlt9 shr 8) then
             begin
              WW := Event.ScanCode - (kbAlt1 shr 8);
              if ShiftState and 3 <> 0 then
                begin
                 if KeyMacroses = nil then
                                        begin
                                         New(KeyMacroses, Init(10,10));
                                         for W := 1 to 10 do KeyMacroses^.Insert(nil);
                                        end;
                 if MacroRecord then begin Macrorecord := False; ClearEvent(Event); Exit; end;
                 MacroRecord := True;
                 KeyMacroses^.AtFree(WW);
                 New(PM, Init);
                 KeyMacroses^.AtInsert(WW,PM);
                 CurrentMacro := PM;
                end else if KeyMacroses <> nil then
                begin
                 if MacroRecord then begin Macrorecord := False; ClearEvent(Event); Exit; end;
                 CurrentMacro := KeyMacroses^.At(WW);
                 MacroPlaying := CurrentMacro <> nil;
                 MacroKey := 0;
                end;
               ClearEvent(Event);
             end;
           if (Event.KeyCode = kbAlt0) and (ShiftState and 3 <> 0) then
              begin Event.What := evCommand; Event.Command := cmListOfDirs; Exit; end;
           if MsgActive then
           if Event.KeyCode = kbLeft then Event.KeyCode := kbShiftTab
             else if Event.KeyCode = kbRight then Event.KeyCode := kbTab;
           if (Event.What = evKeyDown) then
            begin
              if MacroRecord and (CurrentMacro <> nil) then
                  CurrentMacro^.PutKey(Event.KeyCode);
              if (StatusLine <> nil) and not SkyVisible then
                 StatusLine^.HandleEvent(Event);
            end;
       end;
 end;
 case Event.What of
     evCommand:
       case Event.Command of
              cmTree,
              cmGetTeam,
              cmHelp,
              cmClearData,
              cmSearchAdvance,
              cmAdvancePortSetup,
              cmNavyLinkSetup,
              cmQuit : HandleCommand(Event);
       end;
end;
end;

var
     L_Tmr: TEventTimer;


procedure MyApp.Idle;

 procedure L_On; begin xTime.NewTimer(L_Tmr, 2) end;
 procedure NLS;  begin xTime.NewTimer(LSliceTimer, 3) end;

var
  Event: TEvent;

begin

  if not FullSpeed then TinySlice;

  if StartupData.Slice and osuSleep <> 0 then
  case LSliceCnt of

    -1 : if xTime.TimerExpired(L_Tmr) then SliceAwake else
         begin
           xTime.NewTimer(L_Tmr, 18*60);
           repeat
             TinySlice;
             GetMouseEvent(Event);
             if Event.What = evNothing then GetKeyEvent(Event);
             if Event.What <> evNothing then
             begin
               Inc(EventsLen);
               EventQueue[EventsLen] := Event;
               Break;
             end;
           until xTime.TimerExpired(L_Tmr);
           L_On;
         end;

    -2 : if xTime.TimerExpired(LSliceTimer) then begin NLS; LSliceCnt := 0 end;
    -3 : begin xTime.NewTimerSecs(LSliceTimer, 5); LSliceCnt := 0 end;

    else if xTime.TimerExpired(LSliceTimer) then
         begin
           if LSliceCnt > 100 then
           begin
             LSliceCnt := -1;
             L_On;
           end else LSLiceCnt := 0;
           NLS;
         end else Inc(LSliceCnt);
  end;

  {  Put IdleEvt after IdleClick Expired  }
    with IdleEvt do
   if What<>evNothing then if xTime.TimerExpired(IdleClick) then
       begin
        PutEvent(IdleEvt);
        if What = evCommand then
         begin
           What := evBroadCast ;
           PutEvent(IdleEvt);
         end;
        ClearEvent(IdleEvt);
       end;

 if not SkyVisible then
 begin
   TApplication.Idle;
 end;

 UpdateAll(On);

 if CtrlWas then
   if ShiftState and kbCtrlShift = 0 then
     begin
       CtrlWas := Off;
       {if DelSpaces(CmdLine.Str) = '' then}
         Message(@Self, evCommand, cmTouchFile, nil);
     end;

  IdleWas := True;

end;

procedure MyApp.HandleEvent;
 var s: word;

 procedure UpView(P: PView);
 begin
   P^.MakeFirst;
   Clock^.MakeFirst;
 end;

begin
 if Event.What = evMouseDown
  then if (Event.Where.Y = 0) and (Event.Buttons and mbLeftButton <> 0)
        then MenuBar^.HandleEvent(Event);
 if Event.What <> evNothing then inherited HandleEvent(Event);
 case Event.What of
   evCommand : case Event.Command of
     cmUpdateConfig: begin UpdateConfig; WriteConfig; end;
     cmMenuOn: if (Event.InfoPtr = MenuBar) then
                 UpView(MenuBar);
     cmMenuOff: if (Event.InfoPtr = MenuBar) then
                 UpView(Desktop);
     {$IFNDEF NONBP}
     cmEnvEdit: begin
                 S:={$IFDEF DPMI}RSeg{$ENDIF}(PWordArray(Ptr(LoaderSeg, 0))^[$2C div 2]);
                 EditDosEvironment(Ptr(S, 0));
                end;
     {$ENDIF}
     else
      HandleCommand(Event);
   end;
 end;
end;

end.
