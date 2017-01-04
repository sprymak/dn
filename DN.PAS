{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source 1.51.05/DOS
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
//////////////////////////////////////////////////////////////////////////
//
//      LFNизация выполнена Антоном Федоpовым aka DataCompBoy'ем.
//                  В честь Матаpыкиной Ульяны...
//////////////////////////////////////////////////////////////////////////}

program DN;

{$M 25600,0,655350}
{$I STDEFINE.INC}
{$I DN.DEF}
{.$DEFINE GRABPalette}
{$IFNDEF DPMI}{$i overlay.def}{$ENDIF}
uses ExtKBD, {DataCompBoy}
     LFN,    {DataCompBoy}
     LFNCol, {DataCompBoy}

     DN1,{DataCompBoy}

{$IFDEF Debug} DEBUG, {$ENDIF}

{$IFNDEF DPMI}
{$ifdef packoverlay}
     DnOverl, {DataCompBoy}
{$else}
     Overlay,
{$endif}
     Overlays,
{$ENDIF}{DPMI}
     Dos,

     UFNMatch,

     Drivers,
     Objects,
     Views,
     Scroller,
     DNApp,
     Menus,
     FlPanelX,
     FlPanel,
     FileCopy,
     Eraser,
     DiskInfo,
     advance,
     advance1,
     advance2,
     advance3,
     advance4,
     Startup,
     Dialogs,
     Gauges,
     Memory,
     DblWnd,
     Messages,
     HistList,
     FileFind,
     Commands,
     Tree,
     FViewer,
     CmdLine,
     FBB,
     DNStdDlg,
     MemInfo,
     FilesCol,
     UserMenu,
     RStrings,
     ColorSel,
     ColorVGA,
     Colors,
     Microed,
     Macro,
     DNFormat,
     Format,

{$ifdef CdPlayer}
     CdPlayer,
{$endif}

{$IFDEF SpreadSheet}
     Calc,
     CellsCol,
{$ENDIF}

{$IFDEF DEMO}
     Demo,
{$ENDIF}

{$IFDEF DBView}
     DBView,
     DBWatch,
{$ENDIF}

     Reanimator,
     Fixer,
     ArcView,
     HelpKernel,
     HelpFile,
     Validate,

{$IFDEF Modem}
     NavyLink,
     ModemIO,
     Terminal,
     apUART,
     apPort,
{$ENDIF}
     Phones,
     ASCIITab,
     xTime,

{$IFDEF Game}
     Tetris,
{$ENDIF}
     Drives,
     Archiver,
     Idlers,
     Editor,

{$IFDEF PrintManager}
     PrintManager,
{$ENDIF}
     Setups,
     ExtraMemory,
     DNUtil,
     XDblWnd,
     Histries,
     CCalc,
     Novell,
     DnIni,
     DrvTypes; {-$VIV}

{$IFNDEF DPMI}

  {!$O DNApp}
  {!$O Drivers}
  {!$O Overlays}
  {!$O ExtraMemory}
  {!$O Startup}

  {$O DN1}

{$IFDEF FULLOVR}
  {$O RStrings}
  {$O UniWin}
  {$O FlPanelX}
  {$O FlPanel}
  {$O XDBlWnd}
  {$O FStorage}
  {$O Menus}
  {$O Drives}
  {$O DblWnd}
  {$O CmdLine}
  {$O Gauge}
  {$O Gauges}
{$ENDIF}

  {$O UFNMATCH}
  {$O XTIME}
  {$O U_INI}
  {$O U_SRCHF}
  {$O NETINFO}
  {$O HistList}
  {$O Histries}
  {$O FlTools}
  {$O Scroller}
  {$O DiskTool}
  {$O HideView}
  {$O Setups}
  {$O FViewer}
  {$O MicroEd}
  {$O Macro}
  {$O FileCopy}
  {$O DiskInfo}
  {$O Eraser}
  {$O FileFind}
  {$O Dialogs}
  {$O DNUtil}
  {$O Editor}
  {$O DNFormat}
  {$O Format}
  {$O Par}
  {$O CCalc}
  {$O Reanimator}
  {$O ArcView}
  {$O Archiver}
  {$O ASCIITab}
  {$O Phones}
  {$O DNStdDlg}
  {$O SBlocks}
  {$O FBB}
  {$O Tree}
  {$O Messages}
  {$O MemInfo}
  {$O ColorSel}
  {$O Colors}
  {$O UserMenu}
  {$O Validate}
  {$O HelpFile}
  {$O HelpKernel}
  {$O Idlers}
  {$O UUCode}
  {$O Arvid}
  {$O ColorVGA}
  {$O WinClp}
  {$O DiskImg}
  {$O Novell}
(*{$O advance1}
  {$O advance2}
  {$O advance3}*)
  {$O advance4}
  {$O DRVTYPES}
  {$O DNIni}

{$IFDEF Game}
  {$O Tetris}
{$ENDIF}
{$IFDEF PrintManager}
  {$O PrintManager}
{$ENDIF}
{$IFDEF DiskEdit}
  {$O Fixer}
  {$O SWE}
{$ENDIF}
{$IFDEF CDPLAYER}
  {$O CdPlayer}
  {$O CdUtil}
{$ENDIF}
{$IFDEF DBView}
  {$O DBView}
  {$O DBWatch}
{$ENDIF}
{$IFDEF SpreadSheet}
  {$O Calc}
  {$O CellsCol}
{$ENDIF}
{$IFDEF Modem}
  {$O ApFossil}
  {$O ApInt14}
  {$O ApMisc}
  {$O ModemIO}
  {$O Terminal}
  {$O ScrollBk}
  {$O OOCOM}
  {$O OOAbsPcl}
  {$O OOXmodem}
  {$O OOYmodem}
  {$O OOZmodem}
  {$O OOKermit}
  {$O NavyLink}
  {$O LinkUtil}
  {$O ComLnk}
{$ENDIF}
{$IFDEF VIEWSOVR}
  {$O VIEWS}
{$ENDIF}

{$ENDIF}{DPMI}

{$I Version.Inc}
{$I  runcmd.inc}

type
   MyApp = object (TDNApplication)
      procedure HandleEvent(var Event: TEvent); virtual;
      procedure GetEvent(var Event: TEvent); virtual;
      procedure Idle; virtual;
   end;

var
  MyApplication: MyApp;

procedure MyApp.GetEvent;
 var W: Word;
     WW: Word;
     PM: PKeyMacros;

const
     MacroPlaying: Boolean = False;
     MacroKey: Integer = 0;
     CurrentMacro: PKeyMacros = nil;

begin
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
           if (Event.KeyCode = kbAltQ) and Desktop^.GetState(sfFocused) then begin OpenSmartpad; ClearEvent(Event) end;
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
                  QuickExecExternalStr := FreeStr;
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

procedure GiveSlice; assembler; {Gives up remainder of clock cycle }
asm                             { under dos, windows, os/2 }
  cmp   ops, opUnk
  jne   @Chk
  call  CheckOS

@Chk:
  cmp   ops, opDOS   { Compare to DOS }
  je    @MSDOS
  cmp   ops, opDV    { Compare to Desqview }
  je    @DESQView
  cmp   ops, opWin   { Compare to Windows }
  je    @VM
  cmp   ops, opOS2   { Compare OS/2 }
  je    @VM

  jmp   @NONE        { None found, Jump to End }

@MSDOS:
  int   28h          { Interupt 28h }
  jmp   @NONE        { Jump to the end }

@DESQView:
  mov   ax,1000h     { AX = 1000h }
  int   15h          { Call Interupt 15h }
  jmp   @NONE        { Jump to the end }

@VM:
  mov   ax,1680h     { AX = 1680h }
  int   2Fh          { Call Interupt 2Fh for Win-OS/2 TimeSlice }

@NONE:

end; {GiveSlice}


procedure TinySlice;
var Released: boolean;
begin
 Released:=false;

 if (StartupData.Slice  and osuInt15 <> 0) and
    (StartupData.Slice2 and osuOnceRelease <> 0) then
  asm
   cmp DESQDetected, 0
   je  @@1
   mov al, 1
   mov Released, al
   mov ax, $1000
   int 15h
 @@1:
  end;

 if (StartupData.Slice and osuInt2F <> 0) and
    (StartupData.Slice2 and osuOnceRelease <> 0) then
  asm
   mov ax, 1680h
   int 2Fh
   or al, al
   je @@e
   mov al, 1
   mov Released, al
@@e:
  end;

 if StartupData.Slice and osuInt28 <> 0 then
  asm int 28h end;
end;

var
     L_Tmr: TEventTimer;


procedure MyApp.Idle;

 procedure L_On; begin NewTimer(L_Tmr, 2) end;
 procedure NLS;  begin NewTimer(LSliceTimer, 3) end;

var
  Event: TEvent;

begin

  if not FullSpeed then TinySlice;

  if StartupData.Slice and osuSleep <> 0 then
  case LSliceCnt of

    -1 : if TimerExpired(L_Tmr) then SliceAwake else
         begin
           NewTimer(L_Tmr, 18*60);
           repeat
             GiveSlice;
             TinySlice;
             GetMouseEvent(Event);
             if Event.What = evNothing then GetKeyEvent(Event);
             if Event.What <> evNothing then
             begin
               Inc(EventsLen);
               EventQueue[EventsLen] := Event;
               Break;
             end;
           until TimerExpired(L_Tmr);
           L_On;
         end;

    -2 : if TimerExpired(LSliceTimer) then begin NLS; LSliceCnt := 0 end;
    -3 : begin NewTimerSecs(LSliceTimer, 5); LSliceCnt := 0 end;

    else if TimerExpired(LSliceTimer) then
         begin
           if LSliceCnt > 100 then
           begin
             LSliceCnt := -1;
             L_On;
           end else LSLiceCnt := 0;
           NLS;
         end else Inc(LSliceCnt);
  end;

  {$IFDEF DEMO}
  if Demos <> nil then
    begin
      Demos^.Update;
      if Demos = nil then Dec(SkyEnabled);
    end;
  {$ENDIF}


  {  Put IdleEvt after IdleClick Expired  }
    with IdleEvt do
   if What<>evNothing then if TimerExpired(IdleClick) then
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
   if mem[$40:$17] and kbCtrlShift = 0 then
     begin
       CtrlWas := Off;
       {if DelSpaces(CmdLine.Str) = '' then}
         Message(@Self, evCommand, cmTouchFile, nil);
     end;

  IdleWas := True;

end;

procedure MyApp.HandleEvent;

 procedure UpView(P: PView);
 begin
   P^.MakeFirst;
   Clock^.MakeFirst;
 end;

begin
 if Event.What = evMouseDown then
  if (Event.Where.Y = 0) and (Event.Buttons and mbLeftButton <> 0) then
                   begin
                     MenuBar^.HandleEvent(Event);
                   end;
 if Event.What <> evNothing then inherited HandleEvent(Event);
 case Event.What of
   evCommand : case Event.Command of
     cmUpdateConfig: begin UpdateConfig; WriteConfig; end;
     cmMenuOn: if (Event.InfoPtr = MenuBar) then
                 UpView(MenuBar);
     cmMenuOff: if (Event.InfoPtr = MenuBar) then
                 UpView(Desktop);
     cmEnvEdit: {if LoaderSeg <> 0 then }EditDosEvironment(Ptr(PWordArray(Ptr(LoaderSeg, 0))^[$2C div 2], 0));
     else
     {
     cmCalculator,
     cmPutInClipboard,
     cmGetFromClipboard,
     cmListOfDirs,
     cmFormatDisk,
     cmFilePrint,
     cmRSearchAdvance,
     cmSetupPrinter,
     cmOpenSmartpad,
     cmOpenClipBoard,
     cmExecGrabber,
     cmTerminalDefaults,
     cmSetupConfirmation,
     cmCountrySetup,
     cmColumnDefaultsDisk,
     cmColumnDefaultsTemp,
     cmColumnDefaultsFind,
     cmColumnDefaultsArvd,
     cmColumnDefaultsArch,
     cmInsertLink, cmDiskCopy,
     cmHighlightGroups,
     cmDBFView,
     cmNewTable,
     cmSheetLoad,
     cmWKZView,
     cmPhoneBook,
     cmUndial,
     cmSetupModem,
     cmTerminal,
     cmHangUp,
     cmDiskEdit,
     cmTextView,
     cmHexView,
     cmSystemSetup,
     cmInterfaceSetup,
     cmStartup,
     cmSetupMouse,
     cmSaversSetup,
     cmReanimator,
     cmSetVolumeLabel,
     cmXEditFile,
     cmWindowManager,
     cmUserMenu,
     cmGlobalUserMenu,
     cmReadArchive,
     cmScreenRest,
     cmASCIITable,
     cmAbout,
     cmRefresh,
     cmClearDesktop,
     cmFileView,
     cmFileEdit,
     cmIntFileView,
     cmIntFileEdit,
     cmExtFileEdit,
     cmMenuFileEdit,
     cmLocalMenuFileEdit,
     cmExternalViewers,
     cmExternalEditors,
     cmShowUserScreen,
     cmRenameKeyFile,
     cmRFormat,
     cmRestart, (*DataCompBoy*)
     cmCreatePanel,
     cmCreateTree,
     cmFindFile,
     cmFormatDisk,
     cmSystemInfo,
     cmMemoryInfo,
     cmShowOutput,
     cmHistoryList,
     cmLoadDesk,
     cmSaveDesk,
     cmExecFile,
     cmExecString,
     cmExecCommandLine,
     cmChangeColors,
     cmChangeUserMode1,
     cmChangeUserMode2,
     cmKnormal..cmLoadKey,
     cmCancelIdleClick,
     cmHideShowTools,
     cmShowAbout,
     cmShowRegAbout,
     cmChangeMode,
     cmLoadColors,
     cmSetupArchive,
     cmConfigARJ..cmConfigLastArchive,
     cmFMDefaults,
     cmFMSetup,
     cmEditorDefaults,
     cmStoreColors,
     cmPlayCD,
     cmEditHGL,
     cmExecuteDOScmd,
     cmFirstTimePanel,
     cmGame:} HandleCommand(Event);
   end;
 end;
end;

procedure CrLf; assembler;
asm
  MOV  DL,0DH
  MOV  AH,2
  INT  21H
  MOV  DL,0AH
  MOV  AH,2
  INT  21H
end;

var
  ShiftRec: record ScrH, CurY: Byte end absolute FreeStr; { just to reduce DS }

var Ev: TEvent;

begin
 if memAvail < 100000 then begin
    WriteLn(#10#13'Not enough memory for Navigator.');
    WriteLn(      'Please check if 400K memory is available');
    Halt(203);
 end;
 Randomize;

 LoaderSeg := 0;
 LSliceCnt := -3;
 asm
  mov ax, $9900
  int 2Fh
  cmp bx, 'DN'
  jne @@1
  mov DNNumber, Al
  mov RunFirst, Ah
  xor ax, ax
  mov es, ax
  mov ax, $9901
  int 2Fh
  mov ax, es
  mov LoaderSeg, ax
  mov CommandOfs, bx
 @@1:
  mov ah,1
  xor bl,bl
  mov cx, $0607
  push bp
  int 10h
  pop bp
  mov ax, $9905
  xor dx, dx
  xor cx, cx
  push bp
  int  2Fh
  mov  word ptr DDTimer, dx
  mov  word ptr DDTimer+2, cx
  pop  bp
 end;

 if DDTimer > 0 then DDTimer := Get100s-DDTimer;

 TempBounds.Assign(0,0,0,0);

 DoStartUp;

 asm
   MOV  AH, 03 { Get cursor position }
   MOV  BX, 0  { DL - X }
   INT  10H    { DH - Y }
   OR   DL,DL
   JE   @Pass
   CALL CrLf   { if WhereX <> 0 then WriteLn }
   MOV  AH, 03
   MOV  BX, 0
   INT  10H
 @Pass:
   MOV  ShiftRec.CurY,DH
   CALL GetCrtMode { DL - ScreenHeight }
   MOV  ShiftRec.ScrH,DL
 end;

 with ShiftRec do begin
   If ( CurY = ScrH ) and ( InterfaceData.Options and ouiHideStatus = 0 ) then begin
     CrLf;
     asm
       XOR  DX,DX
       MOV  DH,ShiftRec.CurY
       DEC  DH
       MOV  AH,2
       XOR  BX,BX
       INT  10H
     end
   end
 end;

{$IFDEF DEBUG} DEBUG.Flash( $4E64 ); {$ENDIF}
 SetBlink(CurrentBlink);

 NoSound;
 MyApplication.Init;

 if RunFirst then
   begin
     if (StartupData.Load and osuKillHistory <> 0) then ClearHistories;
   end;

{$IFDEF DEBUG} DEBUG.Flash( $1F64 ); {$ENDIF}

 if not RunFirst then EraseFile(SwpDir+'DN'+ItoS(DNNumber)+'.SWP');

 If RunFirst then
   begin
    if (Message( @MyApplication, evBroadcast, cmLookForPanels, NIL ) = NIL)
       then Message( @MyApplication, evCommand, cmFirstTimePanel, NIL );

    FreeStr[1] := Char(FindParam('/P'));
    if (FreeStr[1] > #0) then LoadPalFromFile(Copy(ParamStr(Byte(FreeStr[1])), 3, 255));
   end;

 if FadeDelay >0 then GlowPalette;{Knave}

{$IFDEF GRABPalette}
GrabPalette;
{$ENDIF}
 if DDTimer > 0 then
  begin
   Ev.What := evCommand;
   Ev.Command := cmShowTimeInfo;
   MyApplication.PutEvent(Ev);
  end;

 if not IgnoreOldFiles then CheckForOldFiles;

 MyApplication.Run;
 GlobalMessage(evCommand, cmKillUsed, nil);
 TottalExit := On;
 MyApplication.Done;

END.