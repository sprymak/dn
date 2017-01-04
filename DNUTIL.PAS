{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source
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
//  Version history:
//
//  1.6.RC1
//  dn16rc1-vwr_ext_edt_incorrect_mask_bugfix.patch
//  dn16rc1-pkview_some_dialogs_resources_fix.patch
//
//  2.0.0
//
//////////////////////////////////////////////////////////////////////////}
{$I STDEFINE.INC}

unit DNUtil;

interface
uses Gauges, DnApp, xTime, Views, Drivers, Objects, Tree;

const
  RunMenu: Boolean = False;
  IdleCounter: Word = 0;
  StartTicks: Word = 0;
  NullStr: byte=0;

var
  LoaderSeg: Word;
  CommandOfs: Word;
  RunFirst: Boolean;
  IgnoreOldFiles: Boolean;

type
     PDNAppl = ^TDNAppl;
     TDNAppl = object(TApplication)
        IdleClick : TEventTimer ;
        IdleEvt   : TEvent ;
        TreeReader: PTreeReader;

        Pk1,Pk2,Pk3,Pk4 : PView ;

        procedure ViewFile(Intern: integer; FileName: String); {DataCompBoy} (* X-Man *)
        procedure ViewFilter(FileName: String); {DataCompBoy}
        procedure EditFile(Intern: Boolean; FileName: String); {DataCompBoy}
        procedure OutOfMemory; virtual;
        procedure EventError(var Event: TEvent); virtual;
        procedure HandleCommand(var Event: TEvent);
     end;

function  CheckExit: Boolean;
function  PresentFile(Name: String): PStream; {DataCompBoy}
{$IFDEF OS_DOS}
procedure w95QuitInit;     {Gimly}
function  w95QuitCheck:boolean; {Gimly}
procedure w95QuitCancel;
{$ENDIF}

const w95locked:boolean=FALSE; {Gimly}


implementation
uses Advance2, Commands, Idlers, Startup, Messages, FilesCol, Histries, Dnexec,
     FViewer, Advance1, Dos, LFN, Advance, DnIni, Arcview,
     Microed, EdWin, DnUtil2, usermenu, dnsvld, colors, archread,
     asciitab, ccalc, filefind, videoman, archset, archiver, dnhelp, diskinfo,
     helpfile,Setups
     {$IFDEF DiskFormat},DnFormat{$ENDIF}
     {$IFDEF SpreadSheet},Calc{$ENDIF}
     {$IFDEF DBView},DbView{$ENDIF}
     {$IFDEF PKTVIEW},PKTView{$ENDIF}
     {$IFDEF DPMI},DPMI{$ENDIF}
     {$IFDEF Game},Tetris{$ENDIF}
     {$IFDEF CDPlayer},CdPlayer{$ENDIF}
     {$IFDEF SysInfo},SysInfo{$ENDIF}
     {$IFDEF MemInfo},MemInfo{$ENDIF}
     {$IFDEF Calendar},Calendar{$ENDIF}
     {$IFDEF PRINTMANAGER},PrintManager{$ENDIF}
     {$IFDEF PHONES},Phones{$ENDIF}
     {$IFDEF MODEM},Terminal,modemio
       {$IFDEF PHONES},UDialer{$ENDIF}
       {$IFDEF LINK},NavyLink{$ENDIF}{$ENDIF}
     {$IFDEF DISKEDIT},DiskTool,Fixer,Reanimat,Eraser{$ENDIF}
     ;

        {-DataCompBoy-}
function PresentFile(Name: String): PStream;
 var S: PStream;
begin
 if ExistFile(Name) then
  begin
   S := New(PBufStream, Init(Name, stOpenRead, 2048));
   if S^.Status <> stOK then begin Dispose(S,Done); S:=nil; end
  end
 else s:=nil;
 PresentFile := S;
end;
        {-DataCompBoy-}

function CheckExit: Boolean;
 var Event: TEvent;
 function FindQuit(P: PView): Boolean; {$IFDEF BIT_16}far;{$ENDIF}
 begin
  P^.HandleEvent(Event);
  FindQuit := (Event.What = evNothing) or not P^.Valid(cmQuit);
 end;
begin
{ if FormatWindow <> nil then begin CheckExit := False; Exit; end;}
 Event.What := evCommand; Event.Command := cmQuit;
 CheckExit := Desktop^.FirstThat(@FindQuit) = nil;
end;

procedure TDNAppl.EventError;
begin
{$IFDEF SS}
 if (Event.What = evMouseMove) and (Event.Where.Y = 0) and (Event.Where.X = Size.X - 1)
    and (SSaver = nil) and SaversData.Mouse then InsertIdler;
{$ENDIF}
end;

procedure TDNAppl.OutOfMemory;
begin
  Msg(erNotEnoughMemory, nil, mfError + mfOKButton);
end;

(* X-Man >>> *)
procedure StoreCallback(var Param);
begin
    StoreExtViewer(string(Param))
end;
(* X-Man <<< *)

        {-DataCompBoy-}
procedure TDNAppl.ViewFile;
var
  XT: String;
  W : PWindow;
  R : TRect;
  fr: PFileRec;
  up: tUserParams;
  P : PViewRecord;
  I : Integer;
  PS: PString;
  q : Integer;
  RN: String;

  function View: Boolean;
  begin
   if RN=FileName                                                        (* X-Man *)
   then View:=ExecExtFile('DN.VWR',@up,dlLoadingViewer,StoreCallback,RN) (* X-Man *)
   else View:=ExecExtFile('DN.VWR',@up,dlLoadingViewer,nil,RN);          (* X-Man *)
  end;

  procedure InternalView;
   var S: String;
       V: PFileViewer;
  begin
    W := New(PFileWindow,Init(FileName, RN, ViewerDefaults.Options and vbfHex <> 0));
    if (W <> nil) and (W^.Valid(cmValid)) then
      begin
        V := PFileViewer(W^.Current); S := V^.VFileName; System.Insert(' ', S, 1); UpStr(S);
        PS := @S;
        if (InterfaceData.Options and ouiTrackViewers <> 0) and (ViewHistory <> nil)
          then I := ViewHistory^.IndexOf(@PS) else I := -1;
        If I>=0 then if (PViewRecord(ViewHistory^.At(I))^.fViewMode and not vmInternal)>vmAsm then I:=-1;
        if I >= 0 then
           begin
             P := ViewHistory^.At(I);
             R.Assign(P^.fOrigin.X, P^.fOrigin.Y,
                      P^.fOrigin.X + P^.fSize.X,
                      P^.fOrigin.Y + P^.fSize.Y);
             AdjustToDesktopSize(R, P^.fDeskSize);
             W^.Locate(R);
             with V^, P^ do
              begin
                if fPos+fBufPos > FileSize then
                begin
                  fPos := 0;
                  fBufPos := 0;
                end;
                Seek(fPos);
                BufPos     := fBufPos;
                if XLatFile<>nil then DisposeStr(XLatFile);
                XLatFile   := NewStr(fXlatFile);
                ViewMode   := fViewMode and not vmInternal;
                Filter     := fFilter;
                HexEdit    := fHexEdit;
                Wrap       := fWrap;
                XDelta     := fXDelta;
                HexPos     := fHexPos;
                Cur        := fCur;
                UseXLat    := fUseXLat;
                KeyMap     := fKeyMap;
                if UseXLat
                 then SetXlatFile(XLatFile^, True)
                 else SetXlatFile('', True);
                MarkPos   := fMarks;
                Loaded    := true;
                MakeLines;
              end;
           end;
      end;
  end;

label ex, int, ext; (* X-Man *)
begin
   I:=PosChar('|', FileName);
   If I>0 then begin
    RN:=Copy(FileName, I+1, 255);
    FileName[0]:=Char(I-1);
   end;
   FileName:=lFExpand(FileName);
   if I<=0 then RN:=FileName;
   fr:=CreateFileRec(FileName);
   fillchar(up, sizeof(up), 0);
   up.active:=fr;
   UpStr(XT);
   Desktop^.GetExtent(R);
   Abort := False;
   if (Message(DeskTop, evBroadcast, cmFindView, @FileName) <> nil)
     and ((ReuseViewers=2) or
          ((ReuseViewers=1) and
           (MessageBox(GetString(dlOpenNewWindow), nil, mfYesButton+mfNoButton)<>cmYes)
          )
         )
     then begin TempFile:=''; Exit; end;
   if Intern=1 then (* X-Man *)
    begin
     XT := ' '+RN;
     UpStr(XT);
     PS := @XT;
     if (InterfaceData.Options and ouiTrackViewers <> 0) and
        (ViewHistory <> nil)
      then I := ViewHistory^.IndexOf(@PS) else I := -1;
     if I >= 0 then begin
      P := ViewHistory^.At(I);
      Q := P^.fViewMode;
      if Q and vmInternal = vmInternal then goto Int;
     end else Q:=-1;
Ext:  if ((Intern>0) and (SystemData.Options and ossViewer = 0)) and View (* X-Man *)
      then goto ex;
     XT := UpStrg(GetExt(FileName));
     {$IFDEF DBView}
     if (UpStrg(XT) = '.DBF') or (Q=vmDB) then begin
      W := New(PDBWindow,Init(FileName));
      if (Q=vmDB) and (W<>nil) then
       with P^, W^ do begin
         R.Assign(fOrigin.X, fOrigin.Y, fOrigin.X + fSize.X, fOrigin.Y + fSize.Y);
         AdjustToDesktopSize(R, fDeskSize);
         Locate(R);
        with PDBWindow(W)^.P^ do begin
         Delta := fdDelta;
         Pos   := fdPos;
        end;
       end;
     end else
     {$ENDIF}
     {$IFDEF SpreadSheet}
      if (UpStrg(XT) = '.WKZ') or (Q=vmSpread) then begin
       W := New(PCalcWindow,Init(R, FileName));
       if (Q=vmDB) and (W<>nil) then
        with P^, W^ do begin
          R.Assign(fOrigin.X, fOrigin.Y, fOrigin.X + fSize.X, fOrigin.Y + fSize.Y);
          AdjustToDesktopSize(R, fDeskSize);
          Locate(R);
         with PCalcWindow(W)^.CalcView^ do begin
          Delta       := fsDelta;
          Cur         := fsCur;
          Mark        := fsMark;
          CurrentCalc := fsCurrentCalc;
          SearchPos   := fsSearchPos;
          ErrorCell   := fsErrorCell;
         end;
        end;
      end else
     {$ENDIF}
     {$IFDEF PKTView}
     if IsPktFile(FileName) or (Q=vmPKT)
      then begin
      ViewPktFile(FileName);
      goto ex;
     end else
     if IsMsgFile(FileName) or (Q=vmMSG)
      then begin
      ViewMSGFile(FileName);
      goto ex;
     end else
     {$ENDIF}
        if ArcViewer(FileName, FreeByte) or Abort then goto ex
         else if View then goto ex
          else goto Int;
    end else if Intern=2 then goto Ext else (* X-Man *)
Int:InternalView;
   if (W <> nil) and not W^.Valid(0) then begin Dispose(W,Done); InternalView end;
   StoreViewInfo(W);
   InsertWindow(W);
ex: DelFileRec(fr);
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
procedure TDNAppl.ViewFilter;
var
  W : PWindow;
  R : TRect;
  up: tUserParams;
  P : PViewRecord;
  S: String;
  V: PFileViewer;
  PS: PString;
  I: longint;

begin
 I:=PosChar('|', FileName);
 if I=0 then exit;
 S:=lFExpand(Copy(FileName, I+1, 255));
 FileName[0]:=Chr(I-1);
 FileName:=lFExpand(FileName);
 W := New(PFileWindow,Init(FileName, S, ViewerDefaults.Options and vbfHex <> 0));
 if (W <> nil) and (W^.Valid(cmValid)) then begin
  V := PFileViewer(W^.Current);
  S := V^.VFileName; System.Insert(' ', S, 1); UpStr(S); PS := @S;
  if (InterfaceData.Options and ouiTrackViewers <> 0) and (ViewHistory <> nil)
   then I := ViewHistory^.IndexOf(@PS) else I := -1;
  If I>=0 then
   if (PViewRecord(ViewHistory^.At(I))^.fViewMode and not vmInternal)>3
    then I:=-1;
  if I >= 0 then begin
   P := ViewHistory^.At(I);
   R.Assign(P^.fOrigin.X, P^.fOrigin.Y,
            P^.fOrigin.X + P^.fSize.X,
            P^.fOrigin.Y + P^.fSize.Y);
   AdjustToDesktopSize(R, P^.fDeskSize);
   W^.Locate(R);
   with V^, P^ do begin
    if fPos+fBufPos > FileSize then begin
     fPos := 0;
     fBufPos := 0;
    end;
    Seek(fPos);
    BufPos     := fBufPos;
    if XLatFile<>nil then DisposeStr(XLatFile);
    XLatFile   := NewStr(fXlatFile);
    ViewMode   := fViewMode and not vmInternal;
    Filter     := fFilter;
    HexEdit    := fHexEdit;
    Wrap       := fWrap;
    XDelta     := fXDelta;
    HexPos     := fHexPos;
    Cur        := fCur;
    UseXLat    := fUseXLat;
    KeyMap     := fKeyMap;
    if UseXLat
     then SetXlatFile(XLatFile^, True)
     else SetXlatFile('', True);
    MarkPos   := fMarks;
    Loaded    := true;
    MakeLines;
   end;
  end;
 end;
 if (W <> nil) and W^.Valid(0) then begin
  StoreViewInfo(W);
  InsertWindow(W);
 end else Dispose(W,Done);
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
procedure TDNAppl.EditFile;
var
  W: PWindow;
  R: TRect;
  PS: PString;
  V: PFileEditor;
  I: Integer;
  P: PEditRecord;
  fr: PFileRec;
  up: tUserParams;
  s: string;

  function Edit: Boolean;
  begin
    Edit := ExecExtFile('DN.EDT', @up, dlLoadingEditor, nil, I); (* X-Man *)
  end;

label ex;
begin
  FileName:=lFExpand(FileName);
  fr:=CreateFileRec(FileName);
  fillchar(up, sizeof(up), 0);
  up.active:=fr;
   if (Intern and (SystemData.Options and ossEditor <> 0)) or
      (not Intern and (SystemData.Options and ossEditor = 0)) or
     not Edit then
      begin
        if (Message(DeskTop, evBroadcast, cmFindEdit, @FileName) <> nil)
           and ((ReuseEditors=2) or
                ((ReuseEditors=1) and
                 (MessageBox(GetString(dlOpenNewWindow), nil, mfYesButton+mfNoButton)<>cmYes)
                )
               )
        then
         begin TempFile:=''; Exit; end;
        if (InterfaceData.Options and ouiStoreEditorPosition <> 0) then
         begin
           if not TempBounds.Empty then R := TempBounds
                                    else begin Desktop^.GetExtent(R); LastEditDeskSize := Desktop^.Size end;
           AdjustToDesktopSize(R, LastEditDeskSize);
         end else Desktop^.GetExtent(R);
        W := New(PEditWindow,Init(R, FileName));
        if (W <> nil) and (W^.Valid(cmValid)) then
          begin
            V := PEditWindow(W)^.Intern;
            FreeStr := V^.EditName; System.Insert(' ', FreeStr, 1); UpStr(FreeStr);
            PS := @FreeStr;
            if (InterfaceData.Options and ouiTrackEditors <> 0) and (EditHistory <> nil)
                then I := EditHistory^.IndexOf(@PS) else I := -1;
            if I >= 0 then
              begin
                P := EditHistory^.At(I);
                R.Assign(P^.fOrigin.X, P^.fOrigin.Y, P^.fOrigin.X + P^.fSize.X, P^.fOrigin.Y + P^.fSize.Y);
                AdjustToDesktopSize(R, P^.fDeskSize);
                W^.Locate(R);
                with V^, P^ do
                  begin
                    MarkPos                := fMarks;
                    Mark.A                 := fBlockStart;
                    Mark.B                 := fBlockEnd;
                    ScrollTo(fDelta.X, fDelta.Y);
                    Pos                    := fPos;
                    BlockVisible           := fBlockVisible;
                    VertBlock              := fVerticalBlock;
                    EdOpt.HiLite           := fHighlight;
                    EdOpt.HiliteColumn     := fHiliteColumn;
                    EdOpt.HiliteLine       := fHiliteLine;
                    EdOpt.AutoIndent       := fAutoIndent;
                    EdOpt.AutoJustify      := fAutoJustify;
                    EdOpt.AutoBrackets     := fAutoBrackets;
                    EdOpt.LeftSide         := fLeftSide;
                    EdOpt.RightSide        := fRightSide;
                    EdOpt.InSide           := fInSide;
                    InsertMode             := fInsMode;
                    KeyMap                 := fKeyMap; {-$VIV}
                    WorkString := GetLine(fDelta.Y); {-$VIV 11.05.99}
                  end;
              end else StoreEditInfo(W);

            InsertWindow(W);
          end;
      end;
ex: DelFileRec(fr);
end;
        {-DataCompBoy-}

 procedure Denie; {$IFDEF VIRTUALPASCAL}inline;{$ENDIF}
 begin ExecResource( dlgDenie, FreeStr ); end;

        {-DataCompBoy-}
PROCEDURE TDNAppl.HandleCommand;

 var R: TRect;
     P: PView;
     ST: String;
     W: Word;

{$IFDEF VIRTUALPASCAL} {JO}
     CC: Word;
     F: lFile;
     PJ,Q: String; QQ: integer;
{$ENDIF}

    procedure CE; begin ClearEvent(Event) end;


{$IFDEF VIRTUALPASCAL} {JO}
function GetGLUKName(s: string):string;
 Begin
  GetGLUKName:=GetPath(s)+Copy(GetName(s), 1, 12);
 end;

FUNCTION GetfURZ2(const s: string): string;
 var a,aa,aaa:string;
 Begin
  lFSplit(S, a, aa, aaa);
  aa:=aa+aaa;
  GetfURZ2:=a+Copy(aa, 1, 8)+'.'+Copy(aaa, 9, 3);
 End;
{$ENDIF}

begin
   case Event.Command of
     cmFirstTimePanel: OpenWindow( False );
     cmPutInClipboard: PutInClip(Event.Infoptr);
     cmGetFromClipboard: GetFromClip(Event.Infoptr);
     cmSearchAdvance: begin RSearchAdvance; CE end;
     cmListOfDirs: DoQuickChange;
     {$IFDEF DiskFormat}
       cmFormatDisk: RunFormat;
     {$ENDIF}
     {$IFDEF PrintManager}
       cmFilePrint: PrintFile(PString(Event.InfoPtr)^);
       cmSetupPrinter: SetupPrinter;
     {$ENDIF}
     cmOpenSmartpad: OpenSmartpad;
     cmOpenClipBoard: OpenClipBoard; {-$VOL}
     cmExecGrabber: ScreenGrabber(True);
     {$IFDEF MODEM}
     cmTerminalDefaults: if TerminalSetup and (Term <> nil) then
                           Move(TerminalDefaults, Term^.Emulator, 4);
     {$ENDIF}
     cmSetupConfirmation: ConfirmSetup;
     cmCountrySetup: SetupCountryInfo;
     cmColumnDefaultsDisk: if SetupColumnDefaultsDisk then begin WriteConfig; Application^.Redraw; end;
     cmColumnDefaultsTemp: if SetupColumnDefaultsTemp then begin WriteConfig; Application^.Redraw; end;
     cmColumnDefaultsFind: if SetupColumnDefaultsFind then begin WriteConfig; Application^.Redraw; end;
     cmColumnDefaultsArch: if SetupColumnDefaultsArch then begin WriteConfig; Application^.Redraw; end;
     cmColumnDefaultsArvd: if SetupColumnDefaultsArvd then begin WriteConfig; Application^.Redraw; end;
     cmHighlightGroups: SetHighlightGroups;
     {$IFDEF DBView}
        cmDBFView: InsertWindow(New(PDBWindow,Init(CnvString(Event.InfoPtr))));
     {$ENDIF}
     {$IFDEF SpreadSheet}
        cmNewTable: LoadSheet('');
        cmSheetLoad: OpenSheet;
        cmWKZView: begin
                     DeskTop^.GetExtent(R);
                     InsertWindow(New(PCalcWindow,Init(R, CnvString(Event.InfoPtr))));
                   end;
     {$ENDIF}
     {$IFDEF PHONES}
        cmPhoneBook: PhoneBook(False);
     {$ENDIF}
     {$IFDEF Modem}
     {$IFDEF LINK}
        cmNavyLink: StartLink;
     {$ENDIF}
     {$IFDEF PHONES}
        cmUndial: PhoneBook(True);
     {$ENDIF}
        cmSetupModem: SetupModem;
        cmTerminal: OpenTerminal;
        cmHangUp: if COMPort <> nil then
                   begin
                    P := WriteMsg(GetString(dlDisconnect));
                    HangUp;
                    {$IFDEF PHONES}
                    if Dialer <> nil then Dialer^.Free;
                    {$ENDIF}
                    if Term <> nil then Term^.Owner^.Redraw;
                    if P <> nil then P^.Free;
                   end;
     {$ENDIF}
     {$IFDEF DiskEdit}
        cmDiskEdit: if CheckDisk(GetCurDrive)
                      then InsertWindow(New(PFixerWindow, Init))
                      else Denie;
     {$ENDIF}
     cmTextView, cmHexView: begin
                             ST:=CnvString(Event.InfoPtr);
                             W:=PosChar('|', ST);
                             if W>0
                              then InsertWindow(New(PFileWindow,
                                                    Init(Copy(ST, 1, W-1),
                                                         Copy(ST,W+1,255),
                                                         Event.Command = cmHexView)))
                              else InsertWindow(New(PFileWindow,
                                                    Init(ST, ST,
                                                         Event.Command = cmHexView)))
                            end;
     {$IFDEF MODEM}
     cmAdvancePortSetup: if ExecResource( dlgAdvancedCOMSetup, AdvModemData) = cmOK then
                            begin
                              StoreModemSetup;
                            end;
     {$ENDIF}
     cmSystemSetup: SystemSetup;
     cmInterfaceSetup: InterfaceSetup;
     cmStartup: StartupSetup;
     cmSetupMouse: MouseSetup;
{$IFDEF SS}
     cmSaversSetup: SaversSetup;
{$ENDIF}
{$IFDEF TrashCan}
     cmHideShowTools: if TrashCan^.ImVisible
                        then begin TrashCan^.Hide; TrashCan^.ImVisible:=false; end
                        else begin TrashCan^.Show; TrashCan^.MakeFirst;
                                   TrashCan^.ImVisible:=true;                  end;
{$ENDIF}
{$IFDEF Reanimator}
     cmReanimator: if CheckDisk(GetCurDrive) then OpenReanimator else Denie;
{$ENDIF}
     cmEditHistory: EditHistoryMenu;
     cmViewHistory: ViewHistoryMenu;
{$IFDEF DiskEdit}
     cmSetVolumeLabel: if CheckDisk(GetCurDrive) then SetVLabel else Denie;
{$ENDIF}
     cmXEditFile: OpenEditor;
     cmWindowManager: WindowManager;
     cmUserMenu, cmGlobalUserMenu: if (ExecUserMenu(Event.Command=cmGlobalUserMenu)) then
                begin
                  ST := FreeStr;
                  if CheckExit then
                  begin
                  {$IFNDEF OS2}
                   ST:=SwpDir+'$DN'+ItoS(DNNumber)+'$.BAT ' + ST;
                  {$ELSE}
                   ST:=SwpDir+'$DN'+ItoS(DNNumber)+'$.CMD ' + ST;
                  {$ENDIF}
                   ExecString(@ST,'');
                  end;
                end;
{$IFNDEF MINARCH}
     cmReadArchive: ReadArcList;
{$ENDIF}
{$IFDEF SS}
     cmScreenRest: InsertIdler;
{$ENDIF}
     cmChScreenMode:SelectVideoModeDialog;
     cmASCIITable: ASCIITable;
     cmAbout: begin
                ClearEvent(Event);
                MessageBoxAbout;
                Exit;
              end;
     cmCalculator: InsertCalc;
{$IFDEF Calendar}
     cmCalendar: InsertCalendar;    {JO}
{$ENDIF}
     cmRefresh: Redraw;
     cmClearDesktop: GlobalMessage(evCommand, cmClose, nil);
     cmFileView: ViewFile(2, CnvString(Event.InfoPtr)); (* X-Man *)
     cmFileEdit: EditFile(True, CnvString(Event.InfoPtr));
     cmViewFilter: ViewFilter(CnvString(Event.InfoPtr));
     cmIntFileView: ViewFile(0, CnvString(Event.InfoPtr)); (* X-Man *)
     cmIntFileEdit: EditFile(False, CnvString(Event.InfoPtr));
     cmEditQuickRun: EditFile(True, Copy(SourceDir,1,Byte(ShiftState and 3 = 0)*255)+'DN.XRN');
     cmExtFileEdit: EditFile(True, Copy(SourceDir,1,Byte(ShiftState and 3 = 0)*255)+'DN.EXT');
     cmMenuFileEdit: EditFile(True, SourceDir+'DN.MNU');
     cmLocalMenuFileEdit: EditFile(True, 'DN.MNU');
     cmEditHGL: EditFile(True, SourceDir+'DN.HGL');
     cmEditSPF: EditFile(True, SourceDir+'DN.SPF');
     cmEditINI: EditFile(True, SourceDir+'DN.INI');
     cmExternalViewers: EditFile(True, Copy(SourceDir,1,Byte(ShiftState and 3 = 0)*255)+'DN.VWR');
     cmExternalEditors: EditFile(True, Copy(SourceDir,1,Byte(ShiftState and 3 = 0)*255)+'DN.EDT');
     cmShowUserScreen: ShowUserScreen;
     cmRestart: if not CheckExit
                 then begin ClearEvent(Event); Exit end
                 else begin
                  StringCache^.FreeAll;
                  ExecString(@NullStr,'');
                 end;
     cmCreatePanel: OpenWindow(True);
     cmCreateTree: OpenTreeWindow;
     cmFindFile: FindFile;
{$IFDEF SYSINFO}
     cmSystemInfo: SystemInfo;
{$ENDIF}
{$IFDEF MEMINFO}
     cmMemoryInfo: MemoryInfo;
{$ENDIF}
     cmShowOutput: GetUserScreen;
     cmHistoryList: CmdHistory;
     cmLoadDesk: TDNApplication(Self).RetrieveDesktop
                 (SourceDir+'DN'+GetEnv('DNDSK')+'.DSK', nil, True);
{$IFDEF VIRTUALPASCAL} {JO changes begin}
     cmRetrieveSwp:
     if PosChar(TempFileSWP[1], '+-=<>|[!') = 0  then
      begin lAssignFile(F, TempFileSWP); lSetFAttr(F, $20);  lEraseFile(F); end
      else
  begin
        PJ := TempFileSWP;
        TempFile := Copy(PJ,2,255);
        QQ := Pos('|', TempFile); Dec(QQ);
        if QQ>0 then Q:=Copy(TempFile, QQ+2, 255) else Q:='';
        QQ := QQ and 255; {BP bugfix by piwamoto}
        If Not ExistFile(TempFile) then TempFile:=GetfURZ(Copy(PJ,2,QQ));
        If Not ExistFile(TempFile) then TempFile:=GetfURZ2(Copy(PJ,2,QQ));
        If Not ExistFile(TempFile) then TempFile:=GetGLUKName(Copy(PJ,2,QQ));
        If Not ExistFile(TempFile) then TempFile:=Copy(PJ,2,QQ);
        case PJ[1] of
          '-': CC := cmIntFileView;
          '=': CC := cmDBFView;
          '>': CC := cmWKZView;
          '<': CC := cmTextView;
          '|': CC := cmHexView;
          '[': CC := cmReadArchive;
          '!': CC := cmViewFilter;
          else CC := cmFileView;
        end;
        if CC <> 0 then
                    begin
                      if Q<>'' then TempFile:=TempFile+'|'+Q;
                      Event.What := evCommand;
                      Event.Command := CC;
                      Event.InfoPtr := @TempFile;
                      PutEvent(Event);
                     end;
        RunMenu := False;
       end;
{$ENDIF}               {JO changes end}
     cmSaveDesk: SaveRealDsk;
     cmExecFile: begin DoExecFile(Event.InfoPtr); CE end;
     cmExecString: begin DoExecString(Event.InfoPtr); CE end;
     cmExecCommandLine: if ExecCommandLine then CE;
     cmShowTimeInfo: ShowTimeInfo; {DataCompBoy}
     {$IFDEF CHCOL}
     cmChangeColors: begin ChangeColors; WriteConfig end;
     {$ENDIF}
     cmChangeUserMode1: if ScreenMode <> smSVGALo then SetScrMode(smSVGALo) else SetScrMode(StartupMode);
     cmChangeUserMode2: if ScreenMode <> smSVGAHi then SetScrMode(smSVGAHi) else SetScrMode(StartupMode);
     cmChangeMode: case ScreenMode of
                    sm80x25: if VideoType = vtEGA
                              then SetScrMode ( sm80x43 )
                              else SetScrMode ( sm80x50 );
                    else SetScrMode ( sm80x25 );
                   end;
     cmLoadColors: LoadColors;
     cmSetupArchive: SetupArchive(DefaultArchiver);
     cmUpdateArcFile: UpdateARH(nil);
     cmLoConfigArchiver..cmHiConfigArchiver: SetupArchive(Event.Command);
     cmFMDefaults: FMDefaults;
     cmFMSetup: FMSetup;
     cmDriveInfoSetup: DriveInfoSetup;
     cmExecuteDOScmd: ExecDOSCmd;
     cmEditorDefaults: SetupEditorDefaults;
     cmViewerDefaults: SetupViewerDefaults;
    {$IFDEF CDPlayer}
     cmPlayCD: RunPlayer;
    {$ENDIF}
     cmStoreColors: StoreColors;
     {$IFDEF Game}
     cmGame: if Game = nil then InsertWindow(New(PGameWindow, Init))
                           else Game^.Owner^.Select;
     {$ENDIF}
     cmHideCmdLine: ToggleCmdLine;
     cmGetTeam: if Desktop^.TopView^.HelpCtx = hcAboutDialog then begin
                 Desktop^.TopView^.GetExtent(R);
                 R.Grow(-1,-2); Dec(R.B.Y,2);
                 New(TeamView, Init(R));
                 PGroup(Desktop^.TopView)^.Insert(TeamView);
                 Desktop^.TopView^.HelpCtx := hcTeam;
                end;
        cmQuit: begin
                  if Confirms and cfExitConfirm <> 0 then
                   W := Msg(dlQueryExit, nil, mfYesNoConfirm)
                   else W := cmYes;
                  if (W <> cmYes) or not CheckExit then
                  begin
                    {$IFDEF OS_DOS}
                    if w95locked then w95QuitCancel; {Gimly}
                    {$ENDIF}
                    ClearEvent(Event);
                  end;
                end;
        cmHelp: begin OpenHelp(0); CE end;
        cmHelp2: begin OpenHelp(Event.InfoWord); CE end;
        cmTree: begin ExecTree; CE end;
        cmClearData: begin
                       FillChar(FreeStr, SizeOf(FreeStr), 0);
                       PView(Event.InfoPtr)^.Owner^.SetData(FreeStr);
                       ClearEvent(Event);
                     end;
         cmChLngId: ChLngId;
   end;
end;
        {-DataCompBoy-}

{$IFDEF OS_DOS}
        {Gimly}
const w95QuitEnabled:BYTE=1;

{$IFDEF DPMI}
function Int_2F_168F(const DX: integer):integer;
   var Reg: DPMIRegisters;
begin
    Reg.AX:=$168F;
    Reg.DX:=DX;
    SimulateRealModeInt($2F,Reg);
    Int_2F_168F:=Reg.AX;
end;
{$ENDIF}

procedure w95QuitInit; assembler;
asm
{Checking DN.INI setting... So, it can run only AFTER dn.ini parsing.. }
   MOV AL, SmartWindowsBoxClose
   OR  AL, AL
   JZ  @@No

   MOV AL, OpSys {Check Win9x, WinNT, W2K}
   AND AL, opWin+opWNT
   JNZ @@EnableEvent

@@No:
   XOR AL, AL
   MOV w95QuitEnabled, AL
   JMP @@E

@@EnableEvent:
{ Enablind close event handling... }
{$IFDEF DPMI}
   push 1
   call Int_2F_168F
{$ELSE}
   mov ax, 168fh
   mov dx, 1
   int 2fh
{$ENDIF}
   test AX, AX
   jnz @@No    { Windoze did not want to warn us.. }

@@E:
end;

{const w95Timer:BYTE=1;} { Timer for delayed checking }

function w95QuitCheck:boolean;assembler;
asm
   MOV AL, w95QuitEnabled
   OR  AL, AL
   JZ  @@No

(*) { <- uncomment for less frequently close button state checking}
{ Delay checking }
   MOV al, w95Timer
   dec al
   jnz @@S1
   mov al, 10 {divider}
@@S1:
   mov w95Timer, al
   jnz @@No
(**)

{ Checking the close button state }
{$IFDEF DPMI}
   push 100h
   call Int_2F_168F
{$ELSE}
   MOV AX, 168Fh
   MOV DH, 1
   INT 2fh
{$ENDIF}
   TEST AX, AX
   JNZ  @@No
{ Pressed - preventing the Windows message from appearing }

   MOV AL, 1
   MOV w95locked, AL

{$IFDEF DPMI}
   push 200h
   call Int_2F_168F
{$ELSE}
   MOV AX, 168Fh
   MOV DH, 2
   INT 2fh
{$ENDIF DPMI}

{ Just disabling close ;) }
{$IFDEF DPMI}
   push 0
   call Int_2F_168F
{$ELSE}
   MOV AX, 168Fh
   XOR DX, DX
   INT 2fh
{$ENDIF DPMI}

   MOV AX, 1 { "X" button pressed }
   JMP @@E
@@No:
   XOR AX, AX
@@E:;
end;

procedure w95QuitCancel;assembler;
  asm
{ Enabling the w95 close button }
{$IFDEF DPMI}
   push 1
   call Int_2F_168F
{$ELSE}
   mov ax, 168Fh
   mov dx, 1
   int 2fh
{$ENDIF DPMI}

{ Cancelling close }
{$IFDEF DPMI}
   push 300h
   call Int_2F_168F
{$ELSE}
   mov ax, 168Fh
   mov dx, 300h
   int 2fh
{$ENDIF}
   mov al, 0
   mov w95locked, al
end;
     {Gimly}
{$ENDIF}

end.
