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
//  dn200-dbf_wkz_restore_position.patch
//  dn200-dbf_wkz-history.patch
//  dn200-empty_dbf.patch
//
//  2.3.0
//  dn230-remove_GZip_compression_and_change_external_filter_view.patch
//  dn2522_manual_save_desktop.patch
//  dn2522-big_help_fix.patch
//  dn2628_use_viewer_history_fix.patch
//
//  2.7.0
//  dn270-search_in_archives.patch
//  dn270-reuse_editor_and_viewers-jo20612a.patch
//  dn281-dbf_wkz_view_from_archive_fix.patch
//  dn281-store_viewer_and_editor_position_fix.patch
//  dn281-save_all_editor_options_to_history.patch
//  dn281-title_update_after_exit_from_help.patch
//  dn281-open_editor_file_doesn't_exist_fix.patch
//  dn2811-enable_DefCodePage_for_new_files.patch
//  dn2825-delete_filtered_file_from_TEMP.patch
//  dn2825-new_logic_for_viewers.patch
//  dn2825-help_bug_fix.patch
//  dn2911-help_bug_fix.patch
//  dn2911-view_files_in_archive_from_history.patch
//  dn2922-ChangeDirMenu_help_bug_fix.patch
//  dn21013-add_flt_files_to_history_fix.patch
//  dn21202-editor(f)-double_items_in_history.patch
//  dn21202-help_showing_improve.patch
//  dn21202-screen_grabber_help.patch
//  dn328-util(f)-screen_grabber_help_fix.patch
//  dn3216-DBF_Viewer(fn)-change_code_page_ability_added.patch
//  dn3315-DBF_Viewer(i)-indicator_and_code_page_improve.patch
//  dn328-InputLine(fi)-new_shortkey_added_and_some_bugs_fixed.patch
//  dn3421-DiskEditor(f)-quit_from_DN_fix.patch
//
//  3.7.0
//  dn3617-system(f)-set_and_get_window_title_more_correct.patch
//  dn3617-output(f)-correct_help_showing.patch
//  dn31005-DiskEditor(f)-quit_from_DN_in_BANK_Version_fix.patch
//  dn31005-bp_to_vp_on_off_true_false.patch
//  dn31005-FileView(f)-view_file_temp_file_fix.patch
//  dn31214-FileView(f)-view_file_temp_file_fix_in_VP.patch
//  dn31220-kernel(if)-work_with_temporary_files.patch
//  dn31220-kernel(f)-BatchExtension_fix.patch
//  dn31220-Kernel(f)-OEM_title_fix_for_VP.patch
//  dn31220-viewer(f)-save_history_for_archived_files.patch
//  dn40205-kernel(f)-BatchExtension_fix.patch
//  dn40328-fix_UC2_AIN_and_DN_micro_improvement.patch
//
//  4.9.0
//
//////////////////////////////////////////////////////////////////////////}
{$I STDEFINE.INC}

unit DNUtil;

interface
uses Gauges, DnApp, xTime, Views, Drivers, Objects,helpfile,Tree;

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

const
  BatchExtension: String[4] = '.BAT';

type
     PDNAppl = ^TDNAppl;
     TDNAppl = object(TApplication)
        IdleClick : TEventTimer ;
        IdleEvt   : TEvent ;
        TreeReader: PTreeReader;

        Pk1,Pk2,Pk3,Pk4 : PView ;

        procedure ViewFile(FromHistory, AltExt, NoExtFile: boolean; FileName: String); { Flash }
        procedure EditFile(Intern: Boolean; FileName: String); {DataCompBoy}
        procedure OutOfMemory; virtual;
        procedure EventError(var Event: TEvent); virtual;
        procedure HandleCommand(var Event: TEvent);
        private
        procedure TextView(FileName, RN: String; Fixed: byte);
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
     FViewer, Advance1, Dos, LFN, Advance, DnIni, Arcview, u_keymap, flpanel,
     Microed, EdWin, DnUtil2, usermenu, dnsvld, colors, archread, gauge,
     asciitab, ccalc, filefind, videoman, archset, archiver, dnhelp, diskinfo,
     HelpKern, advance7, Setups, menus, titleset
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
{--- start -------- Eugeny Zvyagintzev ---- 19-05-2003 ----}
{$IFDEF DiskEdit}
     b: Boolean;
 procedure CloseDiskFixer(P: PView); {$IFDEF BIT_16}far;{$ENDIF}
 begin
  If TypeOf(P^) = TypeOf(TFixerWindow) Then Dispose(P,Done);
 end;
{$ENDIF}
{--- finish -------- Eugeny Zvyagintzev ---- 19-05-2003 ----}

 function FindQuit(P: PView): Boolean; {$IFDEF BIT_16}far;{$ENDIF}
 begin
  P^.HandleEvent(Event);
  FindQuit := (Event.What = evNothing) or not P^.Valid(cmQuit);
 end;
begin
{ if FormatWindow <> nil then begin CheckExit := False; Exit; end;}
 Event.What := evCommand; Event.Command := cmQuit;
{$IFNDEF DiskEdit}
 CheckExit := Desktop^.FirstThat(@FindQuit) = nil;
{$ELSE}
 b:=(Desktop^.FirstThat(@FindQuit) = nil);
 If b Then Desktop^.ForEach(@CloseDiskFixer); {John_SW 19-05-2003 Quit from DN when DiskFixer is active}
 CheckExit := b;
{$ENDIF}
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

const
  ExtFileName: array[boolean] of string[10] =
    ('dn.vwr', 'dnalt.vwr');

(* X-Man >>> *)
procedure StoreCallback(var Param, AltExt);
begin
    StoreExtViewer(string(Param), boolean(AltExt))
end;
(* X-Man <<< *)

procedure TDNAppl.TextView(FileName, RN: String; Fixed: byte);
 var S: String;
     PS: PString;
     V: PFileViewer;
     X: Integer;
     W: PWindow;
     I: Integer;
     P : PViewRecord;
     R : TRect;
begin
  { Flash >>> }
  S:=RN; X:=PosChar(':',S);
  if X>2 then
   begin
   S:=Copy(S,X+1,Length(S)-X); X:=PosChar(':',S);
   if X>0 then
    begin
    S:=Copy(S,X+1,Length(S)-X); X:=PosChar(':',S);
    if X>0 then S:=Copy(S,X+1,Length(S)-X);
    end;
   if S[1]<>'\' then S:='\'+UpStrg(S);
   end else S:=UpStrg(GetSName(RN)+GetExt(RN));
  if (UpStrg(GetExt(FileName))='.FLT') and
     (UpStrg(GetSName(FileName)+GetExt(FileName)) <> S) then
   begin
   if (UpStrg(GetCurrentDir)+'\'<>UpStrg(TempDir)) and
      (not ExistFile(GetCurrentDir+'\'+GetName(RN)) or
       (FileTime(GetCurrentDir+'\'+GetName(RN))=FileTime(TempDir+GetName(RN)))) then
    EraseTempFile(TempDir+GetName(RN));
   if (InterfaceData.Options and ouiTrackViewers <> 0) and (ViewHistory <> nil) then
    begin
    S:=CnvString(PViewRecord(ViewHistory^.At(0))^.fName);
    DelFC(S); RN:=S;
    end;
   end;
  { Flash <<< }
  W := New(PFileWindow,Init(FileName, RN, ViewerDefaults.Options and vbfHex <> 0));
  if (W <> nil) and (W^.Valid(cmValid)) then
    begin
      V := PFileViewer(W^.Current); S := V^.VFileName; System.Insert(' ', S, 1); UpStr(S);
      PS := @S;
      if (InterfaceData.Options and ouiTrackViewers <> 0) and (ViewHistory <> nil)
        then I := ViewHistory^.IndexOf(@PS) else I := -1;
      if I >= 0 then
         if (PViewRecord(ViewHistory^.At(I))^.fViewMode and not vmInternal)>vmAsm then I:=-1;
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
              if (fPos+fBufPos > FileSize) or { Flash }
                 (InterfaceData.Options and ouiStoreViewerPosition = 0) then
              begin
                fPos := 0;
                fBufPos := 0;
              end;
              Seek(fPos);
              if XLatFile <> nil then DisposeStr(XLatFile);
              ViewMode   := fViewMode and not vmInternal;
              BufPos     := fBufPos;
              XLatFile   := NewStr(fXlatFile);
              Filter     := fFilter;
              HexEdit    := fHexEdit;
              Wrap       := fWrap;
              XDelta     := fXDelta;
              HexPos     := fHexPos;
              Cur        := fCur;
              UseXLat    := fUseXLat;
              KeyMap     := fKeyMap;
              MarkPos    := fMarks;
              Loaded     := true;
              if UseXLat
               then SetXlatFile(XLatFile^, True)
               else SetXlatFile('', True);
              MakeLines;
            end;
         end;
      case Fixed of
        1: V^.ViewMode := vmText;
        2: V^.ViewMode := vmHex;
      end;
      if Fixed in [1,2] then V^.MakeLines;
      InsertWindow(W);
    end;
end;

        {-DataCompBoy-} {AK155} { Flash }
procedure TDNAppl.ViewFile(FromHistory, AltExt, NoExtFile: boolean; FileName: String); {AK155}
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
  Ext: Boolean; { Flash }

  function SpecialIntView: Boolean;
    var
      _M: Integer;
{--- start -------- Eugeny Zvyagintzev ---------}
      FileIsDBF: Boolean;
{--- finish -------- Eugeny Zvyagintzev ---------}
    label
      db;
    begin
    SpecialIntView := false;
    XT := ' '+RN;
    UpStr(XT);
    PS := @XT;
    if (InterfaceData.Options and ouiTrackViewers <> 0) and
       (ViewHistory <> nil)
     then I := ViewHistory^.IndexOf(@PS) else I := -1;
    if I >= 0 then begin
     P := ViewHistory^.At(I);
     Q := P^.fViewMode;
     if Q and vmInternal = vmInternal then
       Q := -1;
    end else Q:=-1;
    XT := UpStrg(GetExt(FileName));
    {$IFDEF DBView}
    if (XT = '.DBF') or (Q=vmDB) then begin
{--- start -------- Eugeny Zvyagintzev ---------}
{Use history if possible}
      XT := ' '+RN;
      UpStr(XT);
      PS := @XT;
      Q:=-1;
      if (InterfaceData.Options and ouiTrackViewers <> 0) and
         (ViewHistory <> nil) then
       I := ViewHistory^.IndexOf(@PS)
      else
       I := -1;
      if I >= 0 then
       begin
        P := ViewHistory^.At(I);
        Q:=P^.fViewMode;
       End;
      FileIsDBF:=False;
      W := New(PDBWindow,Init(RN+'|'+FileName,FileIsDBF));
{If file is not valid DBF and NOT empty DBF file then use internal viewer}
      If (W = Nil) And (FileIsDBF = False) Then Exit; { Flash }
{--- finish -------- Eugeny Zvyagintzev ---------}
     if (Q=vmDB) and (W<>nil) then
      with P^, W^ do begin
        R.Assign(fOrigin.X, fOrigin.Y, fOrigin.X + fSize.X, fOrigin.Y + fSize.Y);
        AdjustToDesktopSize(R, fDeskSize);
        Locate(R);
       with PDBWindow(W)^.P^ do begin
        Delta := fdDelta;
        Pos   := fdPos;
        KeyMap:= fKeyMap;    {John_SW 14-03-2003}
        if XLatFile<>nil then DisposeStr(XLatFile);
        XLatFile:= NewStr(fXlatFile);
        UseXLat := fUseXLat;
        if UseXLat
         then SetXlatFile(XLatFile^, True)
         else SetXlatFile('', True);
       end;
      end;
    goto db;
    end;
    {$ENDIF}

    {$IFDEF SpreadSheet}
     if (XT = '.WKZ') or (Q=vmSpread) then begin
{--- start -------- Eugeny Zvyagintzev ---------}
{Use history if possible}
       XT := ' '+RN;
       UpStr(XT);
       PS := @XT;
       if (InterfaceData.Options and ouiTrackViewers <> 0) and
          (ViewHistory <> nil) then
        I := ViewHistory^.IndexOf(@PS)
       else
        I := -1;
       if I >= 0 then
        begin
         P := ViewHistory^.At(I);
         _M:=P^.fViewMode;
         Q:=vmSpread;
        End;
{--- finish -------- Eugeny Zvyagintzev ---------}
      W := New(PCalcWindow,Init(R, RN+'|'+FileName));
{--- start -------- Eugeny Zvyagintzev ---------}
{If file is not valid WKZ then use internal viewer.}
{But CalcWindow will always created.}
       If W = Nil Then Exit; { Flash }
{--- finish -------- Eugeny Zvyagintzev ---------}
      if (Q=vmSpread) and (W<>nil) then
       with P^, W^ do begin
         R.Assign(fOrigin.X, fOrigin.Y, fOrigin.X + fSize.X, fOrigin.Y + fSize.Y);
         AdjustToDesktopSize(R, fDeskSize);
         Locate(R);
{--- start -------- Eugeny Zvyagintzev ---------}
{If previous view mode is NOT vmSpread then position will NOT restored}
         If _M = vmSpread Then
{--- finish -------- Eugeny Zvyagintzev ---------}
        with PCalcWindow(W)^.CalcView^ do begin
         Delta       := fsDelta;
         Cur         := fsCur;
         Mark        := fsMark;
         CurrentCalc := fsCurrentCalc;
         SearchPos   := fsSearchPos;
         ErrorCell   := fsErrorCell;
         HScroll^.Value := Delta.X + Cur.X;{AK155}
         VScroll^.Value := Delta.Y + Cur.Y;{AK155}
         end;
       end;
     goto db;
     end;
    {$ENDIF}

    {$IFDEF PKTView}
    if (IsPktFile(FileName) or (Q=vmPKT)) and (XT = '.PKT') then
      begin
      SpecialIntView := true;
      ViewPktFile(FileName);
      exit;
      end;
    if (IsMsgFile(FileName) or (Q=vmMSG)) and (XT = '.MSG') then
      begin
      SpecialIntView := true;
      ViewMSGFile(FileName);
      exit;
      end;
    {$ENDIF}
    SpecialIntView := ArcViewer(FileName, RN, FreeByte);
    exit;

db:
    if (W <> nil) and not W^.Valid(0) then
      begin
      Dispose(W,Done); exit;
      end;
{    StoreViewInfo(W);  AK155: сохранять бессмысленно (еще нечего)
        и излишне (сохранит Done) }
    InsertWindow(W);
    SpecialIntView := true;
    end;

begin
   Q:=-1; {JOHN_SW}
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
   Desktop^.GetExtent(R);
   Abort := False;
   if (ReuseViewers > 0)
     and (Message(DeskTop, evBroadcast, cmFindView, @FileName) <> nil)
     and ((ReuseViewers=2) or
          ((ReuseViewers=1) and
           (MessageBox(GetString(dlOpenNewWindow), nil, mfYesButton+mfNoButton)<>cmYes)
          )
         )
     then begin TempFile:=''; Exit; end;

     XT := ' '+RN;
     UpStr(XT);
     PS := @XT;
     if (InterfaceData.Options and ouiTrackViewers <> 0) and
        (ViewHistory <> nil) and FromHistory
      then I := ViewHistory^.IndexOf(@PS) else I := -1;
     if I >= 0 then begin
      P := ViewHistory^.At(I);
      Q := P^.fViewMode;
      end;

{ Логика выбора просмотра такая:
  - опция "Встроенный просмотр" включена -
  по F3 (not AltExt) - специальный внутренний, затем согласно dn.vwr, затем текстовый;
  - опция "Встроенный просмотр" выключена -
  по F3 (not AltExt) - согласно dn.vwr, затем специальный внутренний, затем текстовый;
  - вне зависимости от опции "Встроенный просмотр" -
  по Alt-F3 (AltExt) - согласно dnalt.vwr, затем текстовый;
  по Alt-Shift-F3 (это вообще не здесь) - безусловно текстовый. }

   Ext:=(Q=vmAltExt) or AltExt;
   OldTitle:='';
   if (not NoExtFile) and ((Q in [vmExternal, vmAltExt]) or (Q<0) or AltExt) then
     if ExecExtFile(ExtFileName[Ext],@up,dlLoadingViewer,StoreCallback,RN,Ext) then exit;
   if ((Q>vmAltExt) or (Q<0)) and not AltExt and SpecialIntView then exit;
   if not FromHistory and not AltExt and NoExtFile then
     if ExecExtFile(ExtFileName[Ext],@up,dlLoadingViewer,StoreCallback,RN,Ext) then exit;
   TextView(FileName, RN, 0);
   DelFileRec(fr); {Cat}
end;
        {-DataCompBoy-} {AK155} { Flash }

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
    Edit := ExecExtFile('DN.EDT', @up, dlLoadingEditor, nil, I, s); (* X-Man *)
  end;

label ex;
begin
{JO: редактирование найденных в архиве файлов из панели поиска не предусмотрено}
  if PathFoundInArc(FileName) then Exit;
{/JO}
  FileName:=lFExpand(FileName);
  fr:=CreateFileRec(FileName);
  fillchar(up, sizeof(up), 0);
  up.active:=fr;
  { Flash >>> }
  if not ExistFile(FileName) then
   if (SystemData.Options and ossEditor <> 0) then
    Intern := true else Intern := false;
  { Flash <<< }
   if (Intern and (SystemData.Options and ossEditor <> 0)) or
      (not Intern and (SystemData.Options and ossEditor = 0)) or
     not Edit then
      begin
        if (ReuseEditors > 0)
           and (Message(DeskTop, evBroadcast, cmFindEdit, @FileName) <> nil)
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
            { Flash >>> }
            if UpStrg(DefCodePage) = 'DOS' then V^.KeyMap:=kmAscii else
            if UpStrg(DefCodePage) = 'WIN' then V^.KeyMap:=kmAnsi  else
            if UpStrg(DefCodePage) = 'KOI' then V^.KeyMap:=kmKoi8r;
            { Flash <<< }
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
                    { Flash >>> }
                    if (InterfaceData.Options and ouiStoreEditorPosition = 0) then
                     begin
                     fPos.X := 0; fPos.Y := 0;
                     fDelta.X := 0; fDelta.Y := 0;
                     fBlockStart.X := 0; fBlockStart.Y := 0;
                     fBlockEnd.X := 0; fBlockEnd.Y := 0;
                     end;
                    { Flash <<< }
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
                    { Flash >>> }
                    EdOpt.BackIndent       := fBackIndent;
                    EdOpt.AutoWrap         := fAutoWrap;
                    OptimalFill            := fOptimalFill;
                    TabReplace             := fTabReplace;
                    EdOpt.SmartTab         := fSmartTab;
                    { Flash <<< }
                    WorkString := GetLine(fDelta.Y); {-$VIV 11.05.99}
                  end;
              end{ else StoreEditInfo(W)}; { Commented by Flash 21-12-2002 }
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
     P,P1: PView;
     ST: String;
     W: Word;
     FileIsDBF: Boolean;
     ViewMode: Byte;

     HFile: PHelpFile;
     HelpStrm: PDosStream;
     OldCtx  : Word;
     Title : TTitleStr; { Flash }

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
        cmDBFView: InsertWindow(New(PDBWindow,Init(CnvString(Event.InfoPtr),FileIsDBF)));
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
                             if Event.Command = cmTextView then ViewMode := 1 else ViewMode := 2;
                             if W>0 then
                               TextView(Copy(ST, 1, W-1), Copy(ST, W+1, 255), ViewMode)
                             else
                               TextView(ST, ST, ViewMode)
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
                   ST:=SwpDir+'$DN'+ItoS(DNNumber)+'$'+BatchExtension+' '+ST;
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
     cmCalculator: InsertCalc(Event.InfoPtr);
{$IFDEF Calendar}
     cmCalendar: InsertCalendar;    {JO}
{$ENDIF}
     cmRefresh: Redraw;
     cmClearDesktop: GlobalMessage(evCommand, cmClose, nil);
     cmFileView:    if (SystemData.Options and ossViewer = 0) then
                     ViewFile(False, False, False, CnvString(Event.InfoPtr))
                    else
                     ViewFile(False, False, True, CnvString(Event.InfoPtr));
     cmFileEdit: EditFile(True, CnvString(Event.InfoPtr));
     cmViewFilter: ViewFile(False, True, True, CnvString(Event.InfoPtr));{piwamoto} { Flash }
     cmIntFileView: ViewFile(False, True, False, CnvString(Event.InfoPtr)); (* X-Man *) { Flash }
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
{John_SW  7-10-2003: Extract correct file name first!}
        if TempFile[1]='|' then TempFile:=Copy(TempFile,2,Length(TempFile)-1);
        QQ := Pos('|', TempFile);
        if QQ>0 then
         begin
          Q:=Copy(TempFile, QQ, 255);
          TempFile:=Copy(PJ,2,QQ-1);
         end
        else Q:='';
        if TempFile[Length(TempFile)]='|' then TempFile:=Copy(TempFile,1,Length(TempFile)-1);

        If Not ExistFile(TempFile) then TempFile:=GetfURZ(Copy(PJ,2,QQ));
        If Not ExistFile(TempFile) then TempFile:=GetfURZ2(Copy(PJ,2,QQ));
        If Not ExistFile(TempFile) then TempFile:=GetGLUKName(Copy(PJ,2,QQ));
        If Not ExistFile(TempFile) then TempFile:=Copy(PJ,2,255);
        case PJ[1] of
          '-': CC := cmIntFileView;
          '<': CC := cmTextView;
          '|': CC := cmHexView;
{$IFDEF DBView}
          '=': CC := cmDBFView;
{$ENDIF}
{$IFDEF SpreadSheet}
          '>': CC := cmWKZView;
{$ENDIF}
{$IFNDEF MINARCH}
          '[': CC := cmReadArchive;
{$ENDIF}
          '!': CC := cmViewFilter;
          else CC := cmFileView;
        end;
{Commented by John_SW  7-10-2003: CC never equals zero!
{        if CC <> 0 then begin}
        if Q<>'' then TempFile:=TempFile+Q;
        Event.What := evCommand;
        Event.Command := CC;
        Event.InfoPtr := @TempFile;
        PutEvent(Event);
{                     end;}
        RunMenu := False;
       end;
{$ENDIF}               {JO changes end}
{--- start -------- Eugeny Zvyagintzev ---- 02-06-2002 ----}
     cmSaveDesk: begin
                  ManualSave:=True;
                  SaveRealDsk;
                  ManualSave:=False;
                 end;
{--- finish -------- Eugeny Zvyagintzev ---- 02-06-2002 ----}
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
{--- start -------- Eugeny Zvyagintzev ---- 06-06-2002 -----}
{Now DN will show help window more correctly}
        cmHelp,cmHelp2: Begin
                 ClearEvent(Event);
                 { Flash >>> }
                 if GrabberActive and HelpInUse then Exit;
                 Title:=WindowTitle;
                 { Flash <<< }
                 if (not HelpInUse) or (HelpWnd=nil) then
                 begin
                  HelpInUse := True;
                  HelpStrm := New(PDosStream,
                                  Init(SourceDir+HelpLngId + '.HLP', stOpenRead{stOpenPacked})
                                 );
                  if HelpStrm^.Status <> stOk then
                    begin
                      Dispose(HelpStrm,Done);
                      HelpStrm := New(PDosStream,
                                      Init(StartupDir+HelpLngId + '.HLP', stOpenRead{stOpenPacked})
                                     );
                    end;
                  HFile := New(PHelpFile, Init(HelpStrm));
                  if HelpStrm^.Status <> stOk then
                  begin
                    Msg(erCantOpenHelp, nil, mfError + mfOkButton);
                    Dispose(HFile,Done);
                    HFile:=nil;
                    HelpStrm:=nil;
                  end
                  else
                  begin
                   if Event.Command=cmHelp
                    then HelpWnd := PHelpWindow(ValidView(New(PHelpWindow,Init(HFile, GetHelpCtx))))
                    else HelpWnd := PHelpWindow(ValidView(New(PHelpWindow,Init(HFile, Word(Event.InfoWord)))));
                   if HelpWnd<>nil then begin
                    if ((DeskTop^.Current<>nil)
                        and (
                         (Desktop^.Current^.GetState(sfModal)) or
                         (Desktop^.Current^.GetState(sfDragging))
                        )
                       ) or MenuActive or GrabberActive or OutputActive { Flash 25-06-2003 }
                     then ExecView(HelpWnd)
                     else begin
                           HelpWnd^.SetState(sfModal, false);
                           HelpWnd^.Options := HelpWnd^.Options or ofTileable or ofTopSelect;
                           Application^.InsertWindow(HelpWnd);
                           Exit;
                          end;
                   end;
                  end;
                  HelpWnd^.Free;
                  HelpInUse := False;
                 end else begin
                  OldCtx:=HelpWnd^.HelpView^.Indexes[HelpWnd^.HelpView^.NumTopics].Index;

                  if Event.Command=cmHelp
                   then HelpWnd^.GotoContext(Word(GetHelpCtx))
                   else HelpWnd^.GotoContext(Word(Event.InfoWord));
                  if Event.Command=cmHelp2 then HelpWnd^.GotoContext(Word(Event.InfoWord));
                  if ((DeskTop^.Current<>nil)
                      and (Desktop^.Current^.GetState(sfModal))
                     ) or MenuActive
                   then begin
                         HelpWnd^.SetState(sfModal, true);
                         P:=HelpWnd^.Next;
                         P1:=DeskTop^.Current;
                         Desktop^.Delete(HelpWnd);
                         ExecView(HelpWnd);
                         HelpWnd^.GotoContext(OldCtx);
                         {HelpWnd^.SetState(sfVisible,False);}
                         Desktop^.InsertBefore(HelpWnd, P);
                         Desktop^.SetCurrent(P1, EnterSelect);
                         {HelpWnd^.Free;
                         Desktop^.SetCurrent(P1, EnterSelect);}
                        end
                   else HelpWnd^.Focus;
                 end;
                 SetTitle(Title); { Flash }
                end;
{--- finish -------- Eugeny Zvyagintzev ---- 06-06-2002 -----}
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
