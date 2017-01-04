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
//  dn16rc1-Folder_description_copying_bugfix_diff141byMV.patch
//  dn16rc1-Bugfix_for_unsuccessful_Move_operation.patch
//  dn16rc1-rename_file_dpmi_lfn.patch
//
//  2.0.0
//  dn200-File_exist_check.patch
//  dn200-Copy_Files_check_before_read.patch
//  dn200-files_panel_bugs_fix.patch
//
//  2.3.0
//  dn230-accept_all_for_rename_file_when_exist.patch
//  dn269-diskinfo.patch
//  dn269_show_help_during_copy_files.patch
//  dn2628-rename_and_copy_dir_fix.patch
//
//  2.7.0
//  dn270-search_in_archives.patch
//  dn270-copy_diz_to_the_same_directory_fix.patch
//  dn270-adding_to_filecopy_history_fix.patch
//  dn270-add_filename_to_history_temp.patch
//  dn2825-copy_file_check_freespace_fix.patch
//  dn2911-copy_file_check_freespace_fix.patch
//  dn2922-copy_file_check_freespace_fix.patch
//  dn21013-copy_file_fix_when_could_not_open_file.patch
//  dn21029-title_update_when_copying.patch
//  dn21029-move_files_fix.patch
//  dn21029-skip_bad_files_improve.patch
//  dn21116-fpanel-move_file_fix_1.patch
//  dn21201-copy_arcpanel_to_temppanel.patch
//  dn21225-FileCopy(f)-copy_more_then_2Gb_fix.patch
//  dn328-FilePanel(f)-reread_directory_fix.patch
//  dn3216-fpanel(i)-refresh_title_only_when_needed.patch
//  dn3315-filecopy(i)-add_filename_to_history.patch
//  dn3315-CopyFile(n)-show_time_and_speed_added.patch
//  dn3323-temp(f)-copy_files_from_archives.patch
//  dn3323-filecopy(f)-title_update_fix.patch
//  dn3323-history(i)-add_directories.patch
//  dn3331-FileFindPanel(f)-reread_directory_fix.patch
//  dn3421-FileCopy(f)-copy_zero_files_fix.patch
//  dn3421-FileCopy(i)-show_copy_time_cosmetic_improve.patch
//
//  3.7.0
//  dn370-archives(if)-improve_and_fix.patch
//  dn31005-bp_to_vp_on_off_true_false.patch
//  dn31029-Compile_by_VP.patch
//  dn40307-define_DN_Micro.patch
//
//  4.9.0
//
//////////////////////////////////////////////////////////////////////////}

{$I STDEFINE.INC}

unit FileCopy;

interface
uses Dos, LFN, LFNCol, TitleSet, UserMenu, {DataCompBoy} collect, advance,
     advance1, advance2, advance3, Objects, Views, Dialogs, Drivers, FilesCol,
     DnIni, filediz, ArvidAvt;

type
    PCopyRec = ^TCopyRec;
    TCopyRec = record
     FC: PFilesCollection;
     Owner: PView;
     Where: TPoint;
    end;

const
     SkipCopyDialog: Boolean = False;
     CopyDirName:    String  =  '';{DataCompBoy}
     RevertBar:      boolean = False;{DataCompBoy}
     CopyFromTemp:   Boolean = False;

procedure CopyFiles(Files: PCollection; Owner: PView; MoveMode: Boolean; FromTemp: Byte);
function  SelectDialog(Select: Boolean; var St: String; var XORSelect: Boolean): Boolean;
procedure BeepAfterCopy;

function  CopyDialog(var CopyDir: String; var Mask: String; {DataCompBoy}
                     var CopyOpt: Word; var CopyMode: Word;
                     var CopyPrn: Boolean; {var Inheread: Byte;}
                     MoveMode: Boolean; Files: PCollection;
                     FromTemp: Byte; Owner: PView; Link: Boolean): Boolean;

implementation
uses DNApp, Startup, Memory, Messages, HistList, Commands,
     xTime, Validate, dnhelp,
    {$IFNDEF NOEXTRA}ExtraMem,{$ENDIF}
    {$IFDEF MODEM}{$IFDEF LINK}NavyLink,{$ENDIF}{$ENDIF}
    {$IFNDEF NONBP} DiskTool,{$ENDIF}
    {$IFDEF VIRTUALPASCAL} VpSysLow, {$ENDIF}
    {$IFDEF DPMI} DPMI, {$ENDIF}
     Gauge, FileFind,
     DNUtil, RStrings, Tree, Archiver, Drives, DiskInfo;

const
      cpmOverwrite = 0;
      cpmAppend    = 1;
      cpmAskOver   = 2;
      cpmSkipAll   = 3;
      cpmRefresh   = 4;
      cpmRename    = 5;

      cpoCheckFree = $01;
      cpoVerify    = $02;
      cpoDesc      = $04;
      cpoMove      = $08;
      cpoFromTemp  = $80;
      cpoCopyDesc  = $40;
      cpoDirEmpty  = $20;
      cpoFitAll    = $10;

      NoCopyLFN: Boolean = True;

type
      PDir = ^TDir;
      TDir = record
        Created: (sNone, sCreated, sErased);
        XName,Name: PString;
      end;

      PFileCopyRec = ^TFileCopyRec;
      TFileCopyRec = record
       Name: PString;
       Attr: Byte;
       Size: TSize;
       Dir: PDir;
       Owner: PFileRec;
       DIZ: PDIZ;
      end;

      PCopyCollection = ^TCopyCollection;
      TCopyCollection = object(TCollection)
       procedure FreeItem(P: Pointer); virtual;
      end;

      PDirCollection = ^TDirCollection;
      TDirCollection = object(TSortedCollection)
       procedure FreeItem(P: Pointer); virtual;
       function Compare(P1, P2: Pointer): Integer; virtual;
      end;


function MemAvail: LongInt;
begin
  MemAvail := MemAdjust(System.MemAvail);
end;

function MaxAvail: LongInt;
begin
  MaxAvail := MemAdjust(System.MaxAvail);
end;

        {-DataCompBoy-}
procedure GetFTimeSizeAttr(const A: string; var ATime: LongInt; var ASize: TSize; var AAttr: Word);
var
  SR: lSearchRec;
begin
  ClrIO;
  lFindFirst(A, $FF, SR);
  ATime := SR.SR.Time;
  ASize := SR.Fullsize;
  AAttr := SR.SR.Attr;
  lFindClose(SR);
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
procedure TCopyCollection.FreeItem;
begin
 if (P <> nil) and (PFileCopyRec(p)^.Name<>nil) then
  DisposeStr(PFileCopyRec(p)^.Name);
 if P <> nil then Dispose(PFileCopyRec(P));
end;
        {-DataCompBoy-}

procedure TDirCollection.FreeItem;
begin
 if P <> nil then
  with PDir(P)^ do
   begin
    DisposeStr(XName);
    DisposeStr(Name);
    Dispose(PDir(P));
   end;
end;

function TDirCollection.Compare;
 var S1, S2: String;
begin
 Compare := 1;
 if PDir(P1)^.Name <> nil then S1 := PDir(P1)^.Name^ else S1 := '';
 if PDir(P2)^.Name <> nil then S2 := PDir(P2)^.Name^ else S2 := '';
 if S1 < S2 then Compare := -1
  else if S1 = S2 then
    begin
     if PDir(P1)^.XName <> nil then S1 := PDir(P1)^.XName^ else S1 := '';
     if PDir(P2)^.XName <> nil then S2 := PDir(P2)^.XName^ else S2 := '';
     if S1 < S2 then Compare := -1
        else if S1 = S2 then Compare := 0
    end
end;

procedure BeepAfterCopy;
  var C: Char;
begin
  DelayTics(1);
  for C := 'A' to 'D' do
    begin
       {$IFDEF VIRTUALPASCAL}
       SysBeepEx{PlaySound}(1259, 55);
       SysBeepEx{PlaySound}(1400, 55);
       {$ELSE}
       Sound(1259);
       DelayTics(1);
       Sound(1400);
       DelayTics(1);
       {$ENDIF}
    end;
   {$IFNDEF VIRTUALPASCAL}
   Sound(1259);
   DelayTics(2);
   NoSound;
   {$ELSE}
   SysBeepEx{PlaySound}(1259, 110);
   {$ENDIF}
end;

        {-DataCompBoy-}
function SelectDialog;
var
  I: Integer;
  P: record
   S: String;
   XorSel: Boolean;
  end;
  Idx: TDlgIdx;
  D: PDialog;
  V, V1: PView;
  R: TRect;

  function FindLine(P: PView): Boolean; {$IFDEF BIT_16}far;{$ENDIF}
  begin
    FindLine := (P <> nil) and (P^.DataSize > 5);
  end;

begin with P do begin
  XORSel := XORSelect;
  SelectDialog := False;
  if LowMemory then Exit;
  if Select then Idx := dlgSelect else Idx := dlgUnselect;
  D := PDialog(Application^.ValidView(PDialog(LoadResource(Idx))));
  if D = nil then Exit;
  S := HistoryStr(hsSelectBox, 0);
  if S = '' then S := x_x;
  V := D^.FirstThat(@FindLine);
  if V <> nil then PInputLine(V)^.SetValidator(New(PFilterValidator,
                                               Init([#32..#255]-
                                                    ['|','>','<'])));
  D^.SetData(S);
  if Desktop^.ExecView(D) = cmCancel then begin Dispose(D,Done); HistoryAdd(hsSelectBox, S); Exit; end;
  SelectDialog := True;
  D^.GetData(S);
  Dispose(D,Done);
  St := S;
  XORSelect := XORSel;
end end;
        {-DataCompBoy-}

type
    TBlock = record
      Len : Word;
      Time: LongInt;
      EOF : Boolean;
      Last: Boolean;
    end;

const
      eoStart  = $01;
      eoEnd    = $02;
      eoAppend = $04;
      eoDir    = $08;
      eoCheck  = $10;

  MemStream: PMemoryStream = nil;
{$IFNDEF NOEXTRA}
  EMSStream: PEMSStream = nil;
  NoUseEMS: Boolean = False;
  XMSStream: PXMSStream = nil;
  NoUseXMS: Boolean = False;
{$ENDIF}

var
   ToDo,ToDoCopy,ToDoClusCopy: TSize;
   ToDoClusCopyTemp: TSize;

type
  PLine = ^TLine;
  TLine = object(TObject)
    Owner: Pointer;
    OldName: Pointer;
    NewName: PString;
    Size: TSize;
    Pos: longint;
    Date: LongInt;
    Len: Word;
    EOF: Byte;
    Attr: Byte;
    constructor Init(var ALen: LongInt; AOwner: Pointer; const AOldName, ANewName: String;
                      ASize:TSize; ADate: LongInt; AAttr: Byte; AEOF: ShortInt);
    procedure PrepareToWrite; virtual;
    function Stream: PStream; virtual;
    procedure Write(var B);
    procedure Read(var B);
    destructor Done; virtual;
  end;

{$IFNDEF NOEXTRA}
  PEMSLine = ^TEMSLine;
  TEMSLine = object(TLine)
    procedure PrepareToWrite; virtual;
    function Stream: PStream; virtual;
  end;

  PXMSLine = ^TXMSLine;
  TXMSLine = object(TLine)
    procedure PrepareToWrite; virtual;
    function Stream: PStream; virtual;
  end;
{$ENDIF}

  PDirName = ^TDirName;
  TDirName = object(TObject)
    OldName, NewName: PString;
    Check: Boolean;
    CopyIt: Boolean;
    Own: Pointer;
    Attr: Byte;
    constructor Init(const AOld, ANew: String; ACopy: Boolean; AnOwn: Pointer; AnAttr: Byte);
    function DOld: String;
    function DNew: String;
    destructor Done; virtual;
  end;

{ TDirName }

constructor TDirName.Init;
begin
  inherited Init;
  OldName := NewStr(AOld);
  NewName := NewStr(fReplace('.\','\',ANew));
  CopyIt := ACopy;
  Own := AnOwn;
  Attr := AnAttr;
end;

function TDirName.DNew;
begin
  if NewName = nil then DNew := '' else DNew := NewName^;
end;

function TDirName.DOld;
begin
  if OldName = nil then DOld := '' else DOld := OldName^;
end;

destructor TDirName.Done;
begin
  DisposeStr(OldName);
  DisposeStr(NewName);
  Inherited Done;      {DataCompBoy}
end;

{ TLine }

constructor TLine.Init;
begin
  inherited Init;
  if MaxAvail < 2048 then Fail;
  Len := ALen; EOF := AEOF; Attr := AAttr; Date := ADate; Size := ASize;
  if Len <> 0 then
    begin
      PrepareToWrite;
      if Len <= 0 then Fail;
      ALen := Len;
    end else EOF := eoStart + eoEnd;
  Owner := AOwner;
  if (EOF<>0) and ((eoStart or eoEnd) <> 0) then
     OldName := NewStr(AOldName);
  NewName := NewStr(ANewName);
end;

procedure TLine.Write;
begin
  if Stream <> nil then Stream^.Write(B, Len);
end;

procedure TLine.Read;
begin
  FillChar(B, Len, 0);
  if Stream <> nil then
    begin
      if Stream^.GetPos<>Pos then Stream^.Seek(Pos);
      Stream^.Read(B, Len);
    end;
end;

destructor TLine.Done;
begin
  DisposeStr(PString(NewName));
  DisposeStr(PString(OldName));
  inherited Done;
end;

procedure TLine.PrepareToWrite;
begin
{$IFNDEF NOEXTRA}
  if (XMSStream <> nil) or (EMSStream <> nil) then begin Len := 0; Exit; end;
  if MemStream = nil then
    begin
      Pos := MaxAvail-$4000;
      if Pos < $E000 then Pos := MaxAvail - $3000;
      if Pos < $A000 then Pos := MaxAvail - $2000;
      if Pos < $8000 then Pos := MaxAvail - $1000;
      if Pos < $1000 then begin Len := 0; Exit end;
      New(MemStream, Init(Pos, 2048));
    end;
  Pos := Stream^.GetPos;
  if Pos + Len > Stream^.GetSize then Len := Stream^.GetSize - Pos;
{$ELSE}
  if MemStream = nil then
    begin
     Pos:=(MaxAvail div 10) * 9;
     if (CopyLimit > 0) and
        (Pos > CopyLimit shl 10) and
        ((opSys and opDos)<>opDos)
      then Pos:=CopyLimit shl 10;
     New(MemStream, Init(Pos, 32768));
    end;
  Pos := Stream^.GetPos;
  if Pos + Len > Stream^.GetSize then Len := Stream^.GetSize - Pos;
{$ENDIF}
end;

function TLine.Stream: PStream;
begin
  Stream := MemStream;
end;


{$IFNDEF NOEXTRA}

{ TEMSLine }

procedure TEMSLine.PrepareToWrite;
begin
  if not EMSFound or NoUseEMS or (XMSStream <> nil) then begin Len := 0; Exit; end;
  if EMSStream = nil then
    begin
      Pos := LongInt(EMSFreePages);
      if Pos < 32 then begin NoUseEMS := True; Len := 0; Exit end;
{-DataCompBoy-}
{piwamoto.change.begin}
      if (Pos > (CopyLimit div 16)) and
         (((opSys and opDos)<>opDos) and (CopyLimit>0))
       then Pos := CopyLimit div 16;
      if Pos > 128 then Pos := 128; {Don't remove it! It's a bugfix!}
{piwamoto.change.end}
{-DataCompBoy-}
      Pos := (Pos-1) * LongInt(16384);
      if Pos > ToDo+10240 then Pos := Round(ToDo+10240);
      New(EMSStream, Init(Pos, Pos));
      if EMSStream^.Status <> stOK then
        begin
          Dispose(EMSStream,Done);
          EMSStream:=nil;
          Len := 0;
          NoUseEMS := True;
          Exit;
        end;
    end;
  Pos := Stream^.GetPos;
  if Pos + Len > Stream^.GetSize then Len := Stream^.GetSize - Pos;
end;

function TEMSLine.Stream: PStream;
begin
  Stream := EMSStream;
end;


{ TXMSLine }

procedure TXMSLine.PrepareToWrite;
begin
  if not XMSFound or NoUseXMS then begin Len := 0; Exit; end;
  if XMSStream = nil then
    begin
      Pos := LongInt(XMSFree);
      if Pos < 512 then begin NoUseXMS := True; Len := 0; Exit; end;
{-DataCompBoy-}
{piwamoto.change.begin}
      if (Pos > CopyLimit) and
         (((opSys and opDos)<>opDos) and (CopyLimit>0))
       then Pos := CopyLimit;
      if Pos > 32768 then Pos := 32768; {Don't remove it! It's a bugfix!}
{piwamoto.change.end}
{-DataCompBoy-}
      Pos := LongInt(Pos-1) shl 10;
      if Pos > ToDo+10240 then Pos := Round(ToDo+10240);
      New(XMSStream, Init(Pos, Pos));
      if XMSStream^.Status <> stOK then
        begin
          Dispose(XMSStream,Done);
          XMSStream:=nil;
          NoUseXMS := True;
          Len := 0;
          Exit;
        end;
    end;
  Pos := Stream^.GetPos;
  if Pos + Len > Stream^.GetSize then Len := Stream^.GetSize - Pos;
end;

function TXMSLine.Stream: PStream;
begin
  Stream := XMSStream;
end;

{$ENDIF NOEXTRA}

{ ----------------------------- File Copy ------------------------------ }

function CDRomInstalled : Boolean;
{$IFNDEF VIRTUALPASCAL}
var
  R : {$IFDEF DPMI}DPMIRegisters{$ELSE}Registers{$ENDIF};
begin
  with R do begin
    AX := $1500;
    BX := $0000;
    {$IFDEF DPMI}SimulateRealModeInt{$ELSE}Intr{$ENDIF}($2F, R);
    CDRomInstalled := (BX <> 0);
  end;
{$ELSE}
begin CDRomInstalled := True
{$ENDIF}
end;

        {-DataCompBoy-}
procedure FilesCopy(Files: PCollection; Owner: PView;
                    const CopyDir, Mask: String; CopyMode, CopyOptions: Word; CopyPrn: Boolean);

var  ReadStream:  lfile;
     WriteStream: lfile;
     ReadPos: TSize;
     CopyCancel: Boolean;
     SkipAllBad: Boolean; {John_SW  05-11-2002}
     B: Pointer;
     BSize: Word;
     TRead, TWrite,
     ToRead, ToWrite: TSize;
     StartTime: LongInt; {John_SW 20-03-2003}
     CopyQueue, Dirs: PCollection;
     Info: PWhileView;
     SoftMode: Boolean;
     R: TRect;
     InhR: Byte;
     ReD: set of Char;
     C: Char;
     Timer: TEventTimer;
     CD_Drives: set of Char;
     AdvCopy, Use40: Boolean;
     _Tmr: TEventTimer;
     IOR: Integer;
     BytesWrited: Longint; {-NeCoder-}
     Drv, SSS: string;

   function GetPercent(N: TSize): Str12;
   var
     T: TSize;
   begin
     if ToDo = 0 then N := 100 else
     begin
       T := ToDo; LowPrec(T, N);
       N := (N*100) / T;
     end;
     GetPercent := ZtoS(N)+'%'; { Flash 04-11-2002 }
   end;

   function MkName(const Nm: String): String;
   begin
    MkName:=Advance2.MkName(Nm, Mask);
   end;

   var
     GrdClick: Boolean;

{--- start -------- Eugeny Zvyagintzev ---- 06-06-2002 -----}
{Uncomment Application and delete Info variables to copy files in background}
   Procedure DispEvents(var CancelParam: Boolean);
   Var Event: TEvent;
       tmp: String;
   Begin
    If Info = Nil Then Exit;
    tmp:='';
    {Application}Info^.GetEvent(Event);
    If (Event.What = evCommand) And (Event.Command = cmCancel) And
       (Event.InfoPtr = Info^.But) Then
     Begin
      Info^.ClearEvent(Event);
      CancelParam:=(MessageBox(GetString(dlQueryAbort), nil, mfYesNoConfirm) = cmYes);
     End;
    If (Event.What <> evNothing) And
       Not ((Event.What = evCommand) And (Event.Command = cmQuit)) Then
     {Application}Info^.HandleEvent(Event);
   End;
{--- finish -------- Eugeny Zvyagintzev ---- 06-06-2002 -----}

   procedure Dispatch;
     var C: Boolean;
    procedure DE; begin GrdClick := True; DispEvents(C) end;

   begin
     C := False;
     if CopyCancel then begin DE; Exit end;
     if TimerExpired(_Tmr) then
     begin
       DE;
       CopyCancel := (C or CtrlBreakHit);
       CtrlBreakHit := False;
       NewTimer(_Tmr, 1);
     end;
   end;

   procedure ForceDispatch;
   begin
     NewTimer(_Tmr, 0); Dispatch;
   end;

   var Wrote: TSize;
       FreeDisk: TSize;

    procedure ImportDIZ(Name, NewName: Str12; LF: String; P: PDIZ; Owen: PString);
    var
      DizPath: String;
      S: String;
      I: Integer;
      NewDescription: PString;
    begin
      DizPath := CalcDPath(P, Owen);
      S:=DizPath; I:=Length(S);
      While (I > 0) and (S[I] <> '\') do Dec(I);
      if I > 0 then Delete(S, 1, I);
      NewDescription:=nil;
      if (P<>nil) then NewDescription:=P^.DIZ;
      ReplaceDIZ(GetDIZOwner(CopyDir,S,False), Name, LF,
                 @NewName, NewDescription);
      { Flash >>> }
      if GetPath(DizPath)=CopyDir then
       ReplaceDIZ(GetDIZOwner(CopyDir,S,False), Name, LF,
                  @Name, NewDescription);
      { Flash <<< }
      if (CopyOptions and cpoMove <> 0) and
         (FMSetup.Options and fmoPreserveDesc = 0) then
       DeleteDIZ(DizPath, Name, LF);
    end;

   procedure RemoveFromTemp(P: PFileRec);
     var I: LongInt;
   begin
     I := 0;
     if (P <> nil) and (TempFiles <> nil) and TempFiles^.Search(P, I)
      then TempFiles^.AtFree(I);
   end;


   function Overwrite(const NName: String; OldS, NewS:TSize; OldT, NewT: LongInt): Word;
    var
        AcceptAll : word ;
        I: Word;
        D: PDialog;
        P: PView;
        R: TRect;
        PP: Array [1..4] of Pointer;
        DD: LongInt;
        DT: DateTime;
        D1, D2,
        L1, L2: String[30];
        S: String;
   begin
    AcceptAll := 0;
    Overwrite := cmSkip;
    UnPackTime(OldT, DT);
    MakeDate(DateMode, DT.Day, DT.Month, DT.Year mod 100, DT.Hour, DT.Min, D2);
    L2 := FStr(OldS);
    UnPackTime(NewT, DT);
    MakeDate(DateMode, DT.Day, DT.Month, DT.Year mod 100, DT.Hour, DT.Min, D1);
    L1 := FStr(NewS);
    If Length( L1 ) < Length( L2 )
      then L1 := PredSpace( L1, Length( L2 ))
      else L2 := PredSpace( L2, Length( L1 ));
    D := PDialog( LoadResource( dlgOverwriteQuery ));
    D^.GetExtent(R); R.Grow(-1, -1); Inc(R.A.Y);
    D^.Options := D^.Options or ofCentered;
    DelRight(D1);
    DelRight(D2);
    D1[9] := '('; AddStr( D1, ')' );
    D2[9] := '('; AddStr( D2, ')' );
    PP[1] := @D1;
    PP[2] := @L1;
    PP[3] := @D2;
    PP[4] := @L2;
    FormatStr(S, GetString(dlFCOver), PP);
    R.B.Y:=R.A.Y+1;
    for i:=1 to Length(S) do if S[i]=^M then Inc(R.B.Y);
    P := New(PStaticText, Init(R,^C+GetString(dlFile)+' '+Cut(NName, 40)+S));
    D^.Insert(P);

    MsgActive := True;
    D^.SetData(AcceptAll);
    I := Desktop^.ExecView(D);
    MsgActive := False;
    NewTimer(Timer, 0);

    if I <> cmCancel then
      begin
        D^.GetData(AcceptAll);
        if (AcceptAll and 1 = 1) then
         case I of
           cmYes: CopyMode := cpmOverwrite;
           cmOk: CopyMode := cpmRename;
           cmSkip: CopyMode := cpmSkipAll;
           cmNo: CopyMode := cpmAppend;
         end;
        OverWrite := I;
      end else Overwrite := cmSkip;

    Dispose(D,Done);

    CopyCancel := I = cmCancel;
   end;

 var SkipRequested: Boolean;
     ExAttr: Word;

   procedure MaxWrite;
     label DoRewrite, DoAppend, 1, 2, lbStartWrite, lbStartCheck, DoNotUse40;
     var I: Integer;
         J: Word;
         P: PLine;
         A,L: longint;
         BB: TSize;
         F: lFile;
         PS: Array[1..2] of Pointer;
         S1: String;
         BufCrc : word ;
         Created: Boolean;
         Opened: Integer;
         MsgResult: integer;

      procedure TryToReset;
      var
        OldAttr: Word;
      begin
{AK155 2-02-2002
Раньше наличие файла проверялось  при помощи lResetFile, что не
вполне корректно. На CD RW под RSJ и, кажется, на некоторых сетевых дисках,
эта операция приводит к созданию файла и ложному запросу о перезаписи
файла. Более того, при отрицательном ответе созданный файл остается
(с нулевой длиной). Проверка при помощи lGetFAttr более чистая.
}
        if CopyMode <> cpmOverwrite  then
         Begin
          ClrIO;
          lGetFAttr(WriteStream, OldAttr);
          if (DosError = 0) then
           Begin
            lResetFile(WriteStream,1);
            Opened:=IOResult;
            Created:=false;
            Exit;
           End;
         End;
        lRewriteFile(WriteStream,1);
        Opened:=IOResult;
        Created:=Opened=0;
      end;

      procedure DoSkip;
      begin
        While (I < CopyQueue^.Count - 1)
              and (PLine(CopyQueue^.At(I))^.EOF and eoEnd = 0) do Inc(I);
        SkipRequested := I >= CopyQueue^.Count - 1;
      end;

    Function CheckI24Abort : boolean ;
      begin
        CheckI24Abort := Abort ;
        if not Abort then Exit ;
        CopyCancel := True;
      end;
const
  CrcTable: array[0..255] of Word = (
    $0000,  $1021,  $2042,  $3063,  $4084,  $50a5,  $60c6,  $70e7,
    $8108,  $9129,  $a14a,  $b16b,  $c18c,  $d1ad,  $e1ce,  $f1ef,
    $1231,  $0210,  $3273,  $2252,  $52b5,  $4294,  $72f7,  $62d6,
    $9339,  $8318,  $b37b,  $a35a,  $d3bd,  $c39c,  $f3ff,  $e3de,
    $2462,  $3443,  $0420,  $1401,  $64e6,  $74c7,  $44a4,  $5485,
    $a56a,  $b54b,  $8528,  $9509,  $e5ee,  $f5cf,  $c5ac,  $d58d,
    $3653,  $2672,  $1611,  $0630,  $76d7,  $66f6,  $5695,  $46b4,
    $b75b,  $a77a,  $9719,  $8738,  $f7df,  $e7fe,  $d79d,  $c7bc,
    $48c4,  $58e5,  $6886,  $78a7,  $0840,  $1861,  $2802,  $3823,
    $c9cc,  $d9ed,  $e98e,  $f9af,  $8948,  $9969,  $a90a,  $b92b,
    $5af5,  $4ad4,  $7ab7,  $6a96,  $1a71,  $0a50,  $3a33,  $2a12,
    $dbfd,  $cbdc,  $fbbf,  $eb9e,  $9b79,  $8b58,  $bb3b,  $ab1a,
    $6ca6,  $7c87,  $4ce4,  $5cc5,  $2c22,  $3c03,  $0c60,  $1c41,
    $edae,  $fd8f,  $cdec,  $ddcd,  $ad2a,  $bd0b,  $8d68,  $9d49,
    $7e97,  $6eb6,  $5ed5,  $4ef4,  $3e13,  $2e32,  $1e51,  $0e70,
    $ff9f,  $efbe,  $dfdd,  $cffc,  $bf1b,  $af3a,  $9f59,  $8f78,
    $9188,  $81a9,  $b1ca,  $a1eb,  $d10c,  $c12d,  $f14e,  $e16f,
    $1080,  $00a1,  $30c2,  $20e3,  $5004,  $4025,  $7046,  $6067,
    $83b9,  $9398,  $a3fb,  $b3da,  $c33d,  $d31c,  $e37f,  $f35e,
    $02b1,  $1290,  $22f3,  $32d2,  $4235,  $5214,  $6277,  $7256,
    $b5ea,  $a5cb,  $95a8,  $8589,  $f56e,  $e54f,  $d52c,  $c50d,
    $34e2,  $24c3,  $14a0,  $0481,  $7466,  $6447,  $5424,  $4405,
    $a7db,  $b7fa,  $8799,  $97b8,  $e75f,  $f77e,  $c71d,  $d73c,
    $26d3,  $36f2,  $0691,  $16b0,  $6657,  $7676,  $4615,  $5634,
    $d94c,  $c96d,  $f90e,  $e92f,  $99c8,  $89e9,  $b98a,  $a9ab,
    $5844,  $4865,  $7806,  $6827,  $18c0,  $08e1,  $3882,  $28a3,
    $cb7d,  $db5c,  $eb3f,  $fb1e,  $8bf9,  $9bd8,  $abbb,  $bb9a,
    $4a75,  $5a54,  $6a37,  $7a16,  $0af1,  $1ad0,  $2ab3,  $3a92,
    $fd2e,  $ed0f,  $dd6c,  $cd4d,  $bdaa,  $ad8b,  $9de8,  $8dc9,
    $7c26,  $6c07,  $5c64,  $4c45,  $3ca2,  $2c83,  $1ce0,  $0cc1,
    $ef1f,  $ff3e,  $cf5d,  $df7c,  $af9b,  $bfba,  $8fd9,  $9ff8,
    $6e17,  $7e36,  $4e55,  $5e74,  $2e93,  $3eb2,  $0ed1,  $1ef0
  );


  function UpdateCrc(CurByte : Byte; CurCrc : Word) : Word;
    {-Returns an updated CRC16}
  begin
    UpdateCrc := CrcTable[((CurCrc shr 8) and 255)] xor
                 (CurCrc shl 8) xor CurByte;
  end;


    Function GetCrc(var Buf ; BfLen : word ): word;
     var Data : array [0..$FFF] of byte absolute Buf ;
         I : word;
         Crc : word ;
        begin
         GetCrc := 0;
         CRC := $1972 ;
         if BfLen=0 then Exit ;
         for I:=0 to Pred(BfLen) do
         CRC := UpdateCrc( Data[I] , CRC );
         GetCrc := CRC ;
        end;


   procedure NoWrite;
   begin
     CopyCancel := True;
     ForceDispatch;
     CantWrite(CnvString(P^.NewName));
   end;

   { Flash >>> 04-11-2002 }
   function WriteCount: string;
   var S: string;
   begin
     WriteCount := FStr(ToWrite)+GetString(dlBytes)+' ('+GetPercent(ToWrite)+')'+GetString(dlFC_Written);
     S:=GetPercent((ToWrite+ToRead)/2)+' '+GetString(dlCopied);
     if OldTitle=S then Exit;
     SetTitle(S);
     OldTitle:=S;
   end;
   { Flash <<< 04-11-2002 }

   procedure TrySetArch;
   begin
     if (ExAttr and (ReadOnly+SysFile+Hidden) <> 0) then
     begin
       {$IFDEF OS_DOS}DisableAppend;{$ENDIF}
       lSetFAttr(WriteStream, Archive); ClrIO;
     end;
   end;

   var WPWas: Boolean;

{--- start -------- Eugeny Zvyagintzev ---- 22-03-2003 -----}
   Function TimeToStr(t: Comp): String;
   Var i: longint;
   Begin
    Move(t,i,SizeOf(i));
    FreeStr := SStr(i Div 360000, 2, '0')+CountryInfo.TimeSep;
    i := i Mod 360000;
    FreeStr := FreeStr+SStr(i Div 6000, 2, '0')+CountryInfo.TimeSep;
    i := i Mod 6000;
    TimeToStr := FreeStr+SStr(i Div 100, 2, '0');
   End;

   Function WriteTime: String;
   Var
       s,s1,s2,s3: String;
       ElapsedTime: Longint;
       M: Array [1..3] Of Pointer;
       t: TSize;
   Begin
    If ToWrite = 0 Then Exit;
    ElapsedTime:=Get100s-StartTime;
    WriteTime:='';
    If Not (ElapsedTime > 0) Then Exit;
    s1:=TimeToStr(ElapsedTime);
    s2:=TimeToStr(((ToDo-ToWrite)*ElapsedTime)/ToWrite);
    t:=ToWrite/(ElapsedTime/100);
    If t > 1024 Then s3:=ZtoS(t/1024)+' K'
    Else s3:=ZtoS(t)+' ';
    M[1]:=@s1; M[2]:=@s2; M[3]:=@s3;
    FormatStr(s, GetString(dlCopyTimeSpeed), M);
    WriteTime:=s;
   End;
{--- finish -------- Eugeny Zvyagintzev ---- 22-03-2003 -----}

   procedure WriteProgress;
   begin
     Info^.Write(6, StrGrd(P^.Size, Wrote, Info^.Size.X - 6, RevertBar));
     Info^.Write(7, WriteCount);
     If ShowCopyTime Then Info^.Write(9, WriteTime); {John_SW 22-03-2003}
   end;

   { **************** }
   { *** FileCopy *** }
   { **************** }

   Var NFBigger: Boolean;    {John_SW}
       DirInfo : lSearchRec; {John_SW}

   begin
     SkipRequested := False; I := -1; WPWas := False;
     if not Abort or not CopyCancel then
         while True do
           begin
1:            Inc(I); if I = CopyQueue^.Count then
              begin
{-NeCoder-}
                if (BytesWrited > 1048576) and (opSys = opWin) then
                begin
                  {$IFDEF OS_DOS}
                  asm
                    mov ah,0dh  { Flush windows cache }
                    int 21h
                  end;
                  {$ENDIF}
                end;
                BytesWrited:=0;
{-NeCoder-}
                if WPWas then WriteProgress;
                Break;
              end;
              P := CopyQueue^.At(I);
              if P^.Attr and Directory <> 0 then
                begin
                  SSS := CnvString(P^.OldName);
                  if SSS[Length(SSS)] = '\' then Dec(SSS[0]);
                  ClrIO;
                  if CopyOptions and cpoMove <> 0 then lRmDir(SSS);
                  if (P^.Owner <> nil) then
                    begin
                      if IOResult = 0 then
                           PFileRec(P^.Owner)^.Attr := PFileRec(P^.Owner)^.Attr or $8000;
                      if (Owner <> nil) then
                         Message(Owner, evCommand, cmCopyUnselect, P^.Owner);
                    end;
                  Continue;
                end;
              if P^.EOF and eoStart <> 0 then
                begin
2:
                  ExAttr := $FFFF;
                  Wrote := 0;
                  Info^.Write(5, GetString(dlFC_Writing)+Cut(CnvString(P^.NewName), 40)+' ');
                  Info^.Write(6, Strg(#177, Info^.Size.X - 6));
                  Info^.Write(7, WriteCount);
                  Dispatch; GrdClick := False;
                  lAssignFile(WriteStream, CnvString(P^.NewName)); ClrIO;
                  FileMode := $42;

                  If (Not CopyPrn) And (CopyOptions And cpoCheckFree <> 0)
                     And (GetDrInfo(Drv) < P^.Size) Then
                   begin
{--- start -------- Eugeny Zvyagintzev ---- 29-08-2002 ----}
{We have to check for existing file's size when not enough free space}
                    NFBigger:=True;
                    lFindFirst(CnvString(P^.NewName),AnyFile-VolumeId,DirInfo);
                    NFBigger:=(DosError = 0) And (P^.Size > DirInfo.FullSize+GetDrInfo(Drv));
                    lFindClose(DirInfo);
                    If NFBigger Then
                     begin
{--- finish -------- Eugeny Zvyagintzev ---- 29-08-2002 ----}
                      PS[1] := Pointer(LongInt(Drv[1]));
                      PS[2] := P^.OldName;
                      lAssignFile(WriteStream, '');
                      ForceDispatch;
                      if CopyOptions and cpoFitAll = 0 then begin
                         MsgResult:= Msg(erNoDiskSpace, @PS, mfError+mfYesButton+mfNoButton+mfAllButton);
                         CopyCancel := (MsgResult <> cmYes) and (MsgResult <> cmOK);
                         if MsgResult = cmOK then CopyOptions:=CopyOptions or cpoFitAll
                      end else CopyCancel:=False;
                      NewTimer(Timer, 0);
                      DoSkip;
                      if not SkipRequested then Goto 1;
                      if (not CopyCancel) then Continue;
                      Break;
                     end;
                   end;

                  if P^.EOF and eoCheck <> 0 then
                    begin
                       {$IFDEF OS_DOS}DisableAppend;{$ENDIF}
                       if Dos40 and Use40 then
                         begin
                           TryToReset;
                           if not Use40 then goto DoNotUse40;
                           case Opened of
                             0 : begin
                                   if Created then Goto lbStartWrite else
                                                   Goto lbStartCheck;
                                 end;
                             5 : goto lbStartCheck;
                            else begin
                                   NoWrite;
                                   Break;
                                 end;
                           end;
                         end else
                         begin
DoNotUse40:
                           ClrIO; FileMode := $40;
                           lResetFile(WriteStream, 1);
                         end;

                       if InOutRes = 0 then
                         begin
lbStartCheck:
                            case CopyMode of
                              cpmOverwrite: Goto DoRewrite;
                              cpmAppend: Goto DoAppend;
{--- start -------- Eugeny Zvyagintzev ---------}
{No DN will not ask for overwrite file if Accept All was choosen}
                              cpmRename: Begin

                                          S1:=CnvString(P^.NewName);
                                          J:=InputBox(GetString(dlFCRename), GetString(dlFCRenameNew),
                                                    S1, 255, 0);
                                          If (J <> cmOK) Or (S1 = '') Then
                                            Begin
                                             DoSkip;
                                             If Not SkipRequested Then Goto 1;
                                             Break;
                                            End;
                                          DisposeStr(P^.NewName);
                                          P^.NewName := NewStr(S1);
                                          NewTimer(Timer, 0);
                                          Goto 2;
                                         End;
{--- finish -------- Eugeny Zvyagintzev ---------}
                              cpmSkipAll: begin
                                             Info^.Write(2, GetString(dlFC_Exists));
                                             Close(WriteStream.f);
                                             DoSkip;
                                             if not SkipRequested then Goto 1;
                                             Break;
                                          end;
                              cpmRefresh: begin
                                            GetFTime(WriteStream.f, A);
                                            if A >= P^.Date then
                                              begin
                                                Info^.Write(2, GetString(dlFC_Older));
                                                Close(WriteStream.f);
                                                DoSkip;
                                                if not SkipRequested then Goto 1;
                                                Break;
                                              end;
                                          end;
                              else begin
                                      ClrIO; Close(WriteStream.f); ClrIO;
                                      GetFTimeSizeAttr(CnvString(P^.NewName), A, BB, ExAttr);
                                      if DosError <> 0 then
                                      begin
                                        NoWrite;
                                        Break;
                                      end;
                                      case Overwrite(CnvString(P^.NewName), BB, P^.Size, A, P^.Date) of
                                         cmSkip: begin
                                                   DoSkip;
                                                   if not SkipRequested then Goto 1;
                                                   Break;
                                                 end;
                                         cmOK: begin
                                                 S1 := CnvString(P^.NewName);
                                                 J := InputBox(GetString(dlFCRename), GetString(dlFCRenameNew),
                                                           S1, 255, 0);
                                                 if (J <> cmOK) or (S1 = '')  then
                                                   begin
                                                      DoSkip;
                                                      if not SkipRequested then Goto 1;
                                                      Break;
                                                   end;
                                                 DisposeStr(P^.NewName);
                                                 P^.NewName := NewStr(S1);
                                                 NewTimer(Timer, 0);
                                                 Goto 2;
                                               end;
                                         cmNo: Goto doAppend;
                                         cmYes: Goto doRewrite;
                                           else begin
                                                  CopyCancel := True;
                                                  Break;
                                                end;
                                      end;
                                   end;
                            end;
                         end;
                    end;
                  if P^.EOF and eoAppend <> 0 then
                    begin
DoAppend:
                      {$IFDEF OS_DOS}DisableAppend;{$ENDIF}
                      FileMode := $42;
                      lResetFile(WriteStream, 1);
                      J := IOResult;
                      case J of
                        0 : begin
                              ExAttr := $FD00;
                              lSeek(WriteStream,0,2);
                            end;
                        2 : Goto DoRewrite;
                        5 : begin
                              if ExAttr = $FFFF then
                              begin
                                ClrIO;
                                {$IFDEF OS_DOS}DisableAppend;{$ENDIF}
                                lGetFAttr(WriteStream, ExAttr);
                                if DosError = 0 then TrySetArch
                              end else TrySetArch;
                              if CheckI24Abort then Break ;
                              ExAttr := ExAttr or $FE00;
                              {$IFDEF OS_DOS}DisableAppend;{$ENDIF}
                              lResetFile(WriteStream, 1);
                              if IOResult = 0 then lSeek(WriteStream, 0,2);
                            end;
                       end;
                    end else
                          begin
DoRewrite:
                             ClrIo;
                             {$IFDEF OS_DOS}DisableAppend;{$ENDIF}
                             lRewriteFile(WriteStream, 1);
                             if CheckI24Abort then Break ;
                             if (InOutRes = 5) then
                             begin
                                ClrIO;
                                if ExAttr = $FFFF then
                                begin
                                  {$IFDEF OS_DOS}DisableAppend;{$ENDIF}
                                  lGetFAttr(WriteStream, ExAttr);
                                  if DosError = 0 then TrySetArch;
                                end else TrySetArch;
                                if CheckI24Abort then Break ;
                                {$IFDEF OS_DOS}DisableAppend;{$ENDIF}
                                lRewriteFile(WriteStream, 1);
                             end;
                          end;

                  if CheckI24Abort then Break ;
                  if IOResult <> 0 then
                    begin
                      NoWrite;
                      Break;
                    end;
                end;
lbStartWrite:
              P^.Read(B^);
              ClrIO;
              Dispatch;
              if CopyCancel then Break;

              BlockWrite(WriteStream.F, B^, P^.Len, J); IOR := IOResult;

              Inc(BytesWrited,P^.Len);  {-NeCoder-}

              If Not Abort And (CopyOptions and cpoVerify <> 0) And
                 (J > 0) And (Not CopyPrn) Then Begin
                   {$IFDEF OS_DOS}
                   asm
                     mov ah,0dh
                     int 21h
                   end;
                   {$ENDIF}

                   BufCRC := GetCrc( B^ , P^.Len );

                   L:=J;
                   lSeek(WriteStream, 0-L, 2); ClrIO;
                   BlockRead(WriteStream.F, B^, P^.Len, J);

                   if CheckI24Abort then Break ;

                   if (IoResult <> 0) or
                      (J <> P^.Len ) or
                      ( GetCrc( B^ , P^.Len ) <> BufCrc )
                      then
                      begin
                       ForceDispatch;
                       MessageBox(GetString(dlFCVerifyFailed), nil, mfError+mfOKButton);
                       CopyCancel := True ;
                       Break ;
                      end;
                 end;

              Dispatch;
              if Abort or CopyCancel then Break;
              Wrote:=Wrote+J;
              ToWrite:=ToWrite+J;

              if GrdClick then
              begin
                WriteProgress;
                GrdClick := False;
                WPWas := False
              end else WPWas := True;

              if ( J <> P^.Len ) or ( IOR <> 0 ) then begin
                CopyCancel := True;
                ForceDispatch;
                if (CopyPrn) or
                   (IOR = 5) or
                   (GetDrInfo(Drv) > P^.Size)
                 then NoWrite else ErrMsg(dlFBBDiskFull2);
                Break;
              end;

              if P^.EOF and eoEnd <> 0 then
                begin
                  case ExAttr shr 8 of
                    $FD, $FE : ;
                    else SetFTime(WriteStream.F, P^.Date);
                  end;
                  WriteProgress; WPWas := False;
                  Close(WriteStream.F);
                  {$IFDEF OS_DOS}DisableAppend;{$ENDIF}
                  case ExAttr shr 8 of
                    $FF : { Set Source Attribute, if Necessary }
                      if P^.Attr <> Archive then
                               lSetFAttr(WriteStream, P^.Attr);
                    $FE : { Set Dest' Attribute }
                               lSetFAttr(WriteStream, ExAttr and $FF);
                    $FD : { Don't Set any attribute } ;
                   else   { Set Source Attribute, Force }
                               lSetFAttr(WriteStream, P^.Attr);
                  end;
                  if (P^.Owner <> nil) then
                    begin
                      PFileRec(P^.Owner)^.Attr := PFileRec(P^.Owner)^.Attr or $8000;
                      if (Owner <> nil) then
                        Message(Owner, evCommand, cmCopyUnselect, P^.Owner);
                      if (PFileRec(P^.Owner)^.DIZ <> nil) and
                        (CopyOptions and cpoDesc <> 0)
                      then begin
                        Info^.Write(6, GetString(dlExportingDIZ));
                        ImportDIZ(MakeFileName(PFileRec(P^.Owner)^.Name),
                          GetName(lfGetShortFileName(P^.NewName^)),
                          GetLFN(PFileRec(P^.Owner)^.LFN),
                          PFileRec(P^.Owner)^.DIZ, PFileRec(P^.Owner)^.Owner);
                      end;
                    end;
                  if CopyOptions and cpoFromTemp <> 0 then RemoveFromTemp(P^.Owner);
                  if CopyOptions and cpoMove <> 0 then
                    begin
                      Info^.Write(6, GetString(dlDeletingSource));
                      EraseFile( CnvString( P^.OldName ));
                    end;
                end;
           end;
     FreeDisk := -1;
     CopyQueue^.FreeAll;
     if MemStream <> nil then MemStream^.Seek(0);
{$IFNDEF NOEXTRA}
     if EMSStream <> nil then EMSStream^.Seek(0);
     if XMSStream <> nil then XMSStream^.Seek(0);
{$ENDIF}
   end;

   procedure MakeBuffer;
     var I: LongInt;
   begin
      I := MemAvail - $4000;
      if I > MaxAvail-$2000 then I := MaxAvail-$2000;
      if I < 512 then Exit;
      if I > $E000 then I := $C000 else
       if I > $C000 then I := $A000 else
        if I > $8000 then I := $6000 else
         if I > $4000 then I := $2000 else
          if I > $1000 then I := $0800 else I := 0;
      B := MemAlloc(I);
      BSize := I;
   end;

   var DOSErrorCode: Word;

   procedure RnFl(N1,N2 : String);
    var F: lFile;
   begin
    lAssignFile(F, N1);
    lRenameFile(F, N2);
    DOSErrorCode := IOResult;
   end;

   procedure CopyFile(const FName, AddDir: String; Own: Pointer; Ln:TSize; Dtt: LongInt; Attr: Word);
     label 1;
     var P: PLine;
         A, BB, WW: LongInt;
         Rd: TSize;
         I, J: Word;
         FFF: Boolean;
         Was: TSize;
         EOF: Byte;
         NName,S1,S2: String;
         SR: lSearchRec;
         PS: Array [1..2] of Pointer;

     procedure RenameFile;
      var
          F: lFile;
          W: Word;
          SR: lSearchRec;
     begin
      DOSErrorCode := 0;
       RnFl(S1, S2);
       if (DOSErrorCode = 5) or (DOSErrorCode = 183) { Kirill } then
         begin
          lAssignFile(f, S2);
          ClrIO;
          lSetFAttr(F, Archive);
          ClrIO;
          lEraseFile(F);
          RnFl(S1,S2);
         end;
       if (DOSErrorCode = 5) or (DOSErrorCode = 183) { Kirill } then
         begin
           lAssignFile(F, S1);
           lGetFAttr(F, W);
           lSetFAttr(F, Archive);
           ClrIO;
           RnFl(S1, S2);
           lSetFAttr(F, W);
           ClrIO;
         end;
       if not (DOSErrorCode in [0,18]) then
        begin
          ForceDispatch;
          MessageBox(GetString(dlFCNoRename1)+GetString(dlDIFile)+^M^C+Cut(S1,40)
                    +GetString(dlFCNoRename2)+Cut(S2,40), nil, mfError+mfOKButton);
          CopyCancel := True;
        end;
     end;

     procedure DoRename;
       var A: Array [0..10] of PString;
           I,J: Integer;
     begin
       for I := 0 to Min(10, Info^.Lines^.Count-1) do
        begin
          A[I] := Info^.Lines^.At(I);
          Info^.Lines^.AtPut(I, nil);
        end;
       Info^.Write(2, Cut(FName,40));
       Info^.Write(4, GetString(dlFC_To));
       Info^.Write(6, Cut(NName,40));
       Dispatch;
       if not CopyCancel then RenameFile;
       if not CopyCancel and (Own <> nil) then
         begin
          PFileRec(Own)^.Attr := PFileRec(Own)^.Attr or $8000;
          if (PFileRec(Own)^.DIZ <> nil) and
            (CopyOptions and cpoDesc <> 0) then
          begin
             Info^.Write(6, GetString(dlExportingDIZ));
             ImportDIZ(MakeFileName(PFileRec(Own)^.Name),
               GetName(lfGetShortFileName(NName)),
               GetLFN(PFileRec(Own)^.LFN),
               PFileRec(Own)^.DIZ, PFileRec(Own)^.Owner);
          end;
          {$IFNDEF OS2}
          PFileRec(Own)^.Name := Norm12(GetName(lfGetShortFileName(NName)));
          PFileRec(Own)^.Name[9] := ' ';
          {$ENDIF}
          DelLFN(PFileRec(Own)^.LFN);
          PFileRec(Own)^.LFN := AddLFN(GetName(NName));
          if (Owner <> nil) then Message(Owner, evCommand, cmCopyUnselect, Own);
          ToWrite:=ToWrite+Ln;
          ToRead:=ToRead+Ln;
         end;
       Info^.DrawView;
       for I := 0 to Min(10, Info^.Lines^.Count-1) do
          Info^.Lines^.AtReplace(I, A[I]);
     end;

   label NoRename,NoRename1;

   { Flash >>> 04-11-2002 }
   function ReadCount: string;
   var S: string;
   begin
     ReadCount := FStr(ToRead)+GetString(dlBytes)+' ('+GetPercent(ToRead)+')'+GetString(dlFC_WasRead);
     S:=GetPercent((ToWrite+ToRead)/2)+' '+GetString(dlCopied);
     if OldTitle=S then Exit;
     SetTitle(S);
     OldTitle:=S;
   end;
   { Flash <<< 04-11-2002 }

   procedure ReadProgress;
   begin
     Info^.Write(2, StrGrd(Ln, ReadPos, Info^.Size.X - 6, RevertBar));
     Info^.Write(3, ReadCount);
   end;

   var DE: integer;
       D : PDialog; {John_SW}
       R : TRect;   {John_SW}
       S : String;  {John_SW}
   begin
{JO: !!! не копируем файлы найденные в архивах  }
      {if PathFoundInArc(FName) then Exit;} { Commented by Flash 26-03-2003 }
{/JO}
      if CopyPrn then
      begin
        NName := CopyDir;
        while NName[Length(NName)]='\' do dec(NName[0]);
      end else
      begin
        if SoftMode then
          NName := MakeNormName(GetPath(FName), MkName(GetName(FName)))
        else
          if AddDir = '' then NName := MkName(GetName(FName))
                         else NName := MakeNormName(AddDir, GetName(FName));

        NName := MakeNormName(CopyDir, NName);
     1:
        S1 := lFExpand(FName);
        S2 := lFExpand(NName);
        if S1 = S2 then
           begin
{--- start -------- Eugeny Zvyagintzev ---------}
{This 2 line was commented to allow DN display error message}
{during _move_ file to itself}
{Before this comment DN displayed error message only}
{during _copy_ file to itself}
{>>             if CopyOptions and cpoMove = 0 then}
{>>              begin}
{--- finish -------- Eugeny Zvyagintzev ---------}
                if CopyQueue^.Count > 0 then MaxWrite; if CopyCancel then Exit;
                ForceDispatch;
                MessageBox(^C+GetString(dlFile)+' '+FName + GetString(dlFCItself), nil,
                   mfError + mfOKButton);
                CopyCancel := True;
{              end;}
             Exit;
           end;
      end;

      Was := 0;
      ClrIO; lAssignFile(ReadStream, FName);
      if S1[1] in CD_Drives then Attr := Attr and not ReadOnly;

      FileMode := $40;
      {$IFDEF OS_DOS}DisableAppend;{$ENDIF}
      lResetFile(ReadStream, 1); ReadPos := 0;

      if Abort then
      begin
         ClrIO;
         if CopyQueue^.Count > 0 then MaxWrite;
         CopyCancel := True;
         Exit;
      end;

      if IOResult <> 0 then
        begin
           ClrIO;
           if CopyQueue^.Count > 0 then MaxWrite; if CopyCancel then Exit;
           ForceDispatch;
{--- start -------- Eugeny Zvyagintzev ---- 22-10-2002 ----}
           If SkipAllBad Then Exit;
           D := PDialog(LoadResource(dlgSkipBadFile));
           D^.Options := D^.Options or ofCentered;
           S:=Cut(FName,52);
           R.A.X:=1; R.A.Y:=3; R.B.X:=53; R.B.Y:=4;
           D^.Insert(New(PStaticText, Init(R,^C+S)));
           Case Desktop^.ExecView(D) Of
            cmOK: Goto 1;
            cmYes: Begin SkipAllBad:=True; Exit; End;
            cmNo: Exit;
            cmCancel: CopyCancel:=True;
           End;
{--- finish -------- Eugeny Zvyagintzev ---- 22-10-2002 ----}
        end;
      Rd := 0;
      EOF := eoStart or eoCheck * Byte(CopyMode >= cpmAppend);
{--- start -------- Eugeny Zvyagintzev ---------}
{This 3 line was commented to prevent DN first coping files to buffer}
{and _then_ check for file already exists}
{Now DN will first check for file exists}
{>>      if (CopyMode >= cpmAppend) and (CopyOptions and cpoMove <> 0)  and}
{>>          (S1[1] = S2[1]) then}
{>>        begin}
{--- finish -------- Eugeny Zvyagintzev ---------}
           ClrIO; EOF := eoStart or eoCheck;
           lFindFirst(S2, AnyFile, SR); {JO}
           DE:=DOSError;
           lFindClose(SR);
           if DE = 0 then
             begin
               EOF := eoStart;
               Was := SR.FullSize;
               if SR.SR.Attr and Directory <> 0 then
                   begin
                      if CopyQueue^.Count > 0 then MaxWrite; if CopyCancel then Exit;
                      ForceDispatch;
                      MessageBox(GetString(dlFCNotOverDir)+Cut(S2,40), nil, mfError+mfOKButton);
                      Exit;
                   end;
               case CopyMode of
                 cpmAskOver: NoRename:
                             case Overwrite(NName, SR.FullSize, Ln, SR.SR.Time, Dtt) of
                               cmSkip: begin ClrIO; Close(ReadStream.F); ClrIO; Exit; end;
                               cmOK: begin
                                       ClrIO; Close(ReadStream.F);
                                       ClrIO; Close(WriteStream.F); ClrIO;
                                       if InputBox(GetString(dlFCRename), GetString(dlFCRenameNew),
                                            NName, 255, 0) <> cmOK then goto NoRename;
                                       NewTimer(Timer, 0);
                                       Goto 1;
                                     end;
                               cmNo: EOF := EOF or eoAppend;
                             end;
{--- start -------- Eugeny Zvyagintzev ---------}
{No DN will not ask for overwrite file if Accept All was choosen}
                 cpmRename: NoRename1:
                             Begin
                              ClrIO; Close(ReadStream.F);
                              ClrIO; Close(WriteStream.F); ClrIO;
                              If InputBox(GetString(dlFCRename), GetString(dlFCRenameNew),
                                   NName, 255, 0) <> cmOK Then Goto NoRename1;
                              NewTimer(Timer, 0);
                              Goto 1;
                             End;
{--- finish -------- Eugeny Zvyagintzev ---------}
                 cpmRefresh: if Dtt <= SR.SR.Time then
                               begin
                                 Info^.Write(1, GetString(dlFC_Reading)+Cut(FName, 40));
                                 Info^.Write(2, GetString(dlFC_Older));
                                 ClrIO; Close(ReadStream.F); ClrIO;
                                 Exit;
                               end;
                 cpmSkipAll: begin
                               Info^.Write(1, GetString(dlFC_Reading)+Cut(FName, 40));
                               Info^.Write(2, GetString(dlFC_Exists));
                               ClrIO; Close(ReadStream.F); ClrIO;
                               Exit;
                             end;
                 cpmAppend: EOF := EOF or eoAppend;
               end;
             end;
{        end;}
      if (CopyOptions and cpoMove <> 0) and (EOF and eoAppend = 0) and
         (S1[1] = S2[1]) then
         begin
           ClrIO; Close(ReadStream.F); ClrIO;
           DoRename;
           Exit
         end;
      if B = nil then MakeBuffer;
      if B = nil then
         begin
           CopyCancel := True;
           Application^.OutOfMemory;
           ClrIO; Close(ReadStream.F); ClrIO;
           Exit;
         end;
      Info^.Write(1, GetString(dlFC_Reading)+Cut(FName, 40)+' ');
      Info^.Write(2, Strg(#177, Info^.Size.X - 6));
      Info^.Write(3, ReadCount);
      Dispatch; GrdClick := False;
      {Info^.DrawView;}
      {Ln:=lFileSize(ReadStream); {-$VOL}

      repeat
        if CopyQueue^.Count > 400 then MaxWrite; {John_SW  06-11-2002}
        SkipRequested := False;
        if Ln-Rd > BSize then WW := BSize
        else ww:=Round(Ln-Rd);
        P := nil;
{$IFNDEF NOEXTRA}
        if AdvCopy then
        begin
          P := New(PXMSLine, Init( WW,  Own, FName, NName, Ln, Dtt, Attr, EOF));
          if P = nil then P := New(PEMSLine, Init( WW,  Own, FName, NName, Ln, Dtt, Attr, EOF));
        end;
{$ENDIF}
        if P = nil then P := New(PLine, Init( WW,  Own, FName, NName, Ln, Dtt, Attr, EOF));
        if P = nil then
          begin
            if CopyQueue^.Count = 0 then
               begin
                 ClrIO; Close(ReadStream.F); ClrIO;
                 CopyCancel := True;
                 if B <> nil then FreeMem(B, BSize);
                 B := nil;
                 Application^.OutOfMemory;
                 Exit;
               end;
            ReadProgress;
            MaxWrite;
            if SkipRequested then Break;
            Continue
          end;
        J := 0;
        Dispatch;
        if WW > 0 then BlockRead(ReadStream.F, B^, WW, J); ReadPos:=ReadPos+J;
        Dispatch;
        if CopyCancel then Break;
        if Abort then
          begin
             ClrIO;
             if CopyQueue^.Count > 0 then MaxWrite;
             CopyCancel := True;
             Break;
          end;
        if ReadPos >= Ln then
          begin
            EOF := EOF or eoEnd;
            if P^.OldName = nil then P^.OldName := NewStr(FName);
          end;
        P^.EOF := EOF;
        if WW <> J then
          begin
            CopyCancel := True;
            Break;
          end;

        ToRead:=ToRead+J;

        Dispatch;
        if GrdClick then
        begin
          ReadProgress;
          GrdClick := False;
        end;

        P^.Write(B^);
        CopyQueue^.Insert(P);
        if Abort or CopyCancel then Break;
        EOF := EOF and not eoStart;
        Rd:=Rd+WW;
      until CopyCancel or Abort or (Rd = Ln) or System.EOF(ReadStream.F) or SkipRequested; {-$VOL}
      SkipRequested := False;
      ClrIO;
      ReadProgress;
      Close(ReadStream.F);
      if not AdvCopy then MaxWrite;
   end;

   procedure CopyDirectory(const DirName, AddDir: String; Own: Pointer);
     var SR: lSearchRec;
         P: PLine;
         L: LongInt;
   begin
     ClrIO;
     lFindFirst(DirName+'\*.*', AnyFile, SR); {JO}
     while (DOSError = 0) and not Abort and not CopyCancel do
       begin
         if NoCopyLFN then SSS := MakeNormName(AddDir,SR.SR.Name)
                      else SSS := MakeNormName(AddDir,SR.FullName);
         if SR.SR.Attr and Directory = 0 then
           begin
            If NoCopyLFN then
              CopyFile(DirName+'\'+SR.SR.Name, Copy(AddDir,
                       Length(CopyDir)+1,255), nil, SR.FullSize,
                       SR.SR.Time, SR.SR.Attr)
            else
              CopyFile(DirName+'\'+SR.FullName, Copy(AddDir,
                       Length(CopyDir)+1,255), nil, SR.FullSize,
                       SR.SR.Time, SR.SR.Attr);
           end;
         if CopyCancel or Abort then Exit;
         ClrIO;
         lFindNext(SR);
       end;
     lFindClose(SR);
     if CopyCancel or Abort then Exit;
     L := 0;
     P := New(PLine, Init( L,  Own, DirName, '', 0, 0, Directory, eoStart+eoDir));
     if P <> nil then CopyQueue^.Insert(P);
   end;

   procedure MaxRead;
     var I: Integer;
         P: PFileRec;
         S: String;

     procedure DoCopyDirectory(P: PDirName); {$IFDEF BIT_16}far;{$ENDIF}
     begin
       if P^.CopyIt then CopyDirectory(P^.DOld, P^.DNew, P^.Own);
     end;

   begin
     StartTime:=Get100s;
     for I := 0 to Files^.Count - 1 do
       begin
         P := Files^.At(I);
         if NoCopyLFN then S := MakeNormName(P^.Owner^, MakeFileName(P^.Name))
                      else S := MakeNormName(P^.Owner^, GetLFN(P^.LFN));
         if P^.Attr and Directory = 0 then CopyFile(S,'',P, P^.Size, PackedDate(P), P^.Attr);
         if Abort or CopyCancel then Exit;
       end;
     if Abort or CopyCancel then Exit;
     if Dirs <> nil then Dirs^.ForEach(@DoCopyDirectory);
     MaxWrite;
   end;

  procedure DoneStreams;
  begin
    if MemStream <> nil then
      begin
         MemStream^.Seek(0);
         MemStream^.Read(SSS,1);
         Dispose(MemStream,Done);
         MemStream:=nil;
      end;
{$IFNDEF NOEXTRA}
    if EMSStream <> nil then
      begin
         EMSStream^.Seek(0);
         EMSStream^.Read(SSS,1);
         Dispose(EMSStream,Done);
         EMSStream:=nil;
      end;
    if XMSStream <> nil then
      begin
         Dispose(XMSStream,Done);
         XMSStream:=nil;
      end;
{$ENDIF}
  end;

  procedure MakeDirectories;
    var P: PFileRec;
        PD: PDirName;
        I: Integer;
        S: String;
        FrPos: Integer;

    procedure CopyI(Dest, Source: String;
                    Name: String;
                    O: PFileRec; CopyIt: Boolean;
                    Attr: Byte);
      label 1, 2;
      var JJ: Integer;
      var q: string;
    begin
      Info^.Write(5, GetString(dlFCCheckingDirs));
      if (not CopyPrn) and (Dest <> '') and (Dest[Length(Dest)] <> '\') then AddStr(Dest, '\');
      q:=MakeNormName(Dest,Name); if q[length(q)]='.' then dec(q[0]);
      if O = nil then Goto 1;
      if CopyCancel then Goto 2;
      ClrIO;
      SSS := Source;
      SSS := SSS+Copy('\',1,Byte(SSS[Length(SSS)] <> '\'));
      if SSS = Copy(S, 1, Length(SSS)) then
        begin
          ForceDispatch;
          MessageBox(^C + GetString(dlDirectory) + ' ' + Cut(Source, 40) +
                     GetString(erIntoItself), nil, mfError+mfOKButton);
          CopyCancel := True;
          Exit;
        end;
      Dec(SSS[0]);
      if (O <> nil) and (CopyOptions and cpoMove <> 0) and (GetPath(SSS)=S) then
        begin
          CopyIt := False;
          O^.Attr := O^.Attr or $C000;
          Info^.Write(5, '');
          Info^.Write(2, Cut(SSS,40));
          Info^.Write(4, GetString(dlFC_To));
          Source := lFExpand(q);
          Info^.Write(6, Cut(Source,40));
          ClrIO;
          RnFl(SSS, q);
          if DOSErrorCode = 5 then
            begin
              lGetDir(0, ActiveDir);
              Advance2.lChDir(GetPath(SSS));
              RnFl(GetName(SSS), Name);
              Advance2.lChDir(ActiveDir);
            end;
          if Abort then begin CopyCancel := True; Goto 2; end;
          if DOSErrorCode <> 0 then
            begin
              ForceDispatch;
              MessageBox(GetString(dlNotRenameDir)+Source, nil,
                           mfError + mfOKButton);
              CopyCancel := True;
              Goto 2;
            end;
          CreateDirectory(q, False);
          CreateDirectory(SSS, True);
          Info^.Write(2, ''); Info^.Write(4, ''); Info^.Write(6, '');
          Info^.Write(5, GetString(dlFCCheckingDirs));
          {$IFNDEF OS2}
          O^.Name := Norm12(GetName(lfGetShortFileName(MakeNormName(GetPath(Source),Name))));
          O^.Name[9] := ' ';
          {$ENDIF}
          DelLFN(O^.LFN);
          O^.LFN := AddLFN(Name);
          if Owner <> nil then Message(Owner, evCommand, cmCopyUnselect, O);
          O^.Attr := O^.Attr or VolumeID;
          Goto 1;
        end else
             begin
               ClrIO;
               if not CopyPrn then
               begin
                 lMkDir(q);
                 if IOResult = 0 then
                    SetFileAttr(q+#0, Attr and not Directory)
               end;
             end;
      if Abort then begin CopyCancel := True; Goto 2; end;
      if not CopyPrn then CreateDirectory(q, False);
  1:
     if CopyPrn then
     begin
       if Dest = '' then SSS := Name else SSS := Dest;
     end else SSS := q;
     Dirs^.AtInsert(FrPos, New(PDirName, Init(Source, SSS, CopyIt, O, Attr)));
     Inc(FrPos);
  2:
    end;

    procedure CopyF(Dest, Source: String; CopyIt: Boolean; Attr: Byte);
      var SR: lSearchRec;
    begin
      if Dest[Length(Dest)] = '\' then Dec(Dest[0]);
      if Dest[length(Dest)] = '.' then Dec(Dest[0]);
      ClrIO;
      if not CopyPrn then
      begin
        lMkDir(Dest);
        if IOResult = 0 then
          begin
             SetFileAttr(Dest+#0, Attr and not Directory);
             CreateDirectory(Dest, False);
          end;
        ClrIO;
      end;
      lFindFirst(Source+'\*.*', AnyFile, SR); {JO}
      while (DOSError = 0) and not Abort and not CopyCancel do
        begin
          if (SR.SR.Attr and Directory <> 0) and (SR.SR.Name[1] <> '.') then
          begin
           if NoCopyLFN
            then CopyI(Dest, MakeNormName(Source,SR.SR.Name),  SR.SR.Name,  nil,
                       CopyIt, SR.SR.Attr)
            else CopyI(Dest, MakeNormName(Source,SR.FullName), SR.FullName, nil,
                       CopyIt, SR.SR.Attr)
          end else begin
              ToDo:=ToDo+SR.FullSize;
              if (UpCase(Source[1])<>UpCase(Dest[1])) or (CopyOptions and cpoMove=0)
              then begin ToDoCopy:=ToDoCopy+SR.FullSize;
                         if BytesPerCluster=0 then BytesPerCluster:=1;
                         if SR.FullSize>0 then begin
                          ToDoClusCopyTemp:=SR.FullSize / BytesPerCluster;
                          ToDoClusCopy:=ToDoClusCopy +
                            (Round(ToDoClusCopyTemp) +
                             Byte(Round(ToDoClusCopyTemp)-ToDoClusCopyTemp<>0)
                            ) * BytesPerCluster;
                         end;
                   end;
          end;
          ClrIO;
          Dispatch;
          if not CopyCancel then lFindNext(SR);
        end;
      lFindClose(SR);
    end;

    label 1;

    function NoCheck(P: PDirName): Boolean; {$IFDEF BIT_16}far;{$ENDIF}
    begin
      Inc(FrPos);
      NoCheck := not P^.Check;
    end;


  begin
    New(Dirs, Init(10, 10));

    if not CopyPrn then
    begin
      S := CopyDir;
      Inhr := CreateDirInheritance(S, True);
      if Inhr = 0 then Inhr := Length(S);
      Drv := S;
      ReD := [S[1]];
    end else Drv := '';
    if Drv <> '' then GetDrInfo(Drv);{piwamoto}
    for I := 0 to Files^.Count - 1 do
      begin
        P := Files^.At(I);
        if P^.Attr and Directory <> 0 then
         begin
            FrPos := Dirs^.Count;
            if NoCopyLFN then CopyI(CopyDir, MakeNormName(P^.Owner^, MakeFileName(P^.Name)),
                                    MkName(MakeFileName(P^.Name)),P, True, P^.Attr and $3FFF)
                         else CopyI(CopyDir, MakeNormName(P^.Owner^, GetLFN(P^.LFN)),
                                    MkName(GetLFN(P^.LFN)),P, True, P^.Attr and $3FFF);
            if not (Abort or CopyCancel)
              and (P^.DIZ <> nil)
              and (CopyOptions and cpoDesc <> 0)
            then
              ImportDIZ(LowStrg(P^.Name), MakeFileName(P^.Name),
                        GetLFN(P^.LFN), P^.DIZ, P^.Owner);
         end else begin
             ToDo:=ToDo+P^.Size;
             if (CopyDir[1]<>P^.Owner^[1]) or (CopyOptions and cpoMove=0)
             then begin ToDoCopy:=ToDoCopy+P^.Size;
                        If BytesPerCluster=0 then BytesPerCluster:=1;
                        if P^.Size>0 then begin
                          ToDoClusCopyTemp:=P^.Size / BytesPerCluster;
                          ToDoClusCopy:=ToDoClusCopy +
                            (Round(ToDoClusCopyTemp) +
                             Byte(Round(ToDoClusCopyTemp)-ToDoClusCopyTemp<>0)
                            ) * BytesPerCluster;
                        end;
                  end;
         end;
        if CopyCancel then Break;
      end;
1:  repeat
      FrPos := -1;
      PD := Dirs^.FirstThat(@NoCheck);
      if PD = nil then Break;
      CopyF(PD^.DNew, PD^.DOld, PD^.CopyIt, PD^.Attr and $3FFF);
      PD^.Check := True;
    until False;
    Info^.Write(5, '');
  end;

 procedure __Remove;
 var
   RRC: PStringCollection;

 procedure DoRemove(P: PFileRec); {$IFDEF BIT_16}far;{$ENDIF}

   procedure MakeMark(P: PFileRec); {$IFDEF BIT_16}far;{$ENDIF}
   begin
     if (Copy(P^.Owner^,1,Length(SSS))=SSS) then
          P^.Attr := P^.Attr or $4000;
   end;

 begin
    if (P^.Attr and $8000 <> 0) then
      begin
        if P^.Attr and (Directory+VolumeID) = Directory then
           begin
            if NoCopyLFN
             then CreateDirectory(MakeNormName(P^.Owner^, MakeFileName(P^.Name)), True)
             else CreateDirectory(MakeNormName(P^.Owner^, GetLFN(P^.LFN)), True);
           end;
      end;
    if P^.Attr and $4000 = 0 then
      begin
        SSS := CnvString(P^.Owner);
        if SSS[Length(SSS)] = '\' then Dec(SSS[0]);
        if Copy(CopyDir, 1, Length(SSS))=SSS then Inhr := 0;
        RRC^.Insert(NewStr(SSS));
        if P^.Attr and Directory <> 0 then ReD := ReD + [UpCase(SSS[1])];
        Files^.ForEach(@MakeMark);
      end;
 end;

 procedure DoReread(P: PString); {$IFDEF BIT_16}far;{$ENDIF}
 begin
   RereadDirectory(P^);
 end;

 begin
   RRC := New(PStringCollection, Init($10, $8));
   Files^.ForEach(@DoRemove);
   RRC^.ForEach(@DoReread);
   Dispose(RRC,Done);
 end;


 procedure DoReset(P: PFileRec); {$IFDEF BIT_16}far;{$ENDIF}
 var
     i: integer;
     Line: Longint;
     s, s2: string;
 begin
    P^.Attr := P^.Attr and $3FFF;
    CD_Drives := CD_Drives + [UpCase(P^.Owner^[1])];
    If (CopyOptions and cpoDesc)<>0 then
     begin
      if P^.DIZ=nil then begin
       s2:='';
       i:=1;
       repeat
        s:=GetPossibleDizOwner(i);
        Inc(i);
        if s<>'' then S2:=GetDIZ(s, Line, MakeFileName(P^.Name), GetLFN(P^.LFN));
       until (s='') or (s2<>'');
       if s2<>'' then begin
        New(P^.DIZ);
        P^.DIZ^.Owner:=NewStr(S);
        P^.DIZ^.DIZ:=NewStr(S2);
        P^.DIZ^.Line:= Line;
        P^.DIZ^.isDisposable:=True;
       end;
      end;
     end;
 end;

{--- start -------- Eugeny Zvyagintzev ---- 10-02-2003 ----}
Procedure EnableFFP(View: PView); {$IFDEF BIT_16}Far;{$ENDIF}
Var Event: TEvent;
Begin
 Event.What:=evCommand; Event.Command:=cmEnableFileFindPanels;
 Event.InfoPtr:=Nil;
 View^.HandleEvent(Event);
End;

Procedure DisableFFP(View: PView); {$IFDEF BIT_16}Far;{$ENDIF}
Var Event: TEvent;
Begin
 Event.What:=evCommand; Event.Command:=cmDisableFileFindPanels;
 Event.InfoPtr:=Nil;
 View^.HandleEvent(Event);
End;

Procedure RereadFFP(View: PView); {$IFDEF BIT_16}Far;{$ENDIF}
Var Event: TEvent;
Begin
 Event.What:=evCommand; Event.Command:=cmReReadFileFindPanels;
 Event.InfoPtr:=Nil;
 View^.HandleEvent(Event);
End;

{--- finish -------- Eugeny Zvyagintzev ---- 10-02-2003 ----}

 label 1;

var
  Flush: Boolean;

   { ***************** }
   { *** FilesCopy *** }
   { ***************** }
label qqqq;
begin
  if (CopyOptions and cpoMove <> 0) And
     (Files^.Count=1) And
     (PFileRec(Files^.At(0))^.Attr and Directory<>0) and
     (UpStrg(CopyDir[1])=
      UpStrg(Copy(CnvString(PFileRec(Files^.At(0))^.Owner),1,1))
     ) and
     (UpStrg(MakeNormName(CopyDir,''))<>
      UpStrg(MakeNormName(CnvString(PFileRec(Files^.At(0))^.Owner),''))
     ) then begin
   lAssignFile( ReadStream,
                MakeNormName(
                 CnvString(PFileRec(Files^.At(0))^.Owner),
                 GetLFN(PFileRec(Files^.At(0))^.LFN)
                )
              );
   ClrIO;
   if NoCopyLFN
    then SSS:=MkName(MakeFileName(PFileRec(Files^.At(0))^.Name))
    else SSS:=MkName(GetLFN(PFileRec(Files^.At(0))^.LFN));
   SSS:=MakeNormName(CopyDir,SSS);
   lRenameFile( ReadStream, SSS );
   If IOResult<>0 then goto qqqq;
   GlobalMessage(evCommand, cmPanelReread,  PFileRec(Files^.At(0))^.Owner);
   GlobalMessage(evCommand, cmPanelReread,  @CopyDir);
  end else begin
qqqq:
  {$IFNDEF NONBP}AppendCheck;{$ENDIF}
  Dirs := nil;
  AdvCopy := (not CopyPrn) and (SystemData.Options and ossAdvCopy <> 0);
  Use40 := Dos40 and AdvCopy;
  SoftMode := (CopyOptions and cpoDirEmpty <> 0) and
              (CopyOptions and (cpoMove+cpoFromTemp) = (cpoMove+cpoFromTemp));
  NewTimer(Timer, 0);
  NewTimer(_Tmr, 0); GrdClick := True;
  FreeDisk := -1;
  CopyCancel := False;
  SkipAllBad:=False; {John_SW  05-11-2002}
  ReD := [];
  CD_Drives := [];
  ToRead := 0; ToWrite := 0; ToDo := 0; ToDoCopy := 0; ToDoClusCopy := 0;
  If ShowCopyTime Then R.Assign(0,0,60,15) {John_SW 22-03-2003}
  Else R.Assign(0,0,40,13);
  New(Info, Init(R));
  if CopyOptions and cpoMove <> 0 then Info^.Top := GetString(dlFCMove)
                                  else Info^.Top := GetString(dlFCCopy);
  Info^.Options := Info^.Options or ofSelectable or ofCentered; {JOHN_SW}
  Info^.HelpCtx:=hcCopyInProgress;                              {JOHN_SW}
  Info^.SetState(sfModal,True);                                 {JOHN_SW}
  Desktop^.Insert(Info);
  Files^.ForEach(@DoReset);
  if (SystemData.Options and ossRemoveCD_RO <> 0) and CDRomInstalled then
    begin
       for C := 'A' to 'Z' do
        if (c in CD_Drives) and not IsDriveCDROM(c) then
           CD_Drives := CD_Drives - [c];
    end else CD_Drives := [];
  MakeDirectories;
  if CopyCancel then Goto 1;
  MemStream := nil;
{$IFNDEF NOEXTRA}
  EMSStream := nil; NoUseEMS := False;
  XMSStream := nil; NoUseXMS := False;
{$ENDIF}
  B := nil;
  New(CopyQueue, Init(250, 100));
  Info^.Bottom := GetString(dlFC_Total)+FStr(ToDo)+GetString(dlBytes);
  Info^.DrawView;
  If (Not CopyPrn) And (CopyOptions And cpoCheckFree<>0) And
     (ToDoClusCopy>GetDrInfo(Drv))
  And (MessageBox(GetString(erNoDiskSpacePre),@Byte(Drv[1]),mfError+mfYesButton+mfNoButton)<>cmYes)
  then CopyCancel:=True;
  if CopyCancel then goto 1;
  MaxRead;
  if Abort or CopyCancel then
    begin
      ClrIO;
      Close(ReadStream.F);
      ClrIO;
      Close(WriteStream.F);
      if IOResult = 0 then lEraseFile(WriteStream);
    end else if (FMSetup.Options and fmoBeep <> 0) and
                (ElapsedTimeInSecs(Timer) > 30) then BeepAfterCopy;
  if B <> nil then FreeMem(B, BSize); B:=nil;
  Dispose(CopyQueue,Done); CopyQueue:=nil;
  DoneStreams;
1:
  Flush := (SystemData.Options and ossFlushDsk <> 0);
  if Flush then
  begin
    Info^.ClearInterior;
    Info^.Write(5, GetString(dlFlushingBuffers)+'...');
  end else Dispose(Info,Done);

  {$IFDEF OS_DOS}
  if AppendInstalled then
    asm
      mov ax, $B708
      mov bx, AppendState
      push bp
      int  $2F
      pop  bp
    end;
  {$ENDIF}
  if Dirs <> nil then Dispose(Dirs,Done); Dirs:=nil;

  Desktop^.ForEach(@DisableFFP); {John_SW 10-02-2003}
  if CopyOptions and cpoMove <> 0 then __Remove;
  Desktop^.ForEach(@EnableFFP);  {John_SW 10-02-2003}
  Desktop^.ForEach(@ReReadFFP);  {John_SW 11-04-2003}

  for C := 'A' to 'Z' do
    if C in ReD then
      begin
        SSS := C;
        GlobalMessage(evCommand, cmRereadTree, @SSS);
      end;
  SSS := Copy(lFExpand(CopyDir), 1, Inhr);
  if (Inhr <> 0) then RereadDirectory(SSS);
  Message(Owner, evCommand, cmRecountDirs, nil);
  if Flush then
  begin
    {$IFDEF OS_DOS}
    asm
      mov ah,0dh
      int 21h
    end;
    {$ENDIF}
    Dispose(Info,Done);
  end;
  end;
end;
        {-DataCompBoy-}

const ccCopyMode: Byte = 2;
      ccCopyOpt: Byte = 0;


        {-DataCompBoy-}
function CopyDialog;
  var
    PF: PFileRec;
    D: PDialog;
    P, P1: PView;
    R: TRect;
    DT: Record
     S3: String;
     WW: Word;
     WW1: Word;
    end;
    SR: lSearchRec;
    Idx: TDlgIdx;
    I: Integer;
    SSS: string;
    Nm: String;
    Xt: String;
    S,S1,S4: String;
    C: char;

    IDDQD: record fl, dr: longint; end;

  procedure DoCalc(P: PFileRec); {$IFDEF BIT16}far;{$ENDIF}
  begin with IDDQD do if P^.Attr and Directory <> 0 then inc(dr) else inc(fl) end;

begin
   CopyDialog := False;
   If MoveMode
     then Idx := dlgRenameDialog
     else Idx := dlgCopyDialog;
   DT.WW := ccCopyMode; DT.WW1 := ccCopyOpt;
   DT.WW1 := DT.WW1 and not cpoMove;
   if MoveMode and (FromTemp <> 1) then DT.WW1 := DT.WW1 or cpoMove;
   if (FMSetup.Options and fmoAlwaysCopyDesc <> 0) then DT.WW1 := DT.WW1 or cpoDesc; {JO}
   if LowMemory then Exit;
   D := PDialog(Application^.ValidView(PView(LoadResource(Idx))));
   if D = nil then Exit;
   R.Assign(2,1,D^.Size.X-1,2);
   if MoveMode then S := GetString(dlFCMove1) else S := GetString(dlFCCopy1);
   DT.S3 := '';
   if Files^.Count = 1 then
    begin
     PF := Files^.At(0);
     S1 := #0+GetLFN(PF^.LFN)+#0;
     if MoveMode then DT.S3 := S1;
     if PF^.Attr and Directory = 0 then S1 := GetString(dlDIFile)+' '+S1
     else S1 := GetString(dlDirectory)+' '+S1;
    end else begin
     IDDQD.Dr := 0; IDDQD.Fl := 0;
     Files^.ForEach(@DoCalc);
     if IDDQD.Dr = 0 then S1 := ItoS(IDDQD.Fl) + ' ' + #0 + GetString(dlDIFiles) + #0 else
     if IDDQD.Fl = 0 then S1 := ItoS(IDDQD.Dr) + ' ' + #0 + GetString(dlDirectories) + #0 else
      FormatStr(S1, GetString(dlFilDir), IDDQD);
    end;
   if MoveMode then S := S+S1+GetString(dlFCMove2)
   else S := S+S1+GetString(dlFCCopy2);
   P1 := New(PLabel, Init(R, S, D^.Current));
   D^.Insert(P1);

   if MoveMode and (Files^.Count = 1)
     then DT.S3 := GetLFN(PFileRec(Files^.At(0))^.LFN)
     else GlobalMessageL(evCommand, cmPushName, hsFileCopyName);
   S4 := '';
   if Owner <> nil then Message(Owner^.Owner, evCommand, cmPushFirstName, @S4);
   if CopyDirName <> '' then S4 := CopyDirName;
   CopyDirName := '';
   if S4 <> '' then begin
      if (Files^.Count = 1) and (S4[2] = ':') and (S4[0] > #2) and
         (PFileRec(Files^.At(0))^.Attr and Directory = 0)
          then if LFNDisabled(S4)
                then S4 := MakeNormName(S4, MakeFileName(PFileRec(Files^.At(0))^.Name))
                else S4 := MakeNormName(S4, GetLFN(PFileRec(Files^.At(0))^.LFN));
      { Flash >>> }
      if (Files^.Count=1) and (Copy(S4,1,4)='TEMP')
         and not isDir(GetLFN(PFileRec(Files^.At(0))^.LFN)) then
       HistoryAdd(hsFileCopyName, GetLFN(PFileRec(Files^.At(0))^.LFN));
      if (Pos(':',S4)>2) and (Copy(S4,1,4)<>'TEMP') then
       begin
       HistoryAdd(hsFileCopyName,Copy(S4,1,Pos(':',S4))+GetName(S4));
       HistoryAdd(hsFileCopyName, GetLFN(PFileRec(Files^.At(0))^.LFN));
       end else
      if not isDir(S4) then HistoryAdd(hsFileCopyName,GetName(S4));
      if Files^.Count=1 then HistoryAdd(hsFileCopyName,GetLFN(PF^.LFN));
      { Flash <<< }
   end else if (Files^.Count = 1) then begin
    if LFNDisabled(S4)
     then S4 := MakeFileName(PFileRec(Files^.At(0))^.Name)
     else S4 := GetLFN(PFileRec(Files^.At(0))^.LFN);
     HistoryAdd(hsFileCopyName, S4);
   end;
   DT.S3 := S4;
   with DT do begin
    if DT.S3[2]=':' then C:=S3[1] else C:=GetCurDrive;
    if SystemData.Drives[C] and ossVerify <> 0
     then WW1 := WW1 or cpoVerify
     else WW1 := WW1 and not cpoVerify;
    if SystemData.Drives[C] and ossCheckFreeSpace <> 0
     then WW1 := WW1 or cpoCheckFree
     else WW1 := WW1 and not cpoCheckFree;
   end;
   D^.SetData(DT);

   if not SkipCopyDialog then
    if Desktop^.ExecView(D) = cmCancel then
     begin Dispose(D,Done); Exit end;

   SkipCopyDialog := False;

   D^.GetData(DT);
   S := DT.S3; CopyMode := DT.WW; CopyOpt := DT.WW1;
   if (S[Length(S)] = '.') and (Copy(S,Length(S)-1,2) <> '..') then Dec(S[0]); { Flash }
   ccCopyMode := DT.WW; ccCopyOpt := DT.WW1;
   Dispose(D,Done);
   lFSplit(S, CopyDir, Nm, Xt);
   if S = '' then Exit;
   if not Link then
      begin
        if ArchiveFiles(S, Files, MoveMode, Owner, False) then Exit;
{$IFDEF ARVID}
        if CopyFilesToArvid(S, Files, MoveMode, Owner) then Exit;
{$ENDIF}
        if UpStrg(Copy(S, 1, PosChar(':', S))) = cTEMP_ then
         begin
           CopyToTempDrive(Files, Owner, '');
           Exit;
         end;
        {$IFDEF MODEM}
        {$IFDEF LINK}
        if UpStrg(Copy(S, 1, PosChar(':', S))) = cLINK_ then
           begin
             Mask := Nm+Xt;
             if (PosChar('*', Mask) = 0) and (PosChar('?', Mask) = 0) then
                begin CopyDir := CopyDir + Mask; Mask := '' end;
             Delete(CopyDir, 1, 5);
             CopyToLinkDrive(Files, Owner, CopyDir, Mask);
             Exit;
           end;
       {$ENDIF}
       {$ENDIF}
      end;
   SSS := lFExpand(S); if Abort then Exit;
   Mask := Nm+Xt;
   if (Mask = '') then Mask := x_x else
   if (PosChar('*', Mask) = 0) and (PosChar('?', Mask) = 0) then
    begin
      ClrIO;
      if SSS[0] = #3 then begin CopyDir := S+'\'; Mask := x_x end else
      begin
        If not ((Files^.Count=1) and (UpStrg(S)=UpStrg(GetLFN(PFileRec(Files^.At(0))^.LFN))))
         then begin
          lFindFirst(SSS, AnyFile, SR); {JO}
          if Abort then begin
           lFindClose(SR);
           Exit;
          end;
          if (DOSError = 0) and (SR.SR.Attr and Directory <> 0) then
             begin CopyDir := S+'\'; Mask := x_x end;
          lFindClose(SR);
         end;
      end;
    end;
   S := #0'NUL'#0'PRN'#0'LPT1'#0'LPT2'#0'LPT3'#0'LPT4'#0'COM1'#0'COM2'#0'COM3'#0'COM4';
   if Nm[Length(Nm)]=':'
    then I := Pos(UpStrg(#0+Copy(Nm,1,Length(Nm)-1)+#0), S)
    else I := Pos(UpStrg(#0+Nm+#0), S);
   if (I <> 0) then
     begin
       CopyMode := cpmOverwrite;
       CopyDir := Mask;
       Mask := '';
       CopyPrn := True;
       NoCopyLFN:=true;
     end
     else begin
            if (CopyDir = '') then CopyOpt := CopyOpt or cpoDirEmpty;
            if CopyDir='\' then CopyDir:=GetRoot(GetCurrentDir);
            CopyDir := lFExpand(CopyDir);
            if CopyDir[Length(CopyDir)] <> '\' then AddStr(CopyDir, '\');
            CopyPrn := False;
            NoCopyLFN:=LFNDisabled(CopyDir);
          end;
   CopyDialog := True;
   OldTitle := ''; { Flash 28-03-2003 }
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
procedure CopyFiles;

 var
     CopyOpt: Word;
     CopyMode: Word;
     CopyPrn: Boolean;
     CopyDir: String;
     Mask: String;

begin
   CtrlBreakHit := False;
   Files^.Pack; if Files^.Count <= 0 then Exit;
   if CopyFromTemp then FromTemp := 1;
   if not CopyDialog(CopyDir, Mask, CopyOpt, CopyMode, CopyPrn,
                     MoveMode, Files, FromTemp, Owner, False) then Exit;
   if (CopyDir[Length(CopyDir)] <> '\') and (CopyDir <> '') then
    CopyDir := CopyDir+'\';
   if (FromTemp > 0) and MoveMode then
     begin
       CopyOpt := CopyOpt or cpoFromTemp;
       if CopyOpt and cpoDirEmpty <> 0 then CopyDir := '';
     end;
   Inc(SkyEnabled);
   if StartupData.Slice2 and osuReleaseCopy = 0 then Application^.BFSpeed;
   FilesCopy(Files, Owner, CopyDir, Mask, CopyMode, CopyOpt, CopyPrn);
   if StartupData.Slice2 and osuReleaseCopy = 0 then Application^.EFSpeed;
   Dec(SkyEnabled);
end;

end.
