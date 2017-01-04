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
//  dn16rc1-move_COMBINE_MIXED_from_sources_to_Options_menu_diff140byMV.patch
//  dn16rc1-ctrl-Y_hotkey_used_for_current_to_full_columns_mode.patch
//  dn16rc1-sort_by_description_and_archives_before_exe.patch
//  dn16rc1-advanced_filter_fix_2.patch
//  dn16rc1-Ctrl-N_hotkey_used_for_long-short_names_switching.patch
//
//  2.0.0
//  dn230-files_panel_bugs_fix_1.patch
//  dn269-diskinfo.patch
//
//  2.7.0
//  dn230-histories_for_network_resources.patch
//  dn281-sort_dir_speed.patch
//  dn270-nameless_files_are_extension.patch
//  dn2922-show_size_before_name_if_it_longer_250.patch
//  dn21029-highlight_250_fix-jo21104a.patch
//  dn21116-Ctrl_Slash_Up_Down_fix.patch
//  dn21128-show_LFN_and_desc_in_bottom.patch
//  dn21202-Find_file_and_branch_in_archive.patch
//  dn21225-FilePanel(fi)-lfn_and_description_show_on_divider.patch
//  dn328-FilePanel(f)_jo30224a_crash_fix.patch
//  dn3323-temp(f)-copy_files_from_archives.patch
//
//  3.7.0
//  dn31005-bp_to_vp_on_off_true_false.patch
//
//  4.9.0
//  dn40900-Descriptions(f)-read_and_show_descriptions_fix.patch
//  dn50208-cleanup.patch
//  dn40900-temp_drive_in_directories_history.patch
//  dn50308-descriptions_show_fix.patch
//
//  5.9.0
//
//////////////////////////////////////////////////////////////////////////}
{$I STDEFINE.INC}

unit Drives;

interface
uses Dos, LFN, LFNCol, Objects, Memory, Views, Drivers, DnIni, FilesCol,
     DiskInfo, FStorage, advance, advance1, advance2, advance3, collect;

const
     dsActive      = 0 ;
     dsInvalid     = 1 ;

type

  TSetDriveTypes = ( dtUndefined, dtDisk, dtFind, dtTemp, dtList, dtArcFind, dtArc, dtLink, dtArvid );

  PDrive = ^TDrive;
  TDrive = object(TObject)
   Owner: Pointer;
   Prev: PDrive;
   DriveType: TSetDriveTypes;
   CurDir: String;          {DataCompBoy}
   DIZOwner: String;        {DataCompBoy}
   NoMemory: Boolean;
   Flags: AWord;
   LFNLen: byte; EXTLen: byte;
   DirFLP, FilFLP: Aword;
   Param, OldParam: byte; innum: byte;
   SizeX: LongInt;
   constructor Init(ADrive: Byte; AOwner: Pointer; num: byte);
   constructor Load(var S: TStream);
   procedure Store(var S: TStream); virtual;
   procedure KillUse; virtual;
   procedure lChDir(ADir: String); virtual;     {DataCompBoy}
   function  GetDir: String; virtual;            {DataCompBoy}
   function  GetDirectory(SortMode, PanelFlags: Integer; const FileMask: String;
                          var FreeSpace, TotalInfo: String ): PCollection; virtual;
   procedure CopyFiles(Files: PCollection; Own: PView; MoveMode: Boolean); virtual;
   procedure CopyFilesInto(Files: PCollection; Own: PView; MoveMode: Boolean); virtual;
   procedure EraseFiles(Files: PCollection); virtual;
   procedure UseFile(P: PFileRec; Command: Word); virtual; {DataCompBoy}
   procedure GetFreeSpace(var S: String); virtual;
   function  Disposable: Boolean; virtual;
   function  GetRealName: String; virtual;
   procedure GetFull(var B; P:PFileRec; C, SC:Word); virtual; {DataCompBoy}
   procedure GetEmpty(var B; SC: Word); virtual;
   function  CalcLengthWithoutName: Integer; virtual;
   function  CalcLength: Integer; virtual;
   procedure RereadDirectory(S: String); virtual; {DataCompBoy}
   procedure MakeTop(var S: String); virtual;
   procedure GetDown(var B; C: Word; P: PFileRec); virtual; {DataCompBoy}
   procedure HandleCommand(Command: Word; InfoPtr: Pointer); virtual;
   procedure GetDirInfo(var B: TDiskInfoRec); virtual;
   function  GetRealDir: String; virtual;
   procedure MakeDir; virtual;
   function  isUp: Boolean; virtual;
   procedure ChangeUp(var S: String); virtual;
   procedure ChangeRoot; virtual;
   function  GetFullFlags: Word; virtual;
   procedure EditDescription(PF: PFileRec); virtual; {DataCompBoy}
   procedure GetDirLength(PF: PFileRec); virtual; {DataCompBoy}
   procedure GetParam(n: byte); virtual; {DataCompBoy}
   destructor Done; virtual;
   function OpenDirectory(const Dir: String): PDrive; virtual;
  end;

procedure RereadDirectory(Dir: String);
function CalcTotalInfo(A, B: Word; C: TSize): string;
function GetFilesColl(
    var TotalLen: TSize;
    var TFiles: Word;
    PanelFlags: Integer;
    CurDir: PString;
    const FileMask: string;
    Owner: Pointer;
    SortMode: Integer;
    LFNActive: Boolean): PFilesCollection;

const
  TempDirs: PSortedCollection = nil;
  TempFiles: PFilesCollection = nil;

  DriveLoadingError: Boolean = False;

implementation
uses Startup, Tree, DNApp, RStrings, FileCopy, Eraser, FlPanel, Commands,
     Dialogs, FileFind, FlPanelX, filediz, CmdLine{!!}
     , xTime
     ;

type
        {-DataCompBoy-}
       PDesc = ^TDesc;
       TDesc = record
         {$IFNDEF OS2}
         Name: Str12;
         {$ENDIF}
         LFN: TLFNIndex;
         DIZ: PString;
         Line: LongInt;
       end;
        {-DataCompBoy-}

       PDIZCol = ^TDIZCol;
       TDIZCol = object(TSortedCollection)
         procedure FreeItem(P: Pointer); virtual;
         function Compare(P1, P2: Pointer): Integer; virtual;
       end;

       TFName = record
        {$IFNDEF OS2}
        SFN: Str12;
        {$ENDIF}
        LFN: TLFNIndex;
       end;


function ESC_Pressed: Boolean; var E: TEvent;
begin
  Application^.Idle;
  GetKeyEvent(E); ESC_Pressed := (E.What = evKeyDown) and (E.KeyCode = kbEsc)
end;

        {-DataCompBoy-}
procedure TDIZCol.FreeItem;
begin
 if P <> nil then
  begin
    DisposeStr(PDesc(P)^.DIZ);
    DelLFN(PDesc(P)^.LFN);
    Dispose(PDesc(P));
  end;
end;
        {-DataCompBoy-}

function TDIZCol.Compare;
begin
  {$IFNDEF OS2}
  if PDesc(P1)^.Name = PDesc(P2)^.Name then Compare := 0
   else Compare := CmpLFN(PDesc(P1)^.LFN, PDesc(P2)^.LFN);
  {$ELSE}
  Compare:=CmpLFN(PDesc(P1)^.LFN, PDesc(P2)^.LFN)
  {$ENDIF}
end;

procedure TDrive.GetFreeSpace;{piwamoto}
begin
 S := '~'+FStr(GetDrInfo(CurDir));
end;

        {-DataCompBoy-}
procedure TDrive.GetParam;
begin
 OldParam := Param;
 Param:=n;
 with ColumnsDefaultsDisk do
  begin
   Flags := Params[n].Param;
   LFNLen:=StoI(Params[n].LFNLen);
   if (LFNLen = 0) or (LFNLen > 252) then LFNLen := 252;
   EXTLen:=StoI(Params[n].EXTLen);
   if EXTLen > 252 then EXTLen := 252;
   case FileCase of
    0: FilFLP := 0;
    1: FilFLP := flnLowCase;
    2: FilFLP := flnCapitalCase;
    3: FilFLP := flnUpCase;
    4: FilFLP := 65535;
    5: FilFLP := 65534;
   end;
   case DirsCase of
    0: DirFLP := 0;
    1: DirFLP := flnLowCase;
    2: DirFLP := flnCapitalCase;
    3: DirFLP := flnUpCase;
    4: DirFLP := 65535;
    5: DirFLP := 65534;
   end;
  end;
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
constructor TDrive.Init;
begin
 TObject.Init;
 Owner := AOwner;
 if num < 1 then GetParam(1)
            else GetParam(num);
 innum:=num;
 ClrIO;
 if ADrive < $1B then lGetDir(ADrive, CurDir) {A: ... Z:}
                 else CurDir := ''; {any other - i.e. \\server\share}
 DriveType := dtDisk;
 NoMemory := False;
 OldParam := Param;
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
constructor TDrive.Load(var S: TStream);
begin
 TObject.Init;
 Prev := PDrive(S.Get);
 S.Read(CurDir[0], 1);
 S.Read(CurDir[1], Length(CurDir));
 S.Read(Flags, SizeOf(Flags));
 S.Read(LFNLen, 1);
 S.Read(EXTLen, 1);
 S.Read(DirFLP, 2);
 S.Read(FilFLP, 2);
 S.Read(Param,  1);
 DriveType := dtDisk;
 NoMemory := False;
 OldParam := Param;
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
procedure TDrive.Store(var S: TStream);
begin
 S.Put(Prev);
 S.Write(CurDir, Length(CurDir)+1);
 S.Write(Flags, SizeOf(Flags));
 S.Write(LFNLen, 1);
 S.Write(EXTLen, 1);
 S.Write(DirFLP, 2);
 S.Write(FilFLP, 2);
 S.Write(Param,  1);
end;
        {-DataCompBoy-}

destructor TDrive.Done;
begin
 if Prev <> nil then Dispose(Prev,Done);
 inherited Done;
end;

function TDrive.Disposable;
begin
 Disposable := True;
end;

        {-DataCompBoy-}
procedure TDrive.ChangeUp;
begin
 S:=GetName(CurDir);
 lChDir(MakeNormName(CurDir,'..'));
 if Abort then Exit;
 if not ((word((@CurDir[1])^)=$2F2F{//}) or (word((@CurDir[1])^)=$5C5C{\\}))
   then lGetDir(0, CurDir);
 if Abort then Exit;
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
procedure TDrive.ChangeRoot;
begin
 lChDir(GetRoot(CurDir));
 if Abort then Exit;
 if not ((word((@CurDir[1])^)=$2F2F{//}) or (word((@CurDir[1])^)=$5C5C{\\}))
   then lGetDir(0, CurDir);
 if Abort then Exit;
end;
        {-DataCompBoy-}

function TDrive.CalcLengthWithoutName;
begin
 CalcLengthWithoutName :=
      (10 + CountryInfo.TimeFmt) * Byte(Flags and psShowSize <> 0) +
      9 * Byte(Flags and psShowDate <> 0) +
      (7 - CountryInfo.TimeFmt) * Byte(Flags and psShowTime <> 0);
end;

function TDrive.CalcLength;
 var B: Word;
begin
 {$IFNDEF OS2} if Flags and psShowLongName <> 0 then {$ENDIF}
    B := CalcLengthWithoutName+LFNLen+1 {$IFNDEF OS2} else B := CalcLengthWithoutName + 13 {$ENDIF};
 if B > MaxViewWidth then B := MaxViewWidth;
 if Flags and psShowDescript <> 0 then CalcLength := MaxViewWidth
                                  else CalcLength := B;
end;

procedure TDrive.MakeTop;
begin
if (LFNLen < 250) {$IFNDEF OS2} or (Flags and psShowLongName = 0){$ENDIF}then
begin
{$IFNDEF OS2}
 if Flags and psShowLongName <> 0 then
     begin
{$ENDIF}
       if LFNLen <= SizeX then
       S := CenterStr(GetString(dlTopLFN), LFNLen)+GetString(dlTopSplit)
         else S := AddSpace(CenterStr(GetString(dlTopLFN), SizeX), LFNLen)
{$IFNDEF OS2}
       ;
     end
   else S := GetString(dlTopName)
{$ENDIF}
end else S := '';
 if Flags and psShowSize     <> 0 then S := S + Copy(GetString(dlTopSize),2-CountryInfo.TimeFmt, 255);
 if Flags and psShowDate     <> 0 then S := S + GetString(dlTopDate);
 if Flags and psShowTime     <> 0 then S := S + Copy(' '+GetString(dlTopTime), 1+CountryInfo.TimeFmt, 255);
 if (LFNLen >= 250) {$IFNDEF OS2} and (Flags and psShowLongName <> 0){$ENDIF}
         then S := S  + AddSpace(CenterStr(GetString(dlTopLFN), SizeX-Length(S)), LFNLen) else
 if Flags and psShowDescript <> 0 then S := S + ' ' + GetString(dlPnlDescription) + ' ' + Strg(#32,255);
end;

procedure TDrive.GetFull;
 var
     X: AWord;
     OPT, CO: Word;
     LS: Byte;
     NFM: TNameFormatMode;
     QW: Boolean;
     LFNLonger250: Boolean;
     S1: String[40];
     S, Ext: String;
 label Longer250, Longer250_1, NE;
begin
X := 0;
CO := C;
LFNLonger250 := (LFNLen >= 250){$IFNDEF OS2} and (Flags and psShowLongName <> 0){$ENDIF};
if not LFNLonger250 then {JO: если ширина колонки имени больше 250 символов, её показываем после остальных колонок}
Longer250:
 begin
{$IFNDEF OS2}
  if Flags and psShowLongName <> 0 then
   begin
    S:=GetLFN(P^.LFN);
{$ELSE}
    S := P^.Name;
{$ENDIF}
    Ext := GetExt(S);
    NFM:=nfmNull;
    if EXTLen<>0 then begin
     case DriveType of
      dtLink, dtArc, dtDisk: QW := (Flags and psVariableExtDA<>0);
      dtFind, dtTemp, dtList, dtArvid: QW := (Flags and psVariableExtSTA<>0);
     end;
     if QW and ((Length(Ext)=1) or (Length(S) = Length(Ext))) then goto NE;
     OPT:=flnHardExtArea or flnAutoHideDot or flnUseCutChar or flnPadRight or flnHighlight or flnHandleTildes
    end else
NE:  OPT:=flnPreferName or flnUseCutChar or flnPadRight or flnHighlight or flnHandleTildes;
    if P^.Selected and (FMSetup.Show and fmsTagChar<>0) then OPT:=OPT+flnSelected;

   case P^.Attr and (Hidden+SysFile) of               {Pavel Anufrikov -> }
    Hidden  :        NFM:=nfmHidden;
    SysFile :        NFM:=nfmSystem;
    Hidden+SysFile : NFM:=nfmHiddenSystem;
   end{case};                                         { <- Pavel Anufrikov}

    if P^.Attr and Directory <> 0
      then if DirFLP < 65534
             then OPT:=OPT or DirFLP
             else if DirFLP = 65535
                    then if (Length(GetSName(S))<=8) and
                            (Length(Ext)<=4)
                           then OPT:=OPT or flnUpCase
                           else
                    else if (not IsMixedCase(S))
                           then OPT:=OPT or flnUpCase
                           else
      else if FilFLP < 65534
             then OPT:=OPT or FilFLP
             else if FilFLP = 65535
                    then if (Length(GetSName(S))<=8) and
                            (Length(Ext)<=4)
                           then OPT:=OPT or flnLowCase
                           else
                    else if (not IsMixedCase(S))
                           then OPT:=OPT or flnLowCase
                           else ;

    S:=FormatLongName(S, LFNLen, EXTLen, OPT, NFM, nil);
    if P^.Selected and (FMSetup.Show and fmsTagChar=0) then S:='~'+S+'~';
{$IFNDEF OS2}
   end
  else
   begin
    NFM:=nfmNull;
    OPT:=flnHardExtArea or flnAutoHideDot or flnPadRight or flnHighlight or flnHandleTildes;
    if P^.Selected and (FMSetup.Show and fmsTagChar<>0) then OPT:=OPT+flnSelected;

    case P^.Attr and (Hidden+SysFile) of               {Pavel Anufrikov -> }
     Hidden  :        NFM:=nfmHidden;
     SysFile :        NFM:=nfmSystem;
     Hidden+SysFile : NFM:=nfmHiddenSystem;
    end{case};                                         { <- Pavel Anufrikov}
    if P^.Attr and SysFile<>0 then OPT:=OPT or flnCapitalCase;

    if P^.Attr and Directory <>0 then OPT:=OPT or flnUpCase
                                 else OPT:=OPT or flnLowCase;
    S:=FormatLongName(MakeFileName(P^.Name), 12, 3, OPT, NFM, nil);
    if P^.Selected and (FMSetup.Show and fmsTagChar=0) then S:='~'+S+'~';
   end;
{!!!!!!!!!!!}
{$ENDIF}
  MoveCStr(TWordArray(B)[X], S, C);
  {$IFNDEF OS2}if Flags and psShowLongName <> 0 then {$ENDIF} Inc(X, LFNLen)
  {$IFNDEF OS2}  else Inc(X, 12) {$ENDIF}
  ;
  if X >= 255 then Exit;
{-----------}
  if LFNLonger250 then goto Longer250_1;
{JO: не понял, зачем DataCompBoy ввёл условие                                }
{    PView(Owner)^.GetState(sfFocused), приводившее к исчезновению выделения }
{    цветом всего, что идёт после имени у помеченных файлов при переходе     }
{    на противоположную панель; без этого условия всё работает нормально     }
  if P^.Selected {and PView(Owner)^.GetState(sfFocused)} then
    begin
      C := C shr 8;
      SC := SC and $00FF + C shl 8;
    end;
  Inc(X);
  if X >= 255 then Exit;
  TWordArray(B)[X-1]:=SC;
 end {условие 250 символов}
  else if P^.Selected then
    begin
      C := C shr 8;
      SC := SC and $00FF + C shl 8;
    end;

  if (P^.Size > 0) or (P^.Attr and Directory = 0) or (P^.Attr and $8000 <> 0)
     and (P^.Size >= 0)
    then S := FileSizeStr( P^.Size )
    else if P^.TType = ttUpDir
      then S := GetString( dlUpDir )
      else S := GetString( dlSubDir );
  for LS := 1 to (9 - Length(S) + CountryInfo.TimeFmt) do S := ' ' + S; {JO}
  MakeDate(DateMode, P^.Day, P^.Month, P^.Yr, P^.Hour, P^.Minute, S1);
  if Flags and psShowSize <> 0 then
   begin
    MoveStr(TWordArray(B)[X], S, C);
    X := X + 10 + CountryInfo.TimeFmt;
    if X >= 255 then Exit;
    TWordArray(B)[X-1]:=SC;
   end;
  if Flags and psShowDate <> 0 then
   begin
    MoveStr(TWordArray(B)[X], Copy(S1,1,8), C);
    Inc(X, 9);
    if X >= 255 then Exit;
    TWordArray(B)[X-1]:=SC;
   end;
  if Flags and psShowTime <> 0 then
   begin
    MoveStr(TWordArray(B)[X], Copy(S1,10,6-CountryInfo.TimeFmt), C);
    Inc(X, 7 - CountryInfo.TimeFmt);
    if X >= 255 then Exit;
    TWordArray(B)[X-1]:=SC;
   end;
  if LFNLonger250 then begin C := CO; goto Longer250; end;
  if (Flags and psShowDescript <> 0) and ((DriveType = dtDisk) or
     (DriveType = dtArvid)) then
   begin
    if P^.Diz <> nil then S := Copy(AddSpace(CnvString(P^.DIZ^.DIZ), 255), 1, MaxViewWidth - X - 1)
      else S := AddSpace(' ', MaxViewWidth - X - 1);
    MoveStr(TWordArray(B)[X], S, C);
   end;
Longer250_1:
end;

procedure TDrive.GetEmpty;
 var
  X: AWord;
begin
  if (LFNLen < 250) {$IFNDEF OS2} or (Flags and psShowLongName = 0){$ENDIF} then
   begin
  {$IFNDEF OS2} if Flags and psShowLongName <> 0 then {$ENDIF}
                                           X:=LFNLen+1{$IFNDEF OS2} else X:=13{$ENDIF};

    if X >= 255 then Exit;
    TWordArray(B)[X-1]:=SC;
   end else X := 0;
  if Flags and psShowSize <> 0 then
   begin
    X := X + 10 + CountryInfo.TimeFmt;
    if X >= 255 then Exit;
    TWordArray(B)[X-1]:=SC;
   end;
  if Flags and psShowDate <> 0 then
   begin
    Inc(X, 9);
    if X >= 255 then Exit;
    TWordArray(B)[X-1]:=SC;
   end;
  if Flags and psShowTime <> 0 then
   begin
    Inc(X, 7 - CountryInfo.TimeFmt);
    if X >= 255 then Exit;
    TWordArray(B)[X-1]:=SC;
   end;
end;

procedure TDrive.EraseFiles;
begin
 if Disposable then Eraser.EraseFiles(Files);
end;

procedure TDrive.MakeDir;
begin
 MakeDirectory;
end;

procedure TDrive.CopyFiles;
begin
 if ReflectCopyDirection
  then RevertBar:=Message(DeskTop, evBroadCast, cmIsRightPanel, Own) <> Nil
  else RevertBar:=false;
 if Disposable then FileCopy.CopyFiles(Files, Own, MoveMode, 2*Byte(TypeOf(Self)=TypeOf(TFindDrive)));
end;

procedure TDrive.CopyFilesInto;
begin
 if ReflectCopyDirection
  then RevertBar:=(Message(DeskTop, evBroadCast, cmIsRightPanel, Own) <> Nil)
  else RevertBar:=false;
 FileCopy.CopyFiles(Files, Own, MoveMode, 0);
end;

        {-DataCompBoy-}
procedure TDrive.lChDir;
begin
 ClrIO;
 if (Length(ADir)>3) and (ADir[Length(ADir)] in ['\','/']) then dec(ADir[0]);
 if (ADir[1] in ['\', '/']) and not (ADir[2] in ['\', '/']) then
   ADir:=MakeNormName(GetRoot(GetCurrentDir), Copy(ADir, 2, 255));
 LFN.lChDir(ADir);
 if Abort then Exit;
 if not ((word((@ADir[1])^)=$2F2F{//}) or (word((@ADir[1])^)=$5C5C{\\}))
   then LFN.lGetDir(0, ADir);
 if Abort then Exit;
 CurDir := ADir;
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
function TDrive.GetDir;
begin
 GetDir := {$IFNDEF OS2}lfGetLongFileName{$ENDIF}(CurDir);
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
procedure TDrive.UseFile;
 var S: String;
begin
 if P^.Owner<>nil then S := MakeNormName(P^.Owner^, GetLFN(P^.LFN));
 Message(Application, evCommand, Command, @S);
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
(*
procedure ReadFileList(
     const CurDir: String;
     const FName: String;
     var Descriptions: PDIZCol);

var T: PTextReader;
    S: String;
    Nam: String;
    I, J: LongInt; zz: boolean;
    CurLine: LongInt;
    PD: PDesc;
begin
  ClrIO;
  T := New(PTextReader, Init(MakeNormName(CurDir, FName)));
  if (T = nil) or Abort then Exit;
  CurLine := 0;
  While not T^.EOF and (IOResult = 0) do
    begin
      Inc(CurLine);
      S := T^.GetStr;
      if (S <> '') and not (s[1] in [' ',#9]) then
       begin
         zz:=false;
         for i:=2 to length(s) do
          if (s[i] in [' ',#9]) and not zz
           then break
           else if s[i]='"' then zz:=not zz;
         if s[i] in [' ',#9] then dec(i);
         j:=i+1;
         while (s[j] in [' ',#9]) do inc(j);
         if not (S[j] in [' ',#9]) then dec(j);
         Nam:=DelSquashes(Copy(S, 1, i));
         Delete(S, 1, j);

         New(PD);
         {$IFNDEF OS2}
         PD^.Name := Norm12(GetName(lfGetShortFileName(Nam)));
         {$ENDIF}
         PD^.LFN := AddLFN(GetName({$IFNDEF OS2}lfGetLongFileName{$ENDIF}(Nam)));
         PD^.Name[9] := ' ';
         PD^.DIZ := NewStr(S);
         PD^.Line := CurLine;
         if not Descriptions^.Search(PD, I)
          then Descriptions^.AtInsert(I, PD)
          else Descriptions^.FreeItem(PD);
       end;
    end;
  Dispose(T,Done);
end;
        {-DataCompBoy-}
*)

{AK155: JO внес коррекции в части добавления длинных имен,
а потом я почти полностью переписал эту процедуру.
Теперь имена воспринимаются только с первой позиции,
имена в кавычках допускаются. 01 авг 2001}

procedure ReadFileList(
    const CurDir: String;
    const FName: String;
    var Descriptions: PDIZCol);

  var
    CurLine: LongInt;
    T: PTextReader;
    S: String;

    { прочитать непустую строку (хвостовые пробелы отбрасываются)}
  function ReadNextS: Boolean;
    begin
    ReadNextS := False;
    repeat
      if T^.Eof then
        Exit;
      S := T^.GetStr;
      if IOResult <> 0 then
        Exit;
      Inc(CurLine);
      DelRight(S);
    until S <> '';
    ReadNextS := True;
    end;

  var
    I: LongInt;
    j: LongInt;
    NameEnd: LongInt;
    PD: PDesc;
    LS: Byte;
    S1: String;
  label
    ReadNextLine, EndDescr, EndFile;

  begin { ReadFileList }
  ClrIO;
  T := New(PTextReader, Init(MakeNormName(CurDir, FName)));
  if  (T = nil) or Abort then
    Exit;
  PD := nil;
  CurLine := 0;
  if not ReadNextS then
    begin
    Dispose(T, Done);
    Exit;
    end;
  while True do
    begin
    { Обработка нового описания. S уже прочитана.}
    if  (S[1] in [' ', #9, '>']) then
      goto ReadNextLine;
    { игнорируем остаток
         предыдущего многострочного описания }

    LS := Length(S);
    if S[1] = '"' then
      {имя в кавычках - ищем вторую кавычку }
      begin
      NameEnd := 0;
      for j := 2 to LS do
        if S[j] = '"' then
          begin
          NameEnd := j;
          Break;
          end;
      if NameEnd <= 2 then
        goto ReadNextLine;
      if NameEnd = LS then
        goto ReadNextLine; { пустое описание никого не интересует }
      S1 := Copy(S, 2, NameEnd-2);
      end
    else
      {имя не в кавычках - ищем пробел или Tab. При этом
        благодаря DelRight после него что-то должно быть }
      begin
      NameEnd := Pos(' ', S);
      j := Pos(#9, S);
      if  (j <> 0) and (j < NameEnd) then
        NameEnd := j { заведомо не 0}
      else
        begin
        if NameEnd = 0 then
          NameEnd := j; { может быть и 0}
        if NameEnd = 0 then
          goto ReadNextLine; { пустое описание никого не интересует }
        end;
      S1 := Copy(S, 1, NameEnd-1);
      end;
    if PD = nil then
      New(PD);
    {$IFNDEF OS2}
    PD^.Name := Norm12(GetName(lfGetShortFileName(S1)));
    {$ENDIF}
    PD^.LFN := AddLFN(GetName({$IFNDEF OS2}lfGetLongFileName{$ENDIF}(S1)));
    PD^.Line := CurLine;
    if Descriptions^.Search(PD, I) then
      goto ReadNextLine;
    System.Delete(S, 1, NameEnd);
    DelLeft(S);
    S1 := S;

    {
AK155: Дочитываем многострочное описание (сколько влазит в S1).
Признаком дополнительной строки является побел или Tab в начале,
а такэже '>' в начале (files. bbs вформате AllFix).
  Это нормально работает для просмотра описаний, но приводит
к глюкам при попытке редактирования многострочного описания.
Впрочем, при редактировании многострочных описаний глюки
будут в любом случае, так как фактически редактируется только
первая строка. Чтобы избавиться от глюков, надо серьезно переделывать
и формат TDesc, и его отображение, и его редактирование. Можно
ввести коллекцию строк, а не одну строку, и попытаться заюзать
редактор. Или вообще не заниматься самодеятельностью, а просто
вызывать стандартный редактор для файла описаний, установив курсор
на нужную строку (предварительно создав эту строку, если ее не было)
}
    while True do
      begin
      if not ReadNextS then
        Break;
      if not (S[1] in [' ', #9, '>']) then
        Break;
      j := 2;
      while (j <= Length(S)) and (S[j] = ' ') do
        Inc(j);
      LS := Length(S1);
      if LS >= 253 then
        Break;

      S1 := S1+#20' '+Copy(S, j, 253-LS);
      end;
    {JO: пока сделал так: "дочитанные" куски отделяются от основной части (и друг }
    {    от друга) символом #20, который ищется при редактировании и копировании}
    {    описаний и вся "дочитанная" часть описания после него (и он сам) отбрасывается }

    { конец дочитывания многострочного описания }

    PD^.DIZ := NewStr(S1);
    Descriptions^.AtInsert(I, PD);
    PD := nil;
    goto EndDescr;

ReadNextLine:
    if not ReadNextS then
      goto EndFile;
EndDescr:
    end;

EndFile:
  if PD <> nil then
    Dispose(PD);
  Dispose(T, Done);
  end { ReadFileList };

        {-DataCompBoy-}
procedure PrepareDIZ(
        const CurDir: string;
        var DIZOwner: string;
        var Descriptions: PDIZCol);
var
  S: String;
  FName: String;
begin
  ClrIO;
  S := GetDizOwner(CurDir, '', True);
  if S <> '' then
     begin
       FName := GetName(S);
       Descriptions := New(PDIZCol, Init($10,$10));
       ReadFileList(CurDir, FName, Descriptions);
       DIZOwner := MakeNormName(CurDir, FName);
     end;
  ClrIO;
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
procedure TossDescriptions(
    PDizOwner: Pointer;
    Files: PFilesCollection;
    Descriptions: PDIZCol);
var
  I, J: LongInt;
  P: PFileRec;
  FName: TFname;
  PD: PDesc;
begin
 for I := 1 to Files^.Count do
  begin
    P := Files^.At(I-1);
    FName.SFN := P^.Name;
    FName.LFN := P^.LFN;
    if P^.Attr and (Directory+SysFile) <> 0 then LowStr(FName.SFN);
    if Descriptions^.Search(@FName, J) then begin
                                        PD := PDesc(Descriptions^.At(J));
                                        New(P^.DIZ);
                                        P^.DIZ^.DIZ := PD^.DIZ;
                                        P^.DIZ^.Owner := PDIZOwner;
                                        P^.DIZ^.Line := PD^.Line;
                                        P^.DIZ^.isDisposable := True;
                                        PD^.DIZ := nil;
                                       end
                                  else P^.DIZ := nil;
  end;
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
function GetFilesColl;
var
  Files: PFilesCollection;
  SR: lSearchRec;
  NormMask: Boolean;
  P: PFileRec;
  L: TSize;
  fm: string;
{$IFDEF CHDIRDEBUG}
  ev: TEvent;
 const NumCal: word = 0;
       States:Array[0..3] of char = '|/-\';
       PanelState: PChar = Ptr($B800, 0);
       StartChar: Char = ' ';
begin
 PanelState:=Ptr(SegB800,0);
 StartChar:=PanelState^;
{$ELSE}
begin
{$ENDIF}
  {$IFDEF CHDIRTIMER}
  if ShiftState and (kbLeftShift+kbRightShift) <> 0 then DDTimer:=Get100s;
  {$ENDIF}
  TFiles:=0;
  Files := New(PFilesCollection, Init($10, $20));
  PFilesCollection(Files)^.Owner := Owner;
  PFilesCollection(Files)^.SortMode := SortMode;

  NormMask := not PackMask(FileMask, fm, LFNDisabled(CurDir^));

  lFindFirst(MakeNormName(CurDir^,fm), AnyFile, SR); {JO}
  While (DosError = 0) and not Abort and (IOResult = 0) and MemOK do
  begin
{$IFDEF CHDIRDEBUG}
   PanelState^:=States[NumCal];
   inc(NumCal);
   if NumCal=4 then NumCal:=0;
{$ENDIF}
   if ({$IFNDEF OS2}(SR.SR.Attr and VolumeID = 0) and {$ENDIF}
       not ((SR.SR.Name[0] = #1) and (SR.SR.Name[1] = '.'))
      )          and
      ((not Security) or
       (SR.SR.Attr and Hidden=0) or
       (SR.SR.Name[1] = '.')
      )          and
      (NormMask or
       (SR.SR.Attr and Directory <> 0) or
       ((InFilter(SR.FullName, FileMask) and LFNActive) or (InFilter(SR.SR.Name, FileMask) and not LFNActive))
      )          and
      ((SR.SR.Attr and Directory = 0) or
       (SR.SR.Name[1] <> '.') or
       (CurDir^[0] > #3)
      )          then
    begin
      P := NewFileRec(SR.FullName, {$IFNDEF OS2}SR.SR.Name, {$ENDIF}SR.FullSize, SR.SR.Time, SR.SR.Attr, CurDir);
      if SR.SR.Attr and Directory = 0 then
       begin
        TotalLen := TotalLen + P^.Size;
        Inc(TFiles);
       end else
       begin
         if PanelFlags and fmiDirLen <> 0 then
          begin
            if SR.SR.Name[1] = '.' then P^.Size := GetDirLen(CurDir^)
                                   else P^.Size := GetDirLen(MakeNormName(CurDir^,SR.FullName));
            if Abort then begin PanelFlags := PanelFlags and not fmiDirLen end;
            ClrIO;
          end;
        end;
      {with Files^ do AtInsert(Count, P)}
     {JO: это условие было в ритлабовской версии и позволяло не делать      }
     {    Files^.Sort в конце GetFilesColl ;                                }
     {    при переходе от 1.51.05 к 1.51.05a DataCompBoy зачем-то убрал     }
     {    это условие и сделал вместо него Files^.Sort в конце, что заметно }
     {    уменьшило скорость вхождения в каталоги, особенно учитывая нашу   }
     {    тормозную процедуру  PoglSort                                     }
      if (SortMode = -1) or (SortMode = psmUnsorted) then with Files^ do AtInsert(Count, P)
       else Files^.Insert(P);
     {/JO}
    end;
   DosError := 0;
   lFindNext(SR);
  end;
  lFindClose(SR);
  if (Length(CurDir^) > 3) then
  if ((Files^.Count=0) or
      (PFileRec(Files^.At(0))^.TType <> ttUpDir)) then
   begin
    if PanelFlags and fmiDirLen <> 0 then
    begin
      L := GetDirLen(CurDir^); if Abort then L := 0;
      ClrIO;
    end else L := 0;
    Files^.AtInsert(0, NewFileRec('..',{$IFNDEF OS2}'..'{$ENDIF},L,0,Directory,CurDir));
   end;
  GetFilesColl := Files;
{JO: см. выше комментарий к строке, начинающейся с "if SortMode = -1" }
{if (SortMode <> -1) and (SortMode <> psmUnsorted) and (Files^.Count>0) then Files^.Sort;}
{/JO}
{$IFDEF CHDIRDEBUG}
 PanelState^:=StartChar;
{$ENDIF}
{$IFDEF CHDIRTIMER}
 if ShiftState and (kbLeftShift+kbRightShift) <> 0 then begin
  DDTimer:=Get100s-DDTimer;
  ev.what:=evCommand;
  ev.command:=cmShowTimeInfo;
  ev.infoptr:=nil;
  Application^.PutEvent(ev);
 end;
{$ENDIF}
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
function TDrive.GetDirectory;
 var
     I,J: Integer;
     TFiles: Word;
     FreeSpc, TotalLen: TSize;
     Files: PFilesCollection;
     P: PFileRec;
     SR: lSearchRec;
     AllFiles, ShowD: Boolean;
     Descriptions: PDIZCol;
{DataCompBoy
  function Get32(S: String): LongInt;
   var I, J, K: LongInt;
  begin
    J := 0;
    for I := Length(S) downto 1 do
     begin
       if (S[I] >= '0') and (S[I] <= '9') then K := Byte(S[I])-48
                                          else K := Byte(S[I])-55;
       Inc(J, K * LongInt(1 shl (5*(Length(S)-I))));
     end;
     Get32 := J;
  end;
}
begin
 ClrIO;
 TFiles := 0; DOSError := 0; TotalLen := 0;
 DIZOwner := ''; FreeSpace := ''; TotalInfo := '';
 Descriptions := nil;
 Abort := False;
 ShowD := (Flags and psShowDescript <> 0);

 if ShowD or (FMSetup.Options and fmoAlwaysCopyDesc <> 0) or
    (SortMode = psmDIZ) or (PanelFlags and fmiDescriptionInBottom <> 0)
    then PrepareDIZ(CurDir, DIZOwner, Descriptions);

 Files := GetFilesColl(TotalLen, TFiles, PanelFlags, @CurDir, FileMask, Owner, SortMode, (Flags and psShowLongName <> 0));

 NoMemory := not MemOK;

 TotalInfo := CalcTotalInfo(Files^.Count, TFiles, TotalLen);

 if PanelFlags and fmiFree <> 0 then GetFreeSpace(FreeSpace);

 if Descriptions <> nil then
 begin
   TossDescriptions(@DizOwner, Files, Descriptions);
   Dispose(Descriptions,Done);
   if Files^.Sortmode = psmDIZ then Files^.Sort; {JO}
 end;
 GetDirectory := Files;
end;
        {-DataCompBoy-}

function TDrive.isUp;
begin
 {if Length(CurDir)>3 then}isUp := False{ else isUp:=true;}
end;

procedure TDrive.RereadDirectory;
begin
 if Prev <> nil then Prev^.RereadDirectory(S);
end;

        {-DataCompBoy-}
procedure TDrive.GetDirInfo;
begin
  ReadDiskInfo(CurDir, B, Flags);
end;
        {-DataCompBoy-}

procedure TDrive.KillUse;
begin
 if Prev <> nil then Prev^.KillUse;
end;

        {-DataCompBoy-}
procedure TDrive.GetDown;
 var FR: TFileRec;
     S,S1 {$IFDEF OS2} ,S2 {$ENDIF}: String;
begin
    if P=nil then exit;
    FR := P^; FR.Name[9] := ' ';
    if (FR.Attr and Directory = 0) or (FR.Attr and $8000 <> 0) and (FR.Size >= 0)
    or (PFilePanel(Owner)^.PanelFlags and fmiDirLen <> 0)
     {or (FR.Size >= 0)}
      then S := FileSizeStr( FR.Size )
      else if FR.TType = ttUpDir
        then S := GetString( dlUpDir )
        else S := GetString( dlSubDir );
    MakeDate(DateMode, FR.Day, FR.Month, FR.Yr, FR.Hour, FR.Minute, S1);
{$IFNDEF OS2}
    MoveStr(B, MakeFileName(FR.Name), C);
{$ELSE}
    S2 := GetLFN(FR.LFN);
    if Byte(S2[0]) > 14 then
      begin
        S2[0] := #14;
        S2[14] := FMSetup.RestChar[1];
      end;
    MoveStr(B,S2,C);
{$ENDIF}
    MoveStr(TWordArray(B)[22-Length(S)+CountryInfo.TimeFmt],S , C);
    MoveStr(TWordArray(B)[23+CountryInfo.TimeFmt],S1, C);
end;
        {-DataCompBoy-}

function TDrive.GetRealName;
begin
 GetRealName := GetDir;
end;

        {-DataCompBoy-}
function TDrive.GetRealDir;
 var
    MM: record
         case byte of
          1: (L: LongInt; S: String[1]);
          2: (C: Char);
        end;
    D: PDialog;
    C: Char;
    S: String;
begin
 if (DriveType = dtDisk) and
    not((word((@CurDir[1])^)=$2F2F{//}) or (word((@CurDir[1])^)=$5C5C{\\})) then
  begin
   C := GetCurDrive;
   if C = CurDir[1] then
    begin
      ClrIO;
      NeedAbort := True;
      lGetDir(0, S); if Abort then S := CurrentDirectory;
      NeedAbort := True; Advance2.lChDir(CurDir);
      repeat
       Abort := False; NeedAbort := True;
       lGetDir(0, CurDir);
       if Abort then
        begin
         repeat
           MM.L := 0; MM.C := GetCurDrive; MM.S := MM.C;
           D := PDialog(LoadResource(dlgDiskError));
           if D <> nil then
            begin
             D^.SetData(MM);
             Application^.ExecView(D);
             D^.GetData(MM);
             Dispose(D,Done);
            end;
           UpStr(MM.S);
           if ValidDrive(MM.S[1]) then
            begin SetCurDrive(MM.S[1]); Break; end;
         until False;
         Abort := True;
        end;
      until not Abort;
      NeedAbort := False;
      lGetDir(0,CurDir);
      Advance2.lChDir(S);
    end else
    begin
      Advance2.lChDir(CurDir);
      if not Abort then
       repeat
        Abort := False; NeedAbort := True;
        lGetDir(0, CurDir);
        if Abort then
         begin
          repeat
            MM.L := 0; MM.C := GetCurDrive; MM.S := MM.C;
            D := PDialog(LoadResource(dlgDiskError));
            if D <> nil then
             begin
              D^.SetData(MM);
              Application^.ExecView(D);
              D^.GetData(MM);
              Dispose(D,Done);
             end;
            UpStr(MM.S);
            if ValidDrive(MM.S[1]) then
             begin SetCurDrive(MM.S[1]); Break; end;
          until False;
          Abort := True;
         end;
       until not Abort;
      SetCurDrive(C);
    end;
   GetRealDir := CurDir;
  end else GetRealDir := GetDir;
  NeedAbort := False;
end;
        {-DataCompBoy-}

procedure TDrive.HandleCommand;
begin
end;

function TDrive.GetFullFlags;
begin
 GetFullFlags := psShowSize + psShowDate + psShowTime;
end;

procedure TDrive.EditDescription;
begin
  if (DriveType = dtDisk) and (PF^.TType <> ttUpDir) then
    SetDescription(PF, DIZOwner);
end;

        {-DataCompBoy-}
procedure TDrive.GetDirLength(PF: PFileRec);
 var
     J: LongInt;
     I: TSize;
     S: String;
begin
  if (PF^.Attr and $8000 <> 0) or (PF^.Attr and Directory = 0) then Exit;
  S := PF^.Owner^;
  if PF^.TType <> ttUpDir then S := MakeNormName(S, GetLFN(PF^.LFN));
  I := 1;
  PF^.Size := CountDirLen(S, True, I, Integer(J));
  if not Abort then PF^.Attr := PF^.Attr or $8000;
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
function TDrive.OpenDirectory(const Dir: String): PDrive;
  var
      I: LongInt;
      MemReq: LongInt;
      P: PString;
      PI: PView;
      PDrv: PDrive;
      Dirs: PStringCollection;
      Files:PFilesCollection;
      tmr: TEventTimer;

  procedure AddDirectory(S: String);
  begin
     if not MemOk then exit;
     if S[Length(S)] <> '\' then AddStr(S, '\');
     S[1]:=LowCase(S[1]);
     Dirs^.Insert(NewStr(S));
  end;

  procedure ReadDir(Dr: PString);
    var SR: lSearchRec;
  begin
     ClrIO;
     lFindFirst(Dr^+x_x, AnyFile, SR); {JO}
     while not Abort and (DOSError = 0) and MemOk do
       begin
         if (SR.SR.Attr and Hidden = 0) or (not Security) then
           if SR.SR.Attr and Directory = 0 then
             begin
               Files^.AtInsert(Files^.Count, NewFileRec(SR.FullName,
                                                        {$IFNDEF OS2}
                                                        SR.SR.Name,
                                                        {$ENDIF}
                                                        SR.FullSize,
                                                        SR.SR.Time,
                                                        SR.SR.Attr,
                                                        Dr));
               Inc(MemReq, SizeOf(TFileRec));
               Inc(MemReq, Length(Dr^+SR.FullName)+1);
             end else
             if{$IFNDEF OS2}(SR.SR.Name[1] <> '.') and (SR.FullName <> '.') and (SR.FullName <> '..')
                         {$ELSE}(SR.SR.Name <> '..') and (SR.SR.Name <> '.'){$ENDIF}
                                     then AddDirectory(Dr^+SR.FullName);
         lFindNext(SR);
       end;
    lFindClose(SR);
{$IFDEF OS2}
    if DosError = 49 then messagebox(GetString(dl_CodePage_FS_Error), nil, mfError+mfOKButton);
{$ENDIF}
  end;

begin
  NewTimer(Tmr,0);
  Dirs := New(PStringCollection, Init($10,$10));

  PI := WriteMsg(GetString(dlReadingList));
  New(Files, Init($10, $10)); Files^.SortMode := psmOrdered;
  AddDirectory(lFExpand(Dir));
  I := Dirs^.Count-1;
  MemReq := LowMemSize;
  Abort:=False;
  while (I >= 0) and (not Abort) and (System.MemAvail > MemReq) and MemOk do
    begin
      I := Dirs^.Count-1;
      {if Files^.Count >= 2000 then Break;}
      while (I >= 0) and (not Abort) and (System.MemAvail > MemReq) and MemOk do
        begin
          UpdateWriteView(PI);
          P := Dirs^.At(I);
          if (P^[1] >= 'a') and (P^[1] <= 'z') then
            begin
              P^[1]:=UpCase(P^[1]);
              Dirs^.AtDelete(I);
              Dirs^.Insert(P);
              ReadDir(P);
              if TimerExpired(Tmr) then
              begin
                NewTimer(Tmr, 1);
                if ESC_Pressed then Abort := True;
              end;
              Break;
            end;
          Dec(I);
        end;
    end;
  PI^.Free;
  Files^.Sort;
  PDrv := New(PFindDrive, Init(GetString( dlBranch )+Dir, Dirs, Files, FreeByte));
  PDrv^.NoMemory := System.MemAvail <= MemReq;
  OpenDirectory := PDrv;
end;
        {-DataCompBoy-}


function CalcTotalInfo;
var
  S: string;
begin
 if A = 0 then S := GetString(dlDINoFiles)
  else begin
        S := GetString(dlTotal);
        if B = 1 then S := S+'1~ '+GetString(dlDIFile)
                 else S := S+ItoS(B)+'~ '+GetString(dlDIFiles);
        S := S + GetString(dlDIWith) + '~';
        if C = 1 then S := S + '1~ '+GetString(dlDIByte)
                 else S := S + FStr(C)+'~ '+GetString(dlDIBytes);
       end;
 CalcTotalInfo := S;
end;

procedure RereadDirectory;
 var Event: TEvent;

 procedure Action(View: PView); {$IFDEF BIT_16}far;{$ENDIF}
 begin
  Event.What := evCommand; Event.Command := cmRereadDir;
  Event.InfoPtr := @Dir;
  View^.HandleEvent(Event);
 end;

begin
 {$IFNDEF OS2}
 Dir:=lfGetLongFileName(Dir);
 {$ENDIF}
 Desktop^.ForEach(@Action);
end;

end.
