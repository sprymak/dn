{///////////////////////////////////////// ////////////////////////////////
//
//  Dos Navigator Open Source 1.51.08
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

unit Drives;

interface

uses
  Lfn, Files, FlTl,
  fnotify,
  Dos, Objects, Memory, Views, Drivers, DnIni, FilesCol, DiskInfo,
    {FStorage,}
  advance, advance1, advance2, advance3, Collect;

const
  dsActive = 0;
  dsInvalid = 1;

  {$IFDEF Win32}
const
  FILE_ATTRIBUTE_REPARSE_POINT = $400;
  {$ENDIF}

type
  PDrive = ^TDrive;
  TDrive = object(TObject)
    {Cat: этот объект вынесен в плагинную модель; изменять крайне осторожно!}
    Owner: Pointer;
    Prev: PDrive;
    DriveType: (dtUndefined, dtDisk, dtFind, dtTemp, dtList,
      dtArcFind, dtArc, dtLink, dtArvid);
    CurDir: String; {DataCompBoy}
    DizOwner: String; {DataCompBoy}
    NoMemory: boolean;
    Flags: AWord;
    LFNLen: byte;
    EXTLen: byte;
    DirFLP, FilFLP: AWord;
    Param, OldParam: byte;
    innum: byte;
    SizeX: longInt;
    {$IFDEF OS2}
    ShowLogNames: boolean;
    {$ENDIF}
    Constructor Init(ADrive: byte; AOwner: Pointer; Num: byte);
    Constructor Load(var s: TStream);
    procedure Store(var s: TStream); virtual;
    procedure KillUse; virtual;
    procedure lChDir(ADir: String); virtual; {DataCompBoy}
    function GetDir: String; virtual; {DataCompBoy}
    function GetDirectory(SortMode, PanelFlags: integer; const
      FileMask: String;
    var FreeSpace: String; var TotalInfo: TSize): PCollection;
      virtual;
    procedure CopyFiles(Files: PCollection; Own: PView; MoveMode:
      boolean); virtual;
    procedure CopyFilesInto(Files: PCollection; Own: PView; MoveMode:
      boolean); virtual;
    procedure EraseFiles(Files: PCollection); virtual;
    procedure UseFile(P: PFileRec; Command: word); virtual;
      {DataCompBoy}
    procedure GetFreeSpace(var s: String); virtual;
    function Disposable: boolean; virtual;
    function GetRealName: String; virtual;
    function GetInternalName: String; virtual;
    procedure GetFull(var B; P: PFileRec; C, Sc: word); virtual;
      {DataCompBoy}
    procedure GetEmpty(var B; Sc: word); virtual;
    function CalcLengthWithoutName: integer; virtual;
    function CalcLength: integer; virtual;
    procedure RereadDirectory(s: String); virtual; {DataCompBoy}
    procedure MakeTop(var s: String); virtual;
    procedure GetDown(var B; C: word; P: PFileRec); virtual;
      {DataCompBoy}
    procedure HandleCommand(Command: word; InfoPtr: Pointer);
      virtual;
    procedure GetDirInfo(var B: TDiskInfoRec); virtual;
    function GetRealDir: String; virtual;
    procedure MakeDir; virtual;
    function isUp: boolean; virtual;
    procedure ChangeUp(var s: String); virtual;
    procedure ChangeRoot; virtual;
    function GetFullFlags: word; virtual;
    procedure EditDescription(PF: PFileRec); virtual; {DataCompBoy}
    procedure GetDirLength(PF: PFileRec); virtual; {DataCompBoy}
    procedure GetParam(n: byte); virtual; {DataCompBoy}
    destructor Done; virtual;
    function OpenDirectory(const Dir: String): PDrive; virtual;
    end;

procedure RereadDirectory(Dir: String);
function GetFilesColl(
  var TotalLen: TSize;
  var TFiles: word;
  PanelFlags: integer;
  CurDir: PString;
  const FileMask: String;
  Owner: Pointer;
  SortMode: integer
  {$IFNDEF OS2}; LFNActive: boolean {$ENDIF}): PFilesCollection;

const
  TempDirs: PSortedCollection = nil;
  TempFiles: PFilesCollection = nil;

implementation
uses
  Startup, Tree, DNApp, RStrings, FileCopy, Eraser, FlPanel,
    Commands,
  Dialogs, FileFind, FlPanelX, Filediz, CmdLine
  , xTime, Messages;

const
  LowMemSize = $4000; {Local setting}

type
  {-DataCompBoy-}
  PDesc = ^TDesc;
  TDesc = record
    Name: String;
    DIZ: PString;
    Line: longInt;
    end;
  {-DataCompBoy-}

  PDIZCol = ^TDIZCol;
  TDIZCol = object(TSortedCollection)
    procedure FreeItem(P: Pointer); virtual;
    function Compare(P1, P2: Pointer): integer; virtual;
    end;

function ESC_Pressed: boolean;
  var
    E: TEvent;
  begin
    Application^.Idle;
    GetKeyEvent(E);
    ESC_Pressed := (E.What = evKeyDown) and (E.KeyCode = kbESC)
  end;

{-DataCompBoy-}
procedure TDIZCol.FreeItem;
  begin
    if P <> nil then
      begin
        DisposeStr(PDesc(P)^.DIZ);
        Dispose(PDesc(P));
      end;
  end;
{-DataCompBoy-}

function TDIZCol.Compare;
    {  используется фактически только в TDIZCol.Search}
  begin{  унаследованном от TSortedCollection.Search}
    if LowStrg(PDesc(P1)^.Name) < LowStrg(PDesc(P2)^.Name) then
        Compare := -1
    else if LowStrg(PDesc(P1)^.Name) = LowStrg(PDesc(P2)^.Name) then
        Compare := 0
    else
      Compare := 1;
  end;

{Cat: Переделал так, чтобы бралась информация не для буквы диска, а для
      полного пути. Это должно повлиять в ситуациях сетевых путей, а
      также когда один диск подмонтирован к другому. Также отслеживается
      случай ошибки на диске - вместо "-1" возвращаем пустую строку.}
procedure TDrive.GetFreeSpace;
  var
    FreeSpc: TSize;
    Buffer: array[0..255] of Char;
  begin
    Strings.StrPCopy(@Buffer, CurDir);
    FreeSpc := VpSysLow.SysDiskFreeLongX(@Buffer);
    if FreeSpc < 0 then
      s := ''
    else
      s := '~'+FStr(FreeSpc)+'~';
  end;
{/Cat}

{-DataCompBoy-}
procedure TDrive.GetParam;
  begin
    OldParam := Param;
    Param := n;
    with ColumnsDefaultsDisk do
      begin
        Flags := Params[n].Param;
        LFNLen := StoI(Params[n].LFNLen);
        if (LFNLen = 0) or (LFNLen > 252) then
          LFNLen := 252;
        EXTLen := StoI(Params[n].EXTLen);
        if EXTLen > 252 then
          EXTLen := 252;
        case FileCase of
          0:
            FilFLP := 0;
          1:
            FilFLP := flnLowCase;
          2:
            FilFLP := flnCapitalCase;
          3:
            FilFLP := flnUpCase;
          4:
            FilFLP := 65535;
        end {case};
        case DirsCase of
          0:
            DirFLP := 0;
          1:
            DirFLP := flnLowCase;
          2:
            DirFLP := flnCapitalCase;
          3:
            DirFLP := flnUpCase;
          4:
            DirFLP := 65535;
        end {case};
      end;
  end { TDrive.GetParam };
{-DataCompBoy-}

{-DataCompBoy-}
Constructor TDrive.Init;
  begin
    TObject.Init;
    Owner := AOwner;
    if Num < 1 then
      GetParam(1)
    else
      GetParam(Num);
    innum := Num;
    ClrIO;
    if ADrive < $1B then
      lGetDir(ADrive, CurDir) {A: ... Z:}
    else
      CurDir := ''; {any other - i.e. \\server\share}
    {$IFDEF Win32}
    if DnIni.AutoRefreshPanels then{JO}
      NotifyAddWatcher(CurDir); {Cat}
    {$ENDIF}
    DriveType := dtDisk;
    NoMemory := False;
    {$IFDEF OS2}
    ShowLogNames := False;
    {$ENDIF}
    OldParam := Param;
  end { TDrive.Init };
{-DataCompBoy-}

{-DataCompBoy-}
Constructor TDrive.Load(var s: TStream);
  begin
    TObject.Init;
    Prev := PDrive(s.Get);
    s.ReadStrV(CurDir);
      {S.Read(CurDir[0], 1); S.Read(CurDir[1], Length(CurDir));}
    s.Read(Flags, SizeOf(Flags));
    s.Read(LFNLen, 1);
    s.Read(EXTLen, 1);
    s.Read(DirFLP, 2);
    s.Read(FilFLP, 2);
    s.Read(Param, 1);
    s.Read(OldParam, 1);
    {$IFDEF OS2}
    s.Read(ShowLogNames, 1);
    {$ENDIF}
    {$IFDEF Win32}
    if DnIni.AutoRefreshPanels then{JO}
      NotifyAddWatcher(CurDir); {Cat}
    {$ENDIF}
    DriveType := dtDisk;
    NoMemory := False;
  end { TDrive.Load };
{-DataCompBoy-}

{-DataCompBoy-}
procedure TDrive.Store(var s: TStream);
  begin
    s.Put(Prev);
    s.WriteStr(@CurDir); {S.Write(CurDir, Length(CurDir)+1);}
    s.Write(Flags, SizeOf(Flags));
    s.Write(LFNLen, 1);
    s.Write(EXTLen, 1);
    s.Write(DirFLP, 2);
    s.Write(FilFLP, 2);
    s.Write(Param, 1);
    s.Write(OldParam, 1);
    {$IFDEF OS2}
    s.Write(ShowLogNames, 1);
    {$ENDIF}
  end;
{-DataCompBoy-}

destructor TDrive.Done;
  begin
    if Prev <> nil then
      Dispose(Prev, Done);
    inherited Done;
  end;

function TDrive.Disposable;
  begin
    Disposable := True;
  end;

{-DataCompBoy-}
procedure TDrive.ChangeUp;
  begin
    s := GetName(CurDir);
    lChDir(MakeNormName(CurDir, '..'));
    if Abort then
      exit;
    lGetDir(0, CurDir);
    if Abort then
      exit;
  end;
{-DataCompBoy-}

{-DataCompBoy-}
procedure TDrive.ChangeRoot;
  var
    i: word;
    B: boolean;
  begin
    {Cat: проверяем на сетевой путь}
    if CurDir[1] = '\' then
      begin
        B := False;
        for i := 3 to Length(CurDir) do
          if CurDir[i] = '\' then
            if B then
              begin
                CurDir := Copy(CurDir, 1, i-1);
                lChDir(CurDir);
                break;
              end
            else
              B := True;
      end
    else
      {/Cat}
      begin
        lChDir(CurDir[1]+':\');
        {
      if Abort then
        Exit;
      }
        lGetDir(0, CurDir);
        {
      if Abort then
        Exit;
      }
      end;
  end { TDrive.ChangeRoot };
{-DataCompBoy-}

function TDrive.CalcLengthWithoutName;
  begin
    CalcLengthWithoutName :=
    (10+CountryInfo.TimeFmt)*byte(Flags and psShowSize <> 0)+
    9*byte(Flags and psShowDate <> 0)+
    (7-CountryInfo.TimeFmt)*byte(Flags and psShowTime <> 0)+
    9*byte(Flags and psShowCrDate <> 0)+
    (7-CountryInfo.TimeFmt)*byte(Flags and psShowCrTime <> 0)+
    9*byte(Flags and psShowLADate <> 0)+
    (7-CountryInfo.TimeFmt)*byte(Flags and psShowLATime <> 0);
  end;

function TDrive.CalcLength;
  var
    B: word;
  begin
    {$IFNDEF OS2} if Flags and psShowLongName <> 0 then{$ENDIF}
      B := CalcLengthWithoutName+LFNLen+1 {$IFNDEF OS2}
    else
      B := CalcLengthWithoutName+13 {$ENDIF};
    if B > MaxViewWidth then
      B := MaxViewWidth;
    if Flags and psShowDescript <> 0 then
      CalcLength := MaxViewWidth
    else
      CalcLength := B;
  end;

procedure TDrive.MakeTop;
  begin
    if (LFNLen < 250) {$IFNDEF OS2} or (Flags and psShowLongName = 0)
        {$ENDIF}
    then
      begin
        {$IFNDEF OS2}
        if Flags and psShowLongName <> 0 then
          begin
            {$ENDIF}
            if LFNLen <= SizeX then
              s := CenterStr(GetString(dlTopLFN), LFNLen)+GetString(
                dlTopSplit)
            else
              s := AddSpace(CenterStr(GetString(dlTopLFN), SizeX),
                LFNLen)
              {$IFNDEF OS2}
              ;
          end
        else
          s := GetString(dlTopName)
          {$ENDIF}
      end
    else
      s := '';
    if Flags and psShowSize <> 0 then
      s := s+Copy(GetString(dlTopSize), 2-CountryInfo.TimeFmt, 255);
    if Flags and psShowDate <> 0 then
      s := s+GetString(dlTopDate);
    if Flags and psShowTime <> 0 then
      s := s+Copy(' '+GetString(dlTopTime), 1+CountryInfo.TimeFmt,
        255);
    if Flags and psShowCrDate <> 0 then
      s := s+GetString(dlTopCrDate);
    if Flags and psShowCrTime <> 0 then
      s := s+Copy(' '+GetString(dlTopCrTime), 1+CountryInfo.TimeFmt,
        255);
    if Flags and psShowLADate <> 0 then
      s := s+GetString(dlTopLADate);
    if Flags and psShowLATime <> 0 then
      s := s+Copy(' '+GetString(dlTopLATime), 1+CountryInfo.TimeFmt,
        255);
    if (LFNLen >= 250) {$IFNDEF OS2} and (Flags and psShowLongName <> 0)
        {$ENDIF}
    then
      s := s+AddSpace(CenterStr(GetString(dlTopLFN), SizeX-Length(s)),
        LFNLen)
    else if Flags and psShowDescript <> 0 then
      s := s+' '+GetString(dlPnlDescription)+' '+Strg(#32, 255);
  end { TDrive.MakeTop };

procedure TDrive.GetFull;
  var
    s: String;
    s1, s2, S3: String[40];
    X: AWord;
    NFM: TNameFormatMode;
    OPT: word;
    LogName: String;
    LS: byte;
    LFNLonger250: boolean;
    CO: word;
  label Longer250, Longer250_1;
  begin
    X := 0;
    CO := C;
    LFNLonger250 := (LFNLen >= 250) {$IFNDEF OS2} and (Flags and
      psShowLongName <> 0) {$ENDIF};
    if not LFNLonger250 then
        {JO: если ширина колонки имени больше 250 символов, её показываем после остальных колонок}
Longer250:
      begin
        {$IFNDEF OS2}
        if Flags and psShowLongName <> 0 then
          begin
            s := {$IFDEF RecodeWhenDraw}CharToOemStr {$ENDIF}(P^.
              FlName[True]);
            {$ELSE}
            s := P^.FlName[True];
            {$ENDIF}
            NFM := nfmNull;
            if EXTLen = 0
            then
              OPT := flnPreferName or flnUseCutChar or flnPadRight or
                flnHighlight or flnHandleTildes
            else
              OPT := flnHardExtArea or flnAutoHideDot or
                flnUseCutChar or flnPadRight or flnHighlight or
                flnHandleTildes;
            if P^.Selected and (FMSetup.Show and fmsTagChar <> 0)
            then
              OPT := OPT+flnSelected;

            case P^.Attr and (Hidden+SysFile) of
                {Pavel Anufrikov -> }
              Hidden:
                NFM := nfmHidden;
              SysFile:
                NFM := nfmSystem;
              Hidden+SysFile:
                NFM := nfmHiddenSystem;
            end {case}; { <- Pavel Anufrikov}

            if P^.Attr and Directory <> 0 then
              if DirFLP <> 65535
              then
                OPT := OPT or DirFLP
              else if (not IsMixedCase(s)) {JO}
              then
                OPT := OPT or flnUpCase
              else
            else if FilFLP <> 65535
            then
              OPT := OPT or FilFLP
            else if (not IsMixedCase(s)) {JO}
            then
              OPT := OPT or flnLowCase
            else
              ;
            {$IFDEF OS2}
            if ShowLogNames then
              begin
                LogName := '';
                if (s <> '..') and (GetEAString(MakeNormName(P^.
                    Owner^, s), '.LONGNAME', LogName, True) = 0) and
                  (LogName <> '')
                then
                  s := #221+FormatLongName(LogName, LFNLen-1, EXTLen,
                    OPT and not flnUpCase and not flnLowCase and not
                    flnCapitalCase, NFM, nil)
                else
                  s := FormatLongName(s, LFNLen, EXTLen, OPT, NFM,
                    nil);
              end
            else
              {$ENDIF}
              s := FormatLongName(s, LFNLen, EXTLen, OPT, NFM, nil);

            if P^.Selected and (FMSetup.Show and fmsTagChar = 0)
            then
              s := '~'+s+'~';
            {$IFNDEF OS2}
          end
        else
          begin
            NFM := nfmNull;
            OPT := flnHardExtArea or flnAutoHideDot or flnPadRight or
              flnHighlight or flnHandleTildes;
            if P^.Selected and (FMSetup.Show and fmsTagChar <> 0)
            then
              OPT := OPT+flnSelected;

            case P^.Attr and (Hidden+SysFile) of
                {Pavel Anufrikov -> }
              Hidden:
                NFM := nfmHidden;
              SysFile:
                NFM := nfmSystem;
              Hidden+SysFile:
                NFM := nfmHiddenSystem;
            end {case}; { <- Pavel Anufrikov}

            if P^.Attr and SysFile <> 0 then
              OPT := OPT or flnCapitalCase;

            if P^.Attr and Directory <> 0 then
              OPT := OPT or flnUpCase
            else
              OPT := OPT or flnLowCase;
            s := FormatLongName( {$IFDEF RecodeWhenDraw}CharToOemStr
              {$ENDIF}
            (MakeFileName(P^.FlName[False])), 12, 3, OPT, NFM, nil);
            if P^.Selected and (FMSetup.Show and fmsTagChar = 0)
            then
              s := '~'+s+'~';
          end;
        {$ENDIF}
        MoveCStr(TWordArray(B)[X], s, C);
        {$IFNDEF OS2} if Flags and psShowLongName <> 0 then{$ENDIF}
            Inc(X, LFNLen)
          {$IFNDEF OS2}
        else
          Inc(X, 12) {$ENDIF}
          ;
        if X >= 255 then
          exit;
        {-----------}
        if LFNLonger250 then
          goto Longer250_1;
        {JO: не понял, зачем DataCompBoy ввёл условие                                }
        {    PView(Owner)^.GetState(sfFocused), приводившее к исчезновению выделения }
        {    цветом всего, что идёт после имени у помеченных файлов при переходе     }
        {    на противоположную панель; без этого условия всё работает нормально     }
        if P^.Selected {and PView(Owner)^.GetState(sfFocused)} then
          begin
            C := C shr 8;
            Sc := Sc and $00FF+C shl 8;
          end;
        Inc(X);
        if X >= 255 then
          exit;
        TWordArray(B)[X-1] := Sc;
      end {условие 250 символов}
    else if P^.Selected then
      begin
        C := C shr 8;
        Sc := Sc and $00FF+C shl 8;
      end;

    if (P^.Size > 0) or (P^.Attr and Directory = 0) or (P^.Attr and
        $8000 <> 0)
      and (P^.Size >= 0)
    then
      s := FileSizeStr(P^.Size)
    else if P^.TType = ttUpDir
    then
      s := GetString(dlUpDir)
      {$IFDEF Win32}
    else if P^.Attr and FILE_ATTRIBUTE_REPARSE_POINT <> 0 then
      s := GetString(dlSymLink)
      {$ENDIF}
    else
      s := GetString(dlSubDir);
    for LS := 1 to(9-Length(s)+CountryInfo.TimeFmt) do
      s := ' '+s; {JO}
    with TDate4(P^.FDate) do
      MakeDate(DateMode, Day, Month, P^.Yr, Hour, Minute, s1);
    if P^.YrCreat <> 0 then
      with TDate4(P^.FDateCreat) do
        MakeDate(DateMode, Day, Month, P^.YrCreat, Hour, Minute, s2)
      else
      s2 := '';
    if P^.YrLAcc <> 0 then
      with TDate4(P^.FDateLAcc) do
        MakeDate(DateMode, Day, Month, P^.YrLAcc, Hour, Minute, S3)
      else
      S3 := '';
    if Flags and psShowSize <> 0 then
      begin
        MoveStr(TWordArray(B)[X], s, C);
        X := X+10+CountryInfo.TimeFmt;
        if X >= 255 then
          exit;
        TWordArray(B)[X-1] := Sc;
      end;
    if Flags and psShowDate <> 0 then
      begin
        MoveStr(TWordArray(B)[X], Copy(s1, 1, 8), C);
        Inc(X, 9);
        if X >= 255 then
          exit;
        TWordArray(B)[X-1] := Sc;
      end;
    if Flags and psShowTime <> 0 then
      begin
        MoveStr(TWordArray(B)[X], Copy(s1, 10, 6-CountryInfo.TimeFmt), C
          );
        Inc(X, 7-CountryInfo.TimeFmt);
        if X >= 255 then
          exit;
        TWordArray(B)[X-1] := Sc;
      end;
    if Flags and psShowCrDate <> 0 then
      begin
        MoveStr(TWordArray(B)[X], Copy(s2, 1, 8), C);
        Inc(X, 9);
        if X >= 255 then
          exit;
        TWordArray(B)[X-1] := Sc;
      end;
    if Flags and psShowCrTime <> 0 then
      begin
        MoveStr(TWordArray(B)[X], Copy(s2, 10, 6-CountryInfo.TimeFmt), C
          );
        Inc(X, 7-CountryInfo.TimeFmt);
        if X >= 255 then
          exit;
        TWordArray(B)[X-1] := Sc;
      end;
    if Flags and psShowLADate <> 0 then
      begin
        MoveStr(TWordArray(B)[X], Copy(S3, 1, 8), C);
        Inc(X, 9);
        if X >= 255 then
          exit;
        TWordArray(B)[X-1] := Sc;
      end;
    if Flags and psShowLATime <> 0 then
      begin
        MoveStr(TWordArray(B)[X], Copy(S3, 10, 6-CountryInfo.TimeFmt), C
          );
        Inc(X, 7-CountryInfo.TimeFmt);
        if X >= 255 then
          exit;
        TWordArray(B)[X-1] := Sc;
      end;
    if LFNLonger250 then
      begin
        C := CO;
        goto Longer250;
      end;
    if (Flags and psShowDescript <> 0) and ((DriveType = dtDisk) or
      (DriveType = dtArvid))
    then
      begin
        if P^.DIZ <> nil then
          s := Copy(AddSpace(CnvString(P^.DIZ^.DIZ), 255), 1,
            MaxViewWidth-X-1)
        else
          s := AddSpace(' ', MaxViewWidth-X-1);
        MoveStr(TWordArray(B)[X], s, C);
      end;
Longer250_1:
  end { TDrive.GetFull };

procedure TDrive.GetEmpty;
  var
    X: AWord;
  begin
    if (LFNLen < 250) {$IFNDEF OS2} or (Flags and psShowLongName = 0)
        {$ENDIF}
    then
      begin
        {$IFNDEF OS2} if Flags and psShowLongName <> 0 then{$ENDIF}
          X := LFNLen+1 {$IFNDEF OS2}
        else
          X := 13 {$ENDIF};
        if X >= 255 then
          exit;
        TWordArray(B)[X-1] := Sc;
      end
    else
      X := 0;
    if Flags and psShowSize <> 0 then
      begin
        X := X+10+CountryInfo.TimeFmt;
        if X >= 255 then
          exit;
        TWordArray(B)[X-1] := Sc;
      end;
    if Flags and psShowDate <> 0 then
      begin
        Inc(X, 9);
        if X >= 255 then
          exit;
        TWordArray(B)[X-1] := Sc;
      end;
    if Flags and psShowTime <> 0 then
      begin
        Inc(X, 7-CountryInfo.TimeFmt);
        if X >= 255 then
          exit;
        TWordArray(B)[X-1] := Sc;
      end;
    if Flags and psShowCrDate <> 0 then
      begin
        Inc(X, 9);
        if X >= 255 then
          exit;
        TWordArray(B)[X-1] := Sc;
      end;
    if Flags and psShowCrTime <> 0 then
      begin
        Inc(X, 7-CountryInfo.TimeFmt);
        if X >= 255 then
          exit;
        TWordArray(B)[X-1] := Sc;
      end;
    if Flags and psShowLADate <> 0 then
      begin
        Inc(X, 9);
        if X >= 255 then
          exit;
        TWordArray(B)[X-1] := Sc;
      end;
    if Flags and psShowLATime <> 0 then
      begin
        Inc(X, 7-CountryInfo.TimeFmt);
        if X >= 255 then
          exit;
        TWordArray(B)[X-1] := Sc;
      end;
  end { TDrive.GetEmpty };

procedure TDrive.EraseFiles;
  begin
    if Disposable then
      Eraser.EraseFiles(Files);
  end;

procedure TDrive.MakeDir;
  begin
    MakeDirectory;
  end;

procedure TDrive.CopyFiles;
  var
    B: boolean;
  begin
    if ReflectCopyDirection
    then
      RevertBar := Message(Desktop, evBroadcast, cmIsRightPanel, Own) <>
        nil
    else
      RevertBar := False;
    if Disposable then
      FileCopy.CopyFiles(Files, Own, MoveMode, 2*byte(TypeOf(Self) =
        TypeOf(TFindDrive)));
  end;

procedure TDrive.CopyFilesInto;
  var
    B: boolean;
  begin
    if ReflectCopyDirection
    then
      RevertBar := (Message(Desktop, evBroadcast, cmIsRightPanel,
        Own) <> nil)
    else
      RevertBar := False;
    FileCopy.CopyFiles(Files, Own, MoveMode, 0);
  end;

{-DataCompBoy-}
procedure TDrive.lChDir;
  var
    i: word;
    s: String;

  function AskRetry(Drive: Char): boolean;
    begin
      ClrIO;
      NeedAbort := False;
      SysErrorFunc(21, byte(Drive)-65);
      AskRetry := not Abort;
    end;

  function ValidPath(var ATestDir: String; Ask: boolean): boolean;
    var
      s: String;
      Drive: Char;
      Ok: boolean;
      i: word;
    begin
      Ok := False;
      ClrIO;
      NeedAbort := True;
      ATestDir := lFExpand(ATestDir);
      if (Length(ATestDir) <> 3) and (ADir[Length(ATestDir)] = '\')
      then
        SetLength(ATestDir, Length(ATestDir)-1);
      {Cat: проверяем на сетевой путь}
      if (Length(ATestDir) > 2) and (ATestDir[1] = '\') and (
          ATestDir[2] = '\')
      then
        begin
          Ok := False;
          for i := 3 to Length(ATestDir) do
            if ATestDir[i] = '\' then
              begin
                Ok := True;
                break;
              end;
        end
      else
        {/Cat}
        begin
          if (Length(ATestDir) > 1) and (ATestDir[2] = ':') then
            Drive := ATestDir[1]
          else
            Drive := Char(GetDrive+byte('A'));
          s := Drive+':\';
          repeat
            ClrIO;
            NeedAbort := True;
            Lfn.lChDir(s);
            i := IOResult;
            Abort := Abort or (i <> 0);
            if Abort then
              begin
                if Ask and AskRetry(Drive) then
                  continue;
              end
            else
              Ok := True;
            break;
          until False;
        end;
      if Ok then
        repeat
          ClrIO;
          NeedAbort := True;
          Lfn.lChDir(ATestDir);
          i := IOResult;
          Abort := Abort or (i <> 0);
          if Abort then
            begin
              s := GetPath(ATestDir);
              if (Length(s) <> 3) and (s[Length(s)] = '\') then
                  SetLength(s, Length(s)-1);
              if s <> ATestDir then
                begin
                  ATestDir := s;
                  continue;
                end;
              Ok := False;
            end;
          break;
        until False;
      ClrIO;
      NeedAbort := False;
      ValidPath := Ok;
    end { ValidPath };

  begin { TDrive.lChDir }
    {$IFDEF Win32}
    if DnIni.AutoRefreshPanels then{JO}
      NotifyDeleteWatcher(CurDir); {Cat}
    {$ENDIF}
    NeedAbort := True;
    if ValidPath(ADir, True) then
      begin
        {$IFDEF Win32}
        if DnIni.AutoRefreshPanels then{JO}
          NotifyAddWatcher(ADir); {Cat}
        {$ENDIF}
        CurDir := ADir;
        exit;
      end;
    if ValidPath(CurDir, False) then
      begin
        {$IFDEF Win32}
        if DnIni.AutoRefreshPanels then{JO}
          NotifyAddWatcher(CurDir); {Cat}
        {$ENDIF}
        {CurDir:=CurDir;}exit;
      end;
    ADir := 'C:\';
    if ValidPath(ADir, False) then
      begin
        {$IFDEF Win32}
        if DnIni.AutoRefreshPanels then{JO}
          NotifyAddWatcher(ADir); {Cat}
        {$ENDIF}
        CurDir := ADir;
        exit;
      end;
    ADir := 'A:\';
    if ValidPath(ADir, False) then
      begin
        {$IFDEF Win32}
        if DnIni.AutoRefreshPanels then{JO}
          NotifyAddWatcher(ADir); {Cat}
        {$ENDIF}
        CurDir := ADir;
        exit;
      end;
    CurDir := '';
  end { TDrive.lChDir };
{-DataCompBoy-}

{-DataCompBoy-}
function TDrive.GetDir;
  begin
    {$IFDEF OS_DOS}
    GetDir := lfGetLongFileName(CurDir);
    {$ELSE}
    GetDir := CurDir;
    {$ENDIF}
  end;
{-DataCompBoy-}

{-DataCompBoy-}
procedure TDrive.UseFile;
  var
    s: String;
  begin
    if P^.Owner <> nil then
      s := MakeNormName(P^.Owner^, P^.FlName[uLfn]);
    Message(Application, evCommand, Command, @S);
  end;
{-DataCompBoy-}
(*
        {-DataCompBoy-}
procedure ReadFileList(
     const CurDir: String;
     const FName: String;
     var Descriptions: PDIZCol);

var T: PTextReader;
    S: String;
    LS: Byte;
    S1: String;
    I: CondInt;
    CurLine: LongInt;
    PD: PDesc;
    zz: Boolean;
    J: Byte;
begin
  ClrIO;
  T := New(PTextReader, Init(MakeNormName(CurDir, FName)));
  if (T = nil) or Abort then Exit;
  CurLine := 0;
  While not T^.EOF and (IOResult = 0) do
    begin
      Inc(CurLine);
      S := T^.GetStr;
      LS := Length(S);
     {DelLeft(S);} {зачем Ритлабсы считали описаниями то, что начиналось с пробела - непонятно}
      S1 := '';
      if (S <> '') and not (S[1] in [' ',#9]) then
       begin
         zz := False;
         J := 1;
         repeat
           if S[J] = '"' then zz := not zz {JO: поддержка описаний для имён с пробелами}
             else S1 := S1 + S[J];
           Inc(J);
          {DelFC(S)}
         until not ((J <= LS) and ((not(S[J] in [' ',#9])) or zz));
         S := Copy(S, J, Length(S)-J+1);
         DelLeft(S);
         New(PD);
{$IFNDEF OS2}
         PD^.Name := LowStrg(Norm12(S1));
         PD^.Name[9] := ' ';
       {$IFDEF OS_DOS}
         PD^.LFN := AddLFN(lfGetLongFileName(MakeNormName(CurDir, S1)));
       {$ELSE}
         PD^.LFN := S1;
       {$ENDIF}
{$ELSE}
         PD^.Name := S1;
{$ENDIF}
         PD^.DIZ := NewStr(S);
         PD^.Line := CurLine;
         if not Descriptions^.Search(PD, I) then
            Descriptions^.AtInsert(I, PD)
         else
            Descriptions^.FreeItem(PD);
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
    t: PTextReader;
    s: String;
    CurLine: longInt;

    { прочитать непустую строку (хвостовые пробелы отбрасываются)}
  function ReadNextS: boolean;
    begin
      ReadNextS := False;
      repeat
        if t^.Eof then
          exit;
        s := t^.GetStr;
        if IOResult <> 0 then
          exit;
        Inc(CurLine);
        DelRight(s);
      until s <> '';
      ReadNextS := True;
    end;

  var
    LS: byte;
    s1: String;
    i: CondInt;
    PD: PDesc;
    j: longInt;
    NameEnd: longInt;
  label
    ReadNextLine, EndDescr, EndFile;

  begin { ReadFileList }
    ClrIO;
    t := New(PTextReader, Init(MakeNormName(CurDir, FName)));
    if (t = nil) or Abort then
      exit;
    PD := nil;
    CurLine := 0;
    if not ReadNextS then
      begin
        Dispose(t, Done);
        exit;
      end;
    while True do
      begin
        { Обработка нового описания. S уже прочитана.}
        if (s[1] in [' ', #9, '>']) then
          goto ReadNextLine;
          { игнорируем остаток
         предыдущего многострочного описания }

        LS := Length(s);
        if s[1] = '"' then{имя в кавычках - ищем вторую кавычку }
          begin
            NameEnd := 0;
            for j := 2 to LS do
              if s[j] = '"' then
                begin
                  NameEnd := j;
                  break;
                end;
            if NameEnd <= 2 then
              goto ReadNextLine;
            if NameEnd = LS then
              goto ReadNextLine;
              { пустое описание никого не интересует }
            s1 := Copy(s, 2, NameEnd-2);
          end
        else
            {имя не в кавычках - ищем пробел или Tab. При этом
        благодаря DelRight после него что-то должно быть }
          begin
            NameEnd := Pos(' ', s);
            j := Pos(#9, s);
            if (j <> 0) and (j < NameEnd) then
              NameEnd := j // заведомо не 0
            else
              begin
                if NameEnd = 0 then
                  NameEnd := j; // может быть и 0
                if NameEnd = 0 then
                  goto ReadNextLine;
                  { пустое описание никого не интересует }
              end;
            s1 := Copy(s, 1, NameEnd-1);
          end;
        if PD = nil then
          New(PD);
        PD^.Name := s1;
        PD^.Line := CurLine;
        if Descriptions^.Search(PD, i) then
          goto ReadNextLine;
        System.Delete(s, 1, NameEnd);
        DelLeft(s);
        s1 := s;

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
              break;
            if not (s[1] in [' ', #9, '>']) then
              break;
            j := 2;
            while (j <= Length(s)) and (s[j] = ' ') do
              Inc(j);
            LS := Length(s1);
            if LS >= 253 then
              break;

            s1 := s1+#20' '+Copy(s, j, 253-LS);
          end;
        {JO: пока сделал так: "дочитанные" куски отделяются от основной части (и друг }
        {    от друга) символом #20, который ищется при редактировании и копировании}
        {    описаний и вся "дочитанная" часть описания после него (и он сам) отбрасывается }

        { конец дочитывания многострочного описания }

        PD^.DIZ := NewStr(s1);
        Descriptions^.AtInsert(i, PD);
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
    Dispose(t, Done);
  end { ReadFileList };

{-DataCompBoy-}
procedure PrepareDIZ(
  const CurDir: String;
  var DizOwner: String;
  var Descriptions: PDIZCol);
  var
    s: String;
    FName: String;
  begin
    ClrIO;
    s := GetDizOwner(CurDir, '', True);
    if s <> '' then
      begin
        FName := GetName(s);
        Descriptions := New(PDIZCol, Init($10, $10));
        ReadFileList(CurDir, FName, Descriptions);
        DizOwner := MakeNormName(CurDir, FName);
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
    i, j: CondInt;
    P: PFileRec;
    FName: String;
    PD: PDesc;
    iLFN: TUseLFN;
  begin
    for i := 1 to Files^.Count do
      begin
        P := Files^.At(i-1);
        for iLFN := High(TUseLFN) downto Low(TUseLFN) do
          begin
            FName := P^.FlName[iLFN];
            {if P^.Attr and (Directory+SysFile) <> 0 then LowStr(FName);}
            if Descriptions^.Search(@FName, j) then
              begin
                PD := PDesc(Descriptions^.At(j));
                New(P^.DIZ);
                P^.DIZ^.DIZ := PD^.DIZ;
                P^.DIZ^.Owner := PDizOwner;
                P^.DIZ^.Line := PD^.Line;
                P^.DIZ^.isDisposable := True;
                PD^.DIZ := nil;
                break;
              end
          end;
      end;
  end { TossDescriptions };
{-DataCompBoy-}

var
  NoMem: boolean;

  {-DataCompBoy-}
function GetFilesColl;
  var
    Files: PFilesCollection;
    SR: lSearchRec;
    NormMask: boolean;
    P: PFileRec;
    l: TSize;
    Q: longInt;
    fm: String;
    MemReq: longInt;
    MAvail: longInt;

  begin
    TFiles := 0;
    TotalLen := 0;
    Files := New(PFilesCollection, Init($10, $20));
    PFilesCollection(Files)^.Owner := Owner;
    PFilesCollection(Files)^.SortMode := SortMode;
    {$IFDEF Win32}
    PFilesCollection(Files)^.LFNActive := LFNActive; {JO}
    {$ENDIF}
    NormMask := not PackMask(FileMask, fm {$IFDEF OS_DOS}, False
      {$ENDIF});

    {JO: сначала один pаз опpеделяем объём доступной памяти, а затем по ходу дела}
    {    подсчтитываем насколько тpебования памяти pастут и не пpевысили ли они  }
    {    доступный изначально объём                                              }
    MemReq := LowMemSize;
    MAvail := MaxAvail;

    lFindFirst(MakeNormName(CurDir^, fm), AnyFile, SR); {JO}
    while (DOSError = 0) and not Abort and (IOResult = 0) and (
        MAvail > MemReq)
    do
      begin
        if ( {$IFNDEF OS2}(SR.SR.Attr and VolumeID = 0) and
          (SR.SR.Name <> '.')
          {$IFDEF Win32}
          and (SR.FullName <> '.')
          and not ((SR.FullName = '..') and (SR.SR.Name[1] <> '.'))
          {$ENDIF}
          {$ELSE}
          (SR.FullName <> '.')
          {$ENDIF}
          ) and
          ((not Security) or
          (SR.SR.Attr and Hidden = 0)
          or
          {$IFNDEF OS2}(SR.SR.Name[1] = '.') {$ELSE}(SR.FullName =
            '..') {$ENDIF}

          ) and
          (NormMask or
          (SR.SR.Attr and Directory <> 0) or
          ((InFilter(SR.FullName, FileMask) {$IFNDEF OS2} and
            LFNActive) or (InFilter(SR.SR.Name, FileMask) and not
            LFNActive {$ENDIF}))
          ) and
          ((SR.SR.Attr and Directory = 0) or
          not IsDummyDir(SR.SR.Name) or
          (Length(CurDir^) > 3)
          )
        then
          begin
            P := NewFileRec(SR.FullName {$IFNDEF OS2}, SR.SR.Name
              {$ENDIF}
            , SR.FullSize, SR.SR.Time, SR.SR.CreationTime
            , SR.SR.LastAccessTime, SR.SR.Attr, CurDir);
            Inc(MemReq, SizeOf(TFileRec));
            Inc(MemReq, Length(CurDir^+SR.FullName)+2);
            {$IFDEF Win32}
            P^.Attr := SR.SR.FindData.dwFileAttributes; {AK155}
            {$ENDIF}
            if SR.SR.Attr and Directory = 0 then
              begin
                TotalLen := TotalLen+P^.Size;
                Inc(TFiles);
              end
            else
              begin
                if PanelFlags and fmiDirLen <> 0 then
                  begin
                    if SR.FullName = '..' then
                      P^.Size := GetDirLen(CurDir^)
                    else
                      P^.Size := GetDirLen(MakeNormName(CurDir^, SR.
                        FullName));
                    if Abort then
                      begin
                        PanelFlags := PanelFlags and not fmiDirLen
                      end;
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
            if (SortMode = -1) or (SortMode = psmUnsorted) then
              with Files^ do
                AtInsert(Count, P)
              else
              Files^.Insert(P);
            {/JO}
          end;
        DOSError := 0;
        lFindNext(SR);
      end;
    lFindClose(SR);
    NoMem := (MAvail <= MemReq);
    if (Length(CurDir^) > 3) then
      if ((Files^.Count = 0) or
        (PFileRec(Files^.At(0))^.TType <> ttUpDir))
      then
        begin
          if PanelFlags and fmiDirLen <> 0 then
            begin
              l := GetDirLen(CurDir^);
              if Abort then
                l := 0;
              ClrIO;
            end
          else
            l := 0;
          {$IFNDEF OS2}
          Files^.AtInsert(0, NewFileRec('..', '..', l, 0, 0, 0,
            Directory, CurDir));
          {$ELSE}
          Files^.AtInsert(0, NewFileRec('..', l, 0, 0, 0, Directory,
            CurDir));
          {$ENDIF}
        end;
    GetFilesColl := Files;
    {JO: см. выше комментарий к строке, начинающейся с "if SortMode = -1" }
    {if (SortMode <> -1) and (SortMode <> psmUnsorted) and (Files^.Count>0) then Files^.Sort;}
    {/JO}
  end { GetFilesColl };
{-DataCompBoy-}

{-DataCompBoy-}
function TDrive.GetDirectory;
  var
    Files: PFilesCollection;
    SR: lSearchRec;
    P: PFileRec;
    i, j: integer;
    AllFiles, ShowD: boolean;
    FreeSpc: TSize;
    TFiles: word;
    Descriptions: PDIZCol;

  begin
    ClrIO;
    TFiles := 0;
    DOSError := 0;
    DizOwner := '';
    FreeSpace := '';
    Descriptions := nil;
    Abort := False;
    ShowD := (Flags and psShowDescript <> 0);

    if ShowD or (FMSetup.Options and fmoAlwaysCopyDesc <> 0) or
      (SortMode = psmDIZ) or (PanelFlags and fmiDescriptionInBottom <> 0
        )
    then
      PrepareDIZ(CurDir, DizOwner, Descriptions);
    NoMem := False;

    Files := GetFilesColl(TotalInfo, TFiles, PanelFlags, @CurDir,
      FileMask, Owner,
    SortMode {$IFNDEF OS2}, (Flags and psShowLongName <> 0) {$ENDIF});

    NoMemory := NoMem;

    if PanelFlags and fmiFree <> 0 then
      GetFreeSpace(FreeSpace);

    if Descriptions <> nil then
      begin
        TossDescriptions(@DizOwner, Files, Descriptions);
        Dispose(Descriptions, Done);
        if Files^.SortMode = psmDIZ then
          Files^.Sort; {JO}
      end;
    GetDirectory := Files;
  end { TDrive.GetDirectory };
{-DataCompBoy-}

function TDrive.isUp;
  begin
    {if Length(CurDir)>3 then}isUp := False { else isUp:=true;}
  end;

procedure TDrive.RereadDirectory;
  begin
    if Prev <> nil then
      Prev^.RereadDirectory(s);
  end;

{-DataCompBoy-}
procedure TDrive.GetDirInfo;
  begin
    ReadDiskInfo(CurDir, B, Flags);
  end;
{-DataCompBoy-}

procedure TDrive.KillUse;
  begin
    if Prev <> nil then
      Prev^.KillUse;
  end;

{-DataCompBoy-}
procedure TDrive.GetDown;
  var
    fr: TFileRec;
    s, s1, s2, SCreat, SLAcc: String;
  begin
    if P = nil then
      exit;
    Move(P^, fr, TFileRecFixedSize+Length(P^.FlName[True]));
    if (fr.Attr and Directory = 0) or (fr.Attr and $8000 <> 0) and (fr.
        Size >= 0)
      or (PFilePanel(Owner)^.PanelFlags and fmiDirLen <> 0)
      {or (FR.Size >= 0)}
    then
      s := FileSizeStr(fr.Size)
    else if fr.TType = ttUpDir
    then
      s := GetString(dlUpDir)
      {$IFDEF Win32}
    else if fr.Attr and FILE_ATTRIBUTE_REPARSE_POINT <> 0 then
      s := GetString(dlSymLink)
      {$ENDIF}
    else
      s := GetString(dlSubDir);
    with TDate4(fr.FDate) do
      MakeDate(DateMode, Day, Month, fr.Yr, Hour, Minute, s1);
    if fr.YrCreat <> 0 then
      with TDate4(fr.FDateCreat) do
        begin
          MakeDate(DateMode, Day, Month, fr.YrCreat, Hour, Minute,
            SCreat);
          SCreat := GetString(dlCre)+SCreat;
        end
      else
      SCreat := AddSpace('', 19-CountryInfo.TimeFmt);
    if fr.YrLAcc <> 0 then
      with TDate4(fr.FDateLAcc) do
        begin
          MakeDate(DateMode, Day, Month, fr.YrLAcc, Hour, Minute,
            SLAcc); {???}
          SLAcc := GetString(dlLac)+SLAcc;
        end
      else
      SLAcc := '';
    {$IFNDEF OS2}uLfn := Flags and psShowLongName <> 0;
    if not LFNinBottom then
      s2 := fr.FlName[uLfn xor InvLFN]
    else
      {$ENDIF}
      s2 := fr.FlName[True];
    if Length(s2) > 14 then
      begin
        SetLength(s2, 14);
        s2[14] := FMSetup.RestChar[1];
      end;
    MoveStr(B, {$IFDEF RecodeWhenDraw}CharToOemStr {$ENDIF}(s2), C);

    MoveStr(TWordArray(B)[22-Length(s)+CountryInfo.TimeFmt], s, C);
    MoveStr(TWordArray(B)[23+CountryInfo.TimeFmt], s1, C);
    MoveStr(TWordArray(B)[25+Length(s1)+CountryInfo.TimeFmt], SCreat, C)
      ;
    MoveStr(TWordArray(B)[27+Length(s1)+Length(SCreat)+CountryInfo.
      TimeFmt], SLAcc, C);
  end { TDrive.GetDown };
{-DataCompBoy-}

function TDrive.GetRealName;
  begin
    GetRealName := GetDir;
  end;

function TDrive.GetInternalName;
  begin
    GetInternalName := '';
  end;

{-DataCompBoy-}
function TDrive.GetRealDir;
  var
    s: String;
    C: Char;
    D: PDialog;
  var
    MM: record
      case byte of
        1: (l: longInt; s: String[1]);
        2: (C: Char);
    end;
  begin
    if DriveType = dtDisk then
      begin
        C := GetCurDrive;
        if C = CurDir[1] then
          begin
            ClrIO;
            NeedAbort := True;
            lGetDir(0, s);
            if Abort then
              s := CurrentDirectory;
            NeedAbort := True;
            advance2.lChDir(CurDir);
            repeat
              Abort := False;
              NeedAbort := True;
              lGetDir(0, CurDir);
              if Abort then
                begin
                  repeat
                    MM.l := 0;
                    MM.C := GetCurDrive;
                    MM.s := MM.C;
                    D := PDialog(LoadResource(dlgDiskError));
                    if D <> nil then
                      begin
                        D^.SetData(MM);
                        Application^.ExecView(D);
                        D^.GetData(MM);
                        Dispose(D, Done);
                      end;
                    UpShortStr(MM.s);
                    if ValidDrive(MM.s[1]) then
                      begin
                        SetCurDrive(MM.s[1]);
                        break;
                      end;
                  until False;
                  Abort := True;
                end;
            until not Abort;
            NeedAbort := False;
            lGetDir(0, CurDir);
            advance2.lChDir(s);
          end
        else
          begin
            advance2.lChDir(CurDir);
            if not Abort then
              repeat
                Abort := False;
                NeedAbort := True;
                lGetDir(0, CurDir);
                if Abort then
                  begin
                    repeat
                      MM.l := 0;
                      MM.C := GetCurDrive;
                      MM.s := MM.C;
                      D := PDialog(LoadResource(dlgDiskError));
                      if D <> nil then
                        begin
                          D^.SetData(MM);
                          Application^.ExecView(D);
                          D^.GetData(MM);
                          Dispose(D, Done);
                        end;
                      UpShortStr(MM.s);
                      if ValidDrive(MM.s[1]) then
                        begin
                          SetCurDrive(MM.s[1]);
                          break;
                        end;
                    until False;
                    Abort := True;
                  end;
              until not Abort;
            SetCurDrive(C);
          end;
        GetRealDir := CurDir;
      end
    else
      GetRealDir := GetDir;
    NeedAbort := False;
  end { TDrive.GetRealDir };
{-DataCompBoy-}

procedure TDrive.HandleCommand;
  begin
  end;

function TDrive.GetFullFlags;
  begin
    GetFullFlags := psShowSize+psShowDate+psShowTime+
    psShowCrDate+psShowCrTime+psShowLADate+psShowLATime;
  end;

procedure TDrive.EditDescription;
  begin
    if (DriveType = dtDisk) and (PF^.TType <> ttUpDir)
    then
      SetDescription(PF, DizOwner);
  end;

{-DataCompBoy-}
procedure TDrive.GetDirLength(PF: PFileRec);
  var
    s: String;
    i: TSize;
    j: longInt;
    NumDirs: integer;
  begin
    if (PF^.Attr and $8000 <> 0) or (PF^.Attr and Directory = 0)
    then
      exit;
    s := PF^.Owner^;
    if (PF^.TType <> ttUpDir) then
      s := MakeNormName(s, PF^.FlName[True]);
    i := 1;
    PF^.Size := CountDirLen(s, True, i, integer(j), NumDirs);
    if not Abort then
      PF^.Attr := PF^.Attr or $8000;
  end;
{-DataCompBoy-}

{-DataCompBoy-}
function TDrive.OpenDirectory(const Dir: String): PDrive;
  var
    i: longInt;
    PI: PView;
    PDrv: PDrive;
    Dirs: PStringCollection;
    Files: PFilesCollection;
    P: PString;
    tmr: TEventTimer;
    MemReq: longInt;
    MAvail: longInt;

  procedure AddDirectory(s: String);
    begin
      if MAvail <= MemReq then
        exit;
      if s[Length(s)] <> '\' then
        AddStr(s, '\');
      s[1] := LowCase(s[1]);
      Dirs^.Insert(NewStr(s));
      Inc(MemReq, SizeOf(ShortString));
    end;

  procedure ReadDir(dr: PString);
    var
      SR: lSearchRec;
      P: PFileRec;
      D: DateTime;
    begin
      ClrIO;
      lFindFirst(dr^+x_x, AnyFile, SR); {JO}
      while not Abort and (DOSError = 0) and (MAvail > MemReq) do
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
                SR.SR.CreationTime,
                SR.SR.LastAccessTime,
                SR.SR.Attr,
                dr));
                Inc(MemReq, SizeOf(TFileRec));
                Inc(MemReq, Length(dr^+SR.FullName)+2);
              end
            else if {$IFNDEF OS2}(SR.SR.Name[1] <> '.') and (SR.
                FullName <> '.') and (SR.FullName <> '..')
              {$ELSE}(SR.SR.Name <> '..') and (SR.SR.Name <> '.')
                {$ENDIF}
            then
              AddDirectory(dr^+SR.FullName);
          lFindNext(SR);
        end;
      lFindClose(SR);
      {$IFDEF OS2}
      if DOSError = 49 then
        MessageBox(GetString(dl_CodePage_FS_Error), nil, mfError+
          mfOKButton);
      {$ENDIF}
    end { ReadDir };

  begin { TDrive.OpenDirectory }
    NewTimer(tmr, 0);
    Dirs := New(PStringCollection, Init($10, $10, False));

    PI := WriteMsg(GetString(dlReadingList));
    New(Files, Init($10, $10));
    Files^.SortMode := psmOrdered;
    {$IFDEF Win32}
    Files^.LFNActive := True; {JO}
    {$ENDIF}
    {JO: сначала один pаз опpеделяем объём доступной памяти, а затем по ходу дела}
    {    подсчтитываем насколько тpебования памяти pастут и не пpевысили ли они  }
    {    доступный изначально объём                                              }
    MemReq := LowMemSize;
    MAvail := MaxAvail;
    AddDirectory(lFExpand(Dir));
    i := Dirs^.Count-1;
    Abort := False;
    while (i >= 0) and (not Abort) and (MAvail > MemReq) do
      begin
        i := Dirs^.Count-1;
        {if Files^.Count >= 2000 then Break;}
        while (i >= 0) and (not Abort) and (MAvail > MemReq) do
          begin
            UpdateWriteView(PI);
            P := Dirs^.At(i);
            if (P^[1] >= 'a') and (P^[1] <= 'z') then
              begin
                P^[1] := UpCase(P^[1]);
                Dirs^.AtDelete(i);
                Dirs^.Insert(P);
                ReadDir(P);
                if TimerExpired(tmr) then
                  begin
                    NewTimer(tmr, 1);
                    if ESC_Pressed then
                      Abort := True;
                  end;
                break;
              end;
            Dec(i);
          end;
      end;
    PI^.Free;
    // JO: здесь сортировка не нужна, т.к. она делается в TFindDrive.GetDirectory
    //     и в результате мы получаем сортировку дважды
    {Files^.Sort;}
    PDrv := New(PFindDrive, Init(GetString(dlBranch)+Dir, Dirs,
      Files, FreeByte));
    PDrv^.NoMemory := MAvail <= MemReq;
    OpenDirectory := PDrv;
  end { TDrive.OpenDirectory };
{-DataCompBoy-}

procedure RereadDirectory;
  var
    Event: TEvent;

  procedure Action(View: PView);
    begin
      Event.What := evCommand;
      Event.Command := cmRereadDir;
      Event.InfoPtr := @Dir;
      View^.HandleEvent(Event);
    end;

  begin
    {$IFDEF OS_DOS}
    Dir := lfGetLongFileName(Dir);
    {$ENDIF}
    Desktop^.ForEach(@Action);
  end;

end.
