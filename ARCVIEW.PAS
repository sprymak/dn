{/////////////////////////////////////////////////////////////////////////
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
{JO, AK155: 27.11.2002 - добавили раскрытие архивов в ветвь по Ctrl-H}
{JO:  1.12.2002 - добавил поиск файлов внутри архива}
{$I STDEFINE.INC}

unit ArcView;

interface

uses
  Lfn, Files, Tree, Dos, Objects, Views, Drivers,
  FViewer, FlPanel, FilesCol, DiskInfo, DblWnd, HistList, HideView,
  RStrings, Drives, Commands, Archiver, FStorage, Histries,
  advance, advance1, advance2, advance3, ArchDet, Collect
  , arc_RAR, arc_ACE
  ;

type
  PArcDrive = ^TArcDrive;
  TArcDrive = object(TDrive)
    {Cat: этот объект вынесен в плагинную модель; изменять крайне осторожно!}
    ArcName: String; {DataCompBoy}
    VArcName: String; {JO}
    AType: PARJArchive;
    Files: PDirStorage;
    KillAfterUse: boolean;
    FakeKillAfterUse: boolean; {временная пустышка}
    ArcDate: longInt;
    ArcSize: Comp; {сохраняются вместе}
    ForceRescan: boolean;
    Password: String;
    Constructor Init(const AName, VAName: String; ViewMode: byte);
    Constructor InitCol(PC: PDirStorage; const AName, VAName: String);
    Constructor Load(var s: TStream);
    procedure Store(var s: TStream); virtual;
    procedure RereadDirectory(s: String); virtual; {DataCompBoy}
    procedure KillUse; virtual;
    function ReadArchive: boolean;
    procedure lChDir(ADir: String); virtual; {DataCompBoy}
    function GetDir: String; virtual;
    function GetDirectory(SortMode, PanelFlags: integer; const
      FileMask: String;
    var FreeSpace: String; var TotalInfo: TSize): PCollection;
      virtual;
    function Exec(Prg, Cmd: String; Lst: AnsiString; B: boolean):
      boolean;
    {JO:  выделил список файлов в командной строке или путь к               }
    {     файлу-списку в отдельный параметр Lst;                            }
    {     параметр B должен быть False, если используем                     }
    {     файл-список или разархивируем одиночный файл не прибегая к списку }
    {     и True, если используем список в командной строке                 }

    procedure UseFile(P: PFileRec; Command: word); virtual;
      {DataCompBoy}
    function MakeListFile(PC: PCollection; UseUnp: boolean; var B:
      boolean): AnsiString;
    {JO:  параметр UseUnp указывает, будем использовать полученный      }
    {     список файлов для распаковщика (True), или паковщика (False); }
    {     переменная В возвращает, был ли создан фписок в               }
    {     командной строке (True) или в файле-списке (False)            }

    procedure CopyFiles(AFiles: PCollection; Own: PView; MoveMode:
      boolean); virtual;
    procedure CopyFilesInto(AFiles: PCollection; Own: PView;
      MoveMode: boolean); virtual;
    procedure EraseFiles(AFiles: PCollection); virtual;
    {procedure  GetDown(var B; C: Word; P: PFileRec); virtual;}
    procedure GetFull(var B; P: PFileRec; C, Sc: word); virtual;
      {DataCompBoy}
    procedure GetEmpty(var B; Sc: word); virtual;
    function CalcLengthWithoutName: integer; virtual;
    function CalcLength: integer; virtual;
    function GetRealName: String; virtual;
    function GetInternalName: String; virtual;
    procedure MakeTop(var s: String); virtual;
    procedure HandleCommand(Command: word; InfoPtr: Pointer);
      virtual;
    procedure MakeDir; virtual;
    function isUp: boolean; virtual;
    procedure ChangeUp(var s: String); virtual;
    procedure ChangeRoot; virtual;
    procedure ExtractFiles(AFiles: PCollection; ExtrDir: String; Own:
      PView; Options: byte); {DataCompBoy}
    procedure GetFreeSpace(var s: String); virtual;
    procedure GetDirInfo(var B: TDiskInfoRec); virtual;
    function GetFullFlags: word; virtual;
    procedure GetDirLength(PF: PFileRec); virtual; {DataCompBoy}
    procedure GetParam(n: byte); virtual; {DataCompBoy}
    destructor Done; virtual;
    procedure StdMsg4;
    function OpenDirectory(const Dir: String): PDrive; virtual;
    procedure FindFile;
    end;

function ArcViewer(AName, VAName: String; Num: byte): boolean;
  {DataCompBoy}
procedure StdMsg(MsgNo: byte);

const
  ArcPasw: String[32] = '';

type
  PFInfo = ^TFInfo;
  TFInfo = record
    FName: String;
    USize: longInt;
    PSize: longInt;
    Date: longInt;
    Attr: byte;
    Last: byte;
    { 0 - not last    }
    { 1 - archive end }
    { 2 - broken arc  }
    end;

implementation

uses
  VpSysLow,
  Menus, DNApp, Messages, Dialogs, Gauge, FileCopy, Memory, Startup,
  {$IFDEF ARVID}Arvid, {$ENDIF}xTime, VideoMan, DnExec, FileFind
  , UserMenu {JO: для скрывания панелей при разархивировании }
  , Arc_Zip {JO: для CentralDirRecPresent}
  ;

const
  LowMemSize = $4000; {Local setting}

function MaxAvail: longInt;
  begin
    MaxAvail := MemAdjust(System.MaxAvail);
  end;

procedure StdMsg(MsgNo: byte);
  begin
    Application^.Redraw;
    case MsgNo of
      1:
        ErrMsg(dlArcMsg1);
      4:
        MessageBox(GetString(dlArcMsg4)+''''+
          {$IFDEF RecodeWhenDraw}CharToOemStr {$ENDIF}(Cut(
          ArcFileName, 40))+'''', nil, mfOKButton or mfError);
      5:
        Msg(dlArcMsg5, nil, mfOKButton or mfInformation);
      6:
        if MsgHelpCtx <> 0 then
          MessageBox(GetString(dlArcMsg6)+GetString(dlPressF1),
          nil, mfOKButton or mfError)
        else
          MessageBox(GetString(dlArcMsg6), nil, mfOKButton or
            mfError);
      7:
        Msg(dlArcMsg7, nil, mfOKButton or mfInformation);
    end {case};
  end { StdMsg };

procedure TArcDrive.StdMsg4;
  begin
    if TempFile <> '' then
      TempFile := '';
    StdMsg(4);
    Abort := True;
  end;

{-DataCompBoy-}
procedure TArcDrive.GetParam;
  begin
    Param := n;
    with ColumnsDefaultsArch do
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
  end { TArcDrive.GetParam };
{-DataCompBoy-}

{-DataCompBoy-}
Constructor TArcDrive.Init;
  var
    SR: lSearchRec;
    i: integer;
    XT, Q: String;
  begin
    TObject.Init;
    if ViewMode < 1 then
      GetParam(1)
    else
      GetParam(ViewMode);
    innum := ViewMode;

    i := PosChar(':', Copy(VAName, 3, MaxStringLength))+2;
    if i > 2 then
      VArcName := lFExpand(Copy(VAName, 1, i-1))
    else
      VArcName := lFExpand(VAName);

    i := PosChar(':', Copy(AName, 3, MaxStringLength))+2;
    if i > 2 then
      begin
        Q := Copy(AName, i+1, MaxStringLength);
        if Q[Length(Q)] in ['\', '/'] then
          SetLength(Q, Length(Q)-1);
        ArcName := lFExpand(Copy(AName, 1, i-1));
      end
    else
      begin
        ArcName := lFExpand(AName);
        Q := '\';
      end;

    {lFSplit(ArcName, FreeStr, Nm, Xt);
 if Xt = '' then AddStr(ArcName, '.');}
    XT := GetExt(ArcName);
    if ((XT = '') or (XT = '.')) and (ArcName[Length(ArcName)] <>
        '.')
    then
      ArcName := ArcName+'.';
    lFindFirst(ArcName, AnyFile, SR); {JO}
    lFindClose(SR);
    if DOSError <> 0 then
      begin
        ArcFileName := ArcName;
        VArcFileName := VArcName;
        StdMsg4;
        lFindClose(SR);
        Fail
      end;
    DriveType := dtArc;
    if not ReadArchive or (Files = nil) then
      begin
        Done;
        Fail;
      end;
    KillAfterUse := TempFile <> '';
    TempFile := '';
    Password := '';
    {lFindClose(SR);}
    lChDir(Q);
    AddToDirectoryHistory(ArcName+':'+CurDir, integer(DriveType));
  end { TArcDrive.Init };
{-DataCompBoy-}

{-DataCompBoy-}
Constructor TArcDrive.InitCol;
  var
    SR: lSearchRec;
  begin
    TObject.Init;
    GetParam(1);
    ArcName := lFExpand(AName);
    {VArcName := lFExpand(VAName);}
    lFindFirst(ArcName, AnyFile, SR); {JO}
    ArcDate := SR.SR.Time;
    ArcSize := SR.SR.Size;
    lFindClose(SR);
    DriveType := dtArc;
    Files := PC;
    if (Files = nil) then
      Fail;
    KillAfterUse := TempFile <> '';
    TempFile := '';
    Password := '';
    if ExistFile(ArcName) then
      New(ArcFile, Init(ArcName, stOpenRead, ArcBufSize))
    else
      ArcFile := nil;
    ArcFileName := ArcName;
    VArcFileName := VArcName;
    if (ArcFile = nil) or (ArcFile^.Status <> stOK) then
      begin
        StdMsg(4);
        Dispose(ArcFile, Done);
        ArcFile := nil;
        if Files <> nil then
          begin
            Dispose(Files, Done);
            Files := nil
          end;
        Fail;
      end;
    ArcPos := 0;
    SkipSFX;
    AType := DetectArchive;
    FreeObject(ArcFile);
  end { TArcDrive.InitCol };
{-DataCompBoy-}

{-DataCompBoy-}
Constructor TArcDrive.Load;
  var
    SR: lSearchRec;
  label
    Failure;
  begin
    inherited Load(s);
    s.ReadStrV(ArcName);
      {S.Read(ArcName[0],1); S.Read(ArcName[1],Length(ArcName));}
    {Cat}
    s.ReadStrV(VArcName);
      {S.Read(VArcName[0],1); S.Read(VArcName[1],Length(VArcName));}
    {/Cat}
    s.Read(FakeKillAfterUse, 1);
    {временно}
    KillAfterUse := False;
    s.ReadStrV(Password);
      {S.Read(Password[0],1); S.Read(Password[1],Length(Password));}
    s.Read(ArcDate, SizeOf(ArcDate)+SizeOf(ArcSize));
    ForceRescan := False;
    DriveType := dtArc;
    ArcFileName := ArcName;
    VArcFileName := VArcName;
    lFindFirst(ArcName, AnyFile, SR); {JO}
    lFindClose(SR);
    if DOSError <> 0 then
      goto Failure;
    if (ArcDate <> SR.SR.Time) or (ArcSize <> SR.SR.Size) then
      begin{архив изменился, надо его перечитывать заново}
        Files := PDirStorage(s.Get);
          {AK155 28-11-2002 Тут лучше бы не читать,
     а просто пропустить кусок потока, но я не умею это делать}
        CurDir := '\';
        ReadArchive;
      end
    else
      begin
        Files := PDirStorage(s.Get);
        New(ArcFile, Init(ArcName, stOpenRead, ArcBufSize));
        if (ArcFile = nil) or (ArcFile^.Status <> stOK) then
          begin
            Dispose(ArcFile, Done);
            ArcFile := nil;
            goto Failure;
          end;
        ArcPos := 0;
        SkipSFX;
        AType := DetectArchive;
        FreeObject(ArcFile);
        if AType = nil then
          begin
Failure:
            StdMsg(4);
            if Files <> nil then
              Dispose(Files, Done);
            Files := nil;
            s.Read(ForceRescan, 1);
            Fail;
          end;
      end;
    s.Read(ForceRescan, 1);
  end { TArcDrive.Load };
{-DataCompBoy-}

procedure TArcDrive.KillUse;
  begin
    if Prev <> nil then
      Prev^.KillUse;
    if KillAfterUse then
      EraseTempFile(ArcName);
  end;

procedure TArcDrive.Store;
  begin
    inherited Store(s);
    s.WriteStr(@ArcName); {S.Write(ArcName[0],1 + Length(ArcName));}
    s.WriteStr(@VArcName);
      {S.Write(VArcName[0],1 + Length(VArcName));} {Cat}
    s.Write(KillAfterUse, 1);
    s.WriteStr(@Password);
      {S.Write(Password[0],1 + Length(Password));}
    s.Write(ArcDate, SizeOf(ArcDate)+SizeOf(ArcSize));
    s.Put(Files);
    s.Write(ForceRescan, 1);
  end;

destructor TArcDrive.Done;
  begin
    if Files <> nil then
      Dispose(Files, Done);
    Files := nil;
    if AType <> nil then
      Dispose(AType, Done);
    AType := nil;
    inherited Done;
  end;

{-DataCompBoy-}
function TArcDrive.ReadArchive;
  var
    PF: PArcFile;
    P: PWhileView;
    R: TRect;
    ln: longInt;
    Cancel: boolean;
    t: TEventTimer;
    olf: String;
    SR: lSearchRec;
  begin
    {AK155 26-11-2002 Перечитываем архив тогда и только тогда, когда
 у него изменилась дата/время или длина }
    lFindFirst(ArcName, AnyFile, SR);
    lFindClose(SR);
    if (ArcDate = SR.SR.Time) and (ArcSize = SR.SR.Size) then
      begin
        ReadArchive := True;
        exit;
      end;
    ArcDate := SR.SR.Time;
    ArcSize := SR.SR.Size;
    {/AK155}
    CtrlBreakHit := False;
    ReadArchive := False;
    New(ArcFile, Init(ArcName, stOpenRead, ArcBufSize));
    ArcFileName := ArcName;
    VArcFileName := VArcName;
    if (ArcFile = nil) or (ArcFile^.Status <> stOK) then
      begin
        Dispose(ArcFile, Done);
        ArcFile := nil;
        StdMsg4;
        exit;
      end;
    if Files <> nil then
      Dispose(Files, Done);
    Files := nil;
    Files := nil;
    ArcPos := 0;
    SkipSFX;
    if AType <> nil then
      Dispose(AType, Done);
    AType := nil; {DataCompBoy}
    AType := DetectArchive;
    if AType = nil then
      begin
        FreeObject(ArcFile);
        exit;
      end;
    New(Files, Init);
    if Files = nil then
      exit;
    P := nil;
    R.Assign(1, 1, 30, 10);
    {P := WriteMsg(GetString(dlArcReadArc));}
    ln := ArcFile^.GetSize+1;
    Cancel := False;
    PReader := nil;
    Inc(SkyEnabled);
    NewTimer(t, 6);
    if (StartupData.Slice2 and osuReleaseArch = 0) then
      Application^.BFSpeed;
    repeat
      if TimerExpired(t) then
        begin
          if P = nil then
            begin
              New(P, Init(R));
              PReader := P;
              P^.Top := GetString(dlArcReadArc);
              P^.Write(1, GetString(dlPercentComplete));
              Desktop^.Insert(P);
            end;
          P^.Write(2, Copy(Strg(#219, 25 div (ln div (ArcFile^.
            GetPos+1)))+Strg(#177, 25), 1, 25));
          P^.Write(3, ItoS(Files^.Files)+GetString(dlFilesFound));
          NewTimer(t, 6);
        end;
      AType^.GetFile;
      if FileInfo.Last = 0 then
        begin
          Replace('/', '\', FileInfo.FName);
          {piwamoto: it willn't work due to bugs in FName processing in flpanelx
      if (FileInfo.FName[1] = '.') and (FileInfo.FName[2] = '\') then
        begin
(*         System.Delete(FileInfo.FName,1,2); *)
         Files^.AddFile('\.\..', '\.\..', 0, 0, FileInfo.Date, 0);
        end;
}
          if FileInfo.FName[1] <> '\' then
            FileInfo.FName := '\'+FileInfo.FName;
          if FileInfo.Attr and Directory <> 0 then
            FileInfo.FName := FileInfo.FName+'\';
          olf := FileInfo.FName;
          {$IFNDEF OS2}
          Replace('\.', '\', FileInfo.FName);
          UpStr(FileInfo.FName);
          {$ENDIF}

          {attribute "Hidden" means "with password"}
          Files^.AddFile(olf, FileInfo.USize,
          FileInfo.PSize, FileInfo.Date, FileInfo.Attr);
          if (FileInfo.Attr and Directory <> 0) or (olf[Length(olf)] =
              '\')
          then
            Files^.AddFile(MakeNormName(olf, '..'),
            FileInfo.USize, FileInfo.PSize, FileInfo.Date, 0);

          if (P <> nil) and TimerExpired(t) then
            begin
              DispatchEvents(P, Cancel);
              if Cancel then
                begin
                  StdMsg(5);
                  FileInfo.Last := 1;
                end;
            end;
        end;
    until (FileInfo.Last > 0) or LowMemory or CtrlBreakHit;
    if CtrlBreakHit then
      StdMsg(5);
    CtrlBreakHit := False;
    if (StartupData.Slice2 and osuReleaseArch = 0) then
      Application^.EFSpeed;
    Dec(SkyEnabled);
    if P <> nil then
      P^.Free;
    FreeObject(ArcFile);
    CDir := '';
    if (FileInfo.Last = 2) or
      ((AType^.GetID = arcZIP) and not CentralDirRecPresent)
    then
      StdMsg(6);
    ReadArchive := True;
    if Files^.Files = 0 then
      begin
        StdMsg(7);
        ReadArchive := False;
      end;
  end { TArcDrive.ReadArchive };
{-DataCompBoy-}

{-DataCompBoy-}
procedure TArcDrive.lChDir;
  var
    dr: String;
    Nm: String;
    XT: String;
  begin
    if ADir = #0 then
      exit;
    while (PosChar(CurDir[Length(CurDir)], '.\') > 0) do
      SetLength(CurDir, Length(CurDir)-1);
    if IsDummyDir(ADir) then
      begin
        if CurDir <> '' then
          while (CurDir <> '') and (not (CurDir[Length(CurDir)] in [
              '\', '/']))
          do
            SetLength(CurDir, Length(CurDir)-1)
          else
          advance2.lChDir(GetPath(ArcName));
        exit;
      end;
    lFSplit(ADir, dr, Nm, XT);
    if XT = '..' then
      begin
        CurDir := dr;
        while (CurDir <> '') and not (CurDir[Length(CurDir)] in ['\',
            '/'])
        do
          SetLength(CurDir, Length(CurDir)-1);
      end
    else
      CurDir := ADir;
    while (PosChar(CurDir[Length(CurDir)], '.\') > 0) do
      SetLength(CurDir, Length(CurDir)-1);
    if CurDir = '' then
      CurDir := '\';
    if CurDir[1] <> '\' then
      CurDir := '\'+CurDir;
    AddToDirectoryHistory(ArcName+':'+CurDir, integer(DriveType));
  end { TArcDrive.lChDir };
{-DataCompBoy-}

{-DataCompBoy-}
function TArcDrive.GetDir;
  var
    dr: String;
    Nm: String;
    XT: String;
  begin
    while (PosChar(CurDir[Length(CurDir)], '.\') > 0) do
      SetLength(CurDir, Length(CurDir)-1);
    if (Length(CurDir) > 0) and (not (CurDir[1] in ['\', '/'])) then
        CurDir := '\'+CurDir;
    if (Prev <> nil) and (Prev^.DriveType = dtDisk) then
      lFSplit(VArcName, dr, Nm, XT) {JO}
    else
      lFSplit(ArcName, dr, Nm, XT);
    GetDir := AType^.GetSign+Nm+XT+CurDir;
  end;
{-DataCompBoy-}

{-DataCompBoy-}
function TArcDrive.GetDirectory;
  var
    F: PFileRec;
    i, si: longInt;
    AllFiles: boolean;
    AFiles, FD: PFilesCollection;
    TTL, TPL: TSize;
    _USize, _PSize: TSize;
    fr: TFileRec;
    OW: Pointer;
    dr: String;
    MemReq: longInt;
    MAvail: longInt;
  begin
    ReadArchive; {AK155 26-11-2002}
    {While (PosChar(CurDir[Length(CurDir)], '.\') > 0) do SetLength(CurDir, Length(CurDir)-1);}
    AFiles := New(PFilesCollection, Init($10, $10));
    {FD := New(PFilesCollection, Init($40, $10));}
    PFilesCollection(AFiles)^.Owner := Owner;
    PFilesCollection(AFiles)^.SortMode := SortMode;
    {$IFDEF Win32}
    PFilesCollection(AFiles)^.LFNActive := True; {JO}
    {$ENDIF}
    GetDirectory := AFiles;
    while (PosChar(CurDir[Length(CurDir)], '.\') > 0) do
      SetLength(CurDir, Length(CurDir)-1);
    FD := New(PFilesCollection, Init($40, $10));
    FreeSpace := '';
    TTL := 0;
    TPL := 0;
    {GetDirectory := AFiles;}AllFiles := (FileMask = x_x) or (
      FileMask = '*');
    FD^.SortMode := 141;
    FD^.Duplicates := False;
    {$IFDEF Win32}
    FD^.LFNActive := True; {JO}
    {$ENDIF}
    AFiles^.Duplicates := False;
    Files^.ResetPointer('');
    {JO: сначала один pаз опpеделяем объём доступной памяти, а затем по ходу дела}
    {    подсчтитываем насколько тpебования памяти pастут и не пpевысили ли они  }
    {    доступный изначально объём                                              }
    MemReq := LowMemSize;
    MAvail := MaxAvail;
    while not Files^.Last and Files^.GetNextFile and (MAvail >
        MemReq)
    do
      begin
        _USize := Files^.CurFile.Size;
        _PSize := Files^.CurFile.CSize;
        if (UpStrg(CurDir+'\') = UpStrg(Files^.LastDir)) and
          (AllFiles or InFilter(Files^.CurFile.Name, FileMask))
        then
          begin
            if Files^.CurFile.Name = '' then
              continue;
            if Files^.CurFile.Name = '..' then
              continue;
            with Files^.CurFile do
              begin
                F := NewFileRec(Name, {$IFNDEF OS2}GetURZ(Name),
                  {$ENDIF}
                _USize, Date, 0, 0, Attr, @CurDir);
                Inc(MemReq, SizeOf(TFileRec));
                Inc(MemReq, Length(CurDir+Name)+2);
              end;
            F^.PSize := _PSize;
            TTL := TTL+_USize;
            TPL := TPL+_PSize;
          end
        else if (UpStrg(CurDir+'\') = UpStrg(Copy(Files^.LastDir, 1,
            Length(CurDir)+1)))
        then
          begin
            dr := Copy(Files^.LastDir, Length(CurDir)+2,
              MaxStringLength);
            i := PosChar('\', dr);
            if i = 0 then
              i := Length(dr)+1;
            SetLength(dr, i-1);
            if dr = '' then
              continue;
            FillChar(fr, SizeOf(fr), 0);
            CopyShortString(dr, fr.FlName[True]);
            fr.Attr := Directory;
            i := FD^.IndexOf(@FR);
            if i >= 0 then
              with PFileRec(FD^.At(i))^ do
                begin
                  Size := Size+_USize;
                  PSize := PSize+_PSize;
                  TTL := TTL+_USize;
                  TPL := TPL+_PSize;
                  continue;
                end;
            F := NewFileRec(dr, {$IFNDEF OS2}GetURZ(dr), {$ENDIF}
              _USize, ArcDate, 0, 0, $80 or Directory, @CurDir);
            Inc(MemReq, SizeOf(TFileRec));
            Inc(MemReq, Length(CurDir+dr)+2);
            F^.PSize := _PSize;
            TTL := TTL+_USize;
            TPL := TPL+_PSize;
            if FD^.Search(F, si) then
              begin
                DelFileRec(F);
                continue;
              end
              {else FD^.Insert(F);}
            else
              FD^.AtInsert(si, F);
          end
        else
          continue;
        if AFiles^.Search(F, si) then
          DelFileRec(F)
          {else AFiles^.Insert(F);}
        else
          AFiles^.AtInsert(si, F);
      end;

    NoMemory := MAvail <= MemReq;
    FreeSpace := '';
    TotalInfo := TTL;

    if CurDir = '' then
      OW := @ArcName
    else
      OW := @CurDir;
    (* if TTL > MaxLongInt then F := NewFileRec('..', '..',0, ArcDate, Directory, OW) else
 begin
   F := NewFileRec({$IFNDEF OS2}'..',{$ENDIF} '..',Round(TTL), ArcDate, Directory, OW);
   F^.Attr := $8000 or F^.Attr;
 end;
 if TPL < MaxLongInt then F^.PSize := Round(TPL); *)

    F := NewFileRec('..', {$IFNDEF OS2}'..', {$ENDIF} {Round}(TTL),
      ArcDate, 0, 0, Directory, OW);
    F^.Attr := $8000 or F^.Attr;
    F^.PSize := {Round}(TPL);
    AFiles^.AtInsert(0, F);
    FD^.DeleteAll;
    Dispose(FD, Done);
  end { TArcDrive.GetDirectory };
{-DataCompBoy-}

{-DataCompBoy-}
procedure TArcDrive.UseFile;
  var
    SS, s, s2, Q: String;
    C: Char;
    Unp: String;
    ATime: longInt;
    ASize: TSize;
    AAttr: word;
    {$IFNDEF OS2}
  label TryAgain;
  {$ENDIF}
  begin
    TempFile := ''; {-$VOL}
    if (Command = cmEditFile) or (Command = cmFileEdit) or
      (Command = cmIntEditFile) or (Command = cmIntFileEdit)
    then
      exit;
    case Command of
      cmDBFView:
        C := '=';
      cmWKZView:
        C := '>';
      cmTextView, cmViewText:
        C := '<';
      cmHexView:
        C := '|';
      cmIntFileView:
        C := '-';
      else
        C := '+';
    end {case};
    {if not CheckPassword(AF) then Exit;}
    s := ' ';
    if P^.Attr and Hidden <> 0 then
      begin
        s := '';
        {$IFNDEF OS2}
TryAgain:
        {$ENDIF}
        if ExecResource(dlgSetPassword, s) <> cmOK then
          exit;
        { Flash >>> }
        if CheckForSpaces(s) then
          s := ' '+CnvString(AType^.Garble)+s+' '
        else
          {$IFNDEF OS2} if AType^.UseLFN then
          {$ENDIF}
          s := ' '+CnvString(AType^.Garble)+'"'+s+'"'+' '
          {$IFNDEF OS2}
        else
          begin
            MessageBox(GetString(dlSpacesInPassword), nil, mfWarning+
              mfOKButton);
            goto TryAgain;
          end
          {$ENDIF}
          ;
        { Flash <<< }
      end;
    SS := MakeNormName(P^.Owner^, P^.FlName[True]);
    if SS[1] = '\' then
      Delete(SS, 1, 1); {DelFC(SS);}
    {$IFNDEF OS2}
    if AType^.UseLFN then
      s2 := ArcName
    else
      s2 := lfGetShortFileName(ArcName);
    if ArcName[Length(ArcName)] = '.' then
      s2 := s2+'.';
    s := CnvString(AType^.Extract)+' '+s+
    CnvString(AType^.ForceMode)+' '+
    SquashesName(s2)+' '+SquashesName(SS)+' ';
    {$ELSE}
    s := CnvString(AType^.Extract)+' '+s+
    CnvString(AType^.ForceMode)+' '+
    SquashesName(ArcName)+' '+SquashesName(SS)+' ';
    {$ENDIF}
    {   DelDoubles('  ',S);}
      {piwamoto: files can have 2 spaces in names}
    TempFile := C+MakeNormName(TempDir, P^.FlName[True]);
    Q := '|'+GetRealName+':'+MakeNormName(CurDir, P^.FlName[True]);

    s2 := Copy(TempFile, 2, MaxStringLength);

    if C in ['<', '-', '|', '+'] then
      TempFile := TempFile+Q;

    GetFTimeSizeAttr(s2, ATime, ASize, AAttr);
    if not ExistFile(s2) or (PackedDate(P) <> ATime) or (P^.Size <>
        ASize)
    then

      begin
        Unp := CnvString(AType^.UnPacker);
        if (AType^.GetID = arcRAR) and (PosChar(';', Unp) > 0) then
          begin
            if PRARArchive(AType)^.VersionToExtr > 20 then
              Unp := Copy(Unp, PosChar(';', Unp)+1, 255)
            else
              Unp := Copy(Unp, 1, PosChar(';', Unp)-1);
          end;
        { Flash 21-01-2004
          Директорию нужно запоминать на том диске, где находится
          временный каталог. А на том, где лежит архив
          с просматриваемым файлом, она запомнится в любом случае. }
        advance2.lChDir(Copy(TempDir,1,2));
        lGetDir(0, DirToChange);
        advance2.lChDir(TempDir);
        Exec(Unp, {$IFDEF RecodeWhenDraw}OemToCharStr {$ENDIF}(s), '',
          False);
        advance2.lChDir(DirToChange);
        DirToChange := '';
      end;
    TempFileSWP := {$IFDEF RecodeWhenDraw}OemToCharStr {$ENDIF}(
      TempFile); {JO}
    TempFile := ''; {-$VOL}
    Message(Application, evCommand, cmRetrieveSwp, nil); {JO}
  end { TArcDrive.UseFile };
{-DataCompBoy-}

function TArcDrive.Exec;
  var
    s: String;
    SS1: AnsiString;
    SM: word;
    DE: word;
    CmdLineLim, ListLineLim: AWord;
    CmdLineOK: boolean;
    i, j: longInt;

  procedure StdMsg8;
    var
      l: array[0..1] of longInt;
      ST: String;
    begin
      Application^.Redraw;
      ST := s;
      Pointer(l[0]) := @ST;
      l[1] := DE;
      Msg(dlArcMsg8, @L, mfOKButton or mfError);
    end;

  {AK155 20/12/2001 Если под Win32 пытаться в отладчике прошагать
эту функцию, то получается полная блокировка клавиатуры и мыши
сразу на входе (даже с begin сойти не получается).
Этот эффект исчезает, если параметр AnsiString заменить на String.
Под OS/2 все шагается без проблем. Интересно, чей это глюк -
виндового отладчика или виндовой RTL? Хорошо, если первое. }
  begin { TArcDrive.Exec }
    Exec := True;
    s := Prg+' '+Cmd;
    {$IFDEF OS_DOS}
    if AType^.Swap then
      begin
        Exec := Message(Application, evCommand, cmExecString, @S) =
          nil;
        exit;
      end;
    {$ENDIF}
    DoneSysError;
    DoneEvents;
    DoneVideo;
    DoneDOSMem;
    DoneMemory;
    {$IFDEF OS_DOS} asm cld; mov ax,3; int $10; end; {$ENDIF}
    {$IFDEF OS_DOS} //AK155
    {AK155, дописано позже, чем комментарий к OS/2.
Под Win32 тоже не следует заниматься самодеятельностью.
Во-первых, мы отдаем консоль в каком-то не таком состоянии,
так что консольный rar не может вводить с клавиатуры.
Во-вторых, стОило ли работать с ansistring, чтобы потом вызвать
Dos.Exec?}
    s := GetEnv('PATH');
    s := FSearch(Prg, s);
    SwapVectors;
    if InFilter(s, '*.EXE;*.COM')
    then
      Dos.Exec(s, Cmd)
    else
      Dos.Exec(GetEnv('COMSPEC'), '/c '+s+' '+Cmd);
    {$ELSE}
    {AK155 Под OS/2, во-первых, PATH обычно не умещается
    в 255 символов, во-вторых, нет проблем с памятью,
    в третьих архиватор может оказаться ДОСовым.
    Так что пускай PATH просматривает cmd.exe, а мы не
    будем заниматься самодеятельностью }
    if B then
      begin
        {JO: разбираем ту часть командной строки, которая содержит список файлов    }
        {    на куски удобоваримой для командного процессора длины                  }
        if AType^.ShortCmdLine then
          CmdLineLim := {$IFDEF OS2}95 {$ELSE}127 {$ENDIF}
        else
          CmdLineLim := {$IFDEF OS2}1000 {$ELSE}250 {$ENDIF};
        ListLineLim := CmdLineLim-Length(Prg+Cmd)-7;
        CmdLineOK := False;
        SS1 := Lst; {для перестраховки}
        repeat
          if Length(Lst) >= ListLineLim then
            begin
              for i := ListLineLim downto 1 do
                if Lst[i] = #$14 then
                  begin
                    SS1 := Copy(Lst, 1, i-1);
                    Delete(Lst, 1, i);
                    break;
                  end;
            end
          else
            begin
              SS1 := Lst;
              CmdLineOK := True;
            end;
          for j := 1 to Length(SS1) do
            if SS1[j] = #$14 then
              SS1[j] := #$20;
            {JO: заменяем временный символ на пробелы}
          // DelDoubles('  ', S);{files can have 2 spaces in names}
          // AnsiDelDoubles('  ', SS1);
          {JO: AnsiExec - процедура из модуля DNExec , которая }
          {    используется вместо DOS.Exec и в качестве       }
          {    коммандлайна использует строку типа Ansistring  }
          SwapVectors;
          AnsiExec(GetEnv('COMSPEC'), '/c '+s+' '+SS1+' ');
          DE := DOSError;
          ClrIO;
          SwapVectors;
        until CmdLineOK;
      end
    else
      begin
        // DelDoubles('  ', S);{files can have 2 spaces in names}
        // AnsiDelDoubles('  ', Lst);
        SwapVectors;
        AnsiExec(GetEnv('COMSPEC'), '/c '+s+' '+Lst+' ');
        DE := DOSError;
        ClrIO;
        SwapVectors;
        EraseFile(SwpDir+'$DN'+ItoS(DNNumber)+'$.LST');
          {DataCompBoy}
      end;
    {$ENDIF}
    InitDOSMem;
    InitMemory;
    InitVideo;
    InitEvents;
    InitSysError;
    case DE of
      0:
        Application^.Redraw;
      8:
        StdMsg(1);
      else
        StdMsg8;
    end {case};
    // JO: закомментарил, т.к. теперь после разархивирования идёт копирование
    //     через временный каталог и после его удаления панель перечитывается
    { GlobalMessage(evCommand, cmPanelReread, nil);
  GlobalMessage(evCommand, cmRereadInfo, nil);}
  end { TArcDrive.Exec };

function TArcDrive.isUp;
  begin
    isUp := {CurDir = ''}True;
  end;

procedure TArcDrive.ChangeUp;
  begin
    if CurDir <> '' then
      begin
        s := GetName(CurDir);
        lChDir('..');
        exit
      end;
    if Owner = nil then
      exit;
    if Prev = nil then
      begin
        New(Prev, Init(0, Owner, innum));
        if Prev = nil then
          exit;
        {Prev^.Owner := Owner;}
      end;
    PFilePanel(Owner)^.Drive := Prev;
    if TypeOf(Prev^) = TypeOf(TDrive) then
      Prev^.lChDir(GetPath(VArcName));
      {piwamoto: VArcName is a feature, not a bug :-)}
    if (Prev^.DriveType = dtDisk) and
      (PView(Owner)^.GetState(sfSelected+sfActive))
    then
      ActivePanel := Owner;
    GlobalMessage(evCommand, cmRereadInfo, nil);
    if (Prev^.DriveType = dtDisk) then
      s := GetName(VArcName)
    else
      s := GetName(ArcName);
    Prev := nil;
    if KillAfterUse then
      EraseTempFile(ArcName);
    Dispose(PDrive(@Self), Done);
  end { TArcDrive.ChangeUp };

procedure TArcDrive.ChangeRoot;
  begin
    CurDir := '';
  end;

{-DataCompBoy-}
function TArcDrive.MakeListFile;
  var
    F: lText;
    PF: PFileRec;
    PA: PArcFile;
    i: integer;
    s: AnsiString;
    s1: String;

  function GetArcOwn(s: String): String;
    begin
      if Length(s) < 2 then
        begin
          Result := '';
          exit;
        end;
      s[2] := ';';
        {JO: реально не важно на что меняем, лишь бы не ':'}
      Result := Copy(s, PosChar(':', s)+1, MaxStringLength);
    end;

  procedure PutDir(SS: String);
    var
      i: integer;
      s1: String;
    begin
      if not (SS[Length(SS)] in ['\', '/']) then
        AddStr(SS, '\');
      Files^.ResetPointer('');
      while not Files^.Last and Files^.GetNextFile do
        if (SS = Copy(Files^.LastDir, 1, Length(SS)))
          and (Files^.CurFile.Name <> '')
        then
          begin
            s1 := Files^.LastDir+Files^.CurFile.Name;
            if s1[1] in ['\', '/'] then
              Delete(s1, 1, 1); {DelFC(S1);}
            if (Copy(s1, Length(s1)-2, 3) = '\..') or (Copy(s1,
                Length(s1)-2, 3) = '/..')
            then
              SetLength(s1, Length(s1)- {2}3); {JO}
            {JO: нужен ли слэш в конце каталогов - вопрос спорный,          }
            {    но похоже его отсутствие нигде не мешает, а наличие        }
            {    вводит RAR в заблуждение, если не использовать файл-список;}
            {    в дальейшем не исключено, что для каких-то архиваторов     }
            {    потребуется сделать соотв. опцию;                          }
            {    zip работает нормально и с тем, и с другим                 }

            {JO:  используем символ #$14 для временного разделения имён файлов}
            if B then
              s := s+#$14+SquashesName(s1)
            else
              Writeln(F.t, s1);
          end;
    end { PutDir };

  begin { TArcDrive.MakeListFile }
    if UseUnp then
      B := (CnvString(AType^.ExtrListChar) = ' ') or (CnvString(
        AType^.ExtrListChar) = '')
    else
      B := (CnvString(AType^.ComprListChar) = ' ') or (CnvString(
        AType^.ComprListChar) = '');
    if B then
      s := ''
    else
      begin
        s := SwpDir+'$DN'+ItoS(DNNumber)+'$.LST';
        lAssignText(F, s);
        ClrIO;
        lRewriteText(F);
        B := IOResult <> 0;
        if B then
          s := ''
        else
          begin
            if UseUnp then
              s := CnvString(AType^.ExtrListChar)+s
            else
              s := CnvString(AType^.ComprListChar)+s;
          end;
      end;
    for i := 0 to PC^.Count-1 do
      begin
        PF := PC^.At(i);
        {JO: проверка для разархивирования из панели поиска в архивах}
        if PathFoundInArc(PF^.Owner^) then
          s1 := MakeNormName(GetArcOwn(PF^.Owner^), PF^.FlName[True])
        else
          s1 := MakeNormName(PF^.Owner^, PF^.FlName[True]);
        if s1[1] in ['\', '/'] then
          Delete(s1, 1, 1); {DelFC(S1);}

        {JO:  используем символ #$14 для временного разделения имён файлов}
        if PF^.Attr and Directory = 0
        then
          if B then
            s := s+#$14+SquashesName(s1)
          else
            Writeln(F.t, s1)
        else
          PutDir('\'+s1+'\');
        MakeListFile := s;
      end;
    MakeListFile := s;
    if not B then
      Close(F.t);
  end { TArcDrive.MakeListFile };
{-DataCompBoy-}

{-DataCompBoy-}
procedure TArcDrive.ExtractFiles(AFiles: PCollection; ExtrDir:
    String;
  Own: PView; Options: byte);
  var
    SS, ArchiveName: AnsiString;
    s, SCr: String;
    DT: record
      s: String;
      W: word;
      Psw: String[30];
      end;
    ExtrChar: String;
    Nm: String;
    XT: String;
    Pswd: boolean;
    B: boolean;
    SCurDir: String;
    Unp: String;
    DNN: byte;
    TempExtrDir: String;
    TempDirUsed: boolean;
    FCT: PFilesCollection;
    FRT: PFileRec;
    OldConfirms: word;
    PV: PView;
    Inhr: byte;
    SR: lSearchRec;

    {$IFNDEF OS2}
  label TryAgain;
  {$ENDIF}

  procedure UnSelect(P: PFileRec);
    begin
      Message(Owner, evCommand, cmCopyUnselect, P);
      Pswd := Pswd or (P^.Attr and Hidden <> 0);
    end;

  begin { TArcDrive.ExtractFiles }
    while ExtrDir[Length(ExtrDir)] = ' ' do
      SetLength(ExtrDir, Length(ExtrDir)-1);
    if (ExtrDir = '') or (ExtrDir = '..') then
      {$IFNDEF OS2}
      if AType^.UseLFN then
        {$ENDIF}
        lFSplit(VArcName, ExtrDir, Nm, XT)
          {JO: для распаковки по F4 архивов, просмотренных через фильтр}
        {$IFNDEF OS2}
      else
        lFSplit(lfGetShortFileName(ArcName), ExtrDir, Nm, XT)
        {$ENDIF}
        ;

    if not (ExtrDir[Length(ExtrDir)] in ['\', '/']) then
      ExtrDir := ExtrDir+'\';

    SCurDir := CurDir;
    SCr := '';
    if (SCurDir <> '') then
      begin
        while (SCurDir[Length(SCurDir)] = '.') do
          SetLength(SCurDir, Length(SCurDir)-1);
        while (SCurDir <> '') and (SCurDir[1] = '\') do
          Delete(SCurDir, 1, 1);
        if (SCurDir[Length(SCurDir)] <> '\') then
          SCurDir := SCurDir+'\';
        if (CnvString(AType^.SetPathInside) <> '') then
          begin
            SCr := ' '+CnvString(AType^.SetPathInside)+SCurDir+' ';
            SCurDir := '';
          end;
      end;

    {JO}
    // проверяем, содержит ли каталог назначения файлы
    DOSError := 0;
    lFindFirst(MakeNormName(ExtrDir, x_x), AnyFile, SR); {JO}
    if IsDummyDir(SR.FullName) then
      lFindNext(SR);
    if IsDummyDir(SR.FullName) then
      lFindNext(SR);
    lFindClose(SR);
    // для разархивирования на дискеты и тестирования не используем
    // временный подкаталог
    if ((Options and 8) = 0) or ((Options and 2) <> 0) or
      ((DOSError <> 0) and (SCurDir = ''))
    then
      begin
        TempExtrDir := ExtrDir;
        TempDirUsed := False;
      end
    else
      begin
        { даём имя временному подкаталогу в каталоге назначения}
        DNN := DNNumber;
        while True do
          begin
            TempExtrDir := ExtrDir+'$DN'+ItoS(DNN)+'$.EDR';
            ClrIO;
            if PathExist(TempExtrDir) then
              Inc(DNN)
            else
              break;
          end;
        ClrIO;
        TempExtrDir := TempExtrDir+'\';
        TempDirUsed := True;
      end;
    {/JO}

    SS := MakeListFile(AFiles, True, B);
    s := ' ';
    Pswd := False;
    AFiles^.ForEach(@Unselect);
    ExtrChar := CnvString(AType^.ExtractWP);
    if Options and 1 = 0 then
      ExtrChar := CnvString(AType^.Extract);
    if Options and 2 <> 0 then
      ExtrChar := CnvString(AType^.Test);
    if Pswd then
      begin
        if Password = '' then
          {$IFNDEF OS2}
TryAgain:
          {$ENDIF}
          if ExecResource(dlgSetPassword, Password) <> cmOK then
            exit;
        { Flash >>> }
          {JO: взял код Flash из Arcview.TArcDrive.UseFile }
        if CheckForSpaces(Password) then
          s := ' '+CnvString(AType^.Garble)+Password+' '
        else
          {$IFNDEF OS2} if AType^.UseLFN then
          {$ENDIF}
          s := ' '+CnvString(AType^.Garble)+'"'+Password+'"'+' '
          {$IFNDEF OS2}
        else
          begin
            MessageBox(GetString(dlSpacesInPassword), nil, mfWarning+
            mfOKButton);
            goto TryAgain;
          end
          {$ENDIF}
          ;
        { Flash <<< }
      end;
    {$IFNDEF OS2}
    if AType^.UseLFN then
      {$ENDIF}
      ArchiveName := SquashesName(ArcName)
      {$IFNDEF OS2}
    else
      ArchiveName := SquashesName(lfGetShortFileName(ArcName))
      {$ENDIF}
      ;
    if ((Options and 4 <> 0) or TempDirUsed) and
      (CnvString(AType^.ForceMode) <> '')
    then
      s := s+CnvString(AType^.ForceMode)+' ';
    s := s+SCr; {установка пути внутpи аpхива}
    s := ExtrChar+' '+s+ArchiveName;
    Unp := CnvString(AType^.UnPacker);
    if (AType^.GetID = arcRAR) and (PosChar(';', Unp) > 0) then
      begin
        if PRARArchive(AType)^.VersionToExtr > 20 then
          Unp := Copy(Unp, PosChar(';', Unp)+1, 255)
        else
          Unp := Copy(Unp, 1, PosChar(';', Unp)-1);
      end;
    lGetDir(0, DirToChange);
    Inhr := CreateDirInheritance(ExtrDir, True);
    CreateDirInheritance(TempExtrDir, False);
    //JO: если каталог назначения не создался (напpимеp, если диск доступен
    //    только на чтение), то нет смысла и вызывать аpхиватоp
    if not PathExist(TempExtrDir) then
      exit;
    advance2.lChDir(TempExtrDir);
    Exec(Unp, {$IFDEF RecodeWhenDraw}OemToCharStr {$ENDIF}(s), SS, B);
    {JO}
    if not TempDirUsed then
      begin
        advance2.lChDir(DirToChange);
        DirToChange := '';
        GlobalMessage(evCommand, cmPanelReread, nil);
        GlobalMessage(evCommand, cmRereadInfo, nil);
        exit;
      end
    else
      begin
        { перекидываем файлы из временного подкаталога в каталог назначения}
        PV := New(PUserWindow, Init);
        Desktop^.Insert(PV);
        CopyDirContent(TempExtrDir+SCurDir, ExtrDir, True, (Options
          and 4 <> 0));
        PV^.Free;
        { удаляем временный каталог со всем, что в нём осталось}
        SetLength(TempExtrDir, Length(TempExtrDir)-1);
        s := GetPath(TempExtrDir);
        FRT := NewFileRec(GetName(TempExtrDir),
        {$IFNDEF OS2}
        GetName(TempExtrDir),
        {$ENDIF}
        0, 0, 0, 0, Directory,
        @S);
        New(FCT, Init(1, 1));
        FCT^.AtInsert(0, FRT);
        OldConfirms := Confirms;
        Confirms := 0;
        advance2.lChDir(s);
        Eraser.EraseFiles(FCT);
        advance2.lChDir(DirToChange);
        DirToChange := '';
        Confirms := OldConfirms;
        FCT^.DeleteAll;
        Dispose(FCT, Done);
        if Inhr > 0 then
          begin
            GlobalMessage(evCommand, cmPanelReread, nil);
            GlobalMessage(evCommand, cmRereadInfo, nil);
          end;
      end;
    {/JO}
  end { TArcDrive.ExtractFiles };
{-DataCompBoy-}

{-DataCompBoy-}
procedure TArcDrive.CopyFiles;
  var
    DT: record
      s: String;
      W: word;
      Psw: String[30];
      end;
    ExtrDir: String;
    DDr: Char;
  begin
    ExtrDir := '';
    DT.s := '';
    DT.Psw := Password;
    DT.W := UnarchiveOpt and not 2; {JO}
    Message(Application, evCommand, cmPushFirstName, @DT.s);
    if CopyDirName <> '' then
      DT.s := CopyDirName;
    {if DT.S = cTEMP_ then DT.S := '';}
    if DT.s = '' then
      GlobalMessageL(evCommand, cmPushName, hsExtract);
    if DT.s = '' then
      DT.s := HistoryStr(hsExtract, 0);
    {if DT.S = cTEMP_ then DT.S := '';}
    CopyDirName := '';
    if DT.s = cTEMP_ then
      begin
        CopyToTempDrive(AFiles, Own, ArcName);
        exit;
      end;
    {JO}
    // пpовеpяем, находится ли диск в списке дисков, на котоpые надо
    // pазаpхивиpовать не чеpез вpеменный подкаталог (по умолчанию A: и B:)
    if (DT.s <> '') and (Length(DT.s) >= 2) then
      begin
        if DT.s[2] = ':' then
          DDr := UpCase(DT.s[1])
        else
          DDr := #1; {любой символ не входящий в 'A'..'Z'}
      end
    else
      begin
        lGetDir(0, ExtrDir);
        DDr := UpCase(ExtrDir[1]);
        ExtrDir := '';
      end;
    if (DDr in ['A'..'Z']) and
      ((SystemData.Drives[DDr] {$IFNDEF OS_DOS} shl 1 {$ENDIF}) and
      ossUnarcToDirectly <> 0)
    then
      DT.W := DT.W and not 8
    else
      DT.W := DT.W or 8;
    {/JO}
    if not SkipCopyDialog then
      if ExecResource(dlgExtract, DT) <> cmOK then
        exit;
    {JO}
    if ((DT.W and 1) <> (UnarchiveOpt and 1)) or
      ((DT.W and 4) <> (UnarchiveOpt and 4))
    then
      ConfigModified := True;
    UnarchiveOpt := (DT.W and not 2) or 8;
    {/JO}
    SkipCopyDialog := False;
    ExtrDir := DT.s;
    Password := DT.Psw;
    ExtractFiles(AFiles, ExtrDir, Own, DT.W);
  end { TArcDrive.CopyFiles };
{-DataCompBoy-}

procedure TArcDrive.MakeDir;
  begin
  end;

{-DataCompBoy-}
procedure TArcDrive.EraseFiles;
  var
    SS, s: AnsiString;
    PF: PFileRec;
    j, i: word;
    o: PView;
    P: PString;
    B: boolean;
  begin
    if AFiles^.Count = 0 then
      exit;
    if AFiles^.Count = 1 then
      begin
        PF := AFiles^.At(0);
        s := GetString(dlEraseConfirm1)+PF^.FlName[True]+' ?';
      end
    else
      s := GetString(dlEraseConfirms1);
    i := MessageBox(s, nil, mfConfirmation+mfYesButton+mfNoButton
      {+mfFastButton});
    if (i <> cmYes) then
      exit;
    if AFiles^.Count > 1 then
      begin
        s := GetString(dlEraseConfirm2)+ItoS(AFiles^.Count)+' '+
          GetString(dlDIFiles)+' ?';
        j := MessageBox(s, nil, mfConfirmation+mfYesButton+
          mfNoButton);
        if (j <> cmYes) then
          exit;
      end;
    SS := MakeListFile(AFiles, False, B);
    {$IFNDEF OS2}
    if AType^.UseLFN then
      {$ENDIF}
      s := CnvString(AType^.Delete)+' '+SquashesName(ArcName)
      {$IFNDEF OS2}
    else
      s := CnvString(AType^.Delete)+' '+lfGetShortFileName(ArcName)
      {$ENDIF}
      ;

    ForceRescan := True;
    Exec(CnvString(AType^.Packer), s, SS, B);
    ForceRescan := False;
    o := Owner;
    if not ReadArchive then
      begin
        CurDir := '';
        s := Cut(ArcName, 40);
        MessageBox(GetString(dlArcMsg4)+s, nil, mfError+mfOKButton);
        ChangeUp(CurDir);
        PFilePanel(o)^.ReadDirectory;
      end
    else
      PFilePanel(o)^.RereadDir;
  end { TArcDrive.EraseFiles };
{-DataCompBoy-}

function TArcDrive.CalcLengthWithoutName;
  begin
    CalcLengthWithoutName := (10+CountryInfo.TimeFmt)*byte(Flags and
      psShowSize <> 0)+
    11*byte(Flags and psShowPacked <> 0)+
    9*byte(Flags and psShowDate <> 0)+
    (7-CountryInfo.TimeFmt)*byte(Flags and psShowTime <> 0)+
    4*byte(Flags and psShowRatio <> 0);
  end;

function TArcDrive.CalcLength;
  var
    B: word;
  begin
    B := LFNLen+1+CalcLengthWithoutName;
    if B > MaxViewWidth then
      B := MaxViewWidth;
    CalcLength := B;
  end;

procedure TArcDrive.MakeTop;
  begin
    if (LFNLen < 250) then
      begin
        if LFNLen <= SizeX then
          s := CenterStr(GetString(dlTopLFN), LFNLen)+GetString(
            dlTopSplit)
        else
          s := AddSpace(CenterStr(GetString(dlTopLFN), SizeX),
            LFNLen)
      end
    else
      s := '';
    if Flags and psShowSize <> 0 then
      s := s+Copy(GetString(dlTopOriginal), 2-CountryInfo.TimeFmt,
        255);
    if Flags and psShowPacked <> 0 then
      s := s+GetString(dlTopPacked);
    if Flags and psShowRatio <> 0 then
      s := s+GetString(dlTopRatio);
    if Flags and psShowDate <> 0 then
      s := s+GetString(dlTopDate);
    if Flags and psShowTime <> 0 then
      s := s+Copy(' '+GetString(dlTopTime), 1+CountryInfo.TimeFmt,
        255);
    if (LFNLen >= 250) then
      s := s+AddSpace(CenterStr(GetString(dlTopLFN), SizeX-Length(s)),
        LFNLen);
  end { TArcDrive.MakeTop };

procedure TArcDrive.GetFull;
  var
    s: String;
    s1: String[40];
    X: AWord;
    FD: TDate4;
    NFM: TNameFormatMode;
    OPT: word;
    LS: byte;
    CO: word;
  label Longer250, Longer250_1;
  begin
    X := 0;
    CO := C;
    {JO: если ширина колонки имени больше 250 символов, её показываем после     }
    {    остальных колонок                                                      }
    if (LFNLen < 250) then
Longer250:
      begin
        s := P^.FlName[True];
        NFM := nfmNull;
        if EXTLen = 0
        then
          OPT := flnPreferName or flnUseCutChar or flnPadRight or
            flnHighlight or flnHandleTildes
        else
          OPT := flnHardExtArea or flnAutoHideDot or flnUseCutChar or
            flnPadRight or flnHighlight or flnHandleTildes;
        if P^.Selected and (FMSetup.Show and fmsTagChar <> 0) then
            OPT := OPT or flnSelected;

        case P^.Attr and (Hidden+SysFile) of{Pavel Anufrikov -> }
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
        s := FormatLongName(s, LFNLen, EXTLen, OPT, NFM, nil);
        if P^.Selected and (FMSetup.Show and fmsTagChar = 0) then
          s := '~'+s+'~';
        MoveCStr(TWordArray(B)[X], s, C);
        Inc(X, LFNLen);
        if X >= 255 then
          exit;
        {-----------}
        if (LFNLen >= 250) then
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

    FD := TDate4(P^.FDate);
    MakeDate(DateMode, FD.Day, FD.Month, P^.Yr, FD.Hour, FD.Minute, s1);
    if (P^.Size > 0) or (P^.Attr and Directory = 0)
    then
      s := FileSizeStr(P^.Size) {!}
    else if P^.TType = ttUpDir
    then
      s := GetString(dlUpDir)
    else
      s := GetString(dlSubDir);
    if Flags and psShowSize <> 0 then
      begin
        for LS := 1 to(9-Length(s)+CountryInfo.TimeFmt) do
          s := ' '+s; {JO}
        MoveStr(TWordArray(B)[X], s, C);
        X := X+10+CountryInfo.TimeFmt;
        if X >= 255 then
          exit;
        TWordArray(B)[X-1] := Sc;
      end;
    if Flags and psShowPacked <> 0 then
      begin
        if PosChar('<', s) = 0 then
          begin
            s := FStr(P^.PSize);
            for LS := 1 to(10-Length(s)) do
              s := ' '+s; {JO}
          end
        else if (CountryInfo.TimeFmt = 0) then
          s := ' '+s;
        MoveStr(TWordArray(B)[X], s, C);
        Inc(X, 11);
        if X >= 255 then
          exit;
        TWordArray(B)[X-1] := Sc;
      end;
    if Flags and psShowRatio <> 0 then
      begin
        Inc(X, 4);
        if X >= 255 then
          exit;
        if PosChar('<', s) = 0 then
          begin
            s := ZtoS(Percent(P^.Size, P^.PSize));
            s := PredSpace(s, 3);
          end
        else
          s := '   ';
        MoveStr(TWordArray(B)[X-Length(s)-1], s, C);
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
        MoveStr(TWordArray(B)[X], Copy(s1, 10, 6), C);
        Inc(X, 7+CountryInfo.TimeFmt);
        if X >= 255 then
          exit;
        TWordArray(B)[X-1] := Sc;
      end;
    if (LFNLen >= 250) then
      begin
        C := CO;
        goto Longer250;
      end;
Longer250_1:
  end { TArcDrive.GetFull };

procedure TArcDrive.GetEmpty;
  var
    X: AWord;
  begin
    if (LFNLen < 250) then
      begin
        X := LFNLen+1;
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
    if Flags and psShowPacked <> 0 then
      begin
        Inc(X, 11);
        if X >= 255 then
          exit;
        TWordArray(B)[X-1] := Sc;
      end;
    if Flags and psShowRatio <> 0 then
      begin
        Inc(X, 4);
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
        Inc(X, 7+CountryInfo.TimeFmt);
        if X >= 255 then
          exit;
        TWordArray(B)[X-1] := Sc;
      end;
  end { TArcDrive.GetEmpty };

function TArcDrive.GetRealName;
  var
    s: String;
  begin
    s := GetDir;
    GetRealName := Copy(s, 1, PosChar(':', s))+ArcName;
  end;

function TArcDrive.GetInternalName;
  var
    IntPath: String;
  begin
    IntPath := GetDir;
    if PosChar('\', IntPath) > 0 then
      GetInternalName := Copy(IntPath, PosChar('\', IntPath), 255)
    else
      GetInternalName := '';
  end;

procedure TArcDrive.CopyFilesInto;
  begin
    ForceRescan := True;
    ArchiveFiles(GetRealName, AFiles, MoveMode, nil);
    ForceRescan := False;
  end;

procedure TArcDrive.HandleCommand;
  var
    C: PCollection absolute InfoPtr;

  procedure SetPassword;
    var
      s: String;
    begin
      s := Password;
      if ExecResource(dlgSetPassword, s) <> cmOK then
        exit;
      Password := s;
    end;

  procedure TestFiles;
    begin
      ExtractFiles(C, '', Owner, 2);
    end;

  procedure Extract;
    var
      CDir: String;
      Opts: byte;
    begin
      // JO: пpовеpяем, находится ли диск в списке дисков, на котоpые надо
      //     pазаpхивиpовать не чеpез вpеменный подкаталог (по умолчанию A: и B:)
      lGetDir(0, CDir);
      if (UpCase(CDir[1]) in ['A'..'Z']) and
        ((SystemData.Drives[UpCase(CDir[1])] {$IFNDEF OS_DOS} shl 1
          {$ENDIF}) and
        ossUnarcToDirectly <> 0)
      then
        Opts := 1
      else
        Opts := 9;
      ExtractFiles(C, '', Owner, Opts);
    end;

  var
    PDr: PDrive;
    s: String;
    o: PView;

  begin { TArcDrive.HandleCommand }
    case Command of
      cmSetPassword:
        SetPassword;
      cmArcTest:
        TestFiles;
      cmExtractTo:
        Extract;
      cmMakeForced:
        ForceRescan := True;
      cmRereadForced:
        if ForceRescan then
          begin
            ForceRescan := False;
            {$IFNDEF MINARCH}
            if (AType^.GetID = arcUC2) or (AType^.GetID = arcAIN)
            then
              begin
                PFilePanel(Owner)^.ForceReading := True;
              end;
            {$ENDIF}
            o := Owner;
            if not ReadArchive then
              begin
                CurDir := '';
                ChangeUp(CurDir);
              end;
            PFilePanel(o)^.RereadDir;
          end;
      cmFindFile:
        FindFile;
    end {case};
  end { TArcDrive.HandleCommand };

procedure TArcDrive.GetFreeSpace;
  begin
    s := '';
  end;

function ArcViewer;
  var
    P: PDrive;
    E: TEvent;
    XT: String;
    i: byte;
    PathInside: String;
  begin
    {JO: дабы перейти к найденному файлу в архиве из панели поиска}
    PathInside := AName;
    if PathInside[2] = ':' then
      PathInside[2] := ';'; {JO: меняем двоеточие не важно на что }
    i := PosChar(':', PathInside);
    if i > 0 then
      begin
        PathInside := Copy(AName, i+1, 255);
        AName := Copy(AName, 1, i-1);
      end
    else
      PathInside := '';
    {/JO}
    ArcViewer := False;
    P := New(PArcDrive, Init(AName, VAName, Num));
    if Abort then
      begin
        ArcViewer := True;
        exit;
      end;
      { AK155 21-06-2002
    Если файл не прочитался в качестве архива, то он и любым другим
    спосбом не прочитается, так что ArcViewer берет ответственность
    на себя, чтобы во вьювере не продолжать бессмысленные попытки. }
    if P = nil then
      begin
        {$IFDEF ARVID}
        XT := UpStrg(GetExt(AName));
        if (XT = '.TDR') or (XT = '.AVT')
        then
          P := New(PArvidDrive, Init(AName, Num));
        if P = nil then
          {$ENDIF}
          exit;
      end;
    E.What := evCommand;
    E.Command := cmInsertDrive;
    E.InfoPtr := P;
    Desktop^.HandleEvent(E);
    if E.What <> evNothing then
      begin
        Dispose(P, Done);
        exit;
      end
    else
      ArcViewer := True;
    {JO: переходим к найденному файлу в архиве}
    if PathInside <> '' then
      begin
        if Copy(PathInside, Length(PathInside)-1, 2) = '\.' then
            SetLength(PathInside, Length(PathInside)-2);
        if (GetPath(PathInside) <> '\') then
          begin
            P^.lChDir(Copy(GetPath(PathInside), 2, 255));
            Message(Application, evCommand, cmPanelReread, nil);
          end;
      end;
    {/JO}
  end { ArcViewer };

function TArcDrive.GetFullFlags;
  begin
    GetFullFlags := psShowSize+psShowDate+psShowTime+psShowPacked+
      psShowRatio;
  end;

procedure TArcDrive.RereadDirectory;
  begin
    if Prev <> nil then
      Prev^.RereadDirectory(s);
  end;

procedure TArcDrive.GetDirInfo;
  var
    Fl: integer;
    PSz, USz: TSize;

  procedure DoCount(P: PArcFile);
    begin
      if (P <> nil) and (not (P^.FName^[Length(P^.FName^)] in ['\',
          '/']))
      then
        begin
          Inc(Fl);
          USz := USz+P^.USize;
          PSz := PSz+P^.PSize;
        end;
    end;

  begin
    B.Title := NewStr(GetString(dlDICurArchive));
    B.Dir := NewStr(ArcName);

    Fl := Files^.Files;
    PSz := Files^.TotalCLength;
    USz := Files^.TotalLength;
    {
  if Files <> nil then Files^.ForEach(@DoCount);
  }
    B.Files := NewStr(GetString(dlDIArcTotalFiles)+ItoS(Fl)+'~');

    B.Total := NewStr(GetString(dlDIPackedSize)+FStr(PSz)+'~');
    B.Free := NewStr(GetString(dlDIUnpackedSize)+FStr(USz)+'~');

    if AType^.GetID = arcRAR then
      B.VolumeID := NewStr(GetString(dlDIVersionToExtract)+RtoS(
        PRARArchive(AType)^.VersionToExtr/10, 4, 2)+'~');
    if AType^.GetID = arcACE then
      B.VolumeID := NewStr(GetString(dlDIVersionToExtract)+RtoS(
        ACEVerToExtr/10, 4, 2)+'~');

end { TArcDrive.GetDirInfo };

procedure TArcDrive.GetDirLength(PF: PFileRec);
  begin
  end;

function ESC_Pressed: boolean;
  var
    E: TEvent;
  begin
    Application^.Idle;
    GetKeyEvent(E);
    ESC_Pressed := (E.What = evKeyDown) and (E.KeyCode = kbESC)
  end;

function TArcDrive.OpenDirectory(const Dir: String): PDrive;
  var
    PDrv: PDrive;
    Dirs: PStringCollection;
    Fils: PFilesCollection;
    fr: PFileRec;
    tmr: TEventTimer;
    _USize, _PSize: TSize;
    l: integer;
    Root: String;
    PDir: PString;
    PI: PView;
    MemReq: longInt;
    MAvail: longInt;
  begin
    NewTimer(tmr, 0);
    Dirs := New(PStringCollection, Init($10, $10, False));
    PI := WriteMsg(GetString(dlReadingList));
    New(Fils, Init($10, $10));
    Fils^.SortMode := psmOrdered;
    Files^.ResetPointer('');
    Root := UpStrg(CurDir)+'\';
    l := Length(Root);
    {JO: сначала один pаз опpеделяем объём доступной памяти, а затем по ходу дела}
    {    подсчтитываем насколько тpебования памяти pастут и не пpевысили ли они  }
    {    доступный изначально объём                                              }
    MemReq := LowMemSize;
    MAvail := MaxAvail;
    while not Files^.Last and Files^.GetNextFile and (MAvail >
        MemReq)
    do
      begin
        if (Root = Copy(UpStrg(Files^.LastDir), 1, l)) then
          begin
            with Files^.CurFile do
              begin
                if (Name = '') or (Name = '..') then
                  continue;
                _USize := Size;
                _PSize := CSize;
                PDir := NewStr(Files^.LastDir);
                Inc(MemReq, SizeOf(ShortString));
                fr := NewFileRec(Name, {$IFNDEF OS2}GetURZ(Name),
                  {$ENDIF}
                _USize, Date, 0, 0, Attr, PDir);
                Inc(MemReq, SizeOf(TFileRec));
                Inc(MemReq, Length(PDir^+Name)+2);
              end;
            if Dirs^.IndexOf(PDir) = -1 then
              Dirs^.Insert(PDir);
            Fils^.AtInsert(Fils^.Count, fr);
            if TimerExpired(tmr) then
              begin
                NewTimer(tmr, 1);
                if ESC_Pressed then
                  break;
              end;
          end;
      end;
    PI^.Free;
    PDrv := New(PFindDrive, Init(GetString(dlBranch)+Dir, Dirs, Fils,
    FreeByte));
    PDrv^.NoMemory := MAvail <= MemReq;
    OpenDirectory := PDrv;
  end { TArcDrive.OpenDirectory };

type
  TArcFindRec = record
    Mask: String; {DataCompBoy}
    Options: word;
    AddChar: String[3];
    end;

const
  ArcFindRec: TArcFindRec = (Mask: '*.*';
  Options: 2; {рекурсивный поиск}
  AddChar: ''
  );

procedure TArcDrive.FindFile;
  var
    i: longInt;
    PDrv: PDrive;
    Dirs: PStringCollection;
    Fils: PFilesCollection;
    fr: PFileRec;
    tmr: TEventTimer;
    _USize, _PSize: TSize;
    l: integer;
    Root: String;
    PDir: PString;
    LDir, DrName: String;
    DateAfter, DateBefore,
    SizeGreat, SizeLess: longInt;
    Attr: byte;
    PI: PView;
    MemReq: longInt;
    MAvail: longInt;

  begin
    NewTimer(tmr, 0);
    ArcFindRec.AddChar := '';
    ArcFindRec.Options := (FindRec.Options and 1)+
    ((FindRec.Options and 4) shr 1);
    if ExecResource(dlgArcFileFind, ArcFindRec) = cmCancel then
      exit;
    FindRec.Options := (FindRec.Options and $FFFA) or (ArcFindRec.
      Options and 1)
    or ((ArcFindRec.Options and 2) shl 1);
    ConfigModified := True;
    DelLeft(ArcFindRec.Mask);
    DelRight(ArcFindRec.Mask);
    if ArcFindRec.Mask = '' then
      ArcFindRec.Mask := x_x;
    if (Pos('*', ArcFindRec.Mask) = 0) and
      (Pos('.', ArcFindRec.Mask) = 0) and
      (Pos(';', ArcFindRec.Mask) = 0) and
      (Pos('?', ArcFindRec.Mask) = 0)
    then
      ArcFindRec.AddChar := '*.*'
    else
      ArcFindRec.AddChar := '';

    if ArcFindRec.Options and ffoAdvanced <> 0 then
      begin
        DateAfter := ParseTime(AdvanceSearchData.After);
        DateBefore := ParseTime(AdvanceSearchData.Before);
        if DateBefore = 0 then
          DateBefore := $7FFFFFFF;
        SizeGreat := StoI(AdvanceSearchData.Greater);
        SizeLess := StoI(AdvanceSearchData.Less);
        if SizeLess = 0 then
          SizeLess := $7FFFFFFF;
        Attr := 0;
        if AdvanceSearchData.Attr and 1 <> 0 then
          Attr := Archive;
        if AdvanceSearchData.Attr and 2 <> 0 then
          Attr := Attr or SysFile;
        if AdvanceSearchData.Attr and 4 <> 0 then
          Attr := Attr or Hidden;
        if AdvanceSearchData.Attr and 8 <> 0 then
          Attr := Attr or ReadOnly;
      end;

    Dirs := New(PStringCollection, Init($10, $10, False));
    PI := WriteMsg(^M^M^C+GetString(dlSearching)+'...');
    New(Fils, Init($10, $10));
    Fils^.SortMode := psmOrdered;
    Files^.ResetPointer('');
    Root := UpStrg(CurDir)+'\';
    l := Length(Root);
    {JO: сначала один pаз опpеделяем объём доступной памяти, а затем по ходу дела}
    {    подсчтитываем насколько тpебования памяти pастут и не пpевысили ли они  }
    {    доступный изначально объём                                              }
    MemReq := LowMemSize;
    MAvail := MaxAvail;
    while not Files^.Last and Files^.GetNextFile and (MAvail >
        MemReq)
    do
      begin
        if (Root = Copy(UpStrg(Files^.LastDir), 1, l)) then
          begin
            with Files^.CurFile do
              begin
                if ((ArcFindRec.Options and ffoAdvanced = 0) or
                  (Date >= DateAfter) and (Date <= DateBefore)
                  and (Size >= SizeGreat) and (Size <= SizeLess)
                  and ((Attr = 0) or (FileInfo.Attr and Attr <> 0)))
                  and ((ArcFindRec.Options and 2 <> 0) or
                  (Root = UpStrg(Files^.LastDir)))
                  and (Name <> '') and (Name <> '..')
                  and InFilter(Name, ArcFindRec.Mask+ArcFindRec.
                    AddChar)
                then
                  begin
                    _USize := Size;
                    _PSize := CSize;
                    PDir := NewStr(Files^.LastDir);
                    Inc(MemReq, SizeOf(ShortString));
                    fr := NewFileRec(Name, {$IFNDEF OS2}GetURZ(Name),
                      {$ENDIF}
                    _USize, Date, 0, 0, Attr, PDir);
                    Inc(MemReq, SizeOf(TFileRec));
                    Inc(MemReq, Length(PDir^+Name)+2);
                    if Dirs^.IndexOf(PDir) = -1 then
                      Dirs^.Insert(PDir);
                    Fils^.AtInsert(Fils^.Count, fr);
                  end;
                {JO: добавляем каталоги}
                if Length(Files^.LastDir) > l then
                  begin
                    LDir := Files^.LastDir;
                    repeat
                      SetLength(LDir, Length(LDir)-1);
                      for i := Length(LDir) downto l do
                        if LDir[i] = '\' then
                          break;
                      DrName := Copy(LDir, i+1, MaxStringLength);
                      SetLength(LDir, i);
                      if (DrName <> '')
                        //JO: всё равно для каталогов дата и атрибуты показываются
                        //    весьма условно, размер равен нулю, так что при расширенном
                        //    поиске каталоги только мешают
                        and (ArcFindRec.Options and ffoAdvanced = 0)
                        and ((ArcFindRec.Options and 2 <> 0) or
                        (Root = UpStrg(LDir)))
                        and InFilter(DrName, ArcFindRec.Mask+
                          ArcFindRec.AddChar)
                      then
                        begin
                          PDir := NewStr(Copy(LDir, 1, i));
                          Inc(MemReq, SizeOf(ShortString));
                          fr := NewFileRec(DrName, {$IFNDEF OS2}
                            GetURZ(DrName), {$ENDIF}
                          0, ArcDate, 0, 0, $80 or Directory, PDir);
                          Inc(MemReq, SizeOf(TFileRec));
                          Inc(MemReq, Length(PDir^+DrName)+2);
                          if Dirs^.IndexOf(PDir) = -1 then
                            Dirs^.Insert(PDir);
                          if Fils^.Search(fr, i) then
                            DelFileRec(fr)
                          else
                            Fils^.AtInsert(i, fr);
                        end;
                    until Length(LDir) <= l;
                  end; {конец добавления каталогов}
                if TimerExpired(tmr) then
                  begin
                    NewTimer(tmr, 1);
                    if ESC_Pressed then
                      break;
                  end;
              end;
          end;
      end;
    PI^.Free;
    PDrv := New(PFindDrive, Init(GetString(dlFindPanel)+ArcFindRec.
      Mask,
    Dirs, Fils, FreeByte));
    PDrv^.NoMemory := MAvail <= MemReq;
    Message(Owner, evCommand, cmInsertDrive, PDrv);
  end { TArcDrive.FindFile };

end.
