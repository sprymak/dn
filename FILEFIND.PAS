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
{JO: 17.06.2002 - добавил поиск файлов в архивах}
{JO:  2.12.2002 - добавил разархивирование файлов, найденных в архивах}
{$I STDEFINE.INC}
unit FileFind;

interface

uses
  Objects, Views, Dialogs, Drivers, Menus, FileCopy, FilesCol,
    Eraser,
  Drives, HideView, Dos, Gauge, DiskInfo, {FStorage,}Files, DnIni,
  Collect, advance;

type
  TFindRec = record
    Mask: String; {DataCompBoy}
    What: String;
    Options: word;
    Where: word;
    AddChar: String[3];
    end;

  PListItem = ^TListItem;
  TListItem = record
    Next: PListItem;
    Item: Pointer;
    end;

  TAdvanceSearchData = record
    After: String[17]; {Inputline}
    Before: String[17]; {Inputline}
    Greater: String[12]; {Inputline}
    Less: String[12]; {Inputline}
    Attr: word; {Checkbox}
    end;

const
  CFindWindow = #126#127#128#129#130#131#132#133#134#135#136#137#138;
  CFileFinder = #6#7#8#9;
  CFindInfo = #10#11#12#13;

  ffSeFnd = $01; {Found a file} {-$VOL}
  ffSeD2Lng = $02; {Search string overflow} {-$VOL}
  ffSeNotFnd = $FE; {No files found} {-$VOL}

  ffoAdvanced = 1;
  ffoCaseSens = 2;
  ffoRecursive = 4;
  ffoWholeWords = 8; {-$VIV 14.05.99}
  ffoAllCP = 16; {-$VIV 14.05.99}
  ffoInArch = 32; {JO  17.06.02}
  ffoRegExp = 64; {Cat 06.12.01}

  FindRec: TFindRec = (Mask: '*.*';
  What: '';
  Options: 4; {рекурсивный поиск}
  Where: 1; {поиск в текущем каталоге}
  AddChar: ''
  );

  AdvanceSearchData: TAdvanceSearchData =
  (After: '';
  Before: '';
  Greater: '';
  Less: '';
  Attr: 0
  );

  {    TempStored: Boolean = False;}

procedure FindFile;
function FindFiles(var Files: PFilesCollection; var Directories:
  PCollection; var FindRec: TFindRec; PInfo: PWhileView): byte;
function ReadList(const AName: String; var DC: PSortedCollection;
    var FC: PFilesCollection): boolean; {DataCompBoy}
function ParseTime(s: String): longInt;

type

  PFindDrive = ^TFindDrive;
  TFindDrive = object(TDrive)
    {Cat: этот объект вынесен в плагинную модель; изменять крайне осторожно!}
    isDisposable: boolean;
    Files: PFilesCollection;
    Dirs: PSortedCollection;
    ListFile: PString;
    UpFile: PFileRec; {DataCompBoy}
    AMask, AWhat: PString;
    Constructor Load(var s: TStream);
    procedure Store(var s: TStream); virtual;
    procedure NewUpFile;
    Constructor Init(const AName: String; ADirs: PCollection; AFiles:
      PFilesCollection; Num: byte); {DataCompBoy}
    Constructor InitList(const AName: String); {DataCompBoy}
    procedure lChDir(ADir: String); virtual; {DataCompBoy}
    function GetDirectory(SortMode, PanelFlags: integer; const
      FileMask: String;
    var FreeSpace: String; var TotalInfo: TSize): PCollection;
      virtual;
    procedure CopyFiles(AFiles: PCollection; Own: PView; MoveMode:
      boolean); virtual;
    procedure CopyFromArc(AFiles: PFilesCollection; Own: PView);
      {JO}
    procedure CopyFilesInto(AFiles: PCollection; Own: PView;
      MoveMode: boolean); virtual;
    procedure EraseFiles(AFiles: PCollection); virtual;
    procedure UseFile(P: PFileRec; Command: word); virtual; {JO}
    function Disposable: boolean; virtual;
    function GetRealName: String; virtual;
    function GetInternalName: String; virtual;
    function GetDir: String; virtual;
    procedure MakeDir; virtual;
    destructor Done; virtual;
    procedure GetFull(var B; P: PFileRec; C, Sc: word); virtual;
      {DataCompBoy}
    procedure GetEmpty(var B; Sc: word); virtual;
    procedure GetFreeSpace(var s: String); virtual;
    function CalcLengthWithoutName: integer; virtual;
    function CalcLength: integer; virtual;
    procedure MakeTop(var s: String); virtual;
    function isUp: boolean; virtual;
    procedure ChangeUp(var s: String); virtual;
    procedure ChangeRoot; virtual;
    procedure RereadDirectory(s: String); virtual;
    function GetFullFlags: word; virtual;
    procedure GetDirInfo(var B: TDiskInfoRec); virtual;
    procedure GetParam(n: byte); virtual; {DataCompBoy}
    procedure HandleCommand(Command: word; InfoPtr: Pointer);
      virtual;
    function OpenDirectory(const Dir: String): PDrive; virtual;
    end;

  PTempDrive = ^TTempDrive;
  TTempDrive = object(TFindDrive)
    {Cat: этот объект вынесен в плагинную модель; изменять крайне осторожно!}
    Constructor Init(Num: byte);
    Constructor Load(var s: TStream);
    procedure Store(var s: TStream); virtual;
    procedure CopyFilesInto(AFiles: PCollection; Own: PView;
      MoveMode: boolean); virtual;
    function GetRealName: String; virtual;
    function GetInternalName: String; virtual;
    procedure CopyFiles(AFiles: PCollection; Own: PView; MoveMode:
      boolean); virtual;
    procedure EraseFiles(AFiles: PCollection); virtual;
    procedure ChangeRoot; virtual;
    procedure GetDirInfo(var B: TDiskInfoRec); virtual;
    procedure GetParam(n: byte); virtual; {DataCompBoy}
    destructor Done; virtual;
    end;

procedure CopyToTempDrive(AFiles: PCollection; Own: PView;
    ArchiveName: String);

implementation
uses
  Lfn {DataCompBoy}, DNApp, advance1, advance2, advance3, Startup,
  Memory, Messages, HistList, Commands, RStrings, FlPanel, FViewer,
    Microed,
  Tree, xTime, DNUtil, U_KeyMap, {!!}CmdLine, Histries,
  Archiver, ArchDet {JO},
  ArcView {JO: для разархивирования файлов найденных в архивах}
  ;

const
  LowMemSize = $6000; {Local setting}

var
  FAdr: word;
  UsM: word;
  RD, Ofs: word;

function ESC_Pressed: boolean;
  var
    E: TEvent;
  begin
    Application^.Idle;
    GetKeyEvent(E);
    ESC_Pressed := (E.What = evKeyDown) and (E.KeyCode = kbESC)
  end;

function ReadingListMsg: PView;
  begin
    ReadingListMsg := WriteMsg(GetString(dlReadingList))
  end;

{-DataCompBoy-}
procedure FindFile;
  var
    PInfo: PWhileView;
    Files: PFilesCollection;
    Directories: PCollection;
    BB: byte; {-$VOL}
    R: TRect;
  begin
    FindRec.AddChar := '';

    if ExecResource(dlgFileFind, FindRec) = cmCancel then
      exit;
    ConfigModified := True;
    DelLeft(FindRec.Mask);
    DelRight(FindRec.Mask);
    if FindRec.Mask = '' then
      FindRec.Mask := x_x;
    if (Pos('*', FindRec.Mask) = 0) and
      (Pos('.', FindRec.Mask) = 0) and
      (Pos(';', FindRec.Mask) = 0) and
      (Pos('?', FindRec.Mask) = 0)
    then
      FindRec.AddChar := '*.*'
    else
      FindRec.AddChar := '';
    New(Files, Init($10, $10));
    Files^.SortMode := psmOrdered;
    {$IFDEF Win32}
    Files^.LFNActive := True; {JO}
    {$ENDIF}
    Directories := New(PStringCollection, Init(30, 30, False));
    R.Assign(1, 1, 40, 10);
    Inc(SkyEnabled);
    New(PInfo, Init(R));
    PInfo^.Options := PInfo^.Options or ofSelectable or ofCentered;
    if FindRec.What = ''
    then
      PInfo^.Top := GetString(dlDBViewSearch)+Cut(FindRec.Mask, 50)
    else
      PInfo^.Top := GetString(dlDBViewSearch)+Cut(FindRec.Mask, 30)+
        ' | '+Cut(FindRec.What, 17);
    PInfo^.Bottom := GetString(dlNoFilesFound);
    PInfo^.Write(1, GetString(dlDBViewSearchingIn));
    Desktop^.Insert(PInfo);
    if StartupData.Slice2 and osuReleaseFind = 0 then
      Application^.BFSpeed;
    BB := FindFiles(Files, Directories, FindRec, PInfo);
    if StartupData.Slice2 and osuReleaseFind = 0 then
      Application^.EFSpeed;
    Desktop^.Delete(PInfo);
    Dec(SkyEnabled);
    Dispose(PInfo, Done);
    if (BB and ffSeD2Lng) <> 0 then
      MessageBox(GetString(dlSE_Dir2Long), nil, mfWarning+mfOKButton);
    if (BB and ffSeNotFnd) = BB then
      MessageBox(^C+GetString(dlNoFilesFound), nil, mfInformation+
        mfOKButton);
  end { FindFile };
{-DataCompBoy-}

function ParseTime(s: String): longInt;
  label 1, 2;
  var
    DT: DateTime;
    s1, s2, S3: String[5];
    TM: String[10];
    i, j: integer;
    M: longInt;
  begin
    ParseTime := 0;
    DT.Sec := 0;
    DT.Hour := 0;
    DT.Min := 0;
    i := PosChar(' ', s);
    if i > 0 then
      begin
        TM := DelSpaces(Copy(s, i+1, 10));
        if TM <> '' then
          begin
            i := Pos(CountryInfo.TimeSep, TM);
            if i = 0 then
              exit;
            DT.Hour := StoI(Copy(TM, 1, i-1));
            j := 0;
2:
            case UpCase(TM[Length(TM)]) of
              'M':
                begin
                  j := 12;
                  goto 2
                end;
              'A':
                SetLength(TM, Length(TM)-1);
              'P':
                begin
                  j := 12;
                  SetLength(TM, Length(TM)-1);
                end;
              '0'..'9':
                ;
              else
                goto 1;
            end {case};
            Inc(DT.Hour, j);
            DT.Min := StoI(Copy(TM, i+Length(CountryInfo.TimeSep), 10));
          end;
1:
        SetLength(s, PosChar(' ', s)-1);
      end
    else
      TM := '';
    i := Pos(CountryInfo.DateSep, s);
    if i = 0 then
      exit;
    s1 := Copy(s, 1, i-1);
    Delete(s, 1, i+Length(CountryInfo.DateSep)-1);
    i := Pos(CountryInfo.DateSep, s);
    if i = 0 then
      exit;
    s2 := Copy(s, 1, i-1);
    Delete(s, 1, i+Length(CountryInfo.DateSep)-1);
    case CountryInfo.DateFmt of
      0:
        begin
          DT.Year := StoI(s);
          if DT.Year < 80 then
            Inc(DT.Year, 2000)
          else if DT.Year < 100 then
            Inc(DT.Year, 1900);
          DT.Day := StoI(s2);
          if DT.Day > 31 then
            exit;
          DT.Month := StoI(s1);
          if DT.Month > 12 then
            exit;
        end;
1:
        begin
          DT.Year := StoI(s);
          if DT.Year < 80 then
            Inc(DT.Year, 2000)
          else if DT.Year < 100 then
            Inc(DT.Year, 1900);
          DT.Day := StoI(s1);
          if DT.Day > 31 then
            exit;
          DT.Month := StoI(s2);
          if DT.Month > 12 then
            exit;
        end;
2:
        begin
          DT.Year := StoI(s1);
          if DT.Year < 80 then
            Inc(DT.Year, 2000)
          else if DT.Year < 100 then
            Inc(DT.Year, 1900);
          DT.Day := StoI(s);
          if DT.Day > 31 then
            exit;
          DT.Month := StoI(s2);
          if DT.Month > 12 then
            exit;
        end;
    end {case};
    PackTime(DT, M);
    ParseTime := M;
  end { ParseTime };

{-DataCompBoy-}
function FindFiles;
  var
    FN: String;
    dr: String;
    Nm: String;
    XT: String;
    C: Char;
    Drv: PFindDrive;
    Pnl: PView;
    CancelSearch: boolean;
    DateAfter, DateBefore,
    SizeGreat, SizeLess: longInt;
    MaskSearch: boolean;
    Attr: byte;
    Mask: String;
    t: TEventTimer;
    FF: String;
    TotalNum: longInt; {-$VIV}
    FFResult: byte; {-$VOL}
    AType: PARJArchive; {JO}
    CurFileRec: PFileRec; {JO}
    CurP: longInt; {JO}

  procedure InitPanel;
    begin
      Pnl := Message(Desktop, evBroadcast, cmInsertDrive, Drv);
      if Pnl <> nil then
        PFilePanel(Pnl)^.ChangeLocked := True;
    end;

  procedure DispatchEvents;
    var
      Event: TEvent;
    begin
      if PInfo = nil then
        exit;
      Application^.GetEvent(Event);
      if (Event.What = evCommand) and (Event.Command = cmCancel) and
        (Event.InfoPtr = PInfo^.But)
      then
        begin
          PInfo^.ClearEvent(Event);
          CancelSearch := MessageBox(GetString(dlQueryCancelSearch),
            nil, mfYesNoConfirm) = cmYes;
        end;
      if (Event.What <> evNothing) and not ((Event.What = evCommand)
          and (Event.Command = cmQuit))
      then
        Application^.HandleEvent(Event);
    end;

  {JO: добавил поиск в архивах 17.06.02}
  procedure SearchData(Path: String);
    label Skip,
      NotArchive; {JO}
    var
      PDir: PString;
      SR: lSearchRec;
      P: PFileRec;
      i: byte;
      D: DateTime;
      DirCol: PDirCol;
      XL: TXlat;
      {JO}
      LookInArchives: boolean;
      ArcPath: String;
      ArcTime: longInt;
      LDir, DrName: String;
      ArcDirs: PCollection;
      PArcLastDir: PString;
      MemReq: longInt;
      MAvail: longInt;
      {/JO}

    function SearchF: boolean;
      var
        s: TDOSStream;
        {AK155: было TBufStream; буферизация здесь абсолютно не нужна }
      begin
        PInfo^.Write(2, Cut(Path+SR.FullName, 50));
        Inc(TotalNum); {-$VIV}
        PInfo^.Write(3, GetString(dlLookedFiles)+ItoS(TotalNum));
          {-$VIV}
        s.Init(Path+SR.FullName, stOpenRead);
        SearchF := (s.Status = stOK) and
        (FViewer.SearchFileStr(@S, XL, FindRec.What, 0,
        { CaseSensitive }FindRec.Options and ffoCaseSens <> 0,
          {-$VIV 14.05.99}
        { Display       }False,
        { WholeWords    }FindRec.Options and ffoWholeWords <> 0,
        { Back          }False,
        { AllCP         }FindRec.Options and ffoAllCP <> 0,
          {-$VIV 14.05.99}
        { IsRegExp      }FindRec.Options and ffoRegExp <> 0)
          {Cat 06.12.01}
        >= 0);
        s.Done;
      end { SearchF: };

    begin { SearchData }
      LookInArchives := (FindRec.Options and ffoInArch <> 0) and (
        FindRec.What = ''); {JO}
      NullXLAT(XL);
      if (Length(Path)+Length(Mask)) > (MaxPathLen) then{-$VOL}
          {DataCompBoy}
        begin
          FFResult := FFResult or ffSeD2Lng;
          exit;
        end;
      if CancelSearch then
        exit;
      New(DirCol, Init($10, $10, False));
      DirCol^.Insert(NewStr(Path));
      {JO: сначала один pаз опpеделяем объём доступной памяти, а затем по ходу дела}
      {    подсчтитываем насколько тpебования памяти pастут и не пpевысили ли они  }
      {    доступный изначально объём                                              }
      MemReq := LowMemSize;
      MAvail := MaxAvail;
      while DirCol^.Count > 0 do
        begin
          if CancelSearch then
            break;
          Path := PString(DirCol^.At(0))^;
          DirCol^.AtFree(0);
          PDir := nil;
          DOSError := 0;
          if PInfo <> nil then
            PInfo^.Write(2, Cut(Path, 50));
          if (FindRec.What = '') then
            begin
              Inc(TotalNum); {-$VIV}
              PInfo^.Write(3, GetString(dlLookedDirs)+ItoS(TotalNum));
                {-$VIV}
            end;
          if Path[Length(Path)] <> '|' then{JO}
            begin{начало поиска не в архиве}
              lFindFirst(Path+Mask, AnyFile, SR); {JO}

              while (DOSError = 0) and not CancelSearch and (MAvail >
                  MemReq)
              do
                begin
                  if {$IFNDEF OS2}(SR.SR.Attr and VolumeID = 0) and (SR.
                      SR.Name[1] <> '.') and (SR.FullName <> '.')
                      and (SR.FullName <> '..')
                    {$ELSE}(SR.SR.Name <> '..') and (SR.SR.Name <>
                      '.') {$ENDIF}
                    and (not Security or (Security and (SR.SR.Attr
                      and Hidden = 0)))
                    and (MaskSearch or InFilter(SR.FullName, FindRec.
                      Mask+FindRec.AddChar))
                    and ((FindRec.Options and ffoAdvanced = 0) or (SR.SR
                      .Time >= DateAfter)
                    and (SR.SR.Time <= DateBefore) and (SR.FullSize >=
                      SizeGreat) and
                    (SR.FullSize <= SizeLess) and ((Attr = 0) or (SR.SR.
                      Attr and Attr <> 0)))
                    and ((FindRec.What = '') or (SR.SR.Attr and
                      Directory <> 0) and (FindRec.What = '')
                    or (FindRec.What <> '') and SearchF)
                  then
                    begin
                      if PDir = nil then
                        begin
                          PDir := NewStr(Path);
                          Inc(MemReq, SizeOf(ShortString));
                        end;
                      P := NewFileRec(SR.FullName, {$IFNDEF OS2}SR.SR.
                        Name, {$ENDIF}SR.FullSize, SR.SR.Time, SR.SR.
                        CreationTime, SR.SR.LastAccessTime, SR.SR.
                        Attr, PDir);
                      Inc(MemReq, SizeOf(TFileRec));
                      Inc(MemReq, Length(PDir^+SR.FullName)+2);
                      if Pnl = nil then
                        InitPanel;
                      if Pnl <> nil then
                        Message(Pnl, evCommand, cmInsertFile,
                          CopyFileRec(P));
                      Files^.AtInsert(Files^.Count, P);
                      if PInfo <> nil then
                        PInfo^.Bottom := ItoS(Files^.Count)+FF;
                      if TimerExpired(t) then
                        begin
                          DispatchEvents;
                          if PInfo <> nil then
                            PInfo^.DrawView;
                          NewTimer(t, 1);
                        end;
                    end;
                  if TimerExpired(t) then
                    begin
                      DispatchEvents;
                      NewTimer(t, 1);
                    end;
                  DOSError := 0;
                  lFindNext(SR);
                end;
              lFindClose(SR);
              {$IFDEF OS2}
              if DOSError = 49 then
                MessageBox(GetString(dl_CodePage_FS_Error), nil,
                  mfError+mfOKButton);
              {$ENDIF}
            end {конец поиска не в архиве}
          else
            {JO}
            begin{начало поиска в архиве}
              CtrlBreakHit := False;
              ArcPath := Copy(Path, 1, Length(Path)-1);
              ArcTime := FileTime(ArcPath);
              New(ArcFile, Init(ArcPath, stOpenRead, 512));
              if (ArcFile = nil) or (ArcFile^.Status <> stOK) then
                  goto NotArchive;
              ArcPos := 0;
              SkipSFX;
              AType := DetectArchive;
              if (AType = nil)
                or (AType^.GetID in [arcAIN, arcUC2])
                  {временно! - надо решить проблему с лочкой dndosout.bat}
              then
                goto NotArchive;
              ArcDirs := New(PStringCollection, Init(30, 30, False));
              repeat
                AType^.GetFile;
                if FileInfo.Last = 0 then
                  begin
                    Replace('/', '\', FileInfo.FName);
                    with FileInfo do
                      begin
                        if FName[1] <> '\' then
                          FName := '\'+FName;
                        if FName[Length(FName)] = '\' then
                          begin
                            SetLength(FName, Length(FName)-1);
                            Attr := Attr or Directory;
                          end;
                      end;
                    if (GetName(FileInfo.FName) <> '')
                      and InFilter(GetName(FileInfo.FName), FindRec.
                        Mask+FindRec.AddChar)
                      and ((FindRec.Options and ffoAdvanced = 0) or (
                        FileInfo.Date >= DateAfter)
                      and (FileInfo.Date <= DateBefore) and (
                        FileInfo.USize >= SizeGreat)
                      and (FileInfo.USize <= SizeLess) and ((Attr = 0)
                        or (FileInfo.Attr and Attr <> 0)))
                    then
                      begin
                        if (FileInfo.Attr and Directory) <> 0 then
                          begin
                            if FileInfo.FName[Length(FileInfo.FName)] =
                                '\'
                            then
                              PArcLastDir := NewStr(UpStrg(FileInfo.
                                FName))
                            else
                              PArcLastDir := NewStr(UpStrg(FileInfo.
                                FName+'\'));
                            Inc(MemReq, SizeOf(ShortString));
                          end;
                        if (((FileInfo.Attr and Directory) = 0) or
                          (ArcDirs^.IndexOf(PArcLastDir) = -1))
                        then
                          begin
                            PDir := NewStr(ArcPath+':'+GetPath(
                              FileInfo.FName));
                            Inc(MemReq, SizeOf(ShortString));
                            P := NewFileRec(GetName(FileInfo.FName),
                            {$IFNDEF OS2}GetName(FileInfo.FName),
                              {$ENDIF}
                            FileInfo.USize,
                            FileInfo.Date,
                            0,
                            0,
                            FileInfo.Attr,
                            PDir);
                            Inc(MemReq, SizeOf(TFileRec));
                            Inc(MemReq, Length(PDir^+GetName(
                              FileInfo.FName))+2);
                            {$IFNDEF OS2}
                            if UpStrg(P^.FlName[False]) <> UpStrg(P^.
                                FlName[True])
                            then
                              P^.FlName[False] := NoShortName;
                            {$ENDIF}
                            if Directories^.IndexOf(PDir) = -1 then
                              Directories^.Insert(PDir);
                            if Pnl = nil then
                              InitPanel;
                            if Pnl <> nil then
                              Message(Pnl, evCommand,
                              cmInsertFile, CopyFileRec(P));
                            Files^.AtInsert(Files^.Count, P);
                            if (FileInfo.Attr and Directory) <> 0
                            then
                              ArcDirs^.Insert(PArcLastDir);
                            if PInfo <> nil then
                              PInfo^.Bottom := ItoS(Files^.Count)+FF;
                            if TimerExpired(t) then
                              begin
                                DispatchEvents;
                                if PInfo <> nil then
                                  PInfo^.DrawView;
                                NewTimer(t, 1);
                              end;
                          end
                        else
                          DisposeStr(PArcLastDir);
                      end;
                    {добавляем каталоги которые присутствуют только в виде путей к файлам }
                    LDir := GetPath(FileInfo.FName);
                    if Length(LDir) > 2 then
                      repeat
                        PArcLastDir := NewStr(UpStrg(LDir));
                        Inc(MemReq, SizeOf(ShortString));
                        SetLength(LDir, Length(LDir)-1);
                        for i := Length(LDir) downto 1 do
                          if LDir[i] = '\' then
                            break;
                        DrName := Copy(LDir, i+1, MaxStringLength);
                        SetLength(LDir, i);
                        if (DrName <> '') and
                          (((FindRec.Options and ffoAdvanced) = 0) or
                          (Attr = 0) or ((Attr and Directory) <> 0))
                            and
                          InFilter(DrName, FindRec.Mask+FindRec.
                            AddChar) and
                          (ArcDirs^.IndexOf(PArcLastDir) = -1)
                        then
                          begin
                            ArcDirs^.Insert(PArcLastDir);
                            PDir := NewStr(ArcPath+':'+Copy(LDir, 1, i))
                              ;
                            Inc(MemReq, SizeOf(ShortString));
                            P := NewFileRec(DrName, {$IFNDEF OS2}
                              DrName, {$ENDIF}
                            0,
                            ArcTime,
                            0,
                            0,
                            $80 or Directory,
                            PDir);
                            Inc(MemReq, SizeOf(TFileRec));
                            Inc(MemReq, Length(PDir^+DrName)+2);
                            {$IFNDEF OS2}
                            if UpStrg(P^.FlName[False]) <> UpStrg(P^.
                                FlName[True])
                            then
                              P^.FlName[False] := NoShortName;
                            {$ENDIF}
                            if Directories^.IndexOf(PDir) = -1 then
                              Directories^.Insert(PDir);
                            if Pnl = nil then
                              InitPanel;
                            if Pnl <> nil then
                              Message(Pnl, evCommand,
                              cmInsertFile, CopyFileRec(P));
                            Files^.AtInsert(Files^.Count, P);
                            if PInfo <> nil then
                              PInfo^.Bottom := ItoS(Files^.Count)+FF;
                            if TimerExpired(t) then
                              begin
                                DispatchEvents;
                                if PInfo <> nil then
                                  PInfo^.DrawView;
                                NewTimer(t, 1);
                              end;
                          end
                        else
                          DisposeStr(PArcLastDir);
                      until Length(LDir) <= 2;
                    {конец добавления каталогов которые присутствуют только в виде путей к файлам}
                  end;
                if TimerExpired(t) then
                  begin
                    DispatchEvents;
                    NewTimer(t, 1);
                  end;
              until (FileInfo.Last > 0) or CtrlBreakHit or
                CancelSearch or (MAvail <= MemReq);
              ArcDirs^.DeleteAll;
              Dispose(ArcDirs, Done);
              CtrlBreakHit := False;
NotArchive:
              FreeObject(ArcFile);
            end; {конец поиска в архиве}
          {/JO}
          if (MAvail <= MemReq) then
            Drv^.NoMemory := True;

          if TimerExpired(t) then
            begin
              DispatchEvents;
              NewTimer(t, 1);
            end;
          if ((FindRec.Options and ffoRecursive <> ffoRecursive) and
            not LookInArchives) or
            (Path[Length(Path)] = '|')
          then
            goto Skip;
          DOSError := 0;
          lFindFirst(Path+x_x, AnyFile {$IFNDEF OS2} and not
            VolumeID {$ENDIF}, SR); {JO}
          while (DOSError = 0) and not LowMemory and not
              CancelSearch
          do
            begin
              if {$IFNDEF OS2}(SR.SR.Attr and VolumeID = 0) and (SR.SR.
                  Name[1] <> '.') and (SR.FullName <> '.') and (SR.
                  FullName <> '..')
                {$ELSE}(SR.SR.Name <> '..') and (SR.SR.Name <> '.')
                  {$ENDIF}
              then
                if (SR.SR.Attr and Directory <> 0) then
                  begin
                    if (FindRec.Options and ffoRecursive <> 0) then
                      DirCol^.Insert(NewStr(Path+SR.FullName+'\'))
                  end
                else
                  {JO} if LookInArchives and (InExtFilter(SR.
                    FullName, Archives^)
                  or InExtFilter(SR.FullName, AddArchives))
                  {             or ((PosChar('.', SR.Fullname) = 0) and (SR.SR.Attr and SysFile = 0))}
                then
                  DirCol^.Insert(NewStr(Path+SR.FullName+'|'));
              {/JO}
              lFindNext(SR);
            end;
          lFindClose(SR);
          {$IFDEF OS2}
          if DOSError = 49 then
            MessageBox(GetString(dl_CodePage_FS_Error), nil, mfError+
              mfOKButton);
          {$ENDIF}
Skip:
          if PDir <> nil then
            Directories^.Insert(PDir);
          DOSError := 0;
          if PInfo <> nil then
            PInfo^.DrawView;
        end;
      Dispose(DirCol, Done);
      DirCol := nil;
    end { SearchData };

  function SlowDrive(C: Char): boolean;
    begin
      case C of
        'C'..'Z':
          SlowDrive := IsDriveCDROM(C);
        else
          SlowDrive := True;
      end {case};
    end;

  begin { FindFiles }
    TotalNum := 0; {-$VIV}
    NewTimer(t, 0);
    FF := GetString(dlFilesFound);
    if FindRec.Options and ffoAdvanced <> 0 then
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
    CancelSearch := False;
    FFResult := ffSeFnd;
    SearchString.What := FindRec.What;
    SearchString.Opts := ((FindRec.Options and ffoCaseSens) shr 1) or
    ((FindRec.Options and ffoWholeWords) shr 2) or
    ((FindRec.Options and ffoAllCP) shr 2); {-$VIV 14.05.99}
    Microed.SearchData.Line := FindRec.What;
    Microed.SearchData.What := #0;
    Microed.SearchData.Options := SearchString.Opts;
    Microed.SearchData.Scope := 0;

    { Flash >>> }
    lGetDir(0, FN);
    if FN[Length(FN)] <> '\' then
      FN := FN+'\';
    if PathExist(FN+GetPath(FindRec.Mask)) and (Pos('\', FindRec.
        Mask) <> 0)
    then
      begin
        FN := FN+GetPath(FindRec.Mask);
        while Pos('\', FindRec.Mask) <> 0 do
          Delete(FindRec.Mask, 1, Pos('\', FindRec.Mask));
      end;
    if FN[Length(FN)] <> '\' then
      FN := FN+'\';
    { Flash <<< }

    {$IFDEF OS_DOS}
    MaskSearch := not PackMask(FindRec.Mask+FindRec.AddChar, Mask,
      True);
    {$ELSE}
    MaskSearch := not PackMask(FindRec.Mask+FindRec.AddChar, Mask);
    {$ENDIF}

    New(Drv, Init(GetString(dlFindPanel)+FindRec.Mask, Directories,
      Files, FreeByte));
    if FindRec.What <> '' then
      Drv^.AWhat := NewStr(FindRec.What);
    Drv^.AMask := NewStr(FindRec.Mask);
    Drv^.isDisposable := False;
    Pnl := nil;

    case FindRec.Where of
      1:
        SearchData(FN);
      2:
        for C := 'A'to'Z' do
          if ValidDrive(C) then
            if (C = UpCase(FN[1])) or (not SlowDrive(C)) then
                SearchData(C+':\');
      {3: SearchData(Dr)}
      else
        SearchData(Copy(FN, 1, 3));
    end {case};

    if MaxAvail <= LowMemSize then
      Drv^.NoMemory := True;
    if Pnl <> nil then
      begin
        if Pnl <> nil then
          PFilePanel(Pnl)^.ChangeLocked := False;
        Drv^.isDisposable := True;
        CurFileRec :=
        PFilePanel(Pnl)^.Files^.At(PFilePanel(Pnl)^.ScrollBar^.Value);
          {JO}
        Dispose(PFilePanel(Pnl)^.Files, Done);
        PFilePanel(Pnl)^.Files := New(PFilesCollection, Init($10,
          $10));
        Application^.Redraw;
        Message(Pnl, evCommand, cmPanelReread, nil);
        {JO: позиционируем фокус на файле, на котором он был до перечитывания панели}
        with PFilePanel(Pnl)^.Files^ do
          for CurP := 0 to Count-1 do
            if (PFileRec(At(CurP))^.FlName[True] = CurFileRec^.
                FlName[True]) and
              (PFileRec(At(CurP))^.Owner^ = CurFileRec^.Owner^)
            then
              PFilePanel(Pnl)^.ScrollBar^.SetValue(CurP);
        {/JO}
      end;
    if (Files^.Count = 0) or (Pnl = nil) then
      begin
        Dispose(Drv, Done);
        Drv := nil;
        FFResult := FFResult and ffSeNotFnd; {-$VOL}
      end
      // JO: здесь сортировка не нужна, т.к. она делается в TFindDrive.GetDirectory
      //     и в результате мы получаем сортировку дважды
      {else Files^.Sort}
      ;
    FindFiles := FFResult;
  end { FindFiles };
{-DataCompBoy-}

{AK155}
function InsertFile(const s: String; var DC: PSortedCollection;
  var FC: PFilesCollection): boolean;
  var
    Dir: String;
    Nm: String;
    XT: String;
    SR: lSearchRec;
    i: longInt;
    dr: PString;
    Duped: boolean;
  begin
    InsertFile := False;
    if (s = '')
      or (s = '*') or (s = '*.*')
        {JO: иначе в список попадёт весь каталог}
    then
      exit;
    ClrIO;
    lFindFirst(s, AnyFile, SR); {JO}
    if (DOSError = 0) and not Abort then
      begin
        InsertFile := True;
        lFSplit(lFExpand(s), Dir, Nm, XT);
        if DC = nil then
          DC := New(PStringCollection, Init($10, $10, False));
        i := DC^.IndexOf(@Dir);
        if i < 0 then
          begin
            if Dir[Length(Dir)] <> '\' then
              AddStr(Dir, '\');
            dr := NewStr(Dir);
            DC^.Insert(dr);
          end
        else
          dr := DC^.At(i);
      end;
    while (DOSError = 0) and not Abort do
      begin
        if {$IFNDEF OS2}(SR.SR.Name[1] <> '.') and (SR.FullName <>
            '.') and (SR.FullName <> '..') {$ELSE}(SR.SR.Name <>
            '..')
          and (SR.SR.Name <> '.') {$ENDIF}
        then
          begin
            if FC = nil then
              New(FC, Init($10, $10));
            Duped := False;
            for i := 0 to FC^.Count-1 do
              if (SR.FullName = PFileRec(FC^.At(i))^.FlName[True])
                and (dr^ = PFileRec(FC^.At(i))^.Owner^)
              then
                Duped := True;
            if not Duped then
              FC^.AtInsert(FC^.Count, NewFileRec(SR.FullName,
              {$IFNDEF OS2}
              SR.SR.Name,
              {$ENDIF}
              SR.FullSize,
              SR.SR.Time,
              SR.SR.CreationTime,
              SR.SR.LastAccessTime,
              SR.SR.Attr,
              dr
              )
              )
            else
              break;
          end;
        ClrIO;
        lFindNext(SR);
      end;
    lFindClose(SR);
    {$IFDEF OS2}
    if DOSError = 49 then
      MessageBox(GetString(dl_CodePage_FS_Error), nil, mfError+
        mfOKButton);
    {$ENDIF}
  end { InsertFile };

{-DataCompBoy-, AK155}
function ReadList(const AName: String; var DC: PSortedCollection;
  var FC: PFilesCollection): boolean;
  var
    F: PTextReader;
    s: String;
    i, j: longInt;
    P: PFileRec;
    D: DateTime;
    WW: PView;
  begin
    ReadList := False;

    F := New(PTextReader, Init(AName));
    if F = nil then
      exit;

    FC := nil;
    DC := nil;
    WW := ReadingListMsg;
    while not F^.Eof and (IOResult = 0) and not Abort do
      begin
        UpdateWriteView(WW);
        s := F^.GetStr;
        if s <> '' then
          case s[1] of
            ' ', #9, '>':
              begin
                  {игнорируем последующие строки многострочного описания}
              end;
            '"':
              begin{ длинное имя в кавычках }
                System.Delete(s, 1, 1);
                SetLength(s, PosChar('"', s)-1);
                InsertFile(s, DC, FC);
              end;
            else
                {попробуем целиком, или без первого символа (BSO),
              а если не вышло - то до первого пробела или Tab}
              begin
                if not InsertFile(s, DC, FC) and
                  ((s[1] <> '~') and
                  InsertFile(Copy(s, 2, Length(s)-1), DC, FC)) {BSO}
                then
                  begin
                    i := PosChar(' ', s);
                    j := PosChar(#9, s);
                    if (i = 0) or ((j <> 0) and (j < i)) then
                      i := j;
                    if i <> 0 then
                      InsertFile(Copy(s, 1, i-1), DC, FC);
                  end;
              end;
          end {case};
      end;

    Dispose(F, Done);
    WW^.Free;
    if (FC = nil) or (FC^.Count = 0) or (DC = nil) or (DC^.Count = 0)
    then
      begin
        if FC <> nil then
          Dispose(FC, Done);
        if DC <> nil then
          Dispose(DC, Done);
        MessageBox(^C+GetString(dlNoFilesFound), nil, mfInformation+
          mfOKButton);
        exit
      end;
    ReadList := True;
  end { ReadList };
{-DataCompBoy-}

{--------------------------------------------------------}
{-------------------------------------------- TFindDrive }
{--------------------------------------------------------}

{JO}
function GetArcName(s: String): String;
  var
    C: Char;
  begin
    if Length(s) < 2 then
      begin
        Result := '';
        exit;
      end;
    C := s[2];
    s[2] := ';';
      {JO: реально не важно на что меняем, лишь бы не ':'}
    s := Copy(s, 1, PosChar(':', s)-1);
    if Length(s) > 1 then
      s[2] := C;
    Result := s;
  end;
{/JO}

{-DataCompBoy-}
procedure TFindDrive.GetParam;
  begin
    Param := n;
    with ColumnsDefaultsFind do
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
  end { TFindDrive.GetParam };
{-DataCompBoy-}

{-DataCompBoy-}
Constructor TFindDrive.Init;
  var
    s: PString;
    SS: String;
    i: longInt;
  begin
    TObject.Init;
    if Num < 1 then
      GetParam(1)
    else
      GetParam(Num);
    innum := Num;
    ListFile := nil;
    isDisposable := True;
    DriveType := dtFind;
    CurDir := AName;
    Dirs := PSortedCollection(ADirs);
    Files := AFiles;
    lGetDir(0, SS);
    ClrIO;
    s := NewStr(SS);
    i := Dirs^.IndexOf(s);
    if i >= 0 then
      begin
        DisposeStr(s);
        s := Dirs^.At(i);
      end
    else
      Dirs^.Insert(s);
    NewUpFile;
    UpFile^.Owner := s;
  end { TFindDrive.Init };
{-DataCompBoy-}

procedure TFindDrive.NewUpFile;
  begin
    UpFile := NewFileRec( {$IFNDEF OS2}'..', {$ENDIF}'..', 0, 0, 0, 0,
      Directory, nil); {DataCompBoy}
  end;

Constructor TFindDrive.Load(var s: TStream);
  var
    i: longInt;
    Q, Q2: longInt;
  begin
    inherited Load(s);
    isDisposable := True;
    s.Read(DriveType, SizeOf(DriveType));
    AMask := s.ReadStr;
    AWhat := s.ReadStr;
    Dirs := PSortedCollection(s.Get);
    s.Read(i, SizeOf(i));
    if i < 0 then
      i := 0;
    if Dirs = nil then
      Fail;

    s.Read(Q, SizeOf(Q));
    if Q >= 0 then
      begin
        Files := New(PFilesCollection, Init(Q+1, $10));
        Files^.SortMode := psmOrdered;
        {$IFDEF Win32}
        Files^.LFNActive := True; {JO}
        {$ENDIF}
        Files^.Duplicates := TypeOf(Self) = TypeOf(TFindDrive);
        Files^.Owner := @Self;
        for Q2 := 0 to Q do
          begin
            Files^.AtInsert(Q2, LoadFileRecOwn(s, Dirs));
            {     if PFileRec(Files^.At(Q2))^.Owner = nil then
        PFileRec(Files^.At(Q2))^.Owner := NewStr('---:---'); }
          end;
      end;

    ListFile := s.ReadStr;
    NewUpFile;
    UpFile^.Owner := Dirs^.At(i);
  end { TFindDrive.Load };

procedure TFindDrive.Store(var s: TStream);
  var
    i: longInt;
    Q: longInt;
  begin
    inherited Store(s);
    s.Write(DriveType, SizeOf(DriveType));
    s.WriteStr(AMask);
    s.WriteStr(AWhat);
    s.Put(Dirs);
    i := Dirs^.IndexOf(UpFile^.Owner);
    s.Write(i, SizeOf(i));
    if Files = nil then
      Q := -1 {John_SW  22-03-2003}
    else
      Q := Files^.Count-1;
    s.Write(Q, SizeOf(Q));

    for i := 0 to Q do
      StoreFileRecOwn(s, Files^.At(i), Dirs);
    s.WriteStr(ListFile);
  end { TFindDrive.Store };

destructor TFindDrive.Done;
  begin
    DisposeStr(AMask);
    DisposeStr(AWhat);
    if Files <> nil then
      Dispose(Files, Done);
    Files := nil;
    if UpFile <> nil then
      DelFileRec(UpFile);
    if Dirs <> nil then
      Dispose(Dirs, Done);
    Dirs := nil;
    DisposeStr(ListFile);
    inherited Done;
  end;

procedure TFindDrive.MakeDir;
  begin
  end;

procedure TFindDrive.lChDir;
  begin
  end;

{-DataCompBoy-}
function TFindDrive.GetDirectory;
  label LLL;
  var
    AFiles: PFilesCollection;
    SR: lSearchRec;
    P, CR: PFileRec;
    D: DateTime;
    i, n: CondInt;
    DrNm: String;
    AllFiles: boolean;
    FreeSpc: TSize;
    PD: PString;
    OW: Pointer;
    s: String;
  begin
    {$IFNDEF OS2}uLfn := Flags and psShowLongName <> 0; {$ENDIF}

LLL:

    DOSError := 0;
    Abort := False;
    AllFiles := (FileMask = x_x) or (FileMask = '*');
    ClrIO;
    FreeSpc := 0;
    FreeSpace := '';
    TotalInfo := 0;

    AFiles := New(PFilesCollection, Init($10, $10));
    PFilesCollection(AFiles)^.Owner := Owner;
    AFiles^.Duplicates := TypeOf(Self) = TypeOf(TFindDrive);
    {$IFDEF Win32}
    PFilesCollection(AFiles)^.LFNActive := Flags and psShowLongName <> 0
      ; {JO}
    {$ENDIF}
    PFilesCollection(AFiles)^.SortMode := SortMode;
    s := '';
    PD := nil;
    ClrIO;
    if Files <> nil then
      for n := 0 to Files^.Count-1 do
        begin
          CR := Files^.At(n);
          if CR = nil then
            continue;
          if (CR^.Owner = nil)
          then
            OW := Dirs^.At(0)
          else if (s = CR^.Owner^)
          then
            OW := PD
          else if Dirs^.Search(CR^.Owner, i) then
            begin
              PD := Dirs^.At(i);
              OW := PD;
              s := PD^
            end
          else
            OW := Dirs^.At(0);

          P := CopyFileRec(CR);

          if AllFiles or (P^.Attr and Directory <> 0) or
            InFilter(P^.FlName[uLfn], FileMask)
          then
            with AFiles^ do
              AtInsert(Count, P)
            else
            begin
              DelFileRec(P);
              continue
            end;
          TotalInfo := TotalInfo+P^.Size;
          if P^.Attr and Directory <> 0 then
            begin
              P^.Attr := P^.Attr and $7FFF;
              P^.Size := 0;
              if PanelFlags and fmiDirLen <> 0 then
                begin
                  if (SR.SR.Name <> '..') and (SR.SR.Name <> '.')
                  then
                    P^.Size := GetDirLen(MakeNormName(P^.Owner^, P^.
                      FlName[uLfn]));

                  if Abort then
                    begin
                      PanelFlags := PanelFlags and (not fmiDirLen);
                        Abort := False;
                    end;
                end;
            end;
        end;

    if AFiles^.Count > 0 then
      AFiles^.Sort;

    if DriveType <> dtTemp then
      AFiles^.AtInsert(0, CopyFileRec(UpFile));

    GetDirectory := AFiles;
  end { TFindDrive.GetDirectory };

function TFindDrive.CalcLengthWithoutName;
  begin
    CalcLengthWithoutName := (10+CountryInfo.TimeFmt)*byte(Flags and
      psShowSize <> 0)+
    9*byte(Flags and psShowDate <> 0)+
    (7-CountryInfo.TimeFmt)*byte(Flags and psShowTime <> 0)+
    9*byte(Flags and psShowCrDate <> 0)+
    (7-CountryInfo.TimeFmt)*byte(Flags and psShowCrTime <> 0)+
    9*byte(Flags and psShowLADate <> 0)+
    (7-CountryInfo.TimeFmt)*byte(Flags and psShowLATime <> 0);
  end;

function TFindDrive.CalcLength;
  var
    B: word;
  begin
    {$IFNDEF OS2}
    if Flags and psShowLongName <> 0 then
      {$ENDIF}
      B := LFNLen+1+CalcLengthWithoutName+
      252*byte(Flags and psShowDir <> 0)
      {$IFNDEF OS2}
    else
      B := 13+CalcLengthWithoutName+
      252*byte(Flags and psShowDir <> 0)
      {$ENDIF}
      ;
    if B > MaxViewWidth then
      B := MaxViewWidth;
    CalcLength := B;
  end;

procedure TFindDrive.MakeTop;
  var
    Q: String;
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
    else if Flags and psShowDir <> 0 then
      begin
        Q := GetString(dlTopPath);
        s := s+Q+Strg(' ', 252-Length(Q)) {+ '~'#179'~'};
      end;
  end { TFindDrive.MakeTop };

procedure TFindDrive.GetFull;
  var
    X: AWord;
  begin
    inherited GetFull(B, P, C, Sc);
    if (LFNLen < 250) {$IFNDEF OS2} or (Flags and psShowLongName = 0)
        {$ENDIF}
    then
      begin
        {$IFNDEF OS2} if Flags and psShowLongName <> 0 then
          {$ENDIF}X := LFNLen+1 {$IFNDEF OS2}
        else
          X := 13 {$ENDIF};
      end
    else
      X := 0;
    if Flags and psShowSize <> 0 then
      X := X+11;
    if Flags and psShowDate <> 0 then
      Inc(X, 9);
    if Flags and psShowTime <> 0 then
      Inc(X, 6);
    if Flags and psShowCrDate <> 0 then
      Inc(X, 9);
    if Flags and psShowCrTime <> 0 then
      Inc(X, 6);
    if Flags and psShowLADate <> 0 then
      Inc(X, 9);
    if Flags and psShowLATime <> 0 then
      Inc(X, 6);
    if X >= 255 then
      exit;
    if (Flags and psShowDir <> 0) and ((LFNLen < 250) {$IFNDEF OS2} or (
        Flags and psShowLongName = 0) {$ENDIF})
    then
      begin
        C := C shr 8;
        MoveStr(TWordArray(B)[X], AddSpace( {$IFDEF RecodeWhenDraw}
        CharToOemStr {$ENDIF}(P^.Owner^), MaxViewWidth-X-1), C);
        {Inc(X, 80);
    TWordArray(B)[X-1]:=SC;}
      end;
  end { TFindDrive.GetFull };

procedure TFindDrive.GetEmpty;
  var
    X: AWord;
  begin
    inherited GetEmpty(B, Sc);
    if (LFNLen < 250) {$IFNDEF OS2} or (Flags and psShowLongName = 0)
        {$ENDIF}
    then
      begin
        {$IFNDEF OS2} if Flags and psShowLongName <> 0 then
          {$ENDIF}X := LFNLen+1 {$IFNDEF OS2}
        else
          X := 13 {$ENDIF};
      end
    else
      X := 0;
    if Flags and psShowSize <> 0 then
      X := X+11;
    if Flags and psShowDate <> 0 then
      Inc(X, 9);
    if Flags and psShowTime <> 0 then
      Inc(X, 6);
    if Flags and psShowCrDate <> 0 then
      Inc(X, 9);
    if Flags and psShowCrTime <> 0 then
      Inc(X, 6);
    if Flags and psShowLADate <> 0 then
      Inc(X, 9);
    if Flags and psShowLATime <> 0 then
      Inc(X, 6);
    if (X > 0) and (X < 255) then
      TWordArray(B)[X-1] := Sc;
  end { TFindDrive.GetEmpty };

procedure TFindDrive.ChangeUp;
  var
    P: PDrive;
  begin
    if ListFile = nil then
      s := ''
    else
      s := GetName(ListFile^);
    if Owner = nil then
      exit;
    if Prev = nil then
      begin
        New(Prev, Init(0, Owner, innum));
        if Prev = nil then
          exit;
        GlobalMessage(evCommand, cmRereadInfo, nil);
      end;
    PDrive(PFilePanel(Owner)^.Drive) := PDrive(Prev);
    Prev^.lChDir(Prev^.CurDir);
    if (Prev^.DriveType = dtDisk) and
      (PView(Owner)^.GetState(sfSelected+sfActive))
    then
      ActivePanel := Owner;
    GlobalMessage(evCommand, cmRereadInfo, nil);
    Prev := nil;
    Dispose(PDrive(@Self), Done);
  end { TFindDrive.ChangeUp };

procedure TFindDrive.ChangeRoot;
  var
    P: PDrive;
  begin
    if DriveType = dtList then
      begin
        CurDir := '';
        exit;
      end;
    if Prev <> nil then
      begin
        PDrive(PFilePanel(Owner)^.Drive) := PDrive(Prev);
        Prev^.ChangeRoot;
        GlobalMessage(evCommand, cmRereadInfo, nil);
        Prev := nil;
        Dispose(PDrive(@Self), Done);
        exit;
      end;
    New(Prev, Init(0, Owner, innum));
    if Prev = nil then
      exit;
    GlobalMessage(evCommand, cmRereadInfo, nil);
    {Prev^.Owner := Owner;}
    {Prev^.Flags := PFilePanel(Owner)^.Drive^.Flags;}
    PDrive(PFilePanel(Owner)^.Drive) := PDrive(Prev);
    Prev^.ChangeRoot;
    if (PView(Owner)^.GetState(sfSelected+sfActive)) then
      ActivePanel := Owner;
    GlobalMessage(evCommand, cmRereadInfo, nil);
    Prev := nil;
  end { TFindDrive.ChangeRoot };

function TFindDrive.isUp;
  begin
    isUp := True;
  end;

function TFindDrive.Disposable;
  begin
    Disposable := isDisposable;
  end;

procedure TFindDrive.RereadDirectory;
  var
    PV: PView;
  begin
    { if (Prev <> nil) or (Prev^.Prev = nil) then Exit;}
    PV := ReadingListMsg;
    ForceWriteShow(PV);
    if Prev <> nil then
      Prev^.RereadDirectory(s);
    if (DriveType <> dtArcFind) or not ExistFile(GetArcName(UpFile^.
        Owner^))
    then
      DosReread(Files);
    if PV <> nil then
      Dispose(PV, Done);
  end;

function TFindDrive.GetRealName;
  begin
    GetRealName := '';
  end;

function TFindDrive.GetInternalName;
  begin
    GetInternalName := '';
  end;

function TFindDrive.GetDir: String;
  var
    s: String;
    SX: longInt;
  begin
    if ListFile <> nil then
      GetDir := GetString(dlListPanel)+ListFile^
    else if (Pos(GetString(dlBranch), CurDir) = 1) or (CurDir =
        cTEMP_)
    then
      GetDir := CurDir
    else
      begin
        s := GetString(dlFindPanel);
        SX := SizeX-Length(s)-12;
        if (AMask <> nil) and (AWhat <> nil) and (AMask^ <> x_x)
        then
          GetDir := s+
          Cut(AMask^, ((SX div 3) shl 1))+'|'+
          Cut(AWhat^, (SX-Min(((SX div 3) shl 1), Length(AMask^))))
        else if (AWhat <> nil) and ((AMask = nil) or (AMask^ = x_x))
        then
          GetDir := s+Cut('*.*|'+AWhat^, SX) {???}
        else if AMask <> nil then
          GetDir := s+Cut(AMask^, SX)
        else
          GetDir := s+'*.*';
      end;
  end { TFindDrive.GetDir: };

{JO: 20.06.2002 - возможен просмотр файла найденного в архиве}
procedure TFindDrive.UseFile;
  var
    SS, s, s2, Q: String;
    C: Char;
    Unp: String;
    OwnArc: String;
    PathInside: String;
    AType: PARJArchive;
    i: byte;
    {$IFNDEF OS2}
  label TryAgain;
  {$ENDIF}
  begin
    if (DriveType in [dtFind, dtTemp]) and (P^.Owner <> nil) and
      PathFoundInArc(P^.Owner^)
    then
      begin{просмотр файла найденного в архиве}
        TempFile := '';
        if (Command = cmEditFile) or (Command = cmFileEdit) or
          (Command = cmIntEditFile) or (Command = cmIntFileEdit)
        then
          exit;
        { определяем имя архиватора и путь внутри архива}
        OwnArc := P^.Owner^;
        OwnArc[2] := ';';
          {JO: реально не важно, какой символ взять, лишь бы не ':'}
        i := PosChar(':', OwnArc);
        OwnArc := Copy(P^.Owner^, 1, i-1);
        PathInside := Copy(P^.Owner^, i+1, MaxStringLength);
        if PathInside[1] = '\' then
          Delete(PathInside, 1, 1);
        { детектим тип архива}
        New(ArcFile, Init(OwnArc, stOpenRead, 512));
        if (ArcFile = nil) or (ArcFile^.Status <> stOK) then
          begin
            FreeObject(ArcFile); {Abort := true;}exit;
          end;
        ArcPos := 0;
        SkipSFX;
        AType := DetectArchive;
        FreeObject(ArcFile);
        if AType = nil then
          exit;

        case Command of
          cmDBFView:
            C := '=';
          cmWKZView:
            C := '>';
          cmTextView:
            C := '<';
          cmHexView:
            C := '|';
          cmIntFileView:
            C := '-';
          else
            C := '+';
        end {case};
        s := ' ';
        if P^.Attr and Hidden <> 0 then
          begin
            s := '';
            {$IFNDEF OS2}
TryAgain:
            {$ENDIF}
            if ExecResource(dlgSetPassword, s) <> cmOK then
              exit;
            { Flash >>> } {JO: взял код Flash из Arcview }
            if CheckForSpaces(s) then
              s := ' '+CnvString(AType^.Garble)+s+' '
            else
              {$IFNDEF OS2} if AType^.UseLFN then
              {$ENDIF}
              s := ' '+CnvString(AType^.Garble)+'"'+s+'"'+' '
              {$IFNDEF OS2}
            else
              begin
                MessageBox(GetString(dlSpacesInPassword), nil,
                  mfWarning+
                mfOKButton);
                goto TryAgain;
              end
              {$ENDIF}
              ;
            { Flash <<< }
          end;
        SS := MakeNormName(PathInside, P^.FlName[True]);
        if SS[1] = '\' then
          Delete(SS, 1, 1);
        s2 := OwnArc;
        {$IFNDEF OS2}
        if not AType^.UseLFN then
          s2 := lfGetShortFileName(OwnArc);
        if OwnArc[Length(OwnArc)] = '.' then
          s2 := s2+'.';
        {$ENDIF}
        s := CnvString(AType^.Extract)+' '+s+
        CnvString(AType^.ForceMode)+' '+
        SquashesName(OwnArc)+' '+SquashesName(SS)+' ';
        DelDoubles('  ', s);
        TempFile := C+MakeNormName(TempDir, P^.FlName[True]);
        Q := '|'+MakeNormName(P^.Owner^, P^.FlName[True]);

        s2 := Copy(TempFile, 2, MaxStringLength);

        if C in ['<', '-', '|', '+'] then
          TempFile := TempFile+Q;

        if not ExistFile(s2) or (PackedDate(P) <> FileTime(s2)) then
          begin
            Unp := CnvString(AType^.UnPacker);
            if (AType^.GetID = arcRAR) and (PosChar(';', Unp) > 0)
            then
              Unp := Copy(Unp, PosChar(';', Unp)+1, MaxStringLength);
            s := Unp+' '+s;
            lGetDir(0, DirToChange);
            advance2.lChDir(TempDir);
            Message(Application, evCommand, cmExecString, @S);
            advance2.lChDir(DirToChange);
            DirToChange := '';
          end;
        TempFileSWP := {$IFDEF RecodeWhenDraw}OemToCharStr {$ENDIF}(
          TempFile); {JO}
        TempFile := ''; {-$VOL}
        Message(Application, evCommand, cmRetrieveSwp, nil); {JO}
      end {конец просмотра файла найденного в архиве}
    else
      begin
        if (Prev <> nil) and (Prev^.DriveType = dtArc) then
          Prev^.UseFile(P, Command)
        else
          begin
            if P^.Owner <> nil then
              s := MakeNormName(P^.Owner^, P^.FlName[uLfn]);
            Message(Application, evCommand, Command, @S);
          end;
      end;
  end { TFindDrive.UseFile };
{/JO}

procedure NewTemp;
  begin
    if TempFiles = nil then
      New(TempFiles, Init($10, $10));
    TempFiles^.SortMode := psmOrdered;
    {$IFDEF Win32}
    TempFiles^.LFNActive := True; {JO}
    {$ENDIF}
    TempFiles^.Duplicates := False;
  end;

{-DataCompBoy-}
procedure CopyToTempDrive;
  var
    Info: PView;

  procedure AddRec(P: PFileRec);
    var
      DT: DateTime;
      i, j: CondInt;
      l: Pointer;
    begin
      UpdateWriteView(Info);
      if TempFiles^.Search(P, j) then
        exit;
      i := TempDirs^.IndexOf(P^.Owner);
      if i >= 0 then
        l := TempDirs^.At(i)
      else
        begin
          if ArchiveName <> '' then
            begin
              if (P^.Owner^ = '') then
                l := NewStr(ArchiveName+':\')
              else
                l := NewStr(ArchiveName+':'+P^.Owner^);
            end
          else
            l := NewStr(P^.Owner^);
          TempDirs^.Insert(l);
        end;
      TempFiles^.AtInsert(j, NewFileRec(P^.FlName[True],
      {$IFNDEF OS2}P^.FlName[False], {$ENDIF}
      P^.Size,
      PackedDate(P),
      PackedCreationDate(P),
      PackedLastAccDate(P),
      P^.Attr,
      l
      )
      );
    end { AddRec };

  begin { CopyToTempDrive }
    if TempDirs = nil then
      TempDirs := New(PStringCollection, Init(10, 10, False));
    if TempFiles = nil then
      NewTemp;
    Info := WriteMsg(GetString(dlPleaseStandBy));
    AFiles^.ForEach(@AddRec);
    Drives.RereadDirectory(cTEMP_);
    Dispose(Info, Done);
  end { CopyToTempDrive };
{-DataCompBoy-}

{-DataCompBoy-}
procedure TTempDrive.GetParam;
  begin
    Param := n;
    with ColumnsDefaultsTemp do
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
  end { TTempDrive.GetParam };
{-DataCompBoy-}

Constructor TTempDrive.Init;
  var
    s: PString;
    {     I: LongInt;}
  begin
    TObject.Init;
    if Num < 1 then
      GetParam(1)
    else
      GetParam(Num);
    innum := Num;
    if TempDirs = nil then
      TempDirs := New(PStringCollection, Init(10, 10, False));
    if TempFiles = nil then
      NewTemp;
    isDisposable := True;
    Dirs := TempDirs;
    Files := TempFiles;
    CurDir := cTEMP_;
    DriveType := dtTemp;
    ListFile := nil;
    Lfn.lGetDir(0, FreeStr); {System.GetDir(0, FreeStr);} {Cat}
    ClrIO;
    s := NewStr(FreeStr);
    {I := Dirs^.IndexOf(S);
 if I >= 0 then begin DisposeStr(S); S := Dirs^.At(I); end
           else Dirs^.Insert(S);}
    NewUpFile;
    UpFile^.Owner := s; {DataCompBoy}
    UpFile^.OwnerDisposible := True;
  end { TTempDrive.Init };

Constructor TTempDrive.Load(var s: TStream);
  { var Q, Q2: LongInt;}
  begin
    (*
  TDrive.Load(S);
  S.Read(DriveType,SizeOf(DriveType));
  Dirs := PSortedCollection(S.Get);
  if Dirs = nil then Dirs := New(PStringCollection, Init(10, 10));

  S.Read(Q, SizeOf(Q));
  if Q >= 0 then begin
   Files := New(PFilesCollection, Init($10, $10));
   Files^.SortMode := psmOrdered;
   Files^.Duplicates := False;
{  Files^.Owner := @Self;}
   for Q2:=0 to Q do Files^.AtInsert(Q2, LoadFileRecOwn(S, Dirs));
  end else begin
   NewTemp;
   Files := TempFiles;
  end;

  TempFiles := Files;
  TempDirs  := Dirs;
  NewUpFile;
  UpFile^.Owner := S.ReadStr;
  UpFile^.OwnerDisposible:=true; *)

    TDrive.Load(s);
    if TempDirs = nil then
      TempDirs := New(PStringCollection, Init(10, 10, False));
    if TempFiles = nil then
      NewTemp;
    isDisposable := True;
    Dirs := TempDirs;
    Files := TempFiles;
    CurDir := cTEMP_;
    DriveType := dtTemp;
    ListFile := nil;
    NewUpFile;
    UpFile^.Owner := s.ReadStr;
    UpFile^.OwnerDisposible := True;
  end;

procedure TTempDrive.Store;
  var
    i: longInt;
    Q: longInt;
  begin
    TDrive.Store(s);
    { S.Write(DriveType,SizeOf(DriveType));
  S.Put(Dirs);

  if Files=nil then Q:=-1
               else Q:=Files^.Count - 1;
  S.Write(Q, SizeOf(Q));
  for I := 0 to Q do StoreFileRecOwn(S, Files^.At(I), Dirs);}
    s.WriteStr(UpFile^.Owner)
  end;

procedure TTempDrive.CopyFilesInto;
  begin
    CopyToTempDrive(AFiles, Own, '');
  end;

procedure TTempDrive.EraseFiles;
  procedure DoErase(P: PFileRec);
    var
      i: CondInt;
    begin
      i := 0;
      if (P <> nil) and
        (Files^.Search(P, i))
      then
        Files^.AtFree(i);
    end;
  begin
    AFiles^.ForEach(@DoErase);
    Drives.RereadDirectory(cTEMP_);
  end;

function TTempDrive.GetRealName;
  begin
    GetRealName := cTEMP_;
  end;

function TTempDrive.GetInternalName;
  begin
    GetInternalName := '';
  end;

destructor TTempDrive.Done;
  begin
    if UpFile <> nil then
      DelFileRec(UpFile);
    TDrive.Done;
  end;

procedure TFindDrive.GetFreeSpace;
  begin
    s := '';
  end;

function TFindDrive.GetFullFlags;
  begin
    GetFullFlags := psShowSize+psShowDate+psShowTime+
    psShowCrDate+psShowCrTime+psShowLADate+psShowLATime+psShowDir;
  end;

{-DataCompBoy-}
Constructor TFindDrive.InitList;
  var
    FC: PSortedCollection;
    DC: PFilesCollection;
  begin
    if not ReadList(AName, FC, DC) then
      Fail;
    Init(AName, PCollection(FC), DC, FreeByte);
    ListFile := NewStr(AName);
    DriveType := dtList;
    AddToDirectoryHistory(ListFile^, integer(DriveType));
  end;
{-DataCompBoy-}

procedure TTempDrive.CopyFiles;
  var
    B: boolean;
  begin
    if ReflectCopyDirection
    then
      RevertBar := Message(Desktop, evBroadcast, cmIsRightPanel, Own) <>
        nil
    else
      RevertBar := False;
    FileCopy.CopyFiles(AFiles, Own, MoveMode, 1);
  end;

{JO}
procedure TFindDrive.CopyFromArc;
  var
    i: longInt;
    FCCur: PFilesCollection;
    fr: PFileRec;
    CurArcName: String;
    Drv: PDrive;
    ExtrDir: String;
    DT: record
      s: String;
      W: word;
      Psw: String[30];
      end;
    DDr: Char;

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

  begin { TFindDrive.CopyFromArc }
    // JO: выводим диалог разархивирования, общий для всех архивов
    ExtrDir := '';
    DT.s := '';
    DT.Psw := '';
    DT.W := UnarchiveOpt and not 2; {JO}
    Message(Application, evCommand, cmPushFirstName, @DT.s);
    if CopyDirName <> '' then
      DT.s := CopyDirName;
    if DT.s = '' then
      GlobalMessageL(evCommand, cmPushName, hsExtract);
    if DT.s = '' then
      DT.s := HistoryStr(hsExtract, 0);
    CopyDirName := '';
    if DT.s = cTEMP_ then
      begin
        CopyToTempDrive(AFiles, Own, '');
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
    if ((DT.W and 1) <> (UnarchiveOpt and 1)) or
      ((DT.W and 4) <> (UnarchiveOpt and 4))
    then
      ConfigModified := True;
    UnarchiveOpt := DT.W and not 2;
    SkipCopyDialog := False;
    ExtrDir := DT.s;

    // JO: формируем файловые коллекции для каждого архива и разархивируем
    //     файлы из архивов
    repeat
      i := 0;
      fr := AFiles^.At(0);
      // для файлов с разными путями внутри архива запускаем архиватор отдельно,
      // иначе они будут распакованы с созданием подкаталогов, а нам это не надо
      CurArcName := UpStrg(fr^.Owner^);
      New(FCCur, Init($10, $10));
      repeat
        fr := AFiles^.At(i);
        if UpStrg(fr^.Owner^) = CurArcName then
          begin
            FCCur^.AtInsert(FCCur^.Count, fr);
            AFiles^.AtDelete(i);
          end
        else
          Inc(i);
      until i >= AFiles^.Count;
      if FCCur^.Count > 0 then
        begin
          Drv := New(PArcDrive, Init(GetArcName(CurArcName),
          GetArcName(CurArcName), 1));
          if Drv <> nil then
            begin
              Drv^.Owner := Owner;
                // дабы обеспечить снятие выделения в панели
              Drv^.lChDir(GetArcOwn(CurArcName));
              PArcDrive(Drv)^.Password := DT.Psw;
              PArcDrive(Drv)^.ExtractFiles(FCCur, ExtrDir, Own, DT.W);
              Dispose(Drv, Done);
            end;
        end;
      FCCur^.DeleteAll;
      Dispose(FCCur, Done);
    until AFiles^.Count = 0;
  end { TFindDrive.CopyFromArc };
{/JO}

{JO}
procedure TFindDrive.CopyFiles;
  var
    FC_Disk, FC_Arc: PFilesCollection;

    // JO: разделяем коллекцию файлов в панели поиска на две: в одну помещаем
    //     файлы, которые лежат на диске, в другую - которые лежет в архивах
  procedure SeparateCollections(FC_Comm: PFilesCollection;
    var FC_Disk, FC_Arc: PFilesCollection);
    var
      i: longInt;
      fr: PFileRec;
    begin
      FC_Disk := nil;
      FC_Arc := nil;
      if (FC_Comm = nil) or (FC_Comm^.Count = 0) then
        exit;
      New(FC_Disk, Init($10, $10));
      New(FC_Arc, Init($10, $10));
      for i := 0 to FC_Comm^.Count-1 do
        begin
          fr := FC_Comm^.At(i);
          if (fr^.Owner <> nil) and PathFoundInArc(fr^.Owner^) then
            FC_Arc^.AtInsert(FC_Arc^.Count, fr)
          else
            FC_Disk^.AtInsert(FC_Disk^.Count, fr);
        end;
      if FC_Arc^.Count = 0 then
        begin
          Dispose(FC_Arc, Done);
          FC_Arc := nil;
        end;
      if FC_Disk^.Count = 0 then
        begin
          Dispose(FC_Disk, Done);
          FC_Disk := nil;
        end;
    end { SeparateCollections };

  begin { TFindDrive.CopyFiles }
    SeparateCollections(PFilesCollection(AFiles), FC_Disk, FC_Arc);
    if FC_Disk <> nil then
      begin
        if Prev <> nil then
          Prev^.CopyFiles(FC_Disk, Own, MoveMode);
        FC_Disk^.DeleteAll;
        Dispose(FC_Disk, Done);
      end;
    if FC_Arc <> nil then
      begin
        CopyFromArc(FC_Arc, Own);
        FC_Arc^.DeleteAll;
        Dispose(FC_Arc, Done);
      end;
  end { TFindDrive.CopyFiles };
{/JO}

procedure TFindDrive.CopyFilesInto;
  begin
  end;

procedure TFindDrive.EraseFiles;
  begin
    if Prev <> nil then
      Prev^.EraseFiles(AFiles);
  end;

procedure TFindDrive.GetDirInfo;
  var
    Fl: integer;
    Sz: TSize;
    s1, s2: String[40];

    {-DataCompBoy-}
  procedure DoCount(P: PFileRec);
    begin
      if (P <> nil) and (P^.Attr and Directory = 0) then
        begin
          Inc(Fl);
          Sz := Sz+P^.Size;
        end;
    end;
  {-DataCompBoy-}

  begin
    B.Title := NewStr(GetString(dlDIFileFind));
    B.Dir := NewStr(GetString(dlDIFFMask)+Copy(CurDir, PosChar(':',
      CurDir)+1, MaxStringLength)); {DataCompBoy}
    Fl := 0;
    Sz := 0;
    if Files <> nil then
      Files^.ForEach(@DoCount);

    if Fl = 0 then
      B.Files := NewStr(GetString(dlDINoFiles))
    else
      begin
        if Fl = 1 then
          s1 := GetString(dlDIFile)
        else
          s1 := GetString(dlDIFiles);
        if Sz = 1 then
          s2 := GetString(dlDIByte)
        else
          s2 := GetString(dlDIBytes);
        B.Files := NewStr('~'+FStr(Fl)+'~ '+s1+GetString(dlDIWith)+
          '~'+FStr(Sz)+'~ '+s2);
      end;

end { TFindDrive.GetDirInfo };

procedure TFindDrive.HandleCommand(Command: word; InfoPtr: Pointer);
  begin
    if (Prev <> nil) and (Prev^.DriveType = dtArc) and
      ((Command = cmSetPassword) or
      (Command = cmArcTest) or
      (Command = cmExtractTo))
    then
      Prev^.HandleCommand(Command, InfoPtr);
  end;

function TFindDrive.OpenDirectory(const Dir: String): PDrive;
  begin
    OpenDirectory := nil;
  end;

procedure TTempDrive.GetDirInfo;
  var
    Fl: longInt;
    Sz: TSize;
    s1, s2: String[40];

    {-DataCompBoy-}
  procedure DoCount(P: PFileRec);
    begin
      if (P <> nil) and (P^.Attr and Directory = 0) then
        begin
          Inc(Fl);
          Sz := Sz+P^.Size;
        end;
    end;
  {-DataCompBoy-}

  begin
    B.Title := NewStr(GetString(dlDITemporary));
    B.Dir := NewStr(cTEMP_);
    Fl := 0;
    Sz := 0;

    if Files <> nil then
      Files^.ForEach(@DoCount);

    if Fl = 0 then
      B.Files := NewStr(GetString(dlDINoFiles))
    else
      begin
        if Fl = 1 then
          s1 := GetString(dlDIFile)
        else
          s1 := GetString(dlDIFiles);
        if Sz = 1 then
          s2 := GetString(dlDIByte)
        else
          s2 := GetString(dlDIBytes);
        B.Files := NewStr('~'+FStr(Fl)+'~ '+s1+GetString(dlDIWith)+
          '~'+FStr(Sz)+'~ '+s2);
      end;
  end { TTempDrive.GetDirInfo };

procedure TTempDrive.ChangeRoot;
  begin
  end;

end.
