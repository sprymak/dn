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
{$I STDEFINE.INC}
{Cat = Aleksej Kozlov, 2:5030/1326.13@fidonet}

{JO: этот дефайн позволяет при загрузке новой палитры принудительно сделать }
{    цвета диалога сообщений об ошибках стандартными                        }
{.$DEFINE ForceErrPal}

unit DNUtil;

interface

uses
  Dos, Lfn, {SBlocks,}Drivers, Objects, UniWin,
  Views, Histries, Scroller, DNApp, Menus, FlPanel, FileCopy, Eraser,
    DiskInfo,
  {$IFDEF NETINFO}NetInfo, {$ENDIF}
  advance, advance1, advance2, advance3, advance4, advance7,
  Startup, Dialogs, Gauges, Memory, DblWnd, Messages, HistList,
    FileFind,
  Commands, Tree, FViewer, CmdLine, FBB, DNStdDlg,
  FilesCol, UserMenu, RStrings, Colors, Microed, ed2, Editor, EdWin,
  {$IFDEF Calendar}Calendar, {$ENDIF} {JO}
  {$IFDEF SpreadSheet}Calc, CellsCol, {$ENDIF}
  {$IFDEF DBView}DBView, DBWatch, {$ENDIF}
  ArcView, HelpFile, Validate, xTime,
  {$IFDEF Modem}
  {$IFDEF LINK}NavyLink, {$ENDIF}
  Terminal, ScrollBk,
  {$IFDEF PHONES}uDialer, {$ENDIF}
  apPort, apUART,
  {$ENDIF}
  {$IFDEF PHONES}Phones, {$ENDIF}
  ASCIITab,
  {$IFDEF Game}Tetris, {$ENDIF}
  Drives, Archiver, Archset,
  {$IFNDEF MINARCH} {$IFNDEF OS2}Archread, {$ENDIF} {$ENDIF}
  {$IFDEF ARVID}Arvid, {$ENDIF}
  Idlers, FlPanelX, WinClp,
  {$IFDEF Printer}PrintMan, {$ENDIF}
  Setups, RegAll, XDblWnd,
  {$IFDEF Tasklist}
  TaskLst,
  {$ENDIF}
  CCalc, Collect, DnIni, {-$VIV}
  DnExec;

const
  {$IFDEF TrashCan}TrashCan: PTrashCan = nil; {$ENDIF}
  HelpWnd: PHelpWindow = nil;
  HelpInUse: boolean = False;
  RunMenu: boolean = False;
  IdleCounter: word = 0;
  StartTicks: word = 0;
  NullStr: byte = 0;

var
  LoaderSeg: word;
  CommandOfs: word;
  RunFirst: boolean;
  IgnoreOldFiles: boolean;
const
  Virgin: boolean = False; {JO}
  NoTempDir: boolean = False; {JO}

type
  PDNApplication = ^TDNApplication;
  TDNApplication = object(TApplication)
    {Cat: этот объект вынесен в плагинную модель; изменять крайне осторожно!}
    IdleClick: TEventTimer;
    IdleEvt: TEvent;
    TreeReader: PTreeReader;

    Pk1, Pk2, Pk3, Pk4: PView;

    Constructor Init;
    destructor Done; virtual;
    procedure InitMenuBar; virtual;
    procedure InitCommandLine; virtual;
    procedure InitDesktop; virtual;
    procedure InitStatusLine; virtual;
    procedure ViewFile(AltExt, NoExtFile: boolean; FileName: String);
      {AK155}
    procedure AddFormat;
    procedure EditFile(Intern: boolean; FileName: String);
      {DataCompBoy}
    procedure OutOfMemory; virtual;
    procedure RetrieveDesktop(const FileName: String; LS: PStream;
      LoadColors: boolean); {DataCompBoy}
    procedure SaveDesktop(const FileName: String); {DataCompBoy}
    procedure LoadDesktop(var s: TStream);
    procedure StoreDesktop(var s: TStream);
    procedure ChgColors;
    procedure EventError(var Event: TEvent); virtual;
    procedure HandleCommand(var Event: TEvent);
    end;

function CheckExit: boolean;
function PresentFile(Name: String): PStream; {DataCompBoy}
procedure WriteConfig;
procedure ShowTimeInfo;
procedure SetSysColors(var Pal);
procedure LoadPalFromFile(const FN: String); {DataCompBoy}
{$IFDEF OS_DOS}
procedure w95QuitInit; {Gimly}
function w95QuitCheck: boolean; {Gimly}
procedure w95QuitCancel;
{$ENDIF}
procedure ClearSelection(AFP: Pointer {PFilePanelRoot};
FC: Pointer {PFilesCollection - файлы к разотметке });

const
  w95locked: boolean = False; {Gimly}

const
  cfgMouseData = 3;
  cfgInterfaceData = 4;
  cfgSaversData = 5;
  cfgSystemColors = 6;
  cfgPanelDefaults = 7;
  cfgTetrisRec = 9;
  cfgOldCustomMasks = 10; {DataCompBoy}
  cfgPrinterSetup = 11;
  cfgColumnDefaults = 12;
  {cfgSavers              = 13; Obsolete}
  cfgCountryInfo = 14;
  cfgConfirms = 15;
  cfgTermDefaults = 16;
  cfgDirsToChange = 17;
  cfgFindDlgPos = 20;
  cfgCDParams = 21;
  cfgUUEData = 22;
  cfgVGAPalette = 23;
  cfgBlink = 24;
  cfgFFindOptions = 26;
  cfgOldFMSetup = 27; {DataCompBoy}
  cfgDriveInfoData = 28;
  cfgMakeListFile = 29;
  cfgOldEditorDefaults = 30; {DataCompBoy}
  cfgStartupData = 31;
  cfgSystemData = 32;
  cfgINIdata = 33;
  cfgIgnoreOldFiles = 34;
  cfgExtractOptions = 35;
  {New Record Versions}
  cfgOldSystemData = 36; {DataCompBoy}
  cfgNewSaversData = 37;
  cfgColumnsDefaultsDisk = 38;
  cfgColumnsDefaultsFind = 39;
  cfgColumnsDefaultsTemp = 40;
  cfgColumnsDefaultsArch = 41;
  {    cfgColumnsDefaultsArvd = 42; Do not use 42 - old version!!!}
  cfgNewFMSetup = 43; {DataCompBoy}
  cfgOldArcCustomMasks = 44; {DataCompBoy}
  cfgColumnsDefaultsArvd = 45; {DataCompBoy}
  cfgShowScrollBar = 47; {DataCompBoy}
  cfgNewStartupData = 48; {DataCompBoy}
  cfgDefaultArchiver = 49; {DataCompBoy}
  cfgDefaultArchiverMode = 50; {DataCompBoy}
  cfgOld2SystemData = 51; {DataCompBoy}
  cfgOldCustomMasks2 = 52; {DataCompBoy}
  cfgCustomMasks = 53; {DataCompBoy}
  cfgCustomMasks2 = 54; {DataCompBoy}
  cfgArcCustomMasks = 55; {DataCompBoy}
  cfgFMSetup = 56; {DataCompBoy}
  {                      = 57; !DO NOT USE!}
  cfgNewSystemData = 58; {DataCompBoy}
  cfgEditorDefaults = 59; {DataCompBoy}
  cfgNewPanelDefaults = 60; {DataCompBoy}
  cfgSavers = 61; {DataCompBoy}
  cfgChangeCaseOptions = 62; {DataCompBoy}
  cfgINIcrc = 63; {DataCompBoy}
  cfgAppPalette = 64; {JO}
  cfgComareDirsOptions = 65; {JO}

  dlAbout = #13#3'DN/2 Open Source'+
  #13#3'Version %s, %s'+
  #13#3'http://www.dnosp.ru/'#13+
  #13#3'Based on: '+
  'Dos Navigator Open Source 1.51.08'#13+
  {$IFDEF LITE}+#3'Light version'#13+ {$ENDIF}
  +#3'http://www.dnosp.ru'+
  +#13+
  +#13#3'Thanks to fPrint (UK) Ltd for copy of VP'+
  +#13#3'http://www.vpascal.com'#13
  +#13#13
    {#3'Based on Dos Navigator' +
     +#13#3'Copyright (C) 1991-99 RIT Research Labs'#13#13};

type
  PDataSaver = ^TDataSaver;
  TDataSaver = object(TView)
    Constructor Init;
    Constructor Load(var s: TStream);
    procedure Store(var s: TStream);
    destructor Done; virtual;
    end;

const
  DataSaver: PDataSaver = nil;

procedure ExecDNAutoexec;

implementation

uses
  HelpKern,
  DNHelp,
  VideoMan,
  {$ifdef modem}
  modemio,
  {$endif}
  ColorSel,
  UFNMatch {, Crt};

{ Load and Store Palette routines }

procedure LoadIndexes(var s: TStream);
  var
    ColorSize: byte;
  begin
    s.Read(ColorSize, SizeOf(ColorSize));
    if ColorSize > 0 then
      begin
        if ColorIndexes <> nil then
          FreeMem(ColorIndexes, 2+ColorIndexes^.ColorSize);
        GetMem(ColorIndexes, ColorSize);
        s.Read(ColorIndexes^, ColorSize);
        ColorIndexes^.ColorSize := (ColorSize-2);
      end;
  end;

procedure StoreIndexes(var s: TStream);
  var
    ColorSize: byte;
  begin
    if ColorIndexes <> nil then
      ColorSize := 2+ColorIndexes^.ColorSize
    else
      ColorSize := 0;
    s.Write(ColorSize, SizeOf(ColorSize));
    if ColorSize > 0 then
      s.Write(ColorIndexes^, ColorSize);
  end;

Constructor TDataSaver.Init;
  var
    R: TRect;
  begin
    R.Assign(0, 0, 0, 0);
    inherited Init(R);
    SetState(sfVisible, False);
    Options := Options and not ofSelectable;
    EventMask := 0;
    DataSaver := @Self;
  end;

const
  dskViewerFind = 2;
  dskEditorFind = 3;
  dskHideCmdLine = 4;
  dskViewerBounds = 5;
  dskTempContents = 6;
  dskTempContents2 = 7;

  {Cat:warn потенциально глюкавое место: нельзя читать в память больше, чем
          туда может поместиться; кроме того, нежелательно одной операцией
          читать несколько подряд расположенных переменных - это у нас в
          исходнике они подряд, а компилятор может думать иначе}

Constructor TDataSaver.Load;
  var
    D, l: AWord;
    Q, Q2: longInt;
  begin
    if DataSaver <> nil then
      Dispose(DataSaver, Done);
    DataSaver := nil;
    inherited Load(s);
    DataSaver := @Self;
    repeat
      s.Read(D, SizeOf(D));
      if D = 0 then
        break;
      s.Read(l, SizeOf(l));
      case D of
        dskViewerBounds:
          s.Read(LastViewerBounds, l);
        dskViewerFind:
          s.Read(FViewer.SearchString, l);
        dskEditorFind:
          s.Read(Microed.SearchData, l);
        dskHideCmdLine:
          begin
            s.Read(HideCommandLine, l);
            if (CommandLine <> nil) and (CommandLine^.GetState(
                sfVisible) and HideCommandLine)
            then
              ToggleCommandLine(not HideCommandLine);
          end;
        dskTempContents2:
          if TempFiles = nil then
            begin
              TempDirs := PStringCollection(s.Get);
              if TempDirs = nil then
                continue;
              s.Read(Q, SizeOf(Q));
              if Q >= 0 then
                begin
                  TempFiles := New(PFilesCollection, Init(Q+1, $10));
                  TempFiles^.SortMode := psmOrdered;
                  TempFiles^.Duplicates := False;
                  {$IFDEF Win32}
                  TempFiles^.LFNActive := True;
                  {$ENDIF}
                  {TempFiles^.Owner := @Self;}
                  for Q2 := 0 to Q do
                    TempFiles^.AtInsert(Q2, LoadFileRecOwn(s,
                      TempDirs));
                end;
            end
          else
            s.Seek(s.GetPos+l);
        else
          s.Seek(s.GetPos+l);
      end {case};
    until D = 0;
  end { TDataSaver.Load };

procedure TDataSaver.Store;

  var
    D: AWord;
    i, Q, Q2, SPos: longInt;

  procedure StoreBlock(i: AWord; var B; Sz: AWord);
    begin
      s.Write(i, SizeOf(i));
      s.Write(Sz, SizeOf(Sz));
      s.Write(B, Sz);
    end;

  procedure MarkP(Blk: AWord);
    begin
      s.Write(Blk, SizeOf(Blk));
      s.Write(Blk, SizeOf(Blk));
      SPos := s.GetPos;
    end;

  procedure UnMark;
    begin
      i := s.GetPos-SPos;
      s.Seek(SPos-SizeOf(AWord));
      s.Write(i, SizeOf(AWord));
      s.Seek(s.GetSize);
    end;

  begin { TDataSaver.Store }
    inherited Store(s);
    HideCommandLine := (CommandLine <> nil) and not CommandLine^.
      GetState(sfVisible);
    StoreBlock(dskViewerFind, FViewer.SearchString, SizeOf(FViewer.
      SearchString));
    StoreBlock(dskEditorFind, Microed.SearchData, SizeOf(Microed.
      SearchData));
    StoreBlock(dskViewerBounds, LastViewerBounds, SizeOf(
      LastViewerBounds)+SizeOf(TPoint)*2);
    StoreBlock(dskHideCmdLine, HideCommandLine, SizeOf(
      HideCommandLine));
    if (TempFiles <> nil) and (TempFiles^.Count <> 0) then
      begin
        MarkP(dskTempContents2);
        s.Put(TempDirs);
        if TempDirs <> nil then
          begin
            Q := TempFiles^.Count-1;
            s.Write(Q, SizeOf(Q));
            for Q2 := 0 to Q do
              StoreFileRecOwn(s, TempFiles^.At(Q2), TempDirs);
            UnMark;
          end;
      end;
    D := 0;
    s.Write(D, SizeOf(D));
  end { TDataSaver.Store };

destructor TDataSaver.Done;
  begin
    DataSaver := nil;
    inherited Done;
  end;

{-DataCompBoy-}
function PresentFile(Name: String): PStream;
  var
    s: PStream;
  begin
    if ExistFile(Name) then
      begin
        s := New(PBufStream, Init(Name, stOpenRead, 2048));
        if s^.Status <> stOK then
          begin
            Dispose(s, Done);
            s := nil;
          end
      end
    else
      s := nil;
    PresentFile := s;
  end;
{-DataCompBoy-}

function CheckExit: boolean;
  var
    Event: TEvent;
  function FindQuit(P: PView): boolean;
    begin
      P^.HandleEvent(Event);
      FindQuit := (Event.What = evNothing) or not P^.Valid(cmQuit);
    end;
  begin
    { if FormatWindow <> nil then begin CheckExit := False; Exit; end;}
    Event.What := evCommand;
    Event.Command := cmQuit;
    CheckExit := Desktop^.FirstThat(@FindQuit) = nil;
  end;

{-DataCompBoy-}
procedure ClearSwap;
  var
    DT: Dos.DateTime;
    l: longInt;
    SR: lSearchRec;

  procedure ClearFiles(const FileSpec: String);
    begin
      lFindFirst(SwpDir+FileSpec, Archive, SR);
      while DOSError = 0 do
        begin
          if SR.SR.Time < l then
            EraseFile(SR.FullName);
          ClrIO;
          lFindNext(SR);
        end;
      lFindClose(SR);
    end;

  begin
    FillChar(DT, SizeOf(DT), 0);
    GetDate(DT.Year, DT.Month, DT.Day, word(l));

    PackTime(DT, l);
    SetFileAttr(SwpDir+'DN.FLG', 0);
    ClrIO;
    {$IFDEF OS_DOS}
    ClearFiles('DN*.SWP');
    {$ENDIF}
    ClearFiles('$DN*.*');
    ClearFiles('$$DN*.*');
    ClearFiles('$$$DN*.*');
  end { ClearSwap };
{-DataCompBoy-}

{-DataCompBoy-}
Constructor TDNApplication.Init;
  var
    R: TRect;
    i: integer;
    C: Char absolute i { let's so };
    FileName: String;
    LoadStream: PStream;
    Event: TEvent;

  var
    flj: boolean;
  begin
    for C := 'A'to'Z' do
      DrvTrees[C].C := nil;
    LastViewerBounds.Assign(0, 0, 0, 0);

    if RunFirst then
      ClearSwap;

    TApplication.Init;

    LoadHistories;

    GetExtent(R);
    if ShowSeconds then
      R.A.X := R.B.X-10
    else
      R.A.X := R.B.X-7;
    R.B.Y := R.A.Y+1;
    Clock := New(PClockView, Init(R));
    if InterfaceData.Options and ouiClock = 0 then
      Clock^.Hide;
    PClockView(Clock)^.UpDate;

    Desktop^.GetExtent(R);
    Dec(R.B.Y);
    Dec(R.B.X);
    R.A.Y := R.B.Y-3;
    R.A.X := R.B.X-5;
    {$IFDEF TrashCan}
    TrashCan := New(PTrashCan, Init(R));
    TrashCan^.ImVisible := False;
    TrashCan^.Hide;
    Desktop^.Insert(TrashCan);
    {$ENDIF}
    if HideCommandLine then
      CommandLine^.Hide;
    (*
  If RunFirst then
   begin
    OldArchiveName:='';
    NewArchiveName:='';
   end;
*)
    {$IFDEF OS_DOS}
    LoadStream := PresentFile(SwpDir+'DN'+ItoS(DNNumber)+'.SWP');
    if LoadStream = nil then
      {$ENDIF}
      LoadStream := PresentFile(SourceDir+'DN'+GetEnv('DNDSK')+
        '.DSK');
    if LoadStream <> nil then
      RetrieveDesktop('', LoadStream, True);
    InitDrivers;

    {-$VOL begin}
    if not RunFirst or CBAutoSave then
      begin
        ClipBoardStream := GetMeMemoStream;
        LoadStream := PresentFile(SourceDir+'CLIPBOAR'+'.DN');
        if (LoadStream <> nil) and (ClipBoardStream <> nil) then
            begin
            LoadStream^.Seek(0);
            ClipBoardStream^.CopyFrom(LoadStream^, LoadStream^.
              GetSize);
            if SystemData.Options and ossUseSysClip <> 0 then
                SyncClipOut {(false)};
          end;
        FreeObject(LoadStream);
      end;
    {-$VOL end}

    Insert(Clock);

    flj := False;
    if RunFirst then
      for i := 1 to ParamCount do
        begin
          if flj then
            FileName := FileName+ParamStr(i)
          else
            FileName := ParamStr(i);
          if Pos('"', FileName) <> 0 then
            flj := not flj;
          if (FileName[1] <> '/') and not flj then
            EditFile(True, DelSquashes(FileName));
        end;

    if RunMenu then
      begin
        Event.What := evCommand;
        Event.Command := cmUserMenu;
        PutEvent(Event);
      end;
    {$IFDEF OS_DOS}
    if ExistFile(SwpDir+'DN'+ItoS(DNNumber)+'.SWP') then
      EraseByName(SwpDir+'DN'+ItoS(DNNumber)+'.SWP');
    {$ENDIF}
  end { TDNApplication.Init };
{-DataCompBoy-}

procedure TDNApplication.InitCommandLine;
  var
    R: TRect;
    HideCL: boolean;
  begin
    HideCL := (InterfaceData.Options and ouiHideCmdline <> 0) or
      HideCommandLine;

    R.Assign(0, 0, 0, 0);
    TreeReader := New(PTreeReader, Init(R));
    Insert(TreeReader);
    TreeReader^.Hide;

    GetExtent(R);

    R.A.Y := R.B.Y-1-byte(InterfaceData.Options and ouiHideStatus = 0);
    R.B.Y := R.A.Y+1-byte(HideCL);
    CommandLine := New(PCommandLine, Init(R));
    Insert(CommandLine);
    if not HideCL then
      ActivateView(CommandLine);
  end { TDNApplication.InitCommandLine };

{-DataCompBoy-}
procedure SaveRealDsk;
  begin
    PDNApplication(Application)^.SaveDesktop(SourceDir+'DN'+GetEnv(
      'DNDSK')+'.DSK');
  end;
{-DataCompBoy-}

{-DataCompBoy-}
destructor TDNApplication.Done;
  var
    B: word;
    SaveStream: PStream;
  begin
    {-$VOL begin} {if CBAutoSave added by piwamoto}
    if CBAutoSave then
      begin
        SaveStream := New(PBufStream, Init(SourceDir+'CLIPBOAR'+
          '.DN', stCreate, 2048));
        if (SaveStream <> nil) and (SaveStream^.Status = stOK) and (
            ClipBoardStream <> nil)
        then
          begin
            ClipBoardStream^.Seek(0);
            SaveStream^.CopyFrom(ClipBoardStream^, ClipBoardStream^.
              GetSize);
          end;
        Dispose(SaveStream, Done);
        SaveStream := nil;
      end;
    if ClipBoard <> nil then
      Dispose(ClipBoard, Done);
    ClipBoard := nil;
    if ConfigModified then
      WriteConfig;
    if (StartupData.Unload and osuAutosave <> 0)
    then
      SaveRealDsk
      ;
    {AK155 SaveHistories;}
      {все запоминалось по ходу дела, притом с чтением,}
    {а теперь можно только напортить}
    SaveHistories;
      {JO: пока вернул назад, а то история совсем не запоминается}
    HideCommandLine := (CommandLine <> nil) and not CommandLine^.
      GetState(sfVisible);
    B := $8000 or (byte(HideCommandLine));
    inherited Done;
    {$IFDEF Modem}
    SetBlink(StartupData.Unload and osuBlinking <> 0);
    DeallocateScrollBk;
    if StartupData.Unload and osuRestorePal <> 0 then
      ResetVGApalette(True);
    if COMPort <> nil then
      begin
        B := COMPort^.GetBaseAddr;
        if not TottalExit then
          begin
            COMPort^.ptOptionsOff(ptDropModemOnClose);
            COMPort^.ptOptionsOff(ptRestoreOnClose);
          end;
        Dispose(COMPort, Done);
        SetFIFOBuffering(B, False, 0);
      end;
    {$ENDIF}
    DoneDrivers;
  end { TDNApplication.Done };
{-DataCompBoy-}

procedure TDNApplication.EventError;
  begin
    {$IFDEF SS}
    if (Event.What = evMouseMove) and (Event.Where.Y = 0) and (Event.
        Where.X = Size.X-1)
      and (SSaver = nil) and SaversData.Mouse
    then
      InsertIdler;
    {$ENDIF}
  end;

procedure TDNApplication.ChgColors;
  begin
    ChangeColors;
    WriteConfig;
  end;

{JO}
procedure WriteHighlite;
  var
    F: lText;
  begin
    lAssignText(F, SourceDir+'dnhgl.grp');
    lRewriteText(F);
    Writeln(F.t, CustomMask1^.Filter);
    Writeln(F.t, CustomMask2^.Filter);
    Writeln(F.t, CustomMask3^.Filter);
    Writeln(F.t, CustomMask4^.Filter);
    Writeln(F.t, CustomMask5^.Filter);
    Writeln(F.t, CustomMask6^.Filter);
    Writeln(F.t, CustomMask7^.Filter);
    Writeln(F.t, CustomMask8^.Filter);
    Writeln(F.t, CustomMask9^.Filter);
    Writeln(F.t, CustomMask10^.Filter);
    Writeln(F.t, Startup.Archives^.Filter);
    Close(F.t);
  end;
{JO}

{-DataCompBoy-}
procedure WriteConfig;
  {$IFDEF DNPRG}
  var
    s: TBufStream;
    i: AWord;
    SPos: longInt;

  procedure StoreBlock(i: AWord; var B; Sz: AWord);
    begin
      s.Write(i, SizeOf(i));
      s.Write(Sz, SizeOf(Sz));
      s.Write(B, Sz);
    end;

  procedure MarkP(Blk: AWord);
    begin
      s.Write(Blk, SizeOf(Blk));
      s.Write(Blk, SizeOf(Blk));
      SPos := s.GetPos;
    end;

  procedure UnMark;
    begin
      i := s.GetPos-SPos;
      s.Seek(SPos-SizeOf(AWord));
      s.Write(i, SizeOf(AWord));
      s.Seek(s.GetSize);
    end;
  {$ENDIF} {DNPRG}

  begin { WriteConfig }
    {$IFDEF DNPRG}
    ConfigModified := False;
    s.Init(SourceDir+'DN'+GetEnv('DNCFG')+'.CFG', stCreate, 16384);
    if s.Status <> stOK then
      begin
        Msg(erCantOpenConfig, nil, mfError+mfOKButton);
        s.Done;
        exit;
      end;
    s.Write(ConfigSigns[NumSupportedConfigs].Sign[1], ConfigSigns[
      NumSupportedConfigs].SignLen);
    if ConfigSigns[NumSupportedConfigs].HavVer then
      s.Write(VersionWord, SizeOf(VersionWord));
    StoreBlock(cfgNewSystemData, SystemData, SizeOf(SystemData));
    StoreBlock(cfgNewStartupData, StartupData, SizeOf(StartupData));
    StoreBlock(cfgMouseData, MouseData, SizeOf(MouseData));
    StoreBlock(cfgInterfaceData, InterfaceData, SizeOf(InterfaceData));
    StoreBlock(cfgShowScrollBar, ShowScrollBar, SizeOf(ShowScrollBar));
    {$IFDEF SS}
    StoreBlock(cfgNewSaversData, SaversData.Time, SizeOf(SaversData)-
      SizeOf(SaversData.Selected)*2);
    MarkP(cfgSavers);
    s.Put(SaversData.Selected.List);
    UnMark;
    {$ENDIF}
    StoreBlock(cfgSystemColors, SystemColors, SizeOf(SystemColors));
    StoreBlock(cfgNewPanelDefaults, PanelDefaults, SizeOf(
      PanelDefaults));
    StoreBlock(cfgEditorDefaults, EditorDefaults, SizeOf(
      EditorDefaults));
    StoreBlock(cfgTetrisRec, TetrisRec, SizeOf(TetrisRec));
    StoreBlock(cfgArcCustomMasks, Startup.Archives^.Filter, SizeOf(
      Startup.Archives^.Filter));
    MarkP(cfgCustomMasks);
    s.Write(CustomMask1^.Filter, SizeOf(CustomMask1^.Filter));
    s.Write(CustomMask2^.Filter, SizeOf(CustomMask2^.Filter));
    s.Write(CustomMask3^.Filter, SizeOf(CustomMask3^.Filter));
    s.Write(CustomMask4^.Filter, SizeOf(CustomMask4^.Filter));
    s.Write(CustomMask5^.Filter, SizeOf(CustomMask5^.Filter));
    UnMark;
    MarkP(cfgCustomMasks2);
    s.Write(CustomMask6^.Filter, SizeOf(CustomMask6^.Filter)); {JO}
    s.Write(CustomMask7^.Filter, SizeOf(CustomMask7^.Filter));
    s.Write(CustomMask8^.Filter, SizeOf(CustomMask8^.Filter));
    s.Write(CustomMask9^.Filter, SizeOf(CustomMask9^.Filter));
    s.Write(CustomMask10^.Filter, SizeOf(CustomMask10^.Filter));
      {JO}
    UnMark;
    {$IFDEF PRINTMANAGER}
    StoreBlock(cfgPrinterSetup, RPrinterSetup, SizeOf(RPrinterSetup));
    {$ENDIF}
    StoreBlock(cfgColumnsDefaultsDisk, ColumnsDefaultsDisk, SizeOf(
      ColumnsDefaultsDisk));
    StoreBlock(cfgColumnsDefaultsFind, ColumnsDefaultsFind, SizeOf(
      ColumnsDefaultsFind));
    StoreBlock(cfgColumnsDefaultsTemp, ColumnsDefaultsTemp, SizeOf(
      ColumnsDefaultsTemp));
    StoreBlock(cfgColumnsDefaultsArch, ColumnsDefaultsArch, SizeOf(
      ColumnsDefaultsArch));
    StoreBlock(cfgColumnsDefaultsArvd, ColumnsDefaultsArvd, SizeOf(
      ColumnsDefaultsArvd));
    StoreBlock(cfgCountryInfo, CountryInfo, SizeOf(CountryInfo));
    StoreBlock(cfgConfirms, Confirms, SizeOf(Confirms));
    StoreBlock(cfgUUEData, UUDecodeOptions, SizeOf(UUDecodeOptions)+
      SizeOf(TUUEncodeData));
    StoreBlock(cfgMakeListFile, MakeListFileOptions, SizeOf(
      MakeListFileOptions));
    StoreBlock(cfgTermDefaults, TerminalDefaults, SizeOf(
      TerminalDefaults));
    StoreBlock(cfgFMSetup, Startup.FMSetup, SizeOf(Startup.FMSetup));
    StoreBlock(cfgVGAPalette, VGA_palette, SizeOf(VGA_palette));
    StoreBlock(cfgBlink, CurrentBlink, SizeOf(CurrentBlink));
    StoreBlock(cfgFFindOptions, FileFind.FindRec.Options, SizeOf(
      word)*2);
    StoreBlock(cfgDriveInfoData, DriveInfoData, SizeOf(DriveInfoData));
    StoreBlock(cfgExtractOptions, UnarchiveOpt, SizeOf(UnarchiveOpt));
      {JO}
    StoreBlock(cfgDefaultArchiver, DefaultArchiver, SizeOf(
      DefaultArchiver));
    StoreBlock(cfgDefaultArchiverMode, DefaultArcMode, SizeOf(
      DefaultArcMode));
    StoreBlock(cfgExtractOptions, UnarchiveOpt, SizeOf(UnarchiveOpt));
      {JO: два раза???}
    StoreBlock(cfgChangeCaseOptions, ChangeNamesCaseOptions, SizeOf(
      ChangeNamesCaseOptions));
    {StoreBlock(cfgINIcrc, INIstoredcrc, SizeOf(INIstoredcrc));}
    MarkP(cfgINIdata);
    s.Write(INIstoredtime, SizeOf(INIstoredtime));
    s.Write(INIstoredsize, SizeOf(INIstoredsize));
    s.Write(iniparamblock_START, Ofs(iniparamblock_END)-Ofs(
      iniparamblock_START));
    UnMark;

    if IgnoreOldFiles then
      StoreBlock(cfgIgnoreOldFiles, IgnoreOldFiles, 0);

    MarkP(cfgDirsToChange);
    for i := 0 to 8 do
      s.WriteStr(DirsToChange[i]);
    UnMark;
    StoreBlock(cfgAppPalette, appPalette, SizeOf(appPalette));
    StoreBlock(cfgComareDirsOptions, ComareDirsOptions, SizeOf(
      ComareDirsOptions));

    StoreBlock(0, i, 0);
    s.Done;
    WriteHighlite; {JO}
    {$ENDIF} {DNPRG}
  end { WriteConfig };
{-DataCompBoy-}

{-DataCompBoy-}
procedure TDNApplication.LoadDesktop(var s: TStream);
  var
    P: PView;
    PP: PView;
    Pal: PString;
    SaveState: AWord;
  begin
    lGetDir(0, ActiveDir);
    if Desktop^.Valid(cmClose) then
      begin
        Desktop^.Clear;
        repeat
          P := PView(s.Get);

          {Cat}
          if s.Status <> stOK then
            begin
              ErrMsg(erCantReadDesktop);
              break;
            end;
          {/Cat}

          if P <> nil then
            begin
              if DataSaver <> nil then
                begin
                  Dispose(DataSaver, Done);
                  DataSaver := nil;
                  continue
                end;
              P := ValidView(P);
              if P = nil then
                continue;
              SaveState := P^.State;
              P^.Hide;
              Desktop^.InsertView(P, Desktop^.Last);
              if SaveState and sfVisible <> 0 then
                P^.Show;
            end;
        until { P = nil;}s.GetPos = s.GetSize;
        {AK155: P = nil бывает, например, при невозможности открыть
      запомненный просмотр; это не повод терять все остальные настройки }
      end;
    P := Desktop^.Current;
    if P <> nil then
      P^.SetState(sfActive, True);
  end { TDNApplication.LoadDesktop };
{-DataCompBoy-}

procedure TDNApplication.StoreDesktop(var s: TStream);
  var
    Pal: PString;

  procedure WriteView(P: PView);
    type
      TLongInt = array[0..10] of longInt;
      PLongInt = ^TLongInt;
    var
      l: array[0..5] of longInt;
    begin
      if (P <> Desktop^.Last)
        {$IFDEF TrashCan} and (P <> PView(TrashCan)) {$ENDIF}
        and (P <> PView(Desktop^.Background))
        and (TypeOf(P^) <> TypeOf(TWriteWin))
        and (TypeOf(P^) <> TypeOf(THelpWindow))
      then
        s.Put(P);
    end;

  begin
    if DataSaver = nil then
      New(DataSaver, Init);
    if DataSaver <> nil then
      begin
        s.Put(DataSaver);
        Dispose(DataSaver, Done);
        DataSaver := nil;
      end;
    Desktop^.ForEach(@WriteView);
    s.Put(nil);
  end { TDNApplication.StoreDesktop };

function CacheLngId: String;
  begin
    {$IFDEF DNPRG}CacheLngId := versionName+LngId+versionDate+'123';
      {$ENDIF}
  end;

{-DataCompBoy-}
procedure TDNApplication.RetrieveDesktop;
  var
    s: PStream;
    Sign: String[MaxSignLen];
    B, BB: boolean;
    SM: word;
    CC: word;
    R: TRect;
    SaveBounds: TRect;
    PS: PString;
    PJ: PString;
    Event: TEvent;
    F: lFile;
    OldAppSize, OldDskSize: TPoint;
    Q: String;
    QQ: integer;

  procedure GetStringCache;
    var
      SS: String;
      P: PCollection;
    begin
      SS := CnvString(s^.ReadStr);
      if SS <> #0#0#0 then
        begin
          P := PCollection(s^.Get);
          if (StringCache = nil)
          then
            if (SS = CacheLngId)
            then
              StringCache := P
            else
              begin
                InitStringCache;
                Dispose(P, Done)
              end
          else
            Dispose(P, Done);
        end;
    end { GetStringCache };

  function GetGLUKName(s: String): String;
    begin
      GetGLUKName := GetPath(s)+Copy(GetName(s), 1, 12);
    end;

  function GetfURZ2(const s: String): String;
    var
      A, aa, aaa: String;
    begin
      lFSplit(s, A, aa, aaa);
      aa := aa+aaa;
      GetfURZ2 := A+Copy(aa, 1, 8)+'.'+Copy(aaa, 9, 3);
    end;

  label
    Err;
  begin { TDNApplication.RetrieveDesktop }
    CC := 0;
    HideCommandLine := (CommandLine <> nil) and (not CommandLine^.
      GetState(sfVisible));
    SetState(sfActive, True);
    if LS = nil
    then
      s := New(PBufStream, Init(FileName, stOpenRead, 4096))
    else
      s := LS;
    if not Desktop^.Valid(cmClose) then
      exit;
    if LowMemory
    then
      OutOfMemory
    else if (s^.Status <> stOK) or (s^.GetSize < SizeOf(DskSign))
    then
Err:
      ErrMsg(erCantReadDesktop)
    else
      begin
        s^.Read(Sign[1], DskSign.SignLen);
        Sign[0] := Char(DskSign.SignLen);
        if Sign <> DskSign.Sign then
          goto Err;
        GetStringCache;
        if s^.Status <> stOK then
          goto Err;
        PS := s^.ReadStr;
        if PS <> nil then
          lChDir(PS^);
        DisposeStr(PS);
        PJ := s^.ReadStr;
        if (PJ <> nil) and (PJ^ <> '') then
          if PosChar(PJ^[1], '+-=<>|[!') = 0 then
            begin
              lAssignFile(F, PJ^);
              lSetFAttr(F, $20);
              lEraseFile(F);
            end
          else
            begin
              TempFile := Copy(PJ^, 2, MaxStringLength);
              QQ := Pos('|', TempFile);
              Dec(QQ);
              if QQ > 0 then
                Q := Copy(TempFile, QQ+2, MaxStringLength)
              else
                Q := '';
              QQ := QQ and 255; {BP bugfix by piwamoto}
              if not ExistFile(TempFile) then
                TempFile := GetfURZ(Copy(PJ^, 2, QQ));
              if not ExistFile(TempFile) then
                TempFile := GetfURZ2(Copy(PJ^, 2, QQ));
              if not ExistFile(TempFile) then
                TempFile := GetGLUKName(Copy(PJ^, 2, QQ));
              if not ExistFile(TempFile) then
                TempFile := Copy(PJ^, 2, QQ);
              case PJ^[1] of
                '-':
                  CC := cmIntFileView;
                '=':
                  CC := cmDBFView;
                '>':
                  CC := cmWKZView;
                '<':
                  CC := cmTextView;
                '|':
                  CC := cmHexView;
                '[':
                  CC := cmReadArchive;
                '!':
                  CC := cmViewFilter;
                else
                  CC := cmFileView;
              end {case};
              if CC <> 0 then
                begin
                  if Q <> '' then
                    TempFile := TempFile+'|'+Q;
                  Event.What := evCommand;
                  Event.Command := CC;
                  Event.InfoPtr := @TempFile;
                  PutEvent(Event);
                end;
              RunMenu := False;
            end;
        s^.ReadStrV(FreeStr);
          {S^.Read(FreeStr,1); S^.Read(FreeStr[1],length(FreeStr));}
          {Cat}
        s^.ReadStrV(FreeStr);
          {S^.Read(FreeStr,1); S^.Read(FreeStr[1],length(FreeStr));}
          {Cat}
        s^.Read(SM, SizeOf(SM));
        s^.Read(OldAppSize, SizeOf(Size));
        s^.Read(OldDskSize, SizeOf(Size));
        LoadIndexes(s^);
        GetExtent(SaveBounds);
        Desktop^.Clear;
        BB := not (OldDskSize.Equals(Desktop^.Size));
        if BB then
          begin
            Desktop^.Hide;
            R.A := Desktop^.Origin;
            R.B.X := R.A.X+OldDskSize.X;
            R.B.Y := R.A.Y+OldDskSize.Y;
            Desktop^.ChangeBounds(R);
          end;
        {LoadDesktop(S^);} {Cat: перенёс в самый конец}
        DisposeStr(PJ);
        s^.Read(TempBounds, SizeOf(TempBounds));
        {S^.Read(ArcBounds, SizeOf(TempBounds));}
        {$IFDEF TrashCan}s^.Read(TrashCan^.ImVisible, 1); {$ENDIF}
        KeyMacroses := PCollection(s^.Get);
        s^.Read(R, SizeOf(R));
        if not ShowSeconds then
          if R.A.X > (ScreenWidth shr 1) then
            Inc(R.A.X, 3)
          else
            Dec(R.B.X, 3);
        Clock^.Locate(R);
        {S^.Read(ArcFlags, 4);}
        {$IFDEF TrashCan}
        if TrashCan^.ImVisible then
          begin
            TrashCan^.Show;
            s^.Read(R, SizeOf(R));
            TrashCan^.Locate(R)
          end;
        {$ENDIF}
        if PreserveMenuPositions then
          LoadMenuDefaults(MenuBar^.Menu, s^);
        LoadDesktop(s^);
        // JO: нижележащий кусок должен _обязательно_ быть _после_ LoadDesktop,
        //     иначе размер панелей не подстраивается под видеорежим
        if BB then
          begin
            R.Assign(0, byte(InterfaceData.Options and ouiHideMenu = 0),
              Size.X, Size.Y
            -byte(InterfaceData.Options and ouiHideStatus = 0)
            -byte(not HideCommandLine));
            Desktop^.ChangeBounds(R);
            Desktop^.Show;
          end;
      end;
    Desktop^.Redraw;
    GetBounds(R);
    if (Clock^.Size.X <= 0) or (Clock^.Size.Y <= 0) or not R.
        Contains(Clock^.Origin)
    then
      begin
        if ShowSeconds then
          R.Assign(Size.X-10, 0, Size.X, 1)
        else
          R.Assign(Size.X-7, 0, Size.X, 1);
        Clock^.Locate(R);
      end;
    Dispose(s, Done);
    ActivateView(Desktop^.Current);
    GlobalMessage(evCommand, cmRereadForced, nil);
  end { TDNApplication.RetrieveDesktop };
{-DataCompBoy-}

{-DataCompBoy-}
procedure TDNApplication.SaveDesktop(const FileName: String);
  var
    s: PStream;
    F: lFile;
    B: boolean;
    PP: Pointer;
    R: TRect;

  procedure PutStringCache;
    var
      SS: PString;
    begin
      New(SS);
      lFSplit(FileName, FreeStr, FreeStr, SS^);
      if (UpStrg(SS^) = '.DSK') or
        (StringCache = nil) or
        (StringCache^.Count = 0)
      then
        begin
          SS^:= #0#0#0;
          s^.WriteStr(SS);
        end
      else
        begin
          SS^:= CacheLngId;
          s^.WriteStr(SS);
          s^.Put(StringCache);
        end;
      Dispose(SS);
    end { PutStringCache };

  begin { TDNApplication.SaveDesktop }
    ClrIO;
    s := New(PBufStream, Init(FileName, stCreate, 2048));
    if not LowMemory and (s^.Status = stOK) then
      begin
        s^.Write(DskSign.Sign[1], DskSign.SignLen);
        PutStringCache;
        s^.WriteStr(@DirToChange);
        s^.WriteStr(@TempFile);
        s^.WriteStr(@NullStr);
        s^.WriteStr(@NullStr);
        s^.Write(ScreenMode, SizeOf(word));
        s^.Write(Size, SizeOf(Size));
        s^.Write(Desktop^.Size, SizeOf(Size));
        StoreIndexes(s^);
        {StoreDesktop(S^);} {Cat: перенёс в самый конец}
        s^.Write(TempBounds, SizeOf(TempBounds));
        {$IFDEF TrashCan}s^.Write(TrashCan^.ImVisible, 1); {$ENDIF}
        s^.Put(KeyMacroses);
        Clock^.GetBounds(R);
        if R.A.X > (ScreenWidth shr 1) then
          R.A.X := R.B.X-10
        else
          R.B.X := R.A.X+10;
        s^.Write(R, SizeOf(R));
        {$IFDEF TrashCan}
        TrashCan^.GetBounds(R);
        if TrashCan^.ImVisible then
          s^.Write(R, SizeOf(R));
        {$ENDIF}
        if PreserveMenuPositions then
          StoreMenuDefaults(MenuBar^.Menu, s^);
        StoreDesktop(s^);
        if s^.Status <> stOK then
          begin
            PP := @FileName;
            Msg(erCantCreateFile, @PP, mfOKButton+mfError);
            //    MessageBox('S^.Status    = '+ItoS(S^.Status)+#13+
            //               'S^.ErrorInfo = '+ItoS(S^.ErrorInfo), nil, mfOkButton);
            Dispose(s, Done);
            lAssignFile(F, FileName);
            lEraseFile(F);
            exit;
          end;
      end;
    Dispose(s, Done);
  end { TDNApplication.SaveDesktop };
{-DataCompBoy-}

procedure TDNApplication.OutOfMemory;
  begin
    Msg(erNotEnoughMemory, nil, mfError+mfOKButton);
  end;

const
  ExtFileName: array[boolean] of String[10] =
  ('dn.vwr', 'dnalt.vwr');

  {-DataCompBoy-} {AK155}
procedure TDNApplication.ViewFile(AltExt, NoExtFile: boolean;
    FileName: String); {AK155}
  var
    W: PWindow;
    R: TRect;
    fr: PFileRec;
    up: tUserParams;
    P: PViewRecord;
    i, j: integer;
    PS: PString;
    Q: integer;
    RN: String;

  function SpecialIntView: boolean;
    var
      FileIsDBF: boolean;
      XT: String;
    label
      db;
    begin
      SpecialIntView := False;
      XT := ' '+RN;
      UpStr(XT);
      PS := @XT;
      if (InterfaceData.Options and ouiTrackViewers <> 0) and
        (ViewHistory <> nil)
      then
        i := ViewHistory^.IndexOf(@PS)
      else
        i := -1;
      if i >= 0 then
        begin
          P := ViewHistory^.At(i);
          Q := P^.fViewMode;
          if Q and vmInternal = vmInternal then
            Q := -1;
        end
      else
        Q := -1;
      XT := UpStrg(GetExt(FileName));
      {$IFDEF DBView}
      if (XT = '.DBF') or (Q = vmDB) then
        begin
          {--- start -------- Eugeny Zvyagintzev ---------}
          {Use history if possible}
          XT := ' '+RN;
          UpStr(XT);
          PS := @XT;
          Q := -1;
          if (InterfaceData.Options and ouiTrackViewers <> 0) and
            (ViewHistory <> nil)
          then
            i := ViewHistory^.IndexOf(@PS)
          else
            i := -1;
          if i >= 0 then
            begin
              P := ViewHistory^.At(i);
              Q := P^.fViewMode;
            end;
          FileIsDBF := False;
          W := New(PDBWindow, Init(RN+'|'+FileName, FileIsDBF));
          {If file is not valid DBF and NOT empty DBF file then use internal viewer}
          if (W = nil) and (FileIsDBF = False) then
            exit; { Flash }
          {--- finish -------- Eugeny Zvyagintzev ---------}
          if (Q = vmDB) and (W <> nil) then
            with P^, W^ do
              begin
                R.Assign(fOrigin.X, fOrigin.Y, fOrigin.X+fSize.X,
                  fOrigin.Y+fSize.Y);
                AdjustToDesktopSize(R, fDeskSize);
                Locate(R);
                with PDBWindow(W)^.P^ do
                  begin
                    Delta := fdDelta;
                    Pos := fdPos;
                    KeyMap := fKeyMap; {John_SW 14-03-2003}
                    if XLatFile <> nil then
                      DisposeStr(XLatFile);
                    XLatFile := NewStr(fXlatFile);
                    UseXLat := fUseXLat;
                    if UseXLat
                    then
                      SetXlatFile(XLatFile^, True)
                    else
                      SetXlatFile('', True);
                  end;
              end;
          goto db;
        end;
      {$ENDIF}

      {$IFDEF SpreadSheet}
      if (XT = '.WKZ') or (Q = vmSpread) or (Q = vmSpreadSL) then
          begin
          W := New(PCalcWindow, Init(R, FileName));
          if ((Q = vmSpread) or (Q = vmSpreadSL)) and (W <> nil)
          then
            with P^, W^ do
              begin
                R.Assign(fOrigin.X, fOrigin.Y, fOrigin.X+fSize.X,
                  fOrigin.Y+fSize.Y);
                AdjustToDesktopSize(R, fDeskSize);
                Locate(R);
                with PCalcWindow(W)^.CalcView^ do
                  begin
                    Delta := fsDelta;
                    Cur := fsCur;
                    Mark := fsMark;
                    CurrentCalc := fsCurrentCalc;
                    SearchPos := fsSearchPos;
                    ErrorCell := fsErrorCell;
                    HScroll^.Value := Delta.X; {AK155}
                    VScroll^.Value := Delta.Y; {AK155}
                    ShowSeparators := Q = vmSpreadSL; {AK155}
                  end;
              end;
          goto db;
        end;
      {$ENDIF}

      SpecialIntView := ArcViewer(FileName, RN, FreeByte);
      exit;

db:
      if (W <> nil) and not W^.Valid(0) then
        begin
          Dispose(W, Done);
          exit;
        end;
      {    StoreViewInfo(W);  AK155: сохранять бессмысленно (еще нечего)
        и излишне (сохранит Done) }
      InsertWindow(W);
      SpecialIntView := True;
    end { SpecialIntView: };

  procedure TextView;
    var
      s: String;
      V: PFileViewer;
    begin
      W := New(PFileWindow, Init(FileName, RN, EditorDefaults.ViOpt
        and 1 <> 0));
      if W = nil then
        exit;
      if not (W^.Valid(cmValid)) then
        W^.Free
      else
        begin
          V := PFileViewer(W^.Current);
          s := V^.VFileName;
          System.Insert(' ', s, 1);
          UpStr(s);
          PS := @S;
          if (InterfaceData.Options and ouiTrackViewers <> 0) and (
              ViewHistory <> nil)
          then
            i := ViewHistory^.IndexOf(@PS)
          else
            i := -1;
          if i >= 0 then
            if (PViewRecord(ViewHistory^.At(i))^.fViewMode and not
                vmInternal) > vmAsm
            then
              i := -1;
          if i >= 0 then
            begin
              P := ViewHistory^.At(i);
              R.Assign(P^.fOrigin.X, P^.fOrigin.Y,
              P^.fOrigin.X+P^.fSize.X,
              P^.fOrigin.Y+P^.fSize.Y);
              AdjustToDesktopSize(R, P^.fDeskSize);
              W^.Locate(R);
              with V^, P^ do
                begin
                  if fPos > FileSize then
                    fPos := 0;
                  Seek(fPos);
                  if XLatFile <> nil then
                    DisposeStr(XLatFile);
                  XLatFile := NewStr(fXlatFile);
                  ViewMode := fViewMode and not vmInternal;
                  Filter := fFilter;
                  HexEdit := fHexEdit;
                  Wrap := fWrap;
                  XDelta := fXDelta;
                  HexPos := fHexPos;
                  Cur := fCur;
                  UseXLat := fUseXLat;
                  KeyMap := fKeyMap;
                  if UseXLat
                  then
                    SetXlatFile(XLatFile^, True)
                  else
                    SetXlatFile('', True);
                  MarkPos := fMarks;
                  Loaded := True;
                  MakeLines;
                end;
            end;
          InsertWindow(W);
        end;
    end { TextView };

  begin { TDNApplication.ViewFile }
    i := PosChar('|', FileName);
    if i > 0 then
      begin
        RN := Copy(FileName, i+1, MaxStringLength);
        SetLength(FileName, i-1);
        if (ActivePanel <> nil) and (Copy(PFilePanelRoot(ActivePanel)^.
            DirectoryName, 1, 4) = 'UC2:')
        then
          begin
            j := PosChar(';', FileName);
            if j > 0 then
              SetLength(FileName, j-1);
              {AK155 удалить номер версии файла в uc2-архиве }
          end;
      end;
    FileName := lFExpand(FileName);
    if i <= 0 then
      RN := FileName;
    fr := CreateFileRec(FileName);
    FillChar(up, SizeOf(up), 0);
    up.active := fr;
    Desktop^.GetExtent(R);
    Abort := False;
    if (ReuseViewers > 0)
      and (Message(Desktop, evBroadcast, cmFindView, @FileName) <>
        nil)
      and ((ReuseViewers = 2) or
      ((ReuseViewers = 1) and
      (MessageBox(GetString(dlOpenNewWindow), nil, mfYesButton+
        mfNoButton) <> cmYes)
      )
      )
    then
      begin
        TempFile := '';
        exit;
      end;

    { Логика выбора просмотра такая:
  по F3 (not AltExt) - согласно dn.vwr, затем специальный внутренний, затем текстовый;
  по Alt-F3 (AltExt) - согласно dnalt.vwr, затем текстовый;
  по Alt-Shift-F3 (это вообще не здесь) - безусловно текстовый. }

    if not NoExtFile and
      ExecExtFile(ExtFileName[AltExt], @up, dlLoadingViewer)
    then
      exit;
    if not AltExt and SpecialIntView then
      exit;
    TextView;
    DelFileRec(fr); {Cat}
  end { TDNApplication.ViewFile };
{-DataCompBoy-} {AK155}

{-DataCompBoy-}
procedure TDNApplication.EditFile;
  var
    W: PWindow;
    R: TRect;
    PS: PString;
    V: PFileEditor;
    i: integer;
    P: PEditRecord;
    fr: PFileRec;
    up: tUserParams;
    s: String;

  function Edit: boolean;
    begin
      Edit := ExecExtFile('DN.EDT', @up, dlLoadingEditor);
    end;

  label ex;
  begin
    {JO: редактирование найденных в архиве файлов из панели поиска не предусмотрено}
    if PathFoundInArc(FileName) then
      exit;
    {/JO}
    FileName := lFExpand(FileName);
    fr := CreateFileRec(FileName);
    FillChar(up, SizeOf(up), 0);
    up.active := fr;
    if (Intern xor (SystemData.Options and ossEditor = 0)) or
      not Edit
    then
      begin
        if (ReuseEditors > 0)
          and (Message(Desktop, evBroadcast, cmFindEdit, @FileName) <>
            nil)
          and ((ReuseEditors = 2) or
          ((ReuseEditors = 1) and
          (MessageBox(GetString(dlOpenNewWindow), nil, mfYesButton+
            mfNoButton) <> cmYes)
          )
          )
        then
          begin
            TempFile := '';
            exit;
          end;
        if (InterfaceData.Options and ouiStoreEditorPosition <> 0)
        then
          begin
            if not TempBounds.Empty then
              R := TempBounds
            else
              begin
                Desktop^.GetExtent(R);
                LastEditDeskSize := Desktop^.Size
              end;
            AdjustToDesktopSize(R, LastEditDeskSize);
          end
        else
          Desktop^.GetExtent(R);
        W := New(PEditWindow, Init(R, FileName));
        if (W <> nil) and (W^.Valid(cmValid)) then
          begin
            V := PEditWindow(W)^.Intern;
            FreeStr := V^.EditName;
            System.Insert(' ', FreeStr, 1);
            UpStr(FreeStr);
            PS := @FreeStr;
            if (InterfaceData.Options and ouiTrackEditors <> 0) and (
                EditHistory <> nil)
            then
              i := EditHistory^.IndexOf(@PS)
            else
              i := -1;
            if i >= 0 then
              begin
                P := EditHistory^.At(i);
                R.Assign(P^.fOrigin.X, P^.fOrigin.Y, P^.fOrigin.X+P^.
                  fSize.X, P^.fOrigin.Y+P^.fSize.Y);
                AdjustToDesktopSize(R, P^.fDeskSize);
                W^.Locate(R);
                with V^, P^ do
                  begin
                    MarkPos := fMarks;
                    Mark.A := fBlockStart;
                    Mark.B := fBlockEnd;
                    ScrollTo(fDelta.X, fDelta.Y);
                    Pos := fPos;
                    BlockVisible := fBlockVisible;
                    VertBlock := fVerticalBlock;
                    EdOpt.HiLite := fHighlight;
                    EdOpt.HiliteColumn := fHiliteColumn;
                    EdOpt.HiliteLine := fHiliteLine;
                    EdOpt.AutoIndent := fAutoIndent;
                    EdOpt.AutoJustify := fAutoJustify;
                    EdOpt.AutoBrackets := fAutoBrackets;
                    EdOpt.LeftSide := fLeftSide;
                    EdOpt.RightSide := fRightSide;
                    EdOpt.InSide := fInSide;
                    InsertMode := fInsMode;
                    KeyMap := fKeyMap; {-$VIV}
                    { Flash >>> }
                    EdOpt.BackIndent := fBackIndent;
                    EdOpt.AutoWrap := fAutoWrap;
                    OptimalFill := fOptimalFill;
                    TabReplace := fTabReplace;
                    EdOpt.SmartTab := fSmartTab;
                    { Flash <<< }
                    WorkString := GetLine(fDelta.Y);
                      {-$VIV 11.05.99}
                  end;
              end { else StoreEditInfo(W)};
              { Commented by Flash 21-12-2002 }
            InsertWindow(W);
          end;
      end;
ex:
    DelFileRec(fr);
  end { TDNApplication.EditFile };
{-DataCompBoy-}

procedure TDNApplication.AddFormat;
  begin
    { if AddFormatDialog <> cmOK then Exit;
 New(FormatWindow,Init);
 DeskTop^.Insert(FormatWindow);}
  end;

procedure TDNApplication.InitMenuBar;
  var
    R: TRect;
  begin
    GetExtent(R);
    R.B.Y := R.A.Y;
    MenuBar := PMenuBar(LoadResource(dlgMainMenu));
    if MenuBar = nil then
      FatalError('Invalid resource file.');
    MenuBar^.Menu^.Items^.Name^:= '~'+Char(SystemMenuChar)+'~';
    MenuBar^.Locate(R);
    MenuBar^.Options := MenuBar^.Options and (not ofPreProcess) or
      ofPostProcess;
  end;

procedure TDNApplication.InitStatusLine;
  var
    R: TRect;
  begin
    GetExtent(R);
    R.A.Y := R.B.Y;
    R.Move(0, -1);
    StatusLine := PStatusLine(LoadResource(dlgStatusLine));
    StatusLine^.Locate(R);
  end;

procedure TDNApplication.InitDesktop;
  var
    R: TRect;
    WS: word;
  begin
    WS := 0;
    HideCommandLine := (WS and cdnHideCmdLine <> 0);
    DisableCommands(DblWndCommands);
    GetExtent(R);
    if InterfaceData.Options and ouiHideMenu = 0 then
      Inc(R.A.Y);
    if InterfaceData.Options and ouiHideStatus = 0 then
      Dec(R.B.Y);
    if (InterfaceData.Options and ouiHideCmdline = 0) and not
        HideCommandLine
    then
      Dec(R.B.Y);
    New(Desktop, Init(R));
  end;

{-DataCompBoy-}
procedure LoadPalFromFile(const FN: String);
  var
    P: longInt;
    LoadPalette, LoadVGAPalette: boolean;
    ST: String;
    s: TDOSStream;
    Pal: PString;
  begin
    LoadPalette := False;
    LoadVGAPalette := False;
    s.Init(FN, stOpenRead);
    if s.Status = 0 then
      begin
        Pal := s.ReadStr;
        if Pal <> nil then
          begin
            ST := Pal^;
            DisposeStr(Pal);
            while Length(ST) < Length(CColor) do
              ST := ST+#$3F;
            Application^.GetPalette^:= Copy(ST, 1, Length(CColor));
            SetSysColors(Application^.GetPalette^);
            LoadPalette := True;
          end;
        LoadIndexes(s);
        P := 0;
        s.Read(P, SizeOf(P));
        if P = $50414756 then{ VGAP }
          begin
            s.Read(VGA_palette, SizeOf(VGA_palette));
            s.Read(P, SizeOf(P));
            if P = $4B4E4C42 then{ BLNK }
              begin
                s.Read(CurrentBlink, SizeOf(CurrentBlink));
                SetBlink(CurrentBlink);
              end;
            LoadVGAPalette := True;
            if StartupData.Load and osuResetPalette <> 0 then
                SetPalette(VGA_palette);

          end;
          { else
      if Msg(dlRestoreVGAPalette, nil, mfYesNoConfirm) = cmYes then
          ResetVGAPalette( True ) ;}
      end;
    s.Done;
    WriteConfig;

    if LoadPalette then
      begin
        DoneMemory;
        Application^.Redraw;
      end;
  end { LoadPalFromFile };
{-DataCompBoy-}

{-DataCompBoy-}
procedure TDNApplication.HandleCommand;

  var
    R: TRect;
    P, P1: PView;
    ST: String;
    FileIsDBF: boolean;

    HFile: PHelpFile;
    HelpStrm: PDosStream;
    t: TEvent;
    CC: word;
    F: lFile;
    PJ, Q: String;
    QQ: integer;
  procedure OpenWindow(ChDrive: boolean);
    var
      s: String;
    begin
      Desktop^.GetExtent(R);
      s[1] := #0;
      if ChDrive then
        begin
          s := SelectDrive(R.A.X+(R.B.X-R.A.X) div 2, R.A.Y, #0,
            False);
          if s = '' then
            exit;
          s[1] := Char(byte(s[1])-64);
        end;
      {$IFDEF TrashCan}
      if TrashCan^.ImVisible then
        Desktop^.Delete(TrashCan);
      {$ENDIF}
      {P := nil;}
      P := New(PXDoubleWindow, Init(R, 0, byte(s[1])));
      if Abort then
        Dispose(P, Done)
      else
        InsertWindow(PWindow(P));
      {$IFDEF TrashCan}
      if TrashCan^.ImVisible then
        Desktop^.Insert(TrashCan);
      {$ENDIF}
    end { OpenWindow };

  procedure OpenTreeWindow;
    var
      s: String;
    begin
      Desktop^.GetExtent(R);
      s := ChangeDir(GetString(dlSelectDirectory), 0);
      if s = '' then
        exit;
      ClrIO;
      lChDir(s);
      ClrIO;
      s[1] := Char(byte(s[1])-64);
      P := New(PXDoubleWindow, Init(R, 0, byte(s[1])));
      Message(P, evCommand, cmDirTree, nil);
      if Abort then
        Dispose(P, Done)
      else
        InsertWindow(PWindow(P));
    end;

  function NewVideoMenu(Items: PMenuItem): PMenu;
    var
      M: PMenu;
    begin
      M := NewMenu(Items);
      while Items <> nil do
        begin
          if Items^.Command-cmSwitch = ScreenMode then
            begin
              M^.Default := Items;
              break;
            end;
          Items := Items^.Next;
        end;
      NewVideoMenu := M;
    end;

  procedure SelectVideoModeDialog;
    var
      R: record
        VM: integer;
        Cols, Rows: String[3];
        end;
      W: word;
      D: PDialog;
    begin
      case ScreenMode of
        sm80x30:
          R.VM := 1;
        sm80x34:
          R.VM := 2;
        sm80x43:
          R.VM := 3;
        sm80x50:
          R.VM := 4;
        sm80x60:
          R.VM := 5;
        $140A..$FFFE:
          R.VM := 6;
        else
          R.VM := 0;
      end {case};
      R.Cols := ItoS(ScreenWidth);
      R.Rows := ItoS(ScreenHeight);

      D := PDialog(LoadResource(dlgChScreenMode));
      D := PDialog(Application^.ValidView(D));
      if D = nil then
        exit;
      D^.SetData(R);
      if opSys = opWin then
        PRadioButtons(D^.Last^.Prev)^.EnableMask := $19;
      W := Desktop^.ExecView(D);
      if W <> cmCancel then
        D^.GetData(R);
      Dispose(D, Done);
      if W <> cmOK then
        exit;

      case R.VM of
        0:
          W := sm80x25;
        1:
          W := sm80x30;
        2:
          W := sm80x34;
        3:
          W := sm80x43;
        4:
          W := sm80x50;
        5:
          W := sm80x60;
        6:
          W := StoI(R.Cols)*256+StoI(R.Rows);
      end {case};
      {PZ begin 2000.06.29}
      SetScrMode(W);
      if StoreVideoMode <> 0 then
        begin
          case StoreVideoMode of
            1:
              smSVGALo := W;
            2:
              smSVGAHi := W;
            else
              StoreVideoMode := 0;
          end {case};
          SystemData.Mode1 := ItoS(smSVGALo);
          SystemData.Mode2 := ItoS(smSVGAHi);
          Message(Application, evCommand, cmUpdateConfig, nil);
        end;
      {PZ end}
    end { SelectVideoModeDialog };

  procedure MessageBoxAbout;
    var
      D: array[1..2] of PString;
    begin
      {$IFDEF DNPRG}
      D[1] := NewStr(versionName);
      D[2] := NewStr(#13+#3+'Compiled '+versionDate);
      MessageBox2(dlAbout, ^C'Based on Dos Navigator'#13+
      +^C'Copyright (C) 1991-99 RIT Research Labs'#13#13+
      +^C'This product is a FREEWARE', @D, nil, mfAbout+mfOKButton);
      DisposeStr(D[1]);
      DisposeStr(D[2]);
      {$ENDIF}
    end;

  {$IFDEF SpreadSheet}
  {-DataCompBoy-}
  procedure LoadSheet(const SheetName: String);
    begin
      Desktop^.GetExtent(R);
      Desktop^.Insert(ValidView(New(PCalcWindow, Init(R, SheetName))));
    end;
  {-DataCompBoy-}

  {-DataCompBoy-}
  procedure OpenSheet;
    var
      FN: String;
    begin
      if GetFileName(FN, '*.WKZ', GetString(dlOpenFile), GetString(
          dlOpenFileName),
        fdOpenButton) = cmFileOpen
      then
        LoadSheet(FN);
    end;
  {-DataCompBoy-}
  {$ENDIF}

  var
    s: TDOSStream;
    FN: String;
    Pal: PString;
    W: word;
    {WW: Word;
     PW: PWindow;}
    vId: longInt;

  procedure StoreColors;
    begin
      FN := GetFileNameDialog(SourceDir+'COLORS\*.PAL', GetString(
        dlStoreColorPal), GetString(dlFileName),
      fdOKButton+fdHelpButton, hsColors);
      if FN = '' then
        exit;
      s.Init(FN, stCreate);
      if s.Status = 0 then
        begin
          Pal := PString(GetPalette);
          s.WriteStr(Pal);
          StoreIndexes(s);
          vId := $50414756; { VGAP }
          s.Write(vId, SizeOf(vId));
          s.Write(VGA_palette, SizeOf(VGA_palette));
          vId := $4B4E4C42; { BLNK }
          s.Write(vId, SizeOf(vId));
          s.Write(CurrentBlink, SizeOf(CurrentBlink));
        end;
      {else PlaySound(1559, 30);}
      s.Done;
      if not DnIni.AutoRefreshPanels then
        RereadDirectory(GetPath(FN));
    end { StoreColors };

  { procedure LoadColors;
 begin
  asm int 3 end;
  FN := GetFileNameDialog(SourceDir+'COLORS\*.PAL', GetString(dlLoadColorPal), GetString(dlFileName),
                          fdOKButton + fdHelpButton, hsColors);
  if FN = '' then Exit;
  LoadPalFromFile(FN);
 end;}

  procedure LoadColors; {JO}
    var
      More: boolean;
      None: boolean;
    begin
      More := True;
      None := False;
      FN := GetFileNameMenu(SourceDir+'COLORS\', '*.PAL', '', True,
        More, None);
      if More then
        FN := GetFileNameDialog(SourceDir+'COLORS\*.PAL', GetString(
          dlLoadColorPal), GetString(dlFileName),
        fdOKButton+fdHelpButton, hsColors);
      if FN = '' then
        exit;
      LoadPalFromFile(FN);
    end; {JO}

  {$IFNDEF Win32}
  function TryRunOS2(s: PString): boolean;
    var
      B: boolean;
      ST: SessionType;
    begin
      TryRunOS2 := True;
      DelLeft(s^);
      DelRight(s^);
      if s^ = '' then
        exit;
      if s^ = '' then
        exit;
      TryRunOS2 := False;
      if not OS2exec then
        exit;
      case s^[1] of
        '>':
          begin
            B := False;
            ST := stOS2FullScreen
          end;
        '<':
          begin
            B := True;
            ST := stOS2FullScreen
          end;
        ']':
          begin
            B := False;
            ST := stOS2Windowed
          end;
        '[':
          begin
            B := True;
            ST := stOS2Windowed
          end;
        else
          if (CmdLine.Str <> '') and (PCommandLine(CommandLine)^.
              LineType in [ltOS2Window, ltOS2FullScreen])
          then
            begin
              s^:= ' '+s^;
              B := ShiftState and 3 <> 0;
              if PCommandLine(CommandLine)^.LineType = ltOS2Window
              then
                ST := stOS2Windowed
              else
                ST := stOS2FullScreen
            end
          else
            exit;
      end {case};
      TryRunOS2 := True;
      RunOS2Command(Copy(s^, 2, MaxStringLength), B, ST);
      CmdLine.StrModified := True;
      Message(CommandLine, evKeyDown, kbDown, nil);
      ClearEvent(Event);
    end { TryRunOS2 };
  {$ENDIF}

  function ExecCommandLine: boolean;
    var
      s, {$IFDEF RecodeWhenDraw}s1, {$ENDIF}SD: String; {AK155}
      up: tUserParams;
    begin
      ExecCommandLine := False;
      {S := '';}
      CommandLine^.GetData(s);

      if (DelSpaces(s) = '') then
        begin
          SD := '';
          CommandLine^.SetData(SD);
          CommandLine^.DrawView; {JO}
          exit;
        end;
      ExecCommandLine := True;
      if HandleChDirCommand then
        begin
          SD := s; {AK155}
          DelLeft(s);
          DelRight(s);
          if (s[1] in ['c', 'C']) and (s[2] in ['d', 'D']) and (s[3] =
              ' ')
          then
            begin
              System.Delete(s, 1, 2); {DelFC(S); DelFC(S);}DelLeft(s);
              s := DelSquashes(s);
              if PathExist(s) then
                begin
                  {Cat} if s[2] = ':' then
                    s[1] := UpCase(s[1]); {/Cat}
                  Message(Application, evBroadcast,
                    cmChangeDirectory, @S);
                end;
              SD := '';
              CommandLine^.SetData(SD);
              CommandLine^.DrawView; {JO}
              exit;
            end;
          if (Length(s) = 2) and ValidDrive(UpCase(s[1])) and (s[2] =
              ':')
          then
            begin
              if s[1] = '*' then
                s := cTEMP_;
              {Cat}s[1] := UpCase(s[1]); {/Cat}
              Message(Desktop, evBroadcast, cmChangeDrv, @S);
              SD := '';
              CommandLine^.SetData(SD);
              CommandLine^.DrawView; {JO}
              exit;
            end;
          s := SD; {AK155 CommandLine^.GetData(S);}
        end;
      {$IFDEF OS_DOS} {AK155}
      if not CheckExit then
        begin
          ClearEvent(Event);
          exit
        end;
      {$ENDIF}
      {$IFNDEF Win32}
      if TryRunOS2(@S) then
        exit;
      {$ENDIF}
      {AK155 очистка комстроки нужна всегда, поэтому она вынесена в общую         }
      {      часть. Очищать лучше перед выполнением, а не после, так как в        }
      {      ExecString она запоминается и восстанавливается. Аналогичная вставка }
      {      сделана в dnexec.ExecFile.RunCommand                                 }
      {JO:  перенёс очистку комстроки после TryRunOS2, так как иначе оно но срабатывает, и при }
      {     запуске набранного в коммандлайне не получается запустить в отдельной сессии       }
      SD := '';
      CommandLine^.SetData(SD);
      CommandLine^.DrawView;
      {AK155}
      Message(Desktop, evBroadcast, cmGetCurrentPosFiles, nil);
      {$IFDEF RecodeWhenDraw}
      s1 := OemToCharStr(s);
      ExecString(@S1, #13+CharToOemStr(ActiveDir)+'>'+s);
      {$ELSE}
      ExecString(@S, #13+ActiveDir+'>'+s);
      {$ENDIF}
      {AK155     S:='';CommandLine^.SetData(S);CommandLine^.DrawView;}
    end { ExecCommandLine: };

  procedure ExecDOSCmd;
    var
      Nm: String;
      XT: String;
    begin
      ST := '';
      Message(Desktop, evBroadcast, cmGetCurrentPosFiles, nil);
      lFSplit(CnvString(CurFileActive), FreeStr, Nm, XT);
      if InExtFilter(CnvString(CurFileActive), Executables) then
        ST := Nm+XT+' ';
      if InputBox(GetString(dlExecDOScmdHdr), GetString(
          dlExecDOScmdLine), ST, 128, hsExecDOSCmd) <> cmOK
      then
        exit;
      {$IFNDEF Win32}
      if not TryRunOS2(@ST) {$IFDEF OS_DOS} and CheckExit {$ENDIF}
          {JO}
      then
        {$ENDIF}
        ExecString(@St, #13#10+ActiveDir+'>'+ST);
      DisposeStr(CurFileActive);
      CurFileActive := nil;
      DisposeStr(CurFilePassive);
      CurFilePassive := nil;
    end { ExecDOSCmd };

  procedure GetUserScreen;
    var
      P: PView;
      PP: PView;
    begin
      PP := nil;
      P := ViewPresent(cmShowOutput, @PP);
      if PP <> nil then
        PP^.Select
      else
        Desktop^.Insert(New(PUserWindow, Init));
    end;

  procedure Denie;
    begin
      ExecResource(dlgDenie, FreeStr);
    end;

  procedure Rebound;
    begin
      GetExtent(R);
      if InterfaceData.Options and ouiHideMenu = 0 then
        Inc(R.A.Y);
      if SystemData.Options and ouiHideStatus = 0 then
        Dec(R.B.Y);
      Dec(R.B.Y);
      Desktop^.Locate(R);
      R.A.Y := R.B.Y;
      Inc(R.B.Y);
      CommandLine^.Locate(R);
    end;

  procedure RSearchAdvance;
    begin
      if ExecResource(dlgAdvanceSearch, AdvanceSearchData) = cmOK
      then
        begin
          PView(Desktop^.Current {Event.InfoPtr})^.GetData(FindRec);
          FindRec.Options := FindRec.Options or ffoAdvanced;
          PView(Desktop^.Current {Event.InfoPtr})^.SetData(FindRec);
        end;
      ClearEvent(Event);
    end;

  {Cat: переписал для поддержки длинных строк в Clipboard-е}
  procedure GetFromClip;
    begin
      if SystemData.Options and ossUseSysClip <> 0 then
        SyncClipOut {(true)};
      if (Microed.ClipBoard = nil) or (Microed.ClipBoard^.At(0) =
          nil)
      then
        PString(Event.InfoPtr)^:= ''
      else
        PString(Event.InfoPtr)^:= PLongString(Microed.ClipBoard^.At(0))^
          ;
    end;

  procedure GetFromClipLong;
    begin
      if SystemData.Options and ossUseSysClip <> 0 then
        SyncClipOut {(true)};
      if (Microed.ClipBoard = nil) or (Microed.ClipBoard^.At(0) =
          nil)
      then
        PLongString(Event.InfoPtr)^:= ''
      else
        PLongString(Event.InfoPtr)^:= PLongString(Microed.ClipBoard^.At(
          0))^;
    end;

  procedure PutInClip;
    begin
      if Microed.ClipBoard <> nil then
        Dispose(Microed.ClipBoard, Done);
      Microed.ClipBoard := New(PLineCollection, Init(1, 1, True));
      Microed.ClipBoard^.Insert(NewLongStr(PString(Event.InfoPtr)^));
      if SystemData.Options and ossUseSysClip <> 0 then
        SyncClipIn;
      if ClipBoardStream <> nil then
        ClipBoardStream^.Seek(Max(ClipBoardStream^.GetPos-4, 0));
      CopyLines2Stream(Microed.ClipBoard, ClipBoardStream);
    end;

  procedure PutInClipLong;
    begin
      if Microed.ClipBoard <> nil then
        Dispose(Microed.ClipBoard, Done);
      Microed.ClipBoard := New(PLineCollection, Init(1, 1, True));
      Microed.ClipBoard^.Insert(NewLongStr(PLongString(Event.InfoPtr)^))
        ;
      if SystemData.Options and ossUseSysClip <> 0 then
        SyncClipIn;
      if ClipBoardStream <> nil then
        ClipBoardStream^.Seek(Max(ClipBoardStream^.GetPos-4, 0));
      CopyLines2Stream(Microed.ClipBoard, ClipBoardStream);
    end;
  {/Cat}

  procedure DoQuickChange;
    var
      R: TRect;
      P: PMenuBox;
      Menu: PMenu;
      Items: PMenuItem;
      C: Char;
      n, Q, j: integer;
    begin
      Items := nil;
      Q := 0;
      j := 0;
      for n := 8 downto 0 do
        if DirsToChange[n] <> nil then
          begin
            FreeStr := '~'+ItoS(n+1)+'~ '+CutH(DirsToChange[n]^, 40);
            Items := NewItem(FreeStr, 'Alt-'+ItoS(n+1), kbAlt1,
              cmQuickChange1+n,
            hcNoContext, Items);
            j := Max(CStrLen(FreeStr), j);
            Inc(Q);
          end;
      if Items = nil then
        begin
          Msg(erNoQuickDirs, nil, mfWarning+mfCancelButton);
          exit;
        end;
      R.Assign(Application^.Size.X div 2-j div 2,
      Application^.Size.Y div 2-Q div 2,
      Application^.Size.X div 2+j div 2+j mod 2,
      Application^.Size.Y div 2+Q div 2+Q mod 2);
      Menu := NewMenu(Items);
      P := New(PMenuBox, Init(R, Menu, nil));
      P^.Options := P^.Options or ofCentered;
      P^.HelpCtx := hcQuickDirs;

      n := Application^.ExecView(P);
      Dispose(P, Done);
      DisposeMenu(Menu);
      if n >= cmQuickChange1 then
        Message(Desktop, evCommand, n, nil);
    end { DoQuickChange };

  procedure ExecTree;
    type
      TGR = record
        s: String;
        Dummy: array[0..32767] of byte;
        end;
    var
      GR: ^TGR;
    begin
      New(GR);
      Desktop^.Current^.GetData(GR^);
      {GR^.S := lFExpand(GR^.S);}
      GR^.s := ChangeDir(GetString(dlChooseDir), 0);
      if GR^.s <> '' then
        Desktop^.Current^.SetData(GR^.s);
      Dispose(GR);
      ClearEvent(Event);
    end;

  procedure SearchAdvance;
    begin
      RSearchAdvance
    end;

  procedure ToggleCmdLine;
    var
      R: TRect;
      B: boolean;
    begin
      if CommandLine = nil then
        exit;
      B := (not CommandLine^.GetState(sfVisible)) and (Str <> '');
      GetExtent(R);
      if InterfaceData.Options and ouiHideMenu = 0 then
        Inc(R.A.Y);
      if InterfaceData.Options and ouiHideStatus = 0 then
        Dec(R.B.Y);
      if B then
        Dec(R.B.Y);
      Desktop^.Locate(R);
      R.A.Y := R.B.Y;
      R.B.Y := R.A.Y+byte(B);
      CommandLine^.Locate(R);
      CommandLine^.SetState(sfVisible, B);
    end { ToggleCmdLine };

  procedure CE;
    begin
      ClearEvent(Event)
    end;

  procedure DoExecFile;
    begin
      if
        {$IFNDEF Win32}
        not TryRunOS2(PString(Event.InfoPtr)) {$IFDEF OS_DOS} and
          CheckExit {$ENDIF} {JO}
        and
        {$ENDIF}
        not ExecCommandLine
      then
        ExecFile(PString(Event.InfoPtr)^);
      CE;
    end;

  procedure DoExecString;
    begin
      {$IFNDEF Win32}
      if not TryRunOS2(PString(Event.InfoPtr)) {$IFDEF OS_DOS} and
          CheckExit {$ENDIF} {JO}
      then
        {$ENDIF}
        ExecString(PString(Event.InfoPtr), '');
      CE;
    end;

  procedure ChLngId;
    var
      s: String;
      SS: String[9];
      l: TStringCollection;
      SR: lSearchRec;
      Current, Default: PMenuItem;
      Menu: PMenu;
      CurIdx, i: integer;
      HMB: PMenuBox;
      R: TRect;
      V: PView;
    function LngMixCase(P: String): String;
      var
        i: integer;
      begin
        for i := 1 to Length(P) do
          if (i = 1) or not (P[i-1] in ['a'..'z',
            'A'..'Z'])
          then
            P[i] := UpCase(P[i])
          else if P[i] in ['A'..'Z']
          then
            P[i] := Chr(Ord(P[i])+Ord('a')-Ord('A'));
        LngMixCase := P
      end;
    begin { ChLngId }
      if (Confirms and cfChaneLngWarning <> 0) then
        Msg(dlChangeLngWarning, nil, mfWarning or mfOKButton);
      l.Init(5, 5, False);
      s := SourceDir;
      if s[Length(s)] <> '\' then
        s := s+'\';
      lFindFirst(s+'*.LNG', AnyFile, SR);
      while DOSError = 0 do
        begin
          if ((SR.SR.Attr and Directory) = 0) and (SR.FullSize > 0)
          then
            begin
              SS[0] := #0;
              while SR.FullName[byte(SS[0])+1] <> '.'
              do
                SS := SS+SR.FullName[byte(SS[0])+1];
              SS := LngMixCase(SS);
              if ValidLngId(SS, False) then
                l.Insert(NewStr(SS))
            end;
          lFindNext(SR)
        end;
      lFindClose(SR);
      s := StartupDir;
      if s[Length(s)] <> '\' then
        s := s+'\';
      lFindFirst(s+'*.LNG', AnyFile, SR);
      while DOSError = 0 do
        begin
          if ((SR.SR.Attr and Directory) = 0) and (SR.FullSize > 0)
          then
            begin
              SS[0] := #0;
              while SR.FullName[byte(SS[0])+1] <> '.'
              do
                SS := SS+SR.FullName[byte(SS[0])+1];
              SS := LngMixCase(SS);
              if ValidLngId(SS, False) and (l.IndexOf(@SS) = -1)
              then
                l.Insert(NewStr(SS));
            end;
          lFindNext(SR)
        end;
      lFindClose(SR);
      s := LngMixCase(LngId);
      CurIdx := l.IndexOf(@S);
      if CurIdx = -1 then
        begin
          s := 'ENGLISH';
          CurIdx := l.IndexOf(@S)
        end;
      if CurIdx = -1 then
        CurIdx := 0;
      if l.Count > 0 then
        begin
          if ShowLanguageMenu then
            begin
              Current := nil;
              for i := l.Count-1 downto 0 do
                begin
                  if i = 9 then
                    s := '~0~ '
                  else if i < 9 then
                    s := '~'+Chr(Ord('1')+i)+'~ '
                  else
                    s := '  ';
                  Current := NewItem(s+CutH(PString(l.At(i))^, 40), '',
                    kbNoKey,
                  cmCancel+1+i, hcChLngId, Current);
                  if i = CurIdx then
                    Default := Current
                end;
              Menu := NewMenu(Current);
              Menu^.Default := Default;
              Desktop^.GetExtent(R);
              R.A.X := ((R.A.X+R.B.X) div 2)-8;
              R.B.X := R.A.X+16;
              R.A.Y := ((R.A.Y+R.B.Y-l.Count) div 2)-1;
              R.B.Y := R.A.Y+l.Count+2;
              if R.A.Y < Desktop^.Origin.Y+1 then
                R.A.Y := Desktop^.Origin.Y+1;
              if R.B.Y > Desktop^.Origin.Y+Desktop^.Size.Y-1
              then
                R.B.Y := Desktop^.Origin.Y+Desktop^.Size.Y-1;
              New(HMB, Init(R, Menu, nil));
              CurIdx := Desktop^.ExecView(HMB)-cmCancel-1;
              Dispose(HMB, Done);
              DisposeMenu(Menu)
            end
          else
            CurIdx := (CurIdx+1) mod l.Count;
          if (CurIdx >= 0) and (PString(l.At(CurIdx))^ <>
            LngMixCase(ActiveLanguage)) {$IFDEF OS_DOS} and
              CheckExit {$ENDIF} {JO}
          then
            begin
              ActiveLanguage := PString(l.At(CurIdx))^;
              if ActiveLanguage = LngMixCase(GetEnv('DNLNG'))
              then
                ActiveLanguage := '';
              SaveDnIniSettings(@ActiveLanguage);
              DoneIniEngine;
              ProbeINI(INIstoredtime, INIstoredsize, INIstoredcrc);
              ConfigModified := True;
            end;
        end;
      l.FreeAll;
      l.Done;
    end { ChLngId };

  function GetGLUKName(s: String): String;
    begin
      GetGLUKName := GetPath(s)+Copy(GetName(s), 1, 12);
    end;

  function GetfURZ2(const s: String): String;
    var
      A, aa, aaa: String;
    begin
      lFSplit(s, A, aa, aaa);
      aa := aa+aaa;
      GetfURZ2 := A+Copy(aa, 1, 8)+'.'+Copy(aaa, 9, 3);
    end;

  begin { TDNApplication.HandleCommand }
    case Event.Command of
      cmFirstTimePanel:
        OpenWindow(False);
      cmGetFromClipboard:
        GetFromClip;
      cmGetFromClipboardLong:
        GetFromClipLong;
      cmPutInClipboard:
        PutInClip;
      cmPutInClipboardLong:
        PutInClipLong;
      cmSearchAdvance:
        SearchAdvance;
      cmListOfDirs:
        DoQuickChange;
      {$IFDEF Printer}
      cmFilePrint:
        PrintFile(PString(Event.InfoPtr)^);
      {$ENDIF}
      {$IFDEF PrintManager}
      cmSetupPrinter:
        SetupPrinter;
      {$ENDIF}
      cmOpenSmartpad:
        OpenSmartpad;
      cmOpenClipBoard:
        OpenClipBoard; {-$VOL}
      cmExecGrabber:
        ScreenGrabber(True);
      {$IFDEF MODEM}
      cmTerminalDefaults:
        if TerminalSetup and (Term <> nil) then
          Move(TerminalDefaults, Term^.Emulator, 4);
      {$ENDIF}
      cmSetupConfirmation:
        ConfirmSetup;
      cmCountrySetup:
        SetupCountryInfo;
      cmColumnDefaultsDisk:
        if SetupColumnDefaultsDisk then
          begin
            WriteConfig;
            Application^.Redraw;
          end;
      cmColumnDefaultsTemp:
        if SetupColumnDefaultsTemp then
          begin
            WriteConfig;
            Application^.Redraw;
          end;
      cmColumnDefaultsFind:
        if SetupColumnDefaultsFind then
          begin
            WriteConfig;
            Application^.Redraw;
          end;
      cmColumnDefaultsArch:
        if SetupColumnDefaultsArch then
          begin
            WriteConfig;
            Application^.Redraw;
          end;
      cmColumnDefaultsArvd:
        if SetupColumnDefaultsArvd then
          begin
            WriteConfig;
            Application^.Redraw;
          end;
      cmHighlightGroups:
        SetHighlightGroups;
      {$IFDEF DBView}
      cmDBFView:
        InsertWindow(New(PDBWindow, Init(CnvString(Event.InfoPtr),
          FileIsDBF)));
      {$ENDIF}
      {$IFDEF SpreadSheet}
      cmNewTable:
        LoadSheet('');
      cmSheetLoad:
        OpenSheet;
      cmWKZView:
        begin
          Desktop^.GetExtent(R);
          InsertWindow(New(PCalcWindow, Init(R, CnvString(Event.
            InfoPtr))));
        end;
      {$ENDIF}
      {$IFDEF PHONES}
      cmPhoneBook:
        PhoneBook(False);
      {$ENDIF}
      {$IFDEF Modem}
      {$IFDEF LINK}
      cmNavyLink:
        StartLink;
      {$ENDIF}
      {$IFDEF PHONES}
      cmUndial:
        PhoneBook(True);
      {$ENDIF}
      cmSetupModem:
        SetupModem;
      cmTerminal:
        OpenTerminal;
      cmHangUp:
        if COMPort <> nil then
          begin
            P := WriteMsg(GetString(dlDisconnect));
            HangUp;
            {$IFDEF PHONES}
            if Dialer <> nil then
              Dialer^.Free;
            {$ENDIF}
            if Term <> nil then
              Term^.Owner^.Redraw;
            if P <> nil then
              P^.Free;
          end;
      {$ENDIF}
      cmTextView, cmHexView:
        begin
          ST := CnvString(Event.InfoPtr);
          W := PosChar('|', ST);
          if W > 0
          then
            InsertWindow(New(PFileWindow,
            Init(Copy(ST, 1, W-1),
            Copy(ST, W+1, MaxStringLength),
            Event.Command = cmHexView)))
          else
            InsertWindow(New(PFileWindow,
            Init(ST, ST,
            Event.Command = cmHexView)))
        end;
      {$IFDEF MODEM}
      cmAdvancePortSetup:
        if ExecResource(dlgAdvancedCOMSetup, AdvModemData) = cmOK
        then
          begin
            StoreModemSetup;
          end;
      {$ENDIF}
      cmSystemSetup:
        SystemSetup;
      cmRSearchAdvance:
        RSearchAdvance;
      cmInterfaceSetup:
        InterfaceSetup;
      cmStartup:
        StartupSetup;
      cmSetupMouse:
        MouseSetup;
      {$IFDEF SS}
      cmSaversSetup:
        SaversSetup;
      {$ENDIF}
      {$IFDEF TrashCan}
      cmHideShowTools:
        if TrashCan^.ImVisible
        then
          begin
            TrashCan^.Hide;
            TrashCan^.ImVisible := False;
          end
        else
          begin
            TrashCan^.Show;
            TrashCan^.MakeFirst;
            TrashCan^.ImVisible := True;
          end;
      {$ENDIF}
      cmEditHistory:
        EditHistoryMenu;
      cmViewHistory:
        ViewHistoryMenu;
      {$IFDEF OS2}
      cmSetVolumeLabel:
        SetVLabel;
      {$ENDIF}
      cmXEditFile:
        OpenEditor;
      cmWindowManager:
        WindowManager;
      cmUserMenu, cmGlobalUserMenu:
        if (ExecUserMenu(Event.Command = cmGlobalUserMenu)) then
          begin
            ST := FreeStr;
            {$IFDEF OS_DOS} if CheckExit then{$ENDIF}{JO}
              begin
                ST := SwpDir+'$DN'+ItoS(DNNumber)+'$'+CmdExt+' '+ST;
                ExecString(@ST, '');
              end;
          end;
      {$IFNDEF MINARCH}
      {$IFNDEF OS2}
      cmReadArchive:
        begin
          ReadArcList;
        end;
      {$ENDIF}
      {$ENDIF}
      {$IFDEF SS}
      cmScreenRest:
        InsertIdler;
      {$ENDIF}
      cmChScreenMode:
        SelectVideoModeDialog;
      cmASCIITable:
        ASCIITable;
      cmAbout:
        begin
          ClearEvent(Event);
          MessageBoxAbout;
          exit;
        end;
      cmCalculator:
        InsertCalc;
      {$IFDEF Calendar}
      cmCalendar:
        InsertCalendar; {JO}
      {$ENDIF}
      {$IFDEF Tasklist}
      cmTasklist:
        InsertTaskList; {JO}
      {$ENDIF}
      cmRefresh:
        Redraw;
      cmClearDesktop:
        GlobalMessage(evCommand, cmClose, nil);
      cmFileView:
        ViewFile(False, False, CnvString(Event.InfoPtr));
      cmFileEdit:
        EditFile(True, CnvString(Event.InfoPtr));
      {cmViewFilter: ViewFile(true, true, CnvString(Event.InfoPtr));}
      cmViewFilter:
        ViewFile(False, True, CnvString(Event.InfoPtr));
        {JO для распознания фильтрованных файлов как архивов}
      cmIntFileView:
        ViewFile(True, False, CnvString(Event.InfoPtr));
      cmIntFileEdit:
        EditFile(False, CnvString(Event.InfoPtr));
      cmEditQuickRun:
        EditFile(True, Copy(SourceDir, 1, byte(ShiftState and 3 = 0)*
          MaxStringLength)+'dn.xrn');
      cmExtFileEdit:
        EditFile(True, Copy(SourceDir, 1, byte(ShiftState and 3 = 0)*
          MaxStringLength)+'dn.ext');
      cmMenuFileEdit:
        EditFile(True, SourceDir+'dn.mnu');
      cmLocalMenuFileEdit:
        EditFile(True, 'dn.mnu');
      cmEditHGL:
        EditFile(True, SourceDir+'dn.hgl');
      cmEditSPF:
        EditFile(True, SourceDir+'dn.spf');
      cmEditINI:
        EditFile(True, SourceDir+'dn.ini');
      cmExternalViewers:
        EditFile(True, Copy(SourceDir, 1, byte(ShiftState and 3 = 0)*
          MaxStringLength)+'dn.vwr');
      cmExternalEditors:
        EditFile(True, Copy(SourceDir, 1, byte(ShiftState and 3 = 0)*
          MaxStringLength)+'dn.edt');
      cmShowUserScreen:
        ShowUserScreen;
      {$IFDEF OS_DOS}
      {-DataCompBoy-}
      cmRestart:
        if not CheckExit
        then
          begin
            ClearEvent(Event);
            exit
          end
        else
          begin
            StringCache^.FreeAll;
            ExecString(@NullStr, '');
          end;
      {-DataCompBoy-}
      {$ENDIF}
      cmCreatePanel:
        OpenWindow(True);
      cmCreateTree:
        OpenTreeWindow;
      cmFormatDisk:
        AddFormat;
      cmFindFile:
        FindFile;
      cmShowOutput:
        GetUserScreen;
      cmHistoryList:
        CmdHistory;
      cmLoadDesk:
        RetrieveDesktop(SourceDir+'DN'+GetEnv('DNDSK')+'.DSK', nil,
          True);
      cmRetrieveSwp:
          {begin
                     LoadStream := PresentFile(SwpDir+'DN'+ItoS(DNNumber)+'.SWP');
                     if LoadStream <> nil then RetrieveDesktop('', LoadStream, True);
                     If ExistFile(SwpDir+'DN'+ItoS(DNNumber)+'.SWP') then EraseByName(SwpDir+'DN'+ItoS(DNNumber)+'.SWP');
                    end;}
        if PosChar(TempFileSWP[1], '+-=<>|[!') = 0 then
          begin
            lAssignFile(F, TempFileSWP);
            lSetFAttr(F, $20);
            lEraseFile(F);
          end
        else
          begin
            CC := 0;
            PJ := TempFileSWP;
            TempFile := Copy(PJ, 2, MaxStringLength);
            QQ := Pos('|', TempFile);
            Dec(QQ);
            if QQ > 0 then
              Q := Copy(TempFile, QQ+2, MaxStringLength)
            else
              Q := '';
            QQ := QQ and 255; {BP bugfix by piwamoto}
            if not ExistFile(TempFile) then
              TempFile := GetfURZ(Copy(PJ, 2, QQ));
            if not ExistFile(TempFile) then
              TempFile := GetfURZ2(Copy(PJ, 2, QQ));
            if not ExistFile(TempFile) then
              TempFile := GetGLUKName(Copy(PJ, 2, QQ));
            if not ExistFile(TempFile) then
              TempFile := Copy(PJ, 2, QQ);
            case PJ[1] of
              '-':
                CC := cmIntFileView;
              '=':
                CC := cmDBFView;
              '>':
                CC := cmWKZView;
              '<':
                CC := cmTextView;
              '|':
                CC := cmHexView;
              '[':
                CC := cmReadArchive;
              '!':
                CC := cmViewFilter;
              else
                CC := cmFileView;
            end {case};
            if CC <> 0 then
              begin
                if Q <> '' then
                  TempFile := TempFile+'|'+Q;
                Event.What := evCommand;
                Event.Command := CC;
                Event.InfoPtr := @TempFile;
                PutEvent(Event);
              end;
            RunMenu := False;
          end;
      cmSaveDesk:
        SaveRealDsk;
      cmExecFile:
        DoExecFile;
      cmExecString:
        DoExecString;
      cmExecCommandLine:
        if ExecCommandLine then
          CE;
      cmShowTimeInfo:
        ShowTimeInfo; {DataCompBoy}
      cmChangeColors:
        ChgColors;
      cmChangeUserMode1:
        if ScreenMode <> smSVGALo then
          SetScrMode(smSVGALo)
        else
          SetScrMode(sm80x25);
      cmChangeUserMode2:
        if ScreenMode <> smSVGAHi then
          SetScrMode(smSVGAHi)
        else
          SetScrMode(sm80x25);
      cmChangeMode:
        begin
          case ScreenMode of
            3, sm80x25:
              if VideoType = vtEGA then
                SetScrMode(sm80x43)
              else
                SetScrMode(sm80x50);
            else
              SetScrMode(sm80x25);
          end {case};
        end;

      cmLoadColors:
        LoadColors;
      cmSetupArchive:
        SetupArchive(DefaultArchiver);
      cmUpdateArcFile:
        UpdateARH(nil);
      cmLoConfigArchiver..cmHiConfigArchiver:
        SetupArchive(Event.Command);
      cmFMDefaults:
        FMDefaults;
      cmFMSetup:
        FMSetup;
      cmDriveInfoSetup:
        DriveInfoSetup;
      cmExecuteDOScmd:
        ExecDOSCmd;
      cmEditorDefaults:
        SetupEditorDefaults;
      cmStoreColors:
        StoreColors;
      {$IFDEF Game}
      cmGame:
        if Game = nil then
          InsertWindow(New(PGameWindow, Init))
        else
          Game^.Owner^.Select;
      {$ENDIF}
      {From  GetEvent}

      cmHideCmdLine:
        ToggleCmdLine;

      cmGetTeam:
        if Desktop^.TopView^.HelpCtx = hcAboutDialog then
          begin
            Desktop^.TopView^.GetExtent(R);
            R.Grow(-1, -2);
            Dec(R.B.Y, 2);
            New(TeamView, Init(R));
            PGroup(Desktop^.TopView)^.Insert(TeamView);
            Desktop^.TopView^.HelpCtx := hcTeam;
          end;
      cmQuit:
        begin
          if Confirms and cfExitConfirm <> 0 then
            W := Msg(dlQueryExit, nil, mfYesNoConfirm)
          else
            W := cmYes;
          if (W <> cmYes) or not CheckExit then
            begin
              {$IFDEF OS_DOS}
              if w95locked then
                w95QuitCancel; {Gimly}
              {$ENDIF}
              ClearEvent(Event);
            end;
        end;
      cmGetCmpNfo:
        begin
          ClearEvent(Event);
          Event.InfoPtr := WriteMsg(GetString(dlComparing));
        end;
      cmNewStrColl:
        begin
          ClearEvent(Event);
          Event.InfoPtr := New(PStringCollection, Init($80, $40,
            False));
        end;
      cmHelp, cmHelp2:
        begin
          ClearEvent(Event);
          if (not HelpInUse) or (HelpWnd = nil) then
            begin
              HelpInUse := True;
              HelpStrm := New(PDosStream,
              Init(SourceDir+HelpLngId+'.HLP', stOpenRead
                {stOpenPacked})
              );
              if HelpStrm^.Status <> stOK then
                begin
                  Dispose(HelpStrm, Done);
                  HelpStrm := New(PDosStream,
                  Init(StartupDir+HelpLngId+'.HLP', stOpenRead
                    {stOpenPacked})
                  );
                end;
              HFile := New(PHelpFile, Init(HelpStrm));
              if HelpStrm^.Status <> stOK then
                begin
                  Msg(erCantOpenHelp, nil, mfError+mfOKButton);
                  Dispose(HFile, Done);
                  HFile := nil;
                  HelpStrm := nil;
                end
              else
                begin
                  if Event.Command = cmHelp
                  then
                    HelpWnd := PHelpWindow(ValidView(New(PHelpWindow,
                      Init(HFile, GetHelpCtx))))
                  else
                    HelpWnd := PHelpWindow(ValidView(New(PHelpWindow,
                      Init(HFile, word(Event.InfoWord)))));
                  if HelpWnd <> nil then
                    begin
                      if ((Desktop^.Current <> nil)
                        and (
                        (Desktop^.Current^.GetState(sfModal)) or
                        (Desktop^.Current^.GetState(sfDragging))
                        )
                        ) or MenuActive
                      then
                        ExecView(HelpWnd)
                      else
                        begin
                          HelpWnd^.SetState(sfModal, False);
                          HelpWnd^.Options := HelpWnd^.Options or
                            ofTileable or ofTopSelect;
                          Application^.InsertWindow(HelpWnd);
                          exit;
                        end;
                    end;
                end;
              HelpWnd^.Free;
              HelpInUse := False;
            end
          else
            begin
              if Event.Command = cmHelp2 then
                HelpWnd^.GotoContext(word(Event.InfoWord));
              if ((Desktop^.Current <> nil)
                and (Desktop^.Current^.GetState(sfModal))
                ) or MenuActive
              then
                begin
                  HelpWnd^.SetState(sfModal, True);
                  P := HelpWnd^.Next;
                  P1 := Desktop^.Current;
                  Desktop^.Delete(HelpWnd);
                  ExecView(HelpWnd);
                  Desktop^.InsertBefore(HelpWnd, P);
                  Desktop^.SetCurrent(P1, EnterSelect);
                end
              else
                HelpWnd^.Focus;
            end;
        end;
      cmTree:
        ExecTree;
      cmClearData:
        begin
          FillChar(FreeStr, SizeOf(FreeStr), 0);
          PView(Event.InfoPtr)^.Owner^.SetData(FreeStr);
          ClearEvent(Event);
        end;
      cmChLngId:
        ChLngId;
    end {case};
  end { TDNApplication.HandleCommand };
{-DataCompBoy-}

procedure ShowTimeInfo;
  begin
    FreeStr := SStr(DDTimer div 360000, 2, '0');
    AddStr(FreeStr, ':');
    DDTimer := DDTimer mod 360000;
    Insert(SStr(DDTimer div 6000, 2, '0'), FreeStr, Length(FreeStr)+1);
    AddStr(FreeStr, ':');
    DDTimer := DDTimer mod 6000;
    Insert(SStr(DDTimer div 100, 2, '0'), FreeStr, Length(FreeStr)+1);
    AddStr(FreeStr, '.');
    Insert(SStr(DDTimer mod 100, 2, '0'), FreeStr, Length(FreeStr)+1);
    MessageBox(GetString(dlElapsedTime)+FreeStr, nil, mfOKButton+
      mfInformation);
    DDTimer := 0; {JO}
  end;

procedure SetSysColors(var Pal);
  type
    PLL = array[0..255] of byte;
  begin
    {!!!
       move(Pll(Pal)[224],ShadowAttr,1);
       move(Pll(Pal)[225],SysColorAttr,2);
       move(Pll(Pal)[227],SysColorButtonAttr,2);
     }
  end;

{$IFDEF OS_DOS}
{Gimly}
const
  w95QuitEnabled: byte = 1;

procedure w95QuitInit; assembler;
asm
   MOV AL, OpSys {Check Win95}
   AND AL, opWin
   JNZ @@EnableEvent

   MOV AL, OpSys {Check WinNT and W2K}
   AND AL, opWNT
   JNZ @@EnableEvent

   XOR AL, AL
   MOV w95QuitEnabled, AL
   JMP @@E

@@EnableEvent:
{ Enablind close event handling... }
   mov ax, 168fh
   mov dx, 1
   int 2fh

@@E:
end
  ;

function w95QuitCheck: boolean; assembler;
asm
   MOV AL, w95QuitEnabled
   OR  AL, AL
   JZ  @@No

{Checking DN.INI setting...}
   MOV AL, SmartWindowsBoxClose
   OR  AL, AL
   JZ  @@No

{ Checking the close button }
   MOV AX, 168Fh
   MOV CX, AX
   MOV DX, 100h
   INT 2fh
   CMP AX, CX
   JZ  @@No

{ Pressed - preventing the Windows message from appearing }
   MOV AX, 168Fh
   MOV DX, 200h
   INT 2fh

   MOV AL, 1
   MOV w95locked, AL

{ Just disabling close ;) }
   MOV AX, 168Fh
   MOV DX, 0h
   INT 2fh

   MOV AX, 1 { "X" button pressed }
   JMP @@E
@@No:
   XOR AX, AX
@@E:;
end
  ;

procedure w95QuitCancel;
  begin
    asm
{ Enabling the cancel button ;) }
   mov ax, 168Fh
   mov dx, 1
   int 2fh

{ Cancelling close }
   mov ax, 168Fh
   mov dx, 300h
   int 2fh
  end
      ;
    w95locked := False;
  end;
{Gimly}
{$ENDIF}

procedure ExecDNAutoexec;
  var
    i: integer;
    s: String;
  begin
    if RunFirst then
      begin
        i := FindParam('/E');
        if i > 0 then
          begin
            s := Copy(ParamStr(i), 3, MaxStringLength);
            ExecString(@S, '');
          end
      end;
  end;

procedure ClearSelection(AFP: Pointer; FC: Pointer);

  procedure UnSelect(P: PFileRec);
    stdcall;
    begin
      if P^.Selected then
        Dec(PFilePanelRoot(AFP)^.SelNum);
      P^.Selected := False;
    end;

  begin
    PFilesCollection(FC)^.ForEach(@UnSelect);
    with PFilePanelRoot(AFP)^ do
      begin
        DrawView;
        if InfoView <> nil then
          InfoView^.DrawView;
      end;
  end;

end.
