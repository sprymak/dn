unit Vars;
(******

Access to global variables from Win32 plugins
Written by Cat 2:5030/1326.13
(for use in DN/2 Plugin Version)

******)


{$I STDEFINE.INC}

interface

uses
  Dos, Objects, Collect, FilesCol, RStrings, Advance, Views, Dialogs, Menus,
  DnApp, Archiver, XDblWnd, DnIni, Startup, Plugin;


{Global variables}

function FileMode: PLongInt;
function DosError: Integer;
function Application: PProgram;
function Desktop: PDesktop;
function StatusLine: PStatusLine;
function MenuBar: PMenuView;
function LngStream: PStream;
function ResourceStream: PStream;
function LStringList: PStringList;
function Resource: PIdxResource;
function EventCatchers: PEventCatcherArray;
procedure AddEventCatchers;
function EventCatchersCount: SmallWord;
procedure SetEventCatchersCount(EventCatchersCount_: SmallWord);
function ArchiveViewers: PArchiveViewerArray;
function StartupDir: String;
function SourceDir: String;
function TempDir: String;
function ArcFile: PStream;
function FileInfo: PFInfo;
function ArcFileName: String;
function VArcFileName: String;


{Create some objects}

function NewCollection(A1, A2: LongInt): PCollection;
function NewStringCollection(A1, A2: LongInt; LS: Boolean): PStringCollection;
function NewFilesCollection(A1, A2: LongInt): PFilesCollection;
function NewDosStream(FileName: FNameStr; Mode: Word): PDosStream;
function NewBufStream(FileName: FNameStr; Mode: Word; Size: Word): PBufStream;
function NewView(X1, Y1, X2, Y2: SmallWord): PView;
function NewWindow(X1, Y1, X2, Y2: SmallWord; Title: String; Number: SmallWord): PWindow;
function NewButton(X1, Y1, X2, Y2: SmallWord; Title: String; cm_, bf_: SmallWord): PButton;
function NewLabel(X1, Y1, X2, Y2: SmallWord; S: String; P:PView): PLabel;
function NewStaticText(X1, Y1, X2, Y2: SmallWord; S: String): PStaticText;
function NewScrollBar(X1, Y1, X2, Y2: SmallWord): PScrollBar;


{TypeOf}

function TypeOf_TXDoubleWindow(P: PObject): Boolean;


{INI variables}

type
  PIniVars = ^TIniVars;
  TIniVars = record
    {Interface}
    CutDriveInfo             : Boolean;
    WinManagerSelectNext     : Boolean;
    DriveSelectVCenter       : Boolean;
    SystemMenuChar           : Byte;
    HorizScrollBarChars      : String[6];
    VertScrollBarChars       : String[6];
    FadeDelay                : LongInt;
    ReflectCopyDirection     : Boolean;
    ReuseViewers             : Byte;
    ReuseEditors             : Byte;
    HistoryErrorBeep         : Boolean;
    PreserveMenuPositions    : Boolean;
    LFNinBottom              : Boolean;
//    DescriptionInBottom      : Boolean;
    PanelDescrArvid          : String[255];
    PanelDescrArc            : String[255];
    PanelDescrTemp           : String[255];
    PanelDescrFind           : String[255];
    PanelDescrDrive          : String[255];
    UseEnterInViewer         : Byte;
    SkipXLatMenu             : Boolean;
    {Clock}
    ShowSeconds              : Boolean;
    ShowCentury              : Boolean;
    ShowDayOfWeek            : Boolean;
    DaysOfWeek               : String[23];
    RightAlignClock          : Boolean;
    {SmartPad}
    SPInsertDate             : Boolean;
    SPLineChar               : Byte;
    {Game}
    EnableGame               : Boolean;
    {Clipboard}
    CBSize                   : LongInt;
    CBAutoSave               : Boolean;
    {Kernel}
    CanUseLFN                : Boolean;
    AutoSave                 : Boolean;
    ShowKeyCode              : Byte;
    CopyLimit                : LongInt;
    HandleChDirCommand       : Boolean;
    StoreVideoMode           : Byte;
    SmartWindowsBoxClose     : LongInt;
    DoVESATest               : Boolean;
    ForceDefaultArchiver     : String[3];
    {Editor}
    UnlimitUnindent          : Boolean;
    Koi8rKeyMap              : Boolean;
    DrawRShift               : Boolean;
    AutoScopeDetect          : Boolean;
    ShowBookmarks            : Boolean;
    FastBookmark             : Boolean;
    DefCodePage              : String[9];
    CapitalCodePageName      : Boolean;
    FastSearchDeep           : LongInt;
    WinManagerPosToEdit      : Boolean;
    AutoBracketPairs         : String[255];
    RecombineLongLines       : Boolean;
    {FilePanels}
    ShowFileMask             : Boolean;
    ShowLongName             : Boolean;
    QuickRenameInDialog      : Boolean;
    UpperCaseSorting         : Boolean;
    AutoRefreshDriveLine     : Boolean;
    QuickSearchType          : Byte;
    {NetInfo}
    NoLevelsInfo             : Boolean;
    {Language}
    ActiveLanguage           : String[9];
    HelpLanguageOverride     : String[9];
    ShowLanguageMenu         : Boolean;
    {RegExp}
    RegExpStr0               : String[255];
    RegExpStr1               : String[255];
    RegExpStr2               : String[255];
    RegExpStr3               : String[255];
    RegExpStr4               : String[255];
    RegExpStr5               : String[255];
    RegExpStr6               : String[255];
    RegExpStr7               : String[255];
    RegExpStr8               : String[255];
    RegExpStr9               : String[255];
    {SetupStorage}
    SystemDataOpt            : LongInt;
    InterfaceDataOpt         : LongInt;
    DriveInfoType            : LongInt;
    FMSetupOpt               : LongInt;
    EditorDefaultsOpt        : LongInt;
    EditorDefaultsOpt2       : LongInt;
    ViewerOpt                : LongInt;
    StartupDataLoad          : LongInt;
    StartupDataUnload        : LongInt;
    StartupDataSlice2        : LongInt;
    ConfirmsOpt              : LongInt;
    NonVIOScreenMode         : LongInt;
    VIOScreenMode            : LongInt;
    QDirs1                   : String[255];
    QDirs2                   : String[255];
    QDirs3                   : String[255];
    QDirs4                   : String[255];
    QDirs5                   : String[255];
    QDirs6                   : String[255];
    QDirs7                   : String[255];
    QDirs8                   : String[255];
    QDirs9                   : String[255];
  end;

function IniVars: PIniVars;


{CFG variables}

type
  PTetrisRec = ^TTetrisRec;

  {function ChangeNamesCaseOptions: PNamesCaseOptions;}
  {function UUDecodeOptions: AWord;}
  {function MakeListFileOptions: AWord;}
  {function UUEncodeData: PUUEncodeData;}
  {function DriveInfoData: AWord;}
  {function StartupData: PStartupData;}
  {function Confirms: Word;}
  {function TerminalDefaults: PTerminalDefaults;}
  {function Archives: PMaskData;}
  {function CustomMask1: PMaskData;}
  {function CustomMask2: PMaskData;}
  {function CustomMask3: PMaskData;}
  {function CustomMask4: PMaskData;}
  {function CustomMask5: PMaskData;}
  {function CustomMask6: PMaskData;}
  {function CustomMask7: PMaskData;}
  {function CustomMask8: PMaskData;}
  {function CustomMask9: PMaskData;}
  {function CustomMask10:PMaskData;}
  {function InterfaceData: PInterfaceData;}
  {function PanelDefaults: PPanelDefaultsData;}
  {function FMSetup: PFMSetup;}
  {function EditorDefaults: PEditorDefaultsData;}
  {function MouseData: PMouseData;}
{$IFDEF SS}
  {function SaversData: PSaversData;}
{$ENDIF}
  {function SystemData: PSystemData;}
  {function ColumnsDefaultsDisk: ???;}
  {function ColumnsDefaultsFind: ???;}
  {function ColumnsFindLast: Byte;}
  {function ColumnsDefaultsTemp: ???;}
  {function ColumnsTempLast: Byte;}
  {function ColumnsDefaultsArch: ???;}
  {function ColumnsDefaultsArvd: ???;}
  {function OldColumnsDefaults: ???;}
{$IFDEF PRINTMANAGER}
  {function RPrinterSetup: ???;}
{$ENDIF}
  function TetrisRec: PTetrisRec;

implementation

(*** Global variables ***)

function FileMode: PLongInt;
begin
  FileMode := @System.FileMode;
end;

function DosError: Integer;
begin
  DosError := Dos.DosError;
end;

function Application: PProgram;
begin
  Application := DnApp.Application;
end;

function Desktop: PDesktop;
begin
  Desktop := DnApp.Desktop;
end;

function StatusLine: PStatusLine;
begin
  StatusLine := DnApp.StatusLine;
end;

function MenuBar: PMenuView;
begin
  MenuBar := DnApp.MenuBar;
end;

function LngStream: PStream;
begin
  LngStream := DnApp.LngStream;
end;

function ResourceStream: PStream;
begin
  ResourceStream := DnApp.ResourceStream;
end;

function LStringList: PStringList;
begin
  LStringList := DnApp.LStringList;
end;

function Resource: PIdxResource;
begin
  Resource := DnApp.Resource;
end;

function EventCatchers: PEventCatcherArray;
begin
  EventCatchers := Plugin.EventCatchers;
end;

procedure AddEventCatchers;
begin
  Inc(Plugin.EventCatchersCount);
  ReAllocMem(Plugin.EventCatchers, Plugin.EventCatchersCount*SizeOf(TEventCatcherInfo));
end;

function EventCatchersCount: SmallWord;
begin
  EventCatchersCount := Plugin.EventCatchersCount;
end;

procedure SetEventCatchersCount(EventCatchersCount_: SmallWord);
begin
  Plugin.EventCatchersCount := EventCatchersCount_;
end;

function ArchiveViewers: PArchiveViewerArray;
begin
  ArchiveViewers := @Plugin.ArchiveViewers;
end;

function StartupDir: String;
begin
  StartupDir := Advance.StartupDir;
end;

function SourceDir: String;
begin
  SourceDir := Advance.SourceDir;
end;

function TempDir: String;
begin
  TempDir := Advance.TempDir;
end;

function ArcFile: PStream;
begin
  ArcFile := Archiver.ArcFile;
end;

function FileInfo: PFInfo;
begin
  FileInfo := @Archiver.FileInfo;
end;

function ArcFileName: String;
begin
  ArcFileName := Archiver.ArcFileName;
end;

function VArcFileName: String;
begin
  VArcFileName := Archiver.VArcFileName;
end;

(*** Create some objects ***)

function NewCollection(A1, A2: LongInt): PCollection;
begin
  NewCollection := New(PCollection, Init(A1, A2));
end;

function NewStringCollection(A1, A2: LongInt; LS: Boolean): PStringCollection;
begin
  NewStringCollection := New(PStringCollection, Init(A1, A2, LS));
end;

function NewFilesCollection(A1, A2: LongInt): PFilesCollection;
begin
  NewFilesCollection := New(PFilesCollection, Init(A1, A2));
end;

function NewDosStream(FileName: FNameStr; Mode: Word): PDosStream;
begin
  NewDosStream := New(PDosStream, Init(FileName, Mode));
end;

function NewBufStream(FileName: FNameStr; Mode: Word; Size: Word): PBufStream;
begin
  NewBufStream := New(PBufStream, Init(FileName, Mode, Size));
end;

function NewView(X1, Y1, X2, Y2: SmallWord): PView;
var
  R: TRect;
begin
  R.Assign(X1, Y1, X2, Y2);
  NewView := New(PView, Init(R));
end;

function NewWindow(X1, Y1, X2, Y2: SmallWord; Title: String; Number: SmallWord): PWindow;
var
  R: TRect;
begin
  R.Assign(X1, Y1, X2, Y2);
  NewWindow := New(PWindow, Init(R, Title, Number));
end;

function NewButton(X1, Y1, X2, Y2: SmallWord; Title: String; cm_, bf_: SmallWord): PButton;
var
  R: TRect;
begin
  R.Assign(X1, Y1, X2, Y2);
  NewButton := New(PButton, Init(R, Title, cm_, bf_));
end;

function NewLabel(X1, Y1, X2, Y2: SmallWord; S: String; P:PView): PLabel;
var
  R: TRect;
begin
  R.Assign(X1, Y1, X2, Y2);
  NewLabel := New(PLabel, Init(R, S, P));
end;

function NewStaticText(X1, Y1, X2, Y2: SmallWord; S: String): PStaticText;
var
  R: TRect;
begin
  R.Assign(X1, Y1, X2, Y2);
  NewStaticText := New(PStaticText, Init(R, S));
end;

function NewScrollBar(X1, Y1, X2, Y2: SmallWord): PScrollBar;
var
  R: TRect;
begin
  R.Assign(X1, Y1, X2, Y2);
  NewScrollBar := New(PScrollBar, Init(R));
end;

(*** TypeOf ***)

function TypeOf_TXDoubleWindow(P: PObject): Boolean;
begin
  TypeOf_TXDoubleWindow := (TypeOf(P^) = TypeOf(TXDoubleWindow));
end;

(*** INI variables ***)

function IniVars: PIniVars;
begin
  IniVars := PIniVars(@DnIni.iniparamblock_START);
end;

(*** CFG variables ***)

function TetrisRec: PTetrisRec;
begin
  TetrisRec := @Startup.TetrisRec;
end;

end.


(** Cat:todo
dnapp
  AppPalette: Integer;
  CommandLine: PView;
  SkyVisible: Boolean;

dnutil
  TrashCan: PTrashCan;
  HelpWnd: PHelpWindow;
  HelpInUse: Boolean;
  RunMenu: Boolean;

flpanelx
  ActivePanel: Pointer;
  CtrlWas: Boolean;
  DirsToChange: array [0..9] of PString;
  CurFileActive: PString;
  CurFilePassive: PString;
  CurrentDirectory: String;
  PShootState: Word;

fviewer
  SearchString: TViewSearch;
  LastViewerBounds: TRect;
  LastViewerDeskSize: TPoint;
  LastEditDeskSize: TPoint;

histries
  CmdStrings: PCollection;
  DirHistory: PCollection;
  EditHistory: PCollection;
  ViewHistory: PCollection;

menus
  MenuActive: Boolean;

messages
  MsgActive : Boolean;

microed
  Clipboard: PCollection;
  ClipboardStream: PStream;
  SearchData: TSearchData;
  MaxCommands: AInt;
  EditCommands: array [1..110] of TEditCommand;

microed2
  TabStep: Integer;
  SmartWindow: PEditWindow;
  ClipboardWindow: PEditWindow;
  SmartWindowPtr: ^PEditWindow;
  ClipboardWindowPtr: ^PEditWindow;

printman
  Printer: PPrintManager;
**)
