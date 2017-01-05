unit _DNFuncs;
(******

DN/2 Plugin Interface - functional model
Copyright (C) 2002 Aleksej Kozlov (Cat)
2:5030/1326.13

******)

{&Delphi+}
{&Use32+}

interface

uses
  _Defines, _Model1
  ;

procedure InitDNFunctions(Functions, Methods: Pointer);
procedure TransportVMT(DNObjVMT, OldVMT, NewVMT: Pointer;
     VMTSize: integer);
function DuplicateVMT(VMT: Pointer; VMTSize: integer): Pointer;
procedure SetVmt(Obj, VMT: Pointer);

type
  PSomeObjects1 = ^TSomeObjects1;
  TSomeObjects1 = packed record
    Application: Pointer;
    Desktop: Pointer;
    StatusLine: Pointer;
    MenuBar: Pointer;
    CommandLine: Pointer;
    ResourceStream: Pointer;
    LngStream: Pointer;
    LStringList: Pointer;
    Resource: PIdxResource;
    StringCache: Pointer;
    end;

  PSomeObjects2 = ^TSomeObjects2;
  TSomeObjects2 = packed record
    StartupDir: String;
    SourceDir: String;
    TempDir: String;
    TempFile: String;
    TempFileSWP: String;
    LngFile: String;
    SwpDir: String;
    end;

  PSomeObjects3 = ^TSomeObjects3;
  TSomeObjects3 = packed record
    EventCatchers: PEventCatcherArray;
    EventCatchersCount: integer;
    ArchiveViewers: TArchiveViewerArray;
    end;

  PSimpleHooks = ^TSimpleHooks;
  TSimpleHooks = packed record
    SetEditorEventHook: function (EditorEventHook: TEditorEventHook)
    : Boolean;
    RemoveEditorEventHook: procedure (EditorEventHook: TEditorEventHook);
    end;

  PSpecialFunctions = ^TSpecialFunctions;
  TSpecialFunctions = packed record
    RuntimePatch: function (OldFunc, NewFunc: Pointer): Boolean;
    end;

  {&AlignRec+}
  PSystemVars = ^TSystemVars;
  TSystemVars = record
    ExitCode: LongInt;
    ErrorAddr: Pointer;
    ExceptionNo: LongInt;
    TlsSharedMem: Pointer;
    TlsSharedMemSize: LongInt;
    DebugHook: Boolean;
    IsConsole: Boolean;
    IsMultiThread: Boolean;
    ExitProc: Pointer;
    XcptProc: Pointer;
    ExceptProc: Pointer;
    ErrorProc: Pointer;
    SmHeapList: Pointer;
    LgHeapList: Pointer;
    HeapError: Pointer;
    Environment: Pointer;
    ExceptClsProc: Pointer;
    ExceptObjProc: Pointer;
    ExceptionClass: TClass;
    CmdLine: PChar;
    ModuleHandle: LongInt;
    RandSeed: LongInt;
    AllocMemCount: LongInt;
    AllocMemSize: LongInt;
    SmHeapBlock: LongInt;
    LgHeapBlock: LongInt;
    HeapLimit: LongInt;
    HeapAllocFlags: LongInt;
    HeapSemaphore: LongInt;
    Test8086: Byte;
    Test8087: Byte;
    {additional}
    HInstance: LongInt;
    HPrevInst: LongInt;
    CmdShow: LongInt;
    NotForYou: LongInt;
    HeapBlockList: Pointer;
    hblNext: LongInt;
    hblAlloc: LongInt;
    MemoryManager: TMemoryManager;
    end;
  {&AlignRec-}

  TDNFunctions = packed record
    DN2Version: integer;
    APIVersion: integer;
    Reserved1: integer;
    SystemVars: PSystemVars;

    SomeObjects1: PSomeObjects1;
    SomeObjects2: PSomeObjects2;
    SomeObjects3: PSomeObjects3;
    Reserved2: integer;
    Reserved3: integer;
    SpecialFunctions: PSpecialFunctions;
    TryExcept: function (Proc: TProcedure): Pointer;
    SimpleHooks: PSimpleHooks;

    MemoryManager: packed record
      GetMem: function (Size: LongInt): Pointer;
      FreeMem: function (P: Pointer): LongInt;
      ReallocMem: function (P: Pointer; Size: LongInt): Pointer;
      end;

    TinySlice: procedure;

    Evalue: function (const S: String; CCV: Pointer): Extended;
    EvalueError: ^Boolean;

    DosError: function : integer;

    lFindFirst: procedure (const Path: String; Attr: word;
       var R: lSearchRec);
    lFindNext: procedure (var R: lSearchRec);
    lFindClose: procedure (var R: lSearchRec);

    CopyFileRec: function (fr: PFileRec): PFileRec;
    CreateFileRec: function (Name: String): PFileRec;
    NewFileRec: function (const {$IFNDEF OS2}Lfn, {$ENDIF}Name: String;
       Size: TSize; Date, CreationDate, LastAccDate: LongInt;
       Attr: word; AOwner: PString): PFileRec;
    DelFileRec: procedure (var fr: PFileRec);
    LoadFileRec: function (var S: TStream): PFileRec;
    StoreFileRec: procedure (var S: TStream; fr: PFileRec);
    LoadFileRecOwn: function (var S: TStream; Dirs: PCollection): PFileRec;
    StoreFileRecOwn: procedure (var S: TStream; fr: PFileRec;
       Dirs: PCollection);

    GetActivePanel: function : Pointer;
    GetPassivePanel: function : Pointer;
    GetSelection: function (P: Pointer; Single: Boolean): PFilesCollection;
    ClearSelection: procedure (AFP: Pointer; FC: Pointer);

    NewStr: function (const S: String): PString;
    NewLongStr: function (const S: LongString): PLongString;
    DisposeStr: procedure (var P: PString);
    DisposeLongStr: procedure (var P: PLongString);
    CnvString: function (P: PString): String;
    CnvLongString: function (P: PLongString): LongString;

    UpStr: procedure (var S: String);
    UpLongStr: procedure (var S: LongString);
    LowStr: procedure (var S: String);
    LowLongStr: procedure (var S: LongString);

    FormatStr: procedure (var Result: String; const Format: String;
       var Params);
    MoveColor: procedure (var Buf; Num: word; Attr: Byte);
    MoveBuf: procedure (var Dest; var Source; Attr: Byte; Count: word);
    MoveChar: procedure (var Dest; C: Char; Attr: Byte; Count: word);
    MoveCStr: procedure (var Dest; const Str: String; Attrs: word);
    MoveStr: procedure (var Dest; const Str: String; Attr: Byte);
    CStrLen: function (const S: String): integer;

    ExecView: function (P: Pointer): integer;
    Reserved4: integer;

    GetString: function (Index: integer): String;
    ExecResource: function (Key: integer; var Data): word;
    LoadResource: function (Key: integer): PObject;

    OpenRez: function (const PluginName: String): LongInt;
    OpenRezX: function (const PluginName: String): LongInt;
    CloseRez: procedure (RezId: LongInt);
    GetRezString: function (RezId: LongInt; ItemId: SmallWord): String;
    GetRezObject: function (RezId: LongInt; ItemId: SmallWord): PObject;

    NewSItem: function (const Str: String; ANext: PSItem): PSItem;

    NewItem: function (Name, Param: TMenuStr; KeyCode: word;
       Command: word; AHelpCtx: word; Next: PMenuItem): PMenuItem;
    NewLine: function (Next: PMenuItem): PMenuItem;
    NewSubMenu: function (Name: TMenuStr; AHelpCtx: word;
       SubMenu: PMenu; Next: PMenuItem): PMenuItem;
    NewMenu: function (Items: PMenuItem): PMenu;
    DisposeMenu: procedure (Menu: PMenu);
    ExecAndDisposeMenu: function (Menu: PMenu): integer;
    StoreMenuDefaults: procedure (Menu: PMenu; var S: TStream);
    LoadMenuDefaults: procedure (Menu: PMenu; var S: TStream);
    LookUpMenu: function (Menu: PMenu; idCheckItem: word; Flags: word)
    : PMenuItem;
    MenuIndexOf: function (Menu: PMenu; idCheckItem: PMenuItem): word;

    NewStatusDef: function (AMin, AMax: word; AItems: PStatusItem;
       ANext: PStatusDef): PStatusDef;
    NewStatusKey: function (const AText: String; AKeyCode: word;
       ACommand: word; ANext: PStatusItem): PStatusItem;

    LngId: function : String;
    HelpLngId: function : String;

    RegisterType: procedure (var S: TStreamRec);
    ReRegisterType: procedure (var S: TStreamRec);

    Message: function (Receiver: PView; What, Command: word;
       InfoPtr: Pointer): Pointer;
    MessageL: function (Receiver: PView; What, Command: word;
       InfoLng: LongInt): Pointer;

    RegisterToPrior: procedure (P: PView);
    RegisterToBackground: procedure (P: PView);
    Deregister: procedure (P: PView);
    UpdateAll: procedure (All: Boolean);

    GetWinNumber: function : AInt;

    MessageBox: function (Msg: String; Params: Pointer; AOptions: word)
    : word;
    MessageBox2: function (Msg1, Msg2: String;
       Params1, Params2: Pointer; AOptions: word): word;
    MessageBoxRect: function (var R: TRect; Msg: String;
       Params: Pointer; AOptions: word): word;
    MessageBox2Rect: function (var R: TRect; Msg1, Msg2: String;
       Lines1: word; Params1, Params2: Pointer; AOptions: word): word;
    InputBox: function (Title: String; ALabel: String; var S: String;
       Limit: word; HistoryId: word): word;
    BigInputBox: function (Title: String; ALabel: String; var S: String;
       Limit: word; HistoryId: word): word;
    InputBoxRect: function (var Bounds: TRect; Title: String;
       ALabel: String; var S: String; Limit: word; HistoryId: word): word;

    GetFileNameDialog: function (Mask, Title, Name: String;
       Buttons, HistoryId: word): String;
    GetFileNameMenu: function (Path, Mask, Default: String;
       PutNumbers: Boolean; var More, None: Boolean): String;

    Reserved5: integer;
    Reserved6: integer;

    UpdateWriteView: procedure (P: Pointer);
    GlobalMessage: function (What, Command: word; InfoPtr: Pointer)
    : Pointer;
    GlobalMessageL: function (What, Command: word; InfoLng: LongInt)
    : Pointer;
    GlobalEvent: procedure (What, Command: word; InfoPtr: Pointer);
    ViewPresent: function (Command: word; InfoPtr: Pointer): PView;
    WriteMsg: function (Text: String): PView;
    ForceWriteShow: procedure (P: Pointer);
    ToggleCommandLine: procedure (OnOff: Boolean);
    AdjustToDesktopSize: procedure (var R: TRect; OldDeskSize: TPoint);

    Reserved7: integer;
    Reserved8: integer;

    HistoryAdd: procedure (Id: Byte; const Str: String);
    HistoryCount: function (Id: Byte): word;
    HistoryStr: function (Id: Byte; Index: integer): String;
    DeleteHistoryStr: procedure (Id: Byte; Index: integer);

    Reserved9: integer;
    Reserved10: integer;

    GetMouseEvent: procedure (var Event: TEvent);
    GetKeyEvent: procedure (var Event: TEvent);

    DispWhileViewEvents: procedure (InfoView: PWhileView;
       var CancelParam: Boolean);

    Reserved11: integer;

    SetTitle: procedure (Text: String);

    SetWinClip: function (PC: PLineCollection): Boolean;
    GetWinClip: function (var PCL: PLineCollection): Boolean;
    GetWinClipSize: function : Boolean;
    SyncClipIn: procedure;
    SyncClipOut: procedure;
    CopyLines2Stream: procedure (PC: PCollection; var PCS: PStream);
    CopyStream2Lines: procedure (PCS: PStream; var PC: PCollection);

    NewTimerSecs: procedure (var ET: TEventTimer; Secs: LongInt);
    NewTimer: procedure (var ET: TEventTimer; Tics: LongInt);
    TimerExpired: function (ET: TEventTimer): Boolean;
    ElapsedTime: function (ET: TEventTimer): LongInt;
    ElapsedTimeInSecs: function (ET: TEventTimer): LongInt;

    GetPossibleDizOwner: function (n: integer): String;
    GetDizOwner: function (const Path, LastOwner: String; Add: Boolean)
    : String;
    CalcDizPath: function (P: PDiz; Owen: PString): String;
    ReplaceDiz: procedure (const DPath, Name: String; ANewName: PString;
       ANewDescription: PString);
    DeleteDiz: procedure (const DPath, Name: String);
    GetDiz: function (const DPath: String; var Line: LongInt;
       const Name: String): String;
    SetDescription: procedure (PF: PFileRec; DizOwner: String);

    Reserved12: integer;
    Reserved13: integer;

    SelectFiles: function (AFP: Pointer; Select, XORs: Boolean): Boolean;
    InvertSelection: procedure (AFP: Pointer; dr: Boolean);
    DragMover: procedure (AP: Pointer; Text: String; AFC, AC: Pointer);
    CM_AdvancedFilter: procedure (AFP: Pointer);
    CM_ArchiveFiles: procedure (AFP: Pointer);
    {CM_Branch: procedure(AFP: Pointer);}
    CM_ChangeDirectory: function (AFP: Pointer): String;
    CM_ChangeCase: procedure (AFP: Pointer);
    CM_CompareDirs: procedure (AFP, IP: Pointer);
    CM_CopyFiles: procedure (AFP: Pointer; MoveMode, Single: Boolean);
    CM_CopyTemp: procedure (AFP: Pointer);
    CM_DragDropper: procedure (AFP: Pointer; CurPos: integer; EV: Pointer);
    CM_Dropped: procedure (AFP, EI: Pointer);
    CM_EraseFiles: procedure (AFP: Pointer; Single: Boolean);
    CM_LongCopy: procedure (AFP: Pointer);
    CM_MakeDir: procedure (AFP: Pointer);
    CM_MakeList: procedure (AFP: Pointer);
    CM_RenameSingleL: procedure (AFP, PEV: Pointer);
    CM_RenameSingleDialog: procedure (AFP, PEV: Pointer);
    CM_SelectColumn: procedure (AFP: Pointer);
    CM_SetAttributes: procedure (AFP: Pointer; Single: Boolean;
       CurPos: integer);
    CM_SetShowParms: procedure (AFP: Pointer);
    CM_SortBy: procedure (AFP: Pointer);
    CM_ToggleLongNames: procedure (AFP: Pointer);
    CM_ToggleShowMode: procedure (AFP: Pointer);
    CM_ToggleDescriptions: procedure (AFP: Pointer);

    Reserved14: integer;
    Reserved15: integer;

    ExecString: procedure (S: PString; WS: String);
    SearchExt: function (FileRec: PFileRec; var HS: String): Boolean;
    ExecExtFile: function (const ExtFName: String;
       UserParams: PUserParams; SIdx: integer): Boolean;
    ExecFile: procedure (const FileName: String);
    AnsiExec: procedure (const Path: String; const ComLine: AnsiString);

    Reserved16: integer;
    Reserved17: integer;

    SelectDrive: function (X, Y: integer; Default: Char;
       IncludeTemp: Boolean): String;
    GetFileType: function (const S: String; Attr: Byte): integer;
    DosReread: procedure (Files: PFilesCollection);

    FnMatch: function (Pattern, Str: String): Boolean;
    SearchFileStr: function (F: PStream; var XLAT: TXlat;
       const What: String; Pos: LongInt;
      CaseSensitive, Display, WholeWords, Back, AllCP, IsRegExp: Boolean)
    : LongInt;
    MakeListFile: procedure (APP: Pointer; Files: PCollection);
    ASCIITable: procedure;
    InsertCalendar: procedure;
    InsertCalc: procedure;
    ChangeColors: procedure;
    WindowManager: procedure;
    SetHighlightGroups: procedure;
    UnpackDiskImages: procedure (AOwner: Pointer; Files: PFilesCollection);
    end;

type
  PTEmptyObject = ^TTEmptyObject;
  TTEmptyObject = packed record
    VMT: PEmptyObjectVMT;
    Init: function (VMT, Obj: Pointer): Pointer;
    Free: procedure (Obj: Pointer);
    end;

  PTObject = ^TTObject;
  TTObject = packed record
    VMT: PObjectVMT;
    Init: function (VMT, Obj: Pointer): Pointer;
    end;

  PTRegExp = ^TTRegExp;
  TTRegExp = packed record
    VMT: PObjectVMT;
    Init: function (VMT, Obj: Pointer): Pointer;
    Reset: procedure (Obj: Pointer);
    CompileString: function (const AExpression: String; Obj: Pointer)
    : Boolean;
    CompileStr: function (AExpression: PChar; Obj: Pointer): Boolean;
    Compile: function (AExpression: PChar; ALength: integer; Obj: Pointer)
    : Boolean;
    Execute: function (AString: PChar; ALength: integer; Obj: Pointer)
    : Boolean;
    SubstituteString: function (ASrc: PChar; const AReplace: String;
       var ADest: String; Obj: Pointer): Boolean;
    SubstituteStr: function (ASrc, AReplace: PChar; ADest: PChar;
       var ALength: integer; Obj: Pointer): Boolean;
    Substitute: function (ASrc, AReplace: PChar; ARLen: integer;
       ADest: PChar; var ADLen: integer; Obj: Pointer): Boolean;
    end;

  PTStream = ^TTStream;
  TTStream = packed record
    VMT: PStreamVMT;
    CopyFrom: procedure (var S: TStream; Count: LongInt; Obj: Pointer);
    Get: function (Obj: Pointer): PObject;
    Put: procedure (P: PObject; Obj: Pointer);
    ReadStr: function (Obj: Pointer): PString;
    ReadLongStr: function (Obj: Pointer): PLongString;
    ReadStrV: procedure (var S: String; Obj: Pointer);
    ReadLongStrV: procedure (var S: LongString; Obj: Pointer);
    Reset: procedure (Obj: Pointer);
    StrRead: function (Obj: Pointer): PChar;
    StrWrite: procedure (P: PChar; Obj: Pointer);
    WriteStr: procedure (P: PString; Obj: Pointer);
    WriteLongStr: procedure (P: PLongString; Obj: Pointer);
    Eof: function (Obj: Pointer): Boolean;
    end;

  PTDosStream = ^TTDosStream;
  TTDosStream = packed record
    VMT: PDosStreamVMT;
    Init: function (FileName: String; Mode: word; VMT, Obj: Pointer)
    : Pointer;
    Open: procedure (FileName: String; Mode: word; Obj: Pointer);
    end;

  PTBufStream = ^TTBufStream;
  TTBufStream = packed record
    VMT: PBufStreamVMT;
    Init: function (FileName: String; Mode: word; Size: LongInt;
       VMT, Obj: Pointer): Pointer;
    end;

  PTMemoryStream = ^TTMemoryStream;
  TTMemoryStream = packed record
    VMT: PMemoryStreamVMT;
    Init: function (ALimit: LongInt; ABlockSize: word; VMT, Obj: Pointer)
    : Pointer;
    end;

  PTCollection = ^TTCollection;
  TTCollection = packed record
    VMT: PCollectionVMT;
    Init: function (ALimit, ADelta: LongInt; VMT, Obj: Pointer): Pointer;
    Load: function (var S: TStream; VMT, Obj: Pointer): Pointer;
    At: function (Index: LongInt; Obj: Pointer): Pointer;
    AtDelete: procedure (Index: LongInt; Obj: Pointer);
    AtFree: procedure (Index: LongInt; Obj: Pointer);
    AtInsert: procedure (Index: LongInt; Item: Pointer; Obj: Pointer);
    AtPut: procedure (Index: LongInt; Item: Pointer; Obj: Pointer);
    AtReplace: procedure (Index: LongInt; Item: Pointer; Obj: Pointer);
    Delete: procedure (Item: Pointer; Obj: Pointer);
    DeleteAll: procedure (Obj: Pointer);
    FirstThat: function (Test: Pointer; Obj: Pointer): Pointer;
    ForEach: procedure (Action: Pointer; Obj: Pointer);
    Free: procedure (Item: Pointer; Obj: Pointer);
    FreeAll: procedure (Obj: Pointer);
    LastThat: function (Test: Pointer; Obj: Pointer): Pointer;
    Pack: procedure (Obj: Pointer);
    Store: procedure (var S: TStream; Obj: Pointer);
    end;

  PTSortedCollection = ^TTSortedCollection;
  TTSortedCollection = packed record
    VMT: PSortedCollectionVMT;
    Init: function (ALimit, ADelta: LongInt; VMT, Obj: Pointer): Pointer;
    Load: function (var S: TStream; VMT, Obj: Pointer): Pointer;
    Store: procedure (var S: TStream; Obj: Pointer);
    Sort: procedure (Obj: Pointer);
    end;

  PTLineCollection = ^TTLineCollection;
  TTLineCollection = packed record
    VMT: PLineCollectionVMT;
    Init: function (ALimit, ADelta: LongInt; ALongStrings: Boolean;
       VMT, Obj: Pointer): Pointer;
    end;

  PTStringCollection = ^TTStringCollection;
  TTStringCollection = packed record
    VMT: PStringCollectionVMT;
    Init: function (ALimit, ADelta: LongInt; ALongStrings: Boolean;
       VMT, Obj: Pointer): Pointer;
    end;

  PTStrCollection = ^TTStrCollection;
  TTStrCollection = packed record
    VMT: PStrCollectionVMT;
    Init: function (ALimit, ADelta: LongInt; VMT, Obj: Pointer): Pointer;
    end;

  PTFilesCollection = ^TTFilesCollection;
  TTFilesCollection = packed record
    VMT: PFilesCollectionVMT;
    Load: function (var S: TStream; VMT, Obj: Pointer): Pointer;
    end;

  PTView = ^TTView;
  TTView = packed record
    VMT: PViewVMT;
    Init: function (var Bounds: TRect; VMT, Obj: Pointer): Pointer;
    Load: function (var S: TStream; VMT, Obj: Pointer): Pointer;
    BlockCursor: procedure (Obj: Pointer);
    ClearEvent: procedure (var Event: TEvent; Obj: Pointer);
    CommandEnabled: function (Command: word; Obj: Pointer): Boolean;
    DisableCommands: procedure (Commands: TCommandSet; Obj: Pointer);
    DragView: procedure (Event: TEvent; Mode: Byte; var Limits: TRect;
       MinSize, MaxSize: TPoint; Obj: Pointer);
    DrawView: procedure (Obj: Pointer);
    EnableCommands: procedure (Commands: TCommandSet; Obj: Pointer);
    EventAvail: function (Obj: Pointer): Boolean;
    Exposed: function (Obj: Pointer): Boolean;
    Focus: function (Obj: Pointer): Boolean;
    GetBounds: procedure (var Bounds: TRect; Obj: Pointer);
    GetClipRect: procedure (var Clip: TRect; Obj: Pointer);
    GetColor: function (Color: word; Obj: Pointer): word;
    GetCommands: procedure (var Commands: TCommandSet; Obj: Pointer);
    GetExtent: procedure (var extent: TRect; Obj: Pointer);
    GetPeerViewPtr: procedure (var S: TStream; var P; Obj: Pointer);
    GetState: function (AState: word; Obj: Pointer): Boolean;
    GrowTo: procedure (X, Y: LongInt; Obj: Pointer);
    Hide: procedure (Obj: Pointer);
    HideCursor: procedure (Obj: Pointer);
    KeyEvent: procedure (var Event: TEvent; Obj: Pointer);
    Locate: procedure (var Bounds: TRect; Obj: Pointer);
    MakeFirst: procedure (Obj: Pointer);
    MakeGlobal: procedure (Source: TPoint; var Dest: TPoint; Obj: Pointer);
    MakeLocal: procedure (Source: TPoint; var Dest: TPoint; Obj: Pointer);
    MouseEvent: function (var Event: TEvent; Mask: word; Obj: Pointer)
    : Boolean;
    MouseInView: function (Mouse: TPoint; Obj: Pointer): Boolean;
    MoveTo: procedure (X, Y: LongInt; Obj: Pointer);
    NextView: function (Obj: Pointer): PView;
    NormalCursor: procedure (Obj: Pointer);
    Prev: function (Obj: Pointer): PView;
    PrevView: function (Obj: Pointer): PView;
    PutInFrontOf: procedure (Target: PView; Obj: Pointer);
    PutPeerViewPtr: procedure (var S: TStream; P: PView; Obj: Pointer);
    Select: procedure (Obj: Pointer);
    SetBounds: procedure (var Bounds: TRect; Obj: Pointer);
    SetCommands: procedure (Commands: TCommandSet; Obj: Pointer);
    SetCursor: procedure (X, Y: LongInt; Obj: Pointer);
    Show: procedure (Obj: Pointer);
    ShowCursor: procedure (Obj: Pointer);
    Store: procedure (var S: TStream; Obj: Pointer);
    TopView: function (Obj: Pointer): PView;
    WriteBuf: procedure (X, Y, W, H: integer; var Buf; Obj: Pointer);
    WriteChar: procedure (X, Y: integer; C: Char; Color: Byte;
       Count: integer; Obj: Pointer);
    WriteLine: procedure (X, Y, W, H: integer; var Buf; Obj: Pointer);
    WriteStr: procedure (X, Y: integer; Str: String; Color: Byte;
       Obj: Pointer);
    MenuEnabled: function (Command: word; Obj: Pointer): Boolean;
    DrawCursor: procedure (Obj: Pointer);
    DrawHide: procedure (LastView: PView; Obj: Pointer);
    DrawShow: procedure (LastView: PView; Obj: Pointer);
    DrawUnderRect: procedure (var R: TRect; LastView: PView; Obj: Pointer);
    DrawUnderView: procedure (DoShadow: Boolean; LastView: PView;
       Obj: Pointer);
    end;

  PTFrame = ^TTFrame;
  TTFrame = packed record
    VMT: PFrameVMT;
    Init: function (var Bounds: TRect; VMT, Obj: Pointer): Pointer;
    end;

  PTScrollBar = ^TTScrollBar;
  TTScrollBar = packed record
    VMT: PScrollBarVMT;
    Init: function (var Bounds: TRect; VMT, Obj: Pointer): Pointer;
    Load: function (var S: TStream; VMT, Obj: Pointer): Pointer;
    SetParams: procedure (AValue, AMin, AMax, APgStep, AArStep: LongInt;
       Obj: Pointer);
    SetRange: procedure (AMin, AMax: LongInt; Obj: Pointer);
    SetStep: procedure (APgStep, AArStep: LongInt; Obj: Pointer);
    SetValue: procedure (AValue: LongInt; Obj: Pointer);
    Store: procedure (var S: TStream; Obj: Pointer);
    end;

  PTGroup = ^TTGroup;
  TTGroup = packed record
    VMT: PGroupVMT;
    Init: function (var Bounds: TRect; VMT, Obj: Pointer): Pointer;
    Load: function (var S: TStream; VMT, Obj: Pointer): Pointer;
    Delete: procedure (P: PView; Obj: Pointer);
    ExecView: function (P: PView; Obj: Pointer): word;
    First: function (Obj: Pointer): PView;
    FirstThat: function (P: Pointer; Obj: Pointer): PView;
    FocusNext: function (Forwards: Boolean; Obj: Pointer): Boolean;
    ForEach: procedure (P: Pointer; Obj: Pointer);
    GetSubViewPtr: procedure (var S: TStream; var P; Obj: Pointer);
    Insert: procedure (P: PView; Obj: Pointer);
    InsertBefore: procedure (P, Target: PView; Obj: Pointer);
    Lock: procedure (Obj: Pointer);
    PutSubViewPtr: procedure (var S: TStream; P: PView; Obj: Pointer);
    SelectNext: procedure (Forwards: Boolean; Obj: Pointer);
    Store: procedure (var S: TStream; Obj: Pointer);
    UnLock: procedure (Obj: Pointer);
    FreeBuffer: procedure (Obj: Pointer);
    GetBuffer: procedure (Obj: Pointer);
    InsertView: procedure (P, Target: PView; Obj: Pointer);
    SetCurrent: procedure (P: PView; Mode: TSelectMode; Obj: Pointer);
    end;

  PTWindow = ^TTWindow;
  TTWindow = packed record
    VMT: PWindowVMT;
    Init: function (var Bounds: TRect; const ATitle: String;
       ANumber: integer; VMT, Obj: Pointer): Pointer;
    Load: function (var S: TStream; VMT, Obj: Pointer): Pointer;
    StandardScrollBar: function (AOptions: word; Obj: Pointer): PScrollBar;
    Store: procedure (var S: TStream; Obj: Pointer);
    end;

  PTMenuView = ^TTMenuView;
  TTMenuView = packed record
    VMT: PMenuViewVMT;
    Init: function (var Bounds: TRect; VMT, Obj: Pointer): Pointer;
    Load: function (var S: TStream; VMT, Obj: Pointer): Pointer;
    FindItem: function (Ch: Char; Obj: Pointer): PMenuItem;
    HotKey: function (KeyCode: word; Obj: Pointer): PMenuItem;
    Store: procedure (var S: TStream; Obj: Pointer);
    end;

  PTMenuBar = ^TTMenuBar;
  TTMenuBar = packed record
    VMT: PMenuBarVMT;
    Init: function (var Bounds: TRect; AMenu: PMenu; VMT, Obj: Pointer)
    : Pointer;
    end;

  PTMenuBox = ^TTMenuBox;
  TTMenuBox = packed record
    VMT: PMenuBoxVMT;
    Init: function (var Bounds: TRect; AMenu: PMenu;
       AParentMenu: PMenuView; VMT, Obj: Pointer): Pointer;
    end;

  PTMenuPopup = ^TTMenuPopup;
  TTMenuPopup = packed record
    VMT: PMenuPopupVMT;
    Init: function (var Bounds: TRect; AMenu: PMenu; VMT, Obj: Pointer)
    : Pointer;
    end;

  PTStatusLine = ^TTStatusLine;
  TTStatusLine = packed record
    VMT: PStatusLineVMT;
    Init: function (var Bounds: TRect; ADefs: PStatusDef;
       VMT, Obj: Pointer): Pointer;
    Load: function (var S: TStream; VMT, Obj: Pointer): Pointer;
    Store: procedure (var S: TStream; Obj: Pointer);
    end;

  PTDialog = ^TTDialog;
  TTDialog = packed record
    VMT: PDialogVMT;
    Init: function (var Bounds: TRect; const ATitle: String;
       VMT, Obj: Pointer): Pointer;
    Load: function (var S: TStream; VMT, Obj: Pointer): Pointer;
    end;

  PTInputLine = ^TTInputLine;
  TTInputLine = packed record
    VMT: PInputLineVMT;
    Init: function (var Bounds: TRect; AMaxLen: AInt; VMT, Obj: Pointer)
    : Pointer;
    Load: function (var S: TStream; VMT, Obj: Pointer): Pointer;
    SelectAll: procedure (Enable: Boolean; Obj: Pointer);
    SetValidator: procedure (AValid: Pointer {PValidator}; Obj: Pointer);
    Store: procedure (var S: TStream; Obj: Pointer);
    CanScroll: function (Delta: integer; Obj: Pointer): Boolean;
    end;

  PTButton = ^TTButton;
  TTButton = packed record
    VMT: PButtonVMT;
    Init: function (var Bounds: TRect; const ATitle: String;
       ACommand: word; AFlags: word; VMT, Obj: Pointer): Pointer;
    Load: function (var S: TStream; VMT, Obj: Pointer): Pointer;
    DrawState: procedure (Down: Boolean; Obj: Pointer);
    MakeDefault: procedure (Enable: Boolean; Obj: Pointer);
    Store: procedure (var S: TStream; Obj: Pointer);
    end;

  PTCluster = ^TTCluster;
  TTCluster = packed record
    VMT: PClusterVMT;
    Init: function (var Bounds: TRect; AStrings: PSItem;
       VMT, Obj: Pointer): Pointer;
    Load: function (var S: TStream; VMT, Obj: Pointer): Pointer;
    ButtonState: function (Item: integer; Obj: Pointer): Boolean;
    DrawBox: procedure (const Icon: String; Marker: Char; Obj: Pointer);
    DrawMultiBox: procedure (const Icon, Marker: String; Obj: Pointer);
    SetButtonState: procedure (AMask: LongInt; Enable: Boolean;
       Obj: Pointer);
    Store: procedure (var S: TStream; Obj: Pointer);
    end;

  PTRadioButtons = ^TTRadioButtons;
  TTRadioButtons = packed record
    VMT: PRadioButtonsVMT;
    end;

  PTCheckBoxes = ^TTCheckBoxes;
  TTCheckBoxes = packed record
    VMT: PCheckBoxesVMT;
    end;

  PTMultiCheckBoxes = ^TTMultiCheckBoxes;
  TTMultiCheckBoxes = packed record
    VMT: PMultiCheckBoxesVMT;
    Init: function (var Bounds: TRect; AStrings: PSItem;
       ASelRange: Byte; AFlags: word; const AStates: String;
       VMT, Obj: Pointer): Pointer;
    Load: function (var S: TStream; VMT, Obj: Pointer): Pointer;
    Store: procedure (var S: TStream; Obj: Pointer);
    end;

  PTScroller = ^TTScroller;
  TTScroller = packed record
    VMT: PScrollerVMT;
    Init: function (var Bounds: TRect;
       AHScrollBar, AVScrollBar: PScrollBar; VMT, Obj: Pointer): Pointer;
    Load: function (var S: TStream; VMT, Obj: Pointer): Pointer;
    ScrollTo: procedure (X, Y: LongInt; Obj: Pointer);
    SetLimit: procedure (X, Y: LongInt; Obj: Pointer);
    Store: procedure (var S: TStream; Obj: Pointer);
    end;

  PTListViewer = ^TTListViewer;
  TTListViewer = packed record
    VMT: PListViewerVMT;
    Init: function (var Bounds: TRect; ANumCols: LongInt;
       AHScrollBar, AVScrollBar: PScrollBar; VMT, Obj: Pointer): Pointer;
    Load: function (var S: TStream; VMT, Obj: Pointer): Pointer;
    SetRange: procedure (ARange: LongInt; Obj: Pointer);
    Store: procedure (var S: TStream; Obj: Pointer);
    end;

  PTListBox = ^TTListBox;
  TTListBox = packed record
    VMT: PListBoxVMT;
    Init: function (var Bounds: TRect; ANumCols: word;
       AScrollBar: PScrollBar; VMT, Obj: Pointer): Pointer;
    Load: function (var S: TStream; VMT, Obj: Pointer): Pointer;
    Store: procedure (var S: TStream; Obj: Pointer);
    end;

  PTStaticText = ^TTStaticText;
  TTStaticText = packed record
    VMT: PStaticTextVMT;
    Init: function (var Bounds: TRect; const AText: String;
       VMT, Obj: Pointer): Pointer;
    Load: function (var S: TStream; VMT, Obj: Pointer): Pointer;
    Store: procedure (var S: TStream; Obj: Pointer);
    end;

  PTParamText = ^TTParamText;
  TTParamText = packed record
    VMT: PParamTextVMT;
    Init: function (var Bounds: TRect; const AText: String;
       AParamCount: AInt; VMT, Obj: Pointer): Pointer;
    Load: function (var S: TStream; VMT, Obj: Pointer): Pointer;
    Store: procedure (var S: TStream; Obj: Pointer);
    end;

  PTLabel = ^TTLabel;
  TTLabel = packed record
    VMT: PLabelVMT;
    Init: function (var Bounds: TRect; const AText: String;
       ALink: PView; VMT, Obj: Pointer): Pointer;
    Load: function (var S: TStream; VMT, Obj: Pointer): Pointer;
    Store: procedure (var S: TStream; Obj: Pointer);
    end;

  PTHistoryViewer = ^TTHistoryViewer;
  TTHistoryViewer = packed record
    VMT: PHistoryViewerVMT;
    Init: function (var Bounds: TRect;
       AHScrollBar, AVScrollBar: PScrollBar; AHistoryId: AWord;
       VMT, Obj: Pointer): Pointer;
    HistoryWidth: function (Obj: Pointer): integer;
    end;

  PTHistoryWindow = ^TTHistoryWindow;
  TTHistoryWindow = packed record
    VMT: PHistoryWindowVMT;
    Init: function (var Bounds: TRect; HistoryId: AWord;
       VMT, Obj: Pointer): Pointer;
    end;

  PTHistory = ^TTHistory;
  TTHistory = packed record
    VMT: PHistoryVMT;
    Init: function (var Bounds: TRect; ALink: PInputline;
       AHistoryId: AWord; VMT, Obj: Pointer): Pointer;
    Load: function (var S: TStream; VMT, Obj: Pointer): Pointer;
    Store: procedure (var S: TStream; Obj: Pointer);
    end;

  PTBackground = ^TTBackground;
  TTBackground = packed record
    VMT: PBackgroundVMT;
    Init: function (var Bounds: TRect; APattern: Char; VMT, Obj: Pointer)
    : Pointer;
    Load: function (var S: TStream; VMT, Obj: Pointer): Pointer;
    Store: procedure (var S: TStream; Obj: Pointer);
    end;

  PTDesktop = ^TTDesktop;
  TTDesktop = packed record
    VMT: PDesktopVMT;
    Init: function (var Bounds: TRect; VMT, Obj: Pointer): Pointer;
    Load: function (var S: TStream; VMT, Obj: Pointer): Pointer;
    Cascade: procedure (var R: TRect; Obj: Pointer);
    Clear: procedure (Obj: Pointer);
    Store: procedure (var S: TStream; Obj: Pointer);
    Tile: procedure (var R: TRect; Obj: Pointer);
    end;

  PTProgram = ^TTProgram;
  TTProgram = packed record
    VMT: PProgramVMT;
    Init: function (VMT, Obj: Pointer): Pointer;
    CanMoveFocus: function (Obj: Pointer): Boolean;
    ExecuteDialog: function (P: PDialog; Data: Pointer; Obj: Pointer)
    : word;
    InsertWindow: function (P: PWindow; Obj: Pointer): PWindow;
    ActivateView: procedure (P: PView; Obj: Pointer);
    SetScreenMode: procedure (Mode: word; Obj: Pointer);
    ValidView: function (P: PView; Obj: Pointer): PView;
    end;

  PTApplication = ^TTApplication;
  TTApplication = packed record
    VMT: PApplicationVMT;
    Init: function (VMT, Obj: Pointer): Pointer;
    Cascade: procedure (Obj: Pointer);
    ShowUserScreen: procedure (Obj: Pointer);
    Tile: procedure (Obj: Pointer);
    end;

  PTDNApplication = ^TTDNApplication;
  TTDNApplication = packed record
    VMT: PDNApplicationVMT;
    Init: function (VMT, Obj: Pointer): Pointer;
    ViewFile: procedure (AltExt, NoExtFile: Boolean; FileName: String;
       Obj: Pointer);
    AddFormat: procedure (Obj: Pointer);
    EditFile: procedure (Intern: Boolean; FileName: String; Obj: Pointer);
    RetrieveDesktop: procedure (const FileName: String; LS: PStream;
       LoadColors: Boolean; Obj: Pointer);
    SaveDesktop: procedure (const FileName: String; Obj: Pointer);
    LoadDesktop: procedure (var S: TStream; Obj: Pointer);
    StoreDesktop: procedure (var S: TStream; Obj: Pointer);
    ChgColors: procedure (Obj: Pointer);
    end;

  PTUniWindow = ^TTUniWindow;
  TTUniWindow = packed record
    VMT: PUniWindowVMT;
    MakeScrollBar: function (AOptions: word; Obj: Pointer): PScrollBar;
    end;

  PTXFileEditor = ^TTXFileEditor;
  TTXFileEditor = packed record
    VMT: PXFileEditorVMT;
    Init: function (var Bounds: TRect;
       AHScrollBar, AVScrollBar: PScrollBar; var FileName: String;
       VMT, Obj: Pointer): Pointer;
    Load: function (var S: TStream; VMT, Obj: Pointer): Pointer;
    Store: procedure (var S: TStream; Obj: Pointer);
    DoHighlite: procedure (var B; const S: LongString;
       const Attr: String; Obj: Pointer);
    GetLine: function (Index: LongInt; Obj: Pointer): LongString;
    GetSelection: function (Obj: Pointer): PCollection;
    ValidBlock: function (Obj: Pointer): Boolean;
    CalcMenu: procedure (Obj: Pointer);
    Search: function (StartX, StartY: word; Obj: Pointer): Boolean;
    InsertBlock: procedure (ABlock: PCollection; SaveUndo: Boolean;
       Obj: Pointer);
    ModifyLine: procedure (Index: LongInt; S: LongString;
       DelSpaces: Boolean; Obj: Pointer);
    SetLimits: procedure (Obj: Pointer);
    ScrollTo: procedure (DeltaX, DeltaY: LongInt; Obj: Pointer);
    LimitX: function (Obj: Pointer): LongInt;
    LimitY: function (Obj: Pointer): LongInt;
    StoreUndoInfo: procedure (What: word; Where: TPoint; var Info;
       Obj: Pointer);
    KeyMapConvertStr: function (S: LongString; toAscii: Boolean;
       Obj: Pointer): LongString;
    KeyMapAtInsert: procedure (n: LongInt; P: PLongString; Obj: Pointer);
    KeyMapAtReplace: procedure (n: LongInt; P: PLongString; Obj: Pointer);
    end;

  PTEditWindow = ^TTEditWindow;
  TTEditWindow = packed record
    VMT: PEditWindowVMT;
    Init: function (R: TRect; FileName: String; VMT, Obj: Pointer)
    : Pointer;
    Load: function (var S: TStream; VMT, Obj: Pointer): Pointer;
    Store: procedure (var S: TStream; Obj: Pointer);
    end;

  PTPercentGauge = ^TTPercentGauge;
  TTPercentGauge = packed record
    VMT: PPercentGaugeVMT;
    Init: function (var Bounds: TRect; AMaxValue: LongInt;
       VMT, Obj: Pointer): Pointer;
    AddProgress: procedure (Progress: LongInt; Obj: Pointer);
    SolveForX: function (Y, Z: LongInt; Obj: Pointer): integer;
    SolveForY: function (X, Z: LongInt; Obj: Pointer): integer;
    end;

  PTBarGauge = ^TTBarGauge;
  TTBarGauge = packed record
    VMT: PBarGaugeVMT;
    end;

  PTWhileView = ^TTWhileView;
  TTWhileView = packed record
    VMT: PWhileViewVMT;
    Init: function (Bounds: TRect; VMT, Obj: Pointer): Pointer;
    Write: procedure (n: integer; S: String; Obj: Pointer);
    ClearInterior: procedure (Obj: Pointer);
    end;

  PTViewScroll = ^TTViewScroll;
  TTViewScroll = packed record
    VMT: PViewScrollVMT;
    GetPartCode: function (Obj: Pointer): LongInt;
    GetSize: function (Obj: Pointer): integer;
    DrawPos: procedure (Pos: integer; Obj: Pointer);
    end;

  PTFileViewer = ^TTFileViewer;
  TTFileViewer = packed record
    VMT: PFileViewerVMT;
    Init: function (var Bounds: TRect; AStream: PStream;
       const AFileName, AVFileName: String; ASB: PView;
       Quick, Hex: Boolean; VMT, Obj: Pointer): Pointer;
    Load: function (var S: TStream; VMT, Obj: Pointer): Pointer;
    Store: procedure (var S: TStream; Obj: Pointer);
    SetXlatFile: procedure (const FName: String; DoRedraw: Boolean;
       Obj: Pointer);
    ReadFile: function (const FName, VFName: String; NewStream: Boolean;
       Obj: Pointer): Boolean;
    WriteModify: function (Obj: Pointer): Boolean;
    Seek: procedure (APos: LongInt; Obj: Pointer);
    SaveToFile: procedure (FN: String; Obj: Pointer);
    DoHighlite: procedure (var B; const S: String; const Attr: String;
       Obj: Pointer);
    SeekEof: procedure (Obj: Pointer);
    SeekBof: procedure (Obj: Pointer);
    BreakOnStreamReadError: function (Obj: Pointer): Boolean;
    end;

  PTDrive = ^TTDrive;
  TTDrive = packed record
    VMT: PDriveVMT;
    Init: function (ADrive: Byte; AOwner: Pointer; Num: Byte;
       VMT, Obj: Pointer): Pointer;
    Load: function (var S: TStream; VMT, Obj: Pointer): Pointer;
    end;

  PTFindDrive = ^TTFindDrive;
  TTFindDrive = packed record
    VMT: PFindDriveVMT;
    Init: function (const AName: String; ADirs: PCollection;
       AFiles: PFilesCollection; Num: Byte; VMT, Obj: Pointer): Pointer;
    InitList: function (const AName: String; VMT, Obj: Pointer): Pointer;
    Load: function (var S: TStream; VMT, Obj: Pointer): Pointer;
    NewUpFile: procedure (Obj: Pointer);
    end;

  PTTempDrive = ^TTTempDrive;
  TTTempDrive = packed record
    VMT: PTempDriveVMT;
    Init: function (Num: Byte; VMT, Obj: Pointer): Pointer;
    Load: function (var S: TStream; VMT, Obj: Pointer): Pointer;
    end;

  PTArcDrive = ^TTArcDrive;
  TTArcDrive = packed record
    VMT: PArcDriveVMT;
    Init: function (const AName, VAName: String; ViewMode: Byte;
       VMT, Obj: Pointer): Pointer;
    InitCol: function (PC: Pointer {PDirStorage};
       const AName, VAName: String; VMT, Obj: Pointer): Pointer;
    Load: function (var S: TStream; VMT, Obj: Pointer): Pointer;
    ReadArchive: function (Obj: Pointer): Boolean;
    Exec: function (Prg, Cmd: String; Lst: AnsiString; B: Boolean;
       Obj: Pointer): Boolean;
    MakeListFile: function (PC: PCollection; UseUnp: Boolean;
       var B: Boolean; Obj: Pointer): AnsiString;
    ExtractFiles: procedure (AFiles: PCollection; ExtrDir: String;
       Own: PView; Options: Byte; Obj: Pointer);
    StdMsg4: procedure (Obj: Pointer);
    end;

  PTArvidDrive = ^TTArvidDrive;
  TTArvidDrive = packed record
    VMT: PArvidDriveVMT;
    Init: function (const AName: String; Num: Byte; VMT, Obj: Pointer)
    : Pointer;
    SeekDirectory: procedure (Obj: Pointer);
    end;

var
  DNFunctions: ^TDNFunctions;

  DNMethods: packed record
    end;
  {begin of DNMethods}
  RecordSize: integer;
  Reserved1: integer;
  Reserved2: integer;
  _TEmptyObject: ^TTEmptyObject;
  _TObject: ^TTObject;
  _TRegExp: ^TTRegExp;
  _T1: Pointer;
  _T2: Pointer;
  _T3: Pointer;
  _TStream: ^TTStream;
  _TDosStream: ^TTDosStream;
  _TBufStream: ^TTBufStream;
  _TMemoryStream: ^TTMemoryStream;
  _TCollection: ^TTCollection;
  _TSortedCollection: ^TTSortedCollection;
  _TLineCollection: ^TTLineCollection;
  _TStringCollection: ^TTStringCollection;
  _TStrCollection: ^TTStrCollection;
  _TFilesCollection: ^TTFilesCollection;
  _TView: ^TTView;
  _TFrame: ^TTFrame;
  _TScrollBar: ^TTScrollBar;
  _TGroup: ^TTGroup;
  _TWindow: ^TTWindow;
  _TMenuView: ^TTMenuView;
  _TMenuBar: ^TTMenuBar;
  _TMenuBox: ^TTMenuBox;
  _TMenuPopup: ^TTMenuPopup;
  _TStatusLine: ^TTStatusLine;
  _TDialog: ^TTDialog;
  _TInputLine: ^TTInputLine;
  _TButton: ^TTButton;
  _TCluster: ^TTCluster;
  _TRadioButtons: ^TTRadioButtons;
  _TCheckBoxes: ^TTCheckBoxes;
  _TMultiCheckBoxes: ^TTMultiCheckBoxes;
  _TScroller: ^TTScroller;
  _TListViewer: ^TTListViewer;
  _TListBox: ^TTListBox;
  _TStaticText: ^TTStaticText;
  _TParamText: ^TTParamText;
  _TLabel: ^TTLabel;
  _THistoryViewer: ^TTHistoryViewer;
  _THistoryWindow: ^TTHistoryWindow;
  _THistory: ^TTHistory;
  _TBackground: ^TTBackground;
  _TDesktop: ^TTDesktop;
  _TProgram: ^TTProgram;
  _TApplication: ^TTApplication;
  _TDNApplication: ^TTDNApplication;
  _TUniWindow: ^TTUniWindow;
  _TXFileEditor: ^TTXFileEditor;
  _TEditWindow: ^TTEditWindow;
  _TPercentGauge: ^TTPercentGauge;
  _TBarGauge: ^TTBarGauge;
  _TWhileView: ^TTWhileView;
  _TViewScroll: ^TTViewScroll;
  _TFileViewer: ^TTFileViewer;
  _TDrive: ^TTDrive;
  _TFindDrive: ^TTFindDrive;
  _TTempDrive: ^TTTempDrive;
  _TArcDrive: ^TTArcDrive;
  _TArvidDrive: ^TTArvidDrive;
  {end of DNMethods}

implementation

procedure InitDNFunctions(Functions, Methods: Pointer);
  begin
  DNFunctions := Functions;
  Move(Methods^, DNMethods, PLongInt(Methods)^);
  SetMemoryManager(TMemoryManager(DNFunctions^.MemoryManager));
  end;

procedure TransportVMT(DNObjVMT, OldVMT, NewVMT: Pointer;
     VMTSize: integer);
  const
    FirstMethodInVMT = 3;
  var
    I: integer;
    Ptr1: PPointerArray absolute DNObjVMT;
    Ptr2: PPointerArray absolute OldVMT;
    Ptr3: PPointerArray absolute NewVMT;
  begin
  for I := FirstMethodInVMT to VMTSize-1+FirstMethodInVMT do
    if Ptr3^[I] = Ptr2^[I] then
      Ptr3^[I] := Ptr1^[I];
  end;

function DuplicateVMT(VMT: Pointer; VMTSize: integer): Pointer;
  begin
  GetMem(Result, VMTSize*4);
  if Result <> nil then
    Move(VMT^, Result^, VMTSize*4);
  end;

procedure SetVmt(Obj, VMT: Pointer);
  begin
  PPointer(Obj)^:= VMT;
  end;

end.
