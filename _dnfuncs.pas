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
  _Defines, _Model1;

procedure InitDNFunctions(Functions, Methods: Pointer);
procedure TransportVMT(DNObjVMT, OldVMT, NewVMT: Pointer; VMTSize:
    integer);
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
    SetEditorEventHook: function (EditorEventHook: TEditorEventHook):
      boolean;
    RemoveEditorEventHook: procedure (EditorEventHook:
      TEditorEventHook);
    end;

  PSpecialFunctions = ^TSpecialFunctions;
  TSpecialFunctions = packed record
    RuntimePatch: function (OldFunc, NewFunc: Pointer): boolean;
    end;

  {&AlignRec+}
  PSystemVars = ^TSystemVars;
  TSystemVars = record
    ExitCode: longInt;
    ErrorAddr: Pointer;
    ExceptionNo: longInt;
    TlsSharedMem: Pointer;
    TlsSharedMemSize: longInt;
    DebugHook: boolean;
    IsConsole: boolean;
    IsMultiThread: boolean;
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
    ModuleHandle: longInt;
    RandSeed: longInt;
    AllocMemCount: longInt;
    AllocMemSize: longInt;
    SmHeapBlock: longInt;
    LgHeapBlock: longInt;
    HeapLimit: longInt;
    HeapAllocFlags: longInt;
    HeapSemaphore: longInt;
    Test8086: byte;
    Test8087: byte;
    {additional}
    HInstance: longInt;
    HPrevInst: longInt;
    CmdShow: longInt;
    NotForYou: longInt;
    HeapBlockList: Pointer;
    hblNext: longInt;
    hblAlloc: longInt;
    MemoryManager: TMemoryManager;
    end;
  {&AlignRec-}

  TDNFunctions = packed record
    DN2Version: integer;
    APIVersion: integer;
    reserved1: integer;
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
      GetMem: function (Size: longInt): Pointer;
      FreeMem: function (P: Pointer): longInt;
      ReallocMem: function (P: Pointer; Size: longInt): Pointer;
      end;

    TinySlice: procedure;

    Evalue: function (const s: String; CCV: Pointer): Extended;
    EvalueError: ^Boolean;

    DOSError: function : integer;

    lFindFirst: procedure (const Path: String; Attr: word; var R:
      lSearchRec);
    lFindNext: procedure (var R: lSearchRec);
    lFindClose: procedure (var R: lSearchRec);

    CopyFileRec: function (fr: PFileRec): PFileRec;
    CreateFileRec: function (Name: String): PFileRec;
    NewFileRec: function (const {$IFNDEF OS2}Lfn, {$ENDIF}Name:
      String; Size: TSize; Date, CreationDate, LastAccDate: longInt;
      Attr: word; AOwner: PString): PFileRec;
    DelFileRec: procedure (var fr: PFileRec);
    LoadFileRec: function (var s: TStream): PFileRec;
    StoreFileRec: procedure (var s: TStream; fr: PFileRec);
    LoadFileRecOwn: function (var s: TStream; Dirs: PCollection):
      PFileRec;
    StoreFileRecOwn: procedure (var s: TStream; fr: PFileRec; Dirs:
      PCollection);

    GetActivePanel: function : Pointer;
    GetPassivePanel: function : Pointer;
    GetSelection: function (P: Pointer; Single: boolean):
      PFilesCollection;
    ClearSelection: procedure (AFP: Pointer; FC: Pointer);

    NewStr: function (const s: String): PString;
    NewLongStr: function (const s: LongString): PLongString;
    DisposeStr: procedure (var P: PString);
    DisposeLongStr: procedure (var P: PLongString);
    CnvString: function (P: PString): String;
    CnvLongString: function (P: PLongString): LongString;

    UpStr: procedure (var s: String);
    UpLongStr: procedure (var s: LongString);
    LowStr: procedure (var s: String);
    LowLongStr: procedure (var s: LongString);

    FormatStr: procedure (var Result: String; const Format: String;
      var Params);
    MoveColor: procedure (var Buf; Num: word; Attr: byte);
    MoveBuf: procedure (var Dest; var Source; Attr: byte; Count:
      word);
    MoveChar: procedure (var Dest; C: Char; Attr: byte; Count: word);
    MoveCStr: procedure (var Dest; const Str: String; Attrs: word);
    MoveStr: procedure (var Dest; const Str: String; Attr: byte);
    CStrLen: function (const s: String): integer;

    ExecView: function (P: Pointer): integer;
    Reserved4: integer;

    GetString: function (Index: integer): String;
    ExecResource: function (Key: integer; var Data): word;
    LoadResource: function (Key: integer): PObject;

    OpenRez: function (const PluginName: String): longInt;
    OpenRezX: function (const PluginName: String): longInt;
    CloseRez: procedure (RezId: longInt);
    GetRezString: function (RezId: longInt; ItemId: SmallWord):
      String;
    GetRezObject: function (RezId: longInt; ItemId: SmallWord):
      PObject;

    NewSItem: function (const Str: String; ANext: PSItem): PSItem;

    NewItem: function (Name, Param: TMenuStr; KeyCode: word; Command:
      word; AHelpCtx: word; Next: PMenuItem): PMenuItem;
    NewLine: function (Next: PMenuItem): PMenuItem;
    NewSubMenu: function (Name: TMenuStr; AHelpCtx: word; SubMenu:
      PMenu; Next: PMenuItem): PMenuItem;
    NewMenu: function (Items: PMenuItem): PMenu;
    DisposeMenu: procedure (Menu: PMenu);
    ExecAndDisposeMenu: function (Menu: PMenu): integer;
    StoreMenuDefaults: procedure (Menu: PMenu; var s: TStream);
    LoadMenuDefaults: procedure (Menu: PMenu; var s: TStream);
    LookUpMenu: function (Menu: PMenu; idCheckItem: word; Flags:
      word): PMenuItem;
    MenuIndexOf: function (Menu: PMenu; idCheckItem: PMenuItem):
      word;

    NewStatusDef: function (AMin, AMax: word; AItems: PStatusItem;
      ANext: PStatusDef): PStatusDef;
    NewStatusKey: function (const AText: String; AKeyCode: word;
      ACommand: word; ANext: PStatusItem): PStatusItem;

    LngId: function : String;
    HelpLngId: function : String;

    RegisterType: procedure (var s: TStreamRec);
    ReRegisterType: procedure (var s: TStreamRec);

    Message: function (Receiver: PView; What, Command: word; InfoPtr:
      Pointer): Pointer;
    MessageL: function (Receiver: PView; What, Command: word;
      InfoLng: longInt): Pointer;

    RegisterToPrior: procedure (P: PView);
    RegisterToBackground: procedure (P: PView);
    Deregister: procedure (P: PView);
    UpdateAll: procedure (All: boolean);

    GetWinNumber: function : AInt;

    MessageBox: function (Msg: String; Params: Pointer; AOptions:
      word): word;
    MessageBox2: function (Msg1, Msg2: String; Params1, Params2:
      Pointer; AOptions: word): word;
    MessageBoxRect: function (var R: TRect; Msg: String; Params:
      Pointer; AOptions: word): word;
    MessageBox2Rect: function (var R: TRect; Msg1, Msg2: String;
      Lines1: word; Params1, Params2: Pointer; AOptions: word): word;
    InputBox: function (Title: String; ALabel: String; var s: String;
      Limit: word; HistoryId: word): word;
    BigInputBox: function (Title: String; ALabel: String; var s:
      String; Limit: word; HistoryId: word): word;
    InputBoxRect: function (var Bounds: TRect; Title: String; ALabel:
      String; var s: String; Limit: word; HistoryId: word): word;

    GetFileNameDialog: function (Mask, Title, Name: String; Buttons,
      HistoryId: word): String;
    GetFileNameMenu: function (Path, Mask, Default: String;
      PutNumbers: boolean; var More, None: boolean): String;

    Reserved5: integer;
    Reserved6: integer;

    UpdateWriteView: procedure (P: Pointer);
    GlobalMessage: function (What, Command: word; InfoPtr: Pointer):
      Pointer;
    GlobalMessageL: function (What, Command: word; InfoLng: longInt):
      Pointer;
    GlobalEvent: procedure (What, Command: word; InfoPtr: Pointer);
    ViewPresent: function (Command: word; InfoPtr: Pointer): PView;
    WriteMsg: function (text: String): PView;
    ForceWriteShow: procedure (P: Pointer);
    ToggleCommandLine: procedure (OnOff: boolean);
    AdjustToDesktopSize: procedure (var R: TRect; OldDeskSize:
      TPoint);

    Reserved7: integer;
    Reserved8: integer;

    HistoryAdd: procedure (Id: byte; const Str: String);
    HistoryCount: function (Id: byte): word;
    HistoryStr: function (Id: byte; Index: integer): String;
    DeleteHistoryStr: procedure (Id: byte; Index: integer);

    Reserved9: integer;
    Reserved10: integer;

    GetMouseEvent: procedure (var Event: TEvent);
    GetKeyEvent: procedure (var Event: TEvent);

    DispWhileViewEvents: procedure (InfoView: PWhileView; var
      CancelParam: boolean);

    Reserved11: integer;

    SetTitle: procedure (text: String);

    SetWinClip: function (PC: PLineCollection): boolean;
    GetWinClip: function (var PCL: PLineCollection): boolean;
    GetWinClipSize: function : boolean;
    SyncClipIn: procedure;
    SyncClipOut: procedure;
    CopyLines2Stream: procedure (PC: PCollection; var PCS: PStream);
    CopyStream2Lines: procedure (PCS: PStream; var PC: PCollection);

    NewTimerSecs: procedure (var ET: TEventTimer; Secs: longInt);
    NewTimer: procedure (var ET: TEventTimer; Tics: longInt);
    TimerExpired: function (ET: TEventTimer): boolean;
    ElapsedTime: function (ET: TEventTimer): longInt;
    ElapsedTimeInSecs: function (ET: TEventTimer): longInt;

    GetPossibleDizOwner: function (n: integer): String;
    GetDizOwner: function (const Path, LastOwner: String; Add:
      boolean): String;
    CalcDizPath: function (P: PDiz; Owen: PString): String;
    ReplaceDiz: procedure (const DPath, Name: String; ANewName:
      PString; ANewDescription: PString);
    DeleteDiz: procedure (const DPath, Name: String);
    GetDiz: function (const DPath: String; var Line: longInt; const
      Name: String): String;
    SetDescription: procedure (PF: PFileRec; DizOwner: String);

    Reserved12: integer;
    Reserved13: integer;

    SelectFiles: function (AFP: Pointer; Select, XORs: boolean):
      boolean;
    InvertSelection: procedure (AFP: Pointer; dr: boolean);
    DragMover: procedure (AP: Pointer; text: String; AFC, AC:
      Pointer);
    CM_AdvancedFilter: procedure (AFP: Pointer);
    CM_ArchiveFiles: procedure (AFP: Pointer);
    {CM_Branch: procedure(AFP: Pointer);}
    CM_ChangeDirectory: function (AFP: Pointer): String;
    CM_ChangeCase: procedure (AFP: Pointer);
    CM_CompareDirs: procedure (AFP, IP: Pointer);
    CM_CopyFiles: procedure (AFP: Pointer; MoveMode, Single: boolean);
    CM_CopyTemp: procedure (AFP: Pointer);
    CM_DragDropper: procedure (AFP: Pointer; CurPos: integer; EV:
      Pointer);
    CM_Dropped: procedure (AFP, EI: Pointer);
    CM_EraseFiles: procedure (AFP: Pointer; Single: boolean);
    CM_LongCopy: procedure (AFP: Pointer);
    CM_MakeDir: procedure (AFP: Pointer);
    CM_MakeList: procedure (AFP: Pointer);
    CM_RenameSingleL: procedure (AFP, PEV: Pointer);
    CM_RenameSingleDialog: procedure (AFP, PEV: Pointer);
    CM_SelectColumn: procedure (AFP: Pointer);
    CM_SetAttributes: procedure (AFP: Pointer; Single: boolean;
      CurPos: integer);
    CM_SetShowParms: procedure (AFP: Pointer);
    CM_SortBy: procedure (AFP: Pointer);
    CM_ToggleLongNames: procedure (AFP: Pointer);
    CM_ToggleShowMode: procedure (AFP: Pointer);
    CM_ToggleDescriptions: procedure (AFP: Pointer);

    Reserved14: integer;
    Reserved15: integer;

    ExecString: procedure (s: PString; WS: String);
    SearchExt: function (FileRec: PFileRec; var HS: String): boolean;
    ExecExtFile: function (const ExtFName: String; UserParams:
      PUserParams; SIdx: integer): boolean;
    ExecFile: procedure (const FileName: String);
    AnsiExec: procedure (const Path: String; const ComLine:
      AnsiString);

    Reserved16: integer;
    Reserved17: integer;

    SelectDrive: function (X, Y: integer; Default: Char; IncludeTemp:
      boolean): String;
    GetFileType: function (const s: String; Attr: byte): integer;
    DosReread: procedure (Files: PFilesCollection);

    FnMatch: function (Pattern, Str: String): boolean;
    SearchFileStr: function (F: PStream; var XLAT: TXlat; const What:
      String; Pos: longInt;
    CaseSensitive, Display, WholeWords, Back, AllCP, IsRegExp:
      boolean): longInt;
    MakeListFile: procedure (APP: Pointer; Files: PCollection);
    ASCIITable: procedure;
    InsertCalendar: procedure;
    InsertCalc: procedure;
    ChangeColors: procedure;
    WindowManager: procedure;
    SetHighlightGroups: procedure;
    UnpackDiskImages: procedure (AOwner: Pointer; Files:
      PFilesCollection);
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
    CompileString: function (const AExpression: String; Obj: Pointer):
      boolean;
    CompileStr: function (AExpression: PChar; Obj: Pointer): boolean;
    Compile: function (AExpression: PChar; ALength: integer; Obj:
      Pointer): boolean;
    Execute: function (AString: PChar; ALength: integer; Obj:
      Pointer): boolean;
    SubstituteString: function (ASrc: PChar; const AReplace: String;
      var ADest: String; Obj: Pointer): boolean;
    SubstituteStr: function (ASrc, AReplace: PChar; ADest: PChar;
      var ALength: integer; Obj: Pointer): boolean;
    Substitute: function (ASrc, AReplace: PChar; ARLen: integer;
      ADest: PChar; var ADLen: integer; Obj: Pointer): boolean;
    end;

  PTStream = ^TTStream;
  TTStream = packed record
    VMT: PStreamVMT;
    CopyFrom: procedure (var s: TStream; Count: longInt; Obj:
      Pointer);
    Get: function (Obj: Pointer): PObject;
    Put: procedure (P: PObject; Obj: Pointer);
    ReadStr: function (Obj: Pointer): PString;
    ReadLongStr: function (Obj: Pointer): PLongString;
    ReadStrV: procedure (var s: String; Obj: Pointer);
    ReadLongStrV: procedure (var s: LongString; Obj: Pointer);
    Reset: procedure (Obj: Pointer);
    StrRead: function (Obj: Pointer): PChar;
    StrWrite: procedure (P: PChar; Obj: Pointer);
    WriteStr: procedure (P: PString; Obj: Pointer);
    WriteLongStr: procedure (P: PLongString; Obj: Pointer);
    Eof: function (Obj: Pointer): boolean;
    end;

  PTDosStream = ^TTDosStream;
  TTDosStream = packed record
    VMT: PDosStreamVMT;
    Init: function (FileName: String; Mode: word; VMT, Obj: Pointer):
      Pointer;
    Open: procedure (FileName: String; Mode: word; Obj: Pointer);
    end;

  PTBufStream = ^TTBufStream;
  TTBufStream = packed record
    VMT: PBufStreamVMT;
    Init: function (FileName: String; Mode: word; Size: longInt; VMT,
      Obj: Pointer): Pointer;
    end;

  PTMemoryStream = ^TTMemoryStream;
  TTMemoryStream = packed record
    VMT: PMemoryStreamVMT;
    Init: function (ALimit: longInt; ABlockSize: word; VMT, Obj:
      Pointer): Pointer;
    end;

  PTCollection = ^TTCollection;
  TTCollection = packed record
    VMT: PCollectionVMT;
    Init: function (ALimit, ADelta: longInt; VMT, Obj: Pointer):
      Pointer;
    Load: function (var s: TStream; VMT, Obj: Pointer): Pointer;
    At: function (Index: longInt; Obj: Pointer): Pointer;
    AtDelete: procedure (Index: longInt; Obj: Pointer);
    AtFree: procedure (Index: longInt; Obj: Pointer);
    AtInsert: procedure (Index: longInt; Item: Pointer; Obj: Pointer);
    AtPut: procedure (Index: longInt; Item: Pointer; Obj: Pointer);
    AtReplace: procedure (Index: longInt; Item: Pointer; Obj:
      Pointer);
    Delete: procedure (Item: Pointer; Obj: Pointer);
    DeleteAll: procedure (Obj: Pointer);
    FirstThat: function (Test: Pointer; Obj: Pointer): Pointer;
    ForEach: procedure (Action: Pointer; Obj: Pointer);
    Free: procedure (Item: Pointer; Obj: Pointer);
    FreeAll: procedure (Obj: Pointer);
    LastThat: function (Test: Pointer; Obj: Pointer): Pointer;
    Pack: procedure (Obj: Pointer);
    Store: procedure (var s: TStream; Obj: Pointer);
    end;

  PTSortedCollection = ^TTSortedCollection;
  TTSortedCollection = packed record
    VMT: PSortedCollectionVMT;
    Init: function (ALimit, ADelta: longInt; VMT, Obj: Pointer):
      Pointer;
    Load: function (var s: TStream; VMT, Obj: Pointer): Pointer;
    Store: procedure (var s: TStream; Obj: Pointer);
    Sort: procedure (Obj: Pointer);
    end;

  PTLineCollection = ^TTLineCollection;
  TTLineCollection = packed record
    VMT: PLineCollectionVMT;
    Init: function (ALimit, ADelta: longInt; ALongStrings: boolean;
      VMT, Obj: Pointer): Pointer;
    end;

  PTStringCollection = ^TTStringCollection;
  TTStringCollection = packed record
    VMT: PStringCollectionVMT;
    Init: function (ALimit, ADelta: longInt; ALongStrings: boolean;
      VMT, Obj: Pointer): Pointer;
    end;

  PTStrCollection = ^TTStrCollection;
  TTStrCollection = packed record
    VMT: PStrCollectionVMT;
    Init: function (ALimit, ADelta: longInt; VMT, Obj: Pointer):
      Pointer;
    end;

  PTFilesCollection = ^TTFilesCollection;
  TTFilesCollection = packed record
    VMT: PFilesCollectionVMT;
    Load: function (var s: TStream; VMT, Obj: Pointer): Pointer;
    end;

  PTView = ^TTView;
  TTView = packed record
    VMT: PViewVMT;
    Init: function (var Bounds: TRect; VMT, Obj: Pointer): Pointer;
    Load: function (var s: TStream; VMT, Obj: Pointer): Pointer;
    BlockCursor: procedure (Obj: Pointer);
    ClearEvent: procedure (var Event: TEvent; Obj: Pointer);
    CommandEnabled: function (Command: word; Obj: Pointer): boolean;
    DisableCommands: procedure (Commands: TCommandSet; Obj: Pointer);
    DragView: procedure (Event: TEvent; Mode: byte; var Limits:
      TRect; MinSize, MaxSize: TPoint; Obj: Pointer);
    DrawView: procedure (Obj: Pointer);
    EnableCommands: procedure (Commands: TCommandSet; Obj: Pointer);
    EventAvail: function (Obj: Pointer): boolean;
    Exposed: function (Obj: Pointer): boolean;
    Focus: function (Obj: Pointer): boolean;
    GetBounds: procedure (var Bounds: TRect; Obj: Pointer);
    GetClipRect: procedure (var Clip: TRect; Obj: Pointer);
    GetColor: function (Color: word; Obj: Pointer): word;
    GetCommands: procedure (var Commands: TCommandSet; Obj: Pointer);
    GetExtent: procedure (var extent: TRect; Obj: Pointer);
    GetPeerViewPtr: procedure (var s: TStream; var P; Obj: Pointer);
    GetState: function (AState: word; Obj: Pointer): boolean;
    GrowTo: procedure (X, Y: longInt; Obj: Pointer);
    Hide: procedure (Obj: Pointer);
    HideCursor: procedure (Obj: Pointer);
    KeyEvent: procedure (var Event: TEvent; Obj: Pointer);
    Locate: procedure (var Bounds: TRect; Obj: Pointer);
    MakeFirst: procedure (Obj: Pointer);
    MakeGlobal: procedure (Source: TPoint; var Dest: TPoint; Obj:
      Pointer);
    MakeLocal: procedure (Source: TPoint; var Dest: TPoint; Obj:
      Pointer);
    MouseEvent: function (var Event: TEvent; Mask: word; Obj:
      Pointer): boolean;
    MouseInView: function (Mouse: TPoint; Obj: Pointer): boolean;
    MoveTo: procedure (X, Y: longInt; Obj: Pointer);
    NextView: function (Obj: Pointer): PView;
    NormalCursor: procedure (Obj: Pointer);
    Prev: function (Obj: Pointer): PView;
    PrevView: function (Obj: Pointer): PView;
    PutInFrontOf: procedure (Target: PView; Obj: Pointer);
    PutPeerViewPtr: procedure (var s: TStream; P: PView; Obj:
      Pointer);
    Select: procedure (Obj: Pointer);
    SetBounds: procedure (var Bounds: TRect; Obj: Pointer);
    SetCommands: procedure (Commands: TCommandSet; Obj: Pointer);
    SetCursor: procedure (X, Y: longInt; Obj: Pointer);
    Show: procedure (Obj: Pointer);
    ShowCursor: procedure (Obj: Pointer);
    Store: procedure (var s: TStream; Obj: Pointer);
    TopView: function (Obj: Pointer): PView;
    WriteBuf: procedure (X, Y, W, H: integer; var Buf; Obj: Pointer);
    WriteChar: procedure (X, Y: integer; C: Char; Color: byte; Count:
      integer; Obj: Pointer);
    WriteLine: procedure (X, Y, W, H: integer; var Buf; Obj: Pointer);
    WriteStr: procedure (X, Y: integer; Str: String; Color: byte;
      Obj: Pointer);
    MenuEnabled: function (Command: word; Obj: Pointer): boolean;
    DrawCursor: procedure (Obj: Pointer);
    DrawHide: procedure (LastView: PView; Obj: Pointer);
    DrawShow: procedure (LastView: PView; Obj: Pointer);
    DrawUnderRect: procedure (var R: TRect; LastView: PView; Obj:
      Pointer);
    DrawUnderView: procedure (DoShadow: boolean; LastView: PView;
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
    Load: function (var s: TStream; VMT, Obj: Pointer): Pointer;
    SetParams: procedure (AValue, AMin, AMax, APgStep, AArStep:
      longInt; Obj: Pointer);
    SetRange: procedure (AMin, AMax: longInt; Obj: Pointer);
    SetStep: procedure (APgStep, AArStep: longInt; Obj: Pointer);
    SetValue: procedure (AValue: longInt; Obj: Pointer);
    Store: procedure (var s: TStream; Obj: Pointer);
    end;

  PTGroup = ^TTGroup;
  TTGroup = packed record
    VMT: PGroupVMT;
    Init: function (var Bounds: TRect; VMT, Obj: Pointer): Pointer;
    Load: function (var s: TStream; VMT, Obj: Pointer): Pointer;
    Delete: procedure (P: PView; Obj: Pointer);
    ExecView: function (P: PView; Obj: Pointer): word;
    First: function (Obj: Pointer): PView;
    FirstThat: function (P: Pointer; Obj: Pointer): PView;
    LastThat: function (P: Pointer; Obj: Pointer): PView;
    FocusNext: function (Forwards: boolean; Obj: Pointer): boolean;
    ForEach: procedure (P: Pointer; Obj: Pointer);
    GetSubViewPtr: procedure (var s: TStream; var P; Obj: Pointer);
    Insert: procedure (P: PView; Obj: Pointer);
    InsertBefore: procedure (P, Target: PView; Obj: Pointer);
    Lock: procedure (Obj: Pointer);
    PutSubViewPtr: procedure (var s: TStream; P: PView; Obj: Pointer);
    SelectNext: procedure (Forwards: boolean; Obj: Pointer);
    Store: procedure (var s: TStream; Obj: Pointer);
    UnLock: procedure (Obj: Pointer);
    FreeBuffer: procedure (Obj: Pointer);
    GetBuffer: procedure (Obj: Pointer);
    InsertView: procedure (P, Target: PView; Obj: Pointer);
    SetCurrent: procedure (P: PView; Mode: TSelectMode; Obj: Pointer);
    end;

  PTWindow = ^TTWindow;
  TTWindow = packed record
    VMT: PWindowVMT;
    Init: function (var Bounds: TRect; const ATitle: String; ANumber:
      integer; VMT, Obj: Pointer): Pointer;
    Load: function (var s: TStream; VMT, Obj: Pointer): Pointer;
    StandardScrollBar: function (AOptions: word; Obj: Pointer):
      PScrollBar;
    Store: procedure (var s: TStream; Obj: Pointer);
    end;

  PTMenuView = ^TTMenuView;
  TTMenuView = packed record
    VMT: PMenuViewVMT;
    Init: function (var Bounds: TRect; VMT, Obj: Pointer): Pointer;
    Load: function (var s: TStream; VMT, Obj: Pointer): Pointer;
    FindItem: function (Ch: Char; Obj: Pointer): PMenuItem;
    HotKey: function (KeyCode: word; Obj: Pointer): PMenuItem;
    Store: procedure (var s: TStream; Obj: Pointer);
    end;

  PTMenuBar = ^TTMenuBar;
  TTMenuBar = packed record
    VMT: PMenuBarVMT;
    Init: function (var Bounds: TRect; AMenu: PMenu; VMT, Obj:
      Pointer): Pointer;
    end;

  PTMenuBox = ^TTMenuBox;
  TTMenuBox = packed record
    VMT: PMenuBoxVMT;
    Init: function (var Bounds: TRect; AMenu: PMenu; AParentMenu:
      PMenuView; VMT, Obj: Pointer): Pointer;
    end;

  PTMenuPopup = ^TTMenuPopup;
  TTMenuPopup = packed record
    VMT: PMenuPopupVMT;
    Init: function (var Bounds: TRect; AMenu: PMenu; VMT, Obj:
      Pointer): Pointer;
    end;

  PTStatusLine = ^TTStatusLine;
  TTStatusLine = packed record
    VMT: PStatusLineVMT;
    Init: function (var Bounds: TRect; ADefs: PStatusDef; VMT, Obj:
      Pointer): Pointer;
    Load: function (var s: TStream; VMT, Obj: Pointer): Pointer;
    Store: procedure (var s: TStream; Obj: Pointer);
    end;

  PTDialog = ^TTDialog;
  TTDialog = packed record
    VMT: PDialogVMT;
    Init: function (var Bounds: TRect; const ATitle: String; VMT,
      Obj: Pointer): Pointer;
    Load: function (var s: TStream; VMT, Obj: Pointer): Pointer;
    end;

  PTInputLine = ^TTInputLine;
  TTInputLine = packed record
    VMT: PInputLineVMT;
    Init: function (var Bounds: TRect; AMaxLen: AInt; VMT, Obj:
      Pointer): Pointer;
    Load: function (var s: TStream; VMT, Obj: Pointer): Pointer;
    SelectAll: procedure (Enable: boolean; Obj: Pointer);
    SetValidator: procedure (AValid: Pointer {PValidator}; Obj:
      Pointer);
    Store: procedure (var s: TStream; Obj: Pointer);
    CanScroll: function (Delta: integer; Obj: Pointer): boolean;
    end;

  PTButton = ^TTButton;
  TTButton = packed record
    VMT: PButtonVMT;
    Init: function (var Bounds: TRect; const ATitle: String;
      ACommand: word; AFlags: word; VMT, Obj: Pointer): Pointer;
    Load: function (var s: TStream; VMT, Obj: Pointer): Pointer;
    DrawState: procedure (Down: boolean; Obj: Pointer);
    MakeDefault: procedure (Enable: boolean; Obj: Pointer);
    Store: procedure (var s: TStream; Obj: Pointer);
    end;

  PTCluster = ^TTCluster;
  TTCluster = packed record
    VMT: PClusterVMT;
    Init: function (var Bounds: TRect; AStrings: PSItem; VMT, Obj:
      Pointer): Pointer;
    Load: function (var s: TStream; VMT, Obj: Pointer): Pointer;
    ButtonState: function (Item: integer; Obj: Pointer): boolean;
    DrawBox: procedure (const Icon: String; Marker: Char; Obj:
      Pointer);
    DrawMultiBox: procedure (const Icon, Marker: String; Obj:
      Pointer);
    SetButtonState: procedure (AMask: longInt; Enable: boolean; Obj:
      Pointer);
    Store: procedure (var s: TStream; Obj: Pointer);
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
    Init: function (var Bounds: TRect; AStrings: PSItem; ASelRange:
      byte; AFlags: word; const AStates: String; VMT, Obj: Pointer):
      Pointer;
    Load: function (var s: TStream; VMT, Obj: Pointer): Pointer;
    Store: procedure (var s: TStream; Obj: Pointer);
    end;

  PTScroller = ^TTScroller;
  TTScroller = packed record
    VMT: PScrollerVMT;
    Init: function (var Bounds: TRect; AHScrollBar, AVScrollBar:
      PScrollBar; VMT, Obj: Pointer): Pointer;
    Load: function (var s: TStream; VMT, Obj: Pointer): Pointer;
    ScrollTo: procedure (X, Y: longInt; Obj: Pointer);
    SetLimit: procedure (X, Y: longInt; Obj: Pointer);
    Store: procedure (var s: TStream; Obj: Pointer);
    end;

  PTListViewer = ^TTListViewer;
  TTListViewer = packed record
    VMT: PListViewerVMT;
    Init: function (var Bounds: TRect; ANumCols: longInt;
      AHScrollBar, AVScrollBar: PScrollBar; VMT, Obj: Pointer):
      Pointer;
    Load: function (var s: TStream; VMT, Obj: Pointer): Pointer;
    SetRange: procedure (ARange: longInt; Obj: Pointer);
    Store: procedure (var s: TStream; Obj: Pointer);
    end;

  PTListBox = ^TTListBox;
  TTListBox = packed record
    VMT: PListBoxVMT;
    Init: function (var Bounds: TRect; ANumCols: word; AScrollBar:
      PScrollBar; VMT, Obj: Pointer): Pointer;
    Load: function (var s: TStream; VMT, Obj: Pointer): Pointer;
    Store: procedure (var s: TStream; Obj: Pointer);
    end;

  PTStaticText = ^TTStaticText;
  TTStaticText = packed record
    VMT: PStaticTextVMT;
    Init: function (var Bounds: TRect; const AText: String; VMT, Obj:
      Pointer): Pointer;
    Load: function (var s: TStream; VMT, Obj: Pointer): Pointer;
    Store: procedure (var s: TStream; Obj: Pointer);
    end;

  PTParamText = ^TTParamText;
  TTParamText = packed record
    VMT: PParamTextVMT;
    Init: function (var Bounds: TRect; const AText: String;
      AParamCount: AInt; VMT, Obj: Pointer): Pointer;
    Load: function (var s: TStream; VMT, Obj: Pointer): Pointer;
    Store: procedure (var s: TStream; Obj: Pointer);
    end;

  PTLabel = ^TTLabel;
  TTLabel = packed record
    VMT: PLabelVMT;
    Init: function (var Bounds: TRect; const AText: String; ALink:
      PView; VMT, Obj: Pointer): Pointer;
    Load: function (var s: TStream; VMT, Obj: Pointer): Pointer;
    Store: procedure (var s: TStream; Obj: Pointer);
    end;

  PTHistoryViewer = ^TTHistoryViewer;
  TTHistoryViewer = packed record
    VMT: PHistoryViewerVMT;
    Init: function (var Bounds: TRect; AHScrollBar, AVScrollBar:
      PScrollBar; AHistoryId: AWord; VMT, Obj: Pointer): Pointer;
    HistoryWidth: function (Obj: Pointer): integer;
    end;

  PTHistoryWindow = ^TTHistoryWindow;
  TTHistoryWindow = packed record
    VMT: PHistoryWindowVMT;
    Init: function (var Bounds: TRect; HistoryId: AWord; VMT, Obj:
      Pointer): Pointer;
    end;

  PTHistory = ^TTHistory;
  TTHistory = packed record
    VMT: PHistoryVMT;
    Init: function (var Bounds: TRect; ALink: PInputline; AHistoryId:
      AWord; VMT, Obj: Pointer): Pointer;
    Load: function (var s: TStream; VMT, Obj: Pointer): Pointer;
    Store: procedure (var s: TStream; Obj: Pointer);
    end;

  PTBackground = ^TTBackground;
  TTBackground = packed record
    VMT: PBackgroundVMT;
    Init: function (var Bounds: TRect; APattern: Char; VMT, Obj:
      Pointer): Pointer;
    Load: function (var s: TStream; VMT, Obj: Pointer): Pointer;
    Store: procedure (var s: TStream; Obj: Pointer);
    end;

  PTDesktop = ^TTDesktop;
  TTDesktop = packed record
    VMT: PDesktopVMT;
    Init: function (var Bounds: TRect; VMT, Obj: Pointer): Pointer;
    Load: function (var s: TStream; VMT, Obj: Pointer): Pointer;
    Cascade: procedure (var R: TRect; Obj: Pointer);
    Clear: procedure (Obj: Pointer);
    Store: procedure (var s: TStream; Obj: Pointer);
    Tile: procedure (var R: TRect; Obj: Pointer);
    end;

  PTProgram = ^TTProgram;
  TTProgram = packed record
    VMT: PProgramVMT;
    Init: function (VMT, Obj: Pointer): Pointer;
    CanMoveFocus: function (Obj: Pointer): boolean;
    ExecuteDialog: function (P: PDialog; Data: Pointer; Obj: Pointer):
      word;
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
    ViewFile: procedure (AltExt, NoExtFile: boolean; FileName:
      String; Obj: Pointer);
    AddFormat: procedure (Obj: Pointer);
    EditFile: procedure (Intern: boolean; FileName: String; Obj:
      Pointer);
    RetrieveDesktop: procedure (const FileName: String; LS: PStream;
      LoadColors: boolean; Obj: Pointer);
    SaveDesktop: procedure (const FileName: String; Obj: Pointer);
    LoadDesktop: procedure (var s: TStream; Obj: Pointer);
    StoreDesktop: procedure (var s: TStream; Obj: Pointer);
    ChgColors: procedure (Obj: Pointer);
    end;

  PTUniWindow = ^TTUniWindow;
  TTUniWindow = packed record
    VMT: PUniWindowVMT;
    MakeScrollBar: function (AOptions: word; Obj: Pointer):
      PScrollBar;
    end;

  PTXFileEditor = ^TTXFileEditor;
  TTXFileEditor = packed record
    VMT: PXFileEditorVMT;
    Init: function (var Bounds: TRect; AHScrollBar, AVScrollBar:
      PScrollBar; var FileName: String; VMT, Obj: Pointer): Pointer;
    Load: function (var s: TStream; VMT, Obj: Pointer): Pointer;
    Store: procedure (var s: TStream; Obj: Pointer);
    DoHighlite: procedure (var B; const s: LongString; const Attr:
      String; Obj: Pointer);
    GetLine: function (Index: longInt; Obj: Pointer): LongString;
    GetSelection: function (Obj: Pointer): PCollection;
    ValidBlock: function (Obj: Pointer): boolean;
    CalcMenu: procedure (Obj: Pointer);
    Search: function (StartX, StartY: word; Obj: Pointer): boolean;
    InsertBlock: procedure (ABlock: PCollection; SaveUndo: boolean;
      Obj: Pointer);
    ModifyLine: procedure (Index: longInt; s: LongString; DelSpaces:
      boolean; Obj: Pointer);
    SetLimits: procedure (Obj: Pointer);
    ScrollTo: procedure (DeltaX, DeltaY: longInt; Obj: Pointer);
    LimitX: function (Obj: Pointer): longInt;
    LimitY: function (Obj: Pointer): longInt;
    StoreUndoInfo: procedure (What: word; Where: TPoint; var Info;
      Obj: Pointer);
    KeyMapConvertStr: function (s: LongString; toAscii: boolean; Obj:
      Pointer): LongString;
    KeyMapAtInsert: procedure (n: longInt; P: PLongString; Obj:
      Pointer);
    KeyMapAtReplace: procedure (n: longInt; P: PLongString; Obj:
      Pointer);
    end;

  PTEditWindow = ^TTEditWindow;
  TTEditWindow = packed record
    VMT: PEditWindowVMT;
    Init: function (R: TRect; FileName: String; VMT, Obj: Pointer):
      Pointer;
    Load: function (var s: TStream; VMT, Obj: Pointer): Pointer;
    Store: procedure (var s: TStream; Obj: Pointer);
    end;

  PTPercentGauge = ^TTPercentGauge;
  TTPercentGauge = packed record
    VMT: PPercentGaugeVMT;
    Init: function (var Bounds: TRect; AMaxValue: longInt; VMT, Obj:
      Pointer): Pointer;
    AddProgress: procedure (Progress: longInt; Obj: Pointer);
    SolveForX: function (Y, Z: longInt; Obj: Pointer): integer;
    SolveForY: function (X, Z: longInt; Obj: Pointer): integer;
    end;

  PTBarGauge = ^TTBarGauge;
  TTBarGauge = packed record
    VMT: PBarGaugeVMT;
    end;

  PTWhileView = ^TTWhileView;
  TTWhileView = packed record
    VMT: PWhileViewVMT;
    Init: function (Bounds: TRect; VMT, Obj: Pointer): Pointer;
    Write: procedure (n: integer; s: String; Obj: Pointer);
    ClearInterior: procedure (Obj: Pointer);
    end;

  PTViewScroll = ^TTViewScroll;
  TTViewScroll = packed record
    VMT: PViewScrollVMT;
    GetPartCode: function (Obj: Pointer): longInt;
    GetSize: function (Obj: Pointer): integer;
    DrawPos: procedure (Pos: integer; Obj: Pointer);
    end;

  PTFileViewer = ^TTFileViewer;
  TTFileViewer = packed record
    VMT: PFileViewerVMT;
    Init: function (var Bounds: TRect; AStream: PStream; const
      AFileName, AVFileName: String; ASB: PView; Quick, Hex: boolean;
      VMT, Obj: Pointer): Pointer;
    Load: function (var s: TStream; VMT, Obj: Pointer): Pointer;
    Store: procedure (var s: TStream; Obj: Pointer);
    SetXlatFile: procedure (const FName: String; DoRedraw: boolean;
      Obj: Pointer);
    ReadFile: function (const FName, VFName: String; NewStream:
      boolean; Obj: Pointer): boolean;
    WriteModify: function (Obj: Pointer): boolean;
    Seek: procedure (APos: longInt; Obj: Pointer);
    SaveToFile: procedure (FN: String; Obj: Pointer);
    DoHighlite: procedure (var B; const s: String; const Attr:
      String; Obj: Pointer);
    SeekEof: procedure (Obj: Pointer);
    SeekBof: procedure (Obj: Pointer);
    BreakOnStreamReadError: function (Obj: Pointer): boolean;
    end;

  PTDrive = ^TTDrive;
  TTDrive = packed record
    VMT: PDriveVMT;
    Init: function (ADrive: byte; AOwner: Pointer; Num: byte; VMT,
      Obj: Pointer): Pointer;
    Load: function (var s: TStream; VMT, Obj: Pointer): Pointer;
    end;

  PTFindDrive = ^TTFindDrive;
  TTFindDrive = packed record
    VMT: PFindDriveVMT;
    Init: function (const AName: String; ADirs: PCollection; AFiles:
      PFilesCollection; Num: byte; VMT, Obj: Pointer): Pointer;
    InitList: function (const AName: String; VMT, Obj: Pointer):
      Pointer;
    Load: function (var s: TStream; VMT, Obj: Pointer): Pointer;
    NewUpFile: procedure (Obj: Pointer);
    end;

  PTTempDrive = ^TTTempDrive;
  TTTempDrive = packed record
    VMT: PTempDriveVMT;
    Init: function (Num: byte; VMT, Obj: Pointer): Pointer;
    Load: function (var s: TStream; VMT, Obj: Pointer): Pointer;
    end;

  PTArcDrive = ^TTArcDrive;
  TTArcDrive = packed record
    VMT: PArcDriveVMT;
    Init: function (const AName, VAName: String; ViewMode: byte; VMT,
      Obj: Pointer): Pointer;
    InitCol: function (PC: Pointer {PDirStorage}; const AName,
      VAName: String; VMT, Obj: Pointer): Pointer;
    Load: function (var s: TStream; VMT, Obj: Pointer): Pointer;
    ReadArchive: function (Obj: Pointer): boolean;
    Exec: function (Prg, Cmd: String; Lst: AnsiString; B: boolean;
      Obj: Pointer): boolean;
    MakeListFile: function (PC: PCollection; UseUnp: boolean; var B:
      boolean; Obj: Pointer): AnsiString;
    ExtractFiles: procedure (AFiles: PCollection; ExtrDir: String;
      Own: PView; Options: byte; Obj: Pointer);
    StdMsg4: procedure (Obj: Pointer);
    end;

  PTArvidDrive = ^TTArvidDrive;
  TTArvidDrive = packed record
    VMT: PArvidDriveVMT;
    Init: function (const AName: String; Num: byte; VMT, Obj:
      Pointer): Pointer;
    SeekDirectory: procedure (Obj: Pointer);
    end;

var
  DNFunctions: ^TDNFunctions;

  DNMethods: packed record
    end;
  {begin of DNMethods}
  RecordSize: integer;
  reserved1: integer;
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

procedure TransportVMT(DNObjVMT, OldVMT, NewVMT: Pointer; VMTSize:
    integer);
  const
    FirstMethodInVMT = 3;
  var
    i: integer;
    Ptr1: PPointerArray absolute DNObjVMT;
    Ptr2: PPointerArray absolute OldVMT;
    Ptr3: PPointerArray absolute NewVMT;
  begin
    for i := FirstMethodInVMT to VMTSize-1+FirstMethodInVMT do
      if Ptr3^[i] = Ptr2^[i] then
        Ptr3^[i] := Ptr1^[i];
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
