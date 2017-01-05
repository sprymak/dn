unit _DNFuncs;
(******

DN/2 Plugin Interface - functional model
Copyright (C) 2002 Aleksej Kozlov (Cat)
2:5030/1326.13

******)

{&Delphi+}

interface

uses
  {Use32,} _Defines, _Model1;

procedure InitDNFunctions(Functions, Methods: Pointer);
procedure TransportVMT(DNObjVMT, OldVMT, NewVMT: Pointer; VMTSize: Integer);
function DuplicateVMT(VMT: Pointer; VMTSize: Integer): Pointer;
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
    LStringList : Pointer;
    Resource:  PIdxResource;
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
    EventCatchersCount: Integer;
    ArchiveViewers: TArchiveViewerArray;
  end;

  PSimpleHooks = ^TSimpleHooks;
  TSimpleHooks = packed record
    SetEditorEventHook: function(EditorEventHook: TEditorEventHook): Boolean;
    RemoveEditorEventHook: procedure(EditorEventHook: TEditorEventHook);
  end;

  TDNFunctions = packed record
    DN2Version: Integer;
    APIVersion: Integer;
    Reserved1: Integer;
    Reserved2: Integer;

    SomeObjects1: PSomeObjects1;
    SomeObjects2: PSomeObjects2;
    SomeObjects3: PSomeObjects3;
    Reserved3: Integer;
    Reserved4: Integer;
    Reserved5: Integer;
    Reserved6: Integer;
    SimpleHooks: PSimpleHooks;

    MemoryManager: packed record
      GetMem: function(Size: Longint): Pointer;
      FreeMem: function(P: Pointer): Longint;
      ReallocMem: function(P: Pointer; Size: Longint): Pointer;
    end;

    TinySlice: procedure;

    Evalue: function(const S: String; VGF: Pointer): Extended;
    EvalueError: ^Boolean;

    DosError: function: Integer;

    lFindFirst: procedure(const Path: String; Attr: Word; var R: lSearchRec);
    lFindNext: procedure(var R: lSearchRec);
    lFindClose: procedure(var R: lSearchRec);

    CopyFileRec: function(FR: PFileRec): PFileRec;
    CreateFileRec: function(Name: String): PFileRec;
    NewFileRec: function(const {$IFNDEF OS2} LFN, {$ENDIF} Name: String; Size: TSize; Date, CreationDate, LastAccDate: LongInt; Attr: Word; AOwner: PString): PFileRec;
    DelFileRec: procedure(var FR: PFileRec);
    LoadFileRec: function(var S: TStream): PFileRec;
    StoreFileRec: procedure(var S: TStream; FR: PFileRec);
    LoadFileRecOwn: function(var S: TStream; Dirs: PCollection): PFileRec;
    StoreFileRecOwn: procedure(var S: TStream; FR: PFileRec; Dirs: PCollection);

    GetActivePanel: function: Pointer;
    GetPassivePanel: function: Pointer;
    GetSelection: function(AFP: Pointer; Single: Boolean): Pointer;
    ClearSelection: procedure(AFP: Pointer; FC: Pointer);

    NewStr: function(const S: String): PString;
    NewLongStr: function(const S: LongString): PLongString;
    DisposeStr: procedure(var P: PString);
    DisposeLongStr: procedure(var P: PLongString);
    CnvString: function(P: PString): String;
    CnvLongString: function(P: PLongString): LongString;

    UpStr: procedure(var S: String);
    UpLongStr: procedure(var S: LongString);
    LowStr: procedure(var S: String);
    LowLongStr: procedure(var S: LongString);

    FormatStr: procedure(var Result: String; const Format: String; var Params);
    MoveColor: procedure(var Buf; Num: Word; Attr: Byte);
    MoveBuf: procedure(var Dest; var Source; Attr: Byte; Count: Word);
    MoveChar: procedure(var Dest; C: Char; Attr: Byte; Count: Word);
    MoveCStr: procedure(var Dest; const Str: String; Attrs: Word);
    MoveStr: procedure(var Dest; const Str: String; Attr: Byte);
    CStrLen: function(const S: String): Integer;

    ExecView: function(P: Pointer): Integer;
    Reserved8: Integer;

    GetString: function(Index: Integer): String;
    ExecResource: function(Key: Integer; var Data): Word;
    LoadResource: function(Key: Integer): Pointer{PObject};

    OpenRez: function(const PluginName: String): LongInt;
    OpenRezX: function(const PluginName: String): LongInt;
    CloseRez: procedure(RezId: LongInt);
    GetRezString: function(RezId: LongInt; ItemId: SmallWord): String;
    GetRezObject: function(RezId: LongInt; ItemId: SmallWord): PObject;

    NewSItem: function(const Str: String; ANext: PSItem): PSItem;

    NewItem: function(Name, Param: TMenuStr; KeyCode: Word; Command: Word; AHelpCtx: Word; Next: PMenuItem): PMenuItem;
    NewLine: function(Next: PMenuItem): PMenuItem;
    NewSubMenu: function(Name: TMenuStr; AHelpCtx: Word; SubMenu: PMenu; Next: PMenuItem): PMenuItem;
    NewMenu: function(Items: PMenuItem): PMenu;
    DisposeMenu: procedure(Menu: PMenu);
    ExecAndDisposeMenu: function(Menu: PMenu): Integer;
    StoreMenuDefaults: procedure(Menu: PMenu; var S {TStream});
    LoadMenuDefaults: procedure(Menu: PMenu; var S {TStream});
    LookUpMenu: function(Menu: PMenu; idCheckItem: Word; Flags: Word): PMenuItem;
    MenuIndexOf: function(Menu: PMenu; idCheckItem: PMenuItem): Word;

    NewStatusDef: function(AMin, AMax: Word; AItems: PStatusItem; ANext: PStatusDef): PStatusDef;
    NewStatusKey: function(const AText: String; AKeyCode: Word; ACommand: Word; ANext: PStatusItem): PStatusItem;

    LngId: function: String;
    HelpLngId: function: String;

    RegisterType: procedure(var S: TStreamRec);
    ReregisterType: procedure(var S: TStreamRec);

    Message: function(Receiver: Pointer{PView}; What, Command: Word; InfoPtr: Pointer): Pointer;
    MessageL: function(Receiver: Pointer{PView}; What, Command: Word; InfoLng: LongInt): Pointer;

    RegisterToPrior: procedure(P: Pointer{PView});
    RegisterToBackground: procedure(P: Pointer{PView});
    Deregister: procedure(P: Pointer{PView});
    UpdateAll: procedure(All: Boolean);


    GetWinNumber: function: AInt;

    MessageBox: function(Msg: String; Params: Pointer; AOptions: Word): Word;
    MessageBox2: function(Msg1,Msg2: String; Params1,Params2: Pointer; AOptions: Word): Word;
    MessageBoxRect: function(var R: TRect; Msg: String; Params: Pointer; AOptions: Word): Word;
    MessageBox2Rect: function(var R: TRect; Msg1,Msg2: String; Lines1: Word; Params1,Params2 : Pointer; AOptions: Word): Word;
    InputBox: function(Title: String; ALabel: String; var S: String; Limit: Word; HistoryID: Word): Word;
    BigInputBox: function(Title: String; ALabel: String; var S: String; Limit: Word; HistoryID: Word): Word;
    InputBoxRect: function(var Bounds: TRect; Title: String; ALabel: String; var S: String; Limit: Word; HistoryID: Word): Word;
  end;

type
  PTEmptyObject = ^TTEmptyObject;
  TTEmptyObject = packed record
    VMT: PEmptyObjectVMT;
    Init: function(VMT, Obj: Pointer): Pointer;
    Free: procedure(Obj: Pointer);
  end;

  PTObject = ^TTObject;
  TTObject = packed record
    VMT: PObjectVMT;
    Init: function(VMT, Obj: Pointer): Pointer;
  end;

  PTRegExp = ^TTRegExp;
  TTRegExp = packed record
    VMT: PObjectVMT;
    Init: function(VMT, Obj: Pointer): Pointer;
    Reset: procedure(Obj: Pointer);
    CompileString: function(const AExpression: String; Obj: Pointer): Boolean;
    CompileStr: function(AExpression: PChar; Obj: Pointer): Boolean;
    Compile: function(AExpression: PChar; ALength: Integer; Obj: Pointer): Boolean;
    Execute: function(AString: PChar; ALength: Integer; Obj: Pointer): Boolean;
    SubstituteString: function(ASrc: PChar; const AReplace: String; var ADest: String; Obj: Pointer): Boolean;
    SubstituteStr: function(ASrc, AReplace: PChar; ADest: PChar; var ALength: Integer; Obj: Pointer): Boolean;
    Substitute: function(ASrc, AReplace: PChar; ARLen: Integer; ADest: PChar; var ADLen: Integer; Obj: Pointer): Boolean;
  end;

  PTStream = ^TTStream;
  TTStream = packed record
    VMT: PStreamVMT;
    CopyFrom: procedure(var S {TStream}; Count: Longint; Obj: Pointer);
    Get: function(Obj: Pointer): Pointer{PObject};
    Put: procedure(P: Pointer{PObject}; Obj: Pointer);
    ReadStr: function(Obj: Pointer): PString;
    ReadLongStr: function(Obj: Pointer): PLongString;
    ReadStrV: procedure(var S: String; Obj: Pointer);
    ReadLongStrV: procedure(var S: LongString; Obj: Pointer);
    Reset: procedure(Obj: Pointer);
    StrRead: function(Obj: Pointer): PChar;
    StrWrite: procedure(P: PChar; Obj: Pointer);
    WriteStr: procedure(P: PString; Obj: Pointer);
    WriteLongStr: procedure(P: PLongString; Obj: Pointer);
    EOF: function(Obj: Pointer): Boolean;
  end;

  PTDosStream = ^TTDosStream;
  TTDosStream = packed record
    VMT: PDosStreamVMT;
    Init: function(FileName: String; Mode: Word; VMT, Obj: Pointer): Pointer;
    Open: procedure(FileName: String; Mode: Word; Obj: Pointer);
  end;

  PTBufStream = ^TTBufStream;
  TTBufStream = packed record
    VMT: PBufStreamVMT;
    Init: function(FileName: String; Mode: Word; Size: LongInt; VMT, Obj: Pointer): Pointer;
  end;

  PTMemoryStream = ^TTMemoryStream;
  TTMemoryStream = packed record
    VMT: PMemoryStreamVMT;
    Init: function(ALimit: LongInt; ABlockSize: Word; VMT, Obj: Pointer): Pointer;
  end;

  PTCollection = ^TTCollection;
  TTCollection = packed record
    VMT: PCollectionVMT;
    Init: function(ALimit, ADelta: LongInt; VMT, Obj: Pointer): Pointer;
    Load: function(var S {TStream}; VMT, Obj: Pointer): Pointer;
    At: function(Index: LongInt; Obj: Pointer): Pointer;
    AtDelete: procedure(Index: LongInt; Obj: Pointer);
    AtFree: procedure(Index: LongInt; Obj: Pointer);
    AtInsert: procedure(Index: LongInt; Item: Pointer; Obj: Pointer);
    AtPut: procedure(Index: LongInt; Item: Pointer; Obj: Pointer);
    AtReplace: procedure(Index: LongInt; Item: Pointer; Obj: Pointer);
    Delete: procedure(Item: Pointer; Obj: Pointer);
    DeleteAll: procedure(Obj: Pointer);
    FirstThat: function(Test: Pointer; Obj: Pointer): Pointer;
    ForEach: procedure(Action: Pointer; Obj: Pointer);
    Free: procedure(Item: Pointer; Obj: Pointer);
    FreeAll: procedure(Obj: Pointer);
    LastThat: function(Test: Pointer; Obj: Pointer): Pointer;
    Pack: procedure(Obj: Pointer);
    Store: procedure(var S {TStream}; Obj: Pointer);
  end;

  PTSortedCollection = ^TTSortedCollection;
  TTSortedCollection = packed record
    VMT: PSortedCollectionVMT;
    Init: function(ALimit, ADelta: LongInt; VMT, Obj: Pointer): Pointer;
    Load: function(var S {TStream}; VMT, Obj: Pointer): Pointer;
    Store: procedure(var S {TStream}; Obj: Pointer);
    Sort: procedure(Obj: Pointer);
  end;

  PTLineCollection = ^TTLineCollection;
  TTLineCollection = packed record
    VMT: PLineCollectionVMT;
    Init: function(ALimit, ADelta: LongInt; ALongStrings: Boolean; VMT, Obj: Pointer): Pointer;
  end;

  PTStringCollection = ^TTStringCollection;
  TTStringCollection = packed record
    VMT: PStringCollectionVMT;
    Init: function(ALimit, ADelta: LongInt; ALongStrings: Boolean; VMT, Obj: Pointer): Pointer;
  end;

  PTStrCollection = ^TTStrCollection;
  TTStrCollection = packed record
    VMT: PStrCollectionVMT;
    Init: function(ALimit, ADelta: LongInt; VMT, Obj: Pointer): Pointer;
  end;

  PTFilesCollection = ^TTFilesCollection;
  TTFilesCollection = packed record
    VMT: PFilesCollectionVMT;
    Load: function(var S {TStream}; VMT, Obj: Pointer): Pointer;
  end;

  PTView = ^TTView;
  TTView = packed record
    VMT: PViewVMT;
    Init: function(var Bounds: TRect; VMT, Obj: Pointer): Pointer;
    Load: function(var S {TStream}; VMT, Obj: Pointer): Pointer;
    BlockCursor: procedure(Obj: Pointer);
    ClearEvent: procedure(var Event: TEvent; Obj: Pointer);
    CommandEnabled: function(Command: Word; Obj: Pointer): Boolean;
    DisableCommands: procedure(Commands: TCommandSet; Obj: Pointer);
    DragView: procedure(Event: TEvent; Mode: Byte; var Limits: TRect; MinSize, MaxSize: TPoint; Obj: Pointer);
    DrawView: procedure(Obj: Pointer);
    EnableCommands: procedure(Commands: TCommandSet; Obj: Pointer);
    EventAvail: function(Obj: Pointer): Boolean;
    Exposed: function(Obj: Pointer): Boolean;
    Focus: function(Obj: Pointer): Boolean;
    GetBounds: procedure(var Bounds: TRect; Obj: Pointer);
    GetClipRect: procedure(var Clip: TRect; Obj: Pointer);
    GetColor: function(Color: Word; Obj: Pointer): Word;
    GetCommands: procedure(var Commands: TCommandSet; Obj: Pointer);
    GetExtent: procedure(var Extent: TRect; Obj: Pointer);
    GetPeerViewPtr: procedure(var S {TStream}; var P; Obj: Pointer);
    GetState: function(AState: Word; Obj: Pointer): Boolean;
    GrowTo: procedure(X, Y: LongInt; Obj: Pointer);
    Hide: procedure(Obj: Pointer);
    HideCursor: procedure(Obj: Pointer);
    KeyEvent: procedure(var Event: TEvent; Obj: Pointer);
    Locate: procedure(var Bounds: TRect; Obj: Pointer);
    MakeFirst: procedure(Obj: Pointer);
    MakeGlobal: procedure(Source: TPoint; var Dest: TPoint; Obj: Pointer);
    MakeLocal: procedure(Source: TPoint; var Dest: TPoint; Obj: Pointer);
    MouseEvent: function(var Event: TEvent; Mask: Word; Obj: Pointer): Boolean;
    MouseInView: function(Mouse: TPoint; Obj: Pointer): Boolean;
    MoveTo: procedure(X, Y: LongInt; Obj: Pointer);
    NextView: function(Obj: Pointer): Pointer{PView};
    NormalCursor: procedure(Obj: Pointer);
    Prev: function(Obj: Pointer): Pointer{PView};
    PrevView: function(Obj: Pointer): Pointer{PView};
    PutInFrontOf: procedure(Target: Pointer{PView}; Obj: Pointer);
    PutPeerViewPtr: procedure(var S {TStream}; P: Pointer{PView}; Obj: Pointer);
    Select: procedure(Obj: Pointer);
    SetBounds: procedure(var Bounds: TRect; Obj: Pointer);
    SetCommands: procedure(Commands: TCommandSet; Obj: Pointer);
    SetCursor: procedure(X, Y: LongInt; Obj: Pointer);
    Show: procedure(Obj: Pointer);
    ShowCursor: procedure(Obj: Pointer);
    Store: procedure(var S {TStream}; Obj: Pointer);
    TopView: function(Obj: Pointer): Pointer{PView};
    WriteBuf: procedure(X, Y, W, H: Integer; var Buf; Obj: Pointer);
    WriteChar: procedure(X, Y: Integer; C: Char; Color: Byte; Count: Integer; Obj: Pointer);
    WriteLine: procedure(X, Y, W, H: Integer; var Buf; Obj: Pointer);
    WriteStr: procedure(X, Y: Integer; Str: String; Color: Byte; Obj: Pointer);
    MenuEnabled: function(Command: Word; Obj: Pointer): Boolean;
    DrawCursor: procedure(Obj: Pointer);
    DrawHide: procedure(LastView: Pointer{PView}; Obj: Pointer);
    DrawShow: procedure(LastView: Pointer{PView}; Obj: Pointer);
    DrawUnderRect: procedure(var R: TRect; LastView: Pointer{PView}; Obj: Pointer);
    DrawUnderView: procedure(DoShadow: Boolean; LastView: Pointer{PView}; Obj: Pointer);
  end;

  PTFrame = ^TTFrame;
  TTFrame = packed record
    VMT: PFrameVMT;
    Init: function(var Bounds: TRect; VMT, Obj: Pointer): Pointer;
  end;

  PTScrollBar = ^TTScrollBar;
  TTScrollBar = packed record
    VMT: PScrollBarVMT;
    Init: function(var Bounds: TRect; VMT, Obj: Pointer): Pointer;
    Load: function(var S {TStream}; VMT, Obj: Pointer): Pointer;
    SetParams: procedure(AValue, AMin, AMax, APgStep, AArStep: LongInt; Obj: Pointer);
    SetRange: procedure(AMin, AMax: LongInt; Obj: Pointer);
    SetStep: procedure(APgStep, AArStep: LongInt; Obj: Pointer);
    SetValue: procedure(AValue: LongInt; Obj: Pointer);
    Store: procedure(var S {TStream}; Obj: Pointer);
  end;

  PTGroup = ^TTGroup;
  TTGroup = packed record
    VMT: PGroupVMT;
    Init: function(var Bounds: TRect; VMT, Obj: Pointer): Pointer;
    Load: function(var S {TStream}; VMT, Obj: Pointer): Pointer;
    Delete: procedure(P: Pointer{PView}; Obj: Pointer);
    ExecView: function(P: Pointer{PView}; Obj: Pointer): Word;
    First: function(Obj: Pointer): Pointer{PView};
    FirstThat: function(P: Pointer; Obj: Pointer): Pointer{PView};
    LastThat: function(P: Pointer; Obj: Pointer): Pointer{PView};
    FocusNext: function(Forwards: Boolean; Obj: Pointer): Boolean;
    ForEach: procedure(P: Pointer; Obj: Pointer);
    GetSubViewPtr: procedure(var S {TStream}; var P; Obj: Pointer);
    Insert: procedure(P: Pointer{PView}; Obj: Pointer);
    InsertBefore: procedure(P, Target: Pointer{PView}; Obj: Pointer);
    Lock: procedure(Obj: Pointer);
    PutSubViewPtr: procedure(var S {TStream}; P: Pointer{PView}; Obj: Pointer);
    SelectNext: procedure(Forwards: Boolean; Obj: Pointer);
    Store: procedure(var S {TStream}; Obj: Pointer);
    Unlock: procedure(Obj: Pointer);
    FreeBuffer: procedure(Obj: Pointer);
    GetBuffer: procedure(Obj: Pointer);
    InsertView: procedure(P, Target: Pointer{PView}; Obj: Pointer);
    SetCurrent: procedure(P: Pointer{PView}; Mode: TSelectMode; Obj: Pointer);
  end;

  PTWindow = ^TTWindow;
  TTWindow = packed record
    VMT: PWindowVMT;
    Init: function(var Bounds: TRect; const ATitle: String; ANumber: Integer; VMT, Obj: Pointer): Pointer;
    Load: function(var S {TStream}; VMT, Obj: Pointer): Pointer;
    StandardScrollBar: function(AOptions: Word; Obj: Pointer): Pointer{PScrollBar};
    Store: procedure(var S {TStream}; Obj: Pointer);
  end;

  PTMenuView = ^TTMenuView;
  TTMenuView = packed record
    VMT: PMenuViewVMT;
    Init: function(var Bounds: TRect; VMT, Obj: Pointer): Pointer;
    Load: function(var S {TStream}; VMT, Obj: Pointer): Pointer;
    FindItem: function(Ch: Char; Obj: Pointer): PMenuItem;
    HotKey: function(KeyCode: Word; Obj: Pointer): PMenuItem;
    Store: procedure(var S {TStream}; Obj: Pointer);
  end;

  PTMenuBar = ^TTMenuBar;
  TTMenuBar = packed record
    VMT: PMenuBarVMT;
    Init: function(var Bounds: TRect; AMenu: PMenu; VMT, Obj: Pointer): Pointer;
  end;

  PTMenuBox = ^TTMenuBox;
  TTMenuBox = packed record
    VMT: PMenuBoxVMT;
    Init: function(var Bounds: TRect; AMenu: PMenu; AParentMenu: Pointer{PMenuView}; VMT, Obj: Pointer): Pointer;
  end;

  PTMenuPopup = ^TTMenuPopup;
  TTMenuPopup = packed record
    VMT: PMenuPopupVMT;
    Init: function(var Bounds: TRect; AMenu: PMenu; VMT, Obj: Pointer): Pointer;
  end;

  PTStatusLine = ^TTStatusLine;
  TTStatusLine = packed record
    VMT: PStatusLineVMT;
    Init: function(var Bounds: TRect; ADefs: PStatusDef; VMT, Obj: Pointer): Pointer;
    Load: function(var S {TStream}; VMT, Obj: Pointer): Pointer;
    Store: procedure(var S {TStream}; Obj: Pointer);
  end;

  PTDialog = ^TTDialog;
  TTDialog = packed record
    VMT: PDialogVMT;
    Init: function(var Bounds: TRect; const ATitle: String; VMT, Obj: Pointer): Pointer;
    Load: function(var S {TStream}; VMT, Obj: Pointer): Pointer;
  end;

  PTInputLine = ^TTInputLine;
  TTInputLine = packed record
    VMT: PInputLineVMT;
    Init: function(var Bounds: TRect; AMaxLen: AInt; VMT, Obj: Pointer): Pointer;
    Load: function(var S {TStream}; VMT, Obj: Pointer): Pointer;
    SelectAll: procedure(Enable: Boolean; Obj:Pointer);
    SetValidator: procedure(AValid: Pointer{PValidator}; Obj:Pointer);
    Store: procedure(var S {TStream}; Obj:Pointer);
    CanScroll: function(Delta: Integer; Obj:Pointer): Boolean;
  end;

  PTButton = ^TTButton;
  TTButton = packed record
    VMT: PButtonVMT;
    Init: function(var Bounds: TRect; const ATitle: String; ACommand: Word; AFlags: Word; VMT, Obj: Pointer): Pointer;
    Load: function(var S {TStream}; VMT, Obj: Pointer): Pointer;
    DrawState: procedure(Down: Boolean; Obj:Pointer);
    MakeDefault: procedure(Enable: Boolean; Obj:Pointer);
    Store: procedure(var S {TStream}; Obj:Pointer);
  end;

  PTCluster = ^TTCluster;
  TTCluster = packed record
    VMT: PClusterVMT;
    Init: function(var Bounds: TRect; AStrings: PSItem; VMT, Obj: Pointer): Pointer;
    Load: function(var S {TStream}; VMT, Obj: Pointer): Pointer;
    ButtonState: function(Item: Integer; Obj:Pointer): Boolean;
    DrawBox: procedure(const Icon: String; Marker: Char; Obj:Pointer);
    DrawMultiBox: procedure(const Icon, Marker: String; Obj:Pointer);
    SetButtonState: procedure(AMask: Longint; Enable: Boolean; Obj:Pointer);
    Store: procedure(var S {TStream}; Obj:Pointer);
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
    Init: function(var Bounds: TRect; AStrings: PSItem; ASelRange: Byte; AFlags: Word; const AStates: String; VMT, Obj: Pointer): Pointer;
    Load: function(var S {TStream}; VMT, Obj: Pointer): Pointer;
    Store: procedure(var S {TStream}; Obj:Pointer);
  end;

  PTScroller = ^TTScroller;
  TTScroller = packed record
    VMT: PScrollerVMT;
    Init: function(var Bounds: TRect; AHScrollBar, AVScrollBar: Pointer{PScrollBar}; VMT, Obj: Pointer): Pointer;
    Load: function(var S {TStream}; VMT, Obj: Pointer): Pointer;
    ScrollTo: procedure(X, Y: LongInt; Obj:Pointer);
    SetLimit: procedure(X, Y: LongInt; Obj:Pointer);
    Store: procedure(var S {TStream}; Obj:Pointer);
  end;

  PTListViewer = ^TTListViewer;
  TTListViewer = packed record
    VMT: PListViewerVMT;
    Init: function(var Bounds: TRect; ANumCols: LongInt; AHScrollBar, AVScrollBar: Pointer{PScrollBar}; VMT, Obj: Pointer): Pointer;
    Load: function(var S {TStream}; VMT, Obj: Pointer): Pointer;
    SetRange: procedure(ARange: LongInt; Obj:Pointer);
    Store: procedure(var S {TStream}; Obj:Pointer);
  end;

  PTListBox = ^TTListBox;
  TTListBox = packed record
    VMT: PListBoxVMT;
    Init: function(var Bounds: TRect; ANumCols: Word; AScrollBar: Pointer{PScrollBar}; VMT, Obj: Pointer): Pointer;
    Load: function(var S {TStream}; VMT, Obj: Pointer): Pointer;
    Store: procedure(var S {TStream}; Obj:Pointer);
  end;

  PTStaticText = ^TTStaticText;
  TTStaticText = packed record
    VMT: PStaticTextVMT;
    Init: function(var Bounds: TRect; const AText: String; VMT, Obj: Pointer): Pointer;
    Load: function(var S {TStream}; VMT, Obj: Pointer): Pointer;
    Store: procedure(var S {TStream}; Obj:Pointer);
  end;

  PTParamText = ^TTParamText;
  TTParamText = packed record
    VMT: PParamTextVMT;
    Init: function(var Bounds: TRect; const AText: String; AParamCount: AInt; VMT, Obj: Pointer): Pointer;
    Load: function(var S {TStream}; VMT, Obj: Pointer): Pointer;
    Store: procedure(var S {TStream}; Obj:Pointer);
  end;

  PTLabel = ^TTLabel;
  TTLabel = packed record
    VMT: PLabelVMT;
    Init: function(var Bounds: TRect; const AText: String; ALink: Pointer{PView}; VMT, Obj: Pointer): Pointer;
    Load: function(var S {TStream}; VMT, Obj: Pointer): Pointer;
    Store: procedure(var S {TStream}; Obj:Pointer);
  end;

  PTHistoryViewer = ^TTHistoryViewer;
  TTHistoryViewer = packed record
    VMT: PHistoryViewerVMT;
    Init: function(var Bounds: TRect; AHScrollBar, AVScrollBar: Pointer{PScrollBar}; AHistoryId: AWord; VMT, Obj: Pointer): Pointer;
    HistoryWidth: function(Obj:Pointer): Integer;
  end;

  PTHistoryWindow = ^TTHistoryWindow;
  TTHistoryWindow = packed record
    VMT: PHistoryWindowVMT;
    Init: function(var Bounds: TRect; HistoryId: AWord; VMT, Obj: Pointer): Pointer;
  end;

  PTHistory = ^TTHistory;
  TTHistory = packed record
    VMT: PHistoryVMT;
    Init: function(var Bounds: TRect; ALink: Pointer{PInputLine}; AHistoryId: AWord; VMT, Obj: Pointer): Pointer;
    Load: function(var S {TStream}; VMT, Obj: Pointer): Pointer;
    Store: procedure(var S {TStream}; Obj:Pointer);
  end;

  PTBackground = ^TTBackground;
  TTBackground = packed record
    VMT: PBackgroundVMT;
    Init: function(var Bounds: TRect; APattern: Char; VMT, Obj: Pointer): Pointer;
    Load: function(var S {TStream}; VMT, Obj: Pointer): Pointer;
    Store: procedure(var S {TStream}; Obj: Pointer);
  end;

  PTDesktop = ^TTDesktop;
  TTDesktop = packed record
    VMT: PDesktopVMT;
    Init: function(var Bounds: TRect; VMT, Obj: Pointer): Pointer;
    Load: function(var S {TStream}; VMT, Obj: Pointer): Pointer;
    Cascade: procedure(var R: TRect; Obj: Pointer);
    Clear: procedure(Obj: Pointer);
    Store: procedure(var S {TStream}; Obj: Pointer);
    Tile: procedure(var R: TRect; Obj: Pointer);
  end;

  PTProgram = ^TTProgram;
  TTProgram = packed record
    VMT: PProgramVMT;
    Init: function(VMT, Obj: Pointer): Pointer;
    CanMoveFocus: function(Obj: Pointer): Boolean;
    ExecuteDialog: function(P: Pointer{PDialog}; Data: Pointer; Obj: Pointer): Word;
    InsertWindow: function(P: Pointer{PWindow}; Obj: Pointer): Pointer{PWindow};
    ActivateView: procedure(P: Pointer{PView}; Obj: Pointer);
    SetScreenMode: procedure(Mode: Word; Obj: Pointer);
    ValidView: function(P: Pointer{PView}; Obj: Pointer): Pointer{PView};
   end;

  PTApplication = ^TTApplication;
  TTApplication = packed record
    VMT: PApplicationVMT;
    Init: function(VMT, Obj: Pointer): Pointer;
    Cascade: procedure(Obj: Pointer);
    ShowUserScreen: procedure(Obj: Pointer);
    Tile: procedure(Obj: Pointer);
  end;

  PTDNApplication = ^TTDNApplication;
  TTDNApplication = packed record
    VMT: PDNApplicationVMT;
    Init: function(VMT, Obj: Pointer): Pointer;
    ViewFile: procedure(AltExt, NoExtFile: Boolean; FileName: String; Obj: Pointer);
    AddFormat: procedure(Obj: Pointer);
    EditFile: procedure(Intern: Boolean; FileName: String; Obj: Pointer);
    RetrieveDesktop: procedure(const FileName: String; LS: Pointer{PStream}; LoadColors: Boolean; Obj: Pointer);
    SaveDesktop: procedure(const FileName: String; Obj: Pointer);
    LoadDesktop: procedure(var S {TStream}; Obj: Pointer);
    StoreDesktop: procedure(var S {TStream}; Obj: Pointer);
    ChgColors: procedure(Obj: Pointer);
  end;

  PTUniWindow = ^TTUniWindow;
  TTUniWindow = packed record
    VMT: PUniWindowVMT;
    MakeScrollBar: function(AOptions: Word; Obj: Pointer): Pointer{PScrollBar};
  end;

  PTXFileEditor = ^TTXFileEditor;
  TTXFileEditor = packed record
    VMT: PXFileEditorVMT;
    Init: function(var Bounds: TRect; AHScrollBar, AVScrollBar: Pointer{PScrollBar}; var FileName: String; VMT, Obj: Pointer): Pointer;
    Load: function(var S {TStream}; VMT, Obj: Pointer): Pointer;
    Store: procedure(var S {TStream}; Obj: Pointer);
    DoHighlite: procedure(var B; const S: LongString; const Attr: String; Obj: Pointer);
    GetLine: function(Index: LongInt; Obj: Pointer): LongString;
    GetSelection: function(Obj: Pointer): Pointer{PCollection};
    ValidBlock: function(Obj: Pointer): Boolean;
    CalcMenu: procedure(Obj: Pointer);
    Search: function(StartX, StartY: Word; Obj: Pointer): Boolean;
    InsertBlock: procedure(ABlock: Pointer{PCollection}; SaveUndo: Boolean; Obj: Pointer);
    ModifyLine: procedure(Index: LongInt; S: LongString; DelSpaces: Boolean; Obj: Pointer);
    SetLimits: procedure(Obj: Pointer);
    ScrollTo: procedure(DeltaX, DeltaY: LongInt; Obj: Pointer);
    LimitX: function(Obj: Pointer): LongInt;
    LimitY: function(Obj: Pointer): LongInt;
    StoreUndoInfo: procedure(What: Word; Where: TPoint; var Info; Obj: Pointer);
    KeyMapConvertStr: function(S: LongString; toAscii: Boolean; Obj: Pointer): LongString;
    KeyMapAtInsert: procedure(N: LongInt; P: PLongString; Obj: Pointer);
    KeyMapAtReplace: procedure(N: LongInt; P: PLongString; Obj: Pointer);
  end;

  PTEditWindow = ^TTEditWindow;
  TTEditWindow = packed record
    VMT: PEditWindowVMT;
    Init: function(R: TRect; FileName: String; VMT, Obj: Pointer): Pointer;
    Load: function(var S {TStream}; VMT, Obj: Pointer): Pointer;
    Store: procedure(var S {TStream}; Obj: Pointer);
  end;

var
  DNFunctions: ^TDNFunctions;

  DNMethods: packed record
  end;
  {begin of DNMethods}
  RecordSize: Integer;
  Reserved1: Integer;
  Reserved2: Integer;
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
  {end of DNMethods}

implementation

procedure InitDNFunctions(Functions, Methods: Pointer);
begin
  DNFunctions := Functions;
  Move(Methods^, DNMethods, PLongInt(Methods)^);
  SetMemoryManager(TMemoryManager(DNFunctions^.MemoryManager));
end;

procedure TransportVMT(DNObjVMT, OldVMT, NewVMT: Pointer; VMTSize: Integer);
const
{$IFDEF VIRTUALPASCAL}
  FirstMethodInVMT = 3;
{$ELSE}
  FirstMethodInVMT = 0;
{$ENDIF}
var
  I: Integer;
  Ptr1: PPointerArray absolute DNObjVMT;
  Ptr2: PPointerArray absolute OldVMT;
  Ptr3: PPointerArray absolute NewVMT;
begin
  for I := FirstMethodInVMT to VMTSize-1+FirstMethodInVMT do
    if Ptr3^[I] = Ptr2^[I] then
      Ptr3^[I] := Ptr1^[I];
end;

function DuplicateVMT(VMT: Pointer; VMTSize: Integer): Pointer;
begin
  GetMem(Result, VMTSize*4);
  if Result <> nil then
    Move(VMT^, Result^, VMTSize*4);
end;

procedure SetVmt(Obj, VMT: Pointer);
begin
  PPointer(Obj)^ := VMT;
end;

end.
