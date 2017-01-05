unit DNFuncs;
(******

Interface with plugins - functions & objects to export
Written by Cat 2:5030/1326.13
(for use in DN/2 Plugin Version)

******)


interface

uses
  {Use32,} Dos, Advance, Advance1, Advance7, Commands,
  Objects, RegExp, Collect, FilesCol, FLTools, LFN,
  Views, Menus, Scroller, Dialogs, Messages, DNApp,
  DNUtil, UniWin, Editor, EdWin, Calculat, Plugin, PlugRez;

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

  PDNFunctions = ^TDNFunctions;
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

    MemoryManager: TMemoryManager;

    TinySlice: procedure;

    Evalue: function(const S: String; VGF: VarGetFunc): CReal;
    EvalueError: ^Boolean;

    DosError: function: Integer;

    lFindFirst: procedure(const Path: String; Attr: Word; var R: lSearchRec);
    lFindNext: procedure(var R: lSearchRec);
    lFindClose: procedure(var R: lSearchRec);

    CopyFileRec: function(FR: PFileRec): PFileRec;
    CreateFileRec: function(Name: String): PFileRec;
    NewFileRec: function(const {$IFNDEF OS2} LFN, {$ENDIF} Name: String; Size: Comp; Date, CreationDate, LastAccDate: LongInt; Attr: Word; AOwner: PString): PFileRec;
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

    ExecView: function(P: PView): Integer;
    Reserved8: Integer;

    GetString: function(Index: TStrIdx): String;
    ExecResource: function(Key: TDlgIdx; var Data): Word;
    LoadResource: function(Key: TDlgIdx): PObject;

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
    StoreMenuDefaults: procedure(Menu: PMenu; var S: TStream);
    LoadMenuDefaults: procedure(Menu: PMenu; var S: TStream);
    LookUpMenu: function(Menu: PMenu; idCheckItem: Word; Flags: Word): PMenuItem;
    MenuIndexOf: function(Menu: PMenu; idCheckItem: PMenuItem): Word;

    NewStatusDef: function(AMin, AMax: Word; AItems: PStatusItem; ANext: PStatusDef): PStatusDef;
    NewStatusKey: function(const AText: String; AKeyCode: Word; ACommand: Word; ANext: PStatusItem): PStatusItem;

    LngId: function: String;
    HelpLngId: function: String;

    RegisterType: procedure(var S: TStreamRec);
    ReRegisterType: procedure(var S: TStreamRec);

    Message: function(Receiver: PView; What, Command: Word; InfoPtr: Pointer): Pointer;
    MessageL: function(Receiver: PView; What, Command: Word; InfoLng: LongInt): Pointer;

    RegisterToPrior: procedure(P: PView);
    RegisterToBackground: procedure(P: PView);
    Deregister: procedure(P: PView);
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

function DosError: Integer;
function ExecView(P: PView): Integer;
function ExecAndDisposeMenu(Menu: PMenu): Integer;
function GetActivePanel: Pointer;
function GetPassivePanel: Pointer;
procedure ClearSelection(AFP: Pointer; FC: Pointer);
function OpenRezX(const PluginName: String): LongInt;

const
  SimpleHooks: TSimpleHooks =
    (
     SetEditorEventHook:    Plugin.SetEditorEventHook;
     RemoveEditorEventHook: Plugin.RemoveEditorEventHook
    );

  DNFunctions: TDNFunctions =
    (
     DN2Version:            0;
     APIVersion:            1;
     Reserved1:             0;
     Reserved2:             0;

     SomeObjects1:          @DNApp.Application;
     SomeObjects2:          @Advance.StartupDir;
     SomeObjects3:          @Plugin.EventCatchers;
     Reserved3:             0;
     Reserved4:             0;
     Reserved5:             0;
     Reserved6:             0;
     SimpleHooks:           @SimpleHooks;

     MemoryManager:         (
                             GetMem: SysGetMem;
                             FreeMem: SysFreeMem;
                             ReallocMem: SysReallocMem
                            );

     TinySlice:             Advance.TinySlice;

     Evalue:                Calculat.Evalue;
     EvalueError:           @Calculat.EvalueError;

     DosError:              DosError;

     lFindFirst:            LFN.lFindFirst;
     lFindNext:             LFN.lFindNext;
     lFindClose:            LFN.lFindClose;

     CopyFileRec:           FilesCol.CopyFileRec;
     CreateFileRec:         FilesCol.CreateFileRec;
     NewFileRec:            FilesCol.NewFileRec;
     DelFileRec:            FilesCol.DelFileRec;
     LoadFileRec:           FilesCol.LoadFileRec;
     StoreFileRec:          FilesCol.StoreFileRec;
     LoadFileRecOwn:        FilesCol.LoadFileRecOwn;
     StoreFileRecOwn:       FilesCol.StoreFileRecOwn;

     GetActivePanel:        GetActivePanel;
     GetPassivePanel:       GetPassivePanel;
     GetSelection:          FlTools.GetSelection;
     ClearSelection:        ClearSelection;

     NewStr:                Advance1.NewStr;
     NewLongStr:            Advance1.NewLongStr;
     DisposeStr:            Advance1.DisposeStr;
     DisposeLongStr:        Advance1.DisposeLongStr;
     CnvString:             Advance1.CnvString;
     CnvLongString:         Advance1.CnvLongString;

     UpStr:                 Advance1.UpStr;
     UpLongStr:             Advance1.UpLongStr;
     LowStr:                Advance1.LowStr;
     LowLongStr:            Advance1.LowLongStr;

     FormatStr:             Drivers.FormatStr;
     MoveColor:             Drivers.MoveColor;
     MoveBuf:               Drivers.MoveBuf;
     MoveChar:              Drivers.MoveChar;
     MoveCStr:              Drivers.MoveCStr;
     MoveStr:               Drivers.MoveStr;
     CStrLen:               Drivers.CStrLen;

     ExecView:              ExecView;
     Reserved8:             0;

     GetString:             DNApp.GetString;
     ExecResource:          DNApp.ExecResource;
     LoadResource:          DNApp.LoadResource;

     OpenRez:               PlugRez.OpenRez;
     OpenRezX:              OpenRezX;
     CloseRez:              PlugRez.CloseRez;
     GetRezString:          PlugRez.GetRezString;
     GetRezObject:          PlugRez.GetRezObject;

     NewSItem:              Dialogs.NewSItem;

     NewItem:               Menus.NewItem;
     NewLine:               Menus.NewLine;
     NewSubMenu:            Menus.NewSubMenu;
     NewMenu:               Menus.NewMenu;
     DisposeMenu:           Menus.DisposeMenu;
     ExecAndDisposeMenu:    ExecAndDisposeMenu;
     StoreMenuDefaults:     Menus.StoreMenuDefaults;
     LoadMenuDefaults:      Menus.LoadMenuDefaults;
     LookUpMenu:            Menus.LookUpMenu;
     MenuIndexOf:           Menus.MenuIndexOf;

     NewStatusDef:          Menus.NewStatusDef;
     NewStatusKey:          Menus.NewStatusKey;

     LngId:                 Advance7.LngId;
     HelpLngId:             Advance7.HelpLngId;

     RegisterType:          Objects.RegisterType;
     ReRegisterType:        Objects.ReRegisterType;

     Message:               Views.Message;
     MessageL:              Views.MessageL;

     RegisterToPrior:       Views.RegisterToPrior;
     RegisterToBackground:  Views.RegisterToBackground;
     Deregister:            Views.Deregister;
     UpdateAll:             Views.UpdateAll;

     GetWinNumber:          Views.GetNum;

     MessageBox:            Messages.MessageBox;
     MessageBox2:           Messages.MessageBox2;
     MessageBoxRect:        Messages.MessageBoxRect;
     MessageBox2Rect:       Messages.MessageBox2Rect;
     InputBox:              Messages.InputBox;
     BigInputBox:           Messages.BigInputBox;
     InputBoxRect:          Messages.InputBoxRect
    );

type
  PTEmptyObject = ^TTEmptyObject;
  TTEmptyObject = packed record
    VMT: Pointer;
    Init: Pointer;
    Free: Pointer;
  end;

  PTObject = ^TTObject;
  TTObject = packed record
    VMT: Pointer;
    Init: Pointer;
  end;

  PTRegExp = ^TTRegExp;
  TTRegExp = packed record
    VMT: Pointer;
    Init: Pointer;
    Reset: Pointer;
    CompileString: Pointer;
    CompileStr: Pointer;
    Compile: Pointer;
    Execute: Pointer;
    SubstituteString: Pointer;
    SubstituteStr: Pointer;
    Substitute: Pointer;
  end;

  PTStream = ^TTStream;
  TTStream = packed record
    VMT: Pointer;
    CopyFrom: Pointer;
    Get: Pointer;
    Put: Pointer;
    ReadStr: Pointer;
    ReadLongStr: Pointer;
    ReadStrV: Pointer;
    ReadLongStrV: Pointer;
    Reset: Pointer;
    StrRead: Pointer;
    StrWrite: Pointer;
    WriteStr: Pointer;
    WriteLongStr: Pointer;
    EOF: Pointer;
  end;

  PTDosStream = ^TTDosStream;
  TTDosStream = packed record
    VMT: Pointer;
    Init: Pointer;
    Open: Pointer;
  end;

  PTBufStream = ^TTBufStream;
  TTBufStream = packed record
    VMT: Pointer;
    Init: Pointer;
  end;

  PTMemoryStream = ^TTMemoryStream;
  TTMemoryStream = packed record
    VMT: Pointer;
    Init: Pointer;
  end;

  PTCollection = ^TTCollection;
  TTCollection = packed record
    VMT: Pointer;
    Init: Pointer;
    Load: Pointer;
    At: Pointer;
    AtDelete: Pointer;
    AtFree: Pointer;
    AtInsert: Pointer;
    AtPut: Pointer;
    AtReplace: Pointer;
    Delete: Pointer;
    DeleteAll: Pointer;
    FirstThat: Pointer;
    ForEach: Pointer;
    Free: Pointer;
    FreeAll: Pointer;
    LastThat: Pointer;
    Pack: Pointer;
    Store: Pointer;
  end;

  PTSortedCollection = ^TTSortedCollection;
  TTSortedCollection = packed record
    VMT: Pointer;
    Init: Pointer;
    Load: Pointer;
    Store: Pointer;
    Sort: Pointer;
  end;

  PTLineCollection = ^TTLineCollection;
  TTLineCollection = packed record
    VMT: Pointer;
    Init: Pointer;
  end;

  PTStringCollection = ^TTStringCollection;
  TTStringCollection = packed record
    VMT: Pointer;
    Init: Pointer;
  end;

  PTStrCollection = ^TTStrCollection;
  TTStrCollection = packed record
    VMT: Pointer;
    Init: Pointer;
  end;

  PTFilesCollection = ^TTFilesCollection;
  TTFilesCollection = packed record
    VMT: Pointer;
    Load: Pointer;
  end;

  PTView = ^TTView;
  TTView = packed record
    VMT: Pointer;
    Init: Pointer;
    Load: Pointer;
    BlockCursor: Pointer;
    ClearEvent: Pointer;
    CommandEnabled: Pointer;
    DisableCommands: Pointer;
    DragView: Pointer;
    DrawView: Pointer;
    EnableCommands: Pointer;
    EventAvail: Pointer;
    Exposed: Pointer;
    Focus: Pointer;
    GetBounds: Pointer;
    GetClipRect: Pointer;
    GetColor: Pointer;
    GetCommands: Pointer;
    GetExtent: Pointer;
    GetPeerViewPtr: Pointer;
    GetState: Pointer;
    GrowTo: Pointer;
    Hide: Pointer;
    HideCursor: Pointer;
    KeyEvent: Pointer;
    Locate: Pointer;
    MakeFirst: Pointer;
    MakeGlobal: Pointer;
    MakeLocal: Pointer;
    MouseEvent: Pointer;
    MouseInView: Pointer;
    MoveTo: Pointer;
    NextView: Pointer;
    NormalCursor: Pointer;
    Prev: Pointer;
    PrevView: Pointer;
    PutInFrontOf: Pointer;
    PutPeerViewPtr: Pointer;
    Select: Pointer;
    SetBounds: Pointer;
    SetCommands: Pointer;
    SetCursor: Pointer;
    Show: Pointer;
    ShowCursor: Pointer;
    Store: Pointer;
    TopView: Pointer;
    WriteBuf: Pointer;
    WriteChar: Pointer;
    WriteLine: Pointer;
    WriteStr: Pointer;
    MenuEnabled: Pointer;
    DrawCursor: Pointer;
    DrawHide: Pointer;
    DrawShow: Pointer;
    DrawUnderRect: Pointer;
    DrawUnderView: Pointer;
  end;

  PTFrame = ^TTFrame;
  TTFrame = packed record
    VMT: Pointer;
    Init: Pointer;
  end;

  PTScrollBar = ^TTScrollBar;
  TTScrollBar = packed record
    VMT: Pointer;
    Init: Pointer;
    Load: Pointer;
    SetParams: Pointer;
    SetRange: Pointer;
    SetStep: Pointer;
    SetValue: Pointer;
    Store: Pointer;
  end;

  PTGroup = ^TTGroup;
  TTGroup = packed record
    VMT: Pointer;
    Init: Pointer;
    Load: Pointer;
    Delete: Pointer;
    ExecView: Pointer;
    First: Pointer;
    FirstThat: Pointer;
    LastThat: Pointer;
    FocusNext: Pointer;
    ForEach: Pointer;
    GetSubViewPtr: Pointer;
    Insert: Pointer;
    InsertBefore: Pointer;
    Lock: Pointer;
    PutSubViewPtr: Pointer;
    SelectNext: Pointer;
    Store: Pointer;
    Unlock: Pointer;
    FreeBuffer: Pointer;
    GetBuffer: Pointer;
    InsertView: Pointer;
    SetCurrent: Pointer;
  end;

  PTWindow = ^TTWindow;
  TTWindow = packed record
    VMT: Pointer;
    Init: Pointer;
    Load: Pointer;
    StandardScrollBar: Pointer;
    Store: Pointer;
  end;

  PTMenuView = ^TTMenuView;
  TTMenuView = packed record
    VMT: Pointer;
    Init: Pointer;
    Load: Pointer;
    FindItem: Pointer;
    HotKey: Pointer;
    Store: Pointer;
  end;

  PTMenuBar = ^TTMenuBar;
  TTMenuBar = packed record
    VMT: Pointer;
    Init: Pointer;
  end;

  PTMenuBox = ^TTMenuBox;
  TTMenuBox = packed record
    VMT: Pointer;
    Init: Pointer;
  end;

  PTMenuPopup = ^TTMenuPopup;
  TTMenuPopup = packed record
    VMT: Pointer;
    Init: Pointer;
  end;

  PTStatusLine = ^TTStatusLine;
  TTStatusLine = packed record
    VMT: Pointer;
    Init: Pointer;
    Load: Pointer;
    Store: Pointer;
  end;

  PTDialog = ^TTDialog;
  TTDialog = packed record
    VMT: Pointer;
    Init: Pointer;
    Load: Pointer;
  end;

  PTInputLine = ^TTInputLine;
  TTInputLine = packed record
    VMT: Pointer;
    Init: Pointer;
    Load: Pointer;
    SelectAll: Pointer;
    SetValidator: Pointer;
    Store: Pointer;
    CanScroll: Pointer;
  end;

  PTButton = ^TTButton;
  TTButton = packed record
    VMT: Pointer;
    Init: Pointer;
    Load: Pointer;
    DrawState: Pointer;
    MakeDefault: Pointer;
    Store: Pointer;
  end;

  PTCluster = ^TTCluster;
  TTCluster = packed record
    VMT: Pointer;
    Init: Pointer;
    Load: Pointer;
    ButtonState: Pointer;
    DrawBox: Pointer;
    DrawMultiBox: Pointer;
    SetButtonState: Pointer;
    Store: Pointer;
  end;

  PTRadioButtons = ^TTRadioButtons;
  TTRadioButtons = packed record
    VMT: Pointer;
  end;

  PTCheckBoxes = ^TTCheckBoxes;
  TTCheckBoxes = packed record
    VMT: Pointer;
  end;

  PTMultiCheckBoxes = ^TTMultiCheckBoxes;
  TTMultiCheckBoxes = packed record
    VMT: Pointer;
    Init: Pointer;
    Load: Pointer;
    Store: Pointer;
  end;

  PTScroller = ^TTScroller;
  TTScroller = packed record
    VMT: Pointer;
    Init: Pointer;
    Load: Pointer;
    ScrollTo: Pointer;
    SetLimit: Pointer;
    Store: Pointer;
  end;

  PTListViewer = ^TTListViewer;
  TTListViewer = packed record
    VMT: Pointer;
    Init: Pointer;
    Load: Pointer;
    SetRange: Pointer;
    Store: Pointer;
  end;

  PTListBox = ^TTListBox;
  TTListBox = packed record
    VMT: Pointer;
    Init: Pointer;
    Load: Pointer;
    Store: Pointer;
  end;

  PTStaticText = ^TTStaticText;
  TTStaticText = packed record
    VMT: Pointer;
    Init: Pointer;
    Load: Pointer;
    Store: Pointer;
  end;

  PTParamText = ^TTParamText;
  TTParamText = packed record
    VMT: Pointer;
    Init: Pointer;
    Load: Pointer;
    Store: Pointer;
  end;

  PTLabel = ^TTLabel;
  TTLabel = packed record
    VMT: Pointer;
    Init: Pointer;
    Load: Pointer;
    Store: Pointer;
  end;

  PTHistoryViewer = ^TTHistoryViewer;
  TTHistoryViewer = packed record
    VMT: Pointer;
    Init: Pointer;
    HistoryWidth: Pointer;
  end;

  PTHistoryWindow = ^TTHistoryWindow;
  TTHistoryWindow = packed record
    VMT: Pointer;
    Init: Pointer;
  end;

  PTHistory = ^TTHistory;
  TTHistory = packed record
    VMT: Pointer;
    Init: Pointer;
    Load: Pointer;
    Store: Pointer;
  end;

  PTBackground = ^TTBackground;
  TTBackground = packed record
    VMT: Pointer;
    Init: Pointer;
    Load: Pointer;
    Store: Pointer;
  end;

  PTDesktop = ^TTDesktop;
  TTDesktop = packed record
    VMT: Pointer;
    Init: Pointer;
    Load: Pointer;
    Cascade: Pointer;
    Clear: Pointer;
    Store: Pointer;
    Tile: Pointer;
  end;

  PTProgram = ^TTProgram;
  TTProgram = packed record
    VMT: Pointer;
    Init: Pointer;
    CanMoveFocus: Pointer;
    ExecuteDialog: Pointer;
    InsertWindow: Pointer;
    ActivateView: Pointer;
    SetScreenMode: Pointer;
    ValidView: Pointer;
   end;

  PTApplication = ^TTApplication;
  TTApplication = packed record
    VMT: Pointer;
    Init: Pointer;
    Cascade: Pointer;
    ShowUserScreen: Pointer;
    Tile: Pointer;
  end;

  PTDNApplication = ^TTDNApplication;
  TTDNApplication = packed record
    VMT: Pointer;
    Init: Pointer;
    ViewFile: Pointer;
    AddFormat: Pointer;
    EditFile: Pointer;
    RetrieveDesktop: Pointer;
    SaveDesktop: Pointer;
    LoadDesktop: Pointer;
    StoreDesktop: Pointer;
    ChgColors: Pointer;
  end;

  PTUniWindow = ^TTUniWindow;
  TTUniWindow = packed record
    VMT: Pointer;
    MakeScrollBar: Pointer;
  end;

  PTXFileEditor = ^TTXFileEditor;
  TTXFileEditor = packed record
    VMT: Pointer;
    Init: Pointer;
    Load: Pointer;
    Store: Pointer;
    DoHighlite: Pointer;
    GetLine: Pointer;
    GetSelection: Pointer;
    ValidBlock: Pointer;
    CalcMenu: Pointer;
    Search: Pointer;
    InsertBlock: Pointer;
    ModifyLine: Pointer;
    SetLimits: Pointer;
    ScrollTo: Pointer;
    LimitX: Pointer;
    LimitY: Pointer;
    StoreUndoInfo: Pointer;
    KeyMapConvertStr: Pointer;
    KeyMapAtInsert: Pointer;
    KeyMapAtReplace: Pointer;
  end;

  PTEditWindow = ^TTEditWindow;
  TTEditWindow = packed record
    VMT: Pointer;
    Init: Pointer;
    Load: Pointer;
    Store: Pointer;
  end;

  PDNMethods = ^TDNMethods;
  TDNMethods = packed record
    RecordSize: Integer;
    Reserved1: Integer;
    Reserved2: Integer;
    _TEmptyObject: PTEmptyObject;
    _TObject: PTObject;
    _TRegExp: PTRegExp;
    _T1: Pointer;
    _T2: Pointer;
    _T3: Pointer;
    _TStream: PTStream;
    _TDosStream: PTDosStream;
    _TBufStream: PTBufStream;
    _TMemoryStream: PTMemoryStream;
    _TCollection: PTCollection;
    _TSortedCollection: PTSortedCollection;
    _TLineCollection: PTLineCollection;
    _TStringCollection: PTStringCollection;
    _TStrCollection: PTStrCollection;
    _TFilesCollection: PTFilesCollection;
    _TView: PTView;
    _TFrame: PTFrame;
    _TScrollBar: PTScrollBar;
    _TGroup: PTGroup;
    _TWindow: PTWindow;
    _TMenuView: PTMenuView;
    _TMenuBar: PTMenuBar;
    _TMenuBox: PTMenuBox;
    _TMenuPopup: PTMenuPopup;
    _TStatusLine: PTStatusLine;
    _TDialog: PTDialog;
    _TInputLine: PTInputLine;
    _TButton: PTButton;
    _TCluster: PTCluster;
    _TRadioButtons: PTRadioButtons;
    _TCheckBoxes: PTCheckBoxes;
    _TMultiCheckBoxes: PTMultiCheckBoxes;
    _TScroller: PTScroller;
    _TListViewer: PTListViewer;
    _TListBox: PTListBox;
    _TStaticText: PTStaticText;
    _TParamText: PTParamText;
    _TLabel: PTLabel;
    _THistoryViewer: PTHistoryViewer;
    _THistoryWindow: PTHistoryWindow;
    _THistory: PTHistory;
    _TBackground: PTBackground;
    _TDesktop: PTDesktop;
    _TProgram: PTProgram;
    _TApplication: PTApplication;
    _TDNApplication: PTDNApplication;
    _TUniWindow: PTUniWindow;
    _TXFileEditor: PTXFileEditor;
    _TEditWindow: PTEditWindow
  end;

const
  _TEmptyObject: TTEmptyObject =
    (
     VMT:                   TypeOf(TEmptyObject);
     Init:                  @TEmptyObject.Init;
     Free:                  @TEmptyObject.Free
    );

  _TObject: TTObject =
    (
     VMT:                   TypeOf(TObject);
     Init:                  @TObject.Init
    );

  _TRegExp: TTRegExp =
    (
     VMT:                   TypeOf(TRegExp);
     Init:                  @TRegExp.Init;
     Reset:                 @TRegExp.Reset;
     CompileString:         @TRegExp.CompileString;
     CompileStr:            @TRegExp.CompileStr;
     Compile:               @TRegExp.Compile;
     Execute:               @TRegExp.Execute;
     SubstituteString:      @TRegExp.SubstituteString;
     SubstituteStr:         @TRegExp.SubstituteStr;
     Substitute:            @TRegExp.Substitute
    );

  _TStream: TTStream =
    (
     VMT:                   TypeOf(TStream);
     CopyFrom:              @TStream.CopyFrom;
     Get:                   @TStream.Get;
     Put:                   @TStream.Put;
     ReadStr:               @TStream.ReadStr;
     ReadLongStr:           @TStream.ReadLongStr;
     ReadStrV:              @TStream.ReadStrV;
     ReadLongStrV:          @TStream.ReadLongStrV;
     Reset:                 @TStream.Reset;
     StrRead:               @TStream.StrRead;
     StrWrite:              @TStream.StrWrite;
     WriteStr:              @TStream.WriteStr;
     WriteLongStr:          @TStream.WriteLongStr;
     EOF:                   @TStream.EOF
    );

  _TDosStream: TTDosStream =
    (
     VMT:                   TypeOf(TDosStream);
     Init:                  @TDosStream.Init;
     Open:                  @TDosStream.Open
    );

  _TBufStream: TTBufStream =
    (
     VMT:                   TypeOf(TBufStream);
     Init:                  @TBufStream.Init
    );

  _TMemoryStream: TTMemoryStream =
    (
     VMT:                   TypeOf(TMemoryStream);
     Init:                  @TMemoryStream.Init
    );

  _TCollection: TTCollection =
    (
     VMT:                   TypeOf(TCollection);
     Init:                  @TCollection.Init;
     Load:                  @TCollection.Load;
     At:                    @TCollection.At;
     AtDelete:              @TCollection.AtDelete;
     AtFree:                @TCollection.AtFree;
     AtInsert:              @TCollection.AtInsert;
     AtPut:                 @TCollection.AtPut;
     AtReplace:             @TCollection.AtReplace;
     Delete:                @TCollection.Delete;
     DeleteAll:             @TCollection.DeleteAll;
     FirstThat:             @TCollection.FirstThat;
     ForEach:               @TCollection.ForEach;
     Free:                  @TCollection.Free;
     FreeAll:               @TCollection.FreeAll;
     LastThat:              @TCollection.LastThat;
     Pack:                  @TCollection.Pack;
     Store:                 @TCollection.Store
    );

  _TSortedCollection: TTSortedCollection =
    (
     VMT:                   TypeOf(TSortedCollection);
     Init:                  @TSortedCollection.Init;
     Load:                  @TSortedCollection.Load;
     Store:                 @TSortedCollection.Store;
     Sort:                  @TSortedCollection.Sort
    );

  _TLineCollection: TTLineCollection =
    (
     VMT:                   TypeOf(TLineCollection);
     Init:                  @TLineCollection.Init
    );

  _TStringCollection: TTStringCollection =
    (
     VMT:                   TypeOf(TStringCollection);
     Init:                  @TStringCollection.Init
    );

  _TStrCollection: TTStrCollection =
    (
     VMT:                   TypeOf(TStrCollection);
     Init:                  @TStrCollection.Init
    );

  _TFilesCollection: TTFilesCollection =
    (
     VMT:                   TypeOf(TFilesCollection);
     Load:                  @TFilesCollection.Load
    );

  _TView: TTView =
    (
     VMT:                   TypeOf(TView);
     Init:                  @TView.Init;
     Load:                  @TView.Load;
     BlockCursor:           @TView.BlockCursor;
     ClearEvent:            @TView.ClearEvent;
     CommandEnabled:        @TView.CommandEnabled;
     DisableCommands:       @TView.DisableCommands;
     DragView:              @TView.DragView;
     DrawView:              @TView.DrawView;
     EnableCommands:        @TView.EnableCommands;
     EventAvail:            @TView.EventAvail;
     Exposed:               @TView.Exposed;
     Focus:                 @TView.Focus;
     GetBounds:             @TView.GetBounds;
     GetClipRect:           @TView.GetClipRect;
     GetColor:              @TView.GetColor;
     GetCommands:           @TView.GetCommands;
     GetExtent:             @TView.GetExtent;
     GetPeerViewPtr:        @TView.GetPeerViewPtr;
     GetState:              @TView.GetState;
     GrowTo:                @TView.GrowTo;
     Hide:                  @TView.Hide;
     HideCursor:            @TView.HideCursor;
     KeyEvent:              @TView.KeyEvent;
     Locate:                @TView.Locate;
     MakeFirst:             @TView.MakeFirst;
     MakeGlobal:            @TView.MakeGlobal;
     MakeLocal:             @TView.MakeLocal;
     MouseEvent:            @TView.MouseEvent;
     MouseInView:           @TView.MouseInView;
     MoveTo:                @TView.MoveTo;
     NextView:              @TView.NextView;
     NormalCursor:          @TView.NormalCursor;
     Prev:                  @TView.Prev;
     PrevView:              @TView.PrevView;
     PutInFrontOf:          @TView.PutInFrontOf;
     PutPeerViewPtr:        @TView.PutPeerViewPtr;
     Select:                @TView.Select;
     SetBounds:             @TView.SetBounds;
     SetCommands:           @TView.SetCommands;
     SetCursor:             @TView.SetCursor;
     Show:                  @TView.Show;
     ShowCursor:            @TView.ShowCursor;
     Store:                 @TView.Store;
     TopView:               @TView.TopView;
     WriteBuf:              @TView.WriteBuf;
     WriteChar:             @TView.WriteChar;
     WriteLine:             @TView.WriteLine;
     WriteStr:              @TView.WriteStr;
     MenuEnabled:           @TView.MenuEnabled;
     DrawCursor:            @TView.DrawCursor;
     DrawHide:              @TView.DrawHide;
     DrawShow:              @TView.DrawShow;
     DrawUnderRect:         @TView.DrawUnderRect;
     DrawUnderView:         @TView.DrawUnderView
    );

  _TFrame: TTFrame =
    (
     VMT:                   TypeOf(TFrame);
     Init:                  @TFrame.Init
    );

  _TScrollBar: TTScrollBar =
    (
     VMT:                   TypeOf(TScrollBar);
     Init:                  @TScrollBar.Init;
     Load:                  @TScrollBar.Load;
     SetParams:             @TScrollBar.SetParams;
     SetRange:              @TScrollBar.SetRange;
     SetStep:               @TScrollBar.SetStep;
     SetValue:              @TScrollBar.SetValue;
     Store:                 @TScrollBar.Store
    );

  _TGroup: TTGroup =
    (
     VMT:                   TypeOf(TGroup);
     Init:                  @TGroup.Init;
     Load:                  @TGroup.Load;
     Delete:                @TGroup.Delete;
     ExecView:              @TGroup.ExecView;
     First:                 @TGroup.First;
     FirstThat:             @TGroup.FirstThat;
     LastThat:              @TGroup.LastThat;
     FocusNext:             @TGroup.FocusNext;
     ForEach:               @TGroup.ForEach;
     GetSubViewPtr:         @TGroup.GetSubViewPtr;
     Insert:                @TGroup.Insert;
     InsertBefore:          @TGroup.InsertBefore;
     Lock:                  @TGroup.Lock;
     PutSubViewPtr:         @TGroup.PutSubViewPtr;
     SelectNext:            @TGroup.SelectNext;
     Store:                 @TGroup.Store;
     Unlock:                @TGroup.Unlock;
     FreeBuffer:            @TGroup.FreeBuffer;
     GetBuffer:             @TGroup.GetBuffer;
     InsertView:            @TGroup.InsertView;
     SetCurrent:            @TGroup.SetCurrent
    );

  _TWindow: TTWindow =
    (
     VMT:                   TypeOf(TWindow);
     Init:                  @TWindow.Init;
     Load:                  @TWindow.Load;
     StandardScrollBar:     @TWindow.StandardScrollBar;
     Store:                 @TWindow.Store
    );

  _TMenuView: TTMenuView =
    (
     VMT:                   TypeOf(TMenuView);
     Init:                  @TMenuView.Init;
     Load:                  @TMenuView.Load;
     FindItem:              @TMenuView.FindItem;
     HotKey:                @TMenuView.HotKey;
     Store:                 @TMenuView.Store
    );

  _TMenuBar: TTMenuBar =
    (
     VMT:                   TypeOf(TMenuBar);
     Init:                  @TMenuBar.Init
    );

  _TMenuBox: TTMenuBox =
    (
     VMT:                   TypeOf(TMenuBox);
     Init:                  @TMenuBox.Init
    );

  _TMenuPopup: TTMenuPopup =
    (
     VMT:                   TypeOf(TMenuPopup);
     Init:                  @TMenuPopup.Init
    );

  _TStatusLine: TTStatusLine =
    (
     VMT:                   TypeOf(TStatusLine);
     Init:                  @TStatusLine.Init;
     Load:                  @TStatusLine.Load;
     Store:                 @TStatusLine.Store
    );

  _TDialog: TTDialog =
    (
     VMT:                   TypeOf(TDialog);
     Init:                  @TDialog.Init;
     Load:                  @TDialog.Load
    );

  _TInputLine: TTInputLine =
    (
     VMT:                   TypeOf(TInputLine);
     Init:                  @TInputLine.Init;
     Load:                  @TInputLine.Load;
     SelectAll:             @TInputLine.SelectAll;
     SetValidator:          @TInputLine.SetValidator;
     Store:                 @TInputLine.Store;
     CanScroll:             @TInputLine.CanScroll
    );

  _TButton: TTButton =
    (
     VMT:                   TypeOf(TButton);
     Init:                  @TButton.Init;
     Load:                  @TButton.Load;
     DrawState:             @TButton.DrawState;
     MakeDefault:           @TButton.MakeDefault;
     Store:                 @TButton.Store
    );

  _TCluster: TTCluster =
    (
     VMT:                   TypeOf(TCluster);
     Init:                  @TCluster.Init;
     Load:                  @TCluster.Load;
     ButtonState:           @TCluster.ButtonState;
     DrawBox:               @TCluster.DrawBox;
     DrawMultiBox:          @TCluster.DrawMultiBox;
     SetButtonState:        @TCluster.SetButtonState;
     Store:                 @TCluster.Store
    );

  _TRadioButtons: TTRadioButtons =
    (
     VMT:                   TypeOf(TRadioButtons)
    );

  _TCheckBoxes: TTCheckBoxes =
    (
     VMT:                   TypeOf(TCheckBoxes)
    );

  _TMultiCheckBoxes: TTMultiCheckBoxes =
    (
     VMT:                   TypeOf(TMultiCheckBoxes);
     Init:                  @TMultiCheckBoxes.Init;
     Load:                  @TMultiCheckBoxes.Load;
     Store:                 @TMultiCheckBoxes.Store
    );

  _TScroller: TTScroller =
    (
     VMT:                   TypeOf(TScroller);
     Init:                  @TScroller.Init;
     Load:                  @TScroller.Load;
     ScrollTo:              @TScroller.ScrollTo;
     SetLimit:              @TScroller.SetLimit;
     Store:                 @TScroller.Store
    );

  _TListViewer: TTListViewer =
    (
     VMT:                   TypeOf(TListViewer);
     Init:                  @TListViewer.Init;
     Load:                  @TListViewer.Load;
     SetRange:              @TListViewer.SetRange;
     Store:                 @TListViewer.Store
    );

  _TListBox: TTListBox =
    (
     VMT:                   TypeOf(TListBox);
     Init:                  @TListBox.Init;
     Load:                  @TListBox.Load;
     Store:                 @TListBox.Store
    );

  _TStaticText: TTStaticText =
    (
     VMT:                   TypeOf(TStaticText);
     Init:                  @TStaticText.Init;
     Load:                  @TStaticText.Load;
     Store:                 @TStaticText.Store
    );

  _TParamText: TTParamText =
    (
     VMT:                   TypeOf(TParamText);
     Init:                  @TParamText.Init;
     Load:                  @TParamText.Load;
     Store:                 @TParamText.Store
    );

  _TLabel: TTLabel =
    (
     VMT:                   TypeOf(TLabel);
     Init:                  @TLabel.Init;
     Load:                  @TLabel.Load;
     Store:                 @TLabel.Store
    );

  _THistoryViewer: TTHistoryViewer =
    (
     VMT:                   TypeOf(THistoryViewer);
     Init:                  @THistoryViewer.Init;
     HistoryWidth:          @THistoryViewer.HistoryWidth
    );

  _THistoryWindow: TTHistoryWindow =
    (
     VMT:                   TypeOf(THistoryWindow);
     Init:                  @THistoryWindow.Init
    );

  _THistory: TTHistory =
    (
     VMT:                   TypeOf(THistory);
     Init:                  @THistory.Init;
     Load:                  @THistory.Load;
     Store:                 @THistory.Store
    );

  _TBackground: TTBackground =
    (
     VMT:                   TypeOf(TBackground);
     Init:                  @TBackground.Init;
     Load:                  @TBackground.Load;
     Store:                 @TBackground.Store
    );

  _TDesktop: TTDesktop =
    (
     VMT:                   TypeOf(TDesktop);
     Init:                  @TDesktop.Init;
     Load:                  @TDesktop.Load;
     Cascade:               @TDesktop.Cascade;
     Clear:                 @TDesktop.Clear;
     Store:                 @TDesktop.Store;
     Tile:                  @TDesktop.Tile
    );

  _TProgram: TTProgram =
    (
     VMT:                   TypeOf(TProgram);
     Init:                  @TProgram.Init;
     CanMoveFocus:          @TProgram.CanMoveFocus;
     ExecuteDialog:         @TProgram.ExecuteDialog;
     InsertWindow:          @TProgram.InsertWindow;
     ActivateView:          @TProgram.ActivateView;
     SetScreenMode:         @TProgram.SetScreenMode;
     ValidView:             @TProgram.ValidView
    );

  _TApplication: TTApplication =
    (
     VMT:                   TypeOf(TApplication);
     Init:                  @TApplication.Init;
     Cascade:               @TApplication.Cascade;
     ShowUserScreen:        @TApplication.ShowUserScreen;
     Tile:                  @TApplication.Tile
    );

  _TDNApplication: TTDNApplication =
    (
     VMT:                   TypeOf(TDNApplication);
     Init:                  @TDNApplication.Init;
     ViewFile:              @TDNApplication.ViewFile;
     AddFormat:             @TDNApplication.AddFormat;
     EditFile:              @TDNApplication.EditFile;
     RetrieveDesktop:       @TDNApplication.RetrieveDesktop;
     SaveDesktop:           @TDNApplication.SaveDesktop;
     LoadDesktop:           @TDNApplication.LoadDesktop;
     StoreDesktop:          @TDNApplication.StoreDesktop;
     ChgColors:             @TDNApplication.ChgColors
    );

  _TUniWindow: TTUniWindow =
    (
     VMT:                   TypeOf(TUniWindow);
     MakeScrollBar:         @TUniWindow.MakeScrollBar
    );

  _TXFileEditor: TTXFileEditor =
    (
     VMT:                   TypeOf(TXFileEditor);
     Init:                  @TXFileEditor.Init;
     Load:                  @TXFileEditor.Load;
     Store:                 @TXFileEditor.Store;
     DoHighlite:            @TXFileEditor.DoHighlite;
     GetLine:               @TXFileEditor.GetLine;
     GetSelection:          @TXFileEditor.GetSelection;
     ValidBlock:            @TXFileEditor.ValidBlock;
     CalcMenu:              @TXFileEditor.CalcMenu;
     Search:                @TXFileEditor.Search;
     InsertBlock:           @TXFileEditor.InsertBlock;
     ModifyLine:            @TXFileEditor.ModifyLine;
     SetLimits:             @TXFileEditor.SetLimits;
     ScrollTo:              @TXFileEditor.ScrollTo;
     LimitX:                @TXFileEditor.LimitX;
     LimitY:                @TXFileEditor.LimitY;
     StoreUndoInfo:         @TXFileEditor.StoreUndoInfo;
     KeyMapConvertStr:      @TXFileEditor.KeyMapConvertStr;
     KeyMapAtInsert:        @TXFileEditor.KeyMapAtInsert;
     KeyMapAtReplace:       @TXFileEditor.KeyMapAtReplace
    );

  _TEditWindow: TTEditWindow =
    (
     VMT:                   TypeOf(TEditWindow);
     Init:                  @TEditWindow.Init;
     Load:                  @TEditWindow.Load;
     Store:                 @TEditWindow.Store
    );

  DNMethods: TDNMethods =
    (
     RecordSize:            SizeOf(TDNMethods);
     Reserved1:             0;
     Reserved2:             0;
     _TEmptyObject:         @_TEmptyObject;
     _TObject:              @_TObject;
     _TRegExp:              @_TRegExp;
     _T1:                   nil;
     _T2:                   nil;
     _T3:                   nil;
     _TStream:              @_TStream;
     _TDosStream:           @_TDosStream;
     _TBufStream:           @_TBufStream;
     _TMemoryStream:        @_TMemoryStream;
     _TCollection:          @_TCollection;
     _TSortedCollection:    @_TSortedCollection;
     _TLineCollection:      @_TLineCollection;
     _TStringCollection:    @_TStringCollection;
     _TStrCollection:       @_TStrCollection;
     _TFilesCollection:     @_TFilesCollection;
     _TView:                @_TView;
     _TFrame:               @_TFrame;
     _TScrollBar:           @_TScrollBar;
     _TGroup:               @_TGroup;
     _TWindow:              @_TWindow;
     _TMenuView:            @_TMenuView;
     _TMenuBar:             @_TMenuBar;
     _TMenuBox:             @_TMenuBox;
     _TMenuPopup:           @_TMenuPopup;
     _TStatusLine:          @_TStatusLine;
     _TDialog:              @_TDialog;
     _TInputLine:           @_TInputLine;
     _TButton:              @_TButton;
     _TCluster:             @_TCluster;
     _TRadioButtons:        @_TRadioButtons;
     _TCheckBoxes:          @_TCheckBoxes;
     _TMultiCheckBoxes:     @_TMultiCheckBoxes;
     _TScroller:            @_TScroller;
     _TListViewer:          @_TListViewer;
     _TListBox:             @_TListBox;
     _TStaticText:          @_TStaticText;
     _TParamText:           @_TParamText;
     _TLabel:               @_TLabel;
     _THistoryViewer:       @_THistoryViewer;
     _THistoryWindow:       @_THistoryWindow;
     _THistory:             @_THistory;
     _TBackground:          @_TBackground;
     _TDesktop:             @_TDesktop;
     _TProgram:             @_TProgram;
     _TApplication:         @_TApplication;
     _TDNApplication:       @_TDNApplication;
     _TUniWindow:           @_TUniWindow;
     _TXFileEditor:         @_TXFileEditor;
     _TEditWindow:          @_TEditWindow
    );

implementation

uses
  XDblWnd, FlPanelX;

function DosError: Integer;
begin
  DosError := Dos.DosError;
end;

function ExecView(P: PView): Integer;
begin
  ExecView := Desktop^.ExecView(P);
end;

function ExecAndDisposeMenu(Menu: PMenu): Integer;
var
  R: TRect;
  MenuBox: PMenuBox;
begin
  MenuBox := New(PMenuBox, Init(R, Menu, nil));
  MenuBox^.Options:=MenuBox^.Options or ofCentered;
  ExecAndDisposeMenu := Desktop^.ExecView(MenuBox);
  DisposeMenu(Menu);
  Dispose(MenuBox, Done);
end;

function GetActivePanel: Pointer;
var
  P: Pointer;
  PX: PXDoubleWindow absolute P;
begin
  P := Desktop^.Current;
  if TypeOf(PX^) = TypeOf(TXDoubleWindow) then
    with PX^ do
      if LPanel^.GetState(sfSelected) then
        GetActivePanel := LPanel
      else
        if RPanel^.GetState(sfSelected) then
          GetActivePanel := RPanel
        else
          GetActivePanel := nil
  else
    GetActivePanel := nil;
end;

function GetPassivePanel: Pointer;
var
  P: Pointer;
  PX: PXDoubleWindow absolute P;
begin
  P := Desktop^.Current;
  if TypeOf(PX^) = TypeOf(TXDoubleWindow) then
    with PX^ do
      if not LPanel^.GetState(sfSelected) then
        if RPanel^.GetState(sfSelected) then
          GetPassivePanel := LPanel
        else
          GetPassivePanel := nil
      else
        if not RPanel^.GetState(sfSelected) then
          GetPassivePanel := RPanel
        else
          GetPassivePanel := nil
  else
    GetPassivePanel := nil;
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

{&Delphi+}
function OpenRezX(const PluginName: String): LongInt;
var
  P: PString;
begin
  Result := OpenRez(PluginName);
  if Result = 0 then
    begin
      P := @PluginName;
      MessageBox(GetString(dlPlugins6), @P, mfError+mfOkButton);
    end;
end;
{&Delphi-}

end.