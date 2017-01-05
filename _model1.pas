unit _Model1;
(******

DN/2 Plugin Interface - functional model
Copyright (C) 2002 Aleksej Kozlov (Cat)
2:5030/1326.13

******)

{&Delphi+}
{&Use32+}

interface

uses
  _Defines;

type
  PEmptyObjectVMT = ^TEmptyObjectVMT;
  PObjectVMT = ^TObjectVMT;
  PRegExpVMT = ^TRegExpVMT;
  PStreamVMT = ^TStreamVMT;
  PDosStreamVMT = ^TDosStreamVMT;
  PBufStreamVMT = ^TBufStreamVMT;
  PMemoryStreamVMT = ^TMemoryStreamVMT;
  PCollectionVMT = ^TCollectionVMT;
  PSortedCollectionVMT = ^TSortedCollectionVMT;
  PLineCollectionVMT = ^TLineCollectionVMT;
  PStringCollectionVMT = ^TStringCollectionVMT;
  PStrCollectionVMT = ^TStrCollectionVMT;
  PFilesCollectionVMT = ^TFilesCollectionVMT;
  PViewVMT = ^TViewVMT;
  PFrameVMT = ^TFrameVMT;
  PScrollBarVMT = ^TScrollBarVMT;
  PGroupVMT = ^TGroupVMT;
  PWindowVMT = ^TWindowVMT;
  PMenuViewVMT = ^TMenuViewVMT;
  PMenuBarVMT = ^TMenuBarVMT;
  PMenuBoxVMT = ^TMenuBoxVMT;
  PMenuPopupVMT = ^TMenuPopupVMT;
  PStatusLineVMT = ^TStatusLineVMT;
  PDialogVMT = ^TDialogVMT;
  PInputLineVMT = ^TInputLineVMT;
  PButtonVMT = ^TButtonVMT;
  PClusterVMT = ^TClusterVMT;
  PRadioButtonsVMT = ^TRadioButtonsVMT;
  PCheckBoxesVMT = ^TCheckBoxesVMT;
  PMultiCheckBoxesVMT = ^TMultiCheckBoxesVMT;
  PScrollerVMT = ^TScrollerVMT;
  PListViewerVMT = ^TListViewerVMT;
  PListBoxVMT = ^TListBoxVMT;
  PStaticTextVMT = ^TStaticTextVMT;
  PParamTextVMT = ^TParamTextVMT;
  PLabelVMT = ^TLabelVMT;
  PHistoryViewerVMT = ^THistoryViewerVMT;
  PHistoryWindowVMT = ^THistoryWindowVMT;
  PHistoryVMT = ^THistoryVMT;
  PBackgroundVMT = ^TBackgroundVMT;
  PDesktopVMT = ^TDesktopVMT;
  PProgramVMT = ^TProgramVMT;
  PApplicationVMT = ^TApplicationVMT;
  PDNApplicationVMT = ^TDNApplicationVMT;
  PUniWindowVMT = ^TUniWindowVMT;
  PXFileEditorVMT = ^TXFileEditorVMT;
  PEditWindowVMT = ^TEditWindowVMT;
  PPercentGaugeVMT = ^TPercentGaugeVMT;
  PBarGaugeVMT = ^TBarGaugeVMT;
  PWhileViewVMT = ^TWhileViewVMT;
  PViewScrollVMT = ^TViewScrollVMT;
  PFileViewerVMT = ^TFileViewerVMT;
  PDriveVMT = ^TDriveVMT;
  PFindDriveVMT = ^TFindDriveVMT;
  PTempDriveVMT = ^TTempDriveVMT;
  PArcDriveVMT = ^TArcDriveVMT;
  PArvidDriveVMT = ^TArvidDriveVMT;


  PEmptyObject = ^TEmptyObject;
  TEmptyObject = packed record
    VMT: PEmptyObjectVMT;
    end;

  PObject = ^TObject;
  TObject = packed record
    VMT: PObjectVMT;
    ObjectIsInited: boolean;
    end;

  PRegExp = ^TRegExp;
  TRegExp = packed record
    VMT: PRegExpVMT;
    ObjectIsInited: boolean;
    FStatus: TRegExpStatus;
    FStart: integer;
    FLength: integer;
    end;

  PStream = ^TStream;
  TStream = packed record
    VMT: PStreamVMT;
    ObjectIsInited: boolean;
    Status: integer;
    ErrorInfo: integer;
    StreamSize: longInt;
    Position: longInt;
    end;

  PDosStream = ^TDosStream;
  TDOSStream = packed record
    VMT: PDosStreamVMT;
    ObjectIsInited: boolean;
    Status: integer;
    ErrorInfo: integer;
    StreamSize: longInt;
    Position: longInt;
    Handle: integer;
    FName: AsciiZ;
    FMode: word;
    end;

  PBufStream = ^TBufStream;
  TBufStream = packed record
    VMT: PBufStreamVMT;
    ObjectIsInited: boolean;
    Status: integer;
    ErrorInfo: integer;
    StreamSize: longInt;
    Position: longInt;
    Handle: integer;
    FName: AsciiZ;
    FMode: word;
    Buffer: PByteArray;
    BufSize: longInt;
    BufPtr: longInt;
    BufEnd: longInt;
    LastMode: byte;
    end;

  PMemoryStream = ^TMemoryStream;
  TMemoryStream = packed record
    VMT: PMemoryStreamVMT;
    ObjectIsInited: boolean;
    Status: integer;
    ErrorInfo: integer;
    StreamSize: longInt;
    Position: longInt;
    BlkCount: longInt;
    BlkSize: word;
    MemSize: longInt;
    BlkList: PPointerArray;
    end;

  PCollection = ^TCollection;
  TCollection = packed record
    VMT: PCollectionVMT;
    ObjectIsInited: boolean;
    Items: Pointer {PItemList};
    Count: longInt;
    Limit: longInt;
    Delta: longInt;
    Status, ErrorInfo: integer;
    end;

  PSortedCollection = ^TSortedCollection;
  TSortedCollection = packed record
    VMT: PSortedCollectionVMT;
    ObjectIsInited: boolean;
    Items: Pointer {PItemList};
    Count: longInt;
    Limit: longInt;
    Delta: longInt;
    Status, ErrorInfo: integer;
    Duplicates: boolean;
    end;

  PLineCollection = ^TLineCollection;
  TLineCollection = packed record
    VMT: PLineCollectionVMT;
    ObjectIsInited: boolean;
    Items: Pointer {PItemList};
    Count: longInt;
    Limit: longInt;
    Delta: longInt;
    Status, ErrorInfo: integer;
    LongStrings: boolean;
    end;

  PStringCollection = ^TStringCollection;
  TStringCollection = packed record
    VMT: PStringCollectionVMT;
    ObjectIsInited: boolean;
    Items: Pointer {PItemList};
    Count: longInt;
    Limit: longInt;
    Delta: longInt;
    Status, ErrorInfo: integer;
    Duplicates: boolean;
    LongStrings: boolean;
    end;

  PStrCollection = ^TStrCollection;
  TStrCollection = packed record
    VMT: PStrCollectionVMT;
    ObjectIsInited: boolean;
    Items: Pointer {PItemList};
    Count: longInt;
    Limit: longInt;
    Delta: longInt;
    Status, ErrorInfo: integer;
    Duplicates: boolean;
    end;

  PFilesCollection = ^TFilesCollection;
  TFilesCollection = packed record
    VMT: PFilesCollectionVMT;
    ObjectIsInited: boolean;
    Items: Pointer {PItemList};
    Count: longInt;
    Limit: longInt;
    Delta: longInt;
    Status, ErrorInfo: integer;
    Duplicates: boolean;
    SortMode: byte;
    Selected: longInt;
    Owner: Pointer;
    {$IFDEF WIN32}
    LFNActive: boolean;
    {$ENDIF}
    end;

  PView = ^TView;
  PGroup = ^TGroup;

  TView = packed record
    VMT: PViewVMT;
    ObjectIsInited: boolean;
    Owner: PGroup;
    Next: PView;
    Origin: TPoint;
    Size: TPoint;
    Cursor: TPoint;
    GrowMode: byte;
    DragMode: byte;
    HelpCtx: AWord;
    State: AWord;
    Options: AWord;
    EventMask: AWord;
    UpTmr: TEventTimer;
    UpdTicks: longInt;
    end;

  PFrame = ^TFrame;
  TFrame = packed record
    VMT: PFrameVMT;
    ObjectIsInited: boolean;
    Owner: PGroup;
    Next: PView;
    Origin: TPoint;
    Size: TPoint;
    Cursor: TPoint;
    GrowMode: byte;
    DragMode: byte;
    HelpCtx: AWord;
    State: AWord;
    Options: AWord;
    EventMask: AWord;
    UpTmr: TEventTimer;
    UpdTicks: longInt;
    end;

  PScrollBar = ^TScrollBar;
  TScrollBar = packed record
    VMT: PScrollBarVMT;
    ObjectIsInited: boolean;
    Owner: PGroup;
    Next: PView;
    Origin: TPoint;
    Size: TPoint;
    Cursor: TPoint;
    GrowMode: byte;
    DragMode: byte;
    HelpCtx: AWord;
    State: AWord;
    Options: AWord;
    EventMask: AWord;
    UpTmr: TEventTimer;
    UpdTicks: longInt;
    Value: longInt;
    Min: longInt;
    Max: longInt;
    PgStep: longInt;
    ArStep: longInt;
    end;

  TGroup = packed record
    VMT: PGroupVMT;
    ObjectIsInited: boolean;
    Owner: PGroup;
    Next: PView;
    Origin: TPoint;
    Size: TPoint;
    Cursor: TPoint;
    GrowMode: byte;
    DragMode: byte;
    HelpCtx: AWord;
    State: AWord;
    Options: AWord;
    EventMask: AWord;
    UpTmr: TEventTimer;
    UpdTicks: longInt;
    Last: PView;
    Current: PView;
    Phase: TPhase;
    Buffer: PVideoBuf;
    Clip: TRect;
    LockFlag: byte;
    EndState: word;
    end;

  PWindow = ^TWindow;
  TWindow = packed record
    VMT: PWindowVMT;
    ObjectIsInited: boolean;
    Owner: PGroup;
    Next: PView;
    Origin: TPoint;
    Size: TPoint;
    Cursor: TPoint;
    GrowMode: byte;
    DragMode: byte;
    HelpCtx: AWord;
    State: AWord;
    Options: AWord;
    EventMask: AWord;
    UpTmr: TEventTimer;
    UpdTicks: longInt;
    Last: PView;
    Current: PView;
    Phase: TPhase;
    Buffer: PVideoBuf;
    Clip: TRect;
    LockFlag: byte;
    EndState: word;
    Flags: byte;
    ZoomRect: TRect;
    MaxiRect: TRect;
    Number: AInt;
    Palette: AInt;
    Frame: PFrame;
    Title: PString;
    end;

  PMenuView = ^TMenuView;
  TMenuView = packed record
    VMT: PViewVMT;
    ObjectIsInited: boolean;
    Owner: PGroup;
    Next: PView;
    Origin: TPoint;
    Size: TPoint;
    Cursor: TPoint;
    GrowMode: byte;
    DragMode: byte;
    HelpCtx: AWord;
    State: AWord;
    Options: AWord;
    EventMask: AWord;
    UpTmr: TEventTimer;
    UpdTicks: longInt;
    ParentMenu: PMenuView;
    Menu: PMenu;
    Current: PMenuItem;
    end;

  PMenuBar = ^TMenuBar;
  TMenuBar = packed record
    VMT: PViewVMT;
    ObjectIsInited: boolean;
    Owner: PGroup;
    Next: PView;
    Origin: TPoint;
    Size: TPoint;
    Cursor: TPoint;
    GrowMode: byte;
    DragMode: byte;
    HelpCtx: AWord;
    State: AWord;
    Options: AWord;
    EventMask: AWord;
    UpTmr: TEventTimer;
    UpdTicks: longInt;
    ParentMenu: PMenuView;
    Menu: PMenu;
    Current: PMenuItem;
    end;

  PMenuBox = ^TMenuBox;
  TMenuBox = packed record
    VMT: PViewVMT;
    ObjectIsInited: boolean;
    Owner: PGroup;
    Next: PView;
    Origin: TPoint;
    Size: TPoint;
    Cursor: TPoint;
    GrowMode: byte;
    DragMode: byte;
    HelpCtx: AWord;
    State: AWord;
    Options: AWord;
    EventMask: AWord;
    UpTmr: TEventTimer;
    UpdTicks: longInt;
    ParentMenu: PMenuView;
    Menu: PMenu;
    Current: PMenuItem;
    TopItem: PMenuItem;
    end;

  PMenuPopup = ^TMenuPopup;
  TMenuPopup = packed record
    VMT: PViewVMT;
    ObjectIsInited: boolean;
    Owner: PGroup;
    Next: PView;
    Origin: TPoint;
    Size: TPoint;
    Cursor: TPoint;
    GrowMode: byte;
    DragMode: byte;
    HelpCtx: AWord;
    State: AWord;
    Options: AWord;
    EventMask: AWord;
    UpTmr: TEventTimer;
    UpdTicks: longInt;
    ParentMenu: PMenuView;
    Menu: PMenu;
    Current: PMenuItem;
    TopItem: PMenuItem;
    end;

  PStatusLine = ^TStatusLine;
  TStatusLine = packed record
    VMT: PViewVMT;
    ObjectIsInited: boolean;
    Owner: PGroup;
    Next: PView;
    Origin: TPoint;
    Size: TPoint;
    Cursor: TPoint;
    GrowMode: byte;
    DragMode: byte;
    HelpCtx: AWord;
    State: AWord;
    Options: AWord;
    EventMask: AWord;
    UpTmr: TEventTimer;
    UpdTicks: longInt;
    Items: PStatusItem;
    Defs: PStatusDef;
    end;

  PDialog = ^TDialog;
  TDialog = packed record
    VMT: PDialogVMT;
    ObjectIsInited: boolean;
    Owner: PGroup;
    Next: PView;
    Origin: TPoint;
    Size: TPoint;
    Cursor: TPoint;
    GrowMode: byte;
    DragMode: byte;
    HelpCtx: AWord;
    State: AWord;
    Options: AWord;
    EventMask: AWord;
    UpTmr: TEventTimer;
    UpdTicks: longInt;
    Last: PView;
    Current: PView;
    Phase: TPhase;
    Buffer: PVideoBuf;
    Clip: TRect;
    LockFlag: byte;
    EndState: word;
    Flags: byte;
    ZoomRect: TRect;
    MaxiRect: TRect;
    Number: AInt;
    Palette: AInt;
    Frame: PFrame;
    Title: PString;
    end;

  PInputline = ^TInputLine;
  TInputLine = packed record
    VMT: PInputLineVMT;
    ObjectIsInited: boolean;
    Owner: PGroup;
    Next: PView;
    Origin: TPoint;
    Size: TPoint;
    Cursor: TPoint;
    GrowMode: byte;
    DragMode: byte;
    HelpCtx: AWord;
    State: AWord;
    Options: AWord;
    EventMask: AWord;
    UpTmr: TEventTimer;
    UpdTicks: longInt;
    Data: PString;
    MaxLen: AInt;
    CurPos: AInt;
    FirstPos: AInt;
    SelStart: AInt;
    SelEnd: AInt;
    Validator: Pointer {PValidator};
    DrawShift: byte;
    CtrlK: boolean;
    end;

  PButton = ^TButton;
  TButton = packed record
    VMT: PButtonVMT;
    ObjectIsInited: boolean;
    Owner: PGroup;
    Next: PView;
    Origin: TPoint;
    Size: TPoint;
    Cursor: TPoint;
    GrowMode: byte;
    DragMode: byte;
    HelpCtx: AWord;
    State: AWord;
    Options: AWord;
    EventMask: AWord;
    UpTmr: TEventTimer;
    UpdTicks: longInt;
    Title: PString;
    Command: AWord;
    Flags: byte;
    AmDefault: boolean;
    end;

  PCluster = ^TCluster;
  TCluster = packed record
    VMT: PClusterVMT;
    ObjectIsInited: boolean;
    Owner: PGroup;
    Next: PView;
    Origin: TPoint;
    Size: TPoint;
    Cursor: TPoint;
    GrowMode: byte;
    DragMode: byte;
    HelpCtx: AWord;
    State: AWord;
    Options: AWord;
    EventMask: AWord;
    UpTmr: TEventTimer;
    UpdTicks: longInt;
    Value: longInt;
    Sel: AInt;
    EnableMask: longInt;
    Strings: TStringCollection;
    end;

  PRadioButtons = ^TRadioButtons;
  TRadioButtons = packed record
    VMT: PRadioButtonsVMT;
    ObjectIsInited: boolean;
    Owner: PGroup;
    Next: PView;
    Origin: TPoint;
    Size: TPoint;
    Cursor: TPoint;
    GrowMode: byte;
    DragMode: byte;
    HelpCtx: AWord;
    State: AWord;
    Options: AWord;
    EventMask: AWord;
    UpTmr: TEventTimer;
    UpdTicks: longInt;
    Value: longInt;
    Sel: AInt;
    EnableMask: longInt;
    Strings: TStringCollection;
    end;

  PCheckBoxes = ^TCheckBoxes;
  TCheckBoxes = packed record
    VMT: PCheckBoxesVMT;
    ObjectIsInited: boolean;
    Owner: PGroup;
    Next: PView;
    Origin: TPoint;
    Size: TPoint;
    Cursor: TPoint;
    GrowMode: byte;
    DragMode: byte;
    HelpCtx: AWord;
    State: AWord;
    Options: AWord;
    EventMask: AWord;
    UpTmr: TEventTimer;
    UpdTicks: longInt;
    Value: longInt;
    Sel: AInt;
    EnableMask: longInt;
    Strings: TStringCollection;
    end;

  PMultiCheckBoxes = ^TMultiCheckBoxes;
  TMultiCheckBoxes = packed record
    VMT: PMultiCheckBoxesVMT;
    ObjectIsInited: boolean;
    Owner: PGroup;
    Next: PView;
    Origin: TPoint;
    Size: TPoint;
    Cursor: TPoint;
    GrowMode: byte;
    DragMode: byte;
    HelpCtx: AWord;
    State: AWord;
    Options: AWord;
    EventMask: AWord;
    UpTmr: TEventTimer;
    UpdTicks: longInt;
    Value: longInt;
    Sel: AInt;
    EnableMask: longInt;
    Strings: TStringCollection;
    SelRange: byte;
    Flags: AWord;
    States: PString;
    end;

  PScroller = ^TScroller;
  TScroller = packed record
    VMT: PScrollerVMT;
    ObjectIsInited: boolean;
    Owner: PGroup;
    Next: PView;
    Origin: TPoint;
    Size: TPoint;
    Cursor: TPoint;
    GrowMode: byte;
    DragMode: byte;
    HelpCtx: AWord;
    State: AWord;
    Options: AWord;
    EventMask: AWord;
    UpTmr: TEventTimer;
    UpdTicks: longInt;
    HScrollBar: PScrollBar;
    VScrollBar: PScrollBar;
    Delta: TPoint;
    Limit: TPoint;
    DrawLock: byte;
    DrawFlag: boolean;
    end;

  PListViewer = ^TListViewer;
  TListViewer = packed record
    VMT: PListViewerVMT;
    ObjectIsInited: boolean;
    Owner: PGroup;
    Next: PView;
    Origin: TPoint;
    Size: TPoint;
    Cursor: TPoint;
    GrowMode: byte;
    DragMode: byte;
    HelpCtx: AWord;
    State: AWord;
    Options: AWord;
    EventMask: AWord;
    UpTmr: TEventTimer;
    UpdTicks: longInt;
    HScrollBar: PScrollBar;
    VScrollBar: PScrollBar;
    NumCols: longInt;
    TopItem: longInt;
    Focused: longInt;
    Range: longInt;
    end;

  PListBox = ^TListBox;
  TListBox = packed record
    VMT: PListBoxVMT;
    ObjectIsInited: boolean;
    Owner: PGroup;
    Next: PView;
    Origin: TPoint;
    Size: TPoint;
    Cursor: TPoint;
    GrowMode: byte;
    DragMode: byte;
    HelpCtx: AWord;
    State: AWord;
    Options: AWord;
    EventMask: AWord;
    UpTmr: TEventTimer;
    UpdTicks: longInt;
    HScrollBar: PScrollBar;
    VScrollBar: PScrollBar;
    NumCols: longInt;
    TopItem: longInt;
    Focused: longInt;
    Range: longInt;
    List: PCollection;
    end;

  PStaticText = ^TStaticText;
  TStaticText = packed record
    VMT: PStaticTextVMT;
    ObjectIsInited: boolean;
    Owner: PGroup;
    Next: PView;
    Origin: TPoint;
    Size: TPoint;
    Cursor: TPoint;
    GrowMode: byte;
    DragMode: byte;
    HelpCtx: AWord;
    State: AWord;
    Options: AWord;
    EventMask: AWord;
    UpTmr: TEventTimer;
    UpdTicks: longInt;
    text: PString;
    end;

  PParamText = ^TParamText;
  TParamText = packed record
    VMT: PParamTextVMT;
    ObjectIsInited: boolean;
    Owner: PGroup;
    Next: PView;
    Origin: TPoint;
    Size: TPoint;
    Cursor: TPoint;
    GrowMode: byte;
    DragMode: byte;
    HelpCtx: AWord;
    State: AWord;
    Options: AWord;
    EventMask: AWord;
    UpTmr: TEventTimer;
    UpdTicks: longInt;
    text: PString;
    ParamCount: AInt;
    ParamList: Pointer;
    end;

  PLabel = ^TLabel;
  TLabel = packed record
    VMT: PLabelVMT;
    ObjectIsInited: boolean;
    Owner: PGroup;
    Next: PView;
    Origin: TPoint;
    Size: TPoint;
    Cursor: TPoint;
    GrowMode: byte;
    DragMode: byte;
    HelpCtx: AWord;
    State: AWord;
    Options: AWord;
    EventMask: AWord;
    UpTmr: TEventTimer;
    UpdTicks: longInt;
    text: PString;
    Link: PView;
    Light: boolean;
    end;

  PHistoryViewer = ^THistoryViewer;
  THistoryViewer = packed record
    VMT: PHistoryViewerVMT;
    ObjectIsInited: boolean;
    Owner: PGroup;
    Next: PView;
    Origin: TPoint;
    Size: TPoint;
    Cursor: TPoint;
    GrowMode: byte;
    DragMode: byte;
    HelpCtx: AWord;
    State: AWord;
    Options: AWord;
    EventMask: AWord;
    UpTmr: TEventTimer;
    UpdTicks: longInt;
    HScrollBar: PScrollBar;
    VScrollBar: PScrollBar;
    NumCols: longInt;
    TopItem: longInt;
    Focused: longInt;
    Range: longInt;
    HistoryId: AWord;
    SearchPos: AWord;
    end;

  PHistoryWindow = ^THistoryWindow;
  THistoryWindow = packed record
    VMT: PHistoryWindowVMT;
    ObjectIsInited: boolean;
    Owner: PGroup;
    Next: PView;
    Origin: TPoint;
    Size: TPoint;
    Cursor: TPoint;
    GrowMode: byte;
    DragMode: byte;
    HelpCtx: AWord;
    State: AWord;
    Options: AWord;
    EventMask: AWord;
    UpTmr: TEventTimer;
    UpdTicks: longInt;
    Last: PView;
    Current: PView;
    Phase: TPhase;
    Buffer: PVideoBuf;
    Clip: TRect;
    LockFlag: byte;
    EndState: word;
    Flags: byte;
    ZoomRect: TRect;
    MaxiRect: TRect;
    Number: AInt;
    Palette: AInt;
    Frame: PFrame;
    Title: PString;
    Viewer: PListViewer;
    end;

  PHistory = ^THistory;
  THistory = packed record
    VMT: PHistoryVMT;
    ObjectIsInited: boolean;
    Owner: PGroup;
    Next: PView;
    Origin: TPoint;
    Size: TPoint;
    Cursor: TPoint;
    GrowMode: byte;
    DragMode: byte;
    HelpCtx: AWord;
    State: AWord;
    Options: AWord;
    EventMask: AWord;
    UpTmr: TEventTimer;
    UpdTicks: longInt;
    Link: PInputline;
    HistoryId: AWord;
    end;

  PBackground = ^TBackground;
  TBackground = packed record
    VMT: PBackgroundVMT;
    ObjectIsInited: boolean;
    Owner: PGroup;
    Next: PView;
    Origin: TPoint;
    Size: TPoint;
    Cursor: TPoint;
    GrowMode: byte;
    DragMode: byte;
    HelpCtx: AWord;
    State: AWord;
    Options: AWord;
    EventMask: AWord;
    UpTmr: TEventTimer;
    UpdTicks: longInt;
    Pattern: Char;
    end;

  PDesktop = ^TDesktop;
  TDesktop = packed record
    VMT: PDesktopVMT;
    ObjectIsInited: boolean;
    Owner: PGroup;
    Next: PView;
    Origin: TPoint;
    Size: TPoint;
    Cursor: TPoint;
    GrowMode: byte;
    DragMode: byte;
    HelpCtx: AWord;
    State: AWord;
    Options: AWord;
    EventMask: AWord;
    UpTmr: TEventTimer;
    UpdTicks: longInt;
    Last: PView;
    Current: PView;
    Phase: TPhase;
    Buffer: PVideoBuf;
    Clip: TRect;
    LockFlag: byte;
    EndState: word;
    Background: PBackground;
    TileColumnsFirst: boolean;
    end;

  PProgram = ^TProgram;
  TProgram = packed record
    VMT: PProgramVMT;
    ObjectIsInited: boolean;
    Owner: PGroup;
    Next: PView;
    Origin: TPoint;
    Size: TPoint;
    Cursor: TPoint;
    GrowMode: byte;
    DragMode: byte;
    HelpCtx: AWord;
    State: AWord;
    Options: AWord;
    EventMask: AWord;
    UpTmr: TEventTimer;
    UpdTicks: longInt;
    Last: PView;
    Current: PView;
    Phase: TPhase;
    Buffer: PVideoBuf;
    Clip: TRect;
    LockFlag: byte;
    EndState: word;
    IdleSecs: TEventTimer;
    FullSpeed: boolean;
    end;

  PApplication = ^TApplication;
  TApplication = packed record
    VMT: PApplicationVMT;
    ObjectIsInited: boolean;
    Owner: PGroup;
    Next: PView;
    Origin: TPoint;
    Size: TPoint;
    Cursor: TPoint;
    GrowMode: byte;
    DragMode: byte;
    HelpCtx: AWord;
    State: AWord;
    Options: AWord;
    EventMask: AWord;
    UpTmr: TEventTimer;
    UpdTicks: longInt;
    Last: PView;
    Current: PView;
    Phase: TPhase;
    Buffer: PVideoBuf;
    Clip: TRect;
    LockFlag: byte;
    EndState: word;
    IdleSecs: TEventTimer;
    FullSpeed: boolean;
    Clock: PView;
    end;

  PDNApplication = ^TDNApplication;
  TDNApplication = packed record
    VMT: PApplicationVMT;
    ObjectIsInited: boolean;
    Owner: PGroup;
    Next: PView;
    Origin: TPoint;
    Size: TPoint;
    Cursor: TPoint;
    GrowMode: byte;
    DragMode: byte;
    HelpCtx: AWord;
    State: AWord;
    Options: AWord;
    EventMask: AWord;
    UpTmr: TEventTimer;
    UpdTicks: longInt;
    Last: PView;
    Current: PView;
    Phase: TPhase;
    Buffer: PVideoBuf;
    Clip: TRect;
    LockFlag: byte;
    EndState: word;
    IdleSecs: TEventTimer;
    FullSpeed: boolean;
    Clock: PView;
    IdleClick: TEventTimer;
    IdleEvt: TEvent;
    TreeReader: Pointer {PTreeReader};
    Pk1, Pk2, Pk3, Pk4: PView;
    end;

  PUniWindow = ^TUniWindow;
  TUniWindow = packed record
    VMT: PUniWindowVMT;
    ObjectIsInited: boolean;
    Owner: PGroup;
    Next: PView;
    Origin: TPoint;
    Size: TPoint;
    Cursor: TPoint;
    GrowMode: byte;
    DragMode: byte;
    HelpCtx: AWord;
    State: AWord;
    Options: AWord;
    EventMask: AWord;
    UpTmr: TEventTimer;
    UpdTicks: longInt;
    Last: PView;
    Current: PView;
    Phase: TPhase;
    Buffer: PVideoBuf;
    Clip: TRect;
    LockFlag: byte;
    EndState: word;
    Flags: byte;
    ZoomRect: TRect;
    MaxiRect: TRect;
    Number: AInt;
    Palette: AInt;
    Frame: PFrame;
    Title: PString;
    end;

  PXFileEditor = ^TXFileEditor;
  TXFileEditor = packed record
    VMT: PViewVMT;
    ObjectIsInited: boolean;
    Owner: PGroup;
    Next: PView;
    Origin: TPoint;
    Size: TPoint;
    Cursor: TPoint;
    GrowMode: byte;
    DragMode: byte;
    HelpCtx: AWord;
    State: AWord;
    Options: AWord;
    EventMask: AWord;
    UpTmr: TEventTimer;
    UpdTicks: longInt;
    HScroll, VScroll: PScrollBar;
    ReplaceAll: boolean;
    Delta: TPoint;
    EditName: String;
    FileLines: PLineCollection;
    isValid: boolean;
    Marking: boolean;
    SmartPad: boolean;
    ClipBrd: boolean;
    OptMenu: PMenu;
    Mark, Sel: TRect;
    Pos, LastPos: TPoint;
    MarkPos: TPosArray;
    WorkLine: longInt;
    DrawMode: integer;
    WorkString, LastShape: LongString;
    OldBlockValid, SearchOnDisplay,
    InsertMode, VertBlock, Modified,
    WorkModified, JustSaved, BlockVisible,
    SpecChar, WasDelete, MouseMark, UnMark,
    LineMarking, OptimalFill, RulerVisible,
    EnableMarking, TabReplace, SearchActive: boolean;
    UndoInfo: PCollection;
    RedoInfo: PCollection;
    UndoTimes, LastSaveUndoTimes: longInt;
    ChPosition: boolean;
    Macros: PCollection;
    Locker: PStream;
    LastDir: integer;
    MemEnough: boolean;
    KeyMap: TKeyMap;
    EdOpt: TEditOptions;
    HiLitePar: THighliteParams;
    InfoL, BMrk: PView;
    MenuItemStr: array[boolean] of PString;
    RegExp: PRegExp;
    end;

  PEditWindow = ^TEditWindow;
  TEditWindow = packed record
    VMT: PEditWindowVMT;
    ObjectIsInited: boolean;
    Owner: PGroup;
    Next: PView;
    Origin: TPoint;
    Size: TPoint;
    Cursor: TPoint;
    GrowMode: byte;
    DragMode: byte;
    HelpCtx: AWord;
    State: AWord;
    Options: AWord;
    EventMask: AWord;
    UpTmr: TEventTimer;
    UpdTicks: longInt;
    Last: PView;
    Current: PView;
    Phase: TPhase;
    Buffer: PVideoBuf;
    Clip: TRect;
    LockFlag: byte;
    EndState: word;
    Flags: byte;
    ZoomRect: TRect;
    MaxiRect: TRect;
    Number: AInt;
    Palette: AInt;
    Frame: PFrame;
    Title: PString;
    AInfo: Pointer {PInfoLine};
    ABookLine: Pointer {PBookmarkLine};
    Intern: Pointer {PFileEditor};
    MenuBar: PMenuBar;
    UpMenu: PMenu;
    ModalEnd: boolean;
    end;

  PPercentGauge = ^TPercentGauge;
  TPercentGauge = packed record
    VMT: PPercentGaugeVMT;
    ObjectIsInited: boolean;
    Owner: PGroup;
    Next: PView;
    Origin: TPoint;
    Size: TPoint;
    Cursor: TPoint;
    GrowMode: byte;
    DragMode: byte;
    HelpCtx: AWord;
    State: AWord;
    Options: AWord;
    EventMask: AWord;
    UpTmr: TEventTimer;
    UpdTicks: longInt;
    MaxValue: longInt;
    CurValue: longInt;
    end;

  PBarGauge = ^TBarGauge;
  TBarGauge = packed record
    VMT: PBarGaugeVMT;
    ObjectIsInited: boolean;
    Owner: PGroup;
    Next: PView;
    Origin: TPoint;
    Size: TPoint;
    Cursor: TPoint;
    GrowMode: byte;
    DragMode: byte;
    HelpCtx: AWord;
    State: AWord;
    Options: AWord;
    EventMask: AWord;
    UpTmr: TEventTimer;
    UpdTicks: longInt;
    MaxValue: longInt;
    CurValue: longInt;
    end;

  PWhileView = ^TWhileView;
  TWhileView = packed record
    VMT: PWhileViewVMT;
    ObjectIsInited: boolean;
    Owner: PGroup;
    Next: PView;
    Origin: TPoint;
    Size: TPoint;
    Cursor: TPoint;
    GrowMode: byte;
    DragMode: byte;
    HelpCtx: AWord;
    State: AWord;
    Options: AWord;
    EventMask: AWord;
    UpTmr: TEventTimer;
    UpdTicks: longInt;
    Last: PView;
    Current: PView;
    Phase: TPhase;
    Buffer: PVideoBuf;
    Clip: TRect;
    LockFlag: byte;
    EndState: word;
    Lines: PCollection;
    But: PButton;
    QuitNormal: boolean;
    Top, Bottom: String;
    Side: (sdLeft, sdRight);
    end;

  PViewScroll = ^TViewScroll;
  TViewScroll = packed record
    VMT: PViewScrollVMT;
    ObjectIsInited: boolean;
    Owner: PGroup;
    Next: PView;
    Origin: TPoint;
    Size: TPoint;
    Cursor: TPoint;
    GrowMode: byte;
    DragMode: byte;
    HelpCtx: AWord;
    State: AWord;
    Options: AWord;
    EventMask: AWord;
    UpTmr: TEventTimer;
    UpdTicks: longInt;
    MaxV, Value: longInt;
    end;

  PFileViewer = ^TFileViewer;
  TFileViewer = packed record
    VMT: PFileViewerVMT;
    ObjectIsInited: boolean;
    Owner: PGroup;
    Next: PView;
    Origin: TPoint;
    Size: TPoint;
    Cursor: TPoint;
    GrowMode: byte;
    DragMode: byte;
    HelpCtx: AWord;
    State: AWord;
    Options: AWord;
    EventMask: AWord;
    UpTmr: TEventTimer;
    UpdTicks: longInt;
    OldSizeX: integer;
    Filtr: boolean;
    NoEdit: boolean;
    FileName: String;
    VFileName: String;
    Buf: PByteArray;
    Fl: PStream;
    UpdateViewTmr: TEventTimer;
    XDelta, ViewMode, HexPos: AInt;
    SearchActive: boolean;
    SearchResultVisible: boolean;
    SearchX: longInt;
    SB: PView;
    Wrap: byte;
    Lines: array[0..200] of
    record
      Pos: longInt;
      len: word;
      end;
    FilePos, FileSize, NumLines: longInt;
    ExposedPos, ExposedLine: longInt;
    Cur: TPoint;
    Info: PView;
    BufPos: longInt;
    BufSize, MaxLines: longInt;
    BufLines: AInt;
    KillAfterUse, isValid, QuickView, Loaded, HexEdit, BufModified:
      boolean;
    FakeKillAfterUse: boolean;
    Filter: byte;
    XLAT: TXlat;
    UseXLat: boolean;
    XLatFile: PString;
    KeyMap: TKeyMap;
    MarkPos: TPosArray;
    CtrlK: boolean;
    CtrlQ: boolean;
    HiLite: boolean;
    ScrollEOF: boolean;
    HiLitePar: THighliteParams;
    end;

  PDrive = ^TDrive;
  TDrive = packed record
    VMT: PDriveVMT;
    ObjectIsInited: boolean;
    Owner: Pointer;
    Prev: PDrive;
    DriveType: TDriveType;
    CurDir: String;
    DizOwner: String;
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
    end;

  PFindDrive = ^TFindDrive;
  TFindDrive = packed record
    VMT: PFindDriveVMT;
    ObjectIsInited: boolean;
    Owner: Pointer;
    Prev: PDrive;
    DriveType: TDriveType;
    CurDir: String;
    DizOwner: String;
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
    isDisposable: boolean;
    Files: PFilesCollection;
    Dirs: PSortedCollection;
    ListFile: PString;
    UpFile: PFileRec;
    AMask, AWhat: PString;
    end;

  PTempDrive = ^TTempDrive;
  TTempDrive = packed record
    VMT: PTempDriveVMT;
    ObjectIsInited: boolean;
    Owner: Pointer;
    Prev: PDrive;
    DriveType: TDriveType;
    CurDir: String;
    DizOwner: String;
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
    isDisposable: boolean;
    Files: PFilesCollection;
    Dirs: PSortedCollection;
    ListFile: PString;
    UpFile: PFileRec;
    AMask, AWhat: PString;
    end;

  PArcDrive = ^TArcDrive;
  TArcDrive = packed record
    VMT: PArcDriveVMT;
    ObjectIsInited: boolean;
    Owner: Pointer;
    Prev: PDrive;
    DriveType: TDriveType;
    CurDir: String;
    DizOwner: String;
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
    ArcName: String;
    VArcName: String;
    AType: Pointer {PARJArchive};
    Files: Pointer {PDirStorage};
    KillAfterUse: boolean;
    FakeKillAfterUse: boolean;
    ArcDate: longInt;
    ForceRescan: boolean;
    Password: String;
    end;

  PArvidDrive = ^TArvidDrive;
  TArvidDrive = packed record
    VMT: PArvidDriveVMT;
    ObjectIsInited: boolean;
    Owner: Pointer;
    Prev: PDrive;
    DriveType: TDriveType;
    CurDir: String;
    DizOwner: String;
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
    Name: PString;
    Stream: PStream;
    CurFile: longInt;
    CurDirPos: longInt;
    PosTableOfs: longInt;
    CurFileNum: AWord;
    CurLevel: AWord;
    CurDate: longInt;
    KillAfterUse: boolean;
    filetype: TAvdType;
    D: TTdrHeader;
    AVT: TAvtHeader;
    TapeFmt: AWord;
    TapeTotalTime: AWord;
    TapeRecordedTime: AWord;
    TotFiles: longInt;
    TotLen: longInt;
    CurDirCellPos: longInt;
    end;


  TEmptyObjectVMT = packed record
    NotForYou1: integer;
    NotForYou2: integer;
    NotForYou3: integer;
    Done: procedure (DP: integer; Obj: Pointer);
    end;

  TObjectVMT = packed record
    NotForYou1: integer;
    NotForYou2: integer;
    NotForYou3: integer;
    Done: procedure (DP: integer; Obj: Pointer);
    end;

  TRegExpVMT = packed record
    NotForYou1: integer;
    NotForYou2: integer;
    NotForYou3: integer;
    Done: procedure (DP: integer; Obj: Pointer);
    Error: procedure (AStatus: TRegExpStatus; Obj: Pointer);
    CheckBreak: function (Obj: Pointer): boolean;
    Escape: procedure (AChar: Char; var ASubExp: PChar; var ALen:
      integer; Obj: Pointer);
    end;

  TStreamVMT = packed record
    NotForYou1: integer;
    NotForYou2: integer;
    NotForYou3: integer;
    Done: procedure (DP: integer; Obj: Pointer);
    Error: procedure (Code, Info: integer; Obj: Pointer);
    Flush: procedure (Obj: Pointer);
    GetPos: function (Obj: Pointer): longInt;
    GetSize: function (Obj: Pointer): longInt;
    Read: procedure (var Buf; Count: longInt; Obj: Pointer);
    Seek: procedure (Pos: longInt; Obj: Pointer);
    Truncate: procedure (Obj: Pointer);
    Write: procedure (const Buf; Count: longInt; Obj: Pointer);
    DoOpen: procedure (OpenMode: word; Obj: Pointer);
    Close: procedure (Obj: Pointer);
    end;

  TDosStreamVMT = packed record
    NotForYou1: integer;
    NotForYou2: integer;
    NotForYou3: integer;
    Done: procedure (DP: integer; Obj: Pointer);
    Error: procedure (Code, Info: integer; Obj: Pointer);
    Flush: procedure (Obj: Pointer);
    GetPos: function (Obj: Pointer): longInt;
    GetSize: function (Obj: Pointer): longInt;
    Read: procedure (var Buf; Count: longInt; Obj: Pointer);
    Seek: procedure (Pos: longInt; Obj: Pointer);
    Truncate: procedure (Obj: Pointer);
    Write: procedure (const Buf; Count: longInt; Obj: Pointer);
    DoOpen: procedure (OpenMode: word; Obj: Pointer);
    Close: procedure (Obj: Pointer);
    ReadBlock: procedure (var Buf; Count: longInt; var BytesRead:
      word; Obj: Pointer);
    end;

  TBufStreamVMT = packed record
    NotForYou1: integer;
    NotForYou2: integer;
    NotForYou3: integer;
    Done: procedure (DP: integer; Obj: Pointer);
    Error: procedure (Code, Info: integer; Obj: Pointer);
    Flush: procedure (Obj: Pointer);
    GetPos: function (Obj: Pointer): longInt;
    GetSize: function (Obj: Pointer): longInt;
    Read: procedure (var Buf; Count: longInt; Obj: Pointer);
    Seek: procedure (Pos: longInt; Obj: Pointer);
    Truncate: procedure (Obj: Pointer);
    Write: procedure (const Buf; Count: longInt; Obj: Pointer);
    DoOpen: procedure (OpenMode: word; Obj: Pointer);
    Close: procedure (Obj: Pointer);
    ReadBlock: procedure (var Buf; Count: longInt; var BytesRead:
      word; Obj: Pointer);
    end;

  TMemoryStreamVMT = packed record
    NotForYou1: integer;
    NotForYou2: integer;
    NotForYou3: integer;
    Done: procedure (DP: integer; Obj: Pointer);
    Error: procedure (Code, Info: integer; Obj: Pointer);
    Flush: procedure (Obj: Pointer);
    GetPos: function (Obj: Pointer): longInt;
    GetSize: function (Obj: Pointer): longInt;
    Read: procedure (var Buf; Count: longInt; Obj: Pointer);
    Seek: procedure (Pos: longInt; Obj: Pointer);
    Truncate: procedure (Obj: Pointer);
    Write: procedure (const Buf; Count: longInt; Obj: Pointer);
    DoOpen: procedure (OpenMode: word; Obj: Pointer);
    Close: procedure (Obj: Pointer);
    end;

  TCollectionVMT = packed record
    NotForYou1: integer;
    NotForYou2: integer;
    NotForYou3: integer;
    Done: procedure (DP: integer; Obj: Pointer);
    Error: procedure (Code, Info: integer; Obj: Pointer);
    FreeItem: procedure (Item: Pointer; Obj: Pointer);
    GetItem: function (var s: TStream; Obj: Pointer): Pointer;
    IndexOf: function (Item: Pointer; Obj: Pointer): longInt;
    Insert: procedure (Item: Pointer; Obj: Pointer);
    PutItem: procedure (var s: TStream; Item: Pointer; Obj: Pointer);
    SetLimit: procedure (ALimit: longInt; Obj: Pointer);
    end;

  TSortedCollectionVMT = packed record
    NotForYou1: integer;
    NotForYou2: integer;
    NotForYou3: integer;
    Done: procedure (DP: integer; Obj: Pointer);
    Error: procedure (Code, Info: integer; Obj: Pointer);
    FreeItem: procedure (Item: Pointer; Obj: Pointer);
    GetItem: function (var s: TStream; Obj: Pointer): Pointer;
    IndexOf: function (Item: Pointer; Obj: Pointer): longInt;
    Insert: procedure (Item: Pointer; Obj: Pointer);
    PutItem: procedure (var s: TStream; Item: Pointer; Obj: Pointer);
    SetLimit: procedure (ALimit: longInt; Obj: Pointer);
    Compare: function (Key1, Key2: Pointer; Obj: Pointer): integer;
    KeyOf: function (Item: Pointer; Obj: Pointer): Pointer;
    Search: function (Key: Pointer; var Index: longInt; Obj: Pointer):
      boolean;
    end;

  TLineCollectionVMT = packed record
    NotForYou1: integer;
    NotForYou2: integer;
    NotForYou3: integer;
    Done: procedure (DP: integer; Obj: Pointer);
    Error: procedure (Code, Info: integer; Obj: Pointer);
    FreeItem: procedure (Item: Pointer; Obj: Pointer);
    GetItem: function (var s: TStream; Obj: Pointer): Pointer;
    IndexOf: function (Item: Pointer; Obj: Pointer): longInt;
    Insert: procedure (Item: Pointer; Obj: Pointer);
    PutItem: procedure (var s: TStream; Item: Pointer; Obj: Pointer);
    SetLimit: procedure (ALimit: longInt; Obj: Pointer);
    end;

  TStringCollectionVMT = packed record
    NotForYou1: integer;
    NotForYou2: integer;
    NotForYou3: integer;
    Done: procedure (DP: integer; Obj: Pointer);
    Error: procedure (Code, Info: integer; Obj: Pointer);
    FreeItem: procedure (Item: Pointer; Obj: Pointer);
    GetItem: function (var s: TStream; Obj: Pointer): Pointer;
    IndexOf: function (Item: Pointer; Obj: Pointer): longInt;
    Insert: procedure (Item: Pointer; Obj: Pointer);
    PutItem: procedure (var s: TStream; Item: Pointer; Obj: Pointer);
    SetLimit: procedure (ALimit: longInt; Obj: Pointer);
    Compare: function (Key1, Key2: Pointer; Obj: Pointer): integer;
    KeyOf: function (Item: Pointer; Obj: Pointer): Pointer;
    Search: function (Key: Pointer; var Index: longInt; Obj: Pointer):
      boolean;
    end;

  TStrCollectionVMT = packed record
    NotForYou1: integer;
    NotForYou2: integer;
    NotForYou3: integer;
    Done: procedure (DP: integer; Obj: Pointer);
    Error: procedure (Code, Info: integer; Obj: Pointer);
    FreeItem: procedure (Item: Pointer; Obj: Pointer);
    GetItem: function (var s: TStream; Obj: Pointer): Pointer;
    IndexOf: function (Item: Pointer; Obj: Pointer): longInt;
    Insert: procedure (Item: Pointer; Obj: Pointer);
    PutItem: procedure (var s: TStream; Item: Pointer; Obj: Pointer);
    SetLimit: procedure (ALimit: longInt; Obj: Pointer);
    Compare: function (Key1, Key2: Pointer; Obj: Pointer): integer;
    KeyOf: function (Item: Pointer; Obj: Pointer): Pointer;
    Search: function (Key: Pointer; var Index: longInt; Obj: Pointer):
      boolean;
    end;

  TFilesCollectionVMT = packed record
    NotForYou1: integer;
    NotForYou2: integer;
    NotForYou3: integer;
    Done: procedure (DP: integer; Obj: Pointer);
    Error: procedure (Code, Info: integer; Obj: Pointer);
    FreeItem: procedure (Item: Pointer; Obj: Pointer);
    GetItem: function (var s: TStream; Obj: Pointer): Pointer;
    IndexOf: function (Item: Pointer; Obj: Pointer): longInt;
    Insert: procedure (Item: Pointer; Obj: Pointer);
    PutItem: procedure (var s: TStream; Item: Pointer; Obj: Pointer);
    SetLimit: procedure (ALimit: longInt; Obj: Pointer);
    Compare: function (Key1, Key2: Pointer; Obj: Pointer): integer;
    KeyOf: function (Item: Pointer; Obj: Pointer): Pointer;
    Search: function (Key: Pointer; var Index: longInt; Obj: Pointer):
      boolean;
    Store: procedure (var s: TStream; Obj: Pointer);
    end;

  TViewVMT = packed record
    NotForYou1: integer;
    NotForYou2: integer;
    NotForYou3: integer;
    Done: procedure (DP: integer; Obj: Pointer);
    Awaken: procedure (Obj: Pointer);
    CalcBounds: procedure (var Bounds: TRect; Delta: TPoint; Obj:
      Pointer);
    ChangeBounds: procedure (var Bounds: TRect; Obj: Pointer);
    DataSize: function (Obj: Pointer): word;
    Draw: procedure (Obj: Pointer);
    EndModal: procedure (Command: word; Obj: Pointer);
    Execute: function (Obj: Pointer): word;
    GetData: procedure (var Rec; Obj: Pointer);
    GetEvent: procedure (var Event: TEvent; Obj: Pointer);
    GetHelpCtx: function (Obj: Pointer): word;
    GetPalette: function (Obj: Pointer): PPalette;
    HandleEvent: procedure (var Event: TEvent; Obj: Pointer);
    PutEvent: procedure (var Event: TEvent; Obj: Pointer);
    SetData: procedure (var Rec; Obj: Pointer);
    SetState: procedure (AState: word; Enable: boolean; Obj: Pointer);
    SizeLimits: procedure (var Min, Max: TPoint; Obj: Pointer);
    Valid: function (Command: word; Obj: Pointer): boolean;
    UpDate: procedure (Obj: Pointer);
    ResetCursor: procedure (Obj: Pointer);
    end;

  TFrameVMT = packed record
    NotForYou1: integer;
    NotForYou2: integer;
    NotForYou3: integer;
    Done: procedure (DP: integer; Obj: Pointer);
    Awaken: procedure (Obj: Pointer);
    CalcBounds: procedure (var Bounds: TRect; Delta: TPoint; Obj:
      Pointer);
    ChangeBounds: procedure (var Bounds: TRect; Obj: Pointer);
    DataSize: function (Obj: Pointer): word;
    Draw: procedure (Obj: Pointer);
    EndModal: procedure (Command: word; Obj: Pointer);
    Execute: function (Obj: Pointer): word;
    GetData: procedure (var Rec; Obj: Pointer);
    GetEvent: procedure (var Event: TEvent; Obj: Pointer);
    GetHelpCtx: function (Obj: Pointer): word;
    GetPalette: function (Obj: Pointer): PPalette;
    HandleEvent: procedure (var Event: TEvent; Obj: Pointer);
    PutEvent: procedure (var Event: TEvent; Obj: Pointer);
    SetData: procedure (var Rec; Obj: Pointer);
    SetState: procedure (AState: word; Enable: boolean; Obj: Pointer);
    SizeLimits: procedure (var Min, Max: TPoint; Obj: Pointer);
    Valid: function (Command: word; Obj: Pointer): boolean;
    UpDate: procedure (Obj: Pointer);
    ResetCursor: procedure (Obj: Pointer);
    end;

  TScrollBarVMT = packed record
    NotForYou1: integer;
    NotForYou2: integer;
    NotForYou3: integer;
    Done: procedure (DP: integer; Obj: Pointer);
    Awaken: procedure (Obj: Pointer);
    CalcBounds: procedure (var Bounds: TRect; Delta: TPoint; Obj:
      Pointer);
    ChangeBounds: procedure (var Bounds: TRect; Obj: Pointer);
    DataSize: function (Obj: Pointer): word;
    Draw: procedure (Obj: Pointer);
    EndModal: procedure (Command: word; Obj: Pointer);
    Execute: function (Obj: Pointer): word;
    GetData: procedure (var Rec; Obj: Pointer);
    GetEvent: procedure (var Event: TEvent; Obj: Pointer);
    GetHelpCtx: function (Obj: Pointer): word;
    GetPalette: function (Obj: Pointer): PPalette;
    HandleEvent: procedure (var Event: TEvent; Obj: Pointer);
    PutEvent: procedure (var Event: TEvent; Obj: Pointer);
    SetData: procedure (var Rec; Obj: Pointer);
    SetState: procedure (AState: word; Enable: boolean; Obj: Pointer);
    SizeLimits: procedure (var Min, Max: TPoint; Obj: Pointer);
    Valid: function (Command: word; Obj: Pointer): boolean;
    UpDate: procedure (Obj: Pointer);
    ResetCursor: procedure (Obj: Pointer);
    ScrollDraw: procedure (Obj: Pointer);
    ScrollStep: function (Part: longInt; Obj: Pointer): longInt;
    end;

  TGroupVMT = packed record
    NotForYou1: integer;
    NotForYou2: integer;
    NotForYou3: integer;
    Done: procedure (DP: integer; Obj: Pointer);
    Awaken: procedure (Obj: Pointer);
    CalcBounds: procedure (var Bounds: TRect; Delta: TPoint; Obj:
      Pointer);
    ChangeBounds: procedure (var Bounds: TRect; Obj: Pointer);
    DataSize: function (Obj: Pointer): word;
    Draw: procedure (Obj: Pointer);
    EndModal: procedure (Command: word; Obj: Pointer);
    Execute: function (Obj: Pointer): word;
    GetData: procedure (var Rec; Obj: Pointer);
    GetEvent: procedure (var Event: TEvent; Obj: Pointer);
    GetHelpCtx: function (Obj: Pointer): word;
    GetPalette: function (Obj: Pointer): PPalette;
    HandleEvent: procedure (var Event: TEvent; Obj: Pointer);
    PutEvent: procedure (var Event: TEvent; Obj: Pointer);
    SetData: procedure (var Rec; Obj: Pointer);
    SetState: procedure (AState: word; Enable: boolean; Obj: Pointer);
    SizeLimits: procedure (var Min, Max: TPoint; Obj: Pointer);
    Valid: function (Command: word; Obj: Pointer): boolean;
    UpDate: procedure (Obj: Pointer);
    ResetCursor: procedure (Obj: Pointer);
    EventError: procedure (var Event: TEvent; Obj: Pointer);
    Redraw: procedure (Obj: Pointer);
    end;

  TWindowVMT = packed record
    NotForYou1: integer;
    NotForYou2: integer;
    NotForYou3: integer;
    Done: procedure (DP: integer; Obj: Pointer);
    Awaken: procedure (Obj: Pointer);
    CalcBounds: procedure (var Bounds: TRect; Delta: TPoint; Obj:
      Pointer);
    ChangeBounds: procedure (var Bounds: TRect; Obj: Pointer);
    DataSize: function (Obj: Pointer): word;
    Draw: procedure (Obj: Pointer);
    EndModal: procedure (Command: word; Obj: Pointer);
    Execute: function (Obj: Pointer): word;
    GetData: procedure (var Rec; Obj: Pointer);
    GetEvent: procedure (var Event: TEvent; Obj: Pointer);
    GetHelpCtx: function (Obj: Pointer): word;
    GetPalette: function (Obj: Pointer): PPalette;
    HandleEvent: procedure (var Event: TEvent; Obj: Pointer);
    PutEvent: procedure (var Event: TEvent; Obj: Pointer);
    SetData: procedure (var Rec; Obj: Pointer);
    SetState: procedure (AState: word; Enable: boolean; Obj: Pointer);
    SizeLimits: procedure (var Min, Max: TPoint; Obj: Pointer);
    Valid: function (Command: word; Obj: Pointer): boolean;
    UpDate: procedure (Obj: Pointer);
    ResetCursor: procedure (Obj: Pointer);
    EventError: procedure (var Event: TEvent; Obj: Pointer);
    Redraw: procedure (Obj: Pointer);
    Close: procedure (Obj: Pointer);
    GetTitle: function (MaxSize: integer; Obj: Pointer): String;
    InitFrame: procedure (Obj: Pointer);
    Zoom: procedure (Obj: Pointer);
    Maxi: procedure (Obj: Pointer);
    ReactOnCmd: function (Obj: Pointer): boolean;
    end;

  TMenuViewVMT = packed record
    NotForYou1: integer;
    NotForYou2: integer;
    NotForYou3: integer;
    Done: procedure (DP: integer; Obj: Pointer);
    Awaken: procedure (Obj: Pointer);
    CalcBounds: procedure (var Bounds: TRect; Delta: TPoint; Obj:
      Pointer);
    ChangeBounds: procedure (var Bounds: TRect; Obj: Pointer);
    DataSize: function (Obj: Pointer): word;
    Draw: procedure (Obj: Pointer);
    EndModal: procedure (Command: word; Obj: Pointer);
    Execute: function (Obj: Pointer): word;
    GetData: procedure (var Rec; Obj: Pointer);
    GetEvent: procedure (var Event: TEvent; Obj: Pointer);
    GetHelpCtx: function (Obj: Pointer): word;
    GetPalette: function (Obj: Pointer): PPalette;
    HandleEvent: procedure (var Event: TEvent; Obj: Pointer);
    PutEvent: procedure (var Event: TEvent; Obj: Pointer);
    SetData: procedure (var Rec; Obj: Pointer);
    SetState: procedure (AState: word; Enable: boolean; Obj: Pointer);
    SizeLimits: procedure (var Min, Max: TPoint; Obj: Pointer);
    Valid: function (Command: word; Obj: Pointer): boolean;
    UpDate: procedure (Obj: Pointer);
    ResetCursor: procedure (Obj: Pointer);
    GetItemRect: procedure (Item: PMenuItem; var R: TRect; Obj:
      Pointer);
    NewSubView: function (var Bounds: TRect; AMenu: PMenu;
      AParentMenu: PMenuView; Obj: Pointer): PMenuView;
    end;

  TMenuBarVMT = packed record
    NotForYou1: integer;
    NotForYou2: integer;
    NotForYou3: integer;
    Done: procedure (DP: integer; Obj: Pointer);
    Awaken: procedure (Obj: Pointer);
    CalcBounds: procedure (var Bounds: TRect; Delta: TPoint; Obj:
      Pointer);
    ChangeBounds: procedure (var Bounds: TRect; Obj: Pointer);
    DataSize: function (Obj: Pointer): word;
    Draw: procedure (Obj: Pointer);
    EndModal: procedure (Command: word; Obj: Pointer);
    Execute: function (Obj: Pointer): word;
    GetData: procedure (var Rec; Obj: Pointer);
    GetEvent: procedure (var Event: TEvent; Obj: Pointer);
    GetHelpCtx: function (Obj: Pointer): word;
    GetPalette: function (Obj: Pointer): PPalette;
    HandleEvent: procedure (var Event: TEvent; Obj: Pointer);
    PutEvent: procedure (var Event: TEvent; Obj: Pointer);
    SetData: procedure (var Rec; Obj: Pointer);
    SetState: procedure (AState: word; Enable: boolean; Obj: Pointer);
    SizeLimits: procedure (var Min, Max: TPoint; Obj: Pointer);
    Valid: function (Command: word; Obj: Pointer): boolean;
    UpDate: procedure (Obj: Pointer);
    ResetCursor: procedure (Obj: Pointer);
    GetItemRect: procedure (Item: PMenuItem; var R: TRect; Obj:
      Pointer);
    NewSubView: function (var Bounds: TRect; AMenu: PMenu;
      AParentMenu: PMenuView; Obj: Pointer): PMenuView;
    end;

  TMenuBoxVMT = packed record
    NotForYou1: integer;
    NotForYou2: integer;
    NotForYou3: integer;
    Done: procedure (DP: integer; Obj: Pointer);
    Awaken: procedure (Obj: Pointer);
    CalcBounds: procedure (var Bounds: TRect; Delta: TPoint; Obj:
      Pointer);
    ChangeBounds: procedure (var Bounds: TRect; Obj: Pointer);
    DataSize: function (Obj: Pointer): word;
    Draw: procedure (Obj: Pointer);
    EndModal: procedure (Command: word; Obj: Pointer);
    Execute: function (Obj: Pointer): word;
    GetData: procedure (var Rec; Obj: Pointer);
    GetEvent: procedure (var Event: TEvent; Obj: Pointer);
    GetHelpCtx: function (Obj: Pointer): word;
    GetPalette: function (Obj: Pointer): PPalette;
    HandleEvent: procedure (var Event: TEvent; Obj: Pointer);
    PutEvent: procedure (var Event: TEvent; Obj: Pointer);
    SetData: procedure (var Rec; Obj: Pointer);
    SetState: procedure (AState: word; Enable: boolean; Obj: Pointer);
    SizeLimits: procedure (var Min, Max: TPoint; Obj: Pointer);
    Valid: function (Command: word; Obj: Pointer): boolean;
    UpDate: procedure (Obj: Pointer);
    ResetCursor: procedure (Obj: Pointer);
    GetItemRect: procedure (Item: PMenuItem; var R: TRect; Obj:
      Pointer);
    NewSubView: function (var Bounds: TRect; AMenu: PMenu;
      AParentMenu: PMenuView; Obj: Pointer): PMenuView;
    end;

  TMenuPopupVMT = packed record
    NotForYou1: integer;
    NotForYou2: integer;
    NotForYou3: integer;
    Done: procedure (DP: integer; Obj: Pointer);
    Awaken: procedure (Obj: Pointer);
    CalcBounds: procedure (var Bounds: TRect; Delta: TPoint; Obj:
      Pointer);
    ChangeBounds: procedure (var Bounds: TRect; Obj: Pointer);
    DataSize: function (Obj: Pointer): word;
    Draw: procedure (Obj: Pointer);
    EndModal: procedure (Command: word; Obj: Pointer);
    Execute: function (Obj: Pointer): word;
    GetData: procedure (var Rec; Obj: Pointer);
    GetEvent: procedure (var Event: TEvent; Obj: Pointer);
    GetHelpCtx: function (Obj: Pointer): word;
    GetPalette: function (Obj: Pointer): PPalette;
    HandleEvent: procedure (var Event: TEvent; Obj: Pointer);
    PutEvent: procedure (var Event: TEvent; Obj: Pointer);
    SetData: procedure (var Rec; Obj: Pointer);
    SetState: procedure (AState: word; Enable: boolean; Obj: Pointer);
    SizeLimits: procedure (var Min, Max: TPoint; Obj: Pointer);
    Valid: function (Command: word; Obj: Pointer): boolean;
    UpDate: procedure (Obj: Pointer);
    ResetCursor: procedure (Obj: Pointer);
    GetItemRect: procedure (Item: PMenuItem; var R: TRect; Obj:
      Pointer);
    NewSubView: function (var Bounds: TRect; AMenu: PMenu;
      AParentMenu: PMenuView; Obj: Pointer): PMenuView;
    end;

  TStatusLineVMT = packed record
    NotForYou1: integer;
    NotForYou2: integer;
    NotForYou3: integer;
    Done: procedure (DP: integer; Obj: Pointer);
    Awaken: procedure (Obj: Pointer);
    CalcBounds: procedure (var Bounds: TRect; Delta: TPoint; Obj:
      Pointer);
    ChangeBounds: procedure (var Bounds: TRect; Obj: Pointer);
    DataSize: function (Obj: Pointer): word;
    Draw: procedure (Obj: Pointer);
    EndModal: procedure (Command: word; Obj: Pointer);
    Execute: function (Obj: Pointer): word;
    GetData: procedure (var Rec; Obj: Pointer);
    GetEvent: procedure (var Event: TEvent; Obj: Pointer);
    GetHelpCtx: function (Obj: Pointer): word;
    GetPalette: function (Obj: Pointer): PPalette;
    HandleEvent: procedure (var Event: TEvent; Obj: Pointer);
    PutEvent: procedure (var Event: TEvent; Obj: Pointer);
    SetData: procedure (var Rec; Obj: Pointer);
    SetState: procedure (AState: word; Enable: boolean; Obj: Pointer);
    SizeLimits: procedure (var Min, Max: TPoint; Obj: Pointer);
    Valid: function (Command: word; Obj: Pointer): boolean;
    UpDate: procedure (Obj: Pointer);
    ResetCursor: procedure (Obj: Pointer);
    Hint: function (AHelpCtx: word; Obj: Pointer): String;
    end;

  TDialogVMT = packed record
    NotForYou1: integer;
    NotForYou2: integer;
    NotForYou3: integer;
    Done: procedure (DP: integer; Obj: Pointer);
    Awaken: procedure (Obj: Pointer);
    CalcBounds: procedure (var Bounds: TRect; Delta: TPoint; Obj:
      Pointer);
    ChangeBounds: procedure (var Bounds: TRect; Obj: Pointer);
    DataSize: function (Obj: Pointer): word;
    Draw: procedure (Obj: Pointer);
    EndModal: procedure (Command: word; Obj: Pointer);
    Execute: function (Obj: Pointer): word;
    GetData: procedure (var Rec; Obj: Pointer);
    GetEvent: procedure (var Event: TEvent; Obj: Pointer);
    GetHelpCtx: function (Obj: Pointer): word;
    GetPalette: function (Obj: Pointer): PPalette;
    HandleEvent: procedure (var Event: TEvent; Obj: Pointer);
    PutEvent: procedure (var Event: TEvent; Obj: Pointer);
    SetData: procedure (var Rec; Obj: Pointer);
    SetState: procedure (AState: word; Enable: boolean; Obj: Pointer);
    SizeLimits: procedure (var Min, Max: TPoint; Obj: Pointer);
    Valid: function (Command: word; Obj: Pointer): boolean;
    UpDate: procedure (Obj: Pointer);
    ResetCursor: procedure (Obj: Pointer);
    EventError: procedure (var Event: TEvent; Obj: Pointer);
    Redraw: procedure (Obj: Pointer);
    Close: procedure (Obj: Pointer);
    GetTitle: function (MaxSize: integer; Obj: Pointer): String;
    InitFrame: procedure (Obj: Pointer);
    Zoom: procedure (Obj: Pointer);
    Maxi: procedure (Obj: Pointer);
    ReactOnCmd: function (Obj: Pointer): boolean;
    end;

  TInputLineVMT = packed record
    NotForYou1: integer;
    NotForYou2: integer;
    NotForYou3: integer;
    Done: procedure (DP: integer; Obj: Pointer);
    Awaken: procedure (Obj: Pointer);
    CalcBounds: procedure (var Bounds: TRect; Delta: TPoint; Obj:
      Pointer);
    ChangeBounds: procedure (var Bounds: TRect; Obj: Pointer);
    DataSize: function (Obj: Pointer): word;
    Draw: procedure (Obj: Pointer);
    EndModal: procedure (Command: word; Obj: Pointer);
    Execute: function (Obj: Pointer): word;
    GetData: procedure (var Rec; Obj: Pointer);
    GetEvent: procedure (var Event: TEvent; Obj: Pointer);
    GetHelpCtx: function (Obj: Pointer): word;
    GetPalette: function (Obj: Pointer): PPalette;
    HandleEvent: procedure (var Event: TEvent; Obj: Pointer);
    PutEvent: procedure (var Event: TEvent; Obj: Pointer);
    SetData: procedure (var Rec; Obj: Pointer);
    SetState: procedure (AState: word; Enable: boolean; Obj: Pointer);
    SizeLimits: procedure (var Min, Max: TPoint; Obj: Pointer);
    Valid: function (Command: word; Obj: Pointer): boolean;
    UpDate: procedure (Obj: Pointer);
    ResetCursor: procedure (Obj: Pointer);
    end;

  TButtonVMT = packed record
    NotForYou1: integer;
    NotForYou2: integer;
    NotForYou3: integer;
    Done: procedure (DP: integer; Obj: Pointer);
    Awaken: procedure (Obj: Pointer);
    CalcBounds: procedure (var Bounds: TRect; Delta: TPoint; Obj:
      Pointer);
    ChangeBounds: procedure (var Bounds: TRect; Obj: Pointer);
    DataSize: function (Obj: Pointer): word;
    Draw: procedure (Obj: Pointer);
    EndModal: procedure (Command: word; Obj: Pointer);
    Execute: function (Obj: Pointer): word;
    GetData: procedure (var Rec; Obj: Pointer);
    GetEvent: procedure (var Event: TEvent; Obj: Pointer);
    GetHelpCtx: function (Obj: Pointer): word;
    GetPalette: function (Obj: Pointer): PPalette;
    HandleEvent: procedure (var Event: TEvent; Obj: Pointer);
    PutEvent: procedure (var Event: TEvent; Obj: Pointer);
    SetData: procedure (var Rec; Obj: Pointer);
    SetState: procedure (AState: word; Enable: boolean; Obj: Pointer);
    SizeLimits: procedure (var Min, Max: TPoint; Obj: Pointer);
    Valid: function (Command: word; Obj: Pointer): boolean;
    UpDate: procedure (Obj: Pointer);
    ResetCursor: procedure (Obj: Pointer);
    Press: procedure (Obj: Pointer);
    end;

  TClusterVMT = packed record
    NotForYou1: integer;
    NotForYou2: integer;
    NotForYou3: integer;
    Done: procedure (DP: integer; Obj: Pointer);
    Awaken: procedure (Obj: Pointer);
    CalcBounds: procedure (var Bounds: TRect; Delta: TPoint; Obj:
      Pointer);
    ChangeBounds: procedure (var Bounds: TRect; Obj: Pointer);
    DataSize: function (Obj: Pointer): word;
    Draw: procedure (Obj: Pointer);
    EndModal: procedure (Command: word; Obj: Pointer);
    Execute: function (Obj: Pointer): word;
    GetData: procedure (var Rec; Obj: Pointer);
    GetEvent: procedure (var Event: TEvent; Obj: Pointer);
    GetHelpCtx: function (Obj: Pointer): word;
    GetPalette: function (Obj: Pointer): PPalette;
    HandleEvent: procedure (var Event: TEvent; Obj: Pointer);
    PutEvent: procedure (var Event: TEvent; Obj: Pointer);
    SetData: procedure (var Rec; Obj: Pointer);
    SetState: procedure (AState: word; Enable: boolean; Obj: Pointer);
    SizeLimits: procedure (var Min, Max: TPoint; Obj: Pointer);
    Valid: function (Command: word; Obj: Pointer): boolean;
    UpDate: procedure (Obj: Pointer);
    ResetCursor: procedure (Obj: Pointer);
    Mark: function (Item: integer; Obj: Pointer): boolean;
    MultiMark: function (Item: integer; Obj: Pointer): byte;
    Press: procedure (Item: integer; Obj: Pointer);
    MovedTo: procedure (Item: integer; Obj: Pointer);
    end;

  TRadioButtonsVMT = packed record
    NotForYou1: integer;
    NotForYou2: integer;
    NotForYou3: integer;
    Done: procedure (DP: integer; Obj: Pointer);
    Awaken: procedure (Obj: Pointer);
    CalcBounds: procedure (var Bounds: TRect; Delta: TPoint; Obj:
      Pointer);
    ChangeBounds: procedure (var Bounds: TRect; Obj: Pointer);
    DataSize: function (Obj: Pointer): word;
    Draw: procedure (Obj: Pointer);
    EndModal: procedure (Command: word; Obj: Pointer);
    Execute: function (Obj: Pointer): word;
    GetData: procedure (var Rec; Obj: Pointer);
    GetEvent: procedure (var Event: TEvent; Obj: Pointer);
    GetHelpCtx: function (Obj: Pointer): word;
    GetPalette: function (Obj: Pointer): PPalette;
    HandleEvent: procedure (var Event: TEvent; Obj: Pointer);
    PutEvent: procedure (var Event: TEvent; Obj: Pointer);
    SetData: procedure (var Rec; Obj: Pointer);
    SetState: procedure (AState: word; Enable: boolean; Obj: Pointer);
    SizeLimits: procedure (var Min, Max: TPoint; Obj: Pointer);
    Valid: function (Command: word; Obj: Pointer): boolean;
    UpDate: procedure (Obj: Pointer);
    ResetCursor: procedure (Obj: Pointer);
    Mark: function (Item: integer; Obj: Pointer): boolean;
    MultiMark: function (Item: integer; Obj: Pointer): byte;
    Press: procedure (Item: integer; Obj: Pointer);
    MovedTo: procedure (Item: integer; Obj: Pointer);
    end;

  TCheckBoxesVMT = packed record
    NotForYou1: integer;
    NotForYou2: integer;
    NotForYou3: integer;
    Done: procedure (DP: integer; Obj: Pointer);
    Awaken: procedure (Obj: Pointer);
    CalcBounds: procedure (var Bounds: TRect; Delta: TPoint; Obj:
      Pointer);
    ChangeBounds: procedure (var Bounds: TRect; Obj: Pointer);
    DataSize: function (Obj: Pointer): word;
    Draw: procedure (Obj: Pointer);
    EndModal: procedure (Command: word; Obj: Pointer);
    Execute: function (Obj: Pointer): word;
    GetData: procedure (var Rec; Obj: Pointer);
    GetEvent: procedure (var Event: TEvent; Obj: Pointer);
    GetHelpCtx: function (Obj: Pointer): word;
    GetPalette: function (Obj: Pointer): PPalette;
    HandleEvent: procedure (var Event: TEvent; Obj: Pointer);
    PutEvent: procedure (var Event: TEvent; Obj: Pointer);
    SetData: procedure (var Rec; Obj: Pointer);
    SetState: procedure (AState: word; Enable: boolean; Obj: Pointer);
    SizeLimits: procedure (var Min, Max: TPoint; Obj: Pointer);
    Valid: function (Command: word; Obj: Pointer): boolean;
    UpDate: procedure (Obj: Pointer);
    ResetCursor: procedure (Obj: Pointer);
    Mark: function (Item: integer; Obj: Pointer): boolean;
    MultiMark: function (Item: integer; Obj: Pointer): byte;
    Press: procedure (Item: integer; Obj: Pointer);
    MovedTo: procedure (Item: integer; Obj: Pointer);
    end;

  TMultiCheckBoxesVMT = packed record
    NotForYou1: integer;
    NotForYou2: integer;
    NotForYou3: integer;
    Done: procedure (DP: integer; Obj: Pointer);
    Awaken: procedure (Obj: Pointer);
    CalcBounds: procedure (var Bounds: TRect; Delta: TPoint; Obj:
      Pointer);
    ChangeBounds: procedure (var Bounds: TRect; Obj: Pointer);
    DataSize: function (Obj: Pointer): word;
    Draw: procedure (Obj: Pointer);
    EndModal: procedure (Command: word; Obj: Pointer);
    Execute: function (Obj: Pointer): word;
    GetData: procedure (var Rec; Obj: Pointer);
    GetEvent: procedure (var Event: TEvent; Obj: Pointer);
    GetHelpCtx: function (Obj: Pointer): word;
    GetPalette: function (Obj: Pointer): PPalette;
    HandleEvent: procedure (var Event: TEvent; Obj: Pointer);
    PutEvent: procedure (var Event: TEvent; Obj: Pointer);
    SetData: procedure (var Rec; Obj: Pointer);
    SetState: procedure (AState: word; Enable: boolean; Obj: Pointer);
    SizeLimits: procedure (var Min, Max: TPoint; Obj: Pointer);
    Valid: function (Command: word; Obj: Pointer): boolean;
    UpDate: procedure (Obj: Pointer);
    ResetCursor: procedure (Obj: Pointer);
    Mark: function (Item: integer; Obj: Pointer): boolean;
    MultiMark: function (Item: integer; Obj: Pointer): byte;
    Press: procedure (Item: integer; Obj: Pointer);
    MovedTo: procedure (Item: integer; Obj: Pointer);
    end;

  TScrollerVMT = packed record
    NotForYou1: integer;
    NotForYou2: integer;
    NotForYou3: integer;
    Done: procedure (DP: integer; Obj: Pointer);
    Awaken: procedure (Obj: Pointer);
    CalcBounds: procedure (var Bounds: TRect; Delta: TPoint; Obj:
      Pointer);
    ChangeBounds: procedure (var Bounds: TRect; Obj: Pointer);
    DataSize: function (Obj: Pointer): word;
    Draw: procedure (Obj: Pointer);
    EndModal: procedure (Command: word; Obj: Pointer);
    Execute: function (Obj: Pointer): word;
    GetData: procedure (var Rec; Obj: Pointer);
    GetEvent: procedure (var Event: TEvent; Obj: Pointer);
    GetHelpCtx: function (Obj: Pointer): word;
    GetPalette: function (Obj: Pointer): PPalette;
    HandleEvent: procedure (var Event: TEvent; Obj: Pointer);
    PutEvent: procedure (var Event: TEvent; Obj: Pointer);
    SetData: procedure (var Rec; Obj: Pointer);
    SetState: procedure (AState: word; Enable: boolean; Obj: Pointer);
    SizeLimits: procedure (var Min, Max: TPoint; Obj: Pointer);
    Valid: function (Command: word; Obj: Pointer): boolean;
    UpDate: procedure (Obj: Pointer);
    ResetCursor: procedure (Obj: Pointer);
    ScrollDraw: procedure (Obj: Pointer);
    end;

  TListViewerVMT = packed record
    NotForYou1: integer;
    NotForYou2: integer;
    NotForYou3: integer;
    Done: procedure (DP: integer; Obj: Pointer);
    Awaken: procedure (Obj: Pointer);
    CalcBounds: procedure (var Bounds: TRect; Delta: TPoint; Obj:
      Pointer);
    ChangeBounds: procedure (var Bounds: TRect; Obj: Pointer);
    DataSize: function (Obj: Pointer): word;
    Draw: procedure (Obj: Pointer);
    EndModal: procedure (Command: word; Obj: Pointer);
    Execute: function (Obj: Pointer): word;
    GetData: procedure (var Rec; Obj: Pointer);
    GetEvent: procedure (var Event: TEvent; Obj: Pointer);
    GetHelpCtx: function (Obj: Pointer): word;
    GetPalette: function (Obj: Pointer): PPalette;
    HandleEvent: procedure (var Event: TEvent; Obj: Pointer);
    PutEvent: procedure (var Event: TEvent; Obj: Pointer);
    SetData: procedure (var Rec; Obj: Pointer);
    SetState: procedure (AState: word; Enable: boolean; Obj: Pointer);
    SizeLimits: procedure (var Min, Max: TPoint; Obj: Pointer);
    Valid: function (Command: word; Obj: Pointer): boolean;
    UpDate: procedure (Obj: Pointer);
    ResetCursor: procedure (Obj: Pointer);
    FocusItem: procedure (Item: longInt; Obj: Pointer);
    GetText: function (Item: longInt; MaxLen: integer; Obj: Pointer):
      String;
    IsSelected: function (Item: longInt; Obj: Pointer): boolean;
    SelectItem: procedure (Item: longInt; Obj: Pointer);
    FocusItemNum: procedure (Item: longInt; Obj: Pointer);
    end;

  TListBoxVMT = packed record
    NotForYou1: integer;
    NotForYou2: integer;
    NotForYou3: integer;
    Done: procedure (DP: integer; Obj: Pointer);
    Awaken: procedure (Obj: Pointer);
    CalcBounds: procedure (var Bounds: TRect; Delta: TPoint; Obj:
      Pointer);
    ChangeBounds: procedure (var Bounds: TRect; Obj: Pointer);
    DataSize: function (Obj: Pointer): word;
    Draw: procedure (Obj: Pointer);
    EndModal: procedure (Command: word; Obj: Pointer);
    Execute: function (Obj: Pointer): word;
    GetData: procedure (var Rec; Obj: Pointer);
    GetEvent: procedure (var Event: TEvent; Obj: Pointer);
    GetHelpCtx: function (Obj: Pointer): word;
    GetPalette: function (Obj: Pointer): PPalette;
    HandleEvent: procedure (var Event: TEvent; Obj: Pointer);
    PutEvent: procedure (var Event: TEvent; Obj: Pointer);
    SetData: procedure (var Rec; Obj: Pointer);
    SetState: procedure (AState: word; Enable: boolean; Obj: Pointer);
    SizeLimits: procedure (var Min, Max: TPoint; Obj: Pointer);
    Valid: function (Command: word; Obj: Pointer): boolean;
    UpDate: procedure (Obj: Pointer);
    ResetCursor: procedure (Obj: Pointer);
    FocusItem: procedure (Item: longInt; Obj: Pointer);
    GetText: function (Item: longInt; MaxLen: integer; Obj: Pointer):
      String;
    IsSelected: function (Item: longInt; Obj: Pointer): boolean;
    SelectItem: procedure (Item: longInt; Obj: Pointer);
    FocusItemNum: procedure (Item: longInt; Obj: Pointer);
    NewLisT: procedure (AList: PCollection; Obj: Pointer);
    end;

  TStaticTextVMT = packed record
    NotForYou1: integer;
    NotForYou2: integer;
    NotForYou3: integer;
    Done: procedure (DP: integer; Obj: Pointer);
    Awaken: procedure (Obj: Pointer);
    CalcBounds: procedure (var Bounds: TRect; Delta: TPoint; Obj:
      Pointer);
    ChangeBounds: procedure (var Bounds: TRect; Obj: Pointer);
    DataSize: function (Obj: Pointer): word;
    Draw: procedure (Obj: Pointer);
    EndModal: procedure (Command: word; Obj: Pointer);
    Execute: function (Obj: Pointer): word;
    GetData: procedure (var Rec; Obj: Pointer);
    GetEvent: procedure (var Event: TEvent; Obj: Pointer);
    GetHelpCtx: function (Obj: Pointer): word;
    GetPalette: function (Obj: Pointer): PPalette;
    HandleEvent: procedure (var Event: TEvent; Obj: Pointer);
    PutEvent: procedure (var Event: TEvent; Obj: Pointer);
    SetData: procedure (var Rec; Obj: Pointer);
    SetState: procedure (AState: word; Enable: boolean; Obj: Pointer);
    SizeLimits: procedure (var Min, Max: TPoint; Obj: Pointer);
    Valid: function (Command: word; Obj: Pointer): boolean;
    UpDate: procedure (Obj: Pointer);
    ResetCursor: procedure (Obj: Pointer);
    GetText: procedure (var s: String; Obj: Pointer);
    end;

  TParamTextVMT = packed record
    NotForYou1: integer;
    NotForYou2: integer;
    NotForYou3: integer;
    Done: procedure (DP: integer; Obj: Pointer);
    Awaken: procedure (Obj: Pointer);
    CalcBounds: procedure (var Bounds: TRect; Delta: TPoint; Obj:
      Pointer);
    ChangeBounds: procedure (var Bounds: TRect; Obj: Pointer);
    DataSize: function (Obj: Pointer): word;
    Draw: procedure (Obj: Pointer);
    EndModal: procedure (Command: word; Obj: Pointer);
    Execute: function (Obj: Pointer): word;
    GetData: procedure (var Rec; Obj: Pointer);
    GetEvent: procedure (var Event: TEvent; Obj: Pointer);
    GetHelpCtx: function (Obj: Pointer): word;
    GetPalette: function (Obj: Pointer): PPalette;
    HandleEvent: procedure (var Event: TEvent; Obj: Pointer);
    PutEvent: procedure (var Event: TEvent; Obj: Pointer);
    SetData: procedure (var Rec; Obj: Pointer);
    SetState: procedure (AState: word; Enable: boolean; Obj: Pointer);
    SizeLimits: procedure (var Min, Max: TPoint; Obj: Pointer);
    Valid: function (Command: word; Obj: Pointer): boolean;
    UpDate: procedure (Obj: Pointer);
    ResetCursor: procedure (Obj: Pointer);
    GetText: procedure (var s: String; Obj: Pointer);
    end;

  TLabelVMT = packed record
    NotForYou1: integer;
    NotForYou2: integer;
    NotForYou3: integer;
    Done: procedure (DP: integer; Obj: Pointer);
    Awaken: procedure (Obj: Pointer);
    CalcBounds: procedure (var Bounds: TRect; Delta: TPoint; Obj:
      Pointer);
    ChangeBounds: procedure (var Bounds: TRect; Obj: Pointer);
    DataSize: function (Obj: Pointer): word;
    Draw: procedure (Obj: Pointer);
    EndModal: procedure (Command: word; Obj: Pointer);
    Execute: function (Obj: Pointer): word;
    GetData: procedure (var Rec; Obj: Pointer);
    GetEvent: procedure (var Event: TEvent; Obj: Pointer);
    GetHelpCtx: function (Obj: Pointer): word;
    GetPalette: function (Obj: Pointer): PPalette;
    HandleEvent: procedure (var Event: TEvent; Obj: Pointer);
    PutEvent: procedure (var Event: TEvent; Obj: Pointer);
    SetData: procedure (var Rec; Obj: Pointer);
    SetState: procedure (AState: word; Enable: boolean; Obj: Pointer);
    SizeLimits: procedure (var Min, Max: TPoint; Obj: Pointer);
    Valid: function (Command: word; Obj: Pointer): boolean;
    UpDate: procedure (Obj: Pointer);
    ResetCursor: procedure (Obj: Pointer);
    GetText: procedure (var s: String; Obj: Pointer);
    end;

  THistoryViewerVMT = packed record
    NotForYou1: integer;
    NotForYou2: integer;
    NotForYou3: integer;
    Done: procedure (DP: integer; Obj: Pointer);
    Awaken: procedure (Obj: Pointer);
    CalcBounds: procedure (var Bounds: TRect; Delta: TPoint; Obj:
      Pointer);
    ChangeBounds: procedure (var Bounds: TRect; Obj: Pointer);
    DataSize: function (Obj: Pointer): word;
    Draw: procedure (Obj: Pointer);
    EndModal: procedure (Command: word; Obj: Pointer);
    Execute: function (Obj: Pointer): word;
    GetData: procedure (var Rec; Obj: Pointer);
    GetEvent: procedure (var Event: TEvent; Obj: Pointer);
    GetHelpCtx: function (Obj: Pointer): word;
    GetPalette: function (Obj: Pointer): PPalette;
    HandleEvent: procedure (var Event: TEvent; Obj: Pointer);
    PutEvent: procedure (var Event: TEvent; Obj: Pointer);
    SetData: procedure (var Rec; Obj: Pointer);
    SetState: procedure (AState: word; Enable: boolean; Obj: Pointer);
    SizeLimits: procedure (var Min, Max: TPoint; Obj: Pointer);
    Valid: function (Command: word; Obj: Pointer): boolean;
    UpDate: procedure (Obj: Pointer);
    ResetCursor: procedure (Obj: Pointer);
    FocusItem: procedure (Item: longInt; Obj: Pointer);
    GetText: function (Item: longInt; MaxLen: integer; Obj: Pointer):
      String;
    IsSelected: function (Item: longInt; Obj: Pointer): boolean;
    SelectItem: procedure (Item: longInt; Obj: Pointer);
    FocusItemNum: procedure (Item: longInt; Obj: Pointer);
    end;

  THistoryWindowVMT = packed record
    NotForYou1: integer;
    NotForYou2: integer;
    NotForYou3: integer;
    Done: procedure (DP: integer; Obj: Pointer);
    Awaken: procedure (Obj: Pointer);
    CalcBounds: procedure (var Bounds: TRect; Delta: TPoint; Obj:
      Pointer);
    ChangeBounds: procedure (var Bounds: TRect; Obj: Pointer);
    DataSize: function (Obj: Pointer): word;
    Draw: procedure (Obj: Pointer);
    EndModal: procedure (Command: word; Obj: Pointer);
    Execute: function (Obj: Pointer): word;
    GetData: procedure (var Rec; Obj: Pointer);
    GetEvent: procedure (var Event: TEvent; Obj: Pointer);
    GetHelpCtx: function (Obj: Pointer): word;
    GetPalette: function (Obj: Pointer): PPalette;
    HandleEvent: procedure (var Event: TEvent; Obj: Pointer);
    PutEvent: procedure (var Event: TEvent; Obj: Pointer);
    SetData: procedure (var Rec; Obj: Pointer);
    SetState: procedure (AState: word; Enable: boolean; Obj: Pointer);
    SizeLimits: procedure (var Min, Max: TPoint; Obj: Pointer);
    Valid: function (Command: word; Obj: Pointer): boolean;
    UpDate: procedure (Obj: Pointer);
    ResetCursor: procedure (Obj: Pointer);
    EventError: procedure (var Event: TEvent; Obj: Pointer);
    Redraw: procedure (Obj: Pointer);
    Close: procedure (Obj: Pointer);
    GetTitle: function (MaxSize: integer; Obj: Pointer): String;
    InitFrame: procedure (Obj: Pointer);
    Zoom: procedure (Obj: Pointer);
    Maxi: procedure (Obj: Pointer);
    ReactOnCmd: function (Obj: Pointer): boolean;
    GetSelection: function (Obj: Pointer): String;
    InitViewer: procedure (HistoryId: AWord; Obj: Pointer);
    end;

  THistoryVMT = packed record
    NotForYou1: integer;
    NotForYou2: integer;
    NotForYou3: integer;
    Done: procedure (DP: integer; Obj: Pointer);
    Awaken: procedure (Obj: Pointer);
    CalcBounds: procedure (var Bounds: TRect; Delta: TPoint; Obj:
      Pointer);
    ChangeBounds: procedure (var Bounds: TRect; Obj: Pointer);
    DataSize: function (Obj: Pointer): word;
    Draw: procedure (Obj: Pointer);
    EndModal: procedure (Command: word; Obj: Pointer);
    Execute: function (Obj: Pointer): word;
    GetData: procedure (var Rec; Obj: Pointer);
    GetEvent: procedure (var Event: TEvent; Obj: Pointer);
    GetHelpCtx: function (Obj: Pointer): word;
    GetPalette: function (Obj: Pointer): PPalette;
    HandleEvent: procedure (var Event: TEvent; Obj: Pointer);
    PutEvent: procedure (var Event: TEvent; Obj: Pointer);
    SetData: procedure (var Rec; Obj: Pointer);
    SetState: procedure (AState: word; Enable: boolean; Obj: Pointer);
    SizeLimits: procedure (var Min, Max: TPoint; Obj: Pointer);
    Valid: function (Command: word; Obj: Pointer): boolean;
    UpDate: procedure (Obj: Pointer);
    ResetCursor: procedure (Obj: Pointer);
    InitHistoryWindow: function (var Bounds: TRect; Obj: Pointer):
      PHistoryWindow;
    RecordHistory: procedure (const s: String; Obj: Pointer);
    end;

  TBackgroundVMT = packed record
    NotForYou1: integer;
    NotForYou2: integer;
    NotForYou3: integer;
    Done: procedure (DP: integer; Obj: Pointer);
    Awaken: procedure (Obj: Pointer);
    CalcBounds: procedure (var Bounds: TRect; Delta: TPoint; Obj:
      Pointer);
    ChangeBounds: procedure (var Bounds: TRect; Obj: Pointer);
    DataSize: function (Obj: Pointer): word;
    Draw: procedure (Obj: Pointer);
    EndModal: procedure (Command: word; Obj: Pointer);
    Execute: function (Obj: Pointer): word;
    GetData: procedure (var Rec; Obj: Pointer);
    GetEvent: procedure (var Event: TEvent; Obj: Pointer);
    GetHelpCtx: function (Obj: Pointer): word;
    GetPalette: function (Obj: Pointer): PPalette;
    HandleEvent: procedure (var Event: TEvent; Obj: Pointer);
    PutEvent: procedure (var Event: TEvent; Obj: Pointer);
    SetData: procedure (var Rec; Obj: Pointer);
    SetState: procedure (AState: word; Enable: boolean; Obj: Pointer);
    SizeLimits: procedure (var Min, Max: TPoint; Obj: Pointer);
    Valid: function (Command: word; Obj: Pointer): boolean;
    UpDate: procedure (Obj: Pointer);
    ResetCursor: procedure (Obj: Pointer);
    end;

  TDesktopVMT = packed record
    NotForYou1: integer;
    NotForYou2: integer;
    NotForYou3: integer;
    Done: procedure (DP: integer; Obj: Pointer);
    Awaken: procedure (Obj: Pointer);
    CalcBounds: procedure (var Bounds: TRect; Delta: TPoint; Obj:
      Pointer);
    ChangeBounds: procedure (var Bounds: TRect; Obj: Pointer);
    DataSize: function (Obj: Pointer): word;
    Draw: procedure (Obj: Pointer);
    EndModal: procedure (Command: word; Obj: Pointer);
    Execute: function (Obj: Pointer): word;
    GetData: procedure (var Rec; Obj: Pointer);
    GetEvent: procedure (var Event: TEvent; Obj: Pointer);
    GetHelpCtx: function (Obj: Pointer): word;
    GetPalette: function (Obj: Pointer): PPalette;
    HandleEvent: procedure (var Event: TEvent; Obj: Pointer);
    PutEvent: procedure (var Event: TEvent; Obj: Pointer);
    SetData: procedure (var Rec; Obj: Pointer);
    SetState: procedure (AState: word; Enable: boolean; Obj: Pointer);
    SizeLimits: procedure (var Min, Max: TPoint; Obj: Pointer);
    Valid: function (Command: word; Obj: Pointer): boolean;
    UpDate: procedure (Obj: Pointer);
    ResetCursor: procedure (Obj: Pointer);
    EventError: procedure (var Event: TEvent; Obj: Pointer);
    Redraw: procedure (Obj: Pointer);
    InitBackground: procedure (Obj: Pointer);
    TileError: procedure (Obj: Pointer);
    end;

  TProgramVMT = packed record
    NotForYou1: integer;
    NotForYou2: integer;
    NotForYou3: integer;
    Done: procedure (DP: integer; Obj: Pointer);
    Awaken: procedure (Obj: Pointer);
    CalcBounds: procedure (var Bounds: TRect; Delta: TPoint; Obj:
      Pointer);
    ChangeBounds: procedure (var Bounds: TRect; Obj: Pointer);
    DataSize: function (Obj: Pointer): word;
    Draw: procedure (Obj: Pointer);
    EndModal: procedure (Command: word; Obj: Pointer);
    Execute: function (Obj: Pointer): word;
    GetData: procedure (var Rec; Obj: Pointer);
    GetEvent: procedure (var Event: TEvent; Obj: Pointer);
    GetHelpCtx: function (Obj: Pointer): word;
    GetPalette: function (Obj: Pointer): PPalette;
    HandleEvent: procedure (var Event: TEvent; Obj: Pointer);
    PutEvent: procedure (var Event: TEvent; Obj: Pointer);
    SetData: procedure (var Rec; Obj: Pointer);
    SetState: procedure (AState: word; Enable: boolean; Obj: Pointer);
    SizeLimits: procedure (var Min, Max: TPoint; Obj: Pointer);
    Valid: function (Command: word; Obj: Pointer): boolean;
    UpDate: procedure (Obj: Pointer);
    ResetCursor: procedure (Obj: Pointer);
    EventError: procedure (var Event: TEvent; Obj: Pointer);
    Redraw: procedure (Obj: Pointer);
    Idle: procedure (Obj: Pointer);
    InitDesktop: procedure (Obj: Pointer);
    InitMenuBar: procedure (Obj: Pointer);
    InitScreen: procedure (Obj: Pointer);
    InitStatusLine: procedure (Obj: Pointer);
    InitCommandLine: procedure (Obj: Pointer);
    OutOfMemory: procedure (Obj: Pointer);
    Run: procedure (Obj: Pointer);
    end;

  TApplicationVMT = packed record
    NotForYou1: integer;
    NotForYou2: integer;
    NotForYou3: integer;
    Done: procedure (DP: integer; Obj: Pointer);
    Awaken: procedure (Obj: Pointer);
    CalcBounds: procedure (var Bounds: TRect; Delta: TPoint; Obj:
      Pointer);
    ChangeBounds: procedure (var Bounds: TRect; Obj: Pointer);
    DataSize: function (Obj: Pointer): word;
    Draw: procedure (Obj: Pointer);
    EndModal: procedure (Command: word; Obj: Pointer);
    Execute: function (Obj: Pointer): word;
    GetData: procedure (var Rec; Obj: Pointer);
    GetEvent: procedure (var Event: TEvent; Obj: Pointer);
    GetHelpCtx: function (Obj: Pointer): word;
    GetPalette: function (Obj: Pointer): PPalette;
    HandleEvent: procedure (var Event: TEvent; Obj: Pointer);
    PutEvent: procedure (var Event: TEvent; Obj: Pointer);
    SetData: procedure (var Rec; Obj: Pointer);
    SetState: procedure (AState: word; Enable: boolean; Obj: Pointer);
    SizeLimits: procedure (var Min, Max: TPoint; Obj: Pointer);
    Valid: function (Command: word; Obj: Pointer): boolean;
    UpDate: procedure (Obj: Pointer);
    ResetCursor: procedure (Obj: Pointer);
    EventError: procedure (var Event: TEvent; Obj: Pointer);
    Redraw: procedure (Obj: Pointer);
    Idle: procedure (Obj: Pointer);
    InitDesktop: procedure (Obj: Pointer);
    InitMenuBar: procedure (Obj: Pointer);
    InitScreen: procedure (Obj: Pointer);
    InitStatusLine: procedure (Obj: Pointer);
    InitCommandLine: procedure (Obj: Pointer);
    OutOfMemory: procedure (Obj: Pointer);
    Run: procedure (Obj: Pointer);
    WhenShow: procedure (Obj: Pointer);
    GetTileRect: procedure (var R: TRect; Obj: Pointer);
    end;

  TDNApplicationVMT = packed record
    NotForYou1: integer;
    NotForYou2: integer;
    NotForYou3: integer;
    Done: procedure (DP: integer; Obj: Pointer);
    Awaken: procedure (Obj: Pointer);
    CalcBounds: procedure (var Bounds: TRect; Delta: TPoint; Obj:
      Pointer);
    ChangeBounds: procedure (var Bounds: TRect; Obj: Pointer);
    DataSize: function (Obj: Pointer): word;
    Draw: procedure (Obj: Pointer);
    EndModal: procedure (Command: word; Obj: Pointer);
    Execute: function (Obj: Pointer): word;
    GetData: procedure (var Rec; Obj: Pointer);
    GetEvent: procedure (var Event: TEvent; Obj: Pointer);
    GetHelpCtx: function (Obj: Pointer): word;
    GetPalette: function (Obj: Pointer): PPalette;
    HandleEvent: procedure (var Event: TEvent; Obj: Pointer);
    PutEvent: procedure (var Event: TEvent; Obj: Pointer);
    SetData: procedure (var Rec; Obj: Pointer);
    SetState: procedure (AState: word; Enable: boolean; Obj: Pointer);
    SizeLimits: procedure (var Min, Max: TPoint; Obj: Pointer);
    Valid: function (Command: word; Obj: Pointer): boolean;
    UpDate: procedure (Obj: Pointer);
    ResetCursor: procedure (Obj: Pointer);
    EventError: procedure (var Event: TEvent; Obj: Pointer);
    Redraw: procedure (Obj: Pointer);
    Idle: procedure (Obj: Pointer);
    InitDesktop: procedure (Obj: Pointer);
    InitMenuBar: procedure (Obj: Pointer);
    InitScreen: procedure (Obj: Pointer);
    InitStatusLine: procedure (Obj: Pointer);
    InitCommandLine: procedure (Obj: Pointer);
    OutOfMemory: procedure (Obj: Pointer);
    Run: procedure (Obj: Pointer);
    WhenShow: procedure (Obj: Pointer);
    GetTileRect: procedure (var R: TRect; Obj: Pointer);
    end;

  TUniWindowVMT = packed record
    NotForYou1: integer;
    NotForYou2: integer;
    NotForYou3: integer;
    Done: procedure (DP: integer; Obj: Pointer);
    Awaken: procedure (Obj: Pointer);
    CalcBounds: procedure (var Bounds: TRect; Delta: TPoint; Obj:
      Pointer);
    ChangeBounds: procedure (var Bounds: TRect; Obj: Pointer);
    DataSize: function (Obj: Pointer): word;
    Draw: procedure (Obj: Pointer);
    EndModal: procedure (Command: word; Obj: Pointer);
    Execute: function (Obj: Pointer): word;
    GetData: procedure (var Rec; Obj: Pointer);
    GetEvent: procedure (var Event: TEvent; Obj: Pointer);
    GetHelpCtx: function (Obj: Pointer): word;
    GetPalette: function (Obj: Pointer): PPalette;
    HandleEvent: procedure (var Event: TEvent; Obj: Pointer);
    PutEvent: procedure (var Event: TEvent; Obj: Pointer);
    SetData: procedure (var Rec; Obj: Pointer);
    SetState: procedure (AState: word; Enable: boolean; Obj: Pointer);
    SizeLimits: procedure (var Min, Max: TPoint; Obj: Pointer);
    Valid: function (Command: word; Obj: Pointer): boolean;
    UpDate: procedure (Obj: Pointer);
    ResetCursor: procedure (Obj: Pointer);
    EventError: procedure (var Event: TEvent; Obj: Pointer);
    Redraw: procedure (Obj: Pointer);
    Close: procedure (Obj: Pointer);
    GetTitle: function (MaxSize: integer; Obj: Pointer): String;
    InitFrame: procedure (Obj: Pointer);
    Zoom: procedure (Obj: Pointer);
    Maxi: procedure (Obj: Pointer);
    ReactOnCmd: function (Obj: Pointer): boolean;
    end;

  TXFileEditorVMT = packed record
    NotForYou1: integer;
    NotForYou2: integer;
    NotForYou3: integer;
    Done: procedure (DP: integer; Obj: Pointer);
    Awaken: procedure (Obj: Pointer);
    CalcBounds: procedure (var Bounds: TRect; Delta: TPoint; Obj:
      Pointer);
    ChangeBounds: procedure (var Bounds: TRect; Obj: Pointer);
    DataSize: function (Obj: Pointer): word;
    Draw: procedure (Obj: Pointer);
    EndModal: procedure (Command: word; Obj: Pointer);
    Execute: function (Obj: Pointer): word;
    GetData: procedure (var Rec; Obj: Pointer);
    GetEvent: procedure (var Event: TEvent; Obj: Pointer);
    GetHelpCtx: function (Obj: Pointer): word;
    GetPalette: function (Obj: Pointer): PPalette;
    HandleEvent: procedure (var Event: TEvent; Obj: Pointer);
    PutEvent: procedure (var Event: TEvent; Obj: Pointer);
    SetData: procedure (var Rec; Obj: Pointer);
    SetState: procedure (AState: word; Enable: boolean; Obj: Pointer);
    SizeLimits: procedure (var Min, Max: TPoint; Obj: Pointer);
    Valid: function (Command: word; Obj: Pointer): boolean;
    UpDate: procedure (Obj: Pointer);
    ResetCursor: procedure (Obj: Pointer);
    HandleCommand: function (var Event: TEvent; Obj: Pointer):
      boolean;
    end;

  TEditWindowVMT = packed record
    NotForYou1: integer;
    NotForYou2: integer;
    NotForYou3: integer;
    Done: procedure (DP: integer; Obj: Pointer);
    Awaken: procedure (Obj: Pointer);
    CalcBounds: procedure (var Bounds: TRect; Delta: TPoint; Obj:
      Pointer);
    ChangeBounds: procedure (var Bounds: TRect; Obj: Pointer);
    DataSize: function (Obj: Pointer): word;
    Draw: procedure (Obj: Pointer);
    EndModal: procedure (Command: word; Obj: Pointer);
    Execute: function (Obj: Pointer): word;
    GetData: procedure (var Rec; Obj: Pointer);
    GetEvent: procedure (var Event: TEvent; Obj: Pointer);
    GetHelpCtx: function (Obj: Pointer): word;
    GetPalette: function (Obj: Pointer): PPalette;
    HandleEvent: procedure (var Event: TEvent; Obj: Pointer);
    PutEvent: procedure (var Event: TEvent; Obj: Pointer);
    SetData: procedure (var Rec; Obj: Pointer);
    SetState: procedure (AState: word; Enable: boolean; Obj: Pointer);
    SizeLimits: procedure (var Min, Max: TPoint; Obj: Pointer);
    Valid: function (Command: word; Obj: Pointer): boolean;
    UpDate: procedure (Obj: Pointer);
    ResetCursor: procedure (Obj: Pointer);
    EventError: procedure (var Event: TEvent; Obj: Pointer);
    Redraw: procedure (Obj: Pointer);
    Close: procedure (Obj: Pointer);
    GetTitle: function (MaxSize: integer; Obj: Pointer): String;
    InitFrame: procedure (Obj: Pointer);
    Zoom: procedure (Obj: Pointer);
    Maxi: procedure (Obj: Pointer);
    ReactOnCmd: function (Obj: Pointer): boolean;
    end;

  TPercentGaugeVMT = packed record
    NotForYou1: integer;
    NotForYou2: integer;
    NotForYou3: integer;
    Done: procedure (DP: integer; Obj: Pointer);
    Awaken: procedure (Obj: Pointer);
    CalcBounds: procedure (var Bounds: TRect; Delta: TPoint; Obj:
      Pointer);
    ChangeBounds: procedure (var Bounds: TRect; Obj: Pointer);
    DataSize: function (Obj: Pointer): word;
    Draw: procedure (Obj: Pointer);
    EndModal: procedure (Command: word; Obj: Pointer);
    Execute: function (Obj: Pointer): word;
    GetData: procedure (var Rec; Obj: Pointer);
    GetEvent: procedure (var Event: TEvent; Obj: Pointer);
    GetHelpCtx: function (Obj: Pointer): word;
    GetPalette: function (Obj: Pointer): PPalette;
    HandleEvent: procedure (var Event: TEvent; Obj: Pointer);
    PutEvent: procedure (var Event: TEvent; Obj: Pointer);
    SetData: procedure (var Rec; Obj: Pointer);
    SetState: procedure (AState: word; Enable: boolean; Obj: Pointer);
    SizeLimits: procedure (var Min, Max: TPoint; Obj: Pointer);
    Valid: function (Command: word; Obj: Pointer): boolean;
    UpDate: procedure (Obj: Pointer);
    ResetCursor: procedure (Obj: Pointer);
    UpdateView: procedure (Progress: longInt; Obj: Pointer);
    end;

  TBarGaugeVMT = packed record
    NotForYou1: integer;
    NotForYou2: integer;
    NotForYou3: integer;
    Done: procedure (DP: integer; Obj: Pointer);
    Awaken: procedure (Obj: Pointer);
    CalcBounds: procedure (var Bounds: TRect; Delta: TPoint; Obj:
      Pointer);
    ChangeBounds: procedure (var Bounds: TRect; Obj: Pointer);
    DataSize: function (Obj: Pointer): word;
    Draw: procedure (Obj: Pointer);
    EndModal: procedure (Command: word; Obj: Pointer);
    Execute: function (Obj: Pointer): word;
    GetData: procedure (var Rec; Obj: Pointer);
    GetEvent: procedure (var Event: TEvent; Obj: Pointer);
    GetHelpCtx: function (Obj: Pointer): word;
    GetPalette: function (Obj: Pointer): PPalette;
    HandleEvent: procedure (var Event: TEvent; Obj: Pointer);
    PutEvent: procedure (var Event: TEvent; Obj: Pointer);
    SetData: procedure (var Rec; Obj: Pointer);
    SetState: procedure (AState: word; Enable: boolean; Obj: Pointer);
    SizeLimits: procedure (var Min, Max: TPoint; Obj: Pointer);
    Valid: function (Command: word; Obj: Pointer): boolean;
    UpDate: procedure (Obj: Pointer);
    ResetCursor: procedure (Obj: Pointer);
    UpdateView: procedure (Progress: longInt; Obj: Pointer);
    end;

  TWhileViewVMT = packed record
    NotForYou1: integer;
    NotForYou2: integer;
    NotForYou3: integer;
    Done: procedure (DP: integer; Obj: Pointer);
    Awaken: procedure (Obj: Pointer);
    CalcBounds: procedure (var Bounds: TRect; Delta: TPoint; Obj:
      Pointer);
    ChangeBounds: procedure (var Bounds: TRect; Obj: Pointer);
    DataSize: function (Obj: Pointer): word;
    Draw: procedure (Obj: Pointer);
    EndModal: procedure (Command: word; Obj: Pointer);
    Execute: function (Obj: Pointer): word;
    GetData: procedure (var Rec; Obj: Pointer);
    GetEvent: procedure (var Event: TEvent; Obj: Pointer);
    GetHelpCtx: function (Obj: Pointer): word;
    GetPalette: function (Obj: Pointer): PPalette;
    HandleEvent: procedure (var Event: TEvent; Obj: Pointer);
    PutEvent: procedure (var Event: TEvent; Obj: Pointer);
    SetData: procedure (var Rec; Obj: Pointer);
    SetState: procedure (AState: word; Enable: boolean; Obj: Pointer);
    SizeLimits: procedure (var Min, Max: TPoint; Obj: Pointer);
    Valid: function (Command: word; Obj: Pointer): boolean;
    UpDate: procedure (Obj: Pointer);
    ResetCursor: procedure (Obj: Pointer);
    EventError: procedure (var Event: TEvent; Obj: Pointer);
    Redraw: procedure (Obj: Pointer);
    end;

  TViewScrollVMT = packed record
    NotForYou1: integer;
    NotForYou2: integer;
    NotForYou3: integer;
    Done: procedure (DP: integer; Obj: Pointer);
    Awaken: procedure (Obj: Pointer);
    CalcBounds: procedure (var Bounds: TRect; Delta: TPoint; Obj:
      Pointer);
    ChangeBounds: procedure (var Bounds: TRect; Obj: Pointer);
    DataSize: function (Obj: Pointer): word;
    Draw: procedure (Obj: Pointer);
    EndModal: procedure (Command: word; Obj: Pointer);
    Execute: function (Obj: Pointer): word;
    GetData: procedure (var Rec; Obj: Pointer);
    GetEvent: procedure (var Event: TEvent; Obj: Pointer);
    GetHelpCtx: function (Obj: Pointer): word;
    GetPalette: function (Obj: Pointer): PPalette;
    HandleEvent: procedure (var Event: TEvent; Obj: Pointer);
    PutEvent: procedure (var Event: TEvent; Obj: Pointer);
    SetData: procedure (var Rec; Obj: Pointer);
    SetState: procedure (AState: word; Enable: boolean; Obj: Pointer);
    SizeLimits: procedure (var Min, Max: TPoint; Obj: Pointer);
    Valid: function (Command: word; Obj: Pointer): boolean;
    UpDate: procedure (Obj: Pointer);
    ResetCursor: procedure (Obj: Pointer);
    end;

  TFileViewerVMT = packed record
    NotForYou1: integer;
    NotForYou2: integer;
    NotForYou3: integer;
    Done: procedure (DP: integer; Obj: Pointer);
    Awaken: procedure (Obj: Pointer);
    CalcBounds: procedure (var Bounds: TRect; Delta: TPoint; Obj:
      Pointer);
    ChangeBounds: procedure (var Bounds: TRect; Obj: Pointer);
    DataSize: function (Obj: Pointer): word;
    Draw: procedure (Obj: Pointer);
    EndModal: procedure (Command: word; Obj: Pointer);
    Execute: function (Obj: Pointer): word;
    GetData: procedure (var Rec; Obj: Pointer);
    GetEvent: procedure (var Event: TEvent; Obj: Pointer);
    GetHelpCtx: function (Obj: Pointer): word;
    GetPalette: function (Obj: Pointer): PPalette;
    HandleEvent: procedure (var Event: TEvent; Obj: Pointer);
    PutEvent: procedure (var Event: TEvent; Obj: Pointer);
    SetData: procedure (var Rec; Obj: Pointer);
    SetState: procedure (AState: word; Enable: boolean; Obj: Pointer);
    SizeLimits: procedure (var Min, Max: TPoint; Obj: Pointer);
    Valid: function (Command: word; Obj: Pointer): boolean;
    UpDate: procedure (Obj: Pointer);
    ResetCursor: procedure (Obj: Pointer);
    HideView: procedure (Obj: Pointer);
    ShowView: procedure (Obj: Pointer);
    CountDown: procedure (Obj: Pointer; ANumber: integer);
    CountUp: procedure (Obj: Pointer; ANumber: integer);
    MakeLines: procedure (Obj: Pointer);
    end;

  TDriveVMT = packed record
    NotForYou1: integer;
    NotForYou2: integer;
    NotForYou3: integer;
    Done: procedure (DP: integer; Obj: Pointer);
    Store: procedure (var s: TStream; Obj: Pointer);
    KillUse: procedure (Obj: Pointer);
    lChDir: procedure (ADir: String; Obj: Pointer);
    GetDir: function (Obj: Pointer): String;
    GetDirectory: function (SortMode, PanelFlags: integer; const
      FileMask: String;
    var TotalInfo: TSize; var FreeSpace: String; Obj: Pointer):
      PCollection;
    CopyFiles: procedure (Files: PCollection; Own: PView; MoveMode:
      boolean; Obj: Pointer);
    CopyFilesInto: procedure (Files: PCollection; Own: PView;
      MoveMode: boolean; Obj: Pointer);
    EraseFiles: procedure (Files: PCollection; Obj: Pointer);
    UseFile: procedure (P: PFileRec; Command: word; Obj: Pointer);
    GetFreeSpace: procedure (var s: String; Obj: Pointer);
    Disposable: function (Obj: Pointer): boolean;
    GetRealName: function (Obj: Pointer): String;
    GetInternalName: function (Obj: Pointer): String;
    GetFull: procedure (var B; P: PFileRec; C, Sc: word; Obj:
      Pointer);
    GetEmpty: procedure (var B; Sc: word; Obj: Pointer);
    CalcLengthWithoutName: function (Obj: Pointer): integer;
    CalcLength: function (Obj: Pointer): integer;
    RereadDirectory: procedure (s: String; Obj: Pointer);
    MakeTop: procedure (var s: String; Obj: Pointer);
    GetDown: procedure (var B; C: word; P: PFileRec; Obj: Pointer);
    HandleCommand: procedure (Command: word; InfoPtr: Pointer; Obj:
      Pointer);
    GetDirInfo: procedure (var B: TDiskInfoRec; Obj: Pointer);
    GetRealDir: function (Obj: Pointer): String;
    MakeDir: procedure (Obj: Pointer);
    isUp: function (Obj: Pointer): boolean;
    ChangeUp: procedure (var s: String; Obj: Pointer);
    ChangeRoot: procedure (Obj: Pointer);
    GetFullFlags: function (Obj: Pointer): word;
    EditDescription: procedure (PF: PFileRec; Obj: Pointer);
    GetDirLength: procedure (PF: PFileRec; Obj: Pointer);
    GetParam: procedure (n: byte; Obj: Pointer);
    end;

  TFindDriveVMT = packed record
    NotForYou1: integer;
    NotForYou2: integer;
    NotForYou3: integer;
    Done: procedure (DP: integer; Obj: Pointer);
    Store: procedure (var s: TStream; Obj: Pointer);
    KillUse: procedure (Obj: Pointer);
    lChDir: procedure (ADir: String; Obj: Pointer);
    GetDir: function (Obj: Pointer): String;
    GetDirectory: function (SortMode, PanelFlags: integer; const
      FileMask: String;
    var TotalInfo: TSize; var FreeSpace: String; Obj: Pointer):
      PCollection;
    CopyFiles: procedure (Files: PCollection; Own: PView; MoveMode:
      boolean; Obj: Pointer);
    CopyFilesInto: procedure (Files: PCollection; Own: PView;
      MoveMode: boolean; Obj: Pointer);
    EraseFiles: procedure (Files: PCollection; Obj: Pointer);
    UseFile: procedure (P: PFileRec; Command: word; Obj: Pointer);
    GetFreeSpace: procedure (var s: String; Obj: Pointer);
    Disposable: function (Obj: Pointer): boolean;
    GetRealName: function (Obj: Pointer): String;
    GetInternalName: function (Obj: Pointer): String;
    GetFull: procedure (var B; P: PFileRec; C, Sc: word; Obj:
      Pointer);
    GetEmpty: procedure (var B; Sc: word; Obj: Pointer);
    CalcLengthWithoutName: function (Obj: Pointer): integer;
    CalcLength: function (Obj: Pointer): integer;
    RereadDirectory: procedure (s: String; Obj: Pointer);
    MakeTop: procedure (var s: String; Obj: Pointer);
    GetDown: procedure (var B; C: word; P: PFileRec; Obj: Pointer);
    HandleCommand: procedure (Command: word; InfoPtr: Pointer; Obj:
      Pointer);
    GetDirInfo: procedure (var B: TDiskInfoRec; Obj: Pointer);
    GetRealDir: function (Obj: Pointer): String;
    MakeDir: procedure (Obj: Pointer);
    isUp: function (Obj: Pointer): boolean;
    ChangeUp: procedure (var s: String; Obj: Pointer);
    ChangeRoot: procedure (Obj: Pointer);
    GetFullFlags: function (Obj: Pointer): word;
    EditDescription: procedure (PF: PFileRec; Obj: Pointer);
    GetDirLength: procedure (PF: PFileRec; Obj: Pointer);
    GetParam: procedure (n: byte; Obj: Pointer);
    end;

  TTempDriveVMT = packed record
    NotForYou1: integer;
    NotForYou2: integer;
    NotForYou3: integer;
    Done: procedure (DP: integer; Obj: Pointer);
    Store: procedure (var s: TStream; Obj: Pointer);
    KillUse: procedure (Obj: Pointer);
    lChDir: procedure (ADir: String; Obj: Pointer);
    GetDir: function (Obj: Pointer): String;
    GetDirectory: function (SortMode, PanelFlags: integer; const
      FileMask: String;
    var TotalInfo: TSize; var FreeSpace: String; Obj: Pointer):
      PCollection;
    CopyFiles: procedure (Files: PCollection; Own: PView; MoveMode:
      boolean; Obj: Pointer);
    CopyFilesInto: procedure (Files: PCollection; Own: PView;
      MoveMode: boolean; Obj: Pointer);
    EraseFiles: procedure (Files: PCollection; Obj: Pointer);
    UseFile: procedure (P: PFileRec; Command: word; Obj: Pointer);
    GetFreeSpace: procedure (var s: String; Obj: Pointer);
    Disposable: function (Obj: Pointer): boolean;
    GetRealName: function (Obj: Pointer): String;
    GetInternalName: function (Obj: Pointer): String;
    GetFull: procedure (var B; P: PFileRec; C, Sc: word; Obj:
      Pointer);
    GetEmpty: procedure (var B; Sc: word; Obj: Pointer);
    CalcLengthWithoutName: function (Obj: Pointer): integer;
    CalcLength: function (Obj: Pointer): integer;
    RereadDirectory: procedure (s: String; Obj: Pointer);
    MakeTop: procedure (var s: String; Obj: Pointer);
    GetDown: procedure (var B; C: word; P: PFileRec; Obj: Pointer);
    HandleCommand: procedure (Command: word; InfoPtr: Pointer; Obj:
      Pointer);
    GetDirInfo: procedure (var B: TDiskInfoRec; Obj: Pointer);
    GetRealDir: function (Obj: Pointer): String;
    MakeDir: procedure (Obj: Pointer);
    isUp: function (Obj: Pointer): boolean;
    ChangeUp: procedure (var s: String; Obj: Pointer);
    ChangeRoot: procedure (Obj: Pointer);
    GetFullFlags: function (Obj: Pointer): word;
    EditDescription: procedure (PF: PFileRec; Obj: Pointer);
    GetDirLength: procedure (PF: PFileRec; Obj: Pointer);
    GetParam: procedure (n: byte; Obj: Pointer);
    end;

  TArcDriveVMT = packed record
    NotForYou1: integer;
    NotForYou2: integer;
    NotForYou3: integer;
    Done: procedure (DP: integer; Obj: Pointer);
    Store: procedure (var s: TStream; Obj: Pointer);
    KillUse: procedure (Obj: Pointer);
    lChDir: procedure (ADir: String; Obj: Pointer);
    GetDir: function (Obj: Pointer): String;
    GetDirectory: function (SortMode, PanelFlags: integer; const
      FileMask: String;
    var TotalInfo: TSize; var FreeSpace: String; Obj: Pointer):
      PCollection;
    CopyFiles: procedure (Files: PCollection; Own: PView; MoveMode:
      boolean; Obj: Pointer);
    CopyFilesInto: procedure (Files: PCollection; Own: PView;
      MoveMode: boolean; Obj: Pointer);
    EraseFiles: procedure (Files: PCollection; Obj: Pointer);
    UseFile: procedure (P: PFileRec; Command: word; Obj: Pointer);
    GetFreeSpace: procedure (var s: String; Obj: Pointer);
    Disposable: function (Obj: Pointer): boolean;
    GetRealName: function (Obj: Pointer): String;
    GetInternalName: function (Obj: Pointer): String;
    GetFull: procedure (var B; P: PFileRec; C, Sc: word; Obj:
      Pointer);
    GetEmpty: procedure (var B; Sc: word; Obj: Pointer);
    CalcLengthWithoutName: function (Obj: Pointer): integer;
    CalcLength: function (Obj: Pointer): integer;
    RereadDirectory: procedure (s: String; Obj: Pointer);
    MakeTop: procedure (var s: String; Obj: Pointer);
    GetDown: procedure (var B; C: word; P: PFileRec; Obj: Pointer);
    HandleCommand: procedure (Command: word; InfoPtr: Pointer; Obj:
      Pointer);
    GetDirInfo: procedure (var B: TDiskInfoRec; Obj: Pointer);
    GetRealDir: function (Obj: Pointer): String;
    MakeDir: procedure (Obj: Pointer);
    isUp: function (Obj: Pointer): boolean;
    ChangeUp: procedure (var s: String; Obj: Pointer);
    ChangeRoot: procedure (Obj: Pointer);
    GetFullFlags: function (Obj: Pointer): word;
    EditDescription: procedure (PF: PFileRec; Obj: Pointer);
    GetDirLength: procedure (PF: PFileRec; Obj: Pointer);
    GetParam: procedure (n: byte; Obj: Pointer);
    end;

  TArvidDriveVMT = packed record
    NotForYou1: integer;
    NotForYou2: integer;
    NotForYou3: integer;
    Done: procedure (DP: integer; Obj: Pointer);
    Store: procedure (var s: TStream; Obj: Pointer);
    KillUse: procedure (Obj: Pointer);
    lChDir: procedure (ADir: String; Obj: Pointer);
    GetDir: function (Obj: Pointer): String;
    GetDirectory: function (SortMode, PanelFlags: integer; const
      FileMask: String;
    var TotalInfo: TSize; var FreeSpace: String; Obj: Pointer):
      PCollection;
    CopyFiles: procedure (Files: PCollection; Own: PView; MoveMode:
      boolean; Obj: Pointer);
    CopyFilesInto: procedure (Files: PCollection; Own: PView;
      MoveMode: boolean; Obj: Pointer);
    EraseFiles: procedure (Files: PCollection; Obj: Pointer);
    UseFile: procedure (P: PFileRec; Command: word; Obj: Pointer);
    GetFreeSpace: procedure (var s: String; Obj: Pointer);
    Disposable: function (Obj: Pointer): boolean;
    GetRealName: function (Obj: Pointer): String;
    GetInternalName: function (Obj: Pointer): String;
    GetFull: procedure (var B; P: PFileRec; C, Sc: word; Obj:
      Pointer);
    GetEmpty: procedure (var B; Sc: word; Obj: Pointer);
    CalcLengthWithoutName: function (Obj: Pointer): integer;
    CalcLength: function (Obj: Pointer): integer;
    RereadDirectory: procedure (s: String; Obj: Pointer);
    MakeTop: procedure (var s: String; Obj: Pointer);
    GetDown: procedure (var B; C: word; P: PFileRec; Obj: Pointer);
    HandleCommand: procedure (Command: word; InfoPtr: Pointer; Obj:
      Pointer);
    GetDirInfo: procedure (var B: TDiskInfoRec; Obj: Pointer);
    GetRealDir: function (Obj: Pointer): String;
    MakeDir: procedure (Obj: Pointer);
    isUp: function (Obj: Pointer): boolean;
    ChangeUp: procedure (var s: String; Obj: Pointer);
    ChangeRoot: procedure (Obj: Pointer);
    GetFullFlags: function (Obj: Pointer): word;
    EditDescription: procedure (PF: PFileRec; Obj: Pointer);
    GetDirLength: procedure (PF: PFileRec; Obj: Pointer);
    GetParam: procedure (n: byte; Obj: Pointer);
    end;

const
  TEmptyObject_VMTSize = SizeOf(TEmptyObjectVMT) div 4-3;
  TObject_VMTSize = SizeOf(TObjectVMT) div 4-3;
  TRegExp_VMTSize = SizeOf(TRegExpVMT) div 4-3;
  TStream_VMTSize = SizeOf(TStreamVMT) div 4-3;
  TDosStream_VMTSize = SizeOf(TDosStreamVMT) div 4-3;
  TBufStream_VMTSize = SizeOf(TBufStreamVMT) div 4-3;
  TMemoryStream_VMTSize = SizeOf(TMemoryStreamVMT) div 4-3;
  TCollection_VMTSize = SizeOf(TCollectionVMT) div 4-3;
  TSortedCollection_VMTSize = SizeOf(TSortedCollectionVMT) div 4-3;
  TLineCollection_VMTSize = SizeOf(TLineCollectionVMT) div 4-3;
  TStringCollection_VMTSize = SizeOf(TStringCollectionVMT) div 4-3;
  TStrCollection_VMTSize = SizeOf(TStrCollectionVMT) div 4-3;
  TFilesCollection_VMTSize = SizeOf(TFilesCollectionVMT) div 4-3;
  TView_VMTSize = SizeOf(TViewVMT) div 4-3;
  TFrame_VMTSize = SizeOf(TFrameVMT) div 4-3;
  TScrollBar_VMTSize = SizeOf(TScrollBarVMT) div 4-3;
  TGroup_VMTSize = SizeOf(TGroupVMT) div 4-3;
  TWindow_VMTSize = SizeOf(TWindowVMT) div 4-3;
  TMenuView_VMTSize = SizeOf(TMenuViewVMT) div 4-3;
  TMenuBar_VMTSize = SizeOf(TMenuBarVMT) div 4-3;
  TMenuBox_VMTSize = SizeOf(TMenuBoxVMT) div 4-3;
  TMenuPopup_VMTSize = SizeOf(TMenuPopupVMT) div 4-3;
  TStatusLine_VMTSize = SizeOf(TStatusLineVMT) div 4-3;
  TDialog_VMTSize = SizeOf(TDialogVMT) div 4-3;
  TInputLine_VMTSize = SizeOf(TInputLineVMT) div 4-3;
  TButton_VMTSize = SizeOf(TButtonVMT) div 4-3;
  TCluster_VMTSize = SizeOf(TClusterVMT) div 4-3;
  TRadioButtons_VMTSize = SizeOf(TRadioButtonsVMT) div 4-3;
  TCheckBoxes_VMTSize = SizeOf(TCheckBoxesVMT) div 4-3;
  TMultiCheckBoxes_VMTSize = SizeOf(TMultiCheckBoxesVMT) div 4-3;
  TScroller_VMTSize = SizeOf(TScrollerVMT) div 4-3;
  TListViewer_VMTSize = SizeOf(TListViewerVMT) div 4-3;
  TListBox_VMTSize = SizeOf(TListBoxVMT) div 4-3;
  TStaticText_VMTSize = SizeOf(TStaticTextVMT) div 4-3;
  TParamText_VMTSize = SizeOf(TParamTextVMT) div 4-3;
  TLabel_VMTSize = SizeOf(TLabelVMT) div 4-3;
  THistoryViewer_VMTSize = SizeOf(THistoryViewerVMT) div 4-3;
  THistoryWindow_VMTSize = SizeOf(THistoryWindowVMT) div 4-3;
  THistory_VMTSize = SizeOf(THistoryVMT) div 4-3;
  TBackground_VMTSize = SizeOf(TBackgroundVMT) div 4-3;
  TDesktop_VMTSize = SizeOf(TDesktopVMT) div 4-3;
  TProgram_VMTSize = SizeOf(TProgramVMT) div 4-3;
  TApplication_VMTSize = SizeOf(TApplicationVMT) div 4-3;
  TDNApplication_VMTSize = SizeOf(TDNApplicationVMT) div 4-3;
  TUniWindow_VMTSize = SizeOf(TUniWindowVMT) div 4-3;
  TEditWindow_VMTSize = SizeOf(TEditWindowVMT) div 4-3;
  TXFileEditor_VMTSize = SizeOf(TXFileEditorVMT) div 4-3;
  TPercentGauge_VMTSize = SizeOf(TPercentGaugeVMT) div 4-3;
  TBarGauge_VMTSize = SizeOf(TBarGaugeVMT) div 4-3;
  TWhileView_VMTSize = SizeOf(TWhileViewVMT) div 4-3;
  TViewScroll_VMTSize = SizeOf(TViewScrollVMT) div 4-3;
  TFileViewer_VMTSize = SizeOf(TFileViewerVMT) div 4-3;
  TDrive_VMTSize = SizeOf(TDriveVMT) div 4-3;
  TFindDrive_VMTSize = SizeOf(TFindDriveVMT) div 4-3;
  TTempDrive_VMTSize = SizeOf(TTempDriveVMT) div 4-3;
  TArcDrive_VMTSize = SizeOf(TArcDriveVMT) div 4-3;
  TArvidDrive_VMTSize = SizeOf(TArvidDriveVMT) div 4-3;

implementation

end.
