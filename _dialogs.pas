unit _Dialogs;
(******

DN/2 Plugin Interface - object model
Copyright (C) 2002 Aleksej Kozlov (Cat)
2:5030/1326.13

******)

{&Delphi+}
{&Use32+}

interface

uses
  _Defines, _Streams, _Collect, _Views;

type
  PDialog = ^TDialog;
  TDialog = object(TWindow)
    Constructor Init(var Bounds: TRect; const ATitle: String);
    Constructor Load(var s: TStream);
    {function GetPalette: PPalette; virtual;}
    {procedure HandleEvent(var Event: TEvent); virtual;}
    {function Valid(Command: Word): Boolean; virtual;}
    end;

  PInputline = ^TInputLine;
  TInputLine = object(TView)
    Data: PString;
    MaxLen: AInt;
    CurPos: AInt;
    FirstPos: AInt;
    SelStart: AInt;
    SelEnd: AInt;
    Validator: Pointer {PValidator};
    DrawShift: byte;
    CtrlK: boolean;
    Constructor Init(var Bounds: TRect; AMaxLen: AInt);
    Constructor Load(var s: TStream);
    {destructor Done; virtual;}
    {function DataSize: Word; virtual;}
    {procedure Draw; virtual;}
    {procedure GetData(var Rec); virtual;}
    {function GetPalette: PPalette; virtual;}
    {procedure HandleEvent(var Event: TEvent); virtual;}
    procedure SelectAll(Enable: boolean);
    {procedure SetData(var Rec); virtual;}
    {procedure SetState(AState: Word; Enable: Boolean); virtual;}
    procedure SetValidator(AValid: Pointer {PValidator});
    procedure Store(var s: TStream);
    {function Valid(Command: Word): Boolean; virtual;}
    function CanScroll(Delta: integer): boolean;
    end;

  PButton = ^TButton;
  TButton = object(TView)
    Title: PString;
    Command: AWord;
    Flags: byte;
    AmDefault: boolean;
    Constructor Init(var Bounds: TRect; const ATitle: String;
      ACommand: word; AFlags: word);
    Constructor Load(var s: TStream);
    {destructor Done; virtual;}
    {procedure Draw; virtual;}
    procedure DrawState(Down: boolean);
    {function GetPalette: PPalette; virtual;}
    {procedure HandleEvent(var Event: TEvent); virtual;}
    procedure MakeDefault(Enable: boolean);
    procedure Press; virtual;
    {procedure SetState(AState: Word; Enable: Boolean); virtual;}
    procedure Store(var s: TStream);
    end;

  PCluster = ^TCluster;
  TCluster = object(TView)
    Value: longInt;
    Sel: AInt;
    EnableMask: longInt;
    Strings: TStringCollection;
    Constructor Init(var Bounds: TRect; AStrings: PSItem);
    Constructor Load(var s: TStream);
    {destructor Done; virtual;}
    function ButtonState(Item: integer): boolean;
    {function DataSize: Word; virtual;}
    procedure DrawBox(const Icon: String; Marker: Char);
    procedure DrawMultiBox(const Icon, Marker: String);
    {procedure GetData(var Rec); virtual;}
    {function GetHelpCtx: Word; virtual;}
    {function GetPalette: PPalette; virtual;}
    {procedure HandleEvent(var Event: TEvent); virtual;}
    function Mark(Item: integer): boolean; virtual;
    function MultiMark(Item: integer): byte; virtual;
    procedure Press(Item: integer); virtual;
    procedure MovedTo(Item: integer); virtual;
    procedure SetButtonState(AMask: longInt; Enable: boolean);
    {procedure SetData(var Rec); virtual;}
    {procedure SetState(AState: Word; Enable: Boolean); virtual;}
    procedure Store(var s: TStream);
    end;

  PRadioButtons = ^TRadioButtons;
  TRadioButtons = object(TCluster)
    {procedure Draw; virtual;}
    {function Mark(Item: Integer): Boolean; virtual;}
    {procedure MovedTo(Item: Integer); virtual;}
    {procedure Press(Item: Integer); virtual;}
    {procedure SetData(var Rec); virtual;}
    end;

  PCheckBoxes = ^TCheckBoxes;
  TCheckBoxes = object(TCluster)
    {procedure Draw; virtual;}
    {function Mark(Item: Integer): Boolean; virtual;}
    {procedure Press(Item: Integer); virtual;}
    end;

  PMultiCheckBoxes = ^TMultiCheckBoxes;
  TMultiCheckBoxes = object(TCluster)
    SelRange: byte;
    Flags: AWord;
    States: PString;
    Constructor Init(var Bounds: TRect; AStrings: PSItem; ASelRange:
      byte; AFlags: word; const AStates: String);
    Constructor Load(var s: TStream);
    {destructor Done; virtual;}
    {function DataSize: Word; virtual;}
    {procedure Draw; virtual;}
    {procedure GetData(var Rec); virtual;}
    {function MultiMark(Item: Integer): Byte; virtual;}
    {procedure Press(Item: Integer); virtual;}
    {procedure SetData(var Rec); virtual;}
    procedure Store(var s: TStream);
    end;

  PScroller = ^TScroller;
  TScroller = object(TView)
    HScrollBar: PScrollBar;
    VScrollBar: PScrollBar;
    Delta: TPoint;
    Limit: TPoint;
    DrawLock: byte;
    DrawFlag: boolean;
    Constructor Init(var Bounds: TRect; AHScrollBar, AVScrollBar:
      PScrollBar);
    Constructor Load(var s: TStream);
    {procedure ChangeBounds(var Bounds: TRect); virtual;}
    {function GetPalette: PPalette; virtual;}
    {procedure HandleEvent(var Event: TEvent); virtual;}
    procedure ScrollDraw; virtual;
    procedure ScrollTo(X, Y: longInt);
    procedure SetLimit(X, Y: longInt);
    {procedure SetState(AState: Word; Enable: Boolean); virtual;}
    procedure Store(var s: TStream);
    end;

  PListViewer = ^TListViewer;
  TListViewer = object(TView)
    HScrollBar: PScrollBar;
    VScrollBar: PScrollBar;
    NumCols: longInt;
    TopItem: longInt;
    Focused: longInt;
    Range: longInt;
    Constructor Init(var Bounds: TRect; ANumCols: longInt;
      AHScrollBar, AVScrollBar: PScrollBar);
    Constructor Load(var s: TStream);
    {procedure ChangeBounds(var Bounds: TRect); virtual;}
    {procedure Draw; virtual;}
    procedure FocusItem(Item: longInt); virtual;
    {function GetPalette: PPalette; virtual;}
    function GetText(Item: longInt; MaxLen: integer): String;
      virtual;
    function IsSelected(Item: longInt): boolean; virtual;
    {procedure HandleEvent(var Event: TEvent); virtual;}
    procedure SelectItem(Item: longInt); virtual;
    procedure SetRange(ARange: longInt);
    {procedure SetState(AState: Word; Enable: Boolean); virtual;}
    procedure Store(var s: TStream);
    procedure FocusItemNum(Item: longInt); virtual;
    end;

  PListBox = ^TListBox;
  TListBox = object(TListViewer)
    List: PCollection;
    Constructor Init(var Bounds: TRect; ANumCols: word; AScrollBar:
      PScrollBar);
    Constructor Load(var s: TStream);
    {function DataSize: Word; virtual;}
    {procedure GetData(var Rec); virtual;}
    {function GetText(Item: LongInt; MaxLen: Integer): String; virtual;}
    procedure NewLisT(AList: PCollection); virtual;
    {procedure SetData(var Rec); virtual;}
    procedure Store(var s: TStream);
    end;

  PStaticText = ^TStaticText;
  TStaticText = object(TView)
    text: PString;
    Constructor Init(var Bounds: TRect; const AText: String);
    Constructor Load(var s: TStream);
    {destructor Done; virtual;}
    {procedure Draw; virtual;}
    {function GetPalette: PPalette; virtual;}
    procedure GetText(var s: String); virtual;
    procedure Store(var s: TStream);
    end;

  PParamText = ^TParamText;
  TParamText = object(TStaticText)
    ParamCount: AInt;
    ParamList: Pointer;
    Constructor Init(var Bounds: TRect; const AText: String;
      AParamCount: AInt);
    Constructor Load(var s: TStream);
    {function DataSize: Word; virtual;}
    {procedure GetText(var S: String); virtual;}
    {procedure SetData(var Rec); virtual;}
    procedure Store(var s: TStream);
    end;

  PLabel = ^TLabel;
  TLabel = object(TStaticText)
    Link: PView;
    Light: boolean;
    Constructor Init(var Bounds: TRect; const AText: String; ALink:
      PView);
    Constructor Load(var s: TStream);
    {procedure Draw; virtual;}
    {function GetPalette: PPalette; virtual;}
    {procedure HandleEvent(var Event: TEvent); virtual;}
    procedure Store(var s: TStream);
    end;

  PHistoryViewer = ^THistoryViewer;
  THistoryViewer = object(TListViewer)
    HistoryId: AWord;
    SearchPos: AWord;
    Constructor Init(var Bounds: TRect; AHScrollBar, AVScrollBar:
      PScrollBar; AHistoryId: AWord);
    {function GetPalette: PPalette; virtual;}
    {function GetText(Item: LongInt; MaxLen: Integer): String; virtual;}
    {procedure HandleEvent(var Event: TEvent); virtual;}
    function HistoryWidth: integer;
    end;

  PHistoryWindow = ^THistoryWindow;
  THistoryWindow = object(TWindow)
    Viewer: PListViewer;
    Constructor Init(var Bounds: TRect; HistoryId: AWord);
    {function GetPalette: PPalette; virtual;}
    function GetSelection: String; virtual;
    procedure InitViewer(HistoryId: AWord); virtual;
    end;

  PHistory = ^THistory;
  THistory = object(TView)
    Link: PInputline;
    HistoryId: AWord;
    Constructor Init(var Bounds: TRect; ALink: PInputline;
      AHistoryId: AWord);
    Constructor Load(var s: TStream);
    {procedure Draw; virtual;}
    {function GetPalette: PPalette; virtual;}
    {procedure HandleEvent(var Event: TEvent); virtual;}
    function InitHistoryWindow(var Bounds: TRect): PHistoryWindow;
      virtual;
    procedure RecordHistory(const s: String); virtual;
    procedure Store(var s: TStream);
    end;

implementation

uses
  _DNFuncs;

Constructor TDialog.Init(var Bounds: TRect; const ATitle: String);
  begin
    _TDialog^.Init(Bounds, ATitle, nil, @Self);
  end;

Constructor TDialog.Load(var s: TStream);
  begin
    _TDialog^.Load(_Model1.TStream(s), nil, @Self);
  end;

Constructor TInputLine.Init(var Bounds: TRect; AMaxLen: AInt);
  begin
    _TInputLine^.Init(Bounds, AMaxLen, nil, @Self);
  end;

Constructor TInputLine.Load(var s: TStream);
  begin
    _TInputLine^.Load(_Model1.TStream(s), nil, @Self);
  end;

procedure TInputLine.SelectAll(Enable: boolean);
  begin
    _TInputLine^.SelectAll(Enable, @Self);
  end;

procedure TInputLine.SetValidator(AValid: Pointer {PValidator});
  begin
    _TInputLine^.SetValidator(AValid, @Self);
  end;

procedure TInputLine.Store(var s: TStream);
  begin
    _TInputLine^.Store(_Model1.TStream(s), @Self);
  end;

function TInputLine.CanScroll(Delta: integer): boolean;
  begin
    Result := _TInputLine^.CanScroll(Delta, @Self);
  end;

Constructor TButton.Init(var Bounds: TRect; const ATitle: String;
    ACommand: word; AFlags: word);
  begin
    _TButton^.Init(Bounds, ATitle, ACommand, AFlags, nil, @Self);
  end;

Constructor TButton.Load(var s: TStream);
  begin
    _TButton^.Load(_Model1.TStream(s), nil, @Self);
  end;

procedure TButton.DrawState(Down: boolean);
  begin
    _TButton^.DrawState(Down, @Self);
  end;

procedure TButton.MakeDefault(Enable: boolean);
  begin
    _TButton^.MakeDefault(Enable, @Self);
  end;

procedure TButton.Press;
  assembler; {&Frame-}
asm
end;

procedure TButton.Store(var s: TStream);
  begin
    _TButton^.Store(_Model1.TStream(s), @Self);
  end;

Constructor TCluster.Init(var Bounds: TRect; AStrings: PSItem);
  begin
    _TCluster^.Init(Bounds, AStrings, nil, @Self);
  end;

Constructor TCluster.Load(var s: TStream);
  begin
    _TCluster^.Load(_Model1.TStream(s), nil, @Self);
  end;

function TCluster.ButtonState(Item: integer): boolean;
  begin
    Result := _TCluster^.ButtonState(Item, @Self);
  end;

procedure TCluster.DrawBox(const Icon: String; Marker: Char);
  begin
    _TCluster^.DrawBox(Icon, Marker, @Self);
  end;

procedure TCluster.DrawMultiBox(const Icon, Marker: String);
  begin
    _TCluster^.DrawMultiBox(Icon, Marker, @Self);
  end;

function TCluster.Mark(Item: integer): boolean;
  assembler; {&Frame-}
asm
end;

function TCluster.MultiMark(Item: integer): byte;
  assembler; {&Frame-}
asm
end;

procedure TCluster.Press(Item: integer);
  assembler; {&Frame-}
asm
end;

procedure TCluster.MovedTo(Item: integer);
  assembler; {&Frame-}
asm
end;

procedure TCluster.SetButtonState(AMask: longInt; Enable: boolean);
  begin
    _TCluster^.SetButtonState(AMask, Enable, @Self);
  end;

procedure TCluster.Store(var s: TStream);
  begin
    _TCluster^.Store(_Model1.TStream(s), @Self);
  end;

Constructor TMultiCheckBoxes.Init(var Bounds: TRect; AStrings:
    PSItem; ASelRange: byte; AFlags: word; const AStates: String);
  begin
    _TMultiCheckBoxes^.Init(Bounds, AStrings, ASelRange, AFlags,
      AStates, nil, @Self);
  end;

Constructor TMultiCheckBoxes.Load(var s: TStream);
  begin
    _TMultiCheckBoxes^.Load(_Model1.TStream(s), nil, @Self);
  end;

procedure TMultiCheckBoxes.Store(var s: TStream);
  begin
    _TMultiCheckBoxes^.Store(_Model1.TStream(s), @Self);
  end;

Constructor TScroller.Init(var Bounds: TRect; AHScrollBar,
    AVScrollBar: PScrollBar);
  begin
    _TScroller^.Init(Bounds, _Model1.PScrollBar(AHScrollBar),
      _Model1.PScrollBar(AVScrollBar), nil, @Self);
  end;

Constructor TScroller.Load(var s: TStream);
  begin
    _TScroller^.Load(_Model1.TStream(s), nil, @Self);
  end;

procedure TScroller.ScrollDraw;
  assembler; {&Frame-}
asm
end;

procedure TScroller.ScrollTo(X, Y: longInt);
  begin
    _TScroller^.ScrollTo(X, Y, @Self);
  end;

procedure TScroller.SetLimit(X, Y: longInt);
  begin
    _TScroller^.SetLimit(X, Y, @Self);
  end;

procedure TScroller.Store(var s: TStream);
  begin
    _TScroller^.Store(_Model1.TStream(s), @Self);
  end;

Constructor TListViewer.Init(var Bounds: TRect; ANumCols: longInt;
    AHScrollBar, AVScrollBar: PScrollBar);
  begin
    _TListViewer^.Init(Bounds, ANumCols, _Model1.PScrollBar(
      AHScrollBar), _Model1.PScrollBar(AVScrollBar), nil, @Self);
  end;

Constructor TListViewer.Load(var s: TStream);
  begin
    _TListViewer^.Load(_Model1.TStream(s), nil, @Self);
  end;

procedure TListViewer.FocusItem(Item: longInt);
  assembler; {&Frame-}
asm
end;

function TListViewer.GetText(Item: longInt; MaxLen: integer): String;
  assembler; {&Frame-}
asm
end;

function TListViewer.IsSelected(Item: longInt): boolean;
  assembler; {&Frame-}
asm
end;

procedure TListViewer.SelectItem(Item: longInt);
  assembler; {&Frame-}
asm
end;

procedure TListViewer.SetRange(ARange: longInt);
  begin
    _TListViewer^.SetRange(ARange, @Self);
  end;

procedure TListViewer.Store(var s: TStream);
  begin
    _TListViewer^.Store(_Model1.TStream(s), @Self);
  end;

procedure TListViewer.FocusItemNum(Item: longInt);
  assembler; {&Frame-}
asm
end;

Constructor TListBox.Init(var Bounds: TRect; ANumCols: word;
    AScrollBar: PScrollBar);
  begin
    _TListBox^.Init(Bounds, ANumCols, _Model1.PScrollBar(AScrollBar),
      nil, @Self);
  end;

Constructor TListBox.Load(var s: TStream);
  begin
    _TListBox^.Load(_Model1.TStream(s), nil, @Self);
  end;

procedure TListBox.NewLisT(AList: PCollection);
  assembler; {&Frame-}
asm
end;

procedure TListBox.Store(var s: TStream);
  begin
    _TListBox^.Store(_Model1.TStream(s), @Self);
  end;

Constructor TStaticText.Init(var Bounds: TRect; const AText: String);
  begin
    _TStaticText^.Init(Bounds, AText, nil, @Self);
  end;

Constructor TStaticText.Load(var s: TStream);
  begin
    _TStaticText^.Load(_Model1.TStream(s), nil, @Self);
  end;

procedure TStaticText.GetText(var s: String);
  assembler; {&Frame-}
asm
end;

procedure TStaticText.Store(var s: TStream);
  begin
    _TStaticText^.Store(_Model1.TStream(s), @Self);
  end;

Constructor TParamText.Init(var Bounds: TRect; const AText: String;
    AParamCount: AInt);
  begin
    _TParamText^.Init(Bounds, AText, AParamCount, nil, @Self);
  end;

Constructor TParamText.Load(var s: TStream);
  begin
    _TParamText^.Load(_Model1.TStream(s), nil, @Self);
  end;

procedure TParamText.Store(var s: TStream);
  begin
    _TParamText^.Store(_Model1.TStream(s), @Self);
  end;

Constructor TLabel.Init(var Bounds: TRect; const AText: String;
    ALink: PView);
  begin
    _TLabel^.Init(Bounds, AText, _Model1.PView(ALink), nil, @Self);
  end;

Constructor TLabel.Load(var s: TStream);
  begin
    _TLabel^.Load(_Model1.TStream(s), nil, @Self);
  end;

procedure TLabel.Store(var s: TStream);
  begin
    _TLabel^.Store(_Model1.TStream(s), @Self);
  end;

Constructor THistoryViewer.Init(var Bounds: TRect; AHScrollBar,
    AVScrollBar: PScrollBar; AHistoryId: AWord);
  begin
    _THistoryViewer^.Init(Bounds, _Model1.PScrollBar(AHScrollBar),
      _Model1.PScrollBar(AVScrollBar), AHistoryId, nil, @Self);
  end;

function THistoryViewer.HistoryWidth: integer;
  begin
    Result := _THistoryViewer^.HistoryWidth(@Self);
  end;

Constructor THistoryWindow.Init(var Bounds: TRect; HistoryId: AWord);
  begin
    _THistoryWindow^.Init(Bounds, HistoryId, nil, @Self);
  end;

function THistoryWindow.GetSelection: String;
  assembler; {&Frame-}
asm
end;

procedure THistoryWindow.InitViewer(HistoryId: AWord);
  assembler; {&Frame-}
asm
end;

Constructor THistory.Init(var Bounds: TRect; ALink: PInputline;
    AHistoryId: AWord);
  begin
    _THistory^.Init(Bounds, _Model1.PInputline(ALink), AHistoryId,
      nil, @Self);
  end;

Constructor THistory.Load(var s: TStream);
  begin
    _THistory^.Load(_Model1.TStream(s), nil, @Self);
  end;

function THistory.InitHistoryWindow(var Bounds: TRect):
    PHistoryWindow;
  assembler; {&Frame-}
asm
end;

procedure THistory.RecordHistory(const s: String);
  assembler; {&Frame-}
asm
end;

procedure THistory.Store(var s: TStream);
  begin
    _THistory^.Store(_Model1.TStream(s), @Self);
  end;

end.
