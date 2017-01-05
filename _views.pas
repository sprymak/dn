unit _Views;
(******

DN/2 Plugin Interface - object model
Copyright (C) 2002 Aleksej Kozlov (Cat)
2:5030/1326.13

******)

{&Delphi+}
{&Use32+}

interface

uses
  _Defines, _Objects, _Streams;

type
  PView = ^TView;
  PGroup = ^TGroup;

  TView = object(TObject)
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
    Constructor Init(var Bounds: TRect);
    Constructor Load(var s: TStream);
    {destructor Done; virtual;}
    procedure Awaken; virtual;
    procedure BlockCursor;
    procedure CalcBounds(var Bounds: TRect; Delta: TPoint); virtual;
    procedure ChangeBounds(var Bounds: TRect); virtual;
    procedure ClearEvent(var Event: TEvent);
    function CommandEnabled(Command: word): boolean;
    function DataSize: word; virtual;
    procedure DisableCommands(Commands: TCommandSet);
    procedure DragView(Event: TEvent; Mode: byte; var Limits: TRect;
      MinSize, MaxSize: TPoint);
    procedure Draw; virtual;
    procedure DrawView;
    procedure EnableCommands(Commands: TCommandSet);
    procedure EndModal(Command: word); virtual;
    function EventAvail: boolean;
    function Execute: word; virtual;
    function Exposed: boolean;
    function Focus: boolean;
    procedure GetBounds(var Bounds: TRect);
    procedure GetClipRect(var Clip: TRect);
    function GetColor(Color: word): word;
    procedure GetCommands(var Commands: TCommandSet);
    procedure GetData(var Rec); virtual;
    procedure GetEvent(var Event: TEvent); virtual;
    procedure GetExtent(var extent: TRect);
    function GetHelpCtx: word; virtual;
    function GetPalette: PPalette; virtual;
    procedure GetPeerViewPtr(var s: TStream; var P);
    function GetState(AState: word): boolean;
    procedure GrowTo(X, Y: longInt);
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure Hide;
    procedure HideCursor;
    procedure KeyEvent(var Event: TEvent);
    procedure Locate(var Bounds: TRect);
    procedure MakeFirst;
    procedure MakeGlobal(Source: TPoint; var Dest: TPoint);
    procedure MakeLocal(Source: TPoint; var Dest: TPoint);
    function MouseEvent(var Event: TEvent; Mask: word): boolean;
    function MouseInView(Mouse: TPoint): boolean;
    procedure MoveTo(X, Y: longInt);
    function NextView: PView;
    procedure NormalCursor;
    function Prev: PView;
    function PrevView: PView;
    procedure PutEvent(var Event: TEvent); virtual;
    procedure PutInFrontOf(Target: PView);
    procedure PutPeerViewPtr(var s: TStream; P: PView);
    procedure Select;
    procedure SetBounds(var Bounds: TRect);
    procedure SetCommands(Commands: TCommandSet);
    procedure SetCursor(X, Y: longInt);
    procedure SetData(var Rec); virtual;
    procedure SetState(AState: word; Enable: boolean); virtual;
    procedure Show;
    procedure ShowCursor;
    procedure SizeLimits(var Min, Max: TPoint); virtual;
    procedure Store(var s: TStream);
    function TopView: PView;
    function Valid(Command: word): boolean; virtual;
    procedure WriteBuf(X, Y, W, H: integer; var Buf);
    procedure WriteChar(X, Y: integer; C: Char; Color: byte; Count:
      integer);
    procedure WriteLine(X, Y, W, H: integer; var Buf);
    procedure WriteStr(X, Y: integer; Str: String; Color: byte);
    procedure UpDate; virtual;
    function MenuEnabled(Command: word): boolean;
    procedure DrawCursor;
    procedure DrawHide(LastView: PView);
    procedure DrawShow(LastView: PView);
    procedure DrawUnderRect(var R: TRect; LastView: PView);
    procedure DrawUnderView(DoShadow: boolean; LastView: PView);
    procedure ResetCursor; virtual;
    end;

  PFrame = ^TFrame;
  TFrame = object(TView)
    Constructor Init(var Bounds: TRect);
    {procedure Draw; virtual;}
    {function GetPalette: PPalette; virtual;}
    {procedure HandleEvent(var Event: TEvent); virtual;}
    {procedure SetState(AState: Word; Enable: Boolean); virtual;}
    end;

  PScrollBar = ^TScrollBar;
  TScrollBar = object(TView)
    Value: longInt;
    Min: longInt;
    Max: longInt;
    PgStep: longInt;
    ArStep: longInt;
    Constructor Init(var Bounds: TRect);
    Constructor Load(var s: TStream);
    {procedure Draw; virtual;}
    {function GetPalette: PPalette; virtual;}
    {procedure HandleEvent(var Event: TEvent); virtual;}
    procedure ScrollDraw; virtual;
    function ScrollStep(Part: longInt): longInt; virtual;
    procedure SetParams(AValue, AMin, AMax, APgStep, AArStep:
      longInt);
    procedure SetRange(AMin, AMax: longInt);
    procedure SetStep(APgStep, AArStep: longInt);
    procedure SetValue(AValue: longInt);
    procedure Store(var s: TStream);
    end;

  TGroup = object(TView)
    Last: PView;
    Current: PView;
    Phase: (phFocused, phPreProcess, phPostProcess);
    Buffer: PVideoBuf;
    Clip: TRect;
    LockFlag: byte;
    EndState: word;
    Constructor Init(var Bounds: TRect);
    Constructor Load(var s: TStream);
    {destructor Done; virtual;}
    {procedure Awaken; virtual;}
    {procedure ChangeBounds(var Bounds: TRect); virtual;}
    {function DataSize: Word; virtual;}
    procedure Delete(P: PView);
    {procedure Draw; virtual;}
    {procedure EndModal(Command: Word); virtual;}
    procedure EventError(var Event: TEvent); virtual;
    function ExecView(P: PView): word;
    {function Execute: Word; virtual;}
    function First: PView;
    function FirstThat(P: Pointer): PView;
    function LastThat(P: Pointer): PView;
    function FocusNext(Forwards: boolean): boolean;
    procedure ForEach(P: Pointer);
    {procedure GetData(var Rec); virtual;}
    {function GetHelpCtx: Word; virtual;}
    procedure GetSubViewPtr(var s: TStream; var P);
    {procedure HandleEvent(var Event: TEvent); virtual;}
    procedure Insert(P: PView);
    procedure InsertBefore(P, Target: PView);
    procedure Lock;
    procedure PutSubViewPtr(var s: TStream; P: PView);
    procedure Redraw; virtual;
    procedure SelectNext(Forwards: boolean);
    {procedure SetData(var Rec); virtual;}
    {procedure SetState(AState: Word; Enable: Boolean); virtual;}
    procedure Store(var s: TStream);
    procedure UnLock;
    {function Valid(Command: Word): Boolean; virtual;}
    procedure FreeBuffer;
    procedure GetBuffer;
    procedure InsertView(P, Target: PView);
    procedure SetCurrent(P: PView; Mode: TSelectMode);
    end;

  PWindow = ^TWindow;
  TWindow = object(TGroup)
    Flags: byte;
    ZoomRect: TRect;
    MaxiRect: TRect;
    Number: AInt;
    Palette: AInt;
    Frame: PFrame;
    Title: PString;
    Constructor Init(var Bounds: TRect; const ATitle: String;
      ANumber: integer);
    Constructor Load(var s: TStream);
    {destructor Done; virtual;}
    procedure Close; virtual;
    {function GetPalette: PPalette; virtual;}
    function GetTitle(MaxSize: integer): String; virtual;
    {procedure HandleEvent(var Event: TEvent); virtual;}
    procedure InitFrame; virtual;
    {procedure SetState(AState: Word; Enable: Boolean); virtual;}
    {procedure SizeLimits(var Min, Max: TPoint); virtual;}
    function StandardScrollBar(AOptions: word): PScrollBar;
    procedure Store(var s: TStream);
    procedure Zoom; virtual;
    procedure Maxi; virtual;
    function ReactOnCmd: boolean; virtual;
    {procedure Draw; virtual;}
    end;

implementation

uses
  _DNFuncs;

Constructor TView.Init(var Bounds: TRect);
  begin
    _TView^.Init(Bounds, nil, @Self);
  end;

Constructor TView.Load(var s: TStream);
  begin
    _TView^.Load(_Model1.TStream(s), nil, @Self);
  end;

procedure TView.Awaken;
  assembler; {&Frame-}
asm
end;

procedure TView.BlockCursor;
  begin
    _TView^.BlockCursor(@Self);
  end;

procedure TView.CalcBounds(var Bounds: TRect; Delta: TPoint);
  assembler; {&Frame-}
asm
end;

procedure TView.ChangeBounds(var Bounds: TRect);
  assembler; {&Frame-}
asm
end;

procedure TView.ClearEvent(var Event: TEvent);
  begin
    _TView^.ClearEvent(Event, @Self);
  end;

function TView.CommandEnabled(Command: word): boolean;
  begin
    Result := _TView^.CommandEnabled(Command, @Self);
  end;

function TView.DataSize: word;
  assembler; {&Frame-}
asm
end;

procedure TView.DisableCommands(Commands: TCommandSet);
  begin
    _TView^.DisableCommands(Commands, @Self);
  end;

procedure TView.DragView(Event: TEvent; Mode: byte; var Limits:
    TRect; MinSize, MaxSize: TPoint);
  begin
    _TView^.DragView(Event, Mode, Limits, MinSize, MaxSize, @Self);
  end;

procedure TView.Draw;
  assembler; {&Frame-}
asm
end;

procedure TView.DrawView;
  begin
    _TView^.DrawView(@Self);
  end;

procedure TView.EnableCommands(Commands: TCommandSet);
  begin
    _TView^.EnableCommands(Commands, @Self);
  end;

procedure TView.EndModal(Command: word);
  assembler; {&Frame-}
asm
end;

function TView.EventAvail: boolean;
  begin
    Result := _TView^.EventAvail(@Self);
  end;

function TView.Execute: word;
  assembler; {&Frame-}
asm
end;

function TView.Exposed: boolean;
  begin
    Result := _TView^.Exposed(@Self);
  end;

function TView.Focus: boolean;
  begin
    Result := _TView^.Focus(@Self);
  end;

procedure TView.GetBounds(var Bounds: TRect);
  begin
    _TView^.GetBounds(Bounds, @Self);
  end;

procedure TView.GetClipRect(var Clip: TRect);
  begin
    _TView^.GetClipRect(Clip, @Self);
  end;

function TView.GetColor(Color: word): word;
  begin
    Result := _TView^.GetColor(Color, @Self);
  end;

procedure TView.GetCommands(var Commands: TCommandSet);
  begin
    _TView^.GetCommands(Commands, @Self);
  end;

procedure TView.GetData(var Rec);
  assembler; {&Frame-}
asm
end;

procedure TView.GetEvent(var Event: TEvent);
  assembler; {&Frame-}
asm
end;

procedure TView.GetExtent(var extent: TRect);
  begin
    _TView^.GetExtent(extent, @Self);
  end;

function TView.GetHelpCtx: word;
  assembler; {&Frame-}
asm
end;

function TView.GetPalette: PPalette;
  assembler; {&Frame-}
asm
end;

procedure TView.GetPeerViewPtr(var s: TStream; var P);
  begin
    _TView^.GetPeerViewPtr(_Model1.TStream(s), P, @Self);
  end;

function TView.GetState(AState: word): boolean;
  begin
    Result := _TView^.GetState(AState, @Self);
  end;

procedure TView.GrowTo(X, Y: longInt);
  begin
    _TView^.GrowTo(X, Y, @Self);
  end;

procedure TView.HandleEvent(var Event: TEvent);
  assembler; {&Frame-}
asm
end;

procedure TView.Hide;
  begin
    _TView^.Hide(@Self);
  end;

procedure TView.HideCursor;
  begin
    _TView^.HideCursor(@Self);
  end;

procedure TView.KeyEvent(var Event: TEvent);
  begin
    _TView^.KeyEvent(Event, @Self);
  end;

procedure TView.Locate(var Bounds: TRect);
  begin
    _TView^.Locate(Bounds, @Self);
  end;

procedure TView.MakeFirst;
  begin
    _TView^.MakeFirst(@Self);
  end;

procedure TView.MakeGlobal(Source: TPoint; var Dest: TPoint);
  begin
    _TView^.MakeGlobal(Source, Dest, @Self);
  end;

procedure TView.MakeLocal(Source: TPoint; var Dest: TPoint);
  begin
    _TView^.MakeLocal(Source, Dest, @Self);
  end;

function TView.MouseEvent(var Event: TEvent; Mask: word): boolean;
  begin
    Result := _TView^.MouseEvent(Event, Mask, @Self);
  end;

function TView.MouseInView(Mouse: TPoint): boolean;
  begin
    Result := _TView^.MouseInView(Mouse, @Self);
  end;

procedure TView.MoveTo(X, Y: longInt);
  begin
    _TView^.MoveTo(X, Y, @Self);
  end;

function TView.NextView: PView;
  begin
    Result := PView(_TView^.NextView(@Self));
  end;

procedure TView.NormalCursor;
  begin
    _TView^.NormalCursor(@Self);
  end;

function TView.Prev: PView;
  begin
    Result := PView(_TView^.Prev(@Self));
  end;

function TView.PrevView: PView;
  begin
    Result := PView(_TView^.PrevView(@Self));
  end;

procedure TView.PutEvent(var Event: TEvent);
  assembler; {&Frame-}
asm
end;

procedure TView.PutInFrontOf(Target: PView);
  begin
    _TView^.PutInFrontOf(_Model1.PView(Target), @Self);
  end;

procedure TView.PutPeerViewPtr(var s: TStream; P: PView);
  begin
    _TView^.PutPeerViewPtr(_Model1.TStream(s), _Model1.PView(P),
      @Self);
  end;

procedure TView.Select;
  begin
    _TView^.Select(@Self);
  end;

procedure TView.SetBounds(var Bounds: TRect);
  begin
    _TView^.SetBounds(Bounds, @Self);
  end;

procedure TView.SetCommands(Commands: TCommandSet);
  begin
    _TView^.SetCommands(Commands, @Self);
  end;

procedure TView.SetCursor(X, Y: longInt);
  begin
    _TView^.SetCursor(X, Y, @Self);
  end;

procedure TView.SetData(var Rec);
  assembler; {&Frame-}
asm
end;

procedure TView.SetState(AState: word; Enable: boolean);
  assembler; {&Frame-}
asm
end;

procedure TView.Show;
  begin
    _TView^.Show(@Self);
  end;

procedure TView.ShowCursor;
  begin
    _TView^.ShowCursor(@Self);
  end;

procedure TView.SizeLimits(var Min, Max: TPoint);
  assembler; {&Frame-}
asm
end;

procedure TView.Store(var s: TStream);
  begin
    _TView^.Store(_Model1.TStream(s), @Self);
  end;

function TView.TopView: PView;
  begin
    Result := PView(_TView^.TopView(@Self));
  end;

function TView.Valid(Command: word): boolean;
  assembler; {&Frame-}
asm
end;

procedure TView.WriteBuf(X, Y, W, H: integer; var Buf);
  begin
    _TView^.WriteBuf(X, Y, W, H, Buf, @Self);
  end;

procedure TView.WriteChar(X, Y: integer; C: Char; Color: byte; Count:
    integer);
  begin
    _TView^.WriteChar(X, Y, C, Color, Count, @Self);
  end;

procedure TView.WriteLine(X, Y, W, H: integer; var Buf);
  begin
    _TView^.WriteLine(X, Y, W, H, Buf, @Self);
  end;

procedure TView.WriteStr(X, Y: integer; Str: String; Color: byte);
  begin
    _TView^.WriteStr(X, Y, Str, Color, @Self);
  end;

procedure TView.UpDate;
  assembler; {&Frame-}
asm
end;

function TView.MenuEnabled(Command: word): boolean;
  begin
    Result := _TView^.MenuEnabled(Command, @Self);
  end;

procedure TView.DrawCursor;
  begin
    _TView^.DrawCursor(@Self);
  end;

procedure TView.DrawHide(LastView: PView);
  begin
    _TView^.DrawHide(_Model1.PView(LastView), @Self);
  end;

procedure TView.DrawShow(LastView: PView);
  begin
    _TView^.DrawShow(_Model1.PView(LastView), @Self);
  end;

procedure TView.DrawUnderRect(var R: TRect; LastView: PView);
  begin
    _TView^.DrawUnderRect(R, _Model1.PView(LastView), @Self);
  end;

procedure TView.DrawUnderView(DoShadow: boolean; LastView: PView);
  begin
    _TView^.DrawUnderView(DoShadow, _Model1.PView(LastView), @Self);
  end;

procedure TView.ResetCursor;
  assembler; {&Frame-}
asm
end;

Constructor TFrame.Init(var Bounds: TRect);
  begin
    _TFrame^.Init(Bounds, nil, @Self);
  end;

Constructor TScrollBar.Init(var Bounds: TRect);
  begin
    _TScrollBar^.Init(Bounds, nil, @Self);
  end;

Constructor TScrollBar.Load(var s: TStream);
  begin
    _TScrollBar^.Load(_Model1.TStream(s), nil, @Self);
  end;

procedure TScrollBar.ScrollDraw;
  assembler; {&Frame-}
asm
end;

function TScrollBar.ScrollStep(Part: longInt): longInt;
  assembler; {&Frame-}
asm
end;

procedure TScrollBar.SetParams(AValue, AMin, AMax, APgStep, AArStep:
    longInt);
  begin
    _TScrollBar^.SetParams(AValue, AMin, AMax, APgStep, AArStep,
      @Self);
  end;

procedure TScrollBar.SetRange(AMin, AMax: longInt);
  begin
    _TScrollBar^.SetRange(AMin, AMax, @Self);
  end;

procedure TScrollBar.SetStep(APgStep, AArStep: longInt);
  begin
    _TScrollBar^.SetStep(APgStep, AArStep, @Self);
  end;

procedure TScrollBar.SetValue(AValue: longInt);
  begin
    _TScrollBar^.SetValue(AValue, @Self);
  end;

procedure TScrollBar.Store(var s: TStream);
  begin
    _TScrollBar^.Store(_Model1.TStream(s), @Self);
  end;

Constructor TGroup.Init(var Bounds: TRect);
  begin
    _TGroup^.Init(Bounds, nil, @Self);
  end;

Constructor TGroup.Load(var s: TStream);
  begin
    _TGroup^.Load(_Model1.TStream(s), nil, @Self);
  end;

procedure TGroup.Delete(P: PView);
  begin
    _TGroup^.Delete(_Model1.PView(P), @Self);
  end;

procedure TGroup.EventError(var Event: TEvent);
  assembler; {&Frame-}
asm
end;

function TGroup.ExecView(P: PView): word;
  begin
    Result := _TGroup^.ExecView(_Model1.PView(P), @Self);
  end;

function TGroup.First: PView;
  begin
    Result := PView(_TGroup^.First(@Self));
  end;

function TGroup.FirstThat(P: Pointer): PView;
  begin
    Result := PView(_TGroup^.FirstThat(P, @Self));
  end;

function TGroup.LastThat(P: Pointer): PView;
  begin
    Result := PView(_TGroup^.LastThat(P, @Self));
  end;

function TGroup.FocusNext(Forwards: boolean): boolean;
  begin
    Result := _TGroup^.FocusNext(Forwards, @Self);
  end;

procedure TGroup.ForEach(P: Pointer);
  begin
    _TGroup^.ForEach(P, @Self);
  end;

procedure TGroup.GetSubViewPtr(var s: TStream; var P);
  begin
    _TGroup^.GetSubViewPtr(_Model1.TStream(s), P, @Self);
  end;

procedure TGroup.Insert(P: PView);
  begin
    _TGroup^.Insert(_Model1.PView(P), @Self);
  end;

procedure TGroup.InsertBefore(P, Target: PView);
  begin
    _TGroup^.InsertBefore(_Model1.PView(P), _Model1.PView(Target),
      @Self);
  end;

procedure TGroup.Lock;
  begin
    _TGroup^.Lock(@Self);
  end;

procedure TGroup.PutSubViewPtr(var s: TStream; P: PView);
  begin
    _TGroup^.PutSubViewPtr(_Model1.TStream(s), _Model1.PView(P),
      @Self);
  end;

procedure TGroup.Redraw;
  assembler; {&Frame-}
asm
end;

procedure TGroup.SelectNext(Forwards: boolean);
  begin
    _TGroup^.SelectNext(Forwards, @Self);
  end;

procedure TGroup.Store(var s: TStream);
  begin
    _TGroup^.Store(_Model1.TStream(s), @Self);
  end;

procedure TGroup.UnLock;
  begin
    _TGroup^.UnLock(@Self);
  end;

procedure TGroup.FreeBuffer;
  begin
    _TGroup^.FreeBuffer(@Self);
  end;

procedure TGroup.GetBuffer;
  begin
    _TGroup^.GetBuffer(@Self);
  end;

procedure TGroup.InsertView(P, Target: PView);
  begin
    _TGroup^.InsertView(_Model1.PView(P), _Model1.PView(Target),
      @Self);
  end;

procedure TGroup.SetCurrent(P: PView; Mode: TSelectMode);
  begin
    _TGroup^.SetCurrent(_Model1.PView(P), Mode, @Self);
  end;

Constructor TWindow.Init(var Bounds: TRect; const ATitle: String;
    ANumber: integer);
  begin
    _TWindow^.Init(Bounds, ATitle, ANumber, nil, @Self);
  end;

Constructor TWindow.Load(var s: TStream);
  begin
    _TWindow^.Load(_Model1.TStream(s), nil, @Self);
  end;

procedure TWindow.Close;
  assembler; {&Frame-}
asm
end;

function TWindow.GetTitle(MaxSize: integer): String;
  assembler; {&Frame-}
asm
end;

procedure TWindow.InitFrame;
  assembler; {&Frame-}
asm
end;

function TWindow.StandardScrollBar(AOptions: word): PScrollBar;
  begin
    Result := PScrollBar(_TWindow^.StandardScrollBar(AOptions, @Self));
  end;

procedure TWindow.Store(var s: TStream);
  begin
    _TWindow^.Store(_Model1.TStream(s), @Self);
  end;

procedure TWindow.Zoom;
  assembler; {&Frame-}
asm
end;

procedure TWindow.Maxi;
  assembler; {&Frame-}
asm
end;

function TWindow.ReactOnCmd: boolean;
  assembler; {&Frame-}
asm
end;

end.
