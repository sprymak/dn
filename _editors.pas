unit _Editors;
(******

DN/2 Plugin Interface - object model
Copyright (C) 2002 Aleksej Kozlov (Cat)
2:5030/1326.13

******)

{&Delphi+}
{&Use32+}

interface

uses
  _Defines, _Streams, _Collect, _Views, _Menus;

type
  PUniWindow = ^TUniWindow;
  TUniWindow = object(TWindow)
    {function GetPalette: PPalette; virtual;}
    function MakeScrollBar(AOptions: word): PScrollBar;
    {procedure InitFrame; virtual;}
    {function ReactOnCmd: Boolean; virtual;}
    end;

  PXFileEditor = ^TXFileEditor;
  TXFileEditor = object(TView)
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
    RegExp: Pointer {PRegExp};
    Constructor Init(var Bounds: TRect; AHScrollBar, AVScrollBar:
      PScrollBar; var FileName: String);
    Constructor Load(var s: TStream);
    {destructor Done; virtual;}
    procedure Store(var s: TStream);
    {procedure Awaken; virtual;}
    procedure DoHighlite(var B; const s: LongString; const Attr:
      String);
    {procedure HandleEvent(var Event: TEvent); virtual;}
    {procedure Draw; virtual;}
    {function Valid(Command: Word): Boolean; virtual;}
    {function GetPalette: PPalette; virtual;}
    function GetLine(Index: longInt): LongString;
    function GetSelection: PCollection;
    {procedure SetState(AState: Word; Enable: Boolean); virtual;}
    function ValidBlock: boolean;
    procedure CalcMenu;
    function Search(StartX, StartY: word): boolean;
    procedure InsertBlock(ABlock: PCollection; SaveUndo: boolean);
    procedure ModifyLine(Index: longInt; s: LongString; DelSpaces:
      boolean);
    procedure SetLimits;
    {procedure ChangeBounds(var R: TRect); virtual;}
    procedure ScrollTo(DeltaX, DeltaY: longInt);
    function LimitX: longInt;
    function LimitY: longInt;
    procedure StoreUndoInfo(What: word; Where: TPoint; var Info);
    function HandleCommand(var Event: TEvent): boolean; virtual;
    function KeyMapConvertStr(s: LongString; toAscii: boolean):
      LongString;
    procedure KeyMapAtInsert(n: longInt; P: PLongString);
    procedure KeyMapAtReplace(n: longInt; P: PLongString);
    end;

  PEditWindow = ^TEditWindow;
  TEditWindow = object(TUniWindow)
    AInfo: Pointer {PInfoLine};
    ABookLine: Pointer {PBookmarkLine};
    Intern: PXFileEditor;
    MenuBar: PMenuBar;
    UpMenu: PMenu;
    ModalEnd: boolean;
    Constructor Init(R: TRect; FileName: String);
    Constructor Load(var s: TStream);
    procedure Store(var s: TStream);
    {function Execute: Word; virtual;}
    {procedure SetState(AState: Word; Enable: Boolean); virtual;}
    end;

implementation

uses
  _DNFuncs;

function TUniWindow.MakeScrollBar(AOptions: word): PScrollBar;
  begin
    Result := PScrollBar(_TUniWindow^.MakeScrollBar(AOptions, @Self));
  end;

Constructor TXFileEditor.Init(var Bounds: TRect; AHScrollBar,
    AVScrollBar: PScrollBar; var FileName: String);
  begin
    _TXFileEditor^.Init(Bounds, _Model1.PScrollBar(AHScrollBar),
      _Model1.PScrollBar(AVScrollBar), FileName, nil, @Self);
  end;

Constructor TXFileEditor.Load(var s: TStream);
  begin
    _TXFileEditor^.Load(_Model1.TStream(s), nil, @Self);
  end;

procedure TXFileEditor.Store(var s: TStream);
  begin
    _TXFileEditor^.Store(_Model1.TStream(s), @Self);
  end;

procedure TXFileEditor.DoHighlite(var B; const s: LongString; const
    Attr: String);
  begin
    _TXFileEditor^.DoHighlite(B, s, Attr, @Self);
  end;

function TXFileEditor.GetLine(Index: longInt): LongString;
  begin
    Result := _TXFileEditor^.GetLine(Index, @Self);
  end;

function TXFileEditor.GetSelection: PCollection;
  begin
    Result := PCollection(_TXFileEditor^.GetSelection(@Self));
  end;

function TXFileEditor.ValidBlock: boolean;
  begin
    Result := _TXFileEditor^.ValidBlock(@Self);
  end;

procedure TXFileEditor.CalcMenu;
  begin
    _TXFileEditor^.CalcMenu(@Self);
  end;

function TXFileEditor.Search(StartX, StartY: word): boolean;
  begin
    Result := _TXFileEditor^.Search(StartX, StartY, @Self);
  end;

procedure TXFileEditor.InsertBlock(ABlock: PCollection; SaveUndo:
    boolean);
  begin
    _TXFileEditor^.InsertBlock(_Model1.PCollection(ABlock), SaveUndo,
      @Self);
  end;

procedure TXFileEditor.ModifyLine(Index: longInt; s: LongString;
    DelSpaces: boolean);
  begin
    _TXFileEditor^.ModifyLine(Index, s, DelSpaces, @Self);
  end;

procedure TXFileEditor.SetLimits;
  begin
    _TXFileEditor^.SetLimits(@Self);
  end;

procedure TXFileEditor.ScrollTo(DeltaX, DeltaY: longInt);
  begin
    _TXFileEditor^.ScrollTo(DeltaX, DeltaY, @Self);
  end;

function TXFileEditor.LimitX: longInt;
  begin
    Result := _TXFileEditor^.LimitX(@Self);
  end;

function TXFileEditor.LimitY: longInt;
  begin
    Result := _TXFileEditor^.LimitY(@Self);
  end;

procedure TXFileEditor.StoreUndoInfo(What: word; Where: TPoint; var
    Info);
  begin
    _TXFileEditor^.StoreUndoInfo(What, Where, Info, @Self);
  end;

function TXFileEditor.HandleCommand(var Event: TEvent): boolean;
  assembler; {&Frame-}
asm
end;

function TXFileEditor.KeyMapConvertStr(s: LongString; toAscii:
    boolean): LongString;
  begin
    Result := _TXFileEditor^.KeyMapConvertStr(s, toAscii, @Self);
  end;

procedure TXFileEditor.KeyMapAtInsert(n: longInt; P: PLongString);
  begin
    _TXFileEditor^.KeyMapAtInsert(n, P, @Self);
  end;

procedure TXFileEditor.KeyMapAtReplace(n: longInt; P: PLongString);
  begin
    _TXFileEditor^.KeyMapAtReplace(n, P, @Self);
  end;

Constructor TEditWindow.Init(R: TRect; FileName: String);
  begin
    _TEditWindow^.Init(R, FileName, nil, @Self);
  end;

Constructor TEditWindow.Load(var s: TStream);
  begin
    _TEditWindow^.Load(_Model1.TStream(s), nil, @Self);
  end;

procedure TEditWindow.Store(var s: TStream);
  begin
    _TEditWindow^.Store(_Model1.TStream(s), @Self);
  end;

end.
