unit _FViewer;
(******

DN/2 Plugin Interface - object model
Copyright (C) 2002 Aleksej Kozlov (Cat)
2:5030/1326.13

******)

{&Delphi+}
{&Use32+}

interface

uses
  _Defines, _Streams, _Views;

type
  PViewScroll = ^TViewScroll;
  TViewScroll = object(TView)
    MaxV, Value: longInt;
    {function GetPalette: PPalette; virtual;}
    {procedure HandleEvent(var Event: TEvent); virtual;}
    function GetPartCode: longInt;
    {procedure Draw; virtual;}
    function GetSize: integer;
    procedure DrawPos(Pos: integer);
    end;

  PFileViewer = ^TFileViewer;
  TFileViewer = object(TView)
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
    Constructor Init(var Bounds: TRect; AStream: PStream;
    const AFileName, AVFileName: String;
    ASB: PView; Quick, Hex: boolean);
    Constructor Load(var s: TStream);
    procedure Store(var s: TStream);
    {destructor Done; virtual;}
    procedure ShowView; virtual;
    procedure HideView; virtual;
    {procedure Draw; virtual;}
    procedure SetXlatFile(const FName: String; DoRedraw: boolean);
    function ReadFile(const FName, VFName: String; NewStream:
      boolean): boolean;
    {procedure SetState(AState: Word; Enable: Boolean); virtual;}
    {procedure HandleEvent(var Event: TEvent); virtual;}
    function WriteModify: boolean;
    procedure CountDown(ANumber: integer); virtual;
    procedure CountUp(ANumber: integer); virtual;
    procedure Seek(APos: longInt);
    procedure MakeLines; virtual;
    procedure SaveToFile(FN: String);
    {function Valid(Command: Word): Boolean; virtual;}
    {procedure ChangeBounds(var Bounds: TRect); virtual;}
    {function  GetPalette: PPalette; virtual;}
    procedure DoHighlite(var B; const s: String; const Attr: String);
    {procedure Update; virtual;}
    procedure SeekEof;
    procedure SeekBof;
    function BreakOnStreamReadError: boolean;
    end;

implementation

uses
  _DNFuncs;

function TViewScroll.GetPartCode: longInt;
  begin
    Result := _TViewScroll^.GetPartCode(@Self);
  end;

function TViewScroll.GetSize: integer;
  begin
    Result := _TViewScroll^.GetSize(@Self);
  end;

procedure TViewScroll.DrawPos(Pos: integer);
  begin
    _TViewScroll^.DrawPos(Pos, @Self);
  end;

Constructor TFileViewer.Init(var Bounds: TRect; AStream: PStream;
  const AFileName, AVFileName: String;
  ASB: PView; Quick, Hex: boolean);
  begin
    _TFileViewer^.Init(Bounds, _Model1.PStream(AStream), AFileName,
      AVFileName, _Model1.PView(ASB), Quick, Hex, nil, @Self);
  end;

Constructor TFileViewer.Load(var s: TStream);
  begin
    _TFileViewer^.Load(_Model1.TStream(s), nil, @Self);
  end;

procedure TFileViewer.Store(var s: TStream);
  begin
    _TFileViewer^.Store(_Model1.TStream(s), @Self);
  end;

procedure TFileViewer.ShowView;
  assembler; {&Frame-}
asm
end;

procedure TFileViewer.HideView;
  assembler; {&Frame-}
asm
end;

procedure TFileViewer.SetXlatFile(const FName: String; DoRedraw:
    boolean);
  begin
    _TFileViewer^.SetXlatFile(FName, DoRedraw, @Self);
  end;

function TFileViewer.ReadFile(const FName, VFName: String; NewStream:
    boolean): boolean;
  begin
    Result := _TFileViewer^.ReadFile(FName, VFName, NewStream, @Self);
  end;

function TFileViewer.WriteModify: boolean;
  begin
    Result := _TFileViewer^.WriteModify(@Self);
  end;

procedure TFileViewer.CountDown(ANumber: integer);
  assembler; {&Frame-}
asm
end;

procedure TFileViewer.CountUp(ANumber: integer);
  assembler; {&Frame-}
asm
end;

procedure TFileViewer.Seek(APos: longInt);
  begin
    _TFileViewer^.Seek(APos, @Self);
  end;

procedure TFileViewer.MakeLines;
  assembler; {&Frame-}
asm
end;

procedure TFileViewer.SaveToFile(FN: String);
  begin
    _TFileViewer^.SaveToFile(FN, @Self);
  end;

procedure TFileViewer.DoHighlite(var B; const s: String; const Attr:
    String);
  begin
    _TFileViewer^.DoHighlite(B, s, Attr, @Self);
  end;

procedure TFileViewer.SeekEof;
  begin
    _TFileViewer^.SeekEof(@Self);
  end;

procedure TFileViewer.SeekBof;
  begin
    _TFileViewer^.SeekBof(@Self);
  end;

function TFileViewer.BreakOnStreamReadError: boolean;
  begin
    Result := _TFileViewer^.BreakOnStreamReadError(@Self);
  end;

end.
