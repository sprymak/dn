unit _Gauge;
(******

DN/2 Plugin Interface - object model
Copyright (C) 2002 Aleksej Kozlov (Cat)
2:5030/1326.13

******)

{&Delphi+}
{&Use32+}

interface

uses
  _Defines, _Collect, _Views, _Dialogs;

type
  PPercentGauge = ^TPercentGauge;
  TPercentGauge = object(TView)
    MaxValue: longInt;
    CurValue: longInt;
    Constructor Init(var Bounds: TRect; AMaxValue: longInt);
    {procedure Draw; virtual;}
    procedure UpdateView(Progress: longInt); virtual;
    procedure AddProgress(Progress: longInt);
    {procedure HandleEvent(var Event: TEvent); virtual;}
    function SolveForX(Y, Z: longInt): integer;
    function SolveForY(X, Z: longInt): integer;
    end;

  PBarGauge = ^TBarGauge;
  TBarGauge = object(TPercentGauge)
    {procedure Draw; virtual;}
    end;

  PWhileView = ^TWhileView;
  TWhileView = object(TGroup)
    Lines: PCollection;
    But: PButton;
    QuitNormal: boolean;
    Top, Bottom: String;
    Constructor Init(Bounds: TRect);
    procedure Write(n: integer; s: String);
    {function GetPalette: PPalette; virtual;}
    {function Valid(C: Word): Boolean; virtual;}
    {procedure SetState(AState: Word; Enable: Boolean); virtual;}
    {procedure Draw; virtual;}
    {procedure HandleEvent(var Event: TEvent); virtual;}
    {destructor Done; virtual;}
    procedure ClearInterior;
    private
    Side: (sdLeft, sdRight);
    end;

implementation

uses
  _DNFuncs;

Constructor TPercentGauge.Init(var Bounds: TRect; AMaxValue: longInt);
  begin
    _TPercentGauge^.Init(Bounds, AMaxValue, nil, @Self);
  end;

procedure TPercentGauge.UpdateView(Progress: longInt);
  assembler; {&Frame-}
asm
end;

procedure TPercentGauge.AddProgress(Progress: longInt);
  begin
    _TPercentGauge^.AddProgress(Progress, @Self);
  end;

function TPercentGauge.SolveForX(Y, Z: longInt): integer;
  begin
    Result := _TPercentGauge^.SolveForX(Y, Z, @Self);
  end;

function TPercentGauge.SolveForY(X, Z: longInt): integer;
  begin
    Result := _TPercentGauge^.SolveForY(X, Z, @Self);
  end;

Constructor TWhileView.Init(Bounds: TRect);
  begin
    _TWhileView^.Init(Bounds, nil, @Self);
  end;

procedure TWhileView.Write(n: integer; s: String);
  begin
    _TWhileView^.Write(n, s, @Self);
  end;

procedure TWhileView.ClearInterior;
  begin
    _TWhileView^.ClearInterior(@Self);
  end;

end.
