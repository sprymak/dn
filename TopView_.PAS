unit TopView_;

interface

uses
  Views, Streams
  ;

type
  PTopView = ^TTopView;
  {`2 Базовый тип для текста, выводимого в заголовке панели. }
  TTopView = object(TView)
    Panel: PView;
    constructor Load(var S: TStream);
    procedure Store(var S: TStream); virtual;
    procedure Draw; virtual;
      {` Текст центрируется, не перекрывая элемент управления окна
      менеджера `}
    function GetPalette: PPalette; virtual;
    function GetText(MaxWidth: Integer): String; virtual;
      {` Этот метод обязательно должен быть перекрыт `}
    end;
  {`}

implementation

uses
  Defines, DblWnd, Drivers, Advance1
  ;

const
  CTopView = #11#12;

constructor TTopView.Load(var S: TStream);
  begin
  inherited Load(S);
  GetPeerViewPtr(S, Panel);
  end;

procedure TTopView.Store(var S: TStream);
  begin
  inherited Store(S);
  PutPeerViewPtr(S, Panel);
  end;

function TTopView.GetPalette: PPalette;
  const
    S: String[Length(CTopView)] = CTopView;
  begin
  GetPalette := @S;
  end;

function TTopView.GetText(MaxWidth: Integer): String;
  begin
  end;

procedure TTopView.Draw;
  var
    C: Word;
    B: TDrawBuffer;
    S: String;
    R, OldR: TRect;
    D: Integer;
    Width: Integer;
  begin
  Width := Panel^.Size.X - 4;
    {4 - это ширина элемента управления (номера окна в левой панели
     и кнопки максимизации в правой панели }
  if Width < 1 then
    Exit;
  S := GetText(Width);
  if Length(S) < Width - 2 then
    S := ' ' + S + ' ';
  R.A := Panel^.Origin;
  R.B.Y := R.A.Y;
  Dec(R.A.Y);
  D := (Width - Length(S) + 4) div 2;
  if D >= 4 then
    Inc(R.A.X, D) { пока можно, центрируем без учёта асимметрии }
  else if PDoubleWindow(Owner)^.Panel[pLeft].AnyPanel = Panel then
    inc(R.A.X, 4) { левая панель, прижимаем к номеру окна }
  else { правая панель, прижимаем к кнопке максимизации }
    inc(R.A.X, Width - Length(S));
  R.B.X := R.A.X + Length(S);
  GetBounds(OldR);
  if not MemEqual(R, OldR, SizeOf(R)) then
    begin
    Locate(R); { Тут будет рекурсия, поэтому второй раз рисовать не надо }
    Exit;
    end;
  C := GetColor(1);
  if not Panel^.GetState(sfSelected) then
    C := GetColor(2);
  MoveStr(B[0], S, C);
  WriteLine(0, 0, Size.X, Size.Y, B);
  end { TTopView.Draw };

end.
