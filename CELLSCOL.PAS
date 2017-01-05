{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source 1.51.08
//  Based on Dos Navigator (C) 1991-99 RIT Research Labs
//
//  This programs is free for commercial and non-commercial use as long as
//  the following conditions are aheared to.
//
//  Copyright remains RIT Research Labs, and as such any Copyright notices
//  in the code are not to be removed. If this package is used in a
//  product, RIT Research Labs should be given attribution as the RIT Research
//  Labs of the parts of the library used. This can be in the form of a textual
//  message at program startup or in documentation (online or textual)
//  provided with the package.
//
//  Redistribution and use in source and binary forms, with or without
//  modification, are permitted provided that the following conditions are
//  met:
//
//  1. Redistributions of source code must retain the copyright
//     notice, this list of conditions and the following disclaimer.
//  2. Redistributions in binary form must reproduce the above copyright
//     notice, this list of conditions and the following disclaimer in the
//     documentation and/or other materials provided with the distribution.
//  3. All advertising materials mentioning features or use of this software
//     must display the following acknowledgement:
//     "Based on Dos Navigator by RIT Research Labs."
//
//  THIS SOFTWARE IS PROVIDED BY RIT RESEARCH LABS "AS IS" AND ANY EXPRESS
//  OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
//  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
//  DISCLAIMED. IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE FOR
//  ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
//  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
//  GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
//  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
//  IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
//  OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
//  ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//
//  The licence and distribution terms for any publically available
//  version or derivative of this code cannot be changed. i.e. this code
//  cannot simply be copied and put under another distribution licence
//  (including the GNU Public Licence).
//
//////////////////////////////////////////////////////////////////////////}
{$I STDEFINE.INC}
{AK155 = Alexey Korop, 2:461/155@fidonet}

unit CellsCol;

interface

uses
  Objects, Collect, Calculat;

type
  PCellRec =^TCellRec;
  TCellRec = record
    Col: Byte;
    Row: AInt;
    Options: AWord;
    Decimals: Byte;
    Value: CReal;
    SucTop: AInt;
    NextC: AInt;
    S: String;
    end;

  TCellSearcRec = record Col: Byte; Row: AInt; end;
  PCellCollection = ^TCellCollection;
  TCellCollection = object(TSortedCollection)
    constructor ShortLoad(var S: TStream);
    procedure ShortStore(var S: TStream);
    procedure FreeItem(Item: Pointer); virtual;
    function NewCellRec(ACol, ARow: AInt; const A_S: String): PCellRec;
      { создать запись }
    function NewItem(ACol, ARow: AInt; const A_S: String): PCellRec;
      { создать запись и поместить ее в коллекцию }
    function ReplaceItem
      (ACol: Byte; ARow: AInt; const A_S: String): PCellRec;
    function Get(Col: byte; Row: AInt): PCellRec;
    procedure SetValue(Col, Row: Integer; AValue: Real);
    procedure DelItem(Col, Row: Integer);
    procedure PutItem(var S: TStream; Item: Pointer); virtual;
    function GetItem(var S: TStream): Pointer; virtual;
    function MakeFormatString(AValue: Real): String;
    function Compare(K1, K2: Pointer): Integer; virtual;
    function TSort(var Start: integer): boolean; {AK155}
    procedure ForRectangle(AX: Byte; AY: AInt; {AK155}
       BX: Byte; BY: AInt; Action: Pointer); {см. комментарий к телу!}
    end;

const
  MaxCellY = 4096;


  LenSavedDataRec = Sizeof(Byte) + SizeOf(AInt) + SizeOf(AWord) +
               SizeOf(Byte) + sizeof(Real);

  LenDataRec = SizeOf(TCellRec) - SizeOf(String) + 1;

{      Options :
        x x x x x x x
        | | | | | | `,
        | | | | | \--^- Cell format (Text, Formula, Value)
        | | | | \---,
        | | | \------^- Justify options (Right, Left, Center)
        | | \-------,
        | \----------<
        \------------^- Display format(As is, Dec, Comma, Exp, Currency,
                                          Logic, Don't display)
}
  coText         = $0000;
  coFormula      = $0001;
  coValue        = $0002;
  coTypeMask = 3;

  coRight        = $0004;
  coCenter       = $0008; {Left is (Options and 3 = 0)}
  coDec          = $0010;
  coComma        = $0020;
  coExp          = $0030;
  coBool         = $0040;
  coCurrency     = $0050;
  coDONT         = $0060;

  CommaChar : Char = ',';
  SeparatorChar: Char = '|';

function MakeComma(S: String): String;
function GetCellCoord(S: String; var X: Byte;var Y: AInt): Boolean;
function GetColName(X: Integer): ShortString;
function GetRowName(Y: Integer): String;
function GetCellName(X, Y: Integer): String;

implementation

uses Memory, advance, advance1, advance2, advance3, ObjType;

const VObjType:AWord=otCellCollection;

procedure TCellCollection.ShortStore(var S: TStream);
var TCount, TLimit, TDelta: AInt;
  procedure DoPutItem(P: Pointer); {$IFDEF BIT_16} far; {$ENDIF}
  begin
   PutItem(S, P);
  end;
begin
  TCount := Count;
  TLimit := Limit;
  TDelta := Delta;
  S.Write(VObjType, SizeOf(AWord));
  S.Write(TCount, SizeOf(AInt));
  S.Write(TLimit, SizeOf(AInt));
  S.Write(TDelta, SizeOf(AInt));
  ForEach(@DoPutItem);
  S.Write(Duplicates, SizeOf(Duplicates));
end;

constructor TCellCollection.ShortLoad(var S: TStream);
var q: AWord;
  C, I: Integer;
  ACount, ALimit, ADelta: AInt;
begin
  S.Read(q, SizeOf(AWord));
  if q<>VObjType then fail;
  S.Read(ACount, SizeOf(AInt));
  S.Read(ALimit, SizeOf(AInt));
  S.Read(ADelta, SizeOf(AInt));
  Inherited Init(ALimit, ADelta);
  SetLimit(ACount);
  for I := 0 to ACount - 1 do AtInsert(I, GetItem(S));
  S.Read(Duplicates, SizeOf(Boolean));
end;

procedure TCellCollection.FreeItem;
begin
 FreeMem(Item, LenDataRec + Length(PCellRec(Item)^.S));
end;

function TCellCollection.MakeFormatString(AValue: Real): String;
 var S: String;
begin
 Str(AValue:20:2, S);
 DelLeft(S);
 MakeFormatString := S;
end;

function TCellCollection.NewCellRec
  (ACol, ARow: AInt; const A_S: String): PCellRec;
begin
 result := MemAlloc(LenDataRec + Length(A_S));
 with result^ do
   begin
   Row := ARow; Col := ACol;
   S := A_S;
   Options := 0;
   Decimals := 0;
   Value := 0;
   end;
end;

function TCellCollection.NewItem
  (ACol, ARow: AInt; const A_S: String): PCellRec;
begin
 result := NewCellRec(ACol, ARow, A_S);
 Insert(result);
end;

procedure TCellCollection.DelItem(Col, Row: Integer);
 var I: Integer;
     P: PCellRec;
begin
 for I := 1 to Count do
  begin
   P := At(I - 1);
   if (P^.Row = Row) and (P^.Col = Col) then
    begin
     AtFree(I - 1); Exit;
    end;
  end;
end;

procedure TCellCollection.SetValue(Col, Row: Integer; AValue: Real);
 var I: Integer;
     P: PCellRec;
begin
 for I := 1 to Count do
  begin
   P := At(I - 1);
   if (P^.Row = Row) and (P^.Col = Col) then
    begin
     P^.Value := AValue;
     Exit;
    end;
  end;
end;

function TCellCollection.ReplaceItem
  (ACol: Byte; ARow: AInt; const A_S: String): PCellRec;
 var I, D, O: Integer;
     SR: TCellSearcRec;
  begin
  SR.Col := ACol; SR.Row := ARow;
  if not Search(@SR, I) then
    begin result := NewCellRec(ACol, ARow, A_S); AtInsert(I, Result); end
  else
    begin
    result := At(I);
    if result^.S <> A_S then
      begin
      O := result^.Options; D := result^.Decimals;
      FreeItem(result);
      result := MemAlloc(LenDataRec + Length(A_S));
      with result^ do
        begin
        Row := ARow; Col := ACol;
        Options := O; Decimals := D; S := A_S;
        AtPut(I, result);
        end;
      end;
    end;
  end;

{ Для совместимости файла wkz со старыми версиями DN, выводится
значение, притом Real, а не CReal (с контролем переполнения).
Для нынешней версии значения в файле не нужны.}

procedure TCellCollection.PutItem;
 var P: PCellRec absolute Item;
 R: Real;
begin
S.Write(P^, LenSavedDataRec-SizeOf(Real));
if P^.Options and (coFormula or coValue) = 0 then
  R := 0
else if abs(P^.Value) < 1.7e38 then
  R := P^.Value
else if P^.Value < 0 then
  R := -1.7e38
else R := 1.7e38;
S.Write(R, SizeOf(R));
S.Write(P^.S, 1+Length(P^.S));
end;

function TCellCollection.GetItem;
 var R: TCellRec;
     l: integer;
begin
 S.Read(R, LenSavedDataRec);
 S.Read(R.S[0], 1);
 S.Read(R.S[1], Length(R.S));
 l := LenDataRec + Length(R.S);
 result := MemAlloc(l);
 Move(R, result^, l);
end;

function TCellCollection.Get(Col: byte; Row: AInt): PCellRec;
 var I: Integer;
     P: PCellRec;
     SR: TCellSearcRec;
begin
 Get := nil;
 SR.Col := Col; SR.Row := Row;
 if Search(@SR, I) then
   Get := At(I);
end;

function TCellCollection.Compare;
 var
   Col1, Col2, Row1, Row2: integer;
begin
 Compare := 0;
 if K1 = nil then Compare := -1
 else if K2 = nil then Compare := 1
 else
   begin
   with PCellRec(K1)^ do begin Col1 := Col; Row1 := Row end;
   with PCellRec(K2)^ do begin Col2 := Col; Row2 := Row end;
   if Row1 < Row2 then Compare := -1
   else if Row1 > Row2 then Compare := 1
   else if Col1 < Col2 then Compare := -1
   else if Col1 > Col2 then Compare := 1
   else Compare := 0;
   end;
end;

const
  delim = ':(),+-*/%'#0 ;
function TCellCollection.TSort(var Start: integer): boolean;
  type
    PSucPool = ^TSucPool;
    TSucPool = array[1..$FFFF] of record suc, next: integer end;
  const
    PoolUnit = 5;
  var
    QLink: PWordArray;
{У Кнута есть еще Count, как синоним QLink, но мы будем писать везде QLink}
    QLinkSize: longint;
    Top: PWordArray; {Top^[0] не используем}
    TopSize: longint;
    SucPool: PSucPool;
    PoolCount: longint;
    P: integer;
    F,R: integer;
    N: integer;
    i,k,j: integer;
    t: integer;
    formula, c1, c2: string;
    op: integer;
    CurCell: PCellRec;
    SR, SR1, SR2: TCellSearcRec;


  function Scan(var sym: string): boolean;
    var
      t0: integer;
    begin
    if formula[t] = #0 then
      begin result := false; exit; end;
    result := true;
    while Pos(formula[t], delim) <> 0 do inc(t);
    t0 := t;
    repeat
      inc(t); op := Pos(formula[t], delim);
    until Op <> 0;
    sym := Copy(formula, t0, t-t0);
    if formula[t] <> #0 then inc(t);
    end;

  procedure RegisterPrev;
    var
      j: integer;
      l: longint;
      NewPool: PSucPool;
    begin
    if Search(@SR, j) then
      begin
      inc(QLink^[k]);
      { P <= Avail }
      inc(P); if P > PoolCount then
        begin
        NewPool := MemAlloc((PoolCount+PoolUnit)*SizeOf(SucPool^[1]));
        l := PoolCount*SizeOf(SucPool^[1]);
        move(SucPool^, NewPool^, l);
        FreeMem(SucPool, l);
        SucPool := NewPool; inc(PoolCount, PoolUnit);
        end;
      with SucPool^[P] do
        begin Suc := k; Next := Top^[j+1]; Top^[j+1] := P; end;
      end;
    end;

  begin {TCellCollection.TSort}
  { Топологическая сортировка. См. Д.Кнут, т.1, 2.3.2.}
  P := 0; { инициализация SucPool}
  N := Count;
  QLinkSize := (N+1)*SizeOf(QLink^[0]);
  QLink := MemAlloc(QLinkSize);
  TopSize := (N+1)*SizeOf(Top^[1]);
  Top := MemAlloc(TopSize);
  PoolCount := PoolUnit;
  SucPool := MemAlloc(PoolCount*SizeOf(SucPool^[1]));
  for k := 1 to N do
    begin
    QLink^[k] := 0; Top^[k] := 0;
    end;
  for k := 1 to N do
    begin
    CurCell := PCellRec(At(k-1));
    with CurCell^ do
      begin
      if (Options and coFormula) <> 0 then
        begin
        formula := s + #0; t := 2;
        while scan(c2) do
          begin
          if Op = 1 then
            begin  {обработка c1:c2}
            c1 := c2; scan(c2);
            if     GetCellCoord(c1, SR1.Col, SR1.Row)
               and GetCellCoord(c2, SR2.Col, SR2.Row)
            then
              for SR.Col := SR1.Col to SR2.Col do
                for SR.Row := SR1.Row to SR2.Row do
                  RegisterPrev;
            scan(c1);
            end
          else
            begin {обработка c2}
            if GetCellCoord(c2, SR.Col, SR.Row) then
              RegisterPrev;
            c1 := c2;
            end;
          end;
        end;
      end;
    end;

  QLink^[0] := 0; R := 0;
  for k := 1 to N do
    if QLink^[k] = 0 then
       begin
       QLink^[R] := k;
       R := k; {в книге ошибочно написано P вместо R}
       end;
  F := QLink^[0];

  Start := F;
  while F <> 0 do
    begin
    CurCell := PCellRec(At(F-1));
    dec(N); P := Top^[F]; Top^[F] := 0;
    while P <> 0 do
      begin
      i := SucPool^[P].Suc;
      dec(QLink^[i]);
      if QLink^[i] = 0 then
        begin QLink^[R] := i; R := i; end;
      P := SucPool^[P].Next;
      end;
    F := QLink^[F];
    CurCell^.NextC := F;
    end;
  result := N=0;
  if not Result then
    begin
    k := 1; while Top^[k] = 0 do inc(k);
    Start := SucPool^[Top^[k]].Suc;
    end;
  FreeMem(Top, TopSize);
  FreeMem(QLink, QLinkSize);
  FreeMem(SucPool, PoolCount*SizeOf(SucPool^[1]));
  end;

 {  То ли в VP ошибка, то ли я чего-то не понимаю, но если переменные,
которые экспортируются из вызывающей программы в программу Action^,
описывать в вызывающей программе обычным образом, то есть как стековые,
то доступ к ним в Action^ иногда получается некорректным. То же самое
относится к ссылкам на поля self, если вызывающая программа - метод.
Возможно, дело в глубине вложенности процедур. Радикальным средством
борьбы с этим глюком является использование в Action^ только
статических переменных. AK155}
procedure TCellCollection.ForRectangle(AX: Byte; AY: AInt;
       BX: Byte; BY: AInt; Action: Pointer);
  type
    TActionProc = procedure (Item: Pointer);
  var
    Y: AInt;
    SR: TCellSearcRec;
    P: PCellRec;
    I: integer;
    ActionProc: TActionProc;
  begin
  @ActionProc := Action; SR.Col := AX;
  for Y := AY to BY do
    begin
    SR.Row := Y;
    Search(@SR, I);
    while true do
      begin
      if (I = Count) then
        exit;
      P := PCellRec(At(I));
      if (P^.Row <> Y) or (P^.Col > BX) then break;
      ActionProc(P);
      inc(I);
      end;
    end;
  end;

{-----------------------------------------------------------------------}

function GetCellCoord(S: String; var X: Byte;var Y:AInt): Boolean;
 var
     I,J, ierr: Integer;
begin
 result := false;
 J := 1; if S[1] = '@' then Inc(J);
 if (length(S) <= J) then
   exit;
 UpStr(S);
 I := J;
 While (I <= Length(S)) and (S[I]>='A') and (S[I]<='Z') do Inc(I);

 {код колонки - с j по i исключительно}
 if I = J + 1 then
   X := Byte(S[J]) - byte('A')
 else if I = J + 2 then
   X := (Byte(S[J]) - (byte('A')-1)) * 26 + Byte(S[J+1]) - byte('A')
 else
   exit; {допускается только 1 или 2 буквы}

 if S[I] = '@' then Inc(I);
 Val(Copy(S, I, 255), Y, ierr);
 if (ierr <> 0) or (Y<=0) or (Y>MaxCellY) then
   exit;
 Dec(Y);
 result := true;
end;

function GetColName(X: Integer): ShortString;
  begin
  Result := 'AA';
  if X < 26 then
    begin dec(result[0]); inc(result[1], X); end
  else
    begin inc(result[1], X div 26 - 1); inc(result[2], X mod 26); end
  end;

function GetRowName(Y: Integer): String;
begin
  Str(Y + 1, result);
end;

function GetCellName(X, Y: Integer): String;
begin
  result := GetColName(X) + GetRowName(Y);
end;

function MakeComma(S: String): String;
 var I: Integer;
  K: Integer;
begin
 I := PosChar('.', S);
 if I = 0 then I := Length(S)+1;
 K := 3;
 while (I - K) > 1 do
 begin
   insert(CommaChar, S, I - K);
   Inc(K, 3);
 end;
 MakeComma := S;
end;

end.
