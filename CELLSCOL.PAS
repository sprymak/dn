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
  PCellrec = ^TCellRec;
  TCellRec = record
    Col: byte;
    Row: AInt;
    Options: AWord;
    Decimals: byte;
    Value: CReal;
    SucTop: AInt;
    NextC: AInt;
    s: String;
    end;

  TCellSearcRec = record
    Col: byte;
    Row: AInt;
    end;
  PCellCollection = ^TCellCollection;
  TCellCollection = object(TSortedCollection)
    Constructor ShortLoad(var s: TStream);
    procedure ShortStore(var s: TStream);
    procedure FreeItem(Item: Pointer); virtual;
    function NewCellRec(ACol, ARow: AInt; const A_S: String):
      PCellrec;
    { создать запись }
    function NewItem(ACol, ARow: AInt; const A_S: String): PCellrec;
    { создать запись и поместить ее в коллекцию }
    function ReplaceItem
    (ACol: byte; ARow: AInt; const A_S: String): PCellrec;
    function Get(Col: byte; Row: AInt): PCellrec;
    procedure SetValue(Col, Row: integer; AValue: real);
    procedure DelItem(Col, Row: integer);
    procedure PutItem(var s: TStream; Item: Pointer); virtual;
    function GetItem(var s: TStream): Pointer; virtual;
    function MakeFormatString(AValue: real): String;
    function Compare(K1, k2: Pointer): integer; virtual;
    function TSort(var Start: integer): boolean; {AK155}
    procedure ForRectangle(AX: byte; AY: AInt; {AK155}
    BX: byte; BY: AInt; Action: Pointer); {см. комментарий к телу!}
    end;

const
  MaxCellY = 4096;

  LenSavedDataRec = SizeOf(byte)+SizeOf(AInt)+SizeOf(AWord)+
  SizeOf(byte)+SizeOf(real);

  LenDataRec = SizeOf(TCellRec)-SizeOf(String)+1;

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
  coText = $0000;
  coFormula = $0001;
  coValue = $0002;
  coTypeMask = 3;

  coRight = $0004;
  coCenter = $0008; {Left is (Options and 3 = 0)}
  coDec = $0010;
  coComma = $0020;
  coExp = $0030;
  coBool = $0040;
  coCurrency = $0050;
  coDONT = $0060;

  CommaChar: Char = ',';
  SeparatorChar: Char = '|';

function MakeComma(s: String): String;
function GetCellCoord(s: String; var X: byte; var Y: AInt): boolean;
function GetColName(X: integer): ShortString;
function GetRowName(Y: integer): String;
function GetCellName(X, Y: integer): String;

implementation

uses
  Memory, advance, advance1, advance2, advance3, ObjType;

const
  VObjType: AWord = otCellCollection;

procedure TCellCollection.ShortStore(var s: TStream);
  var
    TCount, TLimit, TDelta: AInt;
  procedure DoPutItem(P: Pointer);
    begin
      PutItem(s, P);
    end;
  begin
    TCount := Count;
    TLimit := Limit;
    TDelta := Delta;
    s.Write(VObjType, SizeOf(AWord));
    s.Write(TCount, SizeOf(AInt));
    s.Write(TLimit, SizeOf(AInt));
    s.Write(TDelta, SizeOf(AInt));
    ForEach(@DoPutItem);
    s.Write(Duplicates, SizeOf(Duplicates));
  end;

Constructor TCellCollection.ShortLoad(var s: TStream);
  var
    Q: AWord;
    C, i: integer;
    ACount, ALimit, ADelta: AInt;
  begin
    s.Read(Q, SizeOf(AWord));
    if Q <> VObjType then
      Fail;
    s.Read(ACount, SizeOf(AInt));
    s.Read(ALimit, SizeOf(AInt));
    s.Read(ADelta, SizeOf(AInt));
    inherited Init(ALimit, ADelta);
    SetLimit(ACount);
    for i := 0 to ACount-1 do
      AtInsert(i, GetItem(s));
    s.Read(Duplicates, SizeOf(boolean));
  end;

procedure TCellCollection.FreeItem;
  begin
    FreeMem(Item, LenDataRec+Length(PCellrec(Item)^.s));
  end;

function TCellCollection.MakeFormatString(AValue: real): String;
  var
    s: String;
  begin
    Str(AValue: 20: 2, s);
    DelLeft(s);
    MakeFormatString := s;
  end;

function TCellCollection.NewCellRec
  (ACol, ARow: AInt; const A_S: String): PCellrec;
  begin
    Result := MemAlloc(LenDataRec+Length(A_S));
    with Result^ do
      begin
        Row := ARow;
        Col := ACol;
        s := A_S;
        Options := 0;
        Decimals := 0;
        Value := 0;
      end;
  end;

function TCellCollection.NewItem
  (ACol, ARow: AInt; const A_S: String): PCellrec;
  begin
    Result := NewCellRec(ACol, ARow, A_S);
    Insert(Result);
  end;

procedure TCellCollection.DelItem(Col, Row: integer);
  var
    i: integer;
    P: PCellrec;
  begin
    for i := 1 to Count do
      begin
        P := At(i-1);
        if (P^.Row = Row) and (P^.Col = Col) then
          begin
            AtFree(i-1);
            exit;
          end;
      end;
  end;

procedure TCellCollection.SetValue(Col, Row: integer; AValue: real);
  var
    i: integer;
    P: PCellrec;
  begin
    for i := 1 to Count do
      begin
        P := At(i-1);
        if (P^.Row = Row) and (P^.Col = Col) then
          begin
            P^.Value := AValue;
            exit;
          end;
      end;
  end;

function TCellCollection.ReplaceItem
  (ACol: byte; ARow: AInt; const A_S: String): PCellrec;
  var
    i, D, o: integer;
    SR: TCellSearcRec;
  begin
    SR.Col := ACol;
    SR.Row := ARow;
    if not Search(@SR, i) then
      begin
        Result := NewCellRec(ACol, ARow, A_S);
        AtInsert(i, Result);
      end
    else
      begin
        Result := At(i);
        if Result^.s <> A_S then
          begin
            o := Result^.Options;
            D := Result^.Decimals;
            FreeItem(Result);
            Result := MemAlloc(LenDataRec+Length(A_S));
            with Result^ do
              begin
                Row := ARow;
                Col := ACol;
                Options := o;
                Decimals := D;
                s := A_S;
                AtPut(i, Result);
              end;
          end;
      end;
  end { TCellCollection.ReplaceItem };

{ Для совместимости файла wkz со старыми версиями DN, выводится
значение, притом Real, а не CReal (с контролем переполнения).
Для нынешней версии значения в файле не нужны.}

procedure TCellCollection.PutItem;
  var
    P: PCellrec absolute Item;
    R: real;
  begin
    s.Write(P^, LenSavedDataRec-SizeOf(real));
    if P^.Options and (coFormula or coValue) = 0 then
      R := 0
    else if Abs(P^.Value) < 1.7e38 then
      R := P^.Value
    else if P^.Value < 0 then
      R := -1.7e38
    else
      R := 1.7e38;
    s.Write(R, SizeOf(R));
    s.Write(P^.s, 1+Length(P^.s));
  end;

function TCellCollection.GetItem;
  var
    R: TCellRec;
    l: integer;
  begin
    s.Read(R, LenSavedDataRec);
    s.Read(R.s[0], 1);
    s.Read(R.s[1], Length(R.s));
    l := LenDataRec+Length(R.s);
    Result := MemAlloc(l);
    Move(R, Result^, l);
  end;

function TCellCollection.Get(Col: byte; Row: AInt): PCellrec;
  var
    i: integer;
    P: PCellrec;
    SR: TCellSearcRec;
  begin
    Get := nil;
    SR.Col := Col;
    SR.Row := Row;
    if Search(@SR, i) then
      Get := At(i);
  end;

function TCellCollection.Compare;
  var
    Col1, Col2, Row1, Row2: integer;
  begin
    Compare := 0;
    if K1 = nil then
      Compare := -1
    else if k2 = nil then
      Compare := 1
    else
      begin
        with PCellrec(K1)^ do
          begin
            Col1 := Col;
            Row1 := Row
          end;
        with PCellrec(k2)^ do
          begin
            Col2 := Col;
            Row2 := Row
          end;
        if Row1 < Row2 then
          Compare := -1
        else if Row1 > Row2 then
          Compare := 1
        else if Col1 < Col2 then
          Compare := -1
        else if Col1 > Col2 then
          Compare := 1
        else
          Compare := 0;
      end;
  end { TCellCollection.Compare };

const
  Delim = ':(),+-*/%'#0;
function TCellCollection.TSort(var Start: integer): boolean;
  type
    PSucPool = ^TSucPool;
    TSucPool = array[1..$FFFF] of record
      suc, Next: integer
      end;
  const
    PoolUnit = 5;
  var
    QLink: PWordArray;
    {У Кнута есть еще Count, как синоним QLink, но мы будем писать везде QLink}
    QLinkSize: longInt;
    Top: PWordArray; {Top^[0] не используем}
    TopSize: longInt;
    SucPool: PSucPool;
    PoolCount: longInt;
    P: integer;
    F, R: integer;
    n: integer;
    i, k, j: integer;
    t: integer;
    formula, C1, C2: String;
    Op: integer;
    CurCell: PCellrec;
    SR, SR1, SR2: TCellSearcRec;

  function Scan(var sym: String): boolean;
    var
      t0: integer;
    begin
      if formula[t] = #0 then
        begin
          Result := False;
          exit;
        end;
      Result := True;
      while Pos(formula[t], Delim) <> 0 do
        Inc(t);
      t0 := t;
      repeat
        Inc(t);
        Op := Pos(formula[t], Delim);
      until Op <> 0;
      sym := Copy(formula, t0, t-t0);
      if formula[t] <> #0 then
        Inc(t);
    end { Scan };

  procedure RegisterPrev;
    var
      j: integer;
      l: longInt;
      NewPool: PSucPool;
    begin
      if Search(@SR, j) then
        begin
          Inc(QLink^[k]);
          { P <= Avail }
          Inc(P);
          if P > PoolCount then
            begin
              NewPool := MemAlloc((PoolCount+PoolUnit)*SizeOf(
                SucPool^[1]));
              l := PoolCount*SizeOf(SucPool^[1]);
              Move(SucPool^, NewPool^, l);
              FreeMem(SucPool, l);
              SucPool := NewPool;
              Inc(PoolCount, PoolUnit);
            end;
          with SucPool^[P] do
            begin
              suc := k;
              Next := Top^[j+1];
              Top^[j+1] := P;
            end;
        end;
    end { RegisterPrev };

  begin{TCellCollection.TSort}
    { Топологическая сортировка. См. Д.Кнут, т.1, 2.3.2.}
    P := 0; { инициализация SucPool}
    n := Count;
    QLinkSize := (n+1)*SizeOf(QLink^[0]);
    QLink := MemAlloc(QLinkSize);
    TopSize := (n+1)*SizeOf(Top^[1]);
    Top := MemAlloc(TopSize);
    PoolCount := PoolUnit;
    SucPool := MemAlloc(PoolCount*SizeOf(SucPool^[1]));
    for k := 1 to n do
      begin
        QLink^[k] := 0;
        Top^[k] := 0;
      end;
    for k := 1 to n do
      begin
        CurCell := PCellrec(At(k-1));
        with CurCell^ do
          begin
            if (Options and coFormula) <> 0 then
              begin
                formula := s+#0;
                t := 2;
                while Scan(C2) do
                  begin
                    if Op = 1 then
                      begin{обработка c1:c2}
                        C1 := C2;
                        Scan(C2);
                        if GetCellCoord(C1, SR1.Col, SR1.Row)
                          and GetCellCoord(C2, SR2.Col, SR2.Row)
                        then
                          for SR.Col := SR1.Col to SR2.Col do
                            for SR.Row := SR1.Row to SR2.Row do
                              RegisterPrev;
                        Scan(C1);
                      end
                    else
                      begin{обработка c2}
                        if GetCellCoord(C2, SR.Col, SR.Row) then
                          RegisterPrev;
                        C1 := C2;
                      end;
                  end;
              end;
          end;
      end;

    QLink^[0] := 0;
    R := 0;
    for k := 1 to n do
      if QLink^[k] = 0 then
        begin
          QLink^[R] := k;
          R := k; {в книге ошибочно написано P вместо R}
        end;
    F := QLink^[0];

    Start := F;
    while F <> 0 do
      begin
        CurCell := PCellrec(At(F-1));
        Dec(n);
        P := Top^[F];
        Top^[F] := 0;
        while P <> 0 do
          begin
            i := SucPool^[P].suc;
            Dec(QLink^[i]);
            if QLink^[i] = 0 then
              begin
                QLink^[R] := i;
                R := i;
              end;
            P := SucPool^[P].Next;
          end;
        F := QLink^[F];
        CurCell^.NextC := F;
      end;
    Result := n = 0;
    if not Result then
      begin
        k := 1;
        while Top^[k] = 0 do
          Inc(k);
        Start := SucPool^[Top^[k]].suc;
      end;
    FreeMem(Top, TopSize);
    FreeMem(QLink, QLinkSize);
    FreeMem(SucPool, PoolCount*SizeOf(SucPool^[1]));
  end { TCellCollection.TSort };

{  То ли в VP ошибка, то ли я чего-то не понимаю, но если переменные,
которые экспортируются из вызывающей программы в программу Action^,
описывать в вызывающей программе обычным образом, то есть как стековые,
то доступ к ним в Action^ иногда получается некорректным. То же самое
относится к ссылкам на поля self, если вызывающая программа - метод.
Возможно, дело в глубине вложенности процедур. Радикальным средством
борьбы с этим глюком является использование в Action^ только
статических переменных. AK155}
procedure TCellCollection.ForRectangle(AX: byte; AY: AInt;
  BX: byte; BY: AInt; Action: Pointer);
  type
    TActionProc = procedure (Item: Pointer);
  var
    Y: AInt;
    SR: TCellSearcRec;
    P: PCellrec;
    i: integer;
    ActionProc: TActionProc;
  begin
    @ActionProc := Action;
    SR.Col := AX;
    for Y := AY to BY do
      begin
        SR.Row := Y;
        Search(@SR, i);
        while True do
          begin
            if (i = Count) then
              exit;
            P := PCellrec(At(i));
            if (P^.Row <> Y) or (P^.Col > BX) then
              break;
            ActionProc(P);
            Inc(i);
          end;
      end;
  end { TCellCollection.ForRectangle };

{-----------------------------------------------------------------------}

function GetCellCoord(s: String; var X: byte; var Y: AInt): boolean;
  var
    i, j, ierr: integer;
  begin
    Result := False;
    j := 1;
    if s[1] = '@' then
      Inc(j);
    if (Length(s) <= j) then
      exit;
    UpStr(s);
    i := j;
    while (i <= Length(s)) and (s[i] >= 'A') and (s[i] <= 'Z') do
      Inc(i);

    {код колонки - с j по i исключительно}
    if i = j+1 then
      X := byte(s[j])-byte('A')
    else if i = j+2 then
      X := (byte(s[j])-(byte('A')-1))*26+byte(s[j+1])-byte('A')
    else
      exit; {допускается только 1 или 2 буквы}

    if s[i] = '@' then
      Inc(i);
    Val(Copy(s, i, 255), Y, ierr);
    if (ierr <> 0) or (Y <= 0) or (Y > MaxCellY) then
      exit;
    Dec(Y);
    Result := True;
  end { GetCellCoord };

function GetColName(X: integer): ShortString;
  begin
    Result := 'AA';
    if X < 26 then
      begin
        Dec(Result[0]);
        Inc(Result[1], X);
      end
    else
      begin
        Inc(Result[1], X div 26-1);
        Inc(Result[2], X mod 26);
      end
  end;

function GetRowName(Y: integer): String;
  begin
    Str(Y+1, Result);
  end;

function GetCellName(X, Y: integer): String;
  begin
    Result := GetColName(X)+GetRowName(Y);
  end;

function MakeComma(s: String): String;
  var
    i: integer;
    k: integer;
  begin
    i := PosChar('.', s);
    if i = 0 then
      i := Length(s)+1;
    k := 3;
    while (i-k) > 1 do
      begin
        Insert(CommaChar, s, i-k);
        Inc(k, 3);
      end;
    MakeComma := s;
  end;

end.
