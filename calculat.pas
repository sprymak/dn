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
{(c) Alexey Korop (AK155), 2002}
unit Calculat;
{&Delphi+}

interface
uses
  Commands;

type
  CReal = Extended;
  PCReal = ^CReal;

function Evalue(const s: String; CCV: Pointer): CReal;
{Параметр CCV равен nil для простого вычислителя. При вызове
из электронной таблицы - это PCalcView. В этом случае
к функциям добавлятся SUM и MUL, а к операндам
добавляются имена ячеек }

function GetErrOp(var X: integer): String;

var
  EvalueError: boolean;
  CalcErrMess: TStrIdx;
  CalcErrPos: longInt;
  Res: CReal;
  CalcSym: String;

implementation

uses
  advance1, advance
  , math, sysutils, Calc
  ;

var
  Expression: String;
  CurCalcView: PCalcView;

  { Число в указанной системе счичления }
function GetV(s: String; Base: integer; var Value: CReal): boolean;
  const
    HexDigits = '0123456789ABCDEFG';
  var
    RR, FractValue: CReal;
    j: byte;
    C, MaxC: Char;
    D: longInt;
  begin
    Result := False;
    if Length(s) = 0 then
      exit;
    RR := 0;
    UpStr(s);
    {целая часть}
    for j := 1 to Length(s) do
      begin
        C := System.UpCase(s[j]);
        if C = '.' then
          break;
        D := Pos(C, HexDigits)-1;
        if (D < 0) or (D >= Base) then
          exit;
        RR := RR*Base+D;
      end;
    Delete(s, 1, j);
    if (Length(s) < 1) then
      begin
        Value := RR;
        Result := True;
        exit
      end;
    {дробная часть}
    FractValue := 1;
    for j := 1 to Length(s) do
      begin
        FractValue := FractValue/Base;
        D := Pos(s[j], HexDigits)-1;
        if (D < 0) or (D >= Base) then
          exit;
        RR := RR+D*FractValue;
      end;
    Value := RR;
    Result := True;
  end { GetV };

{ После десятичного числа без пробела может быть десятичный множитель
(всякие кило- микро- и т.п.). Множители распознаются международные
(u=микро) и русские. Множитель 'гекто' отсутствует, так как его
международное обозначение 'h' конфликтует с ассемблерной формой
шестнадцатеричного числа }
function GetDec(s: String; var Value: CReal): boolean;
  const
    MultE = ' d  с  m  u  p  n  f  da k  M  G  T  P ';
    MultR = ' д  с  м  мк п  н  ф  да к  М  Г  Т  П ';
    //            -1 -2 -3 -6 -9-12-15 1  3  6  9  12 15
    MultV: array[1..Length(MultE) div 3] of CReal =
    (1e-1, 1e-2, 1e-3, 1e-6, 1e-9, 1e-12, 1e-15
    , 1e1, 1e3, 1e6, 1e9, 1e12, 1e15);
  var
    R: integer;
    l: longInt;
    M: String;
  begin
    Val(s, Value, R);
    if (R <> 0) then
      begin
        M := ' '+Copy(s, R, 255)+' ';
        l := Pos(M, MultE);
        if l = 0 then
          l := Pos(M, MultR);
        if l <> 0 then
          begin
            SetLength(s, R-1);
            Val(s, Value, R);
            Value := Value*MultV[(l+2) div 3];
          end;
      end;
    Result := R = 0;
  end { GetDec };

function GetValue(s: String; var Value: CReal): boolean;
  var
    BaseChar: Char;
  begin
    BaseChar := UpCase(s[Length(s)]);
    if (BaseChar = 'H') then
      begin
        SetLength(s, Length(s)-1);
        GetValue := GetV(s, 16, Value);
      end
    else if s[1] = '$' then
      begin
        Delete(s, 1, 1); {DelFC(S);}
        GetValue := GetV(s, 16, Value)
      end
    else if (UpStrg(Copy(s, 1, 2)) = '0X') then
      GetValue := GetV(Copy(s, 3, 255), 16, Value)
    else if (BaseChar in ['O', 'Q']) then
      begin
        SetLength(s, Length(s)-1);
        GetValue := GetV(s, 8, Value)
      end
    else if (BaseChar = 'B') then
      begin
        SetLength(s, Length(s)-1);
        GetValue := GetV(s, 2, Value)
      end
    else
      GetValue := GetDec(s, Value);
  end { GetValue };

var
  Operand: CReal;
  SymType, PrevSymType: integer;
  CurrOp: integer; { текущая операция (для ошибок)}
  t: byte; { индекс необработанного символа строки выражения }

const
  delimiters: String[50] = '()*/+-=^<>:#,!|\&%~ '#3;
  { Лексические разделители. }

  MaxOp = 127;
  prio1: String[MaxOp] = '19244556733323444553333559';
    { приоритеты в тексте}
  prio2: String[MaxOp] = '00 44556733323444553333558';
    { приоритеты в стеке }
  OpChars = #3'()+-*/^:=<>,#|\&%#4<<<<<<~'#1#2;
  { #1 представляет унарный минус, #2 - унарный плюс,
      #3 - конец выражения; Повторные '<' - это двухсимвольные оп.
      #4 - DIV (это единственная бинарная операция, имеющая
      только текстовое изображение. То, что его номер непосредственно
      предшествует номерам двухсимвольных операций, ниже используется.
      Если понадобится добавлять ещё операции с только текстовыми
      обозначениями, их вставлять вслед за DIV, коррекутируя Op2Base
      и не забывая вставить символ в строки приоритетов.)
  }

  Op2Chars = '<>!';
  Op2 = '>='#0'<='#0'<>'#0'!='#0'<<'#0'>>';
    {двухсимвольные операции}
  OpDiv = 19;
  Op2Base = OpDiv + 1;
  LetterBinOp = ' DIV OR  XOR AND MOD SHL SHR ';
  LetterBinOpType: array[1..7] of integer =
  (OpDiv, 15, 16, 17, 18, Op2Base+4, Op2Base+5);

  FnBase = Length(OpChars)+1;

type
  FnEval = procedure (var D: CReal);
  FnEval2 = procedure (var D: CReal; d2: CReal);
  FnEval3 = procedure (var D: CReal; d2, d3: CReal);
  PFnDesc = ^TFnDesc;
  TFnDesc = object
    n {ame}: String[8];
    E {val}: Pointer {FnEval};
    A {rguments}: integer;
    end;

procedure SinEv(var D: CReal);
  begin
    D := sin(D);
  end;

procedure CosEv(var D: CReal);
  begin
    D := cos(D);
  end;

procedure TgEv(var D: CReal);
  begin
    D := Tan(D);
  end;

procedure CtgEv(var D: CReal);
  begin
    D := Cotan(D);
  end;

procedure CosecEv(var D: CReal);
  begin
    D := 1/sin(D);
  end;

procedure SecEv(var D: CReal);
  begin
    D := 1/cos(D);
  end;

procedure ArcSinEv(var D: CReal);
  begin
    D := ArcSin(D);
  end;

procedure ArcCosEv(var D: CReal);
  begin
    D := ArcCos(D);
  end;

procedure ArcSecEv(var D: CReal);
  begin
    D := ArcCos(1/D);
  end;

procedure ArcCoSecEv(var D: CReal);
  begin
    D := ArcSin(1/D);
  end;

procedure ArcTanEv(var D: CReal);
  begin
    D := ArcTan(D);
  end;

procedure ArcCoTanEv(var D: CReal);
  begin
    D := PI/2-ArcTan(D);
  end;

procedure LnEv(var D: CReal);
  begin
    D := ln(D);
  end;

procedure LgEv(var D: CReal);
  begin
    D := ln(D)/ln(10);
  end;

procedure ExpEv(var D: CReal);
  begin
    D := Exp(D);
  end;

procedure SqrEv(var D: CReal);
  begin
    D := D*D;
  end;

procedure SqrtEv(var D: CReal);
  begin
    D := Sqrt(D);
  end;

procedure SinhEv(var D: CReal);
  begin
    D := Sinh(D);
  end;

procedure CoshEv(var D: CReal);
  begin
    D := Cosh(D);
  end;

procedure TanhEv(var D: CReal);
  begin
    D := Tanh(D);
  end;

procedure CotanhEv(var D: CReal);
  begin
    D := 1/Tanh(D);
  end;

procedure ArcSinhEv(var D: CReal);
  begin
    D := ArcSinh(D);
  end;

procedure ArcCoshEv(var D: CReal);
  begin
    D := ArcCosh(D);
  end;

procedure ArcTanhEv(var D: CReal);
  begin
    D := ln((1+D)/(1-D))/2;
  end;

procedure SignEv(var D: CReal);
  begin
    if D < 0 then
      D := -1
    else if D > 0 then
      D := 1
  end;

procedure NotEv(var D: CReal);
  begin
    D := not Round(D);
  end;

procedure AbsEv(var D: CReal);
  begin
    D := Abs(D);
  end;

procedure RadEv(var D: CReal);
  begin
    D := (D*PI)/180;
  end;

procedure RadGEv(var D: CReal);
  begin
    D := (D*PI)/200;
  end;

procedure DegEv(var D: CReal);
  begin
    D := (D*180)/PI;
  end;

procedure GradEv(var D: CReal);
  begin
    D := (D*200)/PI;
  end;

procedure RoundEv(var D: CReal);
  begin
    D := Round(D);
  end;

procedure FactEv(var D: CReal);
  var
    i: integer;
    R: CReal;
  begin
    R := 1;
    for i := 1 to Trunc(D) do
      R := R*i;
    D := R;
  end;

procedure PiEv(var D: CReal);
  begin
    D := PI;
  end;

procedure LogEv(var D: CReal; d2: CReal);
  begin
    D := logN(D, d2);
  end;

procedure RootEv(var D: CReal; d2: CReal);
  begin
    D := Exp(ln(d2)/D);
  end;

procedure IfEv(var D: CReal; d2, d3: CReal);
  begin
    if D <> 0 then
      D := d2
    else
      D := d3;
  end;

const
  FnMax = 52;
  CalcBase = FnBase+FnMax; { довески от электронной таблицы }
  FnTab: array[0..FnMax-1] of TFnDesc =
  (
  (n: 'SIN'; E: @SinEv; A: 1)
  , (n: 'COS'; E: @CosEv; A: 1)
  , (n: 'TG'; E: @TgEv; A: 1)
  , (n: 'TAN'; E: @TgEv; A: 1)
  , (n: 'CTG'; E: @CtgEv; A: 1)
  , (n: 'COTAN'; E: @CtgEv; A: 1)
  , (n: 'SEC'; E: @SecEv; A: 1)
  , (n: 'COSEC'; E: @CosecEv; A: 1)
  , (n: 'ASIN'; E: @ArcSinEv; A: 1)
  , (n: 'ARCSIN'; E: @ArcSinEv; A: 1)
  , (n: 'ACOS'; E: @ArcCosEv; A: 1)
  , (n: 'ARCCOS'; E: @ArcCosEv; A: 1)
  , (n: 'ARCSEC'; E: @ArcSecEv; A: 1)
  , (n: 'ARCCOSEC'; E: @ArcCosecEv; A: 1)
  , (n: 'ATAN'; E: @ArcTanEv; A: 1)
  , (n: 'ARCTAN'; E: @ArcTanEv; A: 1)
  , (n: 'ACTG'; E: @ArcCoTanEv; A: 1)
  , (n: 'ARCCOTAN'; E: @ArcCoTanEv; A: 1)
  , (n: 'LN'; E: @LnEv; A: 1)
  , (n: 'LG'; E: @LgEv; A: 1)
  , (n: 'EXP'; E: @ExpEv; A: 1)
  , (n: 'SQR'; E: @SqrEv; A: 1)
  , (n: 'SQRT'; E: @SqrtEv; A: 1)
  , (n: 'SH'; E: @SinhEv; A: 1)
  , (n: 'SINH'; E: @SinhEv; A: 1)
  , (n: 'CH'; E: @CoshEv; A: 1)
  , (n: 'COSH'; E: @CoshEv; A: 1)
  , (n: 'TH'; E: @TanhEv; A: 1)
  , (n: 'TANH'; E: @TanhEv; A: 1)
  , (n: 'CTH'; E: @CotanhEv; A: 1)
  , (n: 'COTANH'; E: @CotanhEv; A: 1)
  , (n: 'ARCSINH'; E: @ArcSinhEv; A: 1)
  , (n: 'ASH'; E: @ArcSinEv; A: 1)
  , (n: 'ARCCOSH'; E: @ArcCoshEv; A: 1)
  , (n: 'ACOSH'; E: @ArcCosEv; A: 1)
  , (n: 'ACH'; E: @ArcCosEv; A: 1)
  , (n: 'ATH'; E: @ArcTanhEv; A: 1)
  , (n: 'ARTH'; E: @ArcTanhEv; A: 1)
  , (n: 'ARCTANH'; E: @ArcTanhEv; A: 1)
  , (n: 'FACT'; E: @FactEv; A: 1)
  , (n: 'SIGN'; E: @SignEv; A: 1)
  , (n: 'NOT'; E: @NotEv; A: 1)
  , (n: 'ABS'; E: @AbsEv; A: 1)
  , (n: 'RAD'; E: @RadEv; A: 1)
  , (n: 'RADG'; E: @RadGEv; A: 1)
  , (n: 'DEG'; E: @DegEv; A: 1)
  , (n: 'GRAD'; E: @GradEv; A: 1)
  , (n: 'ROUND'; E: @RoundEv; A: 1)
  , (n: 'PI'; E: @PiEv; A: 0)
  , (n: 'LOG'; E: @LogEv; A: 2)
  , (n: 'ROOT'; E: @RootEv; A: 2)
  , (n: 'IF'; E: @IfEv; A: 3)
  );

procedure ScanSym;
  var
    t0: integer;
    i: integer;
    C: Char;
  begin
    while Expression[t] = ' ' do
      Inc(t);
    t0 := t;
    while Pos(Expression[t], delimiters) = 0 do
      Inc(t);
    SymType := 0;
    C := Expression[t0];
    if t = t0 then
      begin{операция - разделитель}
        Inc(t);
        if Pos(C, Op2Chars) <> 0 then
          begin
            CalcSym := Copy(Expression, t0, 2);
            i := Pos(CalcSym, Op2);
            if i <> 0 then
              begin
                Inc(t);
                SymType := Op2Base+i div 3;
                exit;
              end
          end;
        CalcSym := C;
        SymType := Pos(CalcSym, OpChars);
      end
    else if (C = '$') or ((C >= '0') and (C <= '9')) then
      begin{число}
        if (C <> '$') and (System.UpCase(Expression[t-1]) = 'E')
          and (Expression[t] = '-')
        then
          begin{похоже на число вида 5e-3}
            repeat
              Inc(t)
            until Pos(Expression[t], delimiters) <> 0;
          end;
        CalcSym := {UpStrg(}Copy(Expression, t0, t-t0) {)};
      end
    else
      begin{должна быть функция или буквенная операция вроде AND}
        CalcSym := UpStrg(Copy(Expression, t0, t-t0));
        i := Pos(' '+CalcSym+' ', LetterBinOp);
        if i <> 0 then
          begin
            SymType := LetterBinOpType[(3+i) div 4];
            exit;
          end;

        for i := 0 to FnMax-1 do
          begin
            if CalcSym = FnTab[i].n then
              begin
                SymType := FnBase+i;
                exit;
              end;
          end;
        if CurCalcView <> nil then
          with PCalcView(CurCalcView)^ do
            begin
              if Expression[t] = '(' then
                begin{ что-то вроде sum(a1:a30) в wkz}
                  for i := t+1 to Length(Expression) do
                    if Expression[i] = ')' then
                      begin
                        if GetFuncValue(CalcSym+System.Copy(
                            Expression, t, i-t+1))
                        then
                          begin
                            t := i+1;
                            SymType := CalcBase;
                            exit
                          end
                        else
                          break;
                      end;
                end
              else if GetCellValue(CalcSym) then
                begin
                  SymType := CalcBase;
                  exit
                end;
            end;
      end;
  end { ScanSym };

procedure SetError(Id: TStrIdx);
  begin
    CalcErrMess := Id;
    CalcErrPos := t-2;
    raise eMathError.Create('');
  end;

{ Классический алгоритм с двумя стеками и двумя приоритетами }
var
  DataStack: array[1..20] of CReal;
  TDS: integer; { указатель вершины }
  OpStack: array[1..20] of
  record
    Infix: boolean;
    Op, PrefixCount, Pos: integer
    end;
  TOS: integer; { указатель вершины }
  i: integer;

procedure RegisterOperand; forward;

procedure EvalPrefixOp;
  begin
    CurrOp := OpStack[TOS].Op;
    CalcErrPos := OpStack[TOS].Pos;
    case CurrOp of
      FnBase-3:{ ~ }
        DataStack[TDS] := not Round(DataStack[TDS]);
      FnBase-2:{Унарный плюс}
        begin
        end;
      FnBase-1:{Унарный минус}
        DataStack[TDS] := -DataStack[TDS];
      else
        with FnTab[CurrOp-FnBase] do
          begin
            if TDS < A then
              SetError(dlNoOperand);
            case A of
              {     0: не бывает }
              1:
                FnEval(E)(DataStack[TDS]);
              2:
                begin
                  FnEval2(E)(DataStack[TDS-1], DataStack[TDS]);
                  Dec(TDS);
                end;
              3:
                begin
                  FnEval3(E)(DataStack[TDS-2], DataStack[TDS-1],
                    DataStack[TDS]);
                  Dec(TDS, 2);
                end;
            end {case};
          end;
    end {case};
    Dec(TOS);
    CurrOp := 0;
    RegisterOperand;
  end { EvalPrefixOp };

procedure RegisterOperand;
  begin
    with OpStack[TOS] do
      begin
        Dec(PrefixCount);
        if PrefixCount < 0 then
          SetError(dlMissingOperation);
        if not Infix and (PrefixCount = 0) then
          EvalPrefixOp;
      end;
    SymType := 0;
  end;

procedure ToInt; {возведение в небольшую целую степень }
  var
    n: integer;
    R: CReal;
    neg: boolean;
  begin
    n := Round(DataStack[TDS]);
    R := DataStack[TDS-1];
    DataStack[TDS-1] := 1;
    neg := n < 0;
    if neg then
      begin
        n := -n;
        R := 1/R;
      end;
    while n <> 0 do
      begin
        if odd(n) then
          DataStack[TDS-1] := DataStack[TDS-1]*R;
        R := R*R;
        n := n shr 1;
      end;
  end { ToInt };

procedure EvalText;

  label ExprEnd;

  begin
    t := 1;
    Expression := Expression+#3;

    TOS := 1;
    with OpStack[1] do
      begin
        Infix := True;
        Op := 1;
        PrefixCount := 1;
      end;

    SymType := 1;
    TDS := 0;

    while True do
      begin
        PrevSymType := SymType;
        ScanSym;
        if SymType = 0 then
          begin{ разобрать операнд и положить на стек данных }
            Inc(TDS);
            if not GetValue(CalcSym, DataStack[TDS]) then
              SetError(dlWrongText);
            RegisterOperand;
          end
        else if (SymType = CalcBase) then
          begin{положить на стек значение переменной}
            Inc(TDS);
            DataStack[TDS] := Res;
            RegisterOperand;
          end
        else if (SymType >= FnBase) and (FnTab[SymType-FnBase].A = 0)
        then
          begin
            Inc(TDS);
            FnEval(FnTab[SymType-FnBase].E)(DataStack[TDS]);
            RegisterOperand;
          end
        else
          begin{ разобраться с операцией }
            if (OpStack[TOS].PrefixCount > 0) then
                { ожидается источник значения }
              case SymType of
                2:{ выражение в скобках годится }
                  ;
                4, 5:{ превращаем в унарные плюс и минус }
                  SymType := FnBase-6+SymType;
                FnBase-3..MaxOp:{ унарные операции и функции }
                  ;
                else
                  SetError(dlNoOperand);
              end { case }
            else
              case SymType of
                2, FnBase-3..MaxOp:
                  SetError(dlMissingOperation);
              end { case };
            while prio1[SymType] <= prio2[OpStack[TOS].Op] do
              { выполнять инфиксные операции со стека; префиксные и унарные
        выполняются не тут, а в RegisterOperand }
              begin
                CurrOp := OpStack[TOS].Op;
                CalcErrPos := OpStack[TOS].Pos;
                case CurrOp of
                  4:{+}
                    begin
                      DataStack[TDS-1] := DataStack[TDS-1]+DataStack[
                        TDS];
                      Dec(TDS);
                    end;
                  5:{-}
                    begin
                      DataStack[TDS-1] := DataStack[TDS-1]-DataStack[
                        TDS];
                      Dec(TDS);
                    end;
                  6:{*}
                    begin
                      DataStack[TDS-1] := DataStack[TDS-1]*DataStack[
                        TDS];
                      Dec(TDS);
                    end;
                  7:{/}
                    begin
                      DataStack[TDS-1] := DataStack[TDS-1]/DataStack[
                        TDS];
                      Dec(TDS);
                    end;
                  OpDiv:{/}
                    begin
                      DataStack[TDS-1] :=
                        Trunc(DataStack[TDS-1]) div Trunc(DataStack[TDS]);
                      Dec(TDS);
                    end;
                  8:{^ - возведение в степень }
                    begin
                      if (Abs(DataStack[TDS]) < $7FFFFFFF)
                        and (Trunc(DataStack[TDS]) = DataStack[TDS])
                      then
                        ToInt
                      else
                        DataStack[TDS-1] := Exp(ln(DataStack[TDS-1])*
                          DataStack[TDS]);
                      Dec(TDS);
                    end;
                  9:{ время }
                    begin
                      DataStack[TDS-1] := DataStack[TDS-1]*60+
                        DataStack[TDS];
                      Dec(TDS);
                    end;
                  10:{ = }
                    begin
                      if DataStack[TDS-1] = DataStack[TDS] then
                        DataStack[TDS-1] := -1
                      else
                        DataStack[TDS-1] := 0;
                      Dec(TDS);
                    end;
                  11:{ < }
                    begin
                      if DataStack[TDS-1] < DataStack[TDS] then
                        DataStack[TDS-1] := -1
                      else
                        DataStack[TDS-1] := 0;
                      Dec(TDS);
                    end;
                  12:{ > }
                    begin
                      if DataStack[TDS-1] > DataStack[TDS] then
                        DataStack[TDS-1] := -1
                      else
                        DataStack[TDS-1] := 0;
                      Dec(TDS);
                    end;
                  {13 ',' в стек не попадает }
                  15:{ |  or }
                    begin
                      DataStack[TDS-1] := Round(DataStack[TDS-1]) or
                        Round(DataStack[TDS]);
                      Dec(TDS);
                    end;
                  16:{ \  xor }
                    begin
                      DataStack[TDS-1] := Round(DataStack[TDS-1])
                        xor Round(DataStack[TDS]);
                      Dec(TDS);
                    end;
                  17:{ &  and }
                    begin
                      DataStack[TDS-1] := Round(DataStack[TDS-1])
                        and Round(DataStack[TDS]);
                      Dec(TDS);
                    end;
                  18:{ %  mod }
                    begin
                      DataStack[TDS-1] := Round(DataStack[TDS-1])
                        mod Round(DataStack[TDS]);
                      Dec(TDS);
                    end;
                  Op2Base:{ >= }
                    begin
                      if DataStack[TDS-1] >= DataStack[TDS] then
                        DataStack[TDS-1] := -1
                      else
                        DataStack[TDS-1] := 0;
                      Dec(TDS);
                    end;
                  Op2Base+1:{ <= }
                    begin
                      if DataStack[TDS-1] <= DataStack[TDS] then
                        DataStack[TDS-1] := -1
                      else
                        DataStack[TDS-1] := 0;
                      Dec(TDS);
                    end;
                  14, Op2Base+2, Op2Base+3:{ <> }
                    begin
                      if DataStack[TDS-1] <> DataStack[TDS] then
                        DataStack[TDS-1] := -1
                      else
                        DataStack[TDS-1] := 0;
                      Dec(TDS);
                    end;
                  Op2Base+4:{ <<  shl }
                    begin
                      DataStack[TDS-1] := Round(DataStack[TDS-1])
                        shl Round(DataStack[TDS]);
                      Dec(TDS);
                    end;
                  Op2Base+5:{ >>  shr }
                    begin
                      DataStack[TDS-1] := Round(DataStack[TDS-1])
                        shr Round(DataStack[TDS]);
                      Dec(TDS);
                    end;
                  else
                    SetError(dlNoOperand);
                end {case};
                CurrOp := 0;
                Dec(TOS); //PrevSymType := 0;
              end;
            case SymType of
              1:
                goto ExprEnd;
              3:
                begin{ закрывающую скобку сокращаем с открывающей }
                  if OpStack[TOS].Op <> 2 then
                    SetError(dlMissingLeftBracket);
                  Dec(TOS);
                  SymType := 0; { выр. в скобках - операнд }
                  RegisterOperand;
                end;
              13:{ , }
                begin
                  if (TOS < 3) or (OpStack[TOS].Op <> 2)
                    or (OpStack[TOS-1].PrefixCount = 0)
                  then
                    SetError(dlWrongComma);
                  Inc(OpStack[TOS].PrefixCount);
                  Dec(OpStack[TOS-1].PrefixCount);
                end;
              else
                begin{ положить операцию на стек }
                  Inc(TOS);
                  with OpStack[TOS] do
                    begin
                      Op := SymType;
                      Infix := SymType < FnBase-3;
                      if SymType < FnBase then
                        PrefixCount := 1
                      else
                        PrefixCount := FnTab[SymType-FnBase].A;
                      Pos := t-2;
                    end;
                end;
            end {case};
          end
      end;

ExprEnd:
    if Expression[t-2] <> ' ' then
      Inc(t);
    if (TOS <> 1) or (TDS <> 1) then
      SetError(dlMissingRightBracket);
    Res := DataStack[1];

    { Для extended +0 и -0 имеют разные представления, и -0 потом так
и отображается, удивляя юзера. Вот на этот случай заменяем любой 0,
на просто 0, который есть +0}
    if Res = 0 then
      Res := 0;

end { EvalText };

function GetErrOp(var X: integer): String;
  var
    OpName: String;
  begin
    X := t;
    Result := '';
    case CurrOp of
      0:
        begin
          Result := '';
          exit;
        end;
      1..Op2Base-1:
        OpName := Copy(OpChars, CurrOp, 1);
      Op2Base..FnBase:
        OpName := Copy(Op2, 1+3*(CurrOp-Op2Base), 2);
      else
        OpName := FnTab[CurrOp-FnBase].n
    end {case};
    Result := ' '+OpName;
  end { GetErrOp };

function Evalue(const s: String; CCV: Pointer): CReal;
  var
    o: Pointer;
  begin
    CurCalcView := CCV;
    try
      EvalueError := False;
      CurrOp := 0;
      CalcErrMess := dlMsgError;
      Expression := s;
      EvalText;
      Evalue := Res;
    except
      on E: eMathError do
        EvalueError := True;
      on E: EInvalidArgument do
        EvalueError := True;
    end {except};
  end;

procedure InitUnopPrio;
  var
    l, i: integer;
  begin
    l := Length(prio1);
    SetLength(prio1, MaxOp);
    SetLength(prio2, MaxOp);
    for i := l+1 to Length(prio1) do
      if prio1[i] = #0 then
        begin
          prio1[i] := '9';
          prio2[i] := '8';
        end;
  end;

begin
  InitUnopPrio;
end.

