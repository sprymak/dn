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
{(c) Alexey Korop aka AK155, 2002}
{GetValue - (c) Anton Fedorov aka DataCompBoy, 2000}
UNIT Calculat;
{&Delphi+}

INTERFACE
uses
  Commands;

Type CReal = Extended;
     PCReal = ^CReal;

Type VarGetFunc = Function (VarName: string; var Value: CReal): boolean;

function Evalue(const s: string; aVGF: VarGetFunc): CReal;
function GetErrOp(var x: integer): string;

var
  EvalueError: Boolean;
  CalcErrMess: TStrIdx;
  CalcErrPos: longint;

IMPLEMENTATION

uses
  Advance1
  , math
  ;

var
  Expression: string;
  Res: CReal;


function GetV(S: String; Base: integer; var Value: CReal): boolean;
const
  HexDigits = '0123456789ABCDEFG';
var RR, DegValue: CReal;
    j: byte;
    c, MaxC: char;
    d: longint;
begin
 result := false;
 if Length(S) = 0 then
   exit;
 RR:=0;
 UpStr(S);
 for j:=1 to Length(S) do
   begin
   c := system.Upcase(S[j]);
   if c = '.' then break;
   d := Pos(c, HexDigits)-1;
   if (d < 0) or (d >= Base) then exit;
   RR:=RR*Base + d;
   end;
 Delete(S, 1, j);
 if (Length(S) < 1) then begin
  Value:=RR;
  result:=true;
  exit
 end;
 DegValue:=1;
 for j:=1 to Length(S) do
   begin
   DegValue := DegValue / Base;
   d := Pos(S[j], HexDigits)-1;
   if (d < 0) or (d >= Base) then exit;
   end;
 Value:=RR;
 result:=True;
end;

{ После числа без проблела может быть десятичный множитель
(всякие кило- микро- и т.п. Множители распознаются английские
(u=микро) и русские.  }
function GetDec(S: String; var Value: CReal): boolean;
const
    MultE = ' d  с  m  u  p  n  f  da h  k  M  G  T  P ';
    MultR = ' д  с  м  мк п  н  ф  да г  к  М  Г  Т  П ';
//            -1 -2 -3 -6 -9-12-15 1  2  3  6  9  12 15
  MultV: array [1..length(MultE) div 3] of CReal =
   (1e-1, 1e-2, 1e-3, 1e-6, 1e-9, 1e-12, 1e-15
   , 1e1, 1e2,  1e3,  1e6,  1e9,  1e12,  1e15);
var
  R: Integer;
  l: longint;
  m: string;
begin
 Val(S, Value, R);
 if (R <> 0) then
   begin
   m := ' ' + Copy(S, R, 255) + ' ';
   l := Pos(m, MultE);
   if l = 0 then
     l := Pos(m, MultR);
   if l <> 0 then
     begin
     SetLength(S, R-1);
     Val(S, Value, R);
     Value := Value*MultV[(l+2)div 3];
     end;
   end;
 result := R=0;
end;

function GetValue(S: String; var Value: CReal): boolean;
begin
 GetValue:=false;
 if (S[Length(S)] = 'H') {and (S[1] in ['0'..'9'])} then begin
  SetLength(S, Length(S)-1);
  GetValue:=GetV(S, 16, Value);
 end else
 if S[1] = '$' then begin
  Delete(S, 1, 1); {DelFC(S);}
  GetValue:=GetV(S, 16, Value)
 end else
 if (Length(S) > 2) and (S[1] = '0') and (S[2] = 'X') then begin
  Delete(S, 1, 2); {DelFC(S);}
                   {DelFC(S);}
  GetValue:=GetV(S, 16, Value)
 end else
 if (S[Length(S)] in ['O','Q']) then begin
  SetLength(S, Length(S)-1);
  GetValue:=GetV(S, 8, Value)
 end else
 if (S[Length(S)] = 'B') then begin
  SetLength(S, Length(S)-1);
  GetValue:=GetV(S, 2, Value)
 end else
 GetValue:=GetDec(S, Value);
end;

var
  Operand: CReal;
  Sym: string;
  SymType, PrevSymType: integer;
  CurrOp: integer; { текущая операция (для ошибок)}
  t: byte; { индекс необработанного символа строки выражения }

const
  delimiters:  string[50] = '()*/+-=^<>:#,!|\&%~ '#3;
    { Лексические разделители. }

  MaxOp = 127;
  prio1:  string[MaxOp] = '1924455673332344453333559';  { приоритеты в тексте}
  prio2:  string[MaxOp] = '00 4455673332344453333558';  { приоритеты в стеке }
  OpChars =              #3'()+-*/^:=<>,#|\&%<<<<<<~'#1#2;
    { #1 представляет унарный минус, #2 - унарный плюс,
      #3 - конец выражения; Повторные '<' - это двухсимвольные оп. }

//  ops:    string[50] = OpChars; { односимвольные операции }

  Op2Chars = '<>!';
  Op2 = '>= <= <> != << >>'; {двухсимвольные операции}
  Op2Base = 19;
  LetterBinOp =    ' DIV OR  XOR AND MOD SHL SHR ';
  LetterBinOpType: array[1..7] of integer =
                   (7,  15, 16, 17, 18, Op2Base+4, Op2Base+5);

  FnBase = Length(OpChars)+1;

type
  FnEval = procedure (var d: CReal);
  FnEval2 = procedure (var d: CReal; d2: CReal);
  FnEval3 = procedure (var d: CReal; d2, d3: CReal);
  PFnDesc = ^TFnDesc;
  TFnDesc = object
    N{ame}: string[8];
    E{val}: pointer{FnEval};
    A{rguments}: integer;
    end;

procedure SinEv(var d: CReal);
  begin d := sin(d); end;


procedure CosEv(var d: CReal);
  begin d := cos(d); end;

procedure TgEv(var d: CReal);
  begin d := Tan(d); end;

procedure CtgEv(var d: CReal);
  begin d := Cotan(d); end;

procedure CosecEv(var d: CReal);
  begin d := 1/sin(d); end;

procedure SecEv(var d: CReal);
  begin d := 1/cos(d); end;

procedure ArcSinEv(var d: CReal);
  begin d := ArcSin(d); end;

procedure ArcCosEv(var d: CReal);
  begin d := ArcCos(d); end;

procedure ArcSecEv(var d: CReal);
  begin d := ArcCos(1/d); end;

procedure ArcCoSecEv(var d: CReal);
  begin d := ArcSin(1/d); end;

procedure ArcTanEv(var d: CReal);
  begin d := ArcTan(d); end;

procedure ArcCoTanEv(var d: CReal);
  begin d := Pi/2 - ArcTan(d); end;

procedure LnEv(var d: CReal);
  begin d := Ln(d); end;

procedure LgEv(var d: CReal);
  begin d := Ln(d)/ln(10); end;

procedure ExpEv(var d: CReal);
  begin d := Exp(d); end;

procedure SqrtEv(var d: CReal);
  begin d := Sqrt(d); end;

procedure SinhEv(var d: CReal);
  begin d := Sinh(d); end;

procedure CoshEv(var d: CReal);
  begin d := Sinh(d); end;

procedure TanhEv(var d: CReal);
  begin d := Tanh(d); end;

procedure CotanhEv(var d: CReal);
  begin d := 1 / Tanh(d); end;

procedure ArcSinhEv(var d: CReal);
  begin d := ArcSinh(d); end;

procedure ArcCoshEv(var d: CReal);
  begin d := ArcCosh(d); end;

procedure ArcTanhEv(var d: CReal);
  begin d := Ln( (1+d) / (1-d) ) / 2; end;

procedure SignEv(var d: CReal);
  begin
  if d < 0 then d := -1
  else if d > 0 then d := 1
  end;

procedure AbsEv(var d: CReal);
  begin d := Abs(d); end;

procedure RadEv(var d: CReal);
  begin d := (d*PI) / 180; end;

procedure RadGEv(var d: CReal);
  begin d := (d*PI) / 200; end;

procedure DegEv(var d: CReal);
  begin d := (d*180) / PI; end;

procedure GradEv(var d: CReal);
  begin d := (d*200) / PI; end;

procedure RoundEv(var d: CReal);
  begin d := Round(d); end;

procedure FactEv(var d: CReal);
  var
    i: integer;
    r: CReal;
  begin
  r := 1;
  for i := 1 to Trunc(d) do r := r*i;
  d := r;
  end;

procedure PiEv(var d: CReal);
  begin d := Pi; end;

procedure LogEv(var d: CReal; d2: CReal);
  begin d := logN(d, d2); end;

procedure RootEv(var d: CReal; d2: CReal);
  begin d := Exp(Ln(d2)/d); end;

procedure IfEv(var d: CReal; d2, d3: CReal);
  begin
  if d <> 0 then d := d2
  else d := d3;
  end;

const
  FnMax = 51;
  FnTab: array[0..FnMax-1] of TFnDesc =
   (
    (N:'SIN'; E: @SinEv; A:1)
   ,(N:'COS'; E: @CosEv; A:1)
   ,(N:'TG';  E: @TgEv; A:1)
   ,(N:'TAN';  E: @TgEv; A:1)
   ,(N:'CTG'; E: @CtgEv; A:1)
   ,(N:'COTAN'; E: @CtgEv; A:1)
   ,(N:'SEC'; E: @SecEv; A:1)
   ,(N:'COSEC'; E: @CosecEv; A:1)
   ,(N:'ASIN'; E: @ArcSinEv; A:1)
   ,(N:'ARCSIN'; E: @ArcSinEv; A:1)
   ,(N:'ACOS'; E: @ArcCosEv; A:1)
   ,(N:'ARCCOS'; E: @ArcCosEv; A:1)
   ,(N:'ARCSEC'; E: @ArcSecEv; A:1)
   ,(N:'ARCCOSEC'; E: @ArcCosecEv; A:1)
   ,(N:'ATAN'; E: @ArcTanEv; A:1)
   ,(N:'ARCTAN'; E: @ArcTanEv; A:1)
   ,(N:'ACTG'; E: @ArcCoTanEv; A:1)
   ,(N:'ARCCOTAN'; E: @ArcCoTanEv; A:1)
   ,(N:'LN';  E: @LnEv; A:1)
   ,(N:'LG';  E: @LgEv; A:1)
   ,(N:'EXP'; E: @ExpEv; A:1)
   ,(N:'SQR'; E: @SqrtEv; A:1)
   ,(N:'SQRT'; E: @SqrtEv; A:1)
   ,(N:'SH'; E: @SinhEv; A:1)
   ,(N:'SINH'; E: @SinhEv; A:1)
   ,(N:'CH'; E: @CoshEv; A:1)
   ,(N:'COSH'; E: @CoshEv; A:1)
   ,(N:'TH'; E: @TanhEv; A:1)
   ,(N:'TANH'; E: @TanhEv; A:1)
   ,(N:'CTH'; E: @CotanhEv; A:1)
   ,(N:'COTANH'; E: @CotanhEv; A:1)
   ,(N:'ARCSINH'; E: @ArcSinhEv; A:1)
   ,(N:'ASH'; E: @ArcSinEv; A:1)
   ,(N:'ARCCOSH'; E: @ArcCoshEv; A:1)
   ,(N:'ACOSH'; E: @ArcCosEv; A:1)
   ,(N:'ACH'; E: @ArcCosEv; A:1)
   ,(N:'ATH'; E: @ArcTanhEv; A:1)
   ,(N:'ARTH'; E: @ArcTanhEv; A:1)
   ,(N:'ARCTANH'; E: @ArcTanhEv; A:1)
   ,(N:'FACT'; E: @FactEv; A:1)
   ,(N:'SIGN'; E: @SignEv; A:1)
   ,(N:'ABS'; E: @AbsEv; A:1)
   ,(N:'RAD'; E: @RadEv; A:1)
   ,(N:'RADG'; E: @RadGEv; A:1)
   ,(N:'DEG'; E: @DegEv; A:1)
   ,(N:'GRAD'; E: @GradEv; A:1)
   ,(N:'ROUND'; E: @RoundEv; A:1)
   ,(N:'PI'; E: @PiEv; A:0)
   ,(N:'LOG'; E: @LogEv; A:2)
   ,(N:'ROOT'; E: @RootEv; A:2)
   ,(N:'IF'; E: @IfEv; A:3)
   );

  procedure ScanSym;
  var
    t0: integer;
    i: integer;
    c: char;
  begin
  while Expression[t] =  ' ' do inc(t);
  t0 := t;
  while Pos(Expression[t], delimiters) = 0 do inc(t);
  SymType := 0;
  c := Expression[t0];
  if t = t0 then
    begin {операция - разделитель}
    inc(t);
    if Pos(c, Op2Chars) <> 0 then
      begin
      Sym := Copy(Expression, t0, 2);
      i := Pos(Sym, Op2);
      if i <> 0 then
        begin
        inc(t); SymType := Op2Base + i div 3;
        exit;
        end
      end;
    Sym := c;
    SymType := Pos(Sym, OpChars);
    end
  else if (c = '$') or ((c >= '0') and (c <= '9')) then
    begin {число}
    if (c <> '$') and (system.UpCase(Expression[t-1]) = 'E')
       and (Expression[t] = '-')
    then
      begin {похоже на число вида 5e-3}
      repeat inc(t) until Pos(Expression[t], delimiters) <> 0;
      end;
    Sym := {UpStrg(}copy(Expression, t0, t-t0){)};
    end
  else
    begin {должна быть функция или буквенная операция вроде AND}
    Sym := UpStrg(copy(Expression, t0, t-t0));
    i := Pos (' ' + Sym + ' ', LetterBinOp);
    if i <> 0 then
      begin
      SymType := LetterBinOpType[(3+i) div 4];
      exit;
      end;

    for i := 0 to FnMax-1 do
      begin
      if Sym = FnTab[i].N then
        begin
        SymType := FnBase + i;
        break;
        end;
      end;
    end;
  end;

procedure SetError(Id: TStrIdx);
  begin
  EvalueError := true; CalcErrMess := Id;
  CalcErrPos := t-2;
  end;

{ Классический алгоритм с двумя стеками и двумя приоритетами }
var
  DataStack: array[1..20] of CReal;
  TDS: integer; { указатель вершины }
  OpStack: array[1..20] of
    record Infix: boolean; Op, PrefixCount, Pos: integer end;
  TOS: integer; { указатель вершины }
  i: integer;

procedure RegisterOperand; forward;

procedure EvalPrefixOp;
  begin
  CurrOp := OpStack[TOS].Op; CalcErrPos := OpStack[TOS].Pos;
  case CurrOp of
    FnBase-3: { ~ }
      DataStack[TDS] := not Round(DataStack[TDS]);
    FnBase-2: {Унарный плюс}
      begin  end;
    FnBase-1: {Унарный минус}
      DataStack[TDS] := - DataStack[TDS];
   else with FnTab[CurrOp-FnBase] do
     begin
     if TDS < A then
       begin SetError(dlNoOperand); exit; end;
     case A of
 {     0: не бывает }
      1: FnEval(E)(DataStack[TDS]);
      2: begin
         FnEval2(E)(DataStack[TDS-1], DataStack[TDS]);
         Dec(TDS);
         end;
      3: begin
         FnEval3(E)(DataStack[TDS-2], DataStack[TDS-1], DataStack[TDS]);
         Dec(TDS, 2);
         end;
     end {case};
     end;
  end {case};
  dec(TOS);
  CurrOp := 0;
  RegisterOperand;
  end;

procedure RegisterOperand;
  begin
  with OpStack[TOS] do
    begin
    dec(PrefixCount);
    if PrefixCount < 0 then
      begin SetError(dlMissingOperation); exit; end;
    if not Infix and (PrefixCount=0) then
      EvalPrefixOp;
    end;
  SymType := 0;
  end;

procedure EvalText;


label ExprEnd;

begin
t := 1; Expression := Expression + #3;

TOS := 1; with OpStack[1] do
  begin Infix := true; Op := 1; PrefixCount := 1; end;

SymType := 1;
TDS := 0;

while true do
  begin
  PrevSymType := SymType;
  ScanSym;
  if SymType = 0 then
    begin { разобрать операнд и положить на стек данных }
    Inc(TDS);
    if not GetValue(Sym, DataStack[TDS]) then
      begin SetError(dlWrongText); exit; end;
    RegisterOperand;
    end
  else if (SymType >= FnBase) and (FnTab[SymType-FnBase].A = 0) then
    begin
    inc(TDS); FnEval(FnTab[SymType-FnBase].E)(DataStack[TDS]);
    RegisterOperand;
    end
  else
    begin { разобраться с операцией }
    if (OpStack[TOS].PrefixCount > 0) then { ожидается источник значения }
      case SymType of
        2: { выражение в скобках годится };
        4,5:  { превращаем в унарные плюс и минус }
           SymType := FnBase - 6 + SymType;
        FnBase-3..MaxOp: { унарные операции и функции } ;
        else
          begin SetError(dlNoOperand); dec(t); exit; end;
      end { case }
    else
      case SymType of
        2,FnBase-3..MaxOp:
          begin SetError(dlMissingOperation); exit; end;
      end { case };
    while prio1[SymType] <= prio2[OpStack[TOS].Op] do
      { выполнять инфиксные операции со стека; префиксные и унарные
        выполняются не тут, а в RegisterOperand }
      begin
        CurrOp := OpStack[TOS].Op; CalcErrPos := OpStack[TOS].Pos;
        case CurrOp of
          4: {+}
            begin
            DataStack[TDS-1] := DataStack[TDS-1] + DataStack[TDS];
            Dec(TDS);
            end;
          5: {-}
            begin
            DataStack[TDS-1] := DataStack[TDS-1] - DataStack[TDS];
            Dec(TDS);
            end;
          6: {*}
            begin
            DataStack[TDS-1] := DataStack[TDS-1] * DataStack[TDS];
            Dec(TDS);
            end;
          7: {/}
            begin
            DataStack[TDS-1] := DataStack[TDS-1] / DataStack[TDS];
            Dec(TDS);
            end;
          8: {^ - возведение в степень }
            begin
            DataStack[TDS-1] := exp(ln(DataStack[TDS-1])*DataStack[TDS]);
            Dec(TDS);
            end;
          9: { время }
            begin
            DataStack[TDS-1] := DataStack[TDS-1]*60 + DataStack[TDS];
            Dec(TDS);
            end;
          10: { = }
            begin
            if DataStack[TDS-1] = DataStack[TDS] then
              DataStack[TDS-1] := -1
            else DataStack[TDS-1] := 0;
            dec(TDS);
            end;
          11: { < }
            begin
            if DataStack[TDS-1] < DataStack[TDS] then
              DataStack[TDS-1] := -1
            else DataStack[TDS-1] := 0;
            dec(TDS);
            end;
          12: { > }
            begin
            if DataStack[TDS-1] > DataStack[TDS] then
              DataStack[TDS-1] := -1
            else DataStack[TDS-1] := 0;
            dec(TDS);
            end;
          {13 ',' в стек не попадает }
          15: { |  or }
            begin
            DataStack[TDS-1] := Round(DataStack[TDS-1]) or Round(DataStack[TDS]);
            Dec(TDS);
            end;
          16: { \  xor }
            begin
            DataStack[TDS-1] := Round(DataStack[TDS-1]) xor Round(DataStack[TDS]);
            Dec(TDS);
            end;
          17: { &  and }
            begin
            DataStack[TDS-1] := Round(DataStack[TDS-1]) and Round(DataStack[TDS]);
            Dec(TDS);
            end;
          18: { %  mod }
            begin
            DataStack[TDS-1] := Round(DataStack[TDS-1]) mod Round(DataStack[TDS]);
            Dec(TDS);
            end;
          Op2Base: { >= }
            begin
            if DataStack[TDS-1] >= DataStack[TDS] then
              DataStack[TDS-1] := -1
            else DataStack[TDS-1] := 0;
            dec(TDS);
            end;
          Op2Base+1: { <= }
            begin
            if DataStack[TDS-1] <= DataStack[TDS] then
              DataStack[TDS-1] := -1
            else DataStack[TDS-1] := 0;
            dec(TDS);
            end;
          14, Op2Base+2, Op2Base+3: { <> }
            begin
            if DataStack[TDS-1] <> DataStack[TDS] then
              DataStack[TDS-1] := -1
            else DataStack[TDS-1] := 0;
            dec(TDS);
            end;
          Op2Base+4: { <<  shl }
            begin
            DataStack[TDS-1] := Round(DataStack[TDS-1]) shl Round(DataStack[TDS]);
            Dec(TDS);
            end;
          Op2Base+5: { >>  shr }
            begin
            DataStack[TDS-1] := Round(DataStack[TDS-1]) shr Round(DataStack[TDS]);
            Dec(TDS);
            end;
          else
            begin SetError(dlNoOperand); exit; end;
        end {case};
        CurrOp := 0;
        Dec(TOS); //PrevSymType := 0;
      end;
    case SymType of
     1: goto ExprEnd;
     3:
      begin { закрывающую скобку сокращаем с открывающей }
      if OpStack[TOS].Op <> 2 then
        begin SetError(dlMissingLeftBracket); exit; end;
      dec(TOS);
      SymType := 0; { выр. в скобках - операнд }
      RegisterOperand;
      end;
     13: { , }
      begin
      if (TOS < 3) or (OpStack[TOS].Op <> 2)
        or (OpStack[TOS-1].PrefixCount = 0)
      then begin SetError(dlWrongComma); exit; end;
      inc(OpStack[TOS].PrefixCount);
      dec(OpStack[TOS-1].PrefixCount);
      end;
     else
      begin { положить операцию на стек }
      inc(TOS);
      with OpStack[TOS] do
        begin
        Op := SymType; Infix := SymType < FnBase-3;
        if SymType < FnBase then
          PrefixCount := 1
        else PrefixCount := FnTab[SymType-FnBase].A;
        Pos := t-2;
        end;
      end;
    end{case};
    end
  end;

ExprEnd:
if EvalueError then
  exit;
if Expression[t-2] <> ' ' then inc(t);
if (TOS <> 1) or (TDS <> 1) then
  begin EvalueError := true;
  SetError(dlMissingRightBracket);
  end
else
  Res := DataStack[1];
end;

function GetErrOp(var x: integer): string;
  var
    OpName: string;
  begin
  x := t;;
  case CurrOp of
   0: begin result := ''; exit;  end;
   1..Op2Base-1:
     OpName := Copy(OpChars, CurrOp, 1);
   Op2Base..FnBase:
     OpName := Copy(Op2, 1 + 3*(CurrOp-Op2Base), 2);
   else
     OpName := FnTab[CurrOp-FnBase].N
  end {case};
  result := ' ' + OpName;
  end;


function Evalue(const s: string; aVGF: VarGetFunc): CReal;
var o: pointer;
begin
 EvalueError:=false; CurrOp := 0;
 Expression := s;
 EvalText;
 Evalue:=Res;
end;

procedure InitUnopPrio;
  var
    l, i: integer;
  begin
  l := length(prio1); setlength(prio1, MaxOp); setlength(prio2, MaxOp);
  for i := l+1 to length(prio1) do
    if prio1[i] = #0 then
      begin prio1[i] := '9'; prio2[i] := '8'; end;
  end;

begin
InitUnopPrio;
END.

