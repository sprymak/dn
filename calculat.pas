{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source
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
//////////////////////////////////////////////////////////////////////////
//
//  Version history:
//
//  1.6.RC1
//  dn16rc1-ExponentForm_Calculator.patch
//  dn16rc1-calc_func.patch
//  dn16rc1-calc_func_fix.patch
//  dn16rc1-calculator_fix.patch
//
//  2.0.0
//  dn230-calculate_empty_expression_fix.patch
//  dn2628-calculator_error_messages_improve.patch
//
//  2.7.0
//  dn270-calculator_bugs_fix.patch
//  dn2811-calculator_bugs_fix.patch
//  dn2821-wkz_cell_formula_auto_calculation.patch
//  dn2825-wkz_big_improve_and_various_bugs_fix.patch
//  dn2825-calculator_improve.patch
//  dn2911-calculator_bugs_fix.patch
//  dn2922-calculator_bugs_fix.patch
//  dn2106-calculator_bugs_fix.patch
//  dn21202-calculator(f)-too_large_number_fix.patch
//  dn3216-calculator(n)-new_factorial_implementation.patch
//  dn3216-calculator(f)-double_parameter_functions_fix.patch
//  dn3323-calculator(f)-bugs_fix.patch
//
//  3.7.0
//
//////////////////////////////////////////////////////////////////////////}
{$I STDEFINE.INC}
{(c) Anton Fedorov aka DataCompBoy, 2000}
{(c) Sergey Biryukov aka Flash, 2002}
UNIT Calculat;

INTERFACE

Type CReal = Extended;
     PCReal = ^CReal;

Type VarGetFunc = Function (VarName: string; var Value: CReal): boolean;

function GetValue(S: String; var Value: CReal): boolean;

function Evalue(const as: string; aVGF: VarGetFunc): CReal;

Const EvalueError: Boolean = False;

{ Flash >>> }
      Stack_Overflow          = 1;
      Too_Large_Number        = 5;
      Argument_Missing        = 10;   { Syntax Errors }
      Syntax_Error            = 15;
      Unknown_Function        = 20;
      Left_Bracket_Missing    = 25;
      Right_Bracket_Missing   = 30;
      Operation_Expected      = 35;
      Unknown_Symbol          = 40;
      Two_Operations          = 45;
      Comma_Missing           = 50;
      Too_Many_Commas         = 55;
      Incorrect_Usage         = 60;
      Division_By_Zero        = 100;   { Math Errors }
      LOG_Wrong_Base          = 105;
      Power_Wrong_Index       = 110;
      Root_Wrong_Index        = 115;
      Wrong_Argument          = 120;
      Power_Wrong_Argument    = 125;
      Incorrect_Form          = 200;

const Epsilon : CReal = 3.4e-4932;

var ErrCode: integer;
    ErrString: string[20];
{ Flash <<< }

IMPLEMENTATION

uses Dos, Advance1, Collect;
(*{$IFDEF DPMI}{$L Int10.obp}procedure Exc10Handler; external;{$ENDIF}*)

Var StrToCalcList: PLineCollection; {John_SW}

function dnSin(x: CReal): CReal;
begin
dnSin:=System.Sin(x);
end;

function dnCos(x: CReal): CReal;
begin
dnCos:=System.Cos(x);
end;

function dnArcTan(x: CReal): CReal;
begin
dnArcTan:=System.ArcTan(x)
end;

function dnSh(x: CReal): CReal;
begin
dnSh:=(Exp(x)-Exp(-x))/2
end;

function dnCh(x:CReal):CReal;
begin
dnCh:=(Exp(x)+Exp(-x))/2
end;

function dnTh(x: CReal): CReal;
var K: CReal;
begin
K:=(Exp(x)+Exp(-x));
if Abs(K)>Epsilon
 then dnTh:=(Exp(x)-Exp(-x))/K
 else begin dnTh:=1; ErrCode:=Wrong_Argument; EvalueError:=true; end;
end;

function dnCth(x: CReal): CReal;
var K: CReal;
begin
K:=(Exp(x)-Exp(-x));
if Abs(K)>Epsilon
 then dnCth:=(Exp(x)+Exp(-x))/K
 else begin dnCth:=1; ErrCode:=Wrong_Argument; EvalueError:=true; end;
end;

function dnArsh(x: CReal): CReal;
begin
if (x>=0)
 then dnArsh:=Ln(x+Sqrt(Sqr(x)+1))
 else begin dnArsh:=1; ErrCode:=Wrong_Argument; EvalueError:=true end;
end;

function dnArch(x: CReal): CReal;
begin
if (x>=1)
 then dnArch:=Ln(x+Sqrt(Sqr(x)-1))
 else begin dnArch:=1; ErrCode:=Wrong_Argument; EvalueError:=true end;
end;

function dnArth(x: CReal): CReal;
begin
if (Abs(X)<1-Epsilon)
 then dnArth:=Ln((1+x)/(1-x))/2
 else begin dnArth:=1; ErrCode:=Wrong_Argument; EvalueError:=true end;
end;

function dnArcth(x: CReal): CReal;
begin
if (Abs(X)>1+Epsilon)
 then dnArcth:=Ln((x+1)/(x-1))/2
 else begin dnArcth:=1; ErrCode:=Wrong_Argument; EvalueError:=true end;
end;

function dnSec(x: CReal): CReal;
begin
if (Abs(dnCos(x))>Epsilon)
 then dnSec:=1/dnCos(x)
 else begin dnSec:=1; ErrCode:=Wrong_Argument; EvalueError:=true end;
end;

function dnRad(x: CReal): CReal;
begin
dnRad:=(x*180)/PI
end;

function dnTan(x: CReal): CReal;
begin
if (Abs(dnCos(x))>Epsilon)
 then dnTan:=dnSin(x)/dnCos(x)
 else begin dnTan:=1; ErrCode:=Wrong_Argument; EvalueError:=true end;
end;

function dnGrad(x: CReal): CReal;
begin
dnGrad:=(x*PI)/180
end;

function dnFact(x: CReal): CReal;
var R: CReal; I: longint;
begin
if (x>=0) {and (x<=1754)} then
 begin R:=1; for I:=2 to Trunc(x) do R:=R*I; dnFact:=R; end
else begin dnFact:=1; ErrCode:=Wrong_Argument; EvalueError:=true end;
end;

function dnDFact(x: CReal): CReal;
var R: CReal; I: longint;
begin
if (x>0) {and (x<=1754)} then
 begin
 if Odd(Trunc(x)) then begin R:=1; I:=3; end
                  else begin R:=2; I:=4; end;
 while I<=Trunc(x) do begin R:=R*I; Inc(I,2); end;
 dnDFact:=R;
 end
else begin dnDFact:=1; ErrCode:=Wrong_Argument; EvalueError:=true end;
end;

function dnCoSec(x: CReal): CReal;
begin
if (Abs(Sin(x))>Epsilon)
 then dnCoSec:=1/Sin(x)
 else begin dnCoSec:=1; ErrCode:=Wrong_Argument; EvalueError:=true end;
end;

function dnCoTan(x: CReal): CReal;
begin
if (Abs(Sin(x))>Epsilon)
 then dnCoTan:=Cos(x)/Sin(x)
 else begin dnCoTan:=1; ErrCode:=Wrong_Argument; EvalueError:=true end;
end;

function dnArcCoTan(x: CReal): CReal;
begin
dnArcCoTan:=Pi/2-ArcTan(x)
end;

function dnArcSin(x: CReal): CReal;
begin
if (x>=-1) and (x<=1) then
 if Abs(x+1)<Epsilon then dnArcSin:=-(pi/2) else
  if Abs(x-1)<Epsilon then dnArcSin:=(pi/2) else
   dnArcSin:=ArcTan(x/sqrt(1-sqr(x)))
else begin dnArcSin:=1; ErrCode:=Wrong_Argument; EvalueError:=true end;
end;

function dnArcCos(x: CReal): CReal;
begin
if (x>=-1) and (x<=1) then
 if Abs(x+1)<Epsilon then dnArcCos:=pi else
  if Abs(x-1)<Epsilon then dnArcCos:=0 else
   dnArcCos:=dnArcCoTan(x/sqrt(1-sqr(x)))
else begin dnArcCos:=1; ErrCode:=Wrong_Argument; EvalueError:=true end;
end;

function dnArcSec(x: CReal): CReal;
begin
if Abs(x)>Epsilon
 then dnArcSec:=dnArcCos(1/x)
 else begin dnArcSec:=1; ErrCode:=Wrong_Argument; EvalueError:=true end;
end;

function dnArcCoSec(x: CReal): CReal;
begin
if Abs(x)>Epsilon
 then dnArcCoSec:=dnArcSin(1/x)
 else begin dnArcCoSec:=1; ErrCode:=Wrong_Argument; EvalueError:=true end;
end;

function Step(x,a: CReal): CReal;
var S: CReal;
begin
if ((Abs(x)<Epsilon) and (Abs(a)<Epsilon)) then
 begin Step:=1; ErrCode:=Power_Wrong_Index; EvalueError:=true; Exit end
else
 if (Abs(a)<Epsilon) then begin Step:=1; Exit end
  else
  if a=int(a) then
   if Abs(x)>Epsilon then
    begin
    S:=Exp(a*Ln(Abs(x)));
    if (x=int(x)) and (a>0) then S:=int(S+0.5);
    if (Abs(Trunc(a)) mod 2 = 1) and (x<0) then Step:=-S else Step:=S;
    Exit;
    end else
   begin
   Step:=0; Exit;
   end;
  if x>Epsilon then
   begin
   S:=Exp(a*Ln(x));
   if (a=int(a)) and (x=int(x)) and (a>0) then S:=int(S+0.5);
   Step:=S;
   Exit;
   end
  else begin Step:=1; ErrCode:=Power_Wrong_Argument; EvalueError:=true end;
end;

function dnRoot(a,x: CReal): CReal;
var S: CReal;
begin
if (a<1-Epsilon) or (Abs(Frac(a))>Epsilon)
 then begin dnRoot:=1; ErrCode:=Root_Wrong_Index; EvalueError:=true end
 else
 if (Trunc(a) mod 2 = 1) then
  begin
  if Abs(x)>Epsilon then S:=Exp((1/a)*Ln(Abs(x)))
   else begin dnRoot:=1; ErrCode:=Wrong_Argument; EvalueError:=true end;
  if x<0 then dnRoot:=-S
   else dnRoot:=S
  end
 else if x>Epsilon then dnRoot:=Exp((1/a)*Ln(x))
else begin dnRoot:=1; ErrCode:=Wrong_Argument; EvalueError:=true end;
end;

function dnLog(a,x: CReal): CReal;
begin
if (a<Epsilon) or (Abs(a-1)<Epsilon) then
 begin dnLog:=1; ErrCode:=LOG_Wrong_Base; EvalueError:=true end else
if (x<Epsilon) or (EvalueError) then
 begin dnLog:=1; ErrCode:=Wrong_Argument; EvalueError:=true end else
dnLog:=Ln(x)/Ln(a);
end;

function dnLn(x: CReal): CReal;
begin
if (x<Epsilon)
 then begin dnLn:=1; ErrCode:=Wrong_Argument; EvalueError:=true end
 else dnLn:=System.Ln(x);
end;

function dnNot(x: CReal): CReal;
begin
dnNot:=not Round(x);
end;

function dnAbs(x: CReal): CReal;
begin
dnAbs:=System.Abs(x);
end;

function dnSqr(x: CReal): CReal;
begin
dnSqr:=System.Sqr(x);
end;

function dnSqrt(x: CReal): CReal;
begin
dnSqrt:=System.Sqrt(x);
end;

function dnExp(x: CReal): CReal;
begin
dnExp:=System.Exp(x);
end;

function dnLg(x: CReal): CReal;
begin
dnLg:=dnLn(x)/dnLn(10);
end;

function dnRound(x: CReal): CReal;
begin
dnRound:=System.Round(x);
end;

function dnSign(x: CReal): CReal;
begin
if x>0 then dnSign:=1 else
if x<0 then dnSign:=-1 else
dnSign:=0;
end;

function dnPi: CReal;
begin
dnPi:=System.Pi;
end;

function dnIf(Value,Value2,Value3: CReal): CReal;
begin
if Value<>0 then dnIf:=Value2 else dnIf:=Value3;
end;

const cmd: string[10] = 'CALCULATOR';
      CmdSign: Set of char = ['>', '<', '=', '!', '#', '+', '-', '|',
                              '\', '/', '*', '&', '~', '%', '^'];
      DoubleOperations: string[40] = '>= <= <> != || && << >>';

function IsFunction(s: string; start, stop: byte; var k: byte) : boolean; forward;

function GetValue(S: String; var Value: CReal): boolean;

 function GetHex(var S: String; var Value: CReal): boolean;
 var RR, RRR: CReal;
     l: byte absolute S;
     j: byte;
     Invert: boolean;
 begin
  GetHex := false;
  if L>15 then begin EvalueError:=true; ErrCode:=Too_Large_Number; exit; end;
  if S[1] in ['-','+'] then begin
   Invert:=S[1]='-';
   DelFC(S);
  end else Invert:=False;

  RR:=0;
  for j:=1 to l do
   Case S[j] of
    '0'..'9': RR:=RR*$10 + Ord(S[j])-48;
    'A'..'F': RR:=RR*$10 + Ord(S[j])-55;
    '.': break;
    else exit;
   end;
  Delete(S, 1, j);
  if (L<1) then begin
   GetHex:=true;
   If Invert then Value:=-RR else Value:=RR;
   exit
  end;
  RRR:=RR; RR:=0;
  for j:=1 to l do
   Case S[j] of
    '0'..'9': RR:=RR*$10 + Ord(S[j])-48;
    'A'..'F': RR:=RR*$10 + Ord(S[j])-55;
    else exit;
   end;
  If Invert
   then Value:=-(RRR + RR/Step($10,l))
   else Value:=RRR + RR/Step($10,l);
  GetHex:=True;
 end;

 function GetOct(var S: String; var Value: CReal): boolean;
 var RR, RRR: CReal;
     l: byte absolute S;
     j: byte;
     Invert: boolean;
 begin
  GetOct := false;
  if L>21 then begin EvalueError:=true; ErrCode:=Too_Large_Number; exit; end;
  if S[1] in ['-','+'] then begin
   Invert:=S[1]='-';
   DelFC(S);
  end else Invert:=False;

  RR:=0;
  for j:=1 to l do
   Case S[j] of
    '0'..'7': RR:=RR*8 + Ord(S[j])-48;
    '.': break;
    else exit;
   end;
  Delete(S, 1, j);
  if (L<1) then begin
   GetOct:=true;
   If Invert then Value:=-RR else Value:=RR;
   exit
  end;
  RRR:=RR; RR:=0;
  for j:=1 to l do
   Case S[j] of
    '0'..'7': RR:=RR*8 + Ord(S[j])-48;
    else exit;
   end;
  If Invert
   then Value:=-(RRR + RR/Step(8,l))
   else Value:=RRR + RR/Step(8,l);
  GetOct:=True;
 end;

 function GetBin(var S: String; var Value: CReal): boolean;
 var RR, RRR: CReal;
     l: byte absolute S;
     j: byte;
     Invert: boolean;
 begin
  GetBin := false;
  if L>63 then begin EvalueError:=true; ErrCode:=Too_Large_Number; exit; end;
  if S[1] in ['-','+'] then begin
   Invert:=S[1]='-';
   DelFC(S);
  end else Invert:=False;

  RR:=0;
  for j:=1 to l do
   Case S[j] of
    '0': RR:=RR*2;
    '1': RR:=RR*2 + 1;
    '.': break;
    else exit;
   end;
  Delete(S, 1, j);
  if (L<1) then begin
   GetBin:=true;
   If Invert then Value:=-RR else Value:=RR;
   exit
  end;
  RRR:=RR; RR:=0;
  for j:=1 to l do
   Case S[j] of
    '0': RR:=RR*2;
    '1': RR:=RR*2 + 1;
    else exit;
   end;
  If Invert
   then Value:=-(RRR + RR/Step(2,l))
   else Value:=RRR + RR/Step(2,l);
  GetBin:=True;
 end;

 function GetDec(var S: String; var Value: CReal): boolean;
 var R: Integer;
 begin
  Val(S, Value, R);
  GetDec := R=0;
 end;

var i,k,j1,j2: integer;
    p: byte;
    Result: boolean;
begin
 GetValue:=false;
 if (S[Length(S)] = 'H') and (S[1] in ['0'..'9','A'..'F']) then begin
  Dec(S[0]);
  Result:=GetHex(S, Value);
  if EvalueError then exit;
  if not Result then begin ErrCode:=Incorrect_Form; ErrString:='HEX'; EvalueError:=true; Exit; end;
  GetValue:=Result;
 end else
 if S[1] = '$' then begin
  DelFC(S);
  Result:=GetHex(S, Value);
  if EvalueError then exit;
  if not Result then begin ErrCode:=Incorrect_Form; ErrString:='HEX'; EvalueError:=true; Exit; end;
  GetValue:=Result;
 end else
 if (S[0]>#2) and (S[1] = '0') and (S[2] = 'X') then begin
  DelFC(S); DelFC(S);
  Result:=GetHex(S, Value);
  if EvalueError then exit;
  if not Result then begin ErrCode:=Incorrect_Form; ErrString:='HEX'; EvalueError:=true; Exit; end;
  GetValue:=Result;
 end else
 if (S[Length(S)] in ['O','Q']) then begin
  Dec(S[0]);
  Result:=GetOct(S, Value);
  if EvalueError then exit;
  if not Result then begin ErrCode:=Incorrect_Form; ErrString:='OCT'; EvalueError:=true; Exit; end;
  GetValue:=Result;
 end else
 if (S[0]>#2) and (S[1] = '0') and (S[2] in ['O','Q']) then begin
  DelFC(S); DelFC(S);
  Result:=GetOct(S, Value);
  if EvalueError then exit;
  if not Result then begin ErrCode:=Incorrect_Form; ErrString:='OCT'; EvalueError:=true; Exit; end;
  GetValue:=Result;
 end else
 if (S[Length(S)] = 'B') then begin
  Dec(S[0]);
  Result:=GetBin(S, Value);
  if EvalueError then exit;
  if not Result then begin ErrCode:=Incorrect_Form; ErrString:='BIN'; EvalueError:=true; Exit; end;
  GetValue:=Result;
 end else
 if (S[0]>#2) and (S[1] = '0') and (S[2] = 'B') then begin
  DelFC(S); DelFC(S);
  Result:=GetBin(S, Value);
  if EvalueError then exit;
  if not Result then begin ErrCode:=Incorrect_Form; ErrString:='BIN'; EvalueError:=true; Exit; end;
  GetValue:=Result;
 end else
 begin
 if ((S[0]=#2) and (S[1] = '0') and (S[2] = 'X')) or (S[Length(S)]='H') then
  begin ErrCode:=Incorrect_Form; ErrString:='HEX'; EvalueError:=true; Exit; end;
 if s[Length(s)]<>'H' then
   begin
   j1:=1; j2:=1;
   for i:=1 to Length(s) do
     if (s[i] in ['A'..'Z']) and (s[i]<>'E') then
        begin
        j1:=i;
        j2:=Length(s);
        for k:=j1 to Length(s)-1 do
        if (s[k] in ['A'..'Z'])
           and ((s[k+1] in ['0'..'9']) or (s[k+1] in CmdSign))
           and (s[k]<>'E') then begin j2:=k; break; end;
        if IsFunction(s,j1,j2,p) or (s[j2] in ['H','B','O','Q']) then
         begin ErrCode:=Operation_Expected; EvalueError:=true; Exit; end
        else
         begin ErrCode:=Unknown_Function; EvalueError:=true; Exit; end;
        break;
        end;
   end;
 Result:=GetDec(S, Value);
 if not Result then
   begin   { Flash >>> }
   ErrCode:=Incorrect_Form;
   if Pos('E',S)>0 then ErrString:='EXP' else ErrString:='DEC';
   EvalueError:=true; Exit;
   end;    { Flash <<< }
 GetValue:=Result;
 end
end;

Var S: ^String;
    VGF: VarGetFunc;
    FreeByte: byte;

{$IFNDEF BIT_32}
 {Sergey Abmetko - Unhanled exception bug fix start}
Procedure       Int75Handler;Interrupt;
Begin
  Asm
  (* FPU *)
    mov     al,00h
    out     0F0h,al
  (* PICs *)
    mov     al,20h
    out     20h,al
    out     0A0h,al
  End;
  EvalueError := true;
  ErrCode := Too_Large_Number; { Flash }
End;

Var Int75Old:Pointer;

Procedure SetInt75Handler;
Begin
  GetIntVec($75,Int75Old);
  SetIntVec($75,Addr(Int75Handler));
End;

Procedure RestoreInt75Handler;
Begin
  SetIntVec($75,Int75Old);
End;
 {Sergey Abmetko - Unhanled exception bug fix stop}
{$ENDIF}

type
  VarEval = function : CReal;
  FnEval  = function (d: CReal) : CReal;
  FnEval2 = function (d: CReal; d2: CReal) : CReal;
  FnEval3 = function (d: CReal; d2, d3: CReal) : CReal;
  PFnDesc = ^TFnDesc;
  TFnDesc = object
    N{ame}: string[8];
    E{val}: pointer{FnEval};
    A{rguments}: byte;
    end;

const
  MaxFunctions = 58;
  FnTab: array[0..MaxFunctions-1] of TFnDesc =
    (
    (N:'ABS';      E: @dnAbs;      A:1)
   ,(N:'ACH';      E: @dnArCh;     A:1)
   ,(N:'ACOS';     E: @dnArcCos;   A:1)
   ,(N:'ACOSH';    E: @dnArCh;     A:1)
   ,(N:'ACTG';     E: @dnArcCoTan; A:1)
   ,(N:'ARCCOS';   E: @dnArcCos;   A:1)
   ,(N:'ARCCOSEC'; E: @dnArcCosec; A:1)
   ,(N:'ARCCOSH';  E: @dnArCh;     A:1)
   ,(N:'ARCCOTAN'; E: @dnArcCoTan; A:1)
   ,(N:'ARCCTG';   E: @dnArcCoTan; A:1)
   ,(N:'ARCH';     E: @dnArCh;     A:1)
   ,(N:'ARCSEC';   E: @dnArcSec;   A:1)
   ,(N:'ARCSIN';   E: @dnArcSin;   A:1)
   ,(N:'ARCSINH';  E: @dnArSh;     A:1)
   ,(N:'ARCTAN';   E: @dnArcTan;   A:1)
   ,(N:'ARCTH';    E: @dnArCth;    A:1)
   ,(N:'ARSH';     E: @dnArSh;     A:1)
   ,(N:'ARTH';     E: @dnArTh;     A:1)
   ,(N:'ASH';      E: @dnArSh;     A:1)
   ,(N:'ASIN';     E: @dnArcSin;   A:1)
   ,(N:'ASINH';    E: @dnArSh;     A:1)
   ,(N:'ATAN';     E: @dnArcTan;   A:1)
   ,(N:'ATANH';    E: @dnArTh;     A:1)
   ,(N:'ATH';      E: @dnArTh;     A:1)
   ,(N:'CH';       E: @dnCh;       A:1)
   ,(N:'COS';      E: @dnCos;      A:1)
   ,(N:'COSEC';    E: @dnCoSec;    A:1)
   ,(N:'COSH';     E: @dnCh;       A:1)
   ,(N:'COTAN';    E: @dnCoTan;    A:1)
   ,(N:'COTANH';   E: @dnCth;      A:1)
   ,(N:'CTAN';     E: @dnCoTan;    A:1)
   ,(N:'CTANH';    E: @dnCth;      A:1)
   ,(N:'CTG';      E: @dnCoTan;    A:1)
   ,(N:'CTH';      E: @dnCth;      A:1)
   ,(N:'EXP';      E: @dnExp;      A:1)
   ,(N:'FACT';     E: @dnFact;     A:1)
   ,(N:'DFACT';    E: @dnDFact;    A:1)
   ,(N:'GRAD';     E: @dnGrad;     A:1)
   ,(N:'IF';       E: @dnIf;       A:3)
   ,(N:'LG';       E: @dnLg;       A:1)
   ,(N:'LN';       E: @dnLn;       A:1)
   ,(N:'LOG';      E: @dnLog;      A:2)
   ,(N:'NOT';      E: @dnNot;      A:1)
   ,(N:'PI';       E: @dnPi;       A:0)
   ,(N:'RAD';      E: @dnRad;      A:1)
   ,(N:'ROOT';     E: @dnRoot;     A:2)
   ,(N:'ROUND';    E: @dnRound;    A:1)
   ,(N:'SEC';      E: @dnSec;      A:1)
   ,(N:'SH';       E: @dnSh;       A:1)
   ,(N:'SIGN';     E: @dnSign;     A:1)
   ,(N:'SIN';      E: @dnSin;      A:1)
   ,(N:'SINH';     E: @dnSh;       A:1)
   ,(N:'SQR';      E: @dnSqr;      A:1)
   ,(N:'SQRT';     E: @dnSqrt;     A:1)
   ,(N:'TAN';      E: @dnTan;      A:1)
   ,(N:'TANH';     E: @dnTh;       A:1)
   ,(N:'TG';       E: @dnTan;      A:1)
   ,(N:'TH';       E: @dnTh;       A:1)
   );

BinOpsMax = 28;
BinOpsTab: array[0..BinOpsMax-1] of string[3] =
('>',   '>=',  '<',   '<=',  '<>', '!=',  '#',  '=',  { Relational }
 '+',   '-',   'OR',  'XOR', '||', '|',   '\',        { Adding }
 '*',   '/',   'DIV', 'MOD', '%',  'AND', '&&', '&',
 'SHR', 'SHL', '<<',  '>>',                           { Multiplying }
 '^');                                                { Power }

var Depth: byte;

function DoEval(start, stop: byte): CReal; forward;

{ Flash >>> }
function IsDigit(Expr: String): Boolean;
{ Finds out whether the Expr is a number or not }
begin
IsDigit:=(Pos(Expr[1],'0123456789.')>0)
end;

function IsFunction(s: string; start, stop: byte; var k: byte) : boolean;
var i,j: byte;
begin
IsFunction:=False;
if (start<=0) or (stop>length(s)) then exit;
while s[start] in CmdSign do inc(start);
while s[stop] in CmdSign do dec(stop);
if not (s[stop] in ['A'..'Z']) then exit;
for j:=8 downto 2 do
if stop-start>=j-1 then
 begin
 cmd:=copy(s,stop-j+1,j);
 if (cmd[j]='H') and (cmd[j-1] in ['A'..'F']) then exit;
 for i:=0 to MaxFunctions-1 do
  if cmd = FnTab[i].N then
   begin
   k:=FnTab[i].A;
   IsFunction:=True; exit;
   end;
 end;
end;

function IsBinOp(s: string; start, stop: byte) : boolean;
var i,j: byte;
begin
IsBinOp:=False;
if (start<=0) or (stop>length(s)) then exit;
for j:=3 downto 1 do
if stop-start>=j-1 then
 begin
 cmd:=copy(s,stop-j+1,j);
 if (cmd[j]='H') and (cmd[j-1] in ['A'..'F']) then exit;
 for i:=0 to BinOpsMax-1 do
  if cmd = BinOpsTab[i] then
   begin
   IsBinOp:=True; exit;
   end;
 end;
end;
{ Flash <<< }

function Evalue(const as: string; aVGF: VarGetFunc): CReal;
var LeftBrackets, RightBrackets,  { Flash }
    i,j,k: byte;                  { Flash }
    Brackets: Integer;
begin
{--- start -------- Eugeny Zvyagintzev ---- 14-06-2002 ----}
{We have to check for empty line}
 If As = '' Then
  Begin
   EValue:=0;
   Exit;
  End;
{--- finish -------- Eugeny Zvyagintzev ---- 14-06-2002 ----}
{--- start -------- Eugeny Zvyagintzev ---- 27-08-2002 ----}
{We have to store string for calculation in a recursive call}
 If S <> Nil Then
  Begin
   If StrToCalcList = Nil Then
    New(StrToCalcList,Init(10,10));
   StrToCalcList^.Insert(S);
  End;
{--- finish -------- Eugeny Zvyagintzev ---- 27-08-2002 ----}
 New(S);
 S^:=UpStrg(DelSpaces(as));

 { Flash >>> }
 LeftBrackets:=0; RightBrackets:=0; ErrCode:=0; Evalue:=0; Brackets:=0;
 for i:=1 to Length(S^) do   { Counting brackets }
    begin
    if not (S^[i] in CmdSign) and not (S^[i] in ['A'..'Z','(',')',',','$',':'])
       and not (IsDigit(S^[i])) then
       ErrCode:=Unknown_Symbol
    else
    if S^[i]='(' then begin Inc(LeftBrackets); Inc(Brackets) end else
    if S^[i]=')' then begin Inc(RightBrackets); Dec(Brackets) end;
    if Brackets<0 then begin ErrCode:=Syntax_Error; break; end;
    end;
 if LeftBrackets>RightBrackets then ErrCode:=Right_Bracket_Missing else
 if LeftBrackets<RightBrackets then ErrCode:=Left_Bracket_Missing else
 begin
 if ErrCode<>0 then begin EvalueError:=true; Evalue:=ErrCode; exit end
 else
 for i:=1 to Length(S^)-1 do
    begin
    if (S^[i]='(') and (S^[i+1]=')') then ErrCode:=Syntax_Error
    else
    if (IsDigit(S^[i]) and (S^[i+1]='('))
    or (IsDigit(S^[i+1]) and (S^[i]=')'))
    or (IsDigit(S^[i]) and (S^[i+1]='$'))
    or ((S^[i]=')') and (S^[i+1]='(')) then ErrCode:=Operation_Expected
    else
    if (S^[i]='(') and (S^[i+1] in CmdSign) and not (S^[i+1] in ['+','-','~']) then
       ErrCode:=Syntax_Error
    else
    if ((S^[i+1]=')') and (S^[i] in CmdSign) and (S^[i]<>'!')) then ErrCode:=Syntax_Error
    else
    if (S^[i]='!') then
     begin
     if S^[i+1]='.' then ErrCode:=Syntax_Error else
     if not (S^[i+1] in CmdSign) and not (S^[i+1] in [',',')']) then ErrCode:=Operation_Expected;
     end;
    end;
 end;
 if ErrCode<>0 then
   begin EvalueError:=true; Evalue:=ErrCode; end
 { Flash <<< }

 else begin
 VGF:=aVGF;
 Depth:=0; { Flash }
 EvalueError:=false;
{$IFNDEF BIT_32} SetInt75Handler; {$ENDIF}
 Evalue:=DoEval(1, Length(S^));
{$IFNDEF BIT_32} RestoreInt75Handler; {$ENDIF}
 end;
{--- start -------- Eugeny Zvyagintzev ---- 27-08-2002 ----}
{We have to store string for calculation in a recursive call}
 If S <> Nil Then Begin Dispose(S); S:=Nil; End;
 If StrToCalcList <> Nil Then
  Begin
   S:=StrToCalcList^.At(StrToCalcList^.Count-1);
   StrToCalcList^.AtDelete(StrToCalcList^.Count-1);
   If StrToCalcList^.Count = 0 Then
    Begin
     Dispose(StrToCalcList,Done);
     StrToCalcList:=Nil;
    End;
  End;
{--- finish -------- Eugeny Zvyagintzev ---- 27-08-2002 ----}
end;

{Pavel Anufrikov --> } { Sergey Biryukov >>> }

function TestSignE(start,i : byte) : boolean;
begin
 TestSignE:=false;
 if (start<=0) or (i>length(s^)) then exit;
 if (s^[i-1]<>'E') or (s^[start]='$') then exit;
 dec(i,2);
 while (i>start) and (s^[i] in ['0'..'9','A'..'F']) do dec(i);
 TestSignE := (s^[i-1]<>'0') or (s^[i]<>'X');
end;

{ <-- Pavel Anufrikov} { Sergey Biryukov <<< }

{ Flash >>> }
function DoEval(start, stop: byte): CReal;
 var i,j,k: byte;
     Value,Value2,Value3: CReal;
     ll: longint;
     sl: byte;

 function GetParams(start, stop, num: byte) : boolean;
 var i,j,k,l: byte;
 begin
 GetParams:=false;
 Value:=0; Value2:=0; Value3:=0;
 case num of
  1: Value:=DoEval(start,stop);
  2: begin
     l:=0; for i:=start to stop do if s^[i]=',' then inc(l);
     if (l<1) then begin ErrCode:=Comma_Missing; EvalueError:=True; exit end;
     if (l>1) then begin ErrCode:=Too_Many_Commas; EvalueError:=True; exit end;
     while s^[start]='(' do inc(start);
     while s^[stop]=')' do dec(stop);
     for i:=start to stop do if s^[i]=',' then break;
     if i in [stop,start] then begin ErrCode:=Argument_Missing; EvalueError:=True; exit end;
     j:=i-1; while s^[j]=')' do dec(j);
     k:=i+1; while s^[k]='(' do inc(k);
     Value:=DoEval(start,j); Value2:=DoEval(k,stop);
     end;
  3: begin
     if (s^[start]<>'(') or (s^[stop]<>')') then
      begin ErrCode:=Incorrect_Usage; EvalueError:=True; exit end;
     while s^[start]='(' do inc(start);
     while s^[stop]=')' do dec(stop);
     l:=0; for i:=start to stop do if s^[i]=',' then inc(l);
     if (l<2) then begin ErrCode:=Comma_Missing; EvalueError:=True; exit end;
     if (l>2) then begin ErrCode:=Too_Many_Commas; EvalueError:=True; exit end;
     if Pos(',,',copy(s^,start,stop-start+1))>0 then begin ErrCode:=Argument_Missing; EvalueError:=True; exit end;
     for i:=start to stop do if s^[i]=',' then break;
     if i in [stop,start] then begin ErrCode:=Argument_Missing; EvalueError:=True; exit end;
     dec(i);
     for l:=i+2 to stop do if s^[l]=',' then break;
     if l in [stop,start] then begin ErrCode:=Argument_Missing; EvalueError:=True; exit end;
     dec(l);
     j:=i+2; if j<stop then while s^[j]='(' do inc(j);
     k:=l+2; if k<stop then while s^[k]='(' do inc(k);
     while s^[l]=')' do dec(l); while s^[i]=')' do dec(i);
     Value:=DoEval(start,i); Value2:=DoEval(j,l); Value3:=DoEval(k,stop);
     end;
 end;
 GetParams:=true;
 end;

 function GetCmd(i: byte) : string;
 var s1: string[10];
 begin
 if s^[i] in ['~','-','+'] then begin GetCmd:=s^[i]; Exit; end;
 s1:='';
 repeat dec(i) until not (s^[i] in CmdSign) or (s^[i]='!'); inc(i);
 repeat s1:=s1+s^[i]; inc(i);
 until (not (s^[i] in CmdSign)) or (s^[i] in ['+','-','~']) or (i>stop);
 GetCmd:=s1;
 end;

begin
  if EvalueError then begin DoEval:=ErrCode; exit end;

  if s^[start]='!' then
   begin EvalueError:=true; ErrCode:=Syntax_Error; DoEval:=ErrCode; exit end;

  {Ignore unfinished operator}
  while (s^[stop] in CmdSign) and (stop>0) and (s^[stop]<>'!') do dec(stop);
  Inc(Depth);
  if Depth>63 then begin EvalueError:=true; ErrCode:=Stack_Overflow; DoEval:=ErrCode; exit end;
  if (s^[start] in CmdSign) and (not (s^[start] in ['+','-','~']) or (start>stop)) then
   begin EvalueError:=true; ErrCode:=Syntax_Error; DoEval:=ErrCode; exit end;
  if (stop<start) then begin DoEval:=0; exit end;
  while (s^[start]='+') and (start<=stop) do inc(start);

  if s^[start]<>'-' then
  for i:=0 to BinOpsMax-1 do
  begin
  k:=length(BinOpsTab[i]);
  if (copy(s^,start,k)=BinOpsTab[i]) or
     (copy(s^,stop-k+1,k)=BinOpsTab[i]) then
   begin
   EvalueError:=True; ErrCode:=Argument_Missing; DoEval:=ErrCode; exit
   end;
  end;

  for i:=start+1 to stop-1 do
   begin
   cmd[1]:=s^[i]; k:=1; if cmd[1] in ['+','-'] then continue;
   if IsBinOp(cmd,1,k) and (s^[i-1] in CmdSign) and (s^[i-1]<>'!')
      and (Pos(copy(s^,i-1,2),DoubleOperations)=0) then
     if (cmd[1] in CmdSign) then
      begin ErrCode:=Two_Operations; EvalueError:=True; exit end
     else
      begin ErrCode:=Argument_Missing; EvalueError:=True; exit end;
   end;

  {Level 0... Simple value or variable}
  {Level 1... Brackets}
  {Level 2... 'sin' 'cos' and other - Functions}
  {Level 3... '^' }
  {Level 4... '*' '/' 'div' 'mod' '%' 'and' '&&' '&' 'shr' 'shl' '<<' '>>' - Multiplying}
  {Level 5... '+' '-' 'or' 'xor' '||' '|' '\' - Adding}
  {Level 6... '>' '>=' '<' '<=' '<>' '!=' '#' '=' - Relational}

  {Proceed level 6... '>' '>=' '<' '<=' '<>' '!=' '#' '=' - Relational}
  for i:=stop downto start do begin
   {if s^[i]='(' then begin DoEval:=1; EvalueError:=True; exit end;}
   if s^[i]=')' then begin
    FreeByte:=0;
    for i:=i downto start do
     case s^[i] of
      '(': begin dec(FreeByte); if FreeByte=0 then break end;
      ')': inc(FreeByte);
     end;
    {if FreeByte>0 then begin DoEval:=1; EvalueError:=True; exit end;}
    continue;
   end;
  if s^[i] in CmdSign then
   begin
   cmd:=GetCmd(i); k:=Length(cmd);
   if (cmd='=') and (s^[i-1]<>'!') then
    begin DoEval:=Byte(DoEval(start, i-k) =  DoEval(i+1, stop)); exit end;
   if (cmd='=') and (s^[i-1]='!') then
    begin DoEval:=Byte(DoEval(start, i-2) <> DoEval(i+1, stop)); exit end;
   if cmd='>' then
    begin DoEval:=Byte(DoEval(start, i-k) >  DoEval(i+1, stop)); exit end;
   if cmd='>=' then
    begin DoEval:=Byte(DoEval(start, i-k) >= DoEval(i+1, stop)); exit end;
   if cmd='<'  then
    begin DoEval:=Byte(DoEval(start, i-k) <  DoEval(i+1, stop)); exit end;
   if cmd='<='  then
    begin DoEval:=Byte(DoEval(start, i-k) <= DoEval(i+1, stop)); exit end;
   if cmd='<>'  then
    begin DoEval:=Byte(DoEval(start, i-k) <> DoEval(i+1, stop)); exit end;
   if cmd='#' then
    begin DoEval:=Byte(DoEval(start, i-k) <> DoEval(i+1, stop)); exit end;
   end;
  end;

  {Proceed level 5... '+' '-' 'or' 'xor' '||' '|' '\' - Adding}
  for i:=stop downto start do begin
   {if s^[i]='(' then begin DoEval:=1; EvalueError:=True; exit end;}
   if s^[i]=')' then begin
    FreeByte:=0;
    for i:=i downto start do
     case s^[i] of
      '(': begin dec(FreeByte); if FreeByte=0 then break end;
      ')': inc(FreeByte);
     end;
    {if FreeByte>0 then begin DoEval:=1; EvalueError:=True; exit end;}
    continue;
   end;
   if s^[i] in CmdSign then
    begin
    cmd:=GetCmd(i); k:=Length(cmd);
    if (cmd='+') and (i>start) and (i<stop)
       and (not IsFunction(s^,start,i-k,sl) or (sl=0))
       and not IsBinOp(s^,start,i-k) and not TestSignE(start,i) then
     begin DoEval:=DoEval(start, i-k) + DoEval(i+1, stop); exit end;
    if (cmd='-') and (i>start) and (i<stop)
       and (not IsFunction(s^,start,i-k,sl) or (sl=0))
       and not IsBinOp(s^,start,i-k) and not TestSignE(start,i) then
     begin DoEval:=DoEval(start, i-k) - DoEval(i+1, stop); exit end;
    if (cmd='||') then
     begin DoEval:=Byte((Round(DoEval(start, i-k))<>0) or (Round(DoEval(i+1, stop))<>0)); exit end;
    if (cmd='|') then
     begin DoEval:=Round(DoEval(start, i-k)) or Round(DoEval(i+1, stop)); exit end;
    if (cmd='\') then
     begin DoEval:=Round(DoEval(start, i-k)) xor Round(DoEval(i+1, stop)); exit end;
   end;
   cmd:=copy(s^,i,3); k:=Length(cmd);
   if (cmd='XOR') then
    begin DoEval:=Round(DoEval(start, i-1)) xor Round(DoEval(i+k, stop)); exit end;
   cmd:=copy(s^,i,2); k:=Length(cmd);
   if (cmd='OR') then
    begin DoEval:=Round(DoEval(start, i-1)) or Round(DoEval(i+k, stop)); exit end;
  end;

  {Proceed level 4... '*' '/' 'div' 'mod' '%' 'and' '&&' '&' 'shr' 'shl' '<<' '>>' - Multiplying}
  for i:=stop downto start do begin
   {if s^[i]='(' then begin DoEval:=1; EvalueError:=True; exit end;}
   if s^[i]=')' then begin
    FreeByte:=0;
    for i:=i downto start do
     case s^[i] of
      '(': begin dec(FreeByte); if FreeByte=0 then break end;
      ')': inc(FreeByte);
     end;
    {if FreeByte>0 then begin DoEval:=1; EvalueError:=True; exit end;}
    continue;
   end;
   if s^[i] in CmdSign then
    begin
    cmd:=GetCmd(i); k:=Length(cmd);
    if cmd='*' then
     begin DoEval:=DoEval(start, i-k) * DoEval(i+1, stop); exit end;
    if cmd='/' then begin
     value:=DoEval(i+1, stop);
     if value<>0 then DoEval:=DoEval(start, i-k) / value
                 else begin EvalueError:=True; ErrCode:=Division_By_Zero; DoEval:=ErrCode; end;
     exit end;
    if cmd='&&' then
     begin DoEval:=Byte((Round(DoEval(start, i-k))<>0) and (Round(DoEval(i+1, stop))<>0)); exit end;
    if cmd='&' then
     begin DoEval:=Round(DoEval(start, i-k)) and Round(DoEval(i+1, stop)); exit end;
    if cmd='<<' then
     begin DoEval:=Round(DoEval(start, i-k)) shl Round(DoEval(i+1, stop)); exit end;
    if cmd='>>' then
     begin DoEval:=Round(DoEval(start, i-k)) shr Round(DoEval(i+1, stop)); exit end;
    if cmd='%' then begin
     value:=DoEval(i+1, stop);
     if value<>0 then DoEval:=Round(DoEval(start, i-k)) mod Round(value)
                 else begin EvalueError:=True; ErrCode:=Division_By_Zero; DoEval:=ErrCode; end;
     exit end;
   end;
   if s^[0]>#3 then begin
    cmd:=copy(s^, i, 3); k:=Length(cmd);
    if Cmd = 'DIV' then begin
     value:=DoEval(i+k, stop);
     if value<>0 then DoEval:=Round(DoEval(start, i-1)) div Round(value)
                 else begin EvalueError:=True; ErrCode:=Division_By_Zero; DoEval:=ErrCode; end;
     exit end;
    if Cmd = 'MOD' then begin
     value:=DoEval(i+k, stop);
     if value<>0 then DoEval:=Round(DoEval(start, i-1)) mod Round(value)
                 else begin EvalueError:=True; ErrCode:=Division_By_Zero; DoEval:=ErrCode; end;
     exit end;
    if Cmd = 'AND' then begin LL:=Round(DoEval(start, i-1));
                              DoEval:=LL and Round(DoEval(i+k, stop)); exit end;
    if Cmd = 'SHL' then begin LL:=Round(DoEval(start, i-1));
                              DoEval:=LL shl Round(DoEval(i+k, stop)); exit end;
    if Cmd = 'SHR' then begin LL:=Round(DoEval(start, i-1));
                              DoEval:=LL shr Round(DoEval(i+k, stop)); exit end;
   end;
  end;

  {Proceed level 3... '^' }
  for i:=stop downto start do begin
   {if s^[i]='(' then begin DoEval:=1; EvalueError:=True; exit end;}
   if s^[i]=')' then begin
    FreeByte:=0;
    for i:=i downto start do
     case s^[i] of
      '(': begin dec(FreeByte); if FreeByte=0 then break end;
      ')': inc(FreeByte);
     end;
    {if FreeByte>0 then begin DoEval:=1; EvalueError:=True; exit end;}
    continue;
   end;
   if s^[i]='^' then
    if not (s^[i-1] in CmdSign) or (s^[i-1]='!') then
     begin DoEval:=Step(DoEval(start, i-1), DoEval(i+1, stop)); exit end
    else begin ErrCode:=Two_Operations; EvalueError:=True; exit end;
   end;

  if s^[start]='-' then begin DoEval:= - DoEval(start+1, stop); exit end;

  { Flash >>> }
  {Proceed level 2... 'sin' 'cos' and other - Functions}
  for k:=8 downto 2 do
  if stop-start>=k-1 then
   begin
   cmd:=copy(s^,start,k);
   if (cmd[k]='H') and (cmd[k-1] in ['A'..'F']) and (k=stop) then break;
    for i:=0 to MaxFunctions-1 do
      begin
      if EvalueError then begin DoEval:=ErrCode; exit end;
      if (cmd = FnTab[i].N) then
       with FnTab[i] do
       begin
       sl:=Length(N);
       if A=0 then
           begin
           if IsDigit(s^[start+sl]) then
            begin EvalueError:=true; ErrCode:=Operation_Expected; DoEval:=ErrCode; exit end;
           if (start+sl<=stop) and (s^[start+sl] in ['A'..'Z']) then
            begin EvalueError:=true; ErrCode:=Unknown_Function; DoEval:=ErrCode; exit end;
           DoEval:=VarEval(E);
           exit;
           end;
       if start+sl-1=stop then
        begin
        if N='CH' then break else
        EvalueError:=True; ErrCode:=Argument_Missing; DoEval:=ErrCode; exit;
        end;
       ErrString:=N;
       if A=1 then begin
        if not GetParams(start+sl,stop,1) then DoEval:=ErrCode
         else DoEval:=FnEval(E)(Value); exit; end;
       if A=2 then begin
        if not GetParams(start+sl,stop,2) then DoEval:=ErrCode
         else DoEval:=FnEval2(E)(Value,Value2); exit; end;
       if A=3 then begin
        if not GetParams(start+sl,stop,3) then DoEval:=ErrCode
         else DoEval:=FnEval3(E)(Value,Value2,Value3); exit; end;
       exit;
       end;
      end;
   end;
  { Flash <<< }

  {Proceed level 1... Brackets}
  if (s^[start]='(') then
   if s^[stop]=')' then begin DoEval:=DoEval(start+1, stop-1); exit end;
                   {else begin DoEval:=1; EvalueError:=True; exit end;}

  {Proceed level 0... simple value or variable}

  { Flash >>> }
  if ((s^[stop]<>'H') and (stop-start>0)) or (stop-start=0) then
   begin
   k:=0;
   for i:=start to stop do
    if s^[i] in ['A'..'Z'] then inc(k);
    if (k=stop-start+1) then
     if IsFunction(s^,start,stop,k) and (k>0) then
      begin ErrCode:=Argument_Missing; EvalueError:=true; end else
      begin ErrCode:=Unknown_Function; EvalueError:=true; end
   end;
  { Flash <<< }

  if EvalueError then begin DoEval:=ErrCode; exit; end;

  if @VGF<>nil then
   If VGF(Copy(s^,start,stop-start+1), value) then
    begin DoEval:=value; exit end;

  if s^[start]='-' then begin DoEval:= - DoEval(start+1, stop); exit end;
  if s^[start]='~' then begin DoEval:=not Round(DoEval(start+1, stop)); exit end;
  if Pos('~',copy(s^,start,stop-start+1))>0 then
   begin EvalueError:=true; ErrCode:=Operation_Expected; DoEval:=ErrCode; exit end;

  {Factorial checking}
  if (stop>1) and (s^[stop]='!') then
   begin
   if s^[stop-1]<>'!' then
    begin
    if s^[start]='-' then DoEval:=-dnFact(DoEval(start+1, stop-1))
    else DoEval:=dnFact(DoEval(start, stop-1));
    ErrString:='FACT';
    end
   else
    begin
    if (stop>2) and (s^[stop-2]='!') then
     begin ErrCode:=Wrong_Argument; EvalueError:=True; ErrString:='FACT'; DoEval:=ErrCode; Exit end
    else
     begin
     if s^[start]='-' then DoEval:=-dnDFact(DoEval(start+1, stop-2))
     else DoEval:=dnDFact(DoEval(start, stop-2));
     ErrString:='DFACT';
     end;
    end;
   Exit;
   end;

  while (s^[start]='(') or (s^[start] in CmdSign) do inc(start);
  while (s^[stop]=')') or (s^[stop] in CmdSign) do dec(stop);
  if GetValue(Copy(s^, Start, stop-start+1), Value) then DoEval:=Value
  else begin EvalueError:=True; DoEval:=ErrCode; end;
  Dec(Depth); { Flash }
end;
{ Flash <<< }

END.
