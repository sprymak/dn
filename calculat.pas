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
//  dn370-calculator(i)_optimization.patch
//  dn31029-WKZ(f)-character_@_is_valid_again.patch
//  dn31029-calc(f)-brackets_fix.patch
//  dn31029-Compile_by_VP.patch
//  dn31029-calculator(f)-use_exceptions_in_vp.patch
//  dn31220-built-in_result_variable.patch
//  dn40412-calculator(f)-exponent_form.patch
//
//  4.9.0
//
//////////////////////////////////////////////////////////////////////////}
{(c) Anton Fedorov aka DataCompBoy, 2000}
{(c) Sergey Biryukov aka Flash, 2002-2003}
{$I STDEFINE.INC}
UNIT Calculat;

INTERFACE

Type CReal = Extended;
     PCReal = ^CReal;

Type VarGetFunc = Function (VarName: string; var Value: CReal): boolean;

function Evalue(const as1: string; aVGF: VarGetFunc): CReal;

Const EvalueError: Boolean = False;

{ Flash >>> }
      Stack_Overflow          = 001;
      Too_Large_Number        = 005;
      Argument_Missing        = 010;   { Syntax Errors }
      Too_Many_Arguments      = 015;
      Syntax_Error            = 020;
      Unknown_Function        = 025;
      Left_Bracket_Missing    = 030;
      Right_Bracket_Missing   = 035;
      Operation_Expected      = 040;
      Unknown_Symbol          = 045;
      Two_Operations          = 050;
      Incorrect_Usage         = 055;
      Division_By_Zero        = 100;   { Math Errors }
      LOG_Wrong_Base          = 105;
      Power_Wrong_Index       = 110;
      Root_Wrong_Index        = 115;
      Wrong_Argument          = 120;
      Power_Wrong_Argument    = 125;
      Incorrect_Form          = 200;

const Epsilon : CReal = 3.4e-4932;

var ErrCode, ErrPos: byte;
    ErrString: string[20];
{ Flash <<< }

IMPLEMENTATION

uses Dos, Advance1, Collect{$IFDEF USESYSUTILS}, SysUtils{$ENDIF};

Var StrToCalcList: PLineCollection; {John_SW}

procedure SetErr(Code: byte; Pos: byte); forward;

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
 else begin dnTh:=1; SetErr(Wrong_Argument,0); end;
end;

function dnCth(x: CReal): CReal;
var K: CReal;
begin
K:=(Exp(x)-Exp(-x));
if Abs(K)>Epsilon
 then dnCth:=(Exp(x)+Exp(-x))/K
 else begin dnCth:=1; SetErr(Wrong_Argument,0); end;
end;

function dnArsh(x: CReal): CReal;
begin
if (x>=0)
 then dnArsh:=Ln(x+Sqrt(Sqr(x)+1))
 else begin dnArsh:=1; SetErr(Wrong_Argument,0); end;
end;

function dnArch(x: CReal): CReal;
begin
if (x>=1)
 then dnArch:=Ln(x+Sqrt(Sqr(x)-1))
 else begin dnArch:=1; SetErr(Wrong_Argument,0); end;
end;

function dnArth(x: CReal): CReal;
begin
if (Abs(X)<1-Epsilon)
 then dnArth:=Ln((1+x)/(1-x))/2
 else begin dnArth:=1; SetErr(Wrong_Argument,0); end;
end;

function dnArcth(x: CReal): CReal;
begin
if (Abs(X)>1+Epsilon)
 then dnArcth:=Ln((x+1)/(x-1))/2
 else begin dnArcth:=1; SetErr(Wrong_Argument,0); end;
end;

function dnSec(x: CReal): CReal;
begin
if (Abs(dnCos(x))>Epsilon)
 then dnSec:=1/dnCos(x)
 else begin dnSec:=1; SetErr(Wrong_Argument,0); end;
end;

function dnRad(x: CReal): CReal;
begin
dnRad:=(x*180)/PI
end;

function dnTan(x: CReal): CReal;
begin
if (Abs(dnCos(x))>Epsilon)
 then dnTan:=dnSin(x)/dnCos(x)
 else begin dnTan:=1; SetErr(Wrong_Argument,0); end;
end;

function dnGrad(x: CReal): CReal;
begin
dnGrad:=(x*PI)/180
end;

function dnFact(x: CReal): CReal;
var R: CReal; I: longint;
begin
if (x>=0) and (x<1755) then
 begin R:=1; for I:=2 to Trunc(x) do R:=R*I; dnFact:=R; end
else begin dnFact:=1; SetErr(Wrong_Argument,0); end;
end;

function dnDFact(x: CReal): CReal;
var R: CReal; I: longint;
begin
if (x>0) and (x<3210) then
 begin
 if Odd(Trunc(x)) then begin R:=1; I:=3; end
                  else begin R:=2; I:=4; end;
 while I<=Trunc(x) do begin R:=R*I; Inc(I,2); end;
 dnDFact:=R;
 end
else begin dnDFact:=1; SetErr(Wrong_Argument,0); end;
end;

function dnCoSec(x: CReal): CReal;
begin
if (Abs(Sin(x))>Epsilon)
 then dnCoSec:=1/Sin(x)
 else begin dnCoSec:=1; SetErr(Wrong_Argument,0); end;
end;

function dnCoTan(x: CReal): CReal;
begin
if (Abs(Sin(x))>Epsilon)
 then dnCoTan:=Cos(x)/Sin(x)
 else begin dnCoTan:=1; SetErr(Wrong_Argument,0); end;
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
else begin dnArcSin:=1; SetErr(Wrong_Argument,0); end;
end;

function dnArcCos(x: CReal): CReal;
begin
if (x>=-1) and (x<=1) then
 if Abs(x+1)<Epsilon then dnArcCos:=pi else
  if Abs(x-1)<Epsilon then dnArcCos:=0 else
   dnArcCos:=dnArcCoTan(x/sqrt(1-sqr(x)))
else begin dnArcCos:=1; SetErr(Wrong_Argument,0); end;
end;

function dnArcSec(x: CReal): CReal;
begin
if Abs(x)>Epsilon
 then dnArcSec:=dnArcCos(1/x)
 else begin dnArcSec:=1; SetErr(Wrong_Argument,0); end;
end;

function dnArcCoSec(x: CReal): CReal;
begin
if Abs(x)>Epsilon
 then dnArcCoSec:=dnArcSin(1/x)
 else begin dnArcCoSec:=1; SetErr(Wrong_Argument,0); end;
end;

function Step(x,a: CReal): CReal;
var S: CReal;
begin
if ((Abs(x)<Epsilon) and (Abs(a)<Epsilon)) then
 begin Step:=1; SetErr(Power_Wrong_Index,0); exit end;
if (Abs(a)<Epsilon) then begin Step:=1; exit end;
if a=int(a) then
 begin
 if Abs(x)>Epsilon then
  begin
  S:=Exp(a*Ln(Abs(x))); if (x=int(x)) and (a>0) then S:=int(S+0.5);
  if (Abs(Trunc(a)) mod 2 = 1) and (x<0) then Step:=-S else Step:=S;
  exit
  end;
 Step:=0; exit;
 end;
if x>Epsilon then
 begin
 S:=Exp(a*Ln(x)); if (a=int(a)) and (x=int(x)) and (a>0) then S:=int(S+0.5);
 Step:=S; exit;
 end;
Step:=1; SetErr(Power_Wrong_Argument,0);
end;

function dnRoot(a,x: CReal): CReal;
var S: CReal;
begin
if (a<1-Epsilon) or (Abs(Frac(a))>Epsilon)
 then begin dnRoot:=1; SetErr(Root_Wrong_Index,0); exit end;
if (Trunc(a) mod 2 = 1) then
 begin
 if Abs(x)>Epsilon then S:=Exp((1/a)*Ln(Abs(x)))
  else begin dnRoot:=1; SetErr(Wrong_Argument,0); exit end;
 if x<0 then dnRoot:=-S else dnRoot:=S; exit;
 end;
if x>Epsilon then begin dnRoot:=Exp((1/a)*Ln(x)); exit end;
dnRoot:=1; SetErr(Wrong_Argument,0);
end;

function dnLog(a,x: CReal): CReal;
begin
if (a<Epsilon) or (Abs(a-1)<Epsilon) then
 begin dnLog:=1; SetErr(LOG_Wrong_Base,0); end else
if (x<Epsilon) or (EvalueError) then
 begin dnLog:=1; SetErr(Wrong_Argument,0); end else
dnLog:=Ln(x)/Ln(a);
end;

function dnLn(x: CReal): CReal;
begin
if (x<Epsilon)
 then begin dnLn:=1; SetErr(Wrong_Argument,0); end
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
if x<0 then begin dnSqrt:=1; SetErr(Wrong_Argument,0) end else
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

function dnIf(V1,V2,V3: CReal): CReal;
begin
if V1<>0 then dnIf:=V2 else dnIf:=V3;
end;

const cmd: string[10] = 'CALCULATOR';
      CmdSign: Set of char = ['>', '<', '=', '#', '+', '-', '|',
                              '\', '/', '*', '&', '~', '%', '^'];
      DoubleOperations: string[40] = '>= <= <> != || && << >>';

procedure SetErrPos(Pos: byte);
begin
if ErrPos=0 then ErrPos:=Pos;
end;

procedure SetErr(Code: byte; Pos: byte);
begin
ErrCode:=Code;
SetErrPos(Pos);
EvalueError:=true;
end;

function IsFunction(s: string; start, stop: byte; var k: byte) : boolean; forward;

Var S: ^String;
    VGF: VarGetFunc;

{$IFNDEF VIRTUALPASCAL}
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
  SetErr(Too_Large_Number,0); { Flash }
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
   ,(N:'DFACT';    E: @dnDFact;    A:1)
   ,(N:'EXP';      E: @dnExp;      A:1)
   ,(N:'FACT';     E: @dnFact;     A:1)
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

const BinOpsMax = 28;
BinOpsTab: array[0..BinOpsMax-1] of string[3] =
('>',   '>=',  '<',   '<=',  '<>', '!=',  '#',  '=',  { Relational }
 '+',   '-',   'OR',  'XOR', '||', '|',   '\',        { Adding }
 '*',   '/',   'DIV', 'MOD', '%',  'AND', '&&', '&',
 'SHR', 'SHL', '<<',  '>>',                           { Multiplying }
 '^');                                                { Power }
BinOpRel = 1; BinOpAdd = 2; BinOpMul = 3;

var Depth: byte;

function BinOpType(cmd: string): byte;
var i: byte;
begin
for i:=0 to BinOpsMax-1 do if BinOpsTab[i]=cmd then break;
if i in [0..7]   then begin BinOpType:=BinOpRel; exit end;
if i in [8..14]  then begin BinOpType:=BinOpAdd; exit end;
if i in [15..26] then begin BinOpType:=BinOpMul; exit end;
end;

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

function Evalue(const as1: string; aVGF: VarGetFunc): CReal;
var LBr, RBr,          { Flash }
    i,j,k: byte;       { Flash }
    c,d: char;
    Brackets: Integer;
begin
{$IFDEF USESYSUTILS}try{$ENDIF} { Flash 29-11-2003 }
{--- start -------- Eugeny Zvyagintzev ---- 14-06-2002 ----}
{We have to check for empty line}
 If As1 = '' Then
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
 New(S); S^:=UpStrg(DelSpaces(as1));

 { Flash >>> }
 LBr:=0; RBr:=0; ErrCode:=0; Evalue:=0; Brackets:=0; ErrPos:=0;
 EvalueError:=false;
 for i:=1 to Length(S^) do   { Counting brackets }
    begin
    c:=S^[i];
    if not (c in CmdSign) and not IsDigit(c) and (c<>'!') and (c<>'@') and
       not (c in ['A'..'Z','(',')',',','$',':']) then
       begin SetErr(Unknown_Symbol,i-1); break end
    else
    if c='(' then begin Inc(LBr); Inc(Brackets) end else
    if c=')' then begin Inc(RBr); Dec(Brackets) end;
    if Brackets<0 then break;
    end;
 if LBr>RBr then SetErr(Right_Bracket_Missing,0);
 if LBr<RBr then SetErr(Left_Bracket_Missing,0);
 if ErrCode<>0 then begin Evalue:=ErrCode; exit end;
 for i:=1 to Length(S^)-1 do
    begin
    c:=S^[i]; d:=S^[i+1];
    if (IsDigit(c) and (d='('))
    or (IsDigit(d) and (c=')'))
    or (IsDigit(c) and (d='$'))
    or ((c='!') and not (d in CmdSign) and not (d in [',',')','!']))
    or ((c=')') and (d='(')) then SetErr(Operation_Expected,i+1)
    else
    if ((c='!') and (d='.'))
    or ((c='(') and (d=')'))
    or ((c='(') and (d in CmdSign) and not (d in ['+','-','~']))
    or ((d=')') and (c in CmdSign)) then SetErr(Syntax_Error,i+1);
    end;
 if ErrCode<>0 then begin Evalue:=ErrCode; exit end;
 { Flash <<< }
 VGF:=aVGF;
 Depth:=0; { Flash }
{$IFNDEF VIRTUALPASCAL} SetInt75Handler; {$ENDIF}
 Evalue:=DoEval(1, Length(S^));
{$IFNDEF VIRTUALPASCAL} RestoreInt75Handler; {$ENDIF}
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
{ Flash 29-11-2003 >>> }
{$IFDEF USESYSUTILS}
 except
  on E: EOverflow do
   begin EvalueError := true; SetErr(Too_Large_Number,0); end;
 end;
{$ENDIF}
{ Flash 29-11-2003 <<< }
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

 { This code by Alexey Korop (AK155) was taken from DN/2 >>> }
 function GetV(S: String; Base: integer; var Value: CReal): boolean;
 { Returns a number in specified scale of notation }
 const
   HexDigits = '0123456789ABCDEFG';
 var RR, FractValue: CReal;
     j: byte;
     c: char;
     d: longint;
     l: byte absolute s;
 begin
  GetV := false;
  if l = 0 then exit;
  RR:=0;
  { integer part }
  for j:=1 to l do
    begin
    c := system.Upcase(S[j]); if c = '.' then break;
    d := Pos(c, HexDigits)-1; if (d < 0) or (d >= Base) then exit;
    RR:=RR*Base + d;
    end;
  Delete(S, 1, j);
  if (l < 1) then begin Value:=RR; GetV:=true; exit end;
  { fractional part }
  FractValue:=1;
  for j:=1 to l do
    begin
    FractValue := FractValue / Base;
    d := Pos(S[j], HexDigits)-1; if (d < 0) or (d >= Base) then exit;
    RR := RR + d*FractValue;
    end;
  Value:=RR;
  GetV:=True;
 end;
 { This code by Alexey Korop (AK155) was taken from DN/2 <<< }

{ Flash >>> }
function DoEval(start, stop: byte): CReal;
 var i,j,k: byte;
     V1,V2,V3: CReal;
     ll: longint;
     sl: byte;
     st: string[10];

 function GetDec(var S: String; var Value: CReal): boolean;
{$IFDEF VIRTUALPASCAL}var R: Longint;{$ELSE}var R: Integer;{$ENDIF}
 begin
  Val(S, Value, R);
  GetDec := R=0;
 end;

 function GetValue(S: String; var Value: CReal): boolean;
 var i,j1,j2,k,p,ls: byte;
{$IFNDEF RESULT}Result,{$ENDIF}A,B: boolean;
 begin
  GetValue:=false; ls:=Length(S); if EvalueError then Exit;
  { Checking hexadecimal number }
  A := (S[ls]='H') and (S[1] in ['0'..'9','A'..'F']);
  B := (S[0]>#2) and (S[1]='0') and (S[2]='X');
  if A or B or (S[1]='$') then begin
   if A then Dec(S[0]) else begin DelFC(S); if B then DelFC(S); end;
   Result:=GetV(S, 16, Value);
   if not Result then begin SetErr(Incorrect_Form,start); ErrString:='HEX'; Exit; end;
   {$IFNDEF RESULT}GetValue:=Result;{$ENDIF}Exit;
  end;
  { Checking octal number }
  A := (S[0]>#2) and (S[1]='0') and (S[2] in ['O','Q']);
  if A or (S[ls] in ['O','Q']) then begin
   if A then begin DelFC(S); DelFC(S); end else Dec(S[0]);
   Result:=GetV(S, 8, Value);
   if not Result then begin SetErr(Incorrect_Form,start); ErrString:='OCT'; Exit; end;
   {$IFNDEF RESULT}GetValue:=Result;{$ENDIF}Exit;
  end;
  { Checking binary number }
  A := (S[0]>#2) and (S[1]='0') and (S[2]='B');
  if A or (S[ls]='B') then begin
   if A then begin DelFC(S); DelFC(S); end else Dec(S[0]);
   Result:=GetV(S, 2, Value);
   if not Result then begin SetErr(Incorrect_Form,start); ErrString:='BIN'; Exit; end;
   {$IFNDEF RESULT}GetValue:=Result;{$ENDIF}Exit;
  end;
  if ((S[0]=#2) and (S[1]='0') and (S[2]='X')) or (S[ls]='H') then
   begin SetErr(Incorrect_Form,start); ErrString:='HEX'; Exit; end;
  if S[ls]<>'H' then
   for i:=1 to ls do
    begin
    if S[i] in ['A'..'D','F'..'Z'] then
     begin
     j1:=i; j2:=ls;
     for k:=j1 to ls-1 do
      if (S[k] in ['A'..'D','F'..'Z']) and (IsDigit(S[k+1]) or (S[k+1] in CmdSign))
       then begin j2:=k; break; end;
     if (IsFunction(S,j1,j2,p) or (S[j2] in ['H','B','O','Q'])) and (S[j1-1] in ['0'..'9','E'])
      then SetErr(Operation_Expected,start+j1-1)
      else SetErr(Unknown_Function,start+j1-1);
     Exit;
     end;
    end;
  Result:=GetDec(S, Value);
  if not Result then
    begin   { Flash >>> }
    if Pos('E',S)>0 then ErrString:='EXP' else ErrString:='DEC';
    SetErr(Incorrect_Form,start); Exit;
    end;    { Flash <<< }
{$IFNDEF RESULT}GetValue:=Result;{$ENDIF}Exit;
 end;

 function GetParams(start, stop, num: byte) : boolean;
 var i,j,k,l: byte;
 begin
 GetParams:=false;
 V1:=0; V2:=0; V3:=0;
 if num in [2,3] then
  begin
  l:=0; for i:=start to stop do if s^[i]=',' then inc(l);
  if (l<num-1) then begin SetErr(Argument_Missing,stop); exit end;
  if (l>num-1) then begin SetErr(Too_Many_Arguments,stop); exit end;
  end;
 case num of
  1: V1:=DoEval(start,stop);
  2: begin
     while s^[start]='(' do inc(start); while s^[stop]=')' do dec(stop);
     for i:=start to stop do if s^[i]=',' then break;
     if i=start then SetErr(Argument_Missing,i);
     if i=stop then SetErr(Argument_Missing,i+1);
     if EvalueError then exit;
     j:=i-1; k:=i+1;
     V1:=DoEval(start,j); V2:=DoEval(k,stop);
     end;
  3: begin
     if s^[start]<>'(' then begin SetErr(Incorrect_Usage,start); exit end;
     if s^[stop]<>')' then begin SetErr(Incorrect_Usage,stop); exit end;
     while s^[start]='(' do inc(start); while s^[stop]=')' do dec(stop);
     for i:=start to stop do if s^[i]=',' then break;
     for l:=i+1 to stop do if s^[l]=',' then break;
     if i in [stop,start] then SetErr(Argument_Missing,i);
     if l=i+1 then SetErr(Argument_Missing,l);
     if l=stop then SetErr(Argument_Missing,l+1);
     if EvalueError then exit;
     dec(i); dec(l); j:=i+2; k:=l+2;
     V1:=DoEval(start,i); V2:=DoEval(j,l); V3:=DoEval(k,stop);
     end;
 end;
 GetParams:=true;
 end;

 function GetCmd(i: byte) : string;
 var s1: string[10];
 begin
 if s^[i] in ['~','-','+'] then begin GetCmd:=s^[i]; Exit; end; s1:='';
 repeat dec(i)
 until (not (s^[i] in CmdSign)) or (s^[i+1]='!'); inc(i);
 repeat s1:=s1+s^[i]; inc(i);
 until (not (s^[i] in CmdSign) and (s^[i]<>'!')) or (s^[i] in ['+','-','~']) or (i>stop);
 GetCmd:=s1;
 end;

 function SkipBrackets(start: byte; var j: byte) : boolean;
 var i,k: byte;
 begin
 SkipBrackets:=True;
 if s^[j]=')' then begin
  k:=0;
  for i:=j downto start do
   case s^[i] of
    '(': begin dec(k); if k=0 then break end;
    ')': inc(k);
   end;
  SkipBrackets:=False; j:=i;
  end;
 end;

 function CheckBinarySign(i:byte; c: char): boolean;
 begin
 CheckBinarySign:=False;
 if (cmd=c) and (i>start) and (i<stop)
    and (not IsFunction(s^,start,i-k,sl) or (sl=0))
    and not IsBinOp(s^,start,i-k) and not TestSignE(start,i) then
  CheckBinarySign:=True;
 end;

 function CheckBinOp(i: byte; t: string): boolean;
 begin
 CheckBinOp:=False;
 if (i>stop-length(t)) or (length(s^)<length(t)+2) then Exit;
 if s^[i-1] in ['A'..'Z'] then repeat dec(i) until (i=1) or not (s^[i-1] in ['A'..'Z']);
 cmd:=copy(s^,i,length(t)); k:=Length(cmd);
 if (cmd=t) then
  begin
  CheckBinOp:=True; V1:=DoEval(start, i-1); V2:=DoEval(i+k, stop);
  end;
 end;

 function Divide(cmd: string): CReal;
 begin
 if V2<>0 then
  begin
  if cmd= '/' then Divide:=V1 / V2;
  if (cmd='%') or (cmd='MOD') then Divide:=Round(V1) mod Round(V2);
  if cmd='DIV' then Divide:=Round(V1) div Round(V2);
  end
 else begin SetErr(Division_By_Zero,i); Divide:=ErrCode; end;
 end;

begin
  if EvalueError then begin DoEval:=ErrCode; SetErrPos(i); exit end;

  if s^[start]='!' then
   begin SetErr(Syntax_Error,start); DoEval:=ErrCode; exit end;

  {Ignore unfinished operator}
  while (s^[stop] in CmdSign) and (stop>0) do dec(stop);
  Inc(Depth);
  if Depth>62 then begin SetErr(Stack_Overflow,i); DoEval:=ErrCode; exit end;
  if (s^[start] in CmdSign) and (not (s^[start] in ['+','-','~']) or (start>stop)) then
   begin SetErr(Syntax_Error,start); DoEval:=ErrCode; exit end;
  if (stop<start) then begin DoEval:=0; exit end;
  while (s^[start]='+') and (start<=stop) do inc(start);

  if s^[start]<>'-' then
  for i:=0 to BinOpsMax-1 do
  begin
  k:=length(BinOpsTab[i]); ErrString:=BinOpsTab[i];
  if copy(s^,start,k)=BinOpsTab[i] then SetErr(Argument_Missing,start) else
  if copy(s^,stop-k+1,k)=BinOpsTab[i] then SetErr(Argument_Missing,stop+1);
  if EvalueError then begin DoEval:=ErrCode; exit end;
  end;

  for i:=start+1 to stop-1 do
   begin
   cmd[1]:=s^[i]; k:=1; if cmd[1] in ['+','-'] then continue;
   if IsBinOp(cmd,1,k) and (s^[i-1] in CmdSign)
      and (Pos(copy(s^,i-1,2),DoubleOperations)=0) then
     if (cmd[1] in CmdSign)
      then begin SetErr(Two_Operations,i); exit end
      else begin SetErr(Argument_Missing,i); exit end;
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
   if not SkipBrackets(start,i) or (i in [start,stop]) then continue;
   if s^[i] in CmdSign then
    begin
    cmd:=GetCmd(i); k:=Length(cmd); st:=cmd;
    if BinOpType(cmd)<>BinOpRel then continue;
    V1:=DoEval(start, i-k); V2:=DoEval(i+1, stop); cmd:=st;
    if (cmd='<>') or (cmd='#') or (cmd='!=')
                then DoEval:=Byte(V1 <> V2);
    if cmd='='  then DoEval:=Byte(V1 =  V2);
    if cmd='>'  then DoEval:=Byte(V1 >  V2);
    if cmd='>=' then DoEval:=Byte(V1 >= V2);
    if cmd='<'  then DoEval:=Byte(V1 <  V2);
    if cmd='<=' then DoEval:=Byte(V1 <= V2);
    exit;
    end;
  end;

  {Proceed level 5... '+' '-' 'or' 'xor' '||' '|' '\' - Adding}
  for i:=stop downto start do begin
   if not SkipBrackets(start,i) or (i in [start,stop]) then continue;
   if s^[i] in CmdSign then
    begin
    cmd:=GetCmd(i); k:=Length(cmd); st:=cmd;
    if BinOpType(cmd)<>BinOpAdd then continue;
    if (cmd[1] in ['+','-']) and not CheckBinarySign(i,cmd[1]) then continue;
    V1:=DoEval(start, i-k); V2:=DoEval(i+1, stop); cmd:=st;
    if cmd='+'  then DoEval:=V1 + V2;
    if cmd='-'  then DoEval:=V1 - V2;
    if cmd='||' then DoEval:=Byte((Round(V1)<>0) or (Round(V2)<>0));
    if cmd='|'  then DoEval:=Round(V1) or  Round(V2);
    if cmd='\'  then DoEval:=Round(V1) xor Round(V2);
    exit;
    end;
   if CheckBinOp(i,'XOR') then begin DoEval:=Round(V1) xor Round(V2); exit end;
   if CheckBinOp(i,'OR')  then begin DoEval:=Round(V1) or  Round(V2); exit end;
  end;

  {Proceed level 4... '*' '/' 'div' 'mod' '%' 'and' '&&' '&' 'shr' 'shl' '<<' '>>' - Multiplying}
  for i:=stop downto start do begin
   if not SkipBrackets(start,i) or (i in [start,stop]) then continue;
   if s^[i] in CmdSign then
    begin
    cmd:=GetCmd(i); k:=Length(cmd); st:=cmd;
    if BinOpType(cmd)<>BinOpMul then continue;
    V1:=DoEval(start, i-k); V2:=DoEval(i+1, stop); cmd:=st;
    if cmd='*'  then DoEval:=V1 * V2;
    if cmd='/'  then DoEval:=Divide('/');
    if cmd='%'  then DoEval:=Divide('%');
    if cmd='&&' then DoEval:=Byte((Round(V1)<>0) and (Round(V2)<>0));
    if cmd='&'  then DoEval:=Round(V1) and Round(V2);
    if cmd='<<' then DoEval:=Round(V1) shl Round(V2);
    if cmd='>>' then DoEval:=Round(V1) shr Round(V2);
    exit;
   end;
   if CheckBinOp(i,'DIV') then begin DoEval:=Divide('DIV'); exit end;
   if CheckBinOp(i,'MOD') then begin DoEval:=Divide('MOD'); exit end;
   if CheckBinOp(i,'AND') then begin DoEval:=Round(V1) and Round(V2); exit end;
   if CheckBinOp(i,'SHL') then begin DoEval:=Round(V1) shl Round(V2); exit end;
   if CheckBinOp(i,'SHR') then begin DoEval:=Round(V1) shr Round(V2); exit end;
  end;

  {Proceed level 3... '^' }
  for i:=stop downto start do begin
   if not SkipBrackets(start,i) or (i in [start,stop]) then continue;
   if s^[i]='^' then
    if not (s^[i-1] in CmdSign) then
     begin DoEval:=Step(DoEval(start, i-1), DoEval(i+1, stop)); exit end
    else begin SetErr(Two_Operations,i); exit end;
   end;

  { Flash >>> }
  {Proceed level 2... 'sin' 'cos' and other - Functions}
  for k:=8 downto 2 do
  if stop-start>=k-1 then
   begin
   cmd:=copy(s^,start,k);
   if (cmd[k]='H') and (cmd[k-1] in ['A'..'F']) and (k=stop) then break;
    for i:=0 to MaxFunctions-1 do
     if (cmd = FnTab[i].N) then with FnTab[i] do
      begin
      sl:=Length(N); ErrString:=N;
      if (A<>0) and (start+sl-1=stop) then
       begin
       if N='CH' then break;
       SetErr(Argument_Missing,stop+1); DoEval:=ErrCode; exit;
       end;
      if (A in [1,2,3]) and not GetParams(start+sl,stop,A) then
       begin SetErrPos(start); DoEval:=ErrCode; exit end;
      case A of
       0: begin
          if IsDigit(s^[start+sl]) then
           begin SetErr(Operation_Expected,start+sl); DoEval:=ErrCode; exit end;
          if (start+sl<=stop) and (s^[start+sl] in ['A'..'Z']) then
           begin SetErr(Unknown_Function,start+sl); DoEval:=ErrCode; exit end;
          DoEval:=VarEval(E);
          end;
       1: DoEval:=FnEval(E)(V1);
       2: DoEval:=FnEval2(E)(V1,V2);
       3: DoEval:=FnEval3(E)(V1,V2,V3);
      end;
      if EvalueError then
       begin if A in [1,2,3] then SetErrPos(start+sl+1); DoEval:=ErrCode end;
      exit;
      end;
   end;
  { Flash <<< }

  {Proceed level 1... Brackets}
  if (s^[start]='(') and (s^[stop]=')') then
   begin DoEval:=DoEval(start+1, stop-1); exit end;

  {Proceed level 0... simple value or variable}

  { Flash >>> }
  if ((s^[stop]<>'H') and (stop>start)) or (stop=start) then
   begin
   k:=0;
   for i:=start to stop do
    if s^[i] in ['A'..'Z'] then inc(k);
    if (k=stop-start+1) then
     begin
     ErrString:=copy(s^,i,k);
     if IsFunction(s^,i,stop,k) and (k>0)
      then SetErr(Argument_Missing,stop+1)
      else SetErr(Unknown_Function,start);
     end;
   end;
  { Flash <<< }

  if EvalueError then begin DoEval:=ErrCode; exit; end;

  if @VGF<>nil then
   If VGF(Copy(s^,start,stop-start+1), V1) then
    begin DoEval:=V1; exit end;

  if s^[start]='-' then begin DoEval:= - DoEval(start+1, stop); exit end;
  if s^[start]='~' then begin DoEval:=not Round(DoEval(start+1, stop)); exit end;
  i:=Pos('~',copy(s^,start,stop-start+1));
  if i>0 then begin SetErr(Operation_Expected,i); DoEval:=ErrCode; exit end;

  {Factorial checking}
  if (stop>1) and (s^[stop]='!') then
   begin
   if s^[stop-1]<>'!' then
    begin DoEval:=dnFact(DoEval(start, stop-1)); ErrString:='FACT'; Exit end;
   if (stop>2) and (s^[stop-2]='!') then
    begin SetErr(Wrong_Argument,0); ErrString:='FACT'; DoEval:=ErrCode; Exit end;
   DoEval:=dnDFact(DoEval(start, stop-2));
   ErrString:='DFACT';
   Exit;
   end;

  if GetValue(Copy(s^, start, stop-start+1), V1)
   then DoEval:=V1 else DoEval:=ErrCode;
  Dec(Depth); { Flash }
end;
{ Flash <<< }

END.
