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
//
//////////////////////////////////////////////////////////////////////////}
{$I STDEFINE.INC}
{(c) Anton Fedorov aka DataCompBoy, 2000}
UNIT Calculat;

INTERFACE

Type CReal = Extended;
     PCReal = ^CReal;

 Type VarGetFunc = Function (VarName: string; var Value: CReal): boolean;

function GetValue(S: String; var Value: CReal): boolean;

function Evalue(const as: string; aVGF: VarGetFunc): CReal;

Const EvalueError: Boolean = False;

{ Flash >>> }
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

var ErrCode: integer;
    ErrString: string[20];
{ Flash <<< }

IMPLEMENTATION

uses Dos, Advance1;
(*{$IFDEF DPMI}{$L Int10.obp}procedure Exc10Handler; external;{$ENDIF}*)

const
  Epsilon = 1e-200;

function Sin (x:CReal):CReal;
begin
 if (not EvalueError)
  then Sin := System.Sin(x)
  else Sin := 1;
end;

function Cos (x:CReal):CReal;
begin
 if (not EvalueError)
  then Cos := System.Cos(x)
  else Cos := 1;
end;

function ArcTan (x:CReal):CReal;
begin
 if (not EvalueError)
  then ArcTan := System.ArcTan(x)
  else ArcTan := 1;
end;

function Sh  (x:CReal):CReal;
begin
 if (not EvalueError)
  then Sh := (Exp(x) - Exp(-x)) / 2
  else Sh := 1;
end;

function Ch  (x:CReal):CReal;
begin
 if (not EvalueError)
  then Ch := (Exp(x) + Exp(-x)) / 2
  else Ch := 1;
end;

function Th  (x:CReal):CReal;
var K:CReal;
begin
 if (not EvalueError)
  then begin
        K := (Exp(x) + Exp(-x));
        If Abs(K) > Epsilon
         then Th := (Exp(x) - Exp(-x)) / K
         else begin Th := 1; ErrCode:=Wrong_Argument; ErrString:='TH'; EvalueError := true; end;
       end
  else Th := 1;
end;

function Cth (x:CReal):CReal;
var K:CReal;
begin
 if (not EvalueError)
  then begin
        K := (Exp(x) - Exp(-x));
        If Abs(K) > Epsilon
         then Cth := (Exp(x) + Exp(-x)) / K
         else begin Cth := 1; ErrCode:=Wrong_Argument; ErrString:='CTH'; EvalueError := true; end;
       end
  else Cth := 1;
end;

function Arsh(x:CReal):CReal;
begin
 if (not EvalueError) and (x >= 0)
  then Arsh := Ln(x + Sqrt(Sqr(x)+1))
  else begin Arsh := 1; ErrCode:=Wrong_Argument; ErrString:='ARSH'; EvalueError := true end;
end;

function Arch(x:CReal):CReal;
begin
 if (not EvalueError) and (x >= 1)
  then Arch := Ln(x + Sqrt(Sqr(x)-1))
  else begin Arch := 1; ErrCode:=Wrong_Argument; ErrString:='ARCH'; EvalueError := true end;
end;

function Arth(x:CReal):CReal;
begin
 if (not EvalueError) and (Abs(X) < 1-Epsilon)
  then Arth := Ln( (1+x) / (1-x) ) / 2
  else begin Arth := 1; ErrCode:=Wrong_Argument; ErrString:='ARTH'; EvalueError := true end;
end;

function Arcth(x:CReal):CReal;
begin
 if (not EvalueError) and (Abs(X) > 1+Epsilon)
  then Arcth := Ln( (x+1) / (x-1) ) / 2
  else begin Arcth := 1; ErrCode:=Wrong_Argument; ErrString:='ARCTH'; EvalueError := true end;
end;

function Sec (x:CReal):CReal;
begin
 if (not EvalueError) and (Abs(Cos(X)) > Epsilon)
  then Sec := 1 / Cos(x)
  else begin Sec := 1; ErrCode:=Wrong_Argument; ErrString:='SEC'; EvalueError := true end;
end;

function Rad (x:CReal):CReal;
begin
 if (not EvalueError)
  then Rad := (x*180) / PI
  else Rad := 1;
end;

function Tan (x:CReal):CReal;
begin
 if (not EvalueError) and (Abs(Cos(X)) > Epsilon)
  then Tan := Sin(x) / Cos(x)
  else begin Tan := 1; ErrCode:=Wrong_Argument; ErrString:='TAN'; EvalueError := true end;
end;

function Grad (x:CReal):CReal;
begin
 if (not EvalueError)
  then Grad := (x*PI) / 180
  else Grad := 1;
end;

function Fact (x:CReal):CReal;
 var R: CReal; I: longint;
begin
 if (not EvalueError) and (x >= 1) and (x <= 1750)
  then begin
        R := 1;
        for I := 1 to Trunc(x) do R := R * I;
        Fact := R;
       end
  else begin Fact := 1; ErrCode:=Wrong_Argument; ErrString:='FACT'; EvalueError := true end;
end;

function CoSec (x:CReal):CReal;
begin
 if (not EvalueError) and (Abs(Sin(X)) > Epsilon)
  then CoSec := 1 / Sin(x)
  else begin CoSec := 1; ErrCode:=Wrong_Argument; ErrString:='COSEC'; EvalueError := true end;
end;

function CoTan (x:CReal):CReal;
begin
 if (not EvalueError) and (Abs(Sin(X)) > Epsilon)
  then CoTan := Cos(x) / Sin(x)
  else begin CoTan := 1; ErrCode:=Wrong_Argument; ErrString:='COTAN'; EvalueError := true end;
end;

function ArcCoTan (x:CReal):CReal;
begin
 if (not EvalueError)
  then ArcCoTan := Pi/2 - ArcTan(x)
  else ArcCoTan := 1;
end;

function ArcSin (x:CReal):CReal;
begin
 if (not EvalueError) and (x >= -1) and (x <= 1) then
  if Abs(x+1) < Epsilon then
   ArcSin := -(pi/2)
  else
   if Abs(x-1) < Epsilon then
    ArcSin := (pi/2)
   else
    ArcSin := ArcTan ( x / sqrt(1-sqr(x)))
 else begin ArcSin := 1; ErrCode:=Wrong_Argument; ErrString:='ARCSIN'; EvalueError := true end;
end;

function ArcCos (x:CReal):CReal;
begin
 if (not EvalueError) and (x >= -1) and (x <= 1) then
  if Abs(x+1) < Epsilon then
   ArcCos := pi
  else
   if Abs(x-1) < Epsilon then
    ArcCos := 0
   else
    ArcCos := ArcCoTan ( x / sqrt(1-sqr(x)))
 else begin ArcCos := 1; ErrCode:=Wrong_Argument; ErrString:='ARCCOS'; EvalueError := true end;
end;

function ArcSec (x:CReal):CReal;
begin
 if (not EvalueError) and (Abs(x) > Epsilon)
  then ArcSec := ArcCos(1/x)
  else begin ArcSec := 1; ErrCode:=Wrong_Argument; ErrString:='ARCSEC'; EvalueError := true end;
end;

function ArcCoSec (x:CReal):CReal;
begin
 if (not EvalueError) and (Abs(x) > Epsilon)
  then ArcCoSec := ArcSin(1/x)
  else begin ArcCoSec := 1; ErrCode:=Wrong_Argument; ErrString:='ARCCOSEC'; EvalueError := true end;
end;

function Step (x,a:CReal):CReal;
var S: CReal;
begin
 if ((Abs(x) < Epsilon) and (Abs(a) < Epsilon)) or (EvalueError)
  then begin Step := 1; ErrCode:=Power_Wrong_Index; EvalueError := true end
  else if (Abs(a) < Epsilon) then Step := 1
        else if (Abs(Frac(a)) < Epsilon) then begin
              if Abs(x) > Epsilon then begin
               S:=Exp(a*Ln(Abs(x)));
               if (a=int(a)) and (x=int(x)) then S:=int(S);  { Flash }
               if (Abs(Trunc(a)) mod 2 = 1) and (x < 0)
                then Step := -S
                else Step := S;
              end else Step := 0;
             end else if x > Epsilon
                       then begin  { Flash >>> }
                            S := Exp(a*Ln(x));
                            if (a=int(a)) and (x=int(x)) then S:=int(S);
                            Step := S
                            end    { Flash <<< }
                       else begin Step := 1; ErrCode:=Power_Wrong_Argument; EvalueError := true end;
end;

function Root(x,a:CReal):CReal;
var S: CReal;
begin
 if (a < 1-Epsilon) or (Abs(Frac(a)) > Epsilon) or (EvalueError)
 then begin Root := 1; ErrCode:=Root_Wrong_Index; EvalueError := true end
 else if (Trunc(a) mod 2 = 1)
       then begin
             if Abs(x) > Epsilon
              then S := Exp((1/a)*Ln(Abs(x)))
              else begin Root := 1; ErrCode:=Wrong_Argument; ErrString:='ROOT'; EvalueError := true end;
             if x < 0 then Root := -S
                      else Root := S
            end
       else if x > Epsilon then Root := Exp((1/a)*Ln(x))
                           else begin Root := 1; ErrCode:=Wrong_Argument; ErrString:='ROOT'; EvalueError := true end;
end;

function Log(a,x:CReal):CReal;
begin
 if (a < Epsilon) or (Abs(a-1) < Epsilon) then
  begin Log := 1; ErrCode:=LOG_Wrong_Base; EvalueError := true end else
 if (x < Epsilon) or (EvalueError) then
  begin Log := 1; ErrCode:=Wrong_Argument; ErrString:='LOG'; EvalueError := true end else
 Log := Ln(x)/Ln(a);
end;

function Ln(x:CReal):CReal;
begin
 if (x < Epsilon) or (EvalueError)
  then begin Ln := 1; ErrCode:=Wrong_Argument; ErrString:='LN'; EvalueError := true end
  else Ln := System.Ln(x);
end;

function GetValue(S: String; var Value: CReal): boolean;

 function GetHex(var S: String; var Value: CReal): boolean;
 var RR, RRR: CReal;
     l: byte absolute S;
     j: byte;
     Invert: boolean;
 begin
  GetHex := false;
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

var i,k: integer;
    Result: boolean;
begin
 GetValue:=false;
 if (S[Length(S)] = 'H') and (S[1] in ['0'..'9','A'..'F']) then begin
  Dec(S[0]);
  Result:=GetHex(S, Value);
  if not Result then begin ErrCode:=Incorrect_Form; ErrString:='HEX'; EvalueError:=true; Exit; end;
  GetValue:=Result;
 end else
 if S[1] = '$' then begin
  DelFC(S);
  Result:=GetHex(S, Value);
  if not Result then begin ErrCode:=Incorrect_Form; ErrString:='HEX'; EvalueError:=true; Exit; end;
  GetValue:=Result;
 end else
 if (S[0]>#2) and (S[1] = '0') and (S[2] = 'X') then begin
  DelFC(S);
  DelFC(S);
  Result:=GetHex(S, Value);
  if not Result then begin ErrCode:=Incorrect_Form; ErrString:='HEX'; EvalueError:=true; Exit; end;
  GetValue:=Result;
 end else
 if (S[Length(S)] in ['O','Q']) then begin
  Dec(S[0]);
  Result:=GetOct(S, Value);
  if not Result then begin ErrCode:=Incorrect_Form; ErrString:='OCT'; EvalueError:=true; Exit; end;
  GetValue:=Result;
 end else
 if (S[Length(S)] = 'B') then begin
  Dec(S[0]);
  Result:=GetBin(S, Value);
  if not Result then begin ErrCode:=Incorrect_Form; ErrString:='BIN'; EvalueError:=true; Exit; end;
  GetValue:=Result;
 end else
 begin
 if ((S[0]=#2) and (S[1] = '0') and (S[2] = 'X')) or (S[Length(S)]='H') then
  begin ErrCode:=Incorrect_Form; ErrString:='HEX'; EvalueError:=true; Exit; end;
 if s[Length(s)]<>'H' then
   begin
   for i:=1 to Length(s)-1 do
     if (s[i] in ['1'..'9']) and (s[i+1] in ['A'..'Z']) and (s[i+1]<>'E') then
      begin ErrCode:=Operation_Expected; EvalueError:=true; Exit; end;
   for i:=1 to Length(s) do
     if (s[i] in ['A'..'Z']) and (s[i]<>'E') then
      begin ErrCode:=Unknown_Function; EvalueError:=true; Exit; end;
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

const cmd: string[10] = 'CALCULATOR';
      CmdSign: Set of char = ['>', '<', '=', '!', '#', '+', '-', '|',
                              '\', '/', '*', '&', '~', '%', '^'];
      LatinABC: Set of char = ['A'..'Z'];  { Flash }

function DoEval(start, stop: byte): CReal; forward;

{ Flash >>> }
function IsDigit(Expr: String): Boolean;
{ Finds out whether the Expr is a number or not }
begin
IsDigit:=(Pos(Expr[1],'0123456789.')>0)
end;
{ Flash <<< }

function Evalue(const as: string; aVGF: VarGetFunc): CReal;
var LeftBrackets, RightBrackets,  { Flash }
    i: byte;                      { Flash }
begin
{--- start -------- Eugeny Zvyagintzev ---- 14-06-2002 ----}
{We have to check for empty line}
 If As = '' Then
  Begin
   EValue:=0;
   Exit;
  End;
{--- finish -------- Eugeny Zvyagintzev ---- 14-06-2002 ----}
 New(S);
 S^:=UpStrg(DelSpaces(as));

 { Flash >>> }

 LeftBrackets:=0; RightBrackets:=0; ErrCode:=0; Evalue:=0;
 for i:=1 to Length(S^) do   { Counting brackets }
    begin
    if not (S^[i] in CmdSign) and not (S^[i] in LatinABC)
       and not (S^[i]='(') and not (S^[i]=')') and not (S^[i]=',')
       and not (S^[i]='$') and not (IsDigit(S^[i])) then
       ErrCode:=Unknown_Symbol   { Unknown symbol }
    else
    if S^[i]='(' then Inc(LeftBrackets) else if S^[i]=')' then Inc(RightBrackets);
    end;
 if LeftBrackets>RightBrackets then ErrCode:=Right_Bracket_Missing else
 if LeftBrackets<RightBrackets then ErrCode:=Left_Bracket_Missing
 else
 if ((S^[1] in CmdSign) and (S^[1]<>'(') and (S^[1]<>'-') and (S^[1]<>'+')
    and (S^[1]<>'~')) then
    ErrCode:=Syntax_Error   { Syntax error }
 else
 for i:=1 to Length(S^)-1 do
    begin
    if (S^[i]='(') and (S^[i+1]=')') then ErrCode:=Argument_Missing
    else
    if (IsDigit(S^[i]) and (S^[i+1]='('))
    or (IsDigit(S^[i+1]) and (S^[i]=')')) then ErrCode:=Operation_Expected
    else
    if (S^[i]='(') and (S^[i+1] in CmdSign) and (S^[i+1]<>'~') and (S^[i+1]<>'-')
       then ErrCode:=Syntax_Error
    else
    if (S^[i] in CmdSign) and (S^[i+1] in CmdSign)
       and not (((S^[i]='>') and (S^[i+1]='=')) or ((S^[i]='<') and (S^[i+1]='=')) or
                ((S^[i]='<') and (S^[i+1]='>')) or ((S^[i]='!') and (S^[i+1]='=')) or
                ((S^[i]='|') and (S^[i+1]='|')) or ((S^[i]='&') and (S^[i+1]='&')) or
                ((S^[i]='<') and (S^[i+1]='<')) or ((S^[i]='>') and (S^[i+1]='>')) or
                ((S^[i]='=') and (S^[i+1]='-')))
       then ErrCode:=Two_Operations   { 2 operations one after another }
    end;

 if ErrCode<>0 then
   begin EvalueError:=true; Evalue:=1; end

 { Flash <<< }

 else begin
 VGF:=aVGF;
 EvalueError:=false;
{$IFNDEF BIT_32} SetInt75Handler; {$ENDIF}
 Evalue:=DoEval(1, Length(S^));
{$IFNDEF BIT_32} RestoreInt75Handler; {$ENDIF}
 end;
 Dispose(S);
end;

{Pavel Anufrikov --> }

function TestSignE(start,i : byte) : boolean;
begin
 TestSignE:=false;
 if (s^[i-1]<>'E') then exit;
 dec(i,2);
 while (i>start) and (s^[i] in ['0'..'9','A'..'F']) do dec(i);
 TestSignE := (s^[i-1]<>'0') or (s^[i]<>'X');
end;

{ <-- Pavel Anufrikov}

function DoEval(start, stop: byte): CReal;
 var i,k: byte;
     value: CReal;
     ll: longint;
 Begin
  if EvalueError then begin DoEval:=ErrCode; exit end;

  {Ignore unfinished operator}
  while s^[stop] in CmdSign do dec(stop);

  if s^[start]='+' then inc(start);

  {Level 6... '>' '>=' '<' '<=' '<>' '!=' '#' '=' - Relational}
  {Level 5... '+' '-' 'or' 'xor' '||' '|' '\' - Adding}
  {Level 4... '*' '/' 'div' 'mod' '%' 'and' '&&' '&' 'shr' 'shl' '<<' '>>' - Multiplying}
  {Level 3... '^' }
  {Level 2... 'sin' 'cos' and other - Functions}
  {Level 1... Brackets}
  {Level 0... simple value or variable}

  {Proceed level 6... '>' '>=' '<' '<=' '<>' '!=' '#' '=' - Relational}

  for i:=stop downto start do begin
   if s^[i]='(' then begin DoEval:=1; EvalueError:=True; exit end;
   if s^[i]=')' then begin
    FreeByte:=0;
    for i:=i downto start do
     case s^[i] of
      '(': begin dec(FreeByte); if FreeByte=0 then break end;
      ')': inc(FreeByte);
     end;
    if FreeByte>0 then begin DoEval:=1; EvalueError:=True; exit end;
    continue;
   end;
   if (s^[i]='>') and (i>start) and
      (((i<stop) and (s^[i+1]<>'>')) or ((i>start+1) and (s^[i-1]<>'>'))) then
    begin DoEval:=Byte(DoEval(start, i-1) >  DoEval(i+1, stop)); exit end;
   if (s^[i-1]='>') and (i-1<stop) and (s^[i]='=') and (i-1>start) then    { Flash }
    begin DoEval:=Byte(DoEval(start, i-2) >= DoEval(i+1, stop)); exit end;
   if (s^[i]='<') and (i>start) and
      (((i<stop) and (s^[i+1]<>'<')) or ((i>start+1) and (s^[i-1]<>'<'))) then
    begin DoEval:=Byte(DoEval(start, i-1) <  DoEval(i+1, stop)); exit end;
   if (s^[i-1]='<') and (i-1<stop) and (s^[i]='=') and (i-1>start) then    { Flash }
    begin DoEval:=Byte(DoEval(start, i-2) <= DoEval(i+1, stop)); exit end;
   if (s^[i-1]='<') and (i-1<stop) and (s^[i]='>') and (i-1>start) then    { Flash }
    begin DoEval:=Byte(DoEval(start, i-2) <> DoEval(i+1, stop)); exit end;
   if (s^[i-1]='!') and (i-1<stop) and (s^[i]='=') and (i-1>start) then    { Flash }
    begin DoEval:=Byte(DoEval(start, i-2) <> DoEval(i+1, stop)); exit end;
   if (s^[i]='#') and (i>start) then
    begin DoEval:=Byte(DoEval(start, i-1) <> DoEval(i+1, stop)); exit end;
   if (s^[i]='=') and (i>start) then
    begin DoEval:=Byte(DoEval(start, i-1) =  DoEval(i+1, stop)); exit end;
  end;

  {Proceed level 5... '+' '-' 'or' 'xor' '||' '|' '\' - Adding}

  for i:=stop downto start do begin
   if s^[i]='(' then begin DoEval:=1; EvalueError:=True; exit end;
   if s^[i]=')' then begin
    FreeByte:=0;
    for i:=i downto start do
     case s^[i] of
      '(': begin dec(FreeByte); if FreeByte=0 then break end;
      ')': inc(FreeByte);
     end;
    if FreeByte>0 then begin DoEval:=1; EvalueError:=True; exit end;
    continue;
   end;
   if (s^[i]='+') and (i>start) and not (s^[i-1] in CmdSign) and not TestSignE(start,i) then
    begin DoEval:=DoEval(start, i-1) + DoEval(i+1, stop); exit end;
   if (s^[i]='-') and (i>start) and not (s^[i-1] in CmdSign) and not TestSignE(start,i) then
    begin DoEval:=DoEval(start, i-1) - DoEval(i+1, stop); exit end;
   if (s^[i]='-') and (i=start) and not (s^[i-1] in CmdSign) then
    begin DoEval:= - DoEval(i+1, stop); exit end;
   if (s^[i]='O') and (i<stop) and (s^[i+1]='R') and (i>1) then
    begin DoEval:=Round(DoEval(start, i-1)) or Round(DoEval(i+2, stop)); exit end;
   if (s^[i]='|') and (i>start) and (s^[i-1]='|') then
    begin DoEval:=Byte((Round(DoEval(start, i-2))<>0) or (Round(DoEval(i+1, stop))<>0)); exit end;
   if (s^[i]='|') and (i>1) then
    begin DoEval:=Round(DoEval(start, i-1)) or Round(DoEval(i+1, stop)); exit end;
   if (s^[i]='X') and (i<stop-1) and (s^[i+1]='O') and (s^[i+2]='R') and (i>1) then
    begin DoEval:=Round(DoEval(start, i-1)) xor Round(DoEval(i+3, stop)); exit end;
   if (s^[i]='\') and (i>1) then
    begin DoEval:=Round(DoEval(start, i-1)) xor Round(DoEval(i+1, stop)); exit end;
  end;

  {Proceed level 4... '*' '/' 'div' 'mod' '%' 'and' '&&' '&' 'shr' 'shl' '<<' '>>' - Multiplying}
  for i:=stop downto start do begin
   if s^[i]='(' then begin DoEval:=1; EvalueError:=True; exit end;
   if s^[i]=')' then begin
    FreeByte:=0;
    for i:=i downto start do
     case s^[i] of
      '(': begin dec(FreeByte); if FreeByte=0 then break end;
      ')': inc(FreeByte);
     end;
    if FreeByte>0 then begin DoEval:=1; EvalueError:=True; exit end;
    continue;
   end;
   if s^[i]='*' then
    begin DoEval:=DoEval(start, i-1) * DoEval(i+1, stop); exit end;
   if s^[i]='/' then begin
    value:=DoEval(i+1, stop);
    if value<>0 then DoEval:=DoEval(start, i-1) / value
                else begin EvalueError:=True; ErrCode:=Division_By_Zero; DoEval:=ErrCode; end;
    exit
   end;
   if (s^[i]='&') and (i>start) and (s^[i-1]='&') then
    begin DoEval:=Byte((Round(DoEval(start, i-2))<>0) and (Round(DoEval(i+1, stop))<>0)); exit end;
   if (s^[i]='&') then
    begin DoEval:=Round(DoEval(start, i-1)) and Round(DoEval(i+1, stop)); exit end;
   if (s^[i]='<') and (i<stop) and (s^[i+1]='<') then
    begin DoEval:=Round(DoEval(start, i-1)) shl Round(DoEval(i+2, stop)); exit end;
   if (s^[i]='>') and (i<stop) and (s^[i+1]='>') then
    begin DoEval:=Round(DoEval(start, i-1)) shr Round(DoEval(i+2, stop)); exit end;
   if s^[i]='%' then begin
    value:=DoEval(i+1, stop);
    if value<>0 then DoEval:=Round(DoEval(start, i-1)) mod Round(value)
                else begin EvalueError:=True; ErrCode:=Division_By_Zero; DoEval:=ErrCode; end;
    exit
   end;
   if s^[0]>#3 then begin
    cmd:=copy(s^, i, 3);
    if Cmd = 'DIV' then begin
     value:=DoEval(i+3, stop);
     if value<>0 then DoEval:=Round(DoEval(start, i-1)) div Round(value)
                 else begin EvalueError:=True; ErrCode:=Division_By_Zero; DoEval:=ErrCode; end;
     exit
    end;
    if Cmd = 'MOD' then begin
     value:=DoEval(i+3, stop);
     if value<>0 then DoEval:=Round(DoEval(start, i-1)) mod Round(value)
                 else begin EvalueError:=True; ErrCode:=Division_By_Zero; DoEval:=ErrCode; end;
     exit
    end;
    if Cmd = 'AND' then begin LL:=Round(DoEval(start, i-1));
                              DoEval:=LL and Round(DoEval(i+3, stop)); exit end;
    if Cmd = 'SHL' then begin LL:=Round(DoEval(start, i-1));
                              DoEval:=LL shl Round(DoEval(i+3, stop)); exit end;
    if Cmd = 'SHR' then begin LL:=Round(DoEval(start, i-1));
                              DoEval:=LL shr Round(DoEval(i+3, stop)); exit end;
   end;
  end;

  {Proceed level 3... '^' }
  for i:=stop downto start do begin
   if s^[i]='(' then begin DoEval:=1; EvalueError:=True; exit end;
   if s^[i]=')' then begin
    FreeByte:=0;
    for i:=i downto start do
     case s^[i] of
      '(': begin dec(FreeByte); if FreeByte=0 then break end;
      ')': inc(FreeByte);
     end;
    if FreeByte>0 then begin DoEval:=1; EvalueError:=True; exit end;
    continue;
   end;
   if s^[i]='^' then
    begin DoEval:=Step(DoEval(start, i-1), DoEval(i+1, stop)); exit end;
  end;

  {Proceed level 2... 'sin' 'cos' and other - Functions}
  If stop-start>1 then begin
   cmd:=copy(s^,start,2);
   if cmd = 'SH'       then begin DoEval:=Sh(DoEval(start+2,stop));       exit end else
   if cmd = 'CH'       then begin DoEval:=Ch(DoEval(start+2,stop));       exit end else
   if cmd = 'TH'       then begin DoEval:=Th(DoEval(start+2,stop));       exit end else
   if cmd = 'LN'       then begin DoEval:=Ln(DoEval(start+2,stop));       exit end else
   if cmd = 'LG'       then begin DoEval:=Ln(DoEval(start+2,stop))/Ln(10);exit end else
   if cmd = 'TG'       then begin DoEval:=Tan(DoEval(start+2,stop));      exit end else
   if cmd = 'IF'       then begin
    if (S^[start+2]<>'(') or (S^[stop]<>')')
     then begin ErrCode:=Incorrect_Usage; ErrString:='IF'; DoEval:=ErrCode; EvalueError:=True; exit end;
    ll:=0; for i:=start+3 to stop-1 do if s^[i]=',' then inc(ll);
    if (ll<1) then begin ErrCode:=Comma_Missing; DoEval:=ErrCode; EvalueError:=True; exit end;
    if (ll>2) then begin ErrCode:=Too_Many_Commas; DoEval:=ErrCode; EvalueError:=True; exit end;
    for i:=start+3 to stop-1 do if s^[i]=',' then break;
    dec(i);
    if ll=2 then begin
     for ll:=i+2 to stop-1 do if s^[ll]=',' then break;
     dec(ll);
    end else ll:=stop-1;
    if DoEval(start+3,i)<>0 then DoEval:=DoEval(i+2,ll)
    else if ll<stop-1 then DoEval:=DoEval(ll+2,stop-1) else DoEval:=0;     exit end else

   if stop-start>2 then begin
    cmd:=copy(s^,start,3);
    if cmd = 'NOT'      then begin DoEval:=not Round(DoEval(start+3,stop));exit end else
    if cmd = 'SIN'      then begin DoEval:=Sin(DoEval(start+3,stop));      exit end else
    if (cmd = 'COS') and (copy(s^,start,5)<>'COSEC')  { Flash }
                        then begin DoEval:=Cos(DoEval(start+3,stop));      exit end else
    if cmd = 'TAN'      then begin DoEval:=Tan(DoEval(start+3,stop));      exit end else
    if cmd = 'CTG'      then begin DoEval:=CoTan(DoEval(start+3,stop));    exit end else
    if cmd = 'SEC'      then begin DoEval:=Sec(DoEval(start+3,stop));      exit end else
    if cmd = 'ABS'      then begin DoEval:=Abs(DoEval(start+3,stop));      exit end else
    if (cmd = 'SQR')       { Mercury }
       and (copy(s^,start,4)<>'SQRT')
                        then begin DoEval:=Sqr(DoEval(start+3,stop));      exit end else
    if cmd = 'RAD'      then begin DoEval:=Rad(DoEval(start+3,stop));      exit end else
    if cmd = 'EXP'      then begin DoEval:=Exp(DoEval(start+3,stop));      exit end else
    if cmd = 'CTH'      then begin DoEval:=Cth(DoEval(start+3,stop));      exit end else
    if cmd = 'LOG'      then begin
     for i:=start+3 to stop do if s^[i]=',' then break;
     if (s^[i]<>',') then begin ErrCode:=Comma_Missing; DoEval:=ErrCode; EvalueError:=True; exit end;
     if (S^[Start+3]<>'(') or (S^[stop]<>')') then
      begin ErrCode:=Incorrect_Usage; ErrString:='LOG'; DoEval:=ErrCode; EvalueError:=True; exit end;
     DoEval:=Log(DoEval(start+4,i-1), DoEval(i+1,stop-1));                  exit end else

    if stop-start>3 then begin
     cmd:=copy(s^,start,4);
     if cmd = 'ATAN'     then begin DoEval:=ArcTan(DoEval(start+4,stop));   exit end else
     if cmd = 'CTAN'     then begin DoEval:=CoTan(DoEval(start+4,stop));    exit end else
     if cmd = 'ACOS'     then begin DoEval:=ArcCos(DoEval(start+4,stop));   exit end else
     if cmd = 'ASIN'     then begin DoEval:=ArcSin(DoEval(start+4,stop));   exit end else
     if cmd = 'FACT'     then begin DoEval:=Fact(DoEval(start+4,stop));     exit end else
     if cmd = 'SQRT'     then begin DoEval:=Sqrt(DoEval(start+4,stop));     exit end else
     if cmd = 'GRAD'     then begin DoEval:=Grad(DoEval(start+4,stop));     exit end else
     if cmd = 'ARSH'     then begin DoEval:=Arsh(DoEval(start+4,stop));     exit end else
     if cmd = 'ARCH'     then begin DoEval:=Arch(DoEval(start+4,stop));     exit end else
     if cmd = 'ARTH'     then begin DoEval:=Arth(DoEval(start+4,stop));     exit end else
     if cmd = 'SIGN'     then begin Value:=DoEval(start+4,stop);
                               if Value = 0 then DoEval:=0 else
                                if Value < 0 then DoEval := -1
                                 else DoEval := 1;                          exit end else
     if cmd = 'ROOT'     then begin
      for i:=start+4 to stop do if s^[i]=',' then break;
      if (s^[i]<>',') then begin ErrCode:=Comma_Missing; DoEval:=ErrCode; EvalueError:=True; exit end;
      if (S^[Start+4]<>'(') or (S^[stop]<>')') then
       begin ErrCode:=Incorrect_Usage; ErrString:='ROOT'; DoEval:=ErrCode; EvalueError:=True; exit end;
      DoEval:=Root(DoEval(i+1,stop-1), DoEval(start+5,i-1));
                   {Root(x,a) => "ROOT (A,X)"}                               exit end else
     if stop-start>4 then begin
      cmd:=copy(s^,start,5);
      if cmd = 'COTAN'    then begin DoEval:=CoTan(DoEval(start+5,stop));    exit end else
      if cmd = 'COSEC'    then begin DoEval:=CoSec(DoEval(start+5,stop));    exit end else
      if cmd = 'ARCTH'    then begin DoEval:=Arcth(DoEval(start+5,stop));    exit end else
      if cmd = 'ROUND'    then begin DoEval:=Int(DoEval(start+5,stop)+0.5);  exit end else
      if stop-start>5 then begin
       cmd:=copy(s^,start,6);
       if cmd = 'ARCSIN'   then begin DoEval:=ArcSin(DoEval(start+6,stop));   exit end else
       if (cmd = 'ARCCOS') and (copy(s^,start,8)<>'ARCCOSEC')  { Flash }
                           then begin DoEval:=ArcCos(DoEval(start+6,stop));   exit end else
       if cmd = 'ARCSEC'   then begin DoEval:=ArcSec(DoEval(start+6,stop));   exit end else
       if cmd = 'ARCTAN'   then begin DoEval:=ArcTan(DoEval(start+6,stop));   exit end else
       if cmd = 'ARCCTG' then begin DoEval:=ArcCoTan(DoEval(start+6,stop));   exit end else  {Mercury}
       if stop-start>6 then begin
        cmd:=copy(s^,start,8);
        if cmd = 'ARCCOTAN' then begin DoEval:=ArcCoTan(DoEval(start+8,stop)); exit end else
        if cmd = 'ARCCOSEC' then begin DoEval:=ArcCoSec(DoEval(start+8,stop)); exit end else
       end;
      end;
     end;
    end;
   end;
  end;

  {Proceed level 1... Brackets}
  if (s^[start]='(') then
   if s^[stop]=')' then begin DoEval:=DoEval(start+1, stop-1); exit end
                   else begin DoEval:=1; EvalueError:=True; exit end;

  {Proceed level 0... simple value or variable}
  if copy(s^,start,2) = 'PI' then begin DoEval:=Pi; exit end;

  { Flash >>> }
  if (s^[stop]<>'H') and (stop-start>0) then
   begin
   k:=0;
   for i:=start to stop do
    if s^[i] in ['A'..'Z'] then inc(k);
    if (k=stop-start+1) then
      begin ErrCode:=Argument_Missing; EvalueError:=true; end;
   end;
  { Flash <<< }

  if EvalueError then begin DoEval:=ErrCode; exit; end;

  if @VGF<>nil then
   If VGF(Copy(s^,start,stop-start+1), value) then
    begin DoEval:=value; exit end;

  if s^[Start]='~' then begin LL := 1; inc(Start); end else LL := 0;
  if GetValue(Copy(s^, Start, stop-start+1), Value) then
   if LL<>0 then DoEval:=not Round(value) else DoEval:=Value
  else begin EvalueError:=True; DoEval:=ErrCode; end;
 end;

END.
