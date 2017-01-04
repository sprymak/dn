{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source 1.51.07/DOS
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

unit Advance3; {Misc stuff}
interface
uses advance, dnini, dos, xTime, objects, extramemory;

 procedure LowPrec(var A, B: TSize);
 function  Get100s: LongInt;
 Procedure Sound ( Hz : word );
 Procedure Nosound;

 function  GetSTime: LongInt;
 Procedure SetBlink(Mode : boolean);

 Function  XRandom(N: Integer): Integer; { Rnd -N..N }
 Procedure DosWrite( S: string );
{DataCompBoy Function  KeyPressed: Boolean;}
 Function  FindParam(const S: string): Integer;
 Function  Chk4Dos: Boolean;
 function  GetEnv(S: string): string;
 procedure DisableAppend;
 procedure AppendCheck;


var AppendInstalled: Boolean;
    AppendState: Word;

implementation
uses advance1, advance2;

procedure DisableAppend;
begin
 if AppendInstalled then
   asm
     mov ax, $B707
     mov bx, AppendState
     and bx, $FFFE
     push  bp
     int $2F
     pop bp
   end
end;



procedure LowPrec(var A, B: TSize);
begin
  while (A > $FFFF) or (B > $FFFF) do
  begin
    A := A / 2;
    B := B / 2;
  end;
end;

function Get100s: LongInt;
  var DD,MM,YY, HH, Mn, Sc, Sc100: Word;
begin
  GetDate(YY,MM,DD,Sc100);
  GetTime(HH,Mn,Sc,Sc100);
  Get100s := Sc100+LongInt(Sc)*100+LongInt(Mn)*6000+
             LongInt(HH)*360000+LongInt(DD)*8640000;
end;

Procedure Sound ( Hz : word );assembler;
     asm
        mov     ax,Hz
        cmp     ax,21
        jbe     @@2
        mov     bx,ax
        in      al,061h
        test    al,03
        jnz     @@1
        or      al,03
        out     061h,al
        mov     al,0B6h
        out     043h,al
@@1:    mov     ax,04F38h  {  divider = 144f38h / Hz  }
        mov     dx,014h
        div     bx
        out     042h,al
        mov     al,ah
        out     042h,al
@@2:
    end;

 Procedure Nosound ;assembler;
     asm
        in      al,061h
        and     al,11111100b
        out     061h,al
     end;

function GetSTime: LongInt;
  var H, M, S, SS: Word;
begin
  GetTime(H, M, S, SS);
  GetSTime := SS + LongInt(S)*100 + LongInt(M)*6000 + LongInt(H)*360000;
end;

Procedure SetBlink(Mode : boolean);
var r: registers;
begin
 r.ax:=$1003;
 r.bl:=ord(Mode);
 intr($10, r);
end;
{assembler;
asm
      MOV  AX,1003H
      MOV  BL,Mode
      push bp
      INT  10h
      pop  bp
end;}

FUNCTION XRandom;
begin
  XRandom:=N div 2 - Random(N)
end;

PROCEDURE DosWrite;
var r: registers;
    i: byte;
begin
 if s='' then exit;
 r.ah:=2;
 for i:=1 to length(s) do begin
  r.dl:=byte(s[i]);
  intr($21, r);
 end;
end;
{assembler;
asm
  les  di,S
  xor  cx,cx
  mov  cl,byte ptr es:[di]
  jcxz @empty
@loop:
  inc  di
  mov  ah,2
  mov  dl,byte ptr es:[di]
  push es
  push di
  push cx
  int  21h
  pop  cx
  pop  di
  pop  es
  loop @loop
@empty:
end;}

{DataCompBoy
function KeyPressed: Boolean; assembler;
asm
 mov ah, 1
 int 16h
 mov ax,1
 jnz @@1
 xor ax,ax
@@1:
end;
}

  function FindParam(const S: string): Integer;
    var I: Integer;
  begin
     FindParam := 0;
     for I := 1 to ParamCount do
       if S = Copy(UpStrg(ParamStr(I)),1,Length(S)) then
         begin
           FindParam := I;
           Exit
         end;
     if S[1] = '/' then FindParam := FindParam('-'+Copy(S, 2, 255));
  end;

function Chk4Dos: Boolean; assembler;
  asm
    xor bh, bh
    mov ax, $D44D
    push bp
    int $2F
    pop  bp
    xor bx, bx
    cmp ax, $44DD
    jnz @1
    inc bl
 @1:
    mov ax, bx
  end;


function GetEnv(S: string): string;
begin
  S := DOS.GetEnv(S); DelSpace(S);
  GetEnv := S;
end;

procedure AppendCheck;
 begin
  AppendInstalled := Off;
  if Dos40 then
    asm
      mov  ax, $B700
      push bp
      int  $2F
      pop  bp
      or   al, al
      jz   @@1
      inc  AppendInstalled
      mov  ax, $B706
      push bp
      int  $2F
      pop  bp
      mov  AppendState, BX
@@1:
    end;
 end;

end.
