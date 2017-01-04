{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source 1.51.05/DOS
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
//      LFNизация выполнена Антоном Федоpовым aka DataCompBoy'ем.
//                  В честь Матаpыкиной Ульяны...
//////////////////////////////////////////////////////////////////////////}

{$o+}
unit Advance3; {Misc stuff}
interface
uses advance, dnini, dos, xTime;

 procedure LowPrec(var A, B: TSize);
 procedure ResourceAccessError;
 function  ValidLngId(LI:string; CheckForHelp:boolean):boolean;
 function  HelpLngId: string;
 function  LngId: string;
 function  MemOK: Boolean;
 function  HotKey(const S: String): Char;
 function  Get100s: LongInt;
 Procedure Sound ( Hz : word );
 Procedure Nosound;

 function  GetSTime: LongInt;
 procedure LocateCursor(X, Y: Byte);
 procedure FillWord(var B;Count, W: Word);
 procedure GetUNIXDate(Julian: LongInt; var Year, Month, Day, Hour, Min, Sec: Word);
 Procedure SetBlink(Mode : boolean);

 Function  FindArg(const Command: string; var S: string): Boolean;

 Function  XDiv1024( X: LongInt ): LongInt;
 Function  LongRatio( X, Y: LongInt ): Integer;

 Function  XRandom(N: Integer): Integer; { Rnd -N..N }
 Function  Min(x,y: LongInt):LongInt;
 Function  Max(x,y: LongInt):LongInt;
 Function  Sgn(x  : integer):integer;
 Procedure DosWrite( S: string );
{DataCompBoy Function  KeyPressed: Boolean;}
 Procedure Beep(N: Byte);
 Function  FindParam(const S: string): Integer;
 Function  Chk4Dos: Boolean;
 Function  WinIsHere: Boolean;
 Function  MemAdjust(L: LongInt): LongInt;
 function  GetEnv(S: string): string;

implementation
uses advance1, advance2;

procedure LowPrec(var A, B: TSize);
begin
  while (A > $FFFF) or (B > $FFFF) do
  begin
    A := A / 2;
    B := B / 2;
  end;
end;

procedure ResourceAccessError;
begin
  RunError(219);
end;

function ValidLngId(LI:string; CheckForHelp:boolean):boolean;
var S1,S2:string;
begin
    ValidLngId:=False;
    S1:=GetEnv('DNDLG');
    if S1='' then S1:=SourceDir;
    if not (S1[Byte(S1[0])] in ['\','/']) then S1:=S1+'\';
    S2:=StartupDir;
    if not (S2[Byte(S2[0])] in ['\','/']) then S2:=S2+'\';
    if (not CheckForHelp) and (not ExistFile(S1+LI+'.DLG')) and
    (not ExistFile(S2+LI+'.DLG')) then Exit;
    if (not CheckForHelp) and (not ExistFile(S1+LI+'.LNG')) and
    (not ExistFile(S2+LI+'.LNG')) then Exit;
    if CheckForHelp then begin
        S1:=SourceDir; if not (S1[Byte(S1[0])] in ['\','/']) then S1:=S1+'\';
        if (not ExistFile(S1+LI+'.HLP')) and (not ExistFile(S2+LI+'.HLP'))
        then Exit
    end;
    ValidLngId:=True;
end;

function HelpLngId: string;
var S:string;
begin
    S:=HelpLanguageOverride;
    if not ValidLngId(S,True) then S:=ActiveLanguage;
    if not ValidLngId(S,True) then S:=GetEnv('DNLNG');
    if not ValidLngId(S,True) then S:='English';
    HelpLngId:=S
end;

function LngId: string;
var S:string;
begin
    S:=ActiveLanguage;
    if not ValidLngId(S,False) then S:=GetEnv('DNLNG');
    if not ValidLngId(S,False) then S:='English';
    LngId:=S
end;

function MemOK: Boolean;
begin
  if (MemAdjust(System.MemAvail) < $8000) or (System.MaxAvail < $4000) then MemOK := False
    else MemOK := True;
end;

function HotKey(const S: String): Char;
var
  P: Word;
begin
  P := Pos('~',S);
  if P <> 0 then HotKey := UpCaseArray[S[P+1]]
  else HotKey := #0;
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

procedure LocateCursor(X, Y: Byte); assembler;
asm
        MOV     AH, 2
        XOR     BX, BX
        MOV     DL, X
        MOV     DH, Y
        INT     10H
end;

 procedure FillWord(var B;Count, W: Word); assembler;
 asm
    cld
    mov  ax, W
    les  di, B
    mov  cx, Count
    rep  stosw
 end;

procedure GetUNIXDate(Julian: LongInt; var Year, Month, Day, Hour, Min, Sec: Word);
  var
    L : LongInt;
    DT: DateTime;
begin
  L := xYMTimeStampToPack(Julian);
  UnpackTime(L, DT);
  Year := DT.Year;
  Month := DT.Month;
  Day := DT.Day;
  Hour := DT.Hour;
  Min := DT.Min;
  Sec := DT.Sec;
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

function FindArg(const Command: string; var S: string): Boolean;
 var I: Integer;
begin
  FindArg := Off;
  for I := 1 to ParamCount do
    begin
      S := UpStrg(ParamStr(I));
      if Copy(S, 1, Length(Command)) = Command then
       begin
         Delete(S, 1, Length(Command));
         FindArg := On;
         Exit
       end;
    end;
  S := '';
end;

Function XDiv1024; assembler;
asm
  mov ax,word ptr X
  mov dx,word ptr X + 2
  mov al,ah
  mov ah,dl
  mov dl,dh
  xor dh,dh
  shr dx,1
  rcr ax,1
  shr dx,1
  rcr ax,1
end;

Function LongRatio;
begin
  While X > ( MaxLongInt div 100 ) do begin
    X := X div 128;
    Y := Y div 128;
  end;
  LongRatio := 100 * X div Y;
end;

FUNCTION XRandom;
begin
  XRandom:=N div 2 - Random(N)
end;

FUNCTION Min; begin if x<y then Min:=X else Min:=Y end;
FUNCTION Max; begin if x<y then Max:=Y else Max:=X end;
FUNCTION Sgn; begin if x>0 then Sgn:=1 else if x<0 then Sgn:=-1 else Sgn:=0 end;

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

procedure Beep;

 procedure Twf(a,b : Word);assembler;
 asm
         cli
         mov dx,60
 @L0:    mov bx,b
 @L1:        mov  al,$4A
             out  $61,al
             mov  cx,dx
             inc  cx
 @RepLoop:   loop @RepLoop
             mov  al,$48
             out  $61,al
             mov  cx,a
             sub  cx,dx
 @Rep2:      loop @Rep2
         dec bx
         jnz @L1
         dec dx
         jnz @L0
         sti
 end;

begin
 case N of
  1 : begin Twf(800,2);Twf(1000,3);end;
  2 : begin Twf(1200,2);Twf(700,3);Twf(900,4);end;
  3 : begin Twf(1200,1);end;
 end;
end;

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


{piwamoto.change.begin}
function WinIsHere: Boolean; assembler;
  asm
    mov ax, $1600
    int $2F
    xor bx, bx
    and al,$7f
    jz  @1
    inc bl
 @1:
    mov ax, bx
  end;
{piwamoto.change.end}

function MemAdjust(L: LongInt): LongInt;
begin
  if Linker <> nil then
  begin
    if L > CL_SafeBuf then L := L - CL_SafeBuf else L := 0;
  end;
  MemAdjust := L;
end;

function GetEnv(S: string): string;
begin
  S := DOS.GetEnv(S); DelSpace(S);
  GetEnv := S;
end;

end.
