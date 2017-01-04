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
//  dn16rc1-W2K_compatibility_fix-diff156byMV.patch
//  dn16rc1-vp_noasm_compatible.patch
//
//  2.0.0
//  dn31005-bp_to_vp_on_off_true_false.patch
//
//  4.9.0
//
//////////////////////////////////////////////////////////////////////////}
{$I STDEFINE.INC}

unit Advance3; {Misc stuff}
interface
uses advance, dnini, dos, xTime, objects, ExtraMem;

 procedure LowPrec(var A, B: TSize);
 function  Get100s: LongInt;

{$IFNDEF VIRTUALPASCAL}
 Procedure Sound ( Hz : word );
 Procedure Nosound;
{$ENDIF}

 function  GetSTime: LongInt;

 Function  XRandom(N: Integer): Integer; { Rnd -N..N }
 Procedure DosWrite( S: string );
{DataCompBoy Function  KeyPressed: Boolean;}
 Function  FindParam(const S: string): Integer;
{$IFDEF OS_DOS}
 Function  Chk4Dos: Boolean;
 procedure AppendCheck;
 procedure DisableAppend;
{$ENDIF}
 function  GetEnv(S: string): string;
 Function  GetCrc(StartCrc: longint; var Buf; BufSize: word): Longint ;

{$IFDEF OS_DOS}
var AppendInstalled: Boolean;
    AppendState: Word;
{$ENDIF}

implementation
uses advance1, advance2, advance6;

{$IFDEF OS_DOS}
procedure AppendCheck;
 begin
  AppendInstalled := False;
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

procedure DisableAppend;
begin
 if AppendInstalled then
   asm
     mov ax, $B707
     mov bx, AppendState
     and bx, $FFFE
     push bp
     int  $2F
     pop  bp
   end
end;
{$ENDIF}

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

{$IFNDEF VIRTUALPASCAL}
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
{$ENDIF}

function GetSTime: LongInt;
  var H, M, S, SS: Word;
begin
  GetTime(H, M, S, SS);
  GetSTime := SS + LongInt(S)*100 + LongInt(M)*6000 + LongInt(H)*360000;
end;

FUNCTION XRandom;
begin
  XRandom:=N div 2 - Random(N)
end;

PROCEDURE DosWrite;
{$IFDEF VIRTUALPASCAL}
begin
  Writeln(S);
end;
{$ELSE}
{$IFDEF NOASM}
var
    r: {$IFDEF DPMI}DPMIRegisters{$ELSE}Registers{$ENDIF};
    i: byte;
begin
 if s='' then exit;
 r.ah:=2;
 for i:=1 to length(s) do begin
  r.dl:=byte(s[i]);
  {$IFDEF DPMI}SimulateRealModeInt{$ELSE}Intr{$ENDIF}($21, r);
 end;
end;
{$ELSE}
assembler;
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
end;
{$ENDIF}
{$ENDIF}

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

{$IFDEF OS_DOS}
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
{$ENDIF}

function GetEnv(S: string): string;
begin
  S := DOS.GetEnv(S); DelSpace(S);
  GetEnv := S;
end;

        {-DataCompBoy-}
function UpdateCrc32(CurByte : Byte;
                     CurCrc : LongInt) : LongInt;
  {-Returns an updated crc32}

  (* Model for inline code below
  UpdateCrc32 := Crc_Table^[Byte(CurCrc xor LongInt(CurByte))] xor
                 ((CurCrc shr 8) and $00FFFFFF);
  *)
{$IFNDEF VIRTUALPASCAL}
Inline(
  $8C/$DE/               {        MOV     SI, DS }
  $C5/$3E/>Crc_Table/    {        LDS     DI, Crc_Table }
                         {;Get args -- DX:BX = CurCrc, CX = CurByte;}
  $5B/                   {        POP     BX}
  $5A/                   {        POP     DX}
  $59/                   {        POP     CX}
  $52/                   {        PUSH    DX}
  $53/                   {        PUSH    BX      ;Save original CurCrc}
                         {;CX:AX := Get Crc_Table[CurCrc xor CurByte];}
  $31/$CB/               {        XOR     BX, CX  ;DX:BX = CurCrc xor CurByte}
  $30/$FF/               {        XOR     BH, BH  ;Byte(DX:BX)}
  $D1/$E3/               {        SHL     BX, 1   ;LongInt index}
  $D1/$E3/               {        SHL     BX, 1}
  $03/$DF/               {        ADD     BX, DI}
  $8B/$07/               {        MOV     AX, [BX]}
  $8B/$4F/$02/           {        MOV     CX, [BX+2]}
                         {;DX:BX := (CurCrc shr 8) and $00FFFFFF;}
  $5B/                   {        POP     BX      ;Get original CurCrc}
  $5A/                   {        POP     DX}
  $51/                   {        PUSH    CX      ;Save CX}
  $B9/$08/$00/           {        MOV     CX, 8   ;Shift 8 bits}
  $D1/$EA/               {C1:     SHR     DX, 1   ;Hi reg into carry}
  $D1/$DB/               {        RCR     BX, 1   ;Carry into lo reg}
  $E2/$FA/               {        LOOP    C1      ; for 8 bits}
  $81/$E2/$FF/$00/       {        AND     DX,$00FF}
                         {;DX:AX := ES:AX xor DX:BX (sets function result)}
  $59/                   {        POP     CX}
  $31/$D8/               {        XOR     AX, BX}
  $89/$CB/               {        MOV     BX, CX}
  $31/$DA/               {        XOR     DX, BX}
  $8E/$DE);              {        MOV     DS, SI}
        {-DataCompBoy-}
{$ELSE}
Inline;
begin
  UpdateCrc32 := Crc_Table^[Byte(CurCrc xor LongInt(CurByte))] xor
                 ((CurCrc shr 8) and $00FFFFFF);
end;
{$ENDIF}

Function GetCrc(StartCrc: longint; var Buf; BufSize: word): Longint ;
type
  AA = array[1..$F000] of byte ;
var CNT : word;
    CRC : Longint;
 begin
    if crc_table_empty then MakeCRCTable;
    CRC := StartCrc;
    GetCrc := StartCrc;
     if BufSize = 0 then Exit;
       for CNT :=1 to BufSize do
       CRC := UpdateCrc32(AA(Buf)[CNT] , Crc);
    GetCrc := CRC;
 end;


end.
