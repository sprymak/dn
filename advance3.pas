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

unit Advance3; {Misc stuff}

interface

uses
  Advance, DnIni, Dos, xTime, Objects2
  ;

function GetSTime: LongInt;

function XRandom(N: Integer): Integer; { Rnd -N..N }
procedure DosWrite(S: String);
{DataCompBoy Function  KeyPressed: Boolean;}
function FindParam(const S: String): Integer;
{$IFDEF OS_DOS}
function Chk4Dos: Boolean;
procedure AppendCheck;
procedure DisableAppend;
{$ENDIF}
function GetEnv(S: String): String;
function GetCrc(StartCrc: LongInt; var Buf; BufSize: Word): LongInt;

{$IFDEF OS_DOS}
var
  AppendInstalled: Boolean;
  AppendState: Word;
  {$ENDIF}

implementation
uses
  Advance1, Advance2, Advance6, Commands {Cat}
  ;

{$IFDEF OS_DOS}
procedure AppendCheck;
  begin
  AppendInstalled := False;
  if DOS40 then
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

function GetSTime: LongInt;
  var
    H, M, S, SS: Word;
  begin
  GetTime(H, M, S, SS);
  GetSTime := SS+LongInt(S)*100+LongInt(M)*6000+LongInt(H)*360000;
  end;

function XRandom;
  begin
  XRandom := N div 2-Random(N)
  end;

procedure DosWrite;
  begin
  Writeln(S);
  end;

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

function FindParam(const S: String): Integer;
  var
    I: Integer;
  begin
  FindParam := 0;
  for I := 1 to ParamCount do
    if S = Copy(UpStrg(ParamStr(I)), 1, Length(S)) then
      begin
      FindParam := I;
      Exit
      end;
  if S[1] = '/' then
    FindParam := FindParam('-'+Copy(S, 2, MaxStringLength));
  end;

{$IFDEF OS_DOS}
function Chk4Dos: Boolean;
  assembler;
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

function GetEnv(S: String): String;
  begin
  S := Dos.GetEnv(S);
  DelSpace(S);
  GetEnv := S;
  end;

{-DataCompBoy-}
function UpdateCrc32(CurByte: Byte;
    CurCrc: LongInt): LongInt;
  {-Returns an updated crc32}

  (* Model for inline code below
  UpdateCrc32 := Crc_Table^[Byte(CurCrc xor LongInt(CurByte))] xor
                 ((CurCrc shr 8) and $00FFFFFF);
  *)
  inline;
  begin
  UpdateCrc32 := Crc_Table^[Byte(CurCrc xor LongInt(CurByte))] xor
      ( (CurCrc shr 8) and $00FFFFFF);
  end;

function GetCrc(StartCrc: LongInt; var Buf; BufSize: Word): LongInt;
  type
    AA = array[1..$F000] of Byte;
  var
    CNT: Word;
    CRC: LongInt;
  begin
  if Crc_Table_Empty then
    MakeCRCTable;
  CRC := StartCrc;
  GetCrc := StartCrc;
  if BufSize = 0 then
    Exit;
  for CNT := 1 to BufSize do
    CRC := UpdateCrc32(AA(Buf)[CNT], CRC);
  GetCrc := CRC;
  end;

end.
