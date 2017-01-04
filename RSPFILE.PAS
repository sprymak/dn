{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source 1.51.10
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
unit RSPFile;

interface

Procedure PFOpen (Flags, CS, IP, AX, BX, CX, DX, SI, DI, DS, ES, BP : Word);Interrupt;
 {OPEN PACKED FILE WITH HANDLE
  IN:  AL    - open access mode
       DS:DX - pointer to an ASCIIZ file name
  OUT: AX    - file handle if CF not set
             - error code if CF set
 }

Procedure PFSeek (Flags, CS, IP, AX, BX, CX, DX, SI, DI, DS, ES, BP : Word);Interrupt;
 {MOVE PACKED FILE POINTER USING HANDLE
  IN:  AL    - always must be 0
       BX    - file handle
       CX    - high oder word of bytes to move
       DX    - low order word of bytes to move
  OUT: AX    - error code if CF set
       DX:AX - new pointer location if CF not set
 }

Procedure PFRead (Flags, CS, IP, AX, BX, CX, DX, SI, DI, DS, ES, BP : Word);Interrupt;
 {READ FORM PACKED FILE USING HANDLE
  IN:  BX    - file handle
       CX    - number of bytes to read
       DS:DX - pointer to read buffer
  OUT: AX    - number of bytes read if CF not set
             - error code if CF set
 }

Procedure PFClose(Flags, CS, IP, AX, BX, CX, DX, SI, DI, DS, ES, BP : Word);Interrupt;
 {CLOSE PACKED FILE USING HANDLE
  IN:  BX    - file handle
  OUT: AX    - error code if CF set
 }

Function POpen(Name: PChar; Mode: byte): word;
Function PSeek(F: word; Pos: longint): longint;
Function PRead(F: word; Num: word; Buf: pointer): word;
Procedure PClose(F: word);

implementation
uses Dos, GzIo;

Const MinPFile = 4;
      MaxPFile = 4;
      PFiles: Array[MinPFile..MaxPFile] of gzFile = (nil);

function StrPas(Str: PChar): String; assembler;
{$IFNDEF BIT_32}
 asm
        PUSH    DS
        CLD
        LES     DI,Str
        MOV     CX,0FFFFH
        XOR     AL,AL
        REPNE   SCASB
        NOT     CX
        DEC     CX
        LDS     SI,Str
        LES     DI,@Result
        MOV     AL,CL
        STOSB
        REP     MOVSB
        POP     DS
 end;
{$ELSE BIT_32}{$USES EDI,ESI,ECX}
 asm
        CLD
        LEA     EDI,Str
        MOV     ECX,0FFFFFFFFH
        XOR     AL,AL
        REPNE   SCASB
        NOT     ECX
        DEC     ECX
        LEA     ESI,Str
        MOV     EDI,@Result
        MOV     AL,CL
        STOSB
        REP     MOVSB
 end;
{$ENDIF}

Function POpen;
var I: Word;
    S: String;
    savemode: byte;
begin
 for i:=MinPFile to MaxPFile do if PFiles[i] = nil then break;
 if PFiles[i] <> nil then begin InOutRes:=04; exit end;
 s:=StrPas(Name);
 savemode:=filemode;
 filemode:=mode;
 PFiles[i]:=gzopen(s, 'r');
 filemode:=savemode;
 if PFiles[i] <> nil then POpen:=I else begin InOutRes:=04; exit end;
end;

Procedure PFOpen;
var Name: PChar;
    Mode: byte;
    i: byte;
begin
 Flags:=Flags and not fCarry;

 Name:=Ptr(DS, DX);
 Mode:=Lo(AX);
 I:=POpen(Name, Mode);
 AX:=IOResult;
 if AX<>0 then begin Flags:=Flags or fCarry; exit end;
 AX:=i;
end;

Function PSeek;
begin
 if PFiles[F]<>nil
  then PSeek:=GzSeek(PFiles[F], Pos, SEEK_SET)
  else PSeek:=-1;
end;

Procedure PFSeek;
var Pos : Longint;
    w   : word;
begin
 Flags:=Flags and not fCarry;
 if PFiles[BX]=nil then begin Flags:=Flags or fCarry; AX:=06; Exit end;
 Pos:=CX;
 Pos:=(Pos shl 16) + DX;
 Pos:=PSeek(BX, Pos);
 w:=IOResult;
 if w<>0 then begin Flags:=Flags or fCarry; AX:=w end
         else begin DX:=Pos shr 16; AX:=Pos and $FFFF end;
end;

Function PRead(F: word; Num: word; Buf: pointer): word;
var I: byte;
begin
 if PFiles[F]<>nil
  then PRead:=gzread(PFiles[F], buf, num)
  else PRead:=0;
end;

Procedure PFRead;
begin
 Flags:=Flags and not fCarry;
 if PFiles[BX]=nil
  then begin Flags:=Flags or fCarry; AX:=06 end
  else AX:=PRead(BX, CX, Ptr(DS,DX));
end;

Procedure PClose;
begin
 if PFiles[F]<>nil then gzClose(PFiles[F]);
 PFiles[F]:=nil;
end;

Procedure PFClose;
begin
 Flags:=Flags and not fCarry;
 if PFiles[BX]=nil then begin Flags:=Flags or fCarry; AX:=06; exit end;
 PClose(BX);
end;

end.
