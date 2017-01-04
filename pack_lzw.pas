{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source 1.6.RC1
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
unit Pack_LZW;

interface

const
  LZWCprBufSize  = 24*1024;   { For ComLnk }
{ LZWDCprBufSize = 18*1024; }

procedure LZW_Init;
procedure LZW_Done;

function LZW_Compress  (var Source, Dest; Size: Word): Word;
function LZW_Decompress(var Source, Dest): Word;

(*
  Usage:
     LZW_Init; ToSize:=LZW_Compress(From^, To^, FromSize); LZW_Done;
      If ToSize>FromSize then return $FFFF;

     LZW_Init; ToSize:=LZW_Decompress(From^, To^); LZW_Done;
*)


implementation
{$IFNDEF NONBP}
var
{$IFNDEF BIT_32}
  InputOffs,  InputSeg:  Word;
  OutputOffs, OutputSeg: Word;
  Temp_Offs,  Temp_Seg:  Word;
  OutPutOffs2:           Word;
  TempSeg:               Word;
  le6A, le6C, le6E:                     Word;
  le70, le72, le74:                     Word;
  le76, le77:                           Byte;
  le78:                                 Word;
  le7A_0, le7A_2, le7A_4, le7A_6:       Word;
  le82a, le82b:                         Word;

  CodeTable, PrefixTable, SymbolTable, CodeBuffer: LongInt;

  MaxCode, RunCode, RunningBits,
  CurCode, OldCode, CurBuffShift,
  Temp, DataSize, DataSize2:            Word;
{$ELSE BIT_32}
  InputOffs:  Longint;
  OutputOffs: Longint;
  Temp_Offs:  Longint;
  OutPutOffs2:Longint;
  le6A, le6C, le6E:                     SmallWord;
  le70, le72, le74:                     SmallWord;
  le76, le77:                           Byte;
  le78:                                 SmallWord;
  le7A_0, le7A_2, le7A_4, le7A_6:       SmallWord;
  le82a, le82b:                         SmallWord;

  CodeTable, PrefixTable, SymbolTable, CodeBuffer: LongInt;

  MaxCode, RunCode, RunningBits, CurBuffShift,
  CurCode, OldCode, Temp:               SmallWord;
  DataSize, DataSize2:                  Word;
{$ENDIF}

procedure LZWCompr; {$IFDEF BIT_16} near; {$ENDIF}
external;

procedure LZWDecompr; {$IFDEF BIT_16} near; {$ENDIF}
external;

{$IFNDEF BIT_32}
 {$IFNDEF DPMI}
  {$L lzwc2.OBJ}
  {$L lzwd2.OBJ}
 {$ELSE}
  {$L lzwc2.OBP}
  {$L lzwd2.OBP}
 {$ENDIF}
{$ELSE BIT_32}
 {$L lzwc2_32.OBJ}
 {$L lzwd2_32.OBJ}
{$ENDIF}

var
  LZWbuf: Pointer;

const
  InitCount: Integer = 0;

procedure LZW_Init;
begin
  Inc(InitCount);
  if InitCount = 1 then GetMem(LZWbuf,LZWCprBufSize);
end;

procedure LZW_Done;
begin
  if InitCount = 0 then Exit;
  if InitCount = 1 then FreeMem(LZWbuf,LZWCprBufSize);
  Dec(InitCount);
end;

function LZW_Decompress(var Source, Dest): Word;assembler;
{$IFDEF BIT_32}
asm
        mov     ebx, dword ptr LZWbuf
        mov     edx, dword ptr Source
        mov     edi, dword ptr Dest
        call    LZWDecompr
end;
{$ELSE}
asm
        mov     ax, word ptr LZWbuf[0]
        mov     bx, word ptr LZWbuf[2]
        mov     cx, word ptr Source[0]
        mov     dx, word ptr Source[2]
        mov     si, word ptr Dest[0]
        mov     di, word ptr Dest[2]
        call    LZWDecompr
end;
{$ENDIF}

function LZW_Compress(var Source, Dest; Size: Word): Word;assembler;
{$IFDEF BIT_32}
asm
        mov     eax, Size
        mov     DataSize,  eax
        mov     DataSize2, eax
        mov     ebx, dword ptr LZWbuf
        mov     edx, dword ptr Source
        mov     edi, dword ptr Dest
        call    LZWCompr
end;
{$ELSE BIT_16}
asm
        mov     ax, Size
        mov     DataSize, ax
        mov     DataSize2, ax
        mov     ax, word ptr LZWbuf[0]
        mov     bx, word ptr LZWbuf[2]
        mov     cx, word ptr Source[0]
        mov     dx, word ptr Source[2]
        mov     si, word ptr Dest[0]
        mov     di, word ptr Dest[2]
        call    LZWCompr
end;
{$ENDIF}

{$ELSE VP}
procedure LZW_Init; begin end;
procedure LZW_Done; begin end;
function LZW_Compress  (var Source, Dest; Size: Word): Word; begin LZW_Compress:=Word(-1) end;
function LZW_Decompress(var Source, Dest): Word; begin LZW_Decompress:=Word(-1) end;
{$ENDIF}

end.
