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
UNIT hash;
INTERFACE

Function HashString(S: PChar; Len: Word; Range: Word) : Word; {$IFDEF BIT_16}far;{$ENDIF}

IMPLEMENTATION

Function HashString(S: PChar; Len: Word; Range: Word) : Word;
{$IFNDEF NOASM}
assembler;
const segm= 12; offs=10; ssize=8; modulus=6;
 asm
  push ds; push es; push si; push di;
  sub dx,dx
  sub ax,ax
  mov cx,[bp][ssize]
  jcxz @Exit
  mov ds,[bp][segm]
  mov si,[bp][offs]
  sub bx,bx
  mov di,[bp][ssize]
@loop:
  lodsb
  or al,al
  jz @EndLoop
  xor dl,al
  rol dx,1
  rol dx,1
  rol dx,1
  add dx,di
  sub dx,cx
  loop @loop
@EndLoop:
  mov ax,dx
  sub dx,dx
  div word [bp][modulus]
@Exit:
  mov ax,dx
  pop di; pop si; pop es; pop ds
{$ELSE}
 var Hash: word;
     i: word;
 type PBytesArray=^TBytesArray;
      TBytesArray=array[0..65528] of byte;
 begin
  Hash:=0;
  for i:=0 to len-1 do begin
   if PBytesArray(S)^[i] = 0 then break;
   Hash:=((hi(Hash) shl 8 + (lo(Hash) xor PBytesArray(S)^[i])) shl 3)+Len-i;
  end;
  Hash:=Hash mod Range;
{$ENDIF}
 end;

END.
