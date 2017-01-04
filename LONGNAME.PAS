{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source 1.51.04/DOS
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

unit LongName;
interface

function GetLongFileName(AName: String): String;

implementation

function GetLongFileName(AName: String): String;
var Buffer, BackBuffer: Pointer;
    LSize, IsError, I: Byte;
    S, BName: String;
    S1, O1, S2, O2: Word;
begin
  BName := AName + #0;
  LSize := Length(BName);
  GetMem(Buffer, LSize);
  Move(BName[1], Buffer^, LSize);
  GetMem(BackBuffer, 260);
  IsError := 0;
  S1 := Seg(Buffer^);
  O1 := Ofs(Buffer^);
  S2 := Seg(BackBuffer^);
  O2 := Ofs(BackBuffer^);
  asm
    push ds
    push bp
    mov ax, 7160h
    mov cl, 2
    mov ch, 80h
    mov si, S1
    mov ds, si
    mov si, O1
    mov di, S2
    mov es, di
    mov di, O2
    int 21h
    jc @@error
{piwamoto.change.begin}
    cmp ax,7100h
    jne @@ok
{piwamoto.change.end}
@@error:
    mov IsError,1
@@ok:
    pop bp
    pop ds
  end;
  FreeMem(Buffer, LSize);
  if IsError=1 then GetLongFileName := AName else
  begin
    S[0] := #255;
    Move(BackBuffer^, S[1], 255);
    for I := 1 to 255 do
      if S[I] = #0 then
      begin
        byte(S[0]) := I - 1;
        break;
      end;
    GetLongFileName := S;
  end;
  FreeMem(BackBuffer, 260);
end;

end.
