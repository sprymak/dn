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
unit EXTKbd;

interface

Procedure Init09Handler;

implementation
uses DOS, DnIni, Drivers2;
        {-DataCompBoy-}
const HexChars: array[0..15] of byte=(48, 49, 50, 51, 52, 53, 54, 55, 56 ,57, 65, 66, 67, 68, 69, 70);
const Old09Handler: pointer = nil;
procedure Int09Handler(Flags, CS, IP, AX, BX, CX, DX, SI, DI, DS, ES, BP : Word);
interrupt;
var j: byte;
    b: boolean;
begin
 b := false;
 j := Port[$60];
 if ShowKeyCode and 2>0 then
  begin
   MemW[segB800:0]:=$0700 + HexChars[j shr $4];
   MemW[segB800:2]:=$0700 + HexChars[j and $f];
  end;
 {Ctrl-<1..0>}
 if (Mem[seg0040:$17] and (1+2+4+8) = 4) and (j in [$02..$0B]) then
  begin
   dec(MemW[seg0040:$1A], 2);
   if MemW[seg0040:$1A]<=$1D then MemW[seg0040:$1A]:=$1E;
   MemW[seg0040:MemW[seg0040:$1A]]:=$D000+(J-1) shl 8;
   j:=Port[$61];
   Port[$61]:=j or $80;
   Port[$61]:=j;
   Port[$20]:=$20;
  end
 else
 {Ctrl-Alt-<Z/X>}
 if (Mem[seg0040:$17] and (1+2+4+8) = 4+8) and (j in [$2C,$2D]) then
  begin
   dec(MemW[seg0040:$1A], 2);
   if MemW[seg0040:$1A]<=$1D then MemW[seg0040:$1A]:=$1E;
   MemW[seg0040:MemW[seg0040:$1A]]:=J shl 8 + J and $0F;
   j:=Port[$61];
   Port[$61]:=j or $80;
   Port[$61]:=j;
   Port[$20]:=$20;
  end
 {Ctrl-Shift-Tilde}
 else
 if (Mem[seg0040:$17] and (1+2+4+8) = 1+4) and (j = $29) then
  begin
   b := true;
   dec(MemW[seg0040:$1A], 2);
   if MemW[seg0040:$1A]<=$1D then MemW[seg0040:$1A]:=$1E;
   MemW[seg0040:MemW[seg0040:$1A]]:=$0FE00;
   j:=Port[$61];
   Port[$61]:=j or $80;
   Port[$61]:=j;
   Port[$20]:=$20;
  end
 {Ctrl-Alt-Brackets}
 else
 if (Mem[seg0040:$17] and (1+2+4+8) = 4+8) and (j in [$1B,$1A]) then
  begin
   dec(MemW[seg0040:$1A], 2);
   if MemW[seg0040:$1A]<=$1D then MemW[seg0040:$1A]:=$1E;
   MemW[seg0040:MemW[seg0040:$1A]]:=j shl 8+j;
   j:=Port[$61];
   Port[$61]:=j or $80;
   Port[$61]:=j;
   Port[$20]:=$20;
  end
 {Release BackSpace}
 else
 if (j = $8E) then
  begin
   dec(MemW[seg0040:$1A], 2);
   if MemW[seg0040:$1A]<=$1D then MemW[seg0040:$1A]:=$1E;
   MemW[seg0040:MemW[seg0040:$1A]]:=$FD00;
   j:=Port[$61];
   Port[$61]:=j or $80;
   Port[$61]:=j;
   Port[$20]:=$20;
  end
 {Alt-Semicolon}
 else
 if (Mem[seg0040:$17] and (1+2+4+8) = 8) and (j = $27) then
  begin
   dec(MemW[seg0040:$1A], 2);
   if MemW[seg0040:$1A]<=$1D then MemW[seg0040:$1A]:=$1E;
   MemW[seg0040:MemW[seg0040:$1A]]:=$2700;
   j:=Port[$61];
   Port[$61]:=j or $80;
   Port[$61]:=j;
   Port[$20]:=$20;
  end
 {Alt-Shift-Backspace}
 else
 if (Mem[seg0040:$17] and (4+8) = 8) and
    (Mem[seg0040:$17] and (1+2) <> 0) and (j = $0E) then
  begin
   dec(MemW[seg0040:$1A], 2);
   if MemW[seg0040:$1A]<=$1D then MemW[seg0040:$1A]:=$1E;
   MemW[seg0040:MemW[seg0040:$1A]]:=$9700;
   j:=Port[$61];
   Port[$61]:=j or $80;
   Port[$61]:=j;
   Port[$20]:=$20;
  end
 {Alt-Ctrl-F[1-10]}
 else
 if (Mem[seg0040:$17] and (1+2+4+8) = 4+8) and (j in [$3B..$44]) then
  begin
   dec(MemW[seg0040:$1A], 2);
   if MemW[seg0040:$1A]<=$1D then MemW[seg0040:$1A]:=$1E;
   MemW[seg0040:MemW[seg0040:$1A]]:=$F100+(J-$3B) shl 8;
   j:=Port[$61];
   Port[$61]:=j or $80;
   Port[$61]:=j;
   Port[$20]:=$20;
  end
 {Alt-Ctrl-F[11-12]}
 else
 if (Mem[seg0040:$17] and (1+2+4+8) = 4+8) and (j in [$57,$58]) then
  begin
   dec(MemW[seg0040:$1A], 2);
   if MemW[seg0040:$1A]<=$1D then MemW[seg0040:$1A]:=$1E;
   MemW[seg0040:MemW[seg0040:$1A]]:=$FB00+(J-$57) shl 8;
   j:=Port[$61];
   Port[$61]:=j or $80;
   Port[$61]:=j;
   Port[$20]:=$20;
  end
 {Alt-Shift-[1-9]}
 else
 if (Mem[seg0040:$17] and (4+8) = 4) and
    (Mem[seg0040:$17] and (1+2) <> 0) and
    (j in [$02..$0B]) then
  begin
   dec(MemW[seg0040:$1A], 2);
   if MemW[seg0040:$1A]<=$1D then MemW[seg0040:$1A]:=$1E;
   MemW[seg0040:MemW[seg0040:$1A]]:=$E100+(J-$02) shl 8;
   j:=Port[$61];
   Port[$61]:=j or $80;
   Port[$61]:=j;
   Port[$20]:=$20;
  end
 {Shift-Gray+-}
 else
 if (Mem[seg0040:$17] and (4+8) = 0) and
    (Mem[seg0040:$17] and (1+2) <> 0) and
    (j in [$4E,$4A]) then
  begin
   dec(MemW[seg0040:$1A], 2);
   if MemW[seg0040:$1A]<=$1D then MemW[seg0040:$1A]:=$1E;
   MemW[seg0040:MemW[seg0040:$1A]]:=(J or $80) shl 8;
   j:=Port[$61];
   Port[$61]:=j or $80;
   Port[$61]:=j;
   Port[$20]:=$20;
  end
 {Nothing special}
 else
  asm
   pushf
   call [Old09Handler]
  end;
 if b then DoDump(0, 0);
end;
        {-DataCompBoy-}

var OEP: pointer;
procedure EProc; {$IFDEF BIT_16}far;{$ENDIF}
begin
 ExitProc:=OEP;
 if Old09Handler<>nil then SetIntVec($09, Old09Handler);
end;

Procedure Init09Handler;
begin
 OEP:=ExitProc;
 ExitProc:=@EProc;
 GetIntVec($09, Old09Handler);
 SetIntVec($09,@Int09Handler);
end;

end.
