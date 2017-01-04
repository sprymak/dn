{ÚÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂ¿}
{ÃÅÅ ž­¨â ¤«ï ¤®áây¯  ¤® á¯¥æ¨ «ì­ëå á®ç¥â ­¨© ª« ¢¨èì ÅÅ´}
{ÃÅÅ  €¢â®p: €­â®­ ”¥¤®p®¢ aka DataCompBoy             ÅÅ´}
{ÃÅÅ  ‚ ï« ¢ ç¥áâì Œ â pëª¨­®© “«ìï­ë...               ÅÅ´}
{ÀÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÙ}
unit EXTKbd;

interface
uses DOS, DnIni, DnApp;

implementation
        {-DataCompBoy-}
const HexChars: array[0..15] of byte=(48, 49, 50, 51, 52, 53, 54, 55, 56 ,57, 65, 66, 67, 68, 69, 70);
const Old09Handler: pointer = nil;
procedure Int09Handler(Flags, CS, IP, AX, BX, CX, DX, SI, DI, DS, ES, BP : Word);
interrupt;
var j: byte;
begin
 j:=Port[$60];
 if ShowKeyCode and 2>0 then
  begin
   MemW[$B800:0]:=$0700 + HexChars[j shr $4];
   MemW[$B800:2]:=$0700 + HexChars[j and $f];
  end;
 {Ctrl-<1..0>}
 if (Mem[$40:$17] and (1+2+4+8) = 4) and (j in [$02..$0B]) then
  begin
   dec(MemW[$40:$1A], 2);
   if MemW[$40:$1A]<=$1D then MemW[$40:$1A]:=$1E;
   MemW[$40:MemW[$40:$1A]]:=$D000+(J-1) shl 8;
   j:=Port[$61];
   Port[$61]:=j or $80;
   Port[$61]:=j;
   Port[$20]:=$20;
  end
 else
 {Ctrl-Alt-<Z/X>}
 if (Mem[$40:$17] and (1+2+4+8) = 4+8) and (j in [$2C,$2D]) then
  begin
   dec(MemW[$40:$1A], 2);
   if MemW[$40:$1A]<=$1D then MemW[$40:$1A]:=$1E;
   MemW[$40:MemW[$40:$1A]]:=J shl 8 + J and $0F;
   j:=Port[$61];
   Port[$61]:=j or $80;
   Port[$61]:=j;
   Port[$20]:=$20;
  end
 {Ctrl-Alt-Tilde}
 else
 if (Mem[$40:$17] and (1+2+4+8) = 1+4) and (j = $29) then
  begin
   PApplication(Application)^.DoDump(0);
   dec(MemW[$40:$1A], 2);
   if MemW[$40:$1A]<=$1D then MemW[$40:$1A]:=$1E;
   MemW[$40:MemW[$40:$1A]]:=$0FE00;
   j:=Port[$61];
   Port[$61]:=j or $80;
   Port[$61]:=j;
   Port[$20]:=$20;
  end
 {Ctrl-Alt-Shift-Ins}
 else
 if (Mem[$40:$17] and (1+2+4+8) = 1+4) and (j = $29) then
  begin
   PApplication(Application)^.DoDump(0);
   dec(MemW[$40:$1A], 2);
   if MemW[$40:$1A]<=$1D then MemW[$40:$1A]:=$1E;
   MemW[$40:MemW[$40:$1A]]:=$0FE00;
   j:=Port[$61];
   Port[$61]:=j or $80;
   Port[$61]:=j;
   Port[$20]:=$20;
  end
 {Ctrl-Alt-Brackets}
 else
 if (Mem[$40:$17] and (1+2+4+8) = 4+8) and (j in [$1B,$1A]) then
  begin
   dec(MemW[$40:$1A], 2);
   if MemW[$40:$1A]<=$1D then MemW[$40:$1A]:=$1E;
   MemW[$40:MemW[$40:$1A]]:=j shl 8+j;
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
end;
        {-DataCompBoy-}

var OEP: pointer;
procedure EProc; far;
begin
 ExitProc:=OEP;
 if Old09Handler<>nil then SetIntVec($09, Old09Handler);
end;

begin
 OEP:=ExitProc;
 ExitProc:=@EProc;
 GetIntVec($09, Old09Handler);
 SetIntVec($09,@Int09Handler);
end.