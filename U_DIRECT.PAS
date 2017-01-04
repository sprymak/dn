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
//     "Based on TinyWeb Server by RIT Research Labs."
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

unit u_direct;
interface

  var  VirtualMem:array[1..25,1..80,1..2]of byte;
{$ifndef DPMI}
       VideoMem:array[1..25,1..80,1..2]of byte absolute $B800:$0000;
{$endif}
                    {Y position,
                           X position,
                                 1=Char,2=Attribute}

type
  TWriteBuffer = array[1..80, 1..2] of byte;

  procedure Set_80x25;{set up 3h mode without clear screen    -   80x25}

  procedure PutSymb(x,y:integer;b:byte);
  procedure PutAttr(x,y:integer;b:byte);
  procedure PutExtra(x, y: Integer; ch, c:byte);
  function  GetSymb(x,y:integer):byte;
  function  GetAttr(x,y:integer):byte;
  {new generation of routine}
  procedure dChangeAttr(x1,y1,x2,y2:integer;col:byte);
  function  RealLength(s:string):byte;
  procedure dWrite(x1,y1,x2:integer;s:string);
  procedure dFill(x1,y1,x2,y2:integer;char,attr:byte);
  {with WriteBuffer}
  function  CheckX(x: integer): boolean;
  procedure OutWriteBuffer(X1, Y1, X2: Integer; WB: TWriteBuffer);
  procedure bPutSymb(var WB: TWriteBuffer; x:integer; b:byte);
  procedure bPutAttr(var WB: TWriteBuffer; x:integer; b:byte);
  procedure bPutExtra(var WB: TWriteBuffer; x: Integer; ch, c:byte);
  procedure bChangeAttr(var WB: TWriteBuffer; x1,x2:integer; b:byte);
  procedure bChangeSymb(var WB: TWriteBuffer; x1,x2:integer; b:byte);
  procedure bFill(var WB: TWriteBuffer; x1,x2:integer; ch, col:byte);
  procedure bWrite(var WB: TWriteBuffer; x1,x2:integer; s:string);
  {special ability}
  procedure vPutChar(x,y:integer;b:byte);
  procedure vPutAttr(x,y:integer;b:byte);
  procedure vPutExtra(x, y: Integer; ch, c:byte);
  procedure vChangeAttr(x1,y1,x2,y2:integer;col:byte);
  procedure vWrite(x1,y1,x2:integer;s:string);
  procedure vFill(x1,y1,x2,y2:integer;char,attr:byte);
  procedure ActiveVirtualPage;
  procedure ActiveRealPage;

  function InvalidPoint(X, Y: Integer): Boolean;
  procedure MakeValidRect(var X1, Y1, X2, Y2: Integer);

implementation
const
  XX = 80;
  YY = 25;

  procedure Set_80x25;
    begin
      asm
       mov ah,00h
       mov al,10000011b{03h with save}
       int 10h
      end;
    end;

  function InvalidPoint(X, Y: Integer): Boolean;
  begin
    InvalidPoint := Not ((X>=1) and (X<=80) and (Y>=1) and (Y<=25));
  end;

  procedure MakeValidRect(var X1, Y1, X2, Y2: Integer);
  begin
    if X1 < 1 then X1 := 1;
    if Y1 < 1 then Y1 := 1;
    if X2 > 80 then X2 := 80;
    if Y2 > 25 then Y2 := 25;
  end;

  procedure PutSymb(x,y:integer;b:byte);
    begin
      if InvalidPoint(x, y) then Exit;
{$ifdef DPMI}
      Mem[SegB800:((y-1)*XX+x-1)shl 1]:=b;
{$else}
      VideoMem[y,x,1]:=b;
{$endif}
    end;

  procedure PutAttr(x,y:integer;b:byte);
    begin
      if InvalidPoint(x, y) then Exit;
{$ifdef DPMI}
      Mem[SegB800:(((y-1)*XX+x-1)shl 1)+1]:=b;
{$else}
      VideoMem[y,x,2]:=b;
{$endif}
    end;

  procedure PutExtra(x, y: Integer; ch, c:byte);
  var w: word;
  begin
      if InvalidPoint(x, y) then Exit;
      w := (c shl 8) or ch;
{$ifdef DPMI}
      Move(w, Mem[SegB800:((y-1)*XX+x-1)shl 1], 2);
{$else}
      Move(w, VideoMem[y,x], 2);
{$endif}
  end;

  function  GetSymb(x,y:integer):byte;
    begin
      if InvalidPoint(x, y) then Exit;
{$ifdef DPMI}
      GetSymb:=Mem[SegB800:((y-1)*XX+x-1)shl 1];
{$else}
      GetSymb:=VideoMem[y,x,1];
{$endif}
    end;

  function  GetAttr(x,y:integer):byte;
    begin
      if InvalidPoint(x, y) then Exit;
{$ifdef DPMI}
      GetAttr:=Mem[SegB800:(((y-1)*XX+x-1)shl 1)+1];
{$else}
      GetAttr:=VideoMem[y,x,2];
{$endif}
    end;

  procedure dChangeAttr(x1,y1,x2,y2:integer;col:byte);
   var i,j:integer;
   begin
     MakeValidRect(x1, y1, x2, y2);
{$ifndef DPMI}
     for j:=y1 to y2 do for i:=x1 to x2 do VideoMem[j,i,2]:=col;
{$else}
     for j:=y1 to y2 do for i:=x1 to x2 do PutAttr(i,j,col);
{$endif}
   end;

function RealLength(s: string): byte;
var l, i, r: byte;
begin
  l := byte(s[0]);
  r := l;
  for i := 1 to l do
  if (s[i] = ^Y) or (s[i] = ^Z) then
  begin
    dec(r, 2);
    inc(i);
  end;
  RealLength := r;
end;

  procedure Centered(x1,x2,RealL:integer;var out:string);
   var addL,addR,i:byte;
   begin
     addL:=((x2-x1)+1-RealL)shr 1;
     addR:=(x2-x1)+1-RealL-addL;
     for i:=1 to addL do out:=#$FF+out;
     out:=out+^Z#$FF;
     for i:=1 to addR do out:=out+#$FF;
   end;

  procedure Righted(x1,x2,RealL:integer;var out:string);
   var addL,i:byte;
   begin
     addL:=(x2-x1)+1-RealL;
     for i:=1 to addL do out:=#$FF+out;
   end;

  procedure Lefted(x1,x2,RealL:integer;var out:string);
   var addR,i:byte;
   begin
     addR:=(x2-x1)+1-RealL;
     out:=out+^Z#$FF;
     for i:=1 to addR do out:=out+#$FF;
   end;

  procedure Filered(x1,x2,RealL:integer;var out:string);
   var i:byte;
       ch:char;
   begin
     ch:=out[byte(out[0])];
     for i:=x1+1 to x2 do out:=out+ch;
   end;

  procedure dWrite(x1,y1,x2:integer;s:string);
   var Col,j,o,ll:byte;
       RealL:byte;
       ServCh:char;
       out:string;
   begin
     Col:=$FF;{цвет не менять}
     if (s[1] in [^A..^Y])then
      begin
        ServCh:=s[1];
        out:=copy(s,2,byte(s[0])-1);
        RealL:=RealLength(out);
        if (RealL<(x2-x1+1)) then
         begin
          case ServCh of
           ^C:Centered(x1,x2,RealL,out);
           ^R:Righted(x1,x2,RealL,out);
           ^F:Filered(x1,x2,RealL,out);
           ^L:Lefted(x1,x2,RealL,out);
           else Lefted(x1,x2,RealL,out);
          end;
        end;
      end
     else out:=s;
     o:=0;
     ll:=x2-x1+1;
     for j:=1 to Length(out) do
      begin
        if out[j]=^Z then
         begin
           Col:=byte(out[j+1]);
           inc(j);
         end else
        if out[j]=^Y then
         begin
           if out[j + 1] <> #$FF then PutSymb(x1+o,y1,byte(out[j + 1]));
           if Col<>$FF then PutAttr(x1+o,y1,Col);
           inc(j);
           inc(o);
           if o>=ll then Break;
         end else
         begin
           if out[j] <> #$FF then PutSymb(x1+o,y1,byte(out[j]));
           if Col<>$FF then PutAttr(x1+o,y1,Col);
           inc(o);
           if o>=ll then Break;
         end;
      end;
   end;

  procedure dFill(x1,y1,x2,y2:integer;char,attr:byte);
   var i,j:integer;
   begin
{$ifdef DPMI}
     for i:=x1 to x2 do
       for j:=y1 to y2 do
        begin
          PutSymb(i,j,char);
          PutAttr(i,j,attr);
        end;
{$else}
     MakeValidRect(x1, y1, x2, y2);
     for i:=x1 to x2 do
       for j:=y1 to y2 do
        begin
          VideoMem[j,i,1]:=char;
          VideoMem[j,i,2]:=attr;
        end;
{$endif}
   end;

  {with WriteBuffer}
  function  CheckX(x: integer): boolean;
  begin
    CheckX := (X >= 1) and (X <= 80);
  end;

  procedure OutWriteBuffer(X1, Y1, X2: Integer; WB: TWriteBuffer);
  var Size: Word;
  begin
      Size := (X2 - X1 + 1) shl 1;
{$ifdef DPMI}
      Move(WB, Mem[SegB800:((Y1 - 1) * XX + X1 - 1) shl 1], Size);
{$else}
      Move(WB, VideoMem[Y1, X1], Size);
{$endif}
  end;

  procedure bPutSymb(var WB: TWriteBuffer; x:integer; b:byte);
  begin
    if not CheckX(x) then Exit;
    WB[x, 1] := b;
  end;

  procedure bPutAttr(var WB: TWriteBuffer; x:integer; b:byte);
  begin
    if not CheckX(x) then Exit;
    WB[x, 2] := b;
  end;

  procedure bPutExtra(var WB: TWriteBuffer; x: Integer; ch, c:byte);
  begin
    if not CheckX(x) then Exit;
    WB[x, 1] := ch;
    WB[x, 2] := c;
  end;

  procedure bChangeAttr(var WB: TWriteBuffer; x1,x2:integer; b:byte);
  var I: Integer;
  begin
    if not (CheckX(x1) and CheckX(x2)) then Exit;
    for I := x1 to x2 do WB[I, 2] := b;
  end;

  procedure bChangeSymb(var WB: TWriteBuffer; x1,x2:integer; b:byte);
  var I: Integer;
  begin
    if not (CheckX(x1) and CheckX(x2)) then Exit;
    for I := x1 to x2 do WB[I, 1] := b;
  end;

  procedure bFill(var WB: TWriteBuffer; x1,x2:integer; ch, col:byte);
  var I: Integer;
  begin
    if not (CheckX(x1) and CheckX(x2)) then Exit;
    for I := x1 to x2 do begin
      WB[I, 1] := ch;
      WB[I, 2] := col;
    end;
  end;

  procedure bWrite(var WB: TWriteBuffer; x1,x2:integer; s:string);
  var Col,j,o,ll:byte;
      RealL:byte;
      ServCh:char;
      out:string;
  begin
    if not (CheckX(x1) and CheckX(x2)) then Exit;
    Col:=$FF;{цвет не менять}
    if (s[1] in [^A..^Y])then
     begin
       ServCh:=s[1];
       out:=copy(s,2,byte(s[0])-1);
       RealL:=RealLength(out);
       if (RealL<(x2-x1+1)) then
        begin
         case ServCh of
          ^C: Centered(x1,x2,RealL,out);
          ^R: Righted(x1,x2,RealL,out);
          ^F: Filered(x1,x2,RealL,out);
          ^L: Lefted(x1,x2,RealL,out);
          else Lefted(x1,x2,RealL,out);
         end;
       end;
     end
    else out:=s;
    o:=0;
    ll:=x2-x1+1;
    for j:=1 to Length(out) do
     begin
       if out[j]=^Z then
        begin
          Col:=byte(out[j+1]);
          inc(j);
        end else
       if out[j]=^Y then
        begin
          if out[j + 1] <> #$FF then bPutSymb(WB, x1 + o, byte(out[j + 1]));
          if Col<>$FF then bPutAttr(WB, x1 + o, Col);
          inc(j);
          inc(o);
          if o>=ll then Break;
        end else
        begin
          if out[j] <> #$FF then bPutSymb(WB, x1 + o, byte(out[j]));
          if Col <> $FF then bPutAttr(WB, x1 + o, Col);
          inc(o);
          if o >= ll then Break;
        end;
     end;
  end;

  {virtual ability}
  procedure vPutChar(x,y:integer;b:byte);
    begin
      if InvalidPoint(x, y) then Exit;
      VirtualMem[y,x,1]:=b;
    end;

  procedure vPutAttr(x,y:integer;b:byte);
    begin
      if InvalidPoint(x, y) then Exit;
      VirtualMem[y,x,2]:=b;
    end;

  procedure vPutExtra(x,y:integer; ch, c:byte);
    begin
      vPutChar(x, y, ch);
      vPutAttr(x, y, c);
    end;

  procedure vChangeAttr(x1,y1,x2,y2:integer;col:byte);
   var i,j:integer;
   begin
     MakeValidRect(x1, y1, x2, y2);
     for j:=y1 to y2 do for i:=x1 to x2 do VirtualMem[j,i,2]:=col;
   end;

  procedure vWrite(x1,y1,x2:integer;s:string);
   var Col,j,o,ll:byte;
       RealL:byte;
       ServCh:char;
       out:string;
   label rep;
   begin
     Col:=$FF;{цвет не менять}
     if (s[1] in [^A..^Y])then
      begin
        ServCh:=s[1];
        out:=copy(s,2,byte(s[0])-1);
        RealL:=RealLength(out);
        if (RealL<(x2-x1+1)) then
         begin
          case ServCh of
           ^C:Centered(x1,x2,RealL,out);
           ^R:Righted(x1,x2,RealL,out);
           ^F:Filered(x1,x2,RealL,out);
           ^L:Lefted(x1,x2,RealL,out);
           else Lefted(x1,x2,RealL,out);
          end;
        end;
      end
     else out:=s;
     o:=0;
     ll:=x2-x1+1;
     for j:=1 to byte(out[0]) do
      begin
        if out[j]=^Z then
         begin
           Col:=byte(out[j+1]);
           inc(j);
         end else
        if out[j]=^Y then
         begin
           if not InvalidPoint(x1, y1 + o) then
           begin
             if out[j + 1] <> #$FF then VirtualMem[y1,x1+o,1]:=byte(out[j + 1]);
             if Col<>$FF then VirtualMem[y1,x1+o,2]:=Col;
           end;
           inc(j);
           inc(o);
           if o>=ll then Break;
         end else
         begin
           if not InvalidPoint(x1, y1 + o) then
           begin
             if out[j] <> #$FF then VirtualMem[y1,x1+o,1]:=byte(out[j]);
             if Col<>$FF then VirtualMem[y1,x1+o,2]:=Col;
           end;
           inc(o);
           if o>=ll then Break;
         end;
      end;
   end;

  procedure vFill(x1,y1,x2,y2:integer;char,attr:byte);
   var i,j:integer;
   begin
     MakeValidRect(x1, y1, x2, y2);
     for i:=x1 to x2 do
       for j:=y1 to y2 do
        begin
          VirtualMem[j,i,1]:=char;
          VirtualMem[j,i,2]:=attr;
        end;
   end;

  procedure ActiveVirtualPage;
   begin
{$ifndef DPMI}
     Move(VideoMem,VirtualMem,4000);
{$else}
     Move(Mem[SegB800:0000],VirtualMem,4000);
{$endif}
   end;

  procedure ActiveRealPage;
   begin
{$ifndef DPMI}
     Move(VirtualMem,VideoMem,4000);
{$else}
     Move(VirtualMem,Mem[SegB800:0000],4000);
{$endif}
   end;

end.
