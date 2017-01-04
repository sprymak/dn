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

{Версия 1.2}
unit u_hex;
interface
  function  GetHexByte(b:byte):string;
  function  GetHexWord(w:word):string;
  function  GetHexLongint(l: longint):string;
  function  Hex2Longint(h:string):longint;
  function  IsHexDigit(c:char):boolean;
  function  GetHexAddr(P: Pointer): String;

  {поступает строка с чилом в формате hex Motorola 0xLoHi ==> число Intel int}
  function  Hex2Word(h:string):word;
  function  Hex2Byte(h:string):byte;
  function  Byte2Bit(b:byte):string;
  function  Bit(b:byte;n:byte):boolean;
  function  GetIntelWord(w:word):string;
  function  GetIntelLongint(l:longint):string;
  function  Motorola2Intel(s:string):string;

implementation

function  GetHexAddr(P: Pointer): String;
begin
  GetHexAddr := GetHexWord(Seg(P^)) + ':' + GetHexWord(Ofs(P^));
end;

  function  IsHexDigit(c:char):boolean;
   begin
     if (c in ['0'..'9'])or(c in ['A'..'F'])or(c in ['a'..'f']) then
     IsHexDigit:=true else IsHexDigit:=false;
   end;

  function GetHexByte(b: byte):string;
   const
    hexChars: array [0..$F] of Char ='0123456789ABCDEF';
   var s:string[2];
   begin
    s:=hexChars[b shr 4]+hexChars[b and $F];
    GetHexByte:=s;
   end;

  function GetHexWord(w: Word):string;
   const
    hexChars: array [0..$F] of Char ='0123456789ABCDEF';
   var s:string[4];
   begin
    s:=hexChars[Hi(w) shr 4]+hexChars[Hi(w) and $F]+
       hexChars[Lo(w) shr 4]+hexChars[Lo(w) and $F];
       GetHexWord:=s;
   end;

  function GetHexLongint(l: longint):string;
   const
    hexChars: array [0..$F] of Char ='0123456789ABCDEF';
   var s:string[8];
   begin
    s:=hexChars[Hi(word(l shr 16)) shr 4]+hexChars[Hi(word(l shr 16)) and $F]+
       hexChars[Lo(word(l shr 16)) shr 4]+hexChars[Lo(word(l shr 16)) and $F]+
       hexChars[Hi(word(l)) shr 4]+hexChars[Hi(word(l)) and $F]+
       hexChars[Lo(word(l)) shr 4]+hexChars[Lo(word(l)) and $F];
       GetHexLongint:=s;
   end;

  function Hex2Longint(h:string):longint;
   var sum:longint;
       i,l:word;
   const Mult:array[1..8]of longint=(0,4,8,12,16,20,24,28);
   begin
     sum:=0;
     l:=length(h);
     for i:=1 to l do
       case h[i] of
        '0'..'9':sum:=sum+(byte(h[i])-$30) shl Mult[l-i+1];
        'A'..'F':sum:=sum+(byte(h[i])-$41+10) shl Mult[l-i+1];
        'a'..'f':sum:=sum+(byte(h[i])-$61+10) shl Mult[l-i+1];
        else
         begin
          Hex2Longint:=$0;
          Exit;
         end;
      end;
     Hex2Longint:=sum;
   end;

  function Hex2Word(h:string):word;
   var sum,i,l:word;
   const Mult:array[1..4]of longint=(0,4,8,12);
   begin
     sum:=0;
     l:=length(h);
     for i:=1 to l do
       case h[i] of
        '0'..'9':sum:=sum+(byte(h[i])-$30) shl Mult[l-i+1];
        'A'..'F':sum:=sum+(byte(h[i])-$41+10) shl Mult[l-i+1];
        'a'..'f':sum:=sum+(byte(h[i])-$61+10) shl Mult[l-i+1];
        else
         begin
          Hex2Word:=$0000;
          Exit;
         end;
      end;
     Hex2Word:=sum;
   end;

  function Hex2Byte(h:string):byte;
   var sum,i,l:word;
   const Mult:array[1..2]of word=(0,4);
   begin
     sum:=0;
     l:=length(h);if l>2 then l:=2;
     for i:=1 to l do
       case h[i] of
        '0'..'9':sum:=sum+(byte(h[i])-$30) shl Mult[l-i+1];
        'A'..'F':sum:=sum+(byte(h[i])-$41+10) shl Mult[l-i+1];
        'a'..'f':sum:=sum+(byte(h[i])-$61+10) shl Mult[l-i+1];
        else
         begin
          Hex2Byte:=$00;
          Exit;
         end;
      end;
     Hex2byte:=sum;
   end;

  function  Byte2Bit(b:byte):string;
   var s:string[8];
       i:integer;
   const TwoPower:array[0..7]of byte=(1,2,4,8,16,32,64,128);
   begin
    s:='';
    for i:=0 to 7 do if (b and TwoPower[i])<>0 then s:='1'+s else s:='0'+s;
    Byte2Bit:=s;
   end;

  function  Bit(b:byte;n:byte):boolean;
   var cbyte:byte;
   begin
    cbyte:=1 shl n;
    if (b and cbyte)=cbyte then  bit:=true else bit:=false;
   end;

  function  GetIntelWord(w:word):string;
   var s:string;
   begin
     s:=GetHexWord(w);
     GetIntelWord:=Copy(s,3,2)+Copy(s,1,2);
  end;

  function  GetIntelLongint(l:longint):string;
   var s:string;
   begin
     s:=GetHexLongint(l);
     GetIntelLongint:=Copy(s,7,2)+Copy(s,5,2)+Copy(s,3,2)+Copy(s,1,2);
   end;

  function  Motorola2Intel(s:string):string;
   var i,n:integer;
       s2:string;
   begin
     if (byte(s[0]) mod 2)<>0 then s:='0'+s;
     n:=(byte(s[0]) div 2)-1;
     s2:='';
     for i:=0 to n do
      begin
       s2:=s2+copy(s,byte(s[0])-1-i*2,2);
      end;
     Motorola2Intel:=s2;
   end;

end.
