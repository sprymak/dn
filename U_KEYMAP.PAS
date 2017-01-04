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

{---------------------------------------------------------------------------}
{                                                                           }
{ èéÑÑÖêÜäÄ äéÑéÇõï íÄÅãàñ ASCII(DOS-866), ANSI(WIN-1251), KOI8-R           }
{                                                                           }
{ VIV (C) 1999                                                              }
{                                                                           }
{---------------------------------------------------------------------------}

unit u_keymap;
interface

type
  TKeyMap = (kmAscii, kmAnsi, kmKoi8r); {-$VIV}

function Ascii_Ansi(S: String): String;
function Ansi_Ascii(S: String): String;

procedure Ascii_Ansi_Buf(var Buf; L: Word);
procedure Ansi_Ascii_Buf(var Buf; L: Word);

function Ascii_Koi8r(S: String): String;
function Koi8r_Ascii(S: String): String;

procedure Ascii_Koi8r_Buf(var Buf; L: Word);
procedure Koi8r_Ascii_Buf(var Buf; L: Word);

function RusEngConv(S: String): String;

implementation

type
  TConvertTable = array[0..255] of Byte;

procedure CreateBackTable(SrcTbl: TConvertTable; var TrgTbl: TConvertTable);
var i: integer;
begin
  FillChar(TrgTbl, 256, 0);
  for i := 0 to 255 do
   if SrcTbl[i] <> 0 then TrgTbl[SrcTbl[i]] := i;
end;

procedure Convert_Buf(var Buf; Len: Word; ConvTbl: TConvertTable);
var I: Integer;
    B: Byte;
    ByteBuf: array [0..0] of Byte absolute Buf;
begin
  for I := 0 to Len - 1 do
  begin
    B := ConvTbl[ByteBuf[I]];
    if B <> 0 then ByteBuf[I] := B;
  end;
end;

function Convert(S: String; ConvTbl: TConvertTable): String;
begin
  Convert_Buf(S[1], Length(S), ConvTbl);
  Convert := S;
end;

{ASCII-ANSI}
const
  AnsiConvTable: TConvertTable = (
       0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
       0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
       0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
       0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
       0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
       0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
       0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
       0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     192, 193, 194, 195, 196, 197, 198, 199, 200, 201, 202, 203, 204, 205, 206, 207,
     208, 209, 210, 211, 212, 213, 214, 215, 216, 217, 218, 219, 220, 221, 222, 223,
     224, 225, 226, 227, 228, 229, 230, 231, 232, 233, 234, 235, 236, 237, 238, 239,
     128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143,
     144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159,
     160, 161, 162, 163, 164, 165, 166, 167, 169, 170, 171, 172, 173, 174, 175, 176,
     240, 241, 242, 243, 244, 245, 246, 247, 248, 249, 250, 251, 252, 253, 254, 255,
     168, 184, 177, 178, 179, 180, 181, 182, 183, 185, 186, 187, 188, 189, 190, 191);

var
  AnsiBackConvTable: TConvertTable;

function Ascii_Ansi(S: String): String;
begin
  Ascii_Ansi := Convert(S, AnsiConvTable);
end;

function Ansi_Ascii(S: String): String;
begin
  Ansi_Ascii := Convert(S, AnsiBackConvTable);
end;

procedure Ascii_Ansi_Buf(var Buf; L: Word);
begin
  Convert_Buf(Buf, L, AnsiConvTable);
end;

procedure Ansi_Ascii_Buf(var Buf; L: Word);
begin
  Convert_Buf(Buf, L, AnsiBackConvTable);
end;

{ASCII-KOI8R}
const
  Koi8rConvTable: TConvertTable = (
       0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
       0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
       0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
       0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
       0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
       0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
       0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
       0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     225, 226, 247, 231, 228, 229, 246, 250, 233, 234, 235, 236, 237, 238, 239, 240,
     242, 243, 244, 245, 230, 232, 227, 254, 251, 253, 255, 249, 248, 252, 224, 241,
     193, 194, 215, 199, 196, 197, 214, 218, 201, 202, 203, 204, 205, 206, 207, 208,
     144, 145, 146, 129, 135, 178, 180, 167, 166, 181, 161, 168, 174, 173, 172, 131,
     132, 137, 136, 134, 128, 138, 175, 176, 171, 165, 187, 184, 177, 160, 190, 185,
     186, 182, 183, 170, 169, 162, 164, 189, 188, 133, 130, 141, 140, 142, 143, 139,
     210, 211, 212, 213, 198, 200, 195, 222, 219, 221, 223, 217, 216, 220, 192, 209,
     179, 163, 153, 152, 147, 155, 159, 151, 156, 149, 158, 150, 191, 157, 148, 154);

var
  Koi8rBackConvTable: TConvertTable;

function Ascii_Koi8r(S: String): String;
begin
  Ascii_Koi8r := Convert(S, Koi8rConvTable);
end;

function Koi8r_Ascii(S: String): String;
begin
  Koi8r_Ascii := Convert(S, Koi8rBackConvTable);
end;

procedure Ascii_Koi8r_Buf(var Buf; L: Word);
begin
  Convert_Buf(Buf, L, Koi8rConvTable);
end;

procedure Koi8r_Ascii_Buf(var Buf; L: Word);
begin
  Convert_Buf(Buf, L, Koi8rBackConvTable);
end;

{RUS-ENG}
const
  RusEngConvTable: TConvertTable = (
       0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
       0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
       0,   0, 157,   0,   0,   0,   0, 237,   0,   0,   0,   0, 161,   0, 238,   0,
       0,   0,   0,   0,   0,   0,   0,   0,   0,   0, 134, 166, 129,   0, 158,   0,
       0, 148, 136, 145, 130, 147, 128, 143, 144, 152, 142, 139, 132, 156, 146, 153,
     135, 137, 138, 155, 133, 131, 140, 150, 151, 141, 159, 229,   0, 234,   0,   0,
     241, 228, 168, 225, 162, 227, 160, 175, 224, 232, 174, 171, 164, 236, 226, 233,
     167, 169, 170, 235, 165, 163, 172, 230, 231, 173, 239, 149,   0, 154, 240,   0,
      70,  60,  68,  85,  76,  84,  58,  80,  66,  81,  82,  75,  86,  89,  74,  71,
      72,  67,  78,  69,  65, 123,  87,  88,  73,  79, 125,  83,  77,  34,  62,  90,
     102,  44, 100, 117, 108, 116,  59, 112,  98, 113, 114, 107, 118, 121, 106, 103,
       0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
       0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
       0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     104,  99, 110, 101,  97,  91, 119, 120, 105, 111,  93, 115, 109,  39,  46, 122,
     126,  96,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0);

function RusEngConv(S: String): String;
begin
  RusEngConv := Convert(S, RusEngConvTable);
end;

begin
  CreateBackTable(AnsiConvTable, AnsiBackConvTable);
  CreateBackTable(Koi8rConvTable, Koi8rBackConvTable);
end.
