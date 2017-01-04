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
unit advance6;

interface
uses Objects;

procedure InitUpcase;
procedure MakeCRCTable;
function  GetLineNumberForOffset(const FName: String; Offset: LongInt): LongInt;
function  GetOffsetForLineNumber(const FName: String; LineNm: LongInt): LongInt;
procedure ResourceAccessError;
function  HotKey(const S: String): Char;

implementation
Uses Advance, LFN;

procedure InitUpcase;
 var C: Char;
begin
  Move(CountryInfo.UpperTable, UpcaseArray[#128], 128);
  Move(CountryInfo.UpperTable, LowcaseArray[#128], 128);
  for C := #128 to #255 do
    begin
      if (UpcaseArray[C] <> C) and (UpcaseArray[C] > #127) then
       begin
         LowCaseArray[UpcaseArray[C]] := C;
         LowCaseArray[C] := C;
       end;
    end;
end;

function GetLineNumberForOffset(const FName: String; Offset: LongInt): LongInt;
var
  F: lFile;
  q: PByteArray;
  bl: integer;
  ln: longint;
  fp: longint;
  bp: longint;
begin
 GetMem(q, 4096);
 lAssignFile(F, FName);
 ln:=1; fp:=0;
 lResetFileReadOnly(F,1);
 repeat
  BlockRead(F.F, q^, 4096, bl);
  bp:=0;
  repeat
   if q^[bp]=10 then inc(ln);
   inc(bp); inc(fp);
  until (bp>=bl) or (fp>=offset)
 until fp>=offset;
 GetLineNumberForOffset:=ln;
 close(f.f);
 freemem(q, 4096);
end;

function GetOffsetForLineNumber(const FName: String; LineNm: LongInt): LongInt;
var
  F: lFile;
  q: PByteArray;
  bl: integer;
  ln: longint;
  fp: longint;
  bp: longint;
begin
 GetMem(q, 4096);
 lAssignFile(F, FName);
 ln:=1; fp:=0;
 lResetFileReadOnly(F,1);
 repeat
  BlockRead(F.F, q^, 4096, bl);
  bp:=0;
  repeat
   if q^[bp]=10 then inc(ln);
   inc(bp); inc(fp);
  until (bp>=bl) or (ln>=LineNm);
 until (ln>=LineNm) or EOF(F.F);
 if ln >= LineNm
  then GetOffsetForLineNumber:=fp
  else GetOffsetForLineNumber:=-1;
 close(f.f);
 freemem(q, 4096);
end;

procedure ResourceAccessError;
begin
  RunError(219);
end;

function HotKey(const S: String): Char;
var
  P: Word;
begin
  P := Pos('~',S);
  if P <> 0 then HotKey := UpCaseArray[S[P+1]]
  else HotKey := #0;
end;

procedure MakeCRCTable;
var
 c    : Longint;
 n,k  : integer;
 poly : Longint; { polynomial exclusive-or pattern }

const
 { terms of polynomial defining this crc (except x^32): }
 p: array [0..13] of Byte = (0,1,2,4,5,7,8,10,11,12,16,22,23,26);

begin
  New(Crc_Table);
  { make exclusive-or pattern from polynomial ($EDB88320) }
  poly := 0;
  for n := 0 to (sizeof(p) div sizeof(Byte))-1 do
    poly := poly or (Longint(1) shl (31 - p[n]));

  for n := 0 to 255 do
  begin
    c := n;
    for k := 0 to 7 do
    begin
      if (c and 1) <> 0 then
        c := poly xor (c shr 1)
      else
        c := (c shr 1);
    end;
    crc_table^[n] := c;
  end;
  Crc_table_empty := FALSE;
end;


end.
