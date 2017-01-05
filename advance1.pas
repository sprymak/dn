{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source 1.51.08
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
{AK155 = Alexey Korop, 2:461/155@fidonet}
{Cat = Aleksej Kozlov, 2:5030/1326.13@fidonet}

{Cat
   28/08/2001 - многие имеющиеся функции переделал для совместимости с типом
   AnsiString; добавил аналогичные функции, работающие с типом LongString
   16/01/2002 - функции поиска строки в буфере теперь получают параметры типа
   LongInt вместо Word
}

unit Advance1; {String functions}

interface

uses
  Objects, Advance, Dos, Lfn, U_KeyMap, Commands {Cat}
  ;

function NewStr(const S: String): PString;
function NewLongStr(const S: LongString): PLongString;
procedure DisposeStr(var P: PString);
procedure DisposeLongStr(var P: PLongString);
procedure ReplaceP(var P: PString; S: String);
function CnvString(P: PString): String; {conversion: PString to String}
function CnvLongString(P: PLongString): LongString;
{conversion: PLongString to LongString}
function StrGrd(AMax, ACur: TSize; Wide: Byte; Rev: Boolean): String;
function Percent(AMax, ACur: TSize): TSize;
procedure Hex8Lo(L: LongInt; var HexLo);
procedure AddStr(var S: String; C: Char);
  inline;
  begin
  S := S+C
  end; {Cat}
{procedure DelFC(var s:String);}

{---  ' '-related string functions }
function CenterStr(const s: String; n: Byte): String; {DataCompBoy}
function AddSpace(const s: String; n: Byte): String;
function LongAddSpace(const s: LongString; n: LongInt): LongString;
function PredSpace(s: String; n: Byte): String;
function DelSpaces(s: String): String;
procedure DelSpace(var s: String);
procedure DelRight(var S: String);
procedure LongDelRight(var S: LongString);
function fDelRight(s: String): String;
procedure DelLeft(var S: String);
procedure LongDelLeft(var S: LongString);
function fDelLeft(s: String): String;

{case function}
function Strg(C: Char; Num: Byte): String;
function LongStrg(C: Char; Num: LongInt): LongString;

function UpCase(c: Char): Char;
{AK155}
  inline;
  begin
  UpCase := UpCaseArray[C]
  end;

procedure UpStr(var s: String);
procedure UpLongStr(var s: LongString);
procedure UpShortStr(var s: openstring);
function UpStrg(s: String): String;
function UpLongStrg(s: LongString): LongString;

function LowCase(c: Char): Char;
{AK155}
  inline;
  begin
  LowCase := LowCaseArray[C]
  end;
procedure LowStr(var s: String);
procedure LowLongStr(var s: LongString);
procedure LowShortStr(var s: openstring);
function LowStrg(s: String): String;
function LowLongStrg(s: LongString): LongString;

procedure UpLowStr(var s: String); {JO}
function UpLowStrg(s: String): String; {JO}

procedure CapStr(var S: String);
procedure CapLongStr(var S: LongString);
function CapStrg(S: String): String;
function CapLongStrg(S: LongString): LongString;

{procedure MakeCase(CaseSensitive: Boolean);}

function ItoS(a: LongInt): String;
function ZtoS(a: TSize): String;
function RtoS(a: Real; M, F: Integer): String;
function StoI(const s: String): LongInt;
function SStr(a: LongInt; B: Byte; C: Char): String;
function SSt2(a: LongInt; B: Byte; C: Char): String;
function FStr(a: TSize): String;
function FileSizeStr(X: TSize): String;
function Hex2(a: Byte): Str2;
function Hex4(a: Word): Str4;
function Hex8(a: LongInt): Str8;
function HexChar(a: Byte): Char;
function Replace(const Pattern, ReplaceString: String; var S: String)
  : Boolean;
function fReplace(const SubFrom, SubTo: String; S: String): String;
function PosChar(C: Char; const S: String): Byte;
function CharCount(C: Char; const S: String): Byte; {DataCompBoy}
function FormatTimeStr(H, M, SS: Word): String; {DataCompBoy}
procedure MakeCurrency(R: Real; var S: String);
function GetDateTime(Time: Boolean): String;
function Real2Str(X: Real; n: Byte): String;
function Long2Str(X: LongInt; l: Byte): String;
function Long0Str(X: LongInt; l: Byte): String;
function ToHex(I: Word): String;
procedure DelDoubles(const St: String; var Source: String);
procedure AnsiDelDoubles(const St: String; var Source: AnsiString);
procedure LongDelDoubles(const St: LongString; var Source: LongString);
procedure MakeDate(const Mode, Day, Month, Year, Hour, Min: Word;
     var S: String);
procedure MakeDateFull(const Mode, Day, Month: Word; {-$VOL moidfied}
    Year, Hour: Word;
    const Min: Word;
    var S: String;
    const YFull: Boolean);
function DumpStr(var B; Addr: LongInt; Count: Integer; Filter: Byte)
  : String;

function BackSearchFor(const S: String; var B; l: LongInt;
     CaseSensitive: Boolean): LongInt;
function SearchFor(const S: String; var B; l: LongInt;
     CaseSensitive: Boolean): LongInt;
function BackSearchForAllCP(const S: String; var B; l: LongInt;
     CaseSensitive: Boolean): LongInt; {-$VIV 14.05.99}
function SearchForAllCP(const S: String; var B; l: LongInt;
     CaseSensitive: Boolean): LongInt; {-$VIV ::}

procedure CompressString(var S: LongString);
{AK155}
function PosLastDot(StrToMake: String): Byte;
function IsDummyDir(const DirName: String): Boolean;
procedure CopyShortString(const s1, s2: ShortString);
{/AK155}

implementation

uses
  Advance2, Advance3, DnIni, Startup {Cat}
  ;

function LeadingZero(w: Word): String;
  var
    s: String;
  begin
  Str(w: 0, s);
  LeadingZero := Copy('00', 1, 2-Length(s))+s;
  end;

function StrGrd(AMax, ACur: TSize; Wide: Byte; Rev: Boolean): String;
  var
    A: Byte;
  begin
  if AMax = 0 then
    A := Wide
  else
    A := Round((ACur*Wide)/AMax);
  if Rev then
    StrGrd := Strg(#177, Wide-A)+Strg(#219, A)
  else
    StrGrd := Strg(#219, A)+Strg(#177, Wide-A);
  end;

function Percent(AMax, ACur: TSize): TSize;
  begin
  if AMax = 0 then
    Percent := {0}100 {AK155}
  else
    Percent := (ACur*100)/AMax;
  end;

procedure Hex8Lo(L: LongInt; var HexLo);
  {$IFNDEF NOASM}
  assembler;
  {$USES EDI, EBX, EDX, ECX} {&Frame-}
asm
        cld
        xor     dx,dx
        mov     edi,[HexLo]
        lea     ebx,[LoHexChar]
        mov     dx,[word ptr L+2]
        call    @@OutWord
        mov     dx,[word ptr L+0]
        call    @@OutWord
        jmp     @@LEnd

 @@OutWord:      {DX-word}
        mov     ax,dx
        mov     cl,12
        shr     ax,cl
        xlat
        stosb
        mov     al,dh
        and     al,0Fh
        xlat
        stosb
        mov     al,dl
        mov     cl,4
        shr     al,cl
        xlat
        stosb
        mov     al,dl
        and     al,0Fh
        xlat
        stosb
        retn
 @@LEnd:
 end;
  {$ELSE}
  type
    ChArr = array[1..4] of Char;
  begin { Hex8Lo }
  ChArr(HexLo)[1] := LoHexChar[L shr 24];
  ChArr(HexLo)[2] := LoHexChar[L and $00FF0000 shr 16];
  ChArr(HexLo)[3] := LoHexChar[L and $0000FF00 shr 08];
  ChArr(HexLo)[4] := LoHexChar[L and $000000FF];
  end { Hex8Lo };
{$ENDIF}

{Cat: по-моему, всё проще... это же System.Delete(s, 1, 1)... выкинул нафиг}
(*
procedure DelFC(var s:String);
begin
  if Length(s) > 0 then
    begin
      Move(s[2], s[1], Length(s)-1);
      SetLength(s, Length(s)-1)
    end;
end;
*)

function CenterStr(const s: String; n: Byte): String;
  begin
  if Length(s) >= n then
    CenterStr := Copy(s, 1, n)
  else
    CenterStr := Copy(Strg(#32, (n-Length(s)) div 2)+s+Strg(#32,
               (n-Length(s)) div 2+1), 1, n);
  end;

function AddSpace(const s: String; n: Byte): String;
  {$IFNDEF NOASM}
  assembler;
  {&Frame-} {$USES ESI, EDI, ECX}
asm
   cld
   mov   esi, s
   mov   edi, @Result
   lodsb
   mov   ah, N
   xor   ecx, ecx
   cmp   al, ah
   jae   @JustCopy
   mov   [edi], ah
   inc   edi
   mov   cl, al
   rep   movsb
   sub   ah, al
   mov   al, ' '
   mov   cl, ah
   rep   stosb
   jmp   @LEnd

 @JustCopy:
   stosb
   mov  cl, al
   rep movsb
 @LEnd:
 end;
  {$ELSE}
  var
    s2: String;
  begin { AddSpace }
  s2 := s;
  if Length(s) < n then
    begin
    FillChar(s2[Length(s2)+1], n-Length(s2), ' ');
    s2[0] := Char(n);
    end;
  AddSpace := s2;
  end { AddSpace };
{$ENDIF}

{Cat}
function LongAddSpace(const s: LongString; n: LongInt): LongString;
  var
    s2: LongString;
    L: LongInt;
  begin
  s2 := s;
  L := Length(s);
  if L < n then
    begin
    SetLength(s2, n);
    FillChar(s2[L+1], n-L, ' ');
    end;
  LongAddSpace := s2;
  end;
{/Cat}

function PredSpace;
  begin
  if Length(s) >= n then
    PredSpace := s
  else
    begin
    FillChar(FreeStr[1], 255, ' ');
    Move(s[1], FreeStr[Succ(n-Length(s))], Length(s));
    SetLength(FreeStr, n);
    PredSpace := FreeStr
    end
  end;

procedure DelSpace;
  {$IFNDEF NOASM}
  assembler;
  {&Frame-} {$USES EBX, EDI, ESI, ECX}
asm
  mov  ebx, S
  xor  ecx, ecx
  mov  cl, [ebx]
  jcxz @@1
  mov  edi, 1
  mov  esi, 1
  xor  al, al
  mov  [ebx], al
 @@2:
  mov  al, [ebx+edi]
  cmp  al, ' '
  jz   @@3
  cmp  al, 9
  jz   @@3
  mov  [ebx+esi], al
  inc  byte ptr [ebx]
  inc  esi
 @@3:
  inc  edi
  loop @@2
 @@1:
 end;
  {$ELSE}
  var
    a, b, j: Byte;
  begin { DelSpace }
  if s = '' then
    Exit;
  b := 1;
  j := Length(s);
  s[0] := #0;
  for a := 1 to j do
    if not (s[a] in [' ', #9]) then
      begin
      s[b] := s[a];
      Inc(s[0]);
      Inc(b);
      end;
  end { DelSpace };
{$ENDIF}

function DelSpaces;
  begin
  DelSpace(s);
  DelSpaces := s;
  end;

procedure DelRight(var S: String);
  {$IFNDEF NOASM}
  assembler;
  {&Frame-} {$USES ESI, EBX}
asm
     mov  esi, S
     movzx ebx, byte ptr [esi] {длина}
     inc  ebx
 @@Next:
     dec  ebx
{специальный анализ длины на 0 не нужен, так как 0 - это не пробел и не Tab}
     mov  al, byte ptr [esi+ebx]
     cmp  al, ' '
     je   @@Next
     cmp  al, 9
     je   @@Next
     mov  byte ptr[esi], bl
 end;
  {$ELSE NoAsm}
  var
    L: Byte absolute S;
  begin
  while L > 0 do
    begin
    if S[L] <> ' ' then
      Break;
    Dec(L);
    end;
  end { DelRight };
{$ENDIF}

{Cat}
procedure LongDelRight(var S: LongString);
  var
    I: LongInt;
  begin
  I := Length(S);
  while (I > 0) and (S[I] = ' ') do
    Dec(I);
  SetLength(S, I);
  end;
{/Cat}

function fDelRight(s: String): String;
  begin
  DelRight(s);
  fDelRight := s
  end;

procedure DelLeft(var S: String);
  {$IFNDEF NOASM}
  assembler;
  {&Frame-} {$USES ESI, EDI, ECX}
asm
               cld
               mov     esi, S  {From}
               mov     edi, S
               xor     ecx, ecx
               mov     cl, byte ptr [esi]
               inc     cl
               inc     edi
 @@SearhNoSpace:
               inc     esi
               dec     cl
               jz      @@DoMove
               mov     al, [esi]
               cmp     al, ' '
               je      @@SearhNoSpace
               cmp     al, 9
               je      @@SearhNoSpace

 @@DoMove:     cmp     esi, edi
               je      @@Exit
               mov     byte ptr [edi-1], cl
               or      cl, cl
               jz      @@Exit
               xor     ch, ch
{               shr     cl, 1}
{               rep     movsw}
               rep     movsb
 @@Exit:
 end;
  {$ELSE}
  var
    I: Byte;
    SL: Byte absolute S;
  begin { DelLeft }
  I := 1;
  while (SL >= I) and (S[I] in [#9, ' ']) do
    Inc(I);
  if I > 1 then
    begin
    Dec(SL, I-1);
    Move(S[I], S[1], SL);
    end;
  end { DelLeft };
{$ENDIF}

{Cat}
procedure LongDelLeft(var S: LongString);
  var
    I: LongInt;
  begin
  I := 1;
  while (I <= Length(S)) and (S[I] in [#9, ' ']) do
    Inc(I);
  if I > 1 then
    begin
    Move(S[I], S[1], Length(S)-I+1);
    SetLength(S, Length(S)-I+1);
    end;
  end;
{/Cat}

function fDelLeft(s: String): String;
  begin
  DelLeft(s);
  fDelLeft := s
  end;

(*
Function Strg(c:char; Num: Byte) : string;
{$IFNDEF NOASM}
 assembler;
 {$IFNDEF BIT_32}
 asm
  les  di, @Result
  mov  al, Num
  stosb
  mov  ch, 0
  mov  cl, al
  mov  al, c
  rep  stosb
 end;
 {$ELSE BIT_32}{&Frame-}{$USES EDI, ECX}
 asm
  mov  edi, @Result
  xor  eax, eax
  mov  al, Num
  stosb
  xor  ecx, ecx
  mov  cl, al
  mov  al, c
  rep  stosb
 end;
 {$ENDIF}
{$ELSE}
var S: string;
begin
 fillchar(s[1], num, c);
 s[0]:=char(num);
 Strg:=s;
end;
{$ENDIF}
*)

(*
Procedure LowStr(var s : string);
var i:integer;
begin for i:=1 to Length(s) do
{if s[i]<>#0 then} s[i]:=LowCaseArray[s[i]]; {AK155}
end;
*)

function LowStrg(s: String): String;
  begin
  LowStr(s);
  LowStrg := s;
  end;

{Cat}
function LowLongStrg(s: LongString): LongString;
  begin
  LowLongStr(s);
  LowLongStrg := s;
  end;
{/Cat}

(*
Procedure UpStr(var s : string); var i:integer;
begin for i:=1 to Length(s) do
{if s[i]<>#0 then}  {AK155}
s[i]:=UpCaseArray[s[i]];
end;
*)

function UpStrg(s: String): String;
  begin
  UpStr(s);
  UpStrg := s;
  end;

{Cat}
function UpLongStrg(s: LongString): LongString;
  begin
  UpLongStr(s);
  UpLongStrg := s;
  end;
{/Cat}

procedure UpLowStr(var s: String); {JO}
  begin
  if UpperCaseSorting then
    UpStr(s)
  else
    LowStr(s);
  end;

function UpLowStrg(s: String): String; {JO}
  begin
  if UpperCaseSorting then
    begin
    UpStr(s);
    UpLowStrg := s;
    end
  else
    begin
    LowStr(s);
    UpLowStrg := s;
    end;
  end;

(*
Procedure CapStr(var S: String);
var
  I: Integer;
begin
 I:=1;
 repeat
  While (I<=Length(S)) and (S[I] in BreakChars) do Inc(I);
  If I>Length(S) then break;
  S[I]:=UpCase(S[I]);
  While (I<Length(S)) and (not (S[I] in BreakChars)) do begin
   Inc(I); S[I]:=LowCase(S[I]);
  end;
 until I>=Length(S);
end;
*)

function CapStrg(S: String): String;
  begin
  CapStr(S);
  CapStrg := S;
  end;

{Cat}
function CapLongStrg(S: LongString): LongString;
  begin
  CapLongStr(S);
  CapLongStrg := S;
  end;
{/Cat}

{Cat}
procedure UpStr(var s: String);
  var
    i: LongInt;
  begin
  for i := 1 to Length(s) do
    s[i] := UpCaseArray[s[i]]
  end;

procedure UpLongStr(var s: LongString);
  var
    i: LongInt;
  begin
  for i := 1 to Length(s) do
    s[i] := UpCaseArray[s[i]]
  end;

procedure UpShortStr(var s: openstring);
  var
    i: LongInt;
  begin
  for i := 1 to Length(s) do
    s[i] := UpCaseArray[s[i]]
  end;

procedure LowStr(var s: String);
  var
    i: LongInt;
  begin
  for i := 1 to Length(s) do
    s[i] := LowCaseArray[s[i]]
  end;

procedure LowLongStr(var s: LongString);
  var
    i: LongInt;
  begin
  for i := 1 to Length(s) do
    s[i] := LowCaseArray[s[i]]
  end;

procedure LowShortStr(var s: openstring);
  var
    i: LongInt;
  begin
  for i := 1 to Length(s) do
    s[i] := LowCaseArray[s[i]]
  end;

procedure CapStr(var S: String);
  var
    I: LongInt;
  begin
  I := 1;
  repeat
    while (I <= Length(S)) and (S[I] in BreakChars) do
      Inc(I);
    if I > Length(S) then
      Break;
    S[I] := UpCase(S[I]);
    while (I < Length(S)) and (not (S[I] in BreakChars)) do
      begin
      Inc(I);
      S[I] := LowCase(S[I]);
      end;
  until I >= Length(S);
  end;

procedure CapLongStr(var S: LongString);
  var
    I: LongInt;
  begin
  I := 1;
  repeat
    while (I <= Length(S)) and (S[I] in BreakChars) do
      Inc(I);
    if I > Length(S) then
      Break;
    S[I] := UpCase(S[I]);
    while (I < Length(S)) and (not (S[I] in BreakChars)) do
      begin
      Inc(I);
      S[I] := LowCase(S[I]);
      end;
  until I >= Length(S);
  end;

function Strg(C: Char; Num: Byte): String;
  var
    S: String;
  begin
  SetLength(S, Num);
  FillChar(S[1], Num, C);
  Strg := S;
  end;

function LongStrg(C: Char; Num: LongInt): LongString;
  var
    S: LongString;
  begin
  SetLength(S, Num);
  FillChar(S[1], Num, C);
  LongStrg := S;
  end;
{/Cat}

function ItoS(a: LongInt): String;
  var
    s: String[12];
  begin
  Str(a, s);
  ItoS := s;
  end;

function ZtoS(a: TSize): String;
  var
    s: String[20];
  begin
  Str(a: 0: 0, s);
  ZtoS := s;
  end;

function RtoS(a: Real; M, F: Integer): String;
  var
    s: String[20];
  begin
  Str(a: M: F, s);
  RtoS := s;
  end;

function StoI(const s: String): LongInt;
  var
    i: LongInt;
    j: Integer;
  begin
  Val(s, i, j);
  StoI := i;
  end;

function SStr(a: LongInt; B: Byte; C: Char): String;
  var
    s: String[40];
    i: Integer;
  begin
  Str(a: B, s);
  i := 1;
  while i < B do
    begin
    if s[i] = ' ' then
      s[i] := C
    else
      i := 255;
    Inc(i);
    end;
  SStr := s;
  end;

function SSt2(a: LongInt; B: Byte; C: Char): String;
  var
    s: String[40];
  begin
  Str(a: B, s);
  while (s[1] = ' ') do
    begin
    Delete(s, 1, 1); {DelFC(s);}
    s := s+' '
    end;
  while B > 0 do
    begin
    if s[B] = ' ' then
      s[B] := C
    else
      B := 1;
    Dec(B);
    end;
  SSt2 := s;
  end;

function FStr(a: TSize): String;
  var
    s, s1: String[40];
    s1l: Byte absolute s1;
    i: Integer;
    {$IFNDEF NOASM}C: Char; {$ENDIF}
  begin
  Str(a: 0: 0, s);
  if CountryInfo.ThouSep[0] > #0 then
    begin
    s1 := '';
    {$IFNDEF NOASM}
    C := CountryInfo.ThouSep[1];
    asm
       push esi
       push edi
       push ebx
       push ecx
       push edx
       lea esi, s
       lea edi, s1
       mov ebx, edi
       xor eax, eax
       mov al, [esi]
       mov ecx, 3
       div cl
       mov byte ptr [ebx], 0
       mov cl, [esi]
       mov dl, ah
       Mov al, C
       or  dl, dl
       jnz @@1
       mov dl, 3
      @@1:
       or  cl, cl
       jz  @@3
      @@2:
       mov ah, [esi+1]
       mov byte ptr [edi+1], ah
       inc byte ptr [ebx]
       inc edi
       inc esi
       dec cl
       jz  @@3
       dec dl
       jnz @@2
       mov dl, 3
       mov byte ptr [edi+1], al
       inc edi
       inc byte ptr [ebx]
       jmp @@2
      @@3:
       pop edx
       pop ecx
       pop ebx
       pop edi
       pop esi
      end;
    {$ELSE}
    for i := Length(s) downto 1 do
      begin
      s1 := s[i]+s1;
      if  (i > 1) and ((Byte(s[0])-i+1) mod 3 = 0) then
        s1 := CountryInfo.ThouSep[1]+s1;
      end;
    {$ENDIF}
    end
  else
    s1 := s;
  if Length(s1) > 12 then
    begin
    s1 := FStr(a/1024);
    if s1[s1l] = 'K'
    then
      s1[s1l] := 'M'
    else
      begin
      Inc(s1l);
      s1[s1l] := 'K';
      end;
    end;
  if a = MaxLongInt then
    FStr := '>='+s1
  else
    FStr := s1;
  end { FStr };

function FileSizeStr;
  var
    S: String[40];
    J: TSize;
  label K;
  begin
  if X >= 0
  then
    if X < 10000000 then
      FileSizeStr := FStr(X)
    else
      begin
      J := X/1024;
      if J >= 10000000then
        begin
        J := X/1024;
        Str(J:
          0:
          0, S);
        if Length(S) > 3 then
          Insert(',', S, Length(S)-2);
        FileSizeStr := S+'M';
        Exit;
        end;
        Str(J:
          0:
          0, S);
        if Length(S) > 3 then
          Insert(',', S, Length(S)-2);
        if X = MaxLongInt then
          FileSizeStr := '>='+S+'K'
        else
          FileSizeStr := S+'K';
      end
      else
        FileSizeStr := '?'
  end { FileSizeStr };

function Hex2;
  begin
  Hex2 := HexChar(a shr 4)+HexChar(a)
  end;

function Hex4;
  begin
  Hex4 := HexChar(Hi(a) shr 4)+HexChar(Hi(a))+
    HexChar(Lo(a) shr 4)+HexChar(Lo(a));
  end;

function Hex8;
  var
    s: Str8;
  begin
  s[0] := #8;
  Hex8Lo(a, s[1]);
  Hex8 := s;
  end;

function HexChar(a: Byte): Char;
  {$IFNDEF NOASM}
  assembler;
asm
  mov al,a
  and al,0Fh
  add al,'0'
  cmp al,58
  jc  @@Loc1
  add al,7
@@Loc1:
end;
  {$ELSE}
  begin
  a := a and 15;
  if a < 10 then
    HexChar := Char(Ord('0')+a)
  else
    HexChar := Char(Ord('A')+a-10);
  end;
{$ENDIF}

function Replace;
  var
    I, J, K: Integer;
  begin
  J := 1;
  K := 1;
  Replace := False;
  if  (Pattern = '') or (S = '') then
    Exit;
  repeat
    I := Pos(Pattern, Copy(S, J, MaxStringLength));
    if I > 0 then
      begin
      Delete(S, J+I-1, Length(Pattern));
      Insert(ReplaceString, S, J+I-1);
      Replace := True;
      end;
    K := I;
    Inc(J, I+Length(ReplaceString)-1);
  until I = 0;
  end;

function fReplace(const SubFrom, SubTo: String; S: String): String;
  var
    P: Integer;
  begin
  repeat
    P := Pos(SubFrom, S);
    if P <> 0 then
      begin
      S := Copy(S, 1, P-1)
          +SubTo+Copy(S, P+Length(SubFrom), Length(S)-P-Length(SubFrom)+1);
      end;
  until (P = 0);
  fReplace := S;
  end;

function PosChar;
  {$IFNDEF NOASM}
  assembler;
  {&Frame-} {$USES EDI, EBX, ECX}
asm
    mov edi, S
    xor ecx, ecx
    mov cl, byte ptr [edi]
    mov ebx, ecx
    inc edi
    cld
    mov al, C
    repne scasb
    jnz @S
    sub ebx, ecx
    mov al, bl
    jmp @Q
 @S:xor al, al
 @Q:
 end;
  {$ELSE}
  {Cat}
  var
    I: Integer;
  begin { PosChar }
  for I := 1 to Length(S) do
    if S[I] = C then
      begin
      PosChar := I;
      Exit;
      end;
  PosChar := 0;
  end { PosChar };
{/Cat}
{$ENDIF}

function CharCount(C: Char; const S: String): Byte; {DataCompBoy}
  {$IFNDEF NOASM}
  assembler;
  {&Frame-} {$USES ESI, EBX, ECX}
asm
  mov esi, S
  xor ebx, ebx
  mov bl, byte ptr [esi]
  xor ecx, ecx
  or  bl, bl
  jz  @@Ex
  inc esi
  mov ah, C
 @@NextChar:
  dec ebx
  jz  @@Ex
  mov al, byte ptr [esi+ebx]
  cmp al, ah
  jne @@NextChar
  inc cx
  jmp @@NextChar
 @@Ex:
  mov eax, ecx
 end;
  {$ELSE}
  var
    i, j: Byte;
  begin { CharCount }
  j := 1;
  for i := 1 to Length(S) do
    if S[i] = C then
      Inc(j);
  CharCount := j;
  end { CharCount };
{$ENDIF}

function GetDateTime(Time: Boolean): String;
  var
    S: String[30];
    Y, M, D, DW: Word;
    H, Mn, SS, S100: Word;

  begin
  GetDate(Y, M, D, DW);
  GetTime(H, Mn, SS, S100);
  MakeDateFull(0, D, M, Y, H, Mn, S, not Time); {-$VOL modified}
  if Time then
    S := FormatTimeStr(H, Mn, SS)
  else
    SetLength(S, 10); {S[0] := #10;}
  GetDateTime := S;
  end;

function FormatTimeStr(H, M, SS: Word): String;
  var
    N: String[3];
    S: String[20];
  begin
  if  (CountryInfo.TimeFmt = 0) and (H > 12) then
    begin
    S := LeadingZero(H-12)+CountryInfo.TimeSep+LeadingZero(M)
      +CountryInfo.TimeSep+LeadingZero(SS);
    N := 'pm';
    end
  else
    begin
    S := LeadingZero(H)+CountryInfo.TimeSep+LeadingZero(M)
      +CountryInfo.TimeSep+LeadingZero(SS);
    if CountryInfo.TimeFmt = 0
    then
      if  (H < 12) then
        N := 'am'
      else
        N := 'pm'
    else
      N := '';
    end;
  FormatTimeStr := S+N;
  end { FormatTimeStr };

procedure MakeCurrency;
  var
    I: Integer;
  begin
  with CountryInfo do
    begin
    if  (DecSign >= '0') and (DecSign <= '9') then
      I := Byte(DecSign[1])-48
    else
      I := 2;
    Str(R: 0: I, S);
    I := PosChar('.', S);
    if I = 0 then
      I := Length(S)
    else
      Move(DecSep[1], S[I], Length(DecSep));
    case CurrencyFmt of
      0:
        S := Currency+S;
      1:
        S := S+Currency;
      2:
        S := Currency+' '+S;
      3:
        S := S+' '+Currency;
      4:
        Insert(Currency, S, I);
    end {case};
    end;
  end { MakeCurrency };

function Real2Str;
  begin
  System.Str(X: n, FreeStr);
  Real2Str := FreeStr
  end;
function Long2Str;
  begin
  Str(X: l, FreeStr);
  Long2Str := FreeStr;
  end;
function Long0Str;
  var
    I: Byte;
  begin
  Str(X: l, FreeStr);
  for I := 1 to Length(FreeStr) do
    if FreeStr[I] = ' ' then
      FreeStr[I] := '0';
  Long0Str := FreeStr
  end;
function ToHex;
  var
    s: String;
    c: Byte;
    b: Byte;
  begin
  s := '';
  for c := 1 to 4 do
    begin
    s := Char(48+(I and 15)+Byte((I and 15) > 9)*7)+s;
    I := I div 16
    end;
  ToHex := s;
  end;

procedure DelDoubles;
  var
    t, ls, p: Byte;
    j: Boolean;
  begin
  t := 1;
  ls := Length(St);
  j := True;
  while t+ls <= Length(Source) do
    begin
    if Source[t] = '"' then
      j := not j;
    if j then
      p := Pos(St, Source);
    if j and (p <> 0) then
      Delete(Source, p, 1)
    else
      Inc(t);
    end;
  end;

procedure AnsiDelDoubles;
  var
    t, ls, p: LongInt;
    j: Boolean;
  begin
  t := 1;
  ls := Length(St);
  j := True;
  while t+ls <= Length(Source) do
    begin
    if Source[t] = '"' then
      j := not j;
    if j then
      p := Pos(St, Source);
    if j and (p <> 0) then
      Delete(Source, p, 1)
    else
      Inc(t);
    end;
  end;

{Cat}
procedure LongDelDoubles;
  var
    t, ls, p: LongInt;
    j: Boolean;
  begin
  t := 1;
  ls := Length(St);
  j := True;
  while t+ls <= Length(Source) do
    begin
    if Source[t] = '"' then
      j := not j;
    if j then
      p := Pos(St, Source);
    if j and (p <> 0) then
      Delete(Source, p, 1)
    else
      Inc(t);
    end;
  end;
{/Cat}

procedure MakeDate;
  begin
  MakeDateFull(Mode, Day, Month, Year, Hour, Min, S, False);
  end;

procedure MakeDateFull; {-$VOL modified}

  procedure GetDig(R: Byte; var S);
    {$IFNDEF NOASM}
    assembler;
    {&Frame-} {$USES EBX, ECX}
  asm
   mov ebx, S
   xor eax,eax
   mov al, R
   mov ecx, 10
   div cl
   add al, 48
   add ah, 48
   cmp al, 48
   jnz  @@1
   mov al, '0'
  @@1:
   mov [ebx], ax
  end;
    {$ELSE}
    begin
    Word(S) := Word(((R div 10)+Ord('0'))+((R mod 10)+Ord('0')) shl 8);
    end;
  {$ENDIF}

  begin { MakeDateFull }
  if YFull then
    GetDig(Year div 100, S[16]);
  Year := Year mod 100;
  FillChar(S, 16, 32);
  SetLength(S, 15);
  Move(CountryInfo.DateSep[1], S[3], Length(CountryInfo.DateSep));
  Move(CountryInfo.DateSep[1], S[6], Length(CountryInfo.DateSep));
  Move(CountryInfo.TimeSep[1], S[12], Length(CountryInfo.TimeSep));
  case CountryInfo.DateFmt of
    0:
      begin
      GetDig(Day, S[4]);
      GetDig(Month, S[1]);
      GetDig(Year, S[7]);
      end;
    1:
      begin
      GetDig(Day, S[1]);
      GetDig(Month, S[4]);
      GetDig(Year, S[7]);
      if S[1] = '0' then
        S[1] := ' ';
      end;
    2:
      begin
      GetDig(Day, S[7]);
      GetDig(Month, S[4]);
      GetDig(Year, S[1]);
      end;
  end {case};
  GetDig(Min, S[13]);
  if CountryInfo.TimeFmt = 0 then
    begin
    if Hour <= 12 then
      if Hour = 12 then
        S[15] := 'p'
      else
        S[15] := 'a'
    else
      begin
      Dec(Hour, 12);
      S[15] := 'p'
      end;
    end
  else
    SetLength(S, Length(S)-1);
  GetDig(Hour, S[10]);
  if S[10] = '0' then
    S[10] := ' ';
  if YFull then
    case CountryInfo.DateFmt of
      0, 1:
        S := Copy(S, 1, 6)+S[16]+S[17]+Copy(S, 7, MaxStringLength);
      2:
        S := S[16]+S[17]+S;
    end {case};
  end { MakeDateFull };

const
  LastCase: Byte = 2;

  {Cat: эта процедура больше не используется}
  (*
procedure MakeCase(CaseSensitive: Boolean);
begin
 if Byte(CaseSensitive) <> LastCase then
 if CaseSensitive then
  {$IFNDEF BIT_32}
  asm
   lea bx, Tran
   add bx, 255
   mov cx, 256
 @@1:
   mov ax, cx
   dec ax
   mov ds:[bx], al
   dec bx
   loop @@1
  end
  {$ELSE}
  asm
   push ebx
   push ecx
   lea ebx, Tran
   add ebx, 255
   mov ecx, 256
 @@1:
   mov eax, ecx
   dec eax
   mov [ebx], al
   dec ebx
   loop @@1
   pop ecx
   pop ebx
  end
  {$ENDIF}
 else Move(UpCaseArray, Tran, 256);
 LastCase := Byte(CaseSensitive);
end;
*)

function DumpStr;
  {$IFNDEF NOASM}
  var
    S: String;
  begin
  DumpStr := '';
  if Count <= 0 then
    Exit;
  SetLength(S, Count*4+12);
  asm
  push  edi
  push  ebx
  push  esi
  push  edx
  push  ecx
  mov   edi, B
  lea   ebx, S
  inc   ebx
  mov   ecx, Count
  mov   al, byte ptr Addr+3
  mov   edx, ecx
  mov   ah, al
  mov   ecx, 4
  shr   al, cl
  mov   ecx, edx
  and   ah,0Fh
  add   al,'0'
  cmp   al,58
  jc    @@01
  add   al,7
@@01:
  add   ah,'0'
  cmp   ah,58
  jc    @@02
  add   ah,7
@@02:
  mov   [ebx], ax
  mov   al, byte ptr Addr+2
  mov   edx, ecx
  mov   ah, al
  mov   ecx, 4
  shr   al, cl
  mov   ecx, edx
  and   ah,0Fh
  add   al,'0'
  cmp   al,58
  jc    @@11
  add   al,7
@@11:
  add   ah,'0'
  cmp   ah,58
  jc    @@12
  add   ah,7
@@12:
  mov   [ebx+2], eax
  mov   al, byte ptr Addr+1
  mov   edx, ecx
  mov   ah, al
  mov   ecx, 4
  shr   al, cl
  mov   ecx, edx
  and   ah,0Fh
  add   al,'0'
  cmp   al,58
  jc    @@21
  add   al,7
@@21:
  add   ah,'0'
  cmp   ah,58
  jc    @@22
  add   ah,7
@@22:
  mov   [ebx+4], eax
  mov   al, byte ptr Addr
  mov   edx, ecx
  mov   ah, al
  mov   ecx, 4
  shr   al, cl
  mov   ecx, edx
  and   ah,0Fh
  add   al,'0'
  cmp   al,58
  jc    @@31
  add   al,7
@@31:
  add   ah,'0'
  cmp   ah,58
  jc    @@32
  add   ah,7
@@32:
  mov   [ebx+6], ax
  mov   ax,' :'
  mov   [ebx+8], ax
  add   bx, 10
  mov   esi, ebx
  add   esi, ecx
  add   esi, ecx
  add   esi, ecx
  mov   ax, 0b320h
  mov   [esi], ax
  add   esi, 2
@@2:
  mov   al, [edi]
  mov   dl, al

    cmp  Filter, 0
    jz   @@@2
    cmp  dl, 32
    jnc  @@@4
@@@5:
    mov  dl, 250
    jmp  @@@2
@@@4:
    cmp  Filter, 1
    jnz  @@@2
    cmp  dl, 128
    jnc  @@@5
@@@2:


  or    al, al
  jnz   @@3
  mov   dl, 0
@@3:
  mov   [esi], dl
  mov   edx, ecx
  mov   ah, al
  mov   ecx, 4
  shr   al, cl
  mov   ecx, edx
  and   ah,0Fh
  add   al,'0'
  cmp   al,58
  jc    @@41
  add   al,7
@@41:
  add   ah,'0'
  cmp   ah,58
  jc    @@42
  add   ah,7
@@42:
  mov   [ebx], ax
  mov   al, ' '
  mov   [ebx+2], al
  add   ebx, 3
  inc   esi
  inc   edi
  loop  @@2
  pop   ecx
  pop   edx
  pop   esi
  pop   ebx
  pop   edi
 end;
  DumpStr := S;
  end { DumpStr };
  {$ELSE}
  var
    S: String;
    i, j, l, l0, l1: Byte;
    Buf: array[0..$FF] of Byte absolute B;
  begin { DumpStr }
  DumpStr := '';
  if Count <= 0 then
    Exit;
  j := 1;
  SetLength(S, Count*4+12);
  for i := 0 to 3 do
    begin
    l := Addr shr ((3-i)*8); { call }
    l0 := (l shr 4)+$30;
    l1 := (l and $0F)+$30;
    if l0 > $39 then
      Inc(l0, 7);
    if l1 > $39 then
      Inc(l1, 7);
    S[j] := Char(l0);
    Inc(j);
    S[j] := Char(l1);
    Inc(j);
    end;
  S[9] := ':';
  S[10] := ' ';
  Inc(j, 2);
  S[j+Count*3] := Char($B3);
  S[j+1+Count*3] := ' ';
  for i := 0 to Count-1 do
    begin
    l := Buf[i];
    l0 := (l shr 4)+$30;
    l1 := (l and $0F)+$30;
    if l0 > $39 then
      Inc(l0, 7);
    if l1 > $39 then
      Inc(l1, 7);
    if Filter <> 0 then
      if l < $20 then
        l := 250;
    if Filter <> 1 then
      if l > $80 then
        l := 250;
    S[j] := Char(l0);
    Inc(j);
    S[j] := Char(l1);
    Inc(j);
    S[j] := ' ';
    Inc(j);
    S[i+13+Count*3] := Char(l);
    end;
  DumpStr := S;
  end { DumpStr };
{$ENDIF}

(* function SearchFor;
 var D: array[Char] of Word;
     ChBuf: array [0..0] of Char absolute B;
     I: Integer;
     BB: Byte;
     M: Word;
     C: Char;
begin
 SearchFor := 0;
 if S = '' then Exit;
 MakeCase(CaseSensitive);
 {$IFNDEF BIT_32}
 asm
  lea di, D
  mov ax, ss
  mov es, ax
  mov cx, 256
  lea bx, S
  mov al, ss:[bx]
  xor ah, ah
  cld
  rep stosw
  mov dx, ax
  mov cx, ax
  lea bx, Tran
  lea si, S
@@@2:
  inc si
  mov al, ss:[si]
  push bx
  add bx, ax
  mov al, ds:[bx]
  pop bx
  mov ss:[si], al
  loop @@@2
  mov cx, dx
  lea si, D
  lea bx, S
@@@1:
  inc bx
  dec dx
  mov al, ss:[bx]
  xor ah, ah
  add ax, ax
  push si
  add si, ax
  mov ss:[si], dx
  pop si
  loop @@@1
 end;
 {$ELSE BIT_32}
 asm
  push edi
  push esi
  push ebx
  push edx
  push ecx
  lea edi, D
  mov ecx, 256
  lea ebx, S
  xor eax, eax
  mov al, [ebx]
  cld
  rep stosw
  mov edx, eax
  mov ecx, eax
  lea ebx, Tran
  lea esi, S
@@@2:
  inc esi
  mov al, [esi]
  push ebx
  add ebx, eax
  mov al, [ebx]
  pop ebx
  mov [esi], al
  loop @@@2
  mov ecx, edx
  lea esi, D
  lea ebx, S
@@@1:
  inc ebx
  dec edx
  xor eax, eax
  mov al, [ebx]
  add eax, eax
  push esi
  add esi, eax
  mov [esi], edx
  pop esi
  loop @@@1
  pop ecx
  pop edx
  pop ebx
  pop esi
  pop edi
 end;
 {$ENDIF BIT_32}
 M := Length(S) - 1;
 for I := 1 to Length(S) do S[I] := Tran[S[I]];
 while M < L do *)

{AK155: Выкинул ту муть, что здесь была, и заменил на
нормальнй BM-поиск, сделанный на основе программы
demobmse.pas из SWAG }
(* Public-domain demo of Boyer-Moore search algorithm.  *)
(* Guy McLoughlin - May 2, 1993.                        *)

(* Boyer-Moore index-table data definition.             *)
type
  BMTable = array[0..255] of Byte;

  (***** Create a Boyer-Moore index-table to search with.  *)
procedure Create_BMTable
    ( {output} var BMT: BMTable;
    {input/output} var Pattern: String;
    var UpCaseArray: TXlat; {Cat: UpCase -> UpCaseArray}
    ExactCase: Boolean);
  var
    Index: Byte;
  begin
  FillChar(BMT, SizeOf(BMT), Length(Pattern));
  if not ExactCase then
    for Index := 1 to Length(Pattern) do
      Pattern[Index] := UpCaseArray[Pattern[Index]];
  for Index := 1 to Length(Pattern) do
    BMT[Ord(Pattern[Index])] := (Length(Pattern)-Index)
  end;

(***** Boyer-Moore Search function. Returns 0 if string is not      *)
(*     found. Returns 65,535 if BufferSize is too large.            *)
(*     ie: Greater than 65,520 bytes.                               *)
(*                                                                  *)
function BMsearch( {input } var BMT: BMTable;
    var Buffer;
    BuffSize: LongInt;
    const Pattern: String;
    var UpCaseArray: TXlat; {Cat: UpCase -> UpCaseArray}
    ExactCase: Boolean): {output}LongInt;
  var
    Buffer2: array[1..65520] of Char absolute Buffer;
    Index1,
    Index2,
    PatSize: LongInt;
    c: Char;
    d: LongInt;

  begin
  PatSize := Length(Pattern);
  if PatSize > BuffSize then
    begin
    BMsearch := 0;
    Exit;
    end;
  Index1 := PatSize;
  Index2 := PatSize;
  repeat
    if ExactCase then
      c := Buffer2[Index1]
    else
      c := UpCaseArray[Buffer2[Index1]];
    if  (c = Pattern[Index2]) then
      begin
      Dec(Index1);
      Dec(Index2)
      end
    else
      begin
      d := BMT[Ord(c)];
      if Succ(PatSize-Index2) > d then
        Inc(Index1, Succ(PatSize-Index2))
      else
        Inc(Index1, d);
      Index2 := PatSize
      end;
  until (Index2 < 1) or (Index1 > BuffSize);
  if  (Index1 > BuffSize) then
    BMsearch := 0
  else
    BMsearch := Succ(Index1)
  end { BMsearch };
(* BMsearch.                                            *)

{Cat: обратный Boyer-Moore-поиск, переделал из прямого}
procedure Create_BackBMTable
    ( {output} var BMT: BMTable;
    {input/output} var Pattern: String;
    var UpCaseArray: TXlat;
    ExactCase: Boolean);
  var
    Index: Byte;
  begin
  FillChar(BMT, SizeOf(BMT), Length(Pattern));
  if not ExactCase then
    for Index := 1 to Length(Pattern) do
      Pattern[Index] := UpCaseArray[Pattern[Index]];
  for Index := Length(Pattern) downto 1 do
    BMT[Ord(Pattern[Index])] := (Index-1)
  end;

function BackBMsearch( {input } var BMT: BMTable;
    var Buffer;
    BuffSize: LongInt;
    const Pattern: String;
    var UpCaseArray: TXlat;
    ExactCase: Boolean): {output}LongInt;
  var
    Buffer2: array[1..65520] of Char absolute Buffer;
    Index1,
    Index2,
    PatSize: LongInt;
    c: Char;
    d: LongInt;

  begin
  PatSize := Length(Pattern);
  if PatSize > BuffSize then
    begin
    BackBMsearch := 0;
    Exit;
    end;
  Index1 := BuffSize-PatSize+1;
  Index2 := 1;
  repeat
    if ExactCase then
      c := Buffer2[Index1]
    else
      c := UpCaseArray[Buffer2[Index1]];
    if  (c = Pattern[Index2]) then
      begin
      if  (Index1 < BuffSize) then
        Inc(Index1);
      Inc(Index2)
      end
    else
      begin
      d := BMT[Ord(c)];
      if Index2 > d then
        Dec(Index1, Index2)
      else
        Dec(Index1, d);
      Index2 := 1
      end;
  until (Index2 > PatSize) or (Index1 < 1);
  if  (Index1 < 1) then
    BackBMsearch := 0
  else
    BackBMsearch := (Index1-PatSize)
  end { BackBMsearch };
{/Cat}

{Cat: переписал эту процедуру с использованием обратного Boyer-Moore-поиска}
(*
{AK155 Убрал ненужное и неправильное обращение к FillWord
и слегка оптимизировал код}
function BackSearchForCP(S: String; var B; L: LongInt; CaseSensitive: Boolean): LongInt;
 var D: array[Char] of Word;
     ChBuf: array [0..0] of Char absolute B;
     I: LongInt;
     BB: Byte;
     ls, M: LongInt;
     C: Char;
begin
 BackSearchFor := 0;
 if S = '' then Exit;
 MakeCase(CaseSensitive);
 ls := Length(S);
 for I := 1 to ls do S[I] := Tran[S[I]];
 for C := low(D) to high(D) do D[C] := ls;
 for I := ls downto 1 do
   D[S[I]] := I - 1;
 M := L - ls;
 while M >= 0 do
  begin
   C := Tran[ChBuf[M]];
   if C = S[1] then
    begin
     for I := 0 to ls - 1 do
      begin
       if Tran[ChBuf[M+I]] <> S[I+1] then Break;
       if I = ls - 1 then begin BackSearchFor := M + 1; Exit; end;
      end;
     Dec(M);
    end else Dec(M, D[C]);
  end;
end;
{/AK155}
*)

{Cat: раньше регистронезависимый поиск по всем кодовым страницам был на
      самом деле регистронезависимым только для 866 страницы, т.к. для
      других страниц тоже использовался стандартный UpCaseArray, теперь я
      сделал собственный UpCaseArray для каждой кодовой страницы и заменил
      функции SearchFor и BackSearchFor на SearchForCP и BackSearchForCP}
function SearchForCP(S: String; var B; l: LongInt; var UpCaseArray: TXlat;
    CaseSensitive: Boolean): LongInt;
  var
    BMT: BMTable;
  begin
  {MakeCase(UpCaseArray, CaseSensitive);}
  Create_BMTable(BMT, S, UpCaseArray, CaseSensitive);
  SearchForCP := BMsearch(BMT, B, l, S, UpCaseArray, CaseSensitive);
  end;

function BackSearchForCP(S: String; var B; l: LongInt;
     var UpCaseArray: TXlat;
    CaseSensitive: Boolean): LongInt;
  var
    BMT: BMTable;
  begin
  {MakeCase(UpCaseArray, CaseSensitive);}
  Create_BackBMTable(BMT, S, UpCaseArray, CaseSensitive);
  BackSearchForCP := BackBMsearch(BMT, B, l, S, UpCaseArray,
       CaseSensitive);
  end;

function SearchFor(const S: String; var B; l: LongInt;
     CaseSensitive: Boolean): LongInt;
  begin
  SearchFor := SearchForCP(S, B, l, UpCaseArray_Ascii_Ascii,
       CaseSensitive);
  end;

function BackSearchFor(const S: String; var B; l: LongInt;
     CaseSensitive: Boolean): LongInt;
  begin
  BackSearchFor := BackSearchForCP(S, B, l, UpCaseArray_Ascii_Ascii,
       CaseSensitive);
  end;

function SearchForAllCP(const S: String; var B; l: LongInt;
     CaseSensitive: Boolean): LongInt;
  var
    Res: array[0..6] of LongInt;
    I, Res2: LongInt;
  begin
  {CURRENT}
  Res[0] := SearchForCP(S, B, l, UpCaseArray_Ascii_Ascii, CaseSensitive);
  {ASCII-ANSI}
  Res[1] := SearchForCP(Ascii_Ansi(S), B, l, UpCaseArray_Ascii_Ansi,
       CaseSensitive);
  {ASCII-KOI8R}
  Res[2] := SearchForCP(Ascii_Koi8r(S), B, l, UpCaseArray_Ascii_Koi8r,
       CaseSensitive);
  {ANSI-ASCII}
  Res[3] := SearchForCP(Ansi_Ascii(S), B, l, UpCaseArray_Ansi_Ascii,
       CaseSensitive);
  {KOI8R-ASCII}
  Res[4] := SearchForCP(koi8r_ascii(S), B, l, UpCaseArray_Koi8r_Ascii,
       CaseSensitive);
  {ASCII-ANSI , KOI8R-ASCII}
  Res[5] := SearchForCP(koi8r_ascii(Ascii_Ansi(S)), B, l,
       UpCaseArray_Ascii_Ansi_Koi8r_Ascii, CaseSensitive);
  {ASCII-KOI8 , ANSI-ASCII}
  Res[6] := SearchForCP(Ansi_Ascii(Ascii_Koi8r(S)), B, l,
       UpCaseArray_Ascii_Koi8r_Ansi_Ascii, CaseSensitive);

  Res2 := Res[0];
  for I := 1 to 6 do
    if  (Res2 = 0) or ((Res[I] < Res2) and (Res[I] > 0)) then
      Res2 := Res[I];
  SearchForAllCP := Res2;
  end { SearchForAllCP };

function BackSearchForAllCP(const S: String; var B; l: LongInt;
     CaseSensitive: Boolean): LongInt;
  var
    Res: array[0..6] of LongInt;
    I, Res2: LongInt;
  begin
  {CURRENT}
  Res[0] := BackSearchForCP(S, B, l, UpCaseArray_Ascii_Ascii,
       CaseSensitive);
  {ASCII-ANSI}
  Res[1] := BackSearchForCP(Ascii_Ansi(S), B, l, UpCaseArray_Ascii_Ansi,
       CaseSensitive);
  {ASCII-KOI8R}
  Res[2] := BackSearchForCP(Ascii_Koi8r(S), B, l,
       UpCaseArray_Ascii_Koi8r, CaseSensitive);
  {ANSI-ASCII}
  Res[3] := BackSearchForCP(Ansi_Ascii(S), B, l, UpCaseArray_Ansi_Ascii,
       CaseSensitive);
  {KOI8R-ASCII}
  Res[4] := BackSearchForCP(koi8r_ascii(S), B, l,
       UpCaseArray_Koi8r_Ascii, CaseSensitive);
  {ASCII-ANSI , KOI8R-ASCII}
  Res[5] := BackSearchForCP(koi8r_ascii(Ascii_Ansi(S)), B, l,
       UpCaseArray_Ascii_Ansi_Koi8r_Ascii, CaseSensitive);
  {ASCII-KOI8 , ANSI-ASCII}
  Res[6] := BackSearchForCP(Ansi_Ascii(Ascii_Koi8r(S)), B, l,
       UpCaseArray_Ascii_Koi8r_Ansi_Ascii, CaseSensitive);

  Res2 := Res[0];
  for I := 1 to 6 do
    if  (Res[I] > Res2) then
      Res2 := Res[I];
  BackSearchForAllCP := Res2;
  end { BackSearchForAllCP };
{/Cat}

function NewStr(const S: String): PString;
  var
    P: PString;
  begin
  (*
  GetMem(P, Length(S) + 1);
  if (P <> nil) then
    P^:=S else
    FatalError('No memory for new string');
  NewStr:=P;
  *)
  if S = '' then
    P := nil
  else
    begin
    GetMem(P, Length(S)+1);
    P^:= S;
    end;
  NewStr := P;
  end;

procedure DisposeStr(var P: PString);
  begin
  if P <> nil then
    FreeMem(P, Length(P^)+1);
  P := nil;
  end;

procedure ReplaceP(var P: PString; S: String);
  begin
  DelRight(S);
  if P = nil then
    begin
    if S <> '' then
      P := NewStr(S);
    end
  else if Length(S) = Length(P^) then
    P^:= S
  else
    begin
    DisposeStr(P);
    P := NewStr(S);
    end;
  end;

function CnvString(P: PString): String;
  begin
  if P = nil then
    CnvString := ''
  else
    CnvString := P^;
  end;

{Cat}
function CnvLongString(P: PLongString): LongString;
  begin
  if P = nil then
    CnvLongString := ''
  else
    CnvLongString := P^;
  end;
{/Cat}

{Cat: переписал процедуры NewStr, DisposeStr для совместимости с AnsiString
      добавил процедуры NewLongStr, DisposeLongStr
      вариант не окончательный, возможны изменения
      при изменении обязательно согласовать со следующими процедурами:
        Objects.TStream.ReadAnsiStr
        Objects.TStream.WriteAnsiStr
        WinClpVP.Str2Collection}

function NewLongStr(const S: LongString): PLongString;
  var
    P: PLongString;
  begin
  if S = '' then
    P := nil
  else
    begin
    New(P);
    SetLength(P^, Length(S));
    Move(S[1], P^[1], Length(S))
    end;
  NewLongStr := P;
  end;

procedure DisposeLongStr(var P: PLongString);
  begin
  if P <> nil then
    Dispose(P);
  P := nil;
  end;
{/Cat}

procedure CompressShortStr(var S: String);
  var
    PP: Pointer;
    TSt: Integer;
  begin
  PP := @S;
  TSt := StoI(EditorDefaults.TabSize);
  if TSt = 0 then
    TSt := 8;
  asm
    push ebx
    push edx
    push edi
    push esi
    mov ebx, PP
    xor ecx, ecx
    mov cl, [ebx]
    inc ebx
    jcxz @@Ex
    xor edi, edi
    xor esi, esi
    mov byte ptr [ebx-1], ch
  @@1:
    mov ah, byte ptr TSt
    xor edx, edx
  @@2:
    mov al, [ebx+esi]
    mov [ebx+edi], al
    inc esi
    cmp esi, ecx
    ja  @@Ex
    inc edi
    inc byte ptr [ebx-1]
    cmp al, ' '
    jne @@3
    inc dl
    jmp @@4
   @@3:
    xor dl, dl
   @@4:
    dec ah
    jnz @@2
    or  dl, dl
    jz @@5
    dec dl
    jz @@5
    sub edi, edx
    sub byte ptr [ebx-1], dl
    mov al, 9
    mov [ebx+edi-1], al
   @@5:
    jmp @@1
  @@Ex:
    pop esi
    pop edi
    pop edx
    pop ebx
 end;
  end { CompressShortStr };

procedure CompressString(var S: LongString);
  var
    S1: String;
    S2: LongString;
    I: Word;
  begin
  I := 1;
  S2 := '';
  repeat
    S1 := Copy(S, I, 254);
    CompressShortStr(S1);
    S2 := S2+S1;
    Inc(I, 254);
  until S1 = '';
  S := S2;
  end;

(*
{Cat: добавил поддержку длинных строк
      теперь эта процедура используется вместо аналогичных из MicroEd и Ed2}
procedure CompressString(var S: LongString);
{$IFDEF USELONGSTRING}
{Cat}
var
  I, L, SpaceCount: LongInt;
  TSt1: Integer;
begin
  TSt1 := StoI(EditorDefaults.TabSize);
  if TSt1 = 0 then TSt1 := 8;
  Dec(TSt1);
  L := Length(S);
  I := 1;
  SpaceCount := 0;
  while I<=L do
    begin
      if S[I]=' ' then
        begin
          Inc(SpaceCount);
          if SpaceCount=TSt1+1 then
            begin
              S[I] := #9;
              Delete(S, I-TSt1, TSt1);
              Dec(I, TSt1);
              Dec(L, TSt1);
              SpaceCount := 0;
            end;
        end
      else
        SpaceCount := 0;
      Inc(I);
    end;
end;
{/Cat}
{$ELSE}
var PP: Pointer;
    TSt: Integer;
begin
 PP := @S;
 TSt := StoI(EditorDefaults.TabSize);
 if TSt = 0 then TSt := 8;
 {$IFNDEF BIT_32}
 asm
    les bx, PP
    mov cl, es:[bx]
    inc bx
    xor ch, ch
    jcxz @@Ex
    xor di, di
    xor si, si
    mov byte ptr es:[bx-1], ch
  @@1:
    mov ah, byte ptr TSt
    xor dx, dx
  @@2:
    mov al, es:[bx][si]
    mov es:[bx][di], al
    inc si
    cmp si, cx
    ja  @@Ex
    inc di
    inc byte ptr es:[bx-1]
    cmp al, ' '
    jne @@3
    inc dl
    jmp @@4
   @@3:
    xor Dl, dl
   @@4:
    dec ah
    jnz @@2
    or  dl, dl
    jz @@5
    dec dl
    jz @@5
    sub di, dx
    sub byte ptr es:[bx-1], dl
    mov al, 9
    mov es:[bx][di-1], al
   @@5:
    jmp @@1
  @@Ex:
 end;
 {$ELSE BIT_32}
 asm
    push ebx
    push edx
    push edi
    push esi
    mov ebx, PP
    xor ecx, ecx
    mov cl, [ebx]
    inc ebx
    jcxz @@Ex
    xor edi, edi
    xor esi, esi
    mov byte ptr [ebx-1], ch
  @@1:
    mov ah, byte ptr TSt
    xor edx, edx
  @@2:
    mov al, [ebx+esi]
    mov [ebx+edi], al
    inc esi
    cmp esi, ecx
    ja  @@Ex
    inc edi
    inc byte ptr [ebx-1]
    cmp al, ' '
    jne @@3
    inc dl
    jmp @@4
   @@3:
    xor dl, dl
   @@4:
    dec ah
    jnz @@2
    or  dl, dl
    jz @@5
    dec dl
    jz @@5
    sub edi, edx
    sub byte ptr [ebx-1], dl
    mov al, 9
    mov [ebx+edi-1], al
   @@5:
    jmp @@1
  @@Ex:
    pop esi
    pop edi
    pop edx
    pop ebx
 end;
 {$ENDIF}
end;
{$ENDIF}
*)

{AK155}

function PosLastDot(StrToMake: String): Byte;
  var
    I: Byte;
  begin
  for I := Length(StrToMake) downto 1 do
    if StrToMake[I] = '.' then
      begin
      PosLastDot := I;
      Exit;
      end;
  PosLastDot := Length(StrToMake)+1;
  end;

function IsDummyDir(const DirName: String): Boolean;
  begin
  IsDummyDir := (DirName = '.') or (DirName = '..');
  end;

procedure CopyShortString(const s1, s2: ShortString);
  {&Frame-} {$USES ebx,ecx,edi,esi}
asm
  mov esi,s1
  mov edi,s2
  xor ecx,ecx
  mov cl,[esi]
  mov [edi],cl
  jecxz @@0
@@1:
  inc esi
  inc edi
  mov al,[esi]
  mov [edi],al
  loop @@1
@@0:
end;

{/AK155}

end.
