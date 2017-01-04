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

unit Advance1; {String functions}
interface
uses objects, advance, dos, lfn, u_keymap;

function  NewStr(const S: String): PString;
procedure DisposeStr(var P: PString);
procedure ReplaceP(var P: PString; S: String);
function  CnvString(P: PString): String; {conversion: PString to String}

function  StrGrd(AMax, ACur: TSize; Wide: Byte; Rev: boolean): string;
function  Percent(AMax, ACur: TSize): TSize;
procedure Hex8Lo(L:longInt;var HexLo);
Procedure AddStr(var S ; C : char);
procedure DelFC(var s:string);

{---  ' '-related string functions }
FUNCTION  CenterStr(const s:string; N:byte):string; {DataCompBoy}
FUNCTION  AddSpace(const s:string; N:byte):string;
FUNCTION  PredSpace(s:string; N:byte):string;
Function  DelSpaces(s : string) : string;
procedure DelSpace(var s : string);
Procedure DelRight(var S: string);
Function fDelRight(s : string) : string;
procedure DelLeft (var S: string);
Function fDelLeft (s : string) : string;

{case function}
Function  Strg(c:char; Num: Byte) : string;

Function  UpCase(c : Char) : Char;
Procedure UpStr(var s : string);
Function  UpStrg(s : string) : string;

Function  LowCase(c : Char) : Char;
Procedure LowStr(var s : string);
Function  LowStrg(s : string) : string;

procedure CapStr(var S: String);
function  CapStrg(S: String): String;

procedure MakeCase(CaseSensitive: Boolean);

Function  ItoS(a:longint):string;
Function  ZtoS(a:TSize):string;
Function  RtoS(a:real;m,f:integer):string;
Function  StoI(const s:string):Longint;
Function  SStr(a:longint;b:Byte;c:Char):string;
Function  SSt2(a:longint;b:Byte;c:Char):string;
Function  FStr(a:TSize):string;
Function  FileSizeStr( X: TSize ): string;
Function  Hex2(a : byte) : Str2;
Function  Hex4(a : word) : Str4;
Function  Hex8(a : LongInt) : Str8;
Function  HexChar(a : byte) : char;
FUNCTION  Replace(const Pattern, ReplaceString: string; var S: string): Boolean;
FUNCTION fReplace(const SubFrom, SubTo: String; S: string): String;
FUNCTION  PosChar(C: Char; S: string): Byte;
function  FormatTimeStr(H, M, SS: Word): string; {DataCompBoy}
procedure MakeCurrency(R: Real; var S: string);
function  GetDateTime(Time: Boolean): string;
FUNCTION  Real2Str(X: Real; N: Byte): string;
FUNCTION  Long2Str(X: Longint; L: Byte): string;
FUNCTION  Long0Str(X: Longint; L: Byte): string;
FUNCTION  ToHex(I: Word): string;
function  MakeCMDParams(const S, Fl1, Fl2: string): string; {DataCompBoy}
Procedure DelDoubles(const St : string;var Source : string);
procedure MakeDate(const Mode,Day,Month,Year,Hour,Min: Word; var S: string);
procedure MakeDateFull(const Mode,Day,Month: Word; {-$VOL moidfied}
                       Year,Hour: Word;
                       const Min: Word;
                       var S: string;
                       const YFull: boolean);
Function  DumpStr(var B; Addr: LongInt; Count: Integer; Filter: Byte): string;

Function  BackSearchFor(S: string;var B;L: Word; CaseSensitive: Boolean): Word;
Function  SearchFor(S: string;var B;L: Word; CaseSensitive: Boolean): Word;
Function  BackSearchForAllCP(const S: string;var B;L: Word; CaseSensitive: Boolean): Word; {-$VIV 14.05.99}
Function  SearchForAllCP(const S: string;var B;L: Word; CaseSensitive: Boolean): Word; {-$VIV ::}

procedure FormatStr(var Result: String; const Format: String; var Params);
procedure PrintStr(const S: String);
procedure CompressString(var S: String);

implementation
uses advance2, advance3;

 function LeadingZero(w: Word): string;
   var s: string;
 begin
   Str(w:0, s);
   LeadingZero := Copy('00', 1, 2 - Length(s)) + s;
 end;


function StrGrd(AMax, ACur: TSize; Wide: Byte; Rev: Boolean): string;
var
  A: Byte;
begin
  LowPrec(AMax, ACur);
  if AMax = 0 then A := Wide else A := Round((ACur*Wide) / AMax);
  if Rev then StrGrd := Strg(#177, Wide-A)+Strg(#219,A)
         else StrGrd := Strg(#219,A)+Strg(#177, Wide-A);
end;

function Percent(AMax, ACur: TSize): TSize;
begin
  LowPrec(AMax, ACur);
  if AMax = 0 then Percent := 0 else Percent := (ACur*100) / AMax;
end;

procedure Hex8Lo(L:longInt;var HexLo);assembler;
{$IFNDEF NOASM}
asm
        cld
        les     di,[HexLo]
        lea     bx,[LoHexChar]
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
{$ELSE}
type ChArr: array[1..4] of char;
begin
 ChArr(HexLo)[1]:=LoHexChar[L shr 24];
 ChArr(HexLo)[2]:=LoHexChar[L and $00FF0000 shr 16];
 ChArr(HexLo)[3]:=LoHexChar[L and $0000FF00 shr 08];
 ChArr(HexLo)[4]:=LoHexChar[L and $000000FF];
{$ENDIF}
end;

Procedure AddStr(var S ; C : char);
{$IFNDEF NOASM}
assembler ;
asm
   cld
   les Di,S
   inc ES:DI.byte
   mov al,ES:DI.byte
   sub ah,ah
   add DI,AX
   mov al,C
   Stosb
{$ELSE}
begin
 String(S):=String(S)+C;
{$ENDIF}
end;

procedure DelFC(var s:string);
var
  sl: byte absolute s;
begin
  if sl>0 then begin Dec(sl); Move(s[2], s[1], sl) end;
end;

FUNCTION CenterStr(const s:string; N:byte):string;
begin
 if length(s)>=N then CenterStr:=Copy(S, 1, N)
  else CenterStr:=Copy(Strg(#32, (N-Length(S)) div 2)+S+Strg(#32, (N-Length(S)) div 2 + 1), 1, N);
end;

FUNCTION AddSpace(const s:string; N:byte):string;
{$IFNDEF NOASM}
assembler;
asm
   cld
   push  ds
   lds   si, s
   les   di, @Result
   lodsb
   mov   ah, N
   mov   ch, 0
   cmp   al, ah
   jae   @JustCopy
   mov   [es:di], ah
   inc   di
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
   pop  ds
{$ELSE}
var s2: string;
begin
 s2:=s;
 if length(s)<n then begin
  fillchar(s2[length(s2)+1], n-length(s2), ' ');
  s2[0]:=char(n);
 end;
 AddSpace:=s2;
{$ENDIF}
end;

FUNCTION PredSpace;
begin
  If Length(S)>=N then PredSpace:=S else begin
    FillChar(FreeStr[1],255,' ');
    Move(S[1], FreeStr[succ(N-Length(S))], Length(S));
    FreeStr[0]:=char(N); PredSpace:=FreeStr
  end
end;

procedure DelSpace;
{$IFNDEF NOASM}
assembler;
asm
  les bx, S
  xor ch, ch
  mov cl, es:[bx]
  jcxz @@1
  mov  di, 1
  mov  si, 1
  xor  al, al
  mov  es:[bx], al
@@2:
  mov  al, es:[bx][di]
  cmp  al, ' '
  jz   @@3
  cmp  al, 9
  jz   @@3
  mov  es:[bx][si], al
  inc  byte ptr es:[bx]
  inc  si
@@3:
  inc  di
  loop @@2
@@1:
{$ELSE}
var a, b, j: byte;
begin
 if s='' then exit;
 b:=1;
 j:= length(s); s[0]:=#0;
 for a:=1 to j do
  if not (s[a] in [' ',#9]) then begin s[b]:=s[a]; inc(s[0]); inc(b); end;
{$ENDIF}
end;

Function DelSpaces;
begin
 DelSpace(S);
 DelSpaces := S;
end;

Procedure DelRight;
{$IFNDEF NOASM}
assembler ;
asm
     les Di,S
     mov bl,ES:DI.byte
     sub bh,bh
     or  BX,BX
     jz  @@3
 @@1:
     mov al, ES:[DI+BX]
     cmp al, ' '
     je  @@D
     cmp al, 9
     jne @@2
 @@D:
     dec BX
     jnz @@1
 @@2:
     mov ES:DI.byte,bl
 @@3:
{$ELSE}
var q: byte absolute S;
begin
 for q:=q downto 1 do if S[q]<>' ' then break;
{$ENDIF}
end;

Function fDelRight(s : string) : string;
begin DelRight(S); fDelRight:=s end;

procedure DelLeft(var S: string);
var
  I: Byte;
  SL: byte absolute S;
begin
  I := 1; while (SL>=I) and (S[I] in [#9,' ']) do Inc(I);
  if I>1 then
  begin
    Dec(SL, I-1);
    Move(S[I], S[1], SL);
  end;
end;

Function fDelLeft(s : string) : string;
begin DelLeft(S); fDelLeft:=s end;

Function Strg;
{$IFNDEF NOASM}
assembler;
asm
  les  di, @Result
  mov  al, Num
  stosb
  mov  ch, 0
  mov  cl, al
  mov  al, c
  rep  stosb
{$ELSE}
var S: string;
begin
 fillchar(s[1], num, c);
 s[0]:=char(num);
 Strg:=s;
{$ENDIF}
end;


Function LowCase(c : Char) : Char;
begin if c=#0 then LowCase:=#0 else LowCase:=LowCaseArray[c] end;

Procedure LowStr(var s : string);
var i:integer;
begin for i:=1 to Length(s) do if s[i]<>#0 then s[i]:=LowCaseArray[s[i]]; end;

Function LowStrg(s : string) : string;
begin LowStr(s); LowStrg:=s; end;


Function UpCase(c : Char) : Char;
begin if c=#0 then UpCase:=#0 else UpCase:=UpCaseArray[c] end;

Procedure UpStr(var s : string); var i:integer;
begin for i:=1 to Length(s) do if s[i]<>#0 then s[i]:=UpCaseArray[s[i]]; end;

Function UpStrg(s : string) : string;
begin UpStr(s); UpStrg:=s; end;

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

function CapStrg(S: String): String;
begin CapStr(s); CapStrg:=s; end;


Function  ItoS(a:longint):string;
 var s : string[12];
begin
 Str(a,s);
 ItoS:=s;
end;

Function  ZtoS(a:TSize):string;
 var s : string[20];
begin
 Str(a:0:0,s);
 ZtoS:=s;
end;

Function  RtoS(a:real;m,f:integer):string;
 var s : string[20];
begin
 Str(a:m:f,s);
 RtoS:=s;
end;

Function  StoI(const s:string):Longint;
 var i : longint;
     j : Integer;
begin
 Val(s,i,j);
 StoI:=i;
end;

Function  SStr(a:longint;b:Byte;c:Char):string;
 var s : string[40];
     i : integer;
begin
 Str(a:b,s);i:=1;
 While i<b do
  begin
   if s[i]=' ' then s[i]:=c else i:=255;
   Inc(i);
  end;
 SStr:=s;
end;

Function  SSt2(a:longint;b:Byte;c:Char):string;
 var s : string[40];
begin
 Str(a:b,s);
 while (s[1]=' ') do begin DelFC(s); s:=s+' ' end;
 While b>0 do
  begin
   if s[b]=' ' then s[b]:=c else b:=1;
   dec(b);
  end;
 SSt2:=s;
end;

Function  FStr(a:TSize):string;
 var s,s1 : string[40];
     s1l: byte absolute s1;
     i : Integer;
{$IFNDEF NOASM} C: Char;{$ENDIF}
begin
 Str(A:0:0, S);
 if CountryInfo.ThouSep[0] > #0 then
   begin
      s1:='';
      {$IFNDEF NOASM}
      C := CountryInfo.ThouSep[1];
      asm
       lea si, s
       lea di, s1
       mov bx, di
       mov al, ss:[si]
       xor ah, ah
       mov cx, 3
       div cl
       mov byte ptr ss:[bx], 0
       mov cl, ss:[si]
       mov dl, ah
       Mov al, C
       or  dl, dl
       jnz @@1
       mov dl, 3
      @@1:
       or  cl, cl
       jz  @@3
      @@2:
       mov ah, ss:[si+1]
       mov byte ptr ss:[di+1], ah
       inc byte ptr ss:[bx]
       inc di
       inc si
       dec cl
       jz  @@3
       dec dl
       jnz @@2
       mov dl, 3
       mov byte ptr ss:[di+1], al
       inc di
       inc byte ptr ss:[bx]
       jmp @@2
      @@3:
      {$ELSE}
      for i:=length(s) downto 1 do begin
       s1:=s[i]+s1;
       if (i>1) and ((byte(s[0])-i+1) mod 3=0) then s1:=CountryInfo.ThouSep[1]+s1;
      {$ENDIF}
      end;
   end else S1 := S;
 if Length(S1)>12 then
 begin
   Dec(s1l,3);
   s1[s1l] := 'K';
 end;
 if a=MaxLongInt then
  FStr:='>='+s1 else FStr:=s1;
end;

Function FileSizeStr;
var
  S: string[40];
  J: TSize;
label K;
begin
  If X >= 0
   then If X < 10000000 then FileSizeStr := FStr( X )
                        else begin
                              J := X / 1024;
                              If J >= 10000000then begin
                                                    J := X / 1024;
                                                    Str( J:0:0, S );
                                                    If Length( S ) > 3 then Insert( ',', S, Length( S )-2 );
                                                    FileSizeStr := S + 'M';
                                                    exit;
                                                   end;
                              Str( J:0:0, S );
                              If Length( S ) > 3 then Insert( ',', S, Length( S )-2 );
                              If X = MaxLongInt then FileSizeStr := '>=' + S + 'K'
                                                else FileSizeStr := S + 'K';
                             end
   else FileSizeStr := '?'
end;

Function Hex2;
begin Hex2:=HexChar(a shr 4)+HexChar(a) end;

Function Hex4;
begin
 Hex4:=HexChar(Hi(a) shr 4)+HexChar(Hi(a))+
       HexChar(Lo(a) shr 4)+HexChar(Lo(a));
end;

Function Hex8;
var s: Str8;
begin
 s[0]:=#8; Hex8Lo(a,s[1]); Hex8 := s;
end;

Function HexChar(a : byte) : char;
{$IFNDEF NOASM}
assembler;
label Loc1;
asm
   mov al,a
   and al,0Fh
   add al,'0'
   cmp al,58
   jc  Loc1
   add al,7
Loc1:
{$ELSE}
begin
 a:=a and 15;
 if a<10 then HexChar:=Char(Ord('0')+a)
  else HexChar:=Char(Ord('A')+a-10);
{$ENDIF}
end;

function Replace;
 var I, J, K: Integer;
begin
 J := 1; K := 1; Replace := False;
 if (Pattern = '') or (S = '') then Exit;
 repeat
  I := Pos(Pattern, Copy(S, J, 255));
  if I > 0 then
   begin
    Delete(S, J+I-1, Byte(Pattern[0]));
    Insert(ReplaceString, S, J+I-1);
    Replace := True;
   end;
  K := I;
  Inc(J, I + Length(ReplaceString) - 1);
 until I = 0;
end;

function fReplace(const SubFrom, SubTo: String; S: string): String;
var P: Integer;
begin
  repeat
    P := Pos(SubFrom, S);
    if P <> 0 then begin
      S := Copy(S, 1, P - 1) + SubTo + Copy(S, P + Length(SubFrom), Length(S) - P - Length(SubFrom) + 1);
    end;
  until (P = 0);
  fReplace := S;
end;

FUNCTION  PosChar;
{$IFNDEF NOASM}
assembler;
asm
  les di,S
  xor ch,ch
  mov cl,byte ptr es:[di]
  mov bx,cx
  inc di
  cld
  mov al,C
  repne scasb
  jnz @S
  sub bx,cx
  mov al,bl
  jmp @Q
@S:xor al,al
@Q:
{$ELSE}
var i: byte;
begin
 i:=1;
 for i:=1 to length(s) do if s[i]=c then break;
 if s[i]<>c then PosChar:=0 else PosChar:=i;
{$ENDIF}
end;

function GetDateTime(Time: Boolean): string;
 var S: string[30];
     Y,M,D,DW: Word;
     H,Mn,SS,S100: Word;

begin
  GetDate(Y,M,D,DW);
  GetTime(H,Mn,SS,S100);
  MakeDateFull(0, D, M, Y, H, Mn, S, not Time); {-$VOL modified}
  if Time then S := FormatTimeStr(H, Mn, SS)
          else S[0] := #10;
  GetDateTime := S;
end;

function FormatTimeStr(H, M, SS: Word): string;
 var N: string[3];
     S: string[20];
begin
 if (CountryInfo.TimeFmt = 0) and (H > 12) then
  begin
     S := LeadingZero(h-12) + CountryInfo.TimeSep + LeadingZero(m) + CountryInfo.TimeSep + LeadingZero(ss);
     N := 'pm';
   end
 else
   begin
     S := LeadingZero(h)+ CountryInfo.TimeSep + LeadingZero(m) + CountryInfo.TimeSep + LeadingZero(ss);
     if CountryInfo.TimeFmt = 0
        then if (H < 12) then N := 'am' else N := 'pm'
        else N := '';
   end;
 FormatTimeStr := S + N;
end;

procedure MakeCurrency;
 var I: Integer;
begin
  with CountryInfo do
   begin
     if (DecSign >= '0') and (DecSign <= '9') then I := Byte(DecSign[1])-48 else I := 2;
     Str(R:0:I, S);
     I := PosChar('.', S);
     if I = 0 then I := Length(S)
              else Move(DecSep[1], S[I], Length(DecSep));
     case CurrencyFmt of
       0: S := Currency + S;
       1: S := S + Currency;
       2: S := Currency + ' ' + S;
       3: S := S + ' ' + Currency;
       4: Insert(Currency, S, I);
     end;
   end;
end;

FUNCTION Real2Str; begin System.Str(X:N, FreeStr); Real2Str:=FreeStr end;
FUNCTION Long2Str; begin Str(x:l,FreeStr); Long2Str:=FreeStr; end;
FUNCTION Long0Str; var I: Byte;
begin Str(x:l, FreeStr); For I:=1 to length(FreeStr) do if FreeStr[I]=' ' then FreeStr[I]:='0'; Long0Str:=FreeStr end;
FUNCTION ToHex; var s:string; c:byte; b:byte;
begin s:=''; for c:=1 to 4 do begin
  s:=char(48 + (i and 15) + byte( (i and 15)>9)*7 )+s; i:=i div 16 end;
  ToHex:=s;
end;

function MakeCMDParams(const S, Fl1, Fl2: string): string;
var
      Dr1, Dr2: String;
      Nm1, Nm2: String;
      Xt1, Xt2: String;
begin
  FreeStr := S;
  lFSplit(Fl1, Dr1, Nm1, Xt1);
  lFSplit(Fl2, Dr2, Nm2, Xt2);
  Replace('%%', #0, FreeStr);
  Replace('%11', Nm2+Xt2, FreeStr);
  Replace('%12', Dr2, FreeStr);
  Replace('%13', Nm2, FreeStr);
  Replace('%14', Xt2, FreeStr);
  Replace('%15', Copy(Dr2, 1, Length(Dr2)-Byte(Length(Dr2)>3)), FreeStr);
  Replace('%16', Copy(Dr2, 1, 2), FreeStr);
  Replace('%1', Nm1+Xt1, FreeStr);
  Replace('%2', Dr1, FreeStr);
  Replace('%3', Nm1, FreeStr);
  Replace('%4', Xt1, FreeStr);
  Replace('%5', Copy(Dr1, 1, Length(Dr1)-Byte(Length(Dr1)>3)), FreeStr);
  Replace('%6', Copy(Dr1, 1, 2), FreeStr);
  Replace(#0, '%', FreeStr);
  MakeCMDParams := FreeStr
end;

procedure DelDoubles;
var t:byte;
    j:boolean;
    ls:byte;
begin
 t:=1; ls:=length(ST);
 while t+ls<=length(Source) do
  begin
   if Source[t]='"' then j:=not j;
   if (Pos(ST,Source)=1) and (not j) then Delete(Source,1,1) else inc(t);
  end;
end;

procedure MakeDate;
begin
  MakeDateFull(Mode,Day,Month,Year,Hour,Min,S,False);
end;

procedure MakeDateFull; {-$VOL modified}

 procedure GetDig(R: Byte; var S);
 {$IFNDEF NOASM}
 assembler;
 asm
   les bx, S
   mov al, R
   xor ah,ah
   mov cx, 10
   div cl
   add al, 48
   add ah, 48
   cmp al, 48
   jnz  @@1
   mov al, '0'
  @@1:
   mov es:[bx], ax
 {$ELSE}
 begin
  Word(S):=word(((R div 10)+ord('0')) + ((R mod 10)+ord('0')) shl 8);
 {$ENDIF}
 end;

begin
  if YFull then GetDig(Year div 100, S[16]);
  Year := Year mod 100;
  FillChar(S, 16, 32);
  S[0] := #15;
  Move(CountryInfo.DateSep[1], S[3], Length(CountryInfo.DateSep));
  Move(CountryInfo.DateSep[1], S[6], Length(CountryInfo.DateSep));
  Move(CountryInfo.TimeSep[1], S[12], Length(CountryInfo.TimeSep));
  case CountryInfo.DateFmt of
    0: begin
         GetDig(Day, S[4]);
         GetDig(Month, S[1]);
         GetDig(Year, S[7]);
       end;
    1: begin
         GetDig(Day, S[1]);
         GetDig(Month, S[4]);
         GetDig(Year, S[7]);
         if S[1] = '0' then S[1] := ' ';
       end;
    2: begin
         GetDig(Day, S[7]);
         GetDig(Month, S[4]);
         GetDig(Year, S[1]);
       end;
  end;
  GetDig(Min, S[13]);
  if CountryInfo.TimeFmt = 0 then
    begin
      if Hour <= 12 then if Hour = 12 then S[15] := 'p' else S[15] := 'a'
                    else begin Dec(Hour, 12); S[15] := 'p' end;
    end else Dec(S[0]);
  GetDig(Hour, S[10]);
  if S[10] = '0' then S[10] := ' ';
  if YFull then
    case CountryInfo.DateFmt of
      0,1: S := Copy(S, 1, 6) + S[16] + S[17] + Copy(S, 7, 255);
      2  : S := S[16] + S[17] + S;
    end;
end;

const LastCase: Byte = 2;

procedure MakeCase(CaseSensitive: Boolean);
begin
 if Byte(CaseSensitive) <> LastCase then
 if CaseSensitive then
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
  end else Move(UpCaseArray, Tran, 256);
  LastCase := Byte(CaseSensitive);
end;

function DumpStr;
{$IFNDEF NOASM}
 var S: string;
begin
 DumpStr := '';
 if Count <= 0 then Exit;
 S[0] := Char(Count*4+12);
 asm
  les   di, B
  lea   bx, S
  inc   bx
  mov   cx, Count
  mov   al, byte ptr Addr+3
  mov   dx, cx
  mov   ah, al
  mov   cx, 4
  shr   al, cl
  mov   cx, dx
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
  mov   ss:[bx], ax
  mov   al, byte ptr Addr+2
  mov   dx, cx
  mov   ah, al
  mov   cx, 4
  shr   al, cl
  mov   cx, dx
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
  mov   ss:[bx+2], ax
  mov   al, byte ptr Addr+1
  mov   dx, cx
  mov   ah, al
  mov   cx, 4
  shr   al, cl
  mov   cx, dx
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
  mov   ss:[bx+4], ax
  mov   al, byte ptr Addr
  mov   dx, cx
  mov   ah, al
  mov   cx, 4
  shr   al, cl
  mov   cx, dx
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
  mov   ss:[bx+6], ax
  mov   ax,' :'
  mov   ss:[bx+8], ax
  add   bx, 10
  mov   si, bx
  add   si, cx
  add   si, cx
  add   si, cx
  mov   ax, 0b320h
  mov   ss:[si], ax
  add   si, 2
@@2:
  mov   al, es:[di]
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
  mov   dl, '.'
@@3:
  mov   ss:[si], dl
  mov   dx, cx
  mov   ah, al
  mov   cx, 4
  shr   al, cl
  mov   cx, dx
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
  mov   ss:[bx], ax
  mov   al, ' '
  mov   ss:[bx+2], al
  add   bx, 3
  inc   si
  inc   di
  loop  @@2
 end;
{$ELSE}
var
  S: String;
  i, j, l, l0, l1: Byte;
  Buf: Array[0..$FF] of Byte absolute B;
begin
  DumpStr := '';
  if Count <= 0 then Exit;
  j := 1; S[0] := Char(Count*4 + 12);
  for i := 0 to 3 do begin
    l := Addr shr ((3-i)*8); { call }
    l0 := (l shr 4) + $30;
    l1 := (l and $0F) + $30;
    if l0 > $39 then Inc(l0, 7);
    if l1 > $39 then Inc(l1, 7);
    S[j] := Char(l0);
    Inc(j);
    S[j] := Char(l1);
    Inc(j);
  end;
  S[9] := ':'; S[10] := ' '; Inc(j, 2);
  S[j+Count*3] := Char($B3); S[j+1+Count*3] := ' ';
  for i := 0 to Count-1 do begin
    l := Buf[i];
    l0 := (l shr 4) + $30;
    l1 := (l and $0F) + $30;
    if l0 > $39 then Inc(l0, 7);
    if l1 > $39 then Inc(l1, 7);
    if Filter <> 0 then
      if l < $20 then l := 250;
    if Filter <> 1 then
      if l > $80 then l := 250;
    S[j] := Char(l0);
    Inc(j);
    S[j] := Char(l1);
    Inc(j);
    S[j] := ' ';
    Inc(j);
    S[i + 13 + Count*3] := Char(l);
  end;
{$ENDIF}
  DumpStr := S;
end;

function SearchFor;
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
 M := Length(S) - 1;
 for I := 1 to Length(S) do S[I] := Tran[S[I]];
 while M < L do
  begin
   C := Tran[ChBuf[M]];
   if C = S[Byte(S[0])] then
    begin
     for I := 0 to Byte(S[0]) - 1 do
      begin
       if Tran[ChBuf[M-I]] <> S[Byte(S[0])-I] then Break;
       if I = Byte(S[0]) - 1 then begin SearchFor := M - I + 1; Exit; end;
      end;
     Inc(M);
    end else Inc(M, D[C]);
  end;
end;

function BackSearchFor;
 var D: array[Char] of Word;
     ChBuf: array [0..0] of Char absolute B;
     I: Integer;
     BB: Byte;
     M: LongInt;
     C: Char;
begin
 BackSearchFor := 0;
 if S = '' then Exit;
 MakeCase(CaseSensitive);
 FillWord(D, 255, Length(S));
 for I := 1 to Length(S) do S[I] := Tran[S[I]];
 for I := Length(S) downto 1 do
   D[S[I]] := I - 1;
 M := L;
 M := M - Length(S);
 while M >= 0 do
  begin
   C := Tran[ChBuf[M]];
   if C = S[1] then
    begin
     for I := 0 to Byte(S[0]) - 1 do
      begin
       if Tran[ChBuf[M+I]] <> S[I+1] then Break;
       if I = Byte(S[0]) - 1 then begin BackSearchFor := M + 1; Exit; end;
      end;
     Dec(M);
    end else Dec(M, D[C]);
  end;
end;

{-$VIV 14.05.99}

Function  BackSearchForAllCP(const S: string;var B;L: Word; CaseSensitive: Boolean): Word;
var Res: array[0..6] of Word;
    I, Res2: Word;
    S2: String;
begin
  {CURRENT}
  S2 := S;
  Res[0] := BackSearchFor(S2, B, L, CaseSensitive);
  {ASCII-ANSI}
  S2 := ascii_ansi(S);
  Res[1] := BackSearchFor(S2, B, L, CaseSensitive);
  {ASCII-KOI8R}
  S2 := ascii_koi8r(S);
  Res[2] := BackSearchFor(S2, B, L, CaseSensitive);
  {ANSI-ASCII}
  S2 := ansi_ascii(S);
  Res[3] := BackSearchFor(S2, B, L, CaseSensitive);
  {ANSI-ASCII-KOI8R}
  S2 := ascii_koi8r( ansi_ascii(S) );
  Res[4] := BackSearchFor(S2, B, L, CaseSensitive);
  {KOI8R-ASCII}
  S2 := koi8r_ascii(S);
  Res[5] := BackSearchFor(S2, B, L, CaseSensitive);
  {KOI8R-ASCII-ANSI}
  S2 := ascii_ansi( koi8r_ascii(S) );
  Res[6] := BackSearchFor(S2, B, L, CaseSensitive);
  {::}
  Res2 := Res[0];
  for I := 1 to 6 do
   if (Res[I] > Res2) then Res2:= Res[I];
  BackSearchForAllCP := Res2;
end;

Function  SearchForAllCP(const S: string;var B;L: Word; CaseSensitive: Boolean): Word;
var Res: array[0..6] of Word;
    I, Res2: Word;
    S2: String;
begin
  {CURRENT}
  S2 := S;
  Res[0] := SearchFor(S2, B, L, CaseSensitive);
  {ASCII-ANSI}
  S2 := ascii_ansi(S);
  Res[1] := SearchFor(S2, B, L, CaseSensitive);
  {ASCII-KOI8R}
  S2 := ascii_koi8r(S);
  Res[2] := SearchFor(S2, B, L, CaseSensitive);
  {ANSI-ASCII}
  S2 := ansi_ascii(S);
  Res[3] := SearchFor(S2, B, L, CaseSensitive);
  {ANSI-ASCII-KOI8R}
  S2 := ascii_koi8r( ansi_ascii(S) );
  Res[4] := SearchFor(S2, B, L, CaseSensitive);
  {KOI8R-ASCII}
  S2 := koi8r_ascii(S);
  Res[5] := SearchFor(S2, B, L, CaseSensitive);
  {KOI8R-ASCII-ANSI}
  S2 := ascii_ansi( koi8r_ascii(S) );
  Res[6] := SearchFor(S2, B, L, CaseSensitive);
  {::}
  Res2 := Res[0];
  for I := 1 to 6 do
   if (Res2 = 0) or ((Res[I] < Res2) and (Res[I] > 0)) then Res2:= Res[I];
  SearchForAllCP := Res2;
end;

{-$VIV::}

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
  if S = '' then P := nil else
  begin
    GetMem(P, Length(S) + 1);
    P^ := S;
  end;
  NewStr := P;
end;

procedure DisposeStr(var P: PString);
begin
  if P <> nil then FreeMem(P, Length(P^)+1);
  P:=Nil;
end;

procedure ReplaceP(var P: PString; S: String);
begin
  DelRight(S);
  if P = nil then begin
    if S <> '' then P:=NewStr(S);
  end else
  if Length(S) = Length(P^) then
    P^:=S
  else begin
    DisposeStr(P);
    P:=NewStr(S);
  end;
end;

function CnvString(P: PString): String;
begin
  if P = nil then CnvString:='' else CnvString:=P^;
end;

{ String formatting routines }
{$IFNDEF DPMI}
 {$L FORMAT.OBJ}
{$ELSE}
 {$L FORMAT.OBP}
{$ENDIF}

procedure Format_Str(var Result: String; const Format: String; var Params);
  {$IFDEF BIT_16}far;{$ENDIF} external;

procedure FormatStr;
begin
  if @Params = nil then
    Result:=Format else
    Format_Str(Result, Format, Params);
end;

procedure PrintStr(const S: String); assembler;
asm
    PUSH    DS
    LDS     SI,S
    CLD
    LODSB
    XOR     AH,AH
    XCHG    AX,CX
    MOV     AH,40H
    MOV     BX,1
    MOV     DX,SI
    INT     21H
    POP     DS
end;

procedure CompressString(var S: String);
begin
  DelRight(S);
  asm
     les bx, S
     mov cl, es:[bx]
     inc bx
     xor ch, ch
     jcxz @@Ex
     xor di, di
     xor si, si
     mov byte ptr es:[bx-1], ch
   @@1:
     mov ah, 8
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
end;

end.
