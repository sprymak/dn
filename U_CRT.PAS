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
Unit u_crt;
interface
uses crt, u_direct, dos, objects;

{$I buttcode.inc}

Type
  TYearFormat = (yfShort, yf2000);

  PSItem = ^TSItem; {цепочка строк}
  TSItem = Record
    S: PString;
    N: PSItem;
  end;

  procedure save_old(var p:pointer;x1,y1,x2,y2:integer);         {save screen area in heap}
  procedure restore_old(var p:pointer;x1,y1,x2,y2:integer);      {restore screena rea from heap}
  function  rec_size(x1,y1,x2,y2:integer):word;                  {get size of heap to store area}
  function  int2str(i:longint;d:integer):string;                 {convert integer to string}
  function  rel2str(r:real;m,f:integer):string;                  {convert real to string}
  function  dig2str(d:word):string;                              {convert word to string with 2 char min; example: 5 --> '05'}
  procedure RealVal(S: String; var R: Real; var Error: Integer); {runtime error free convert String to Real}
  function  UpperCase(s:sTriNG):STRING;      {make upper case of string ! russia char too}
  function  LowerCase(s:sTriNG):STRING;      {make lower case of string ! russia char too}
  procedure cls(attr:byte);                  {clear screen with attr}
  procedure ClearKeyb;                       {clear keyboard buffer}
  procedure ScrollWindowUp(X1,Y1,X2,Y2,Count,C:byte);            {scroll screen area up ! BIOS}
  procedure ScrollWindowDown(X1,Y1,X2,Y2,Count,C:byte);          {scroll screen area down ! BIOS}
  procedure ClearWindow(X1, Y1, X2, Y2: Integer; C: Byte);       {clear screen area with attr ! BIOS}
  procedure Wait;                         {wait for any key down}
  procedure WaitTick(n:word);             {wait n tick of system clock ! normaly 18.2 tick by sec}
  function  YNWait:boolean;               {wait for Y(y,enter) or N(n,esc) : true if Y down}
  procedure GetCode(var code:word);       {get last code down or bcNoKey else}
  procedure WaitCode(var code:word);      {wait while user press a key and get it}
  function  NewStr(S: String): PString;   {allocate dynamic string ! not PChar - 100% string compitable, max 255 char length}
  procedure DisposeStr(var P: PString);   {free dynamic string from heap}

  {routines for manipulating with chain of string}
  function  NewSItem(AString: String; Next: PSItem): PSItem;    {allocation; insert as "Next" another NextSItem and get chain}
  function  NumberSItems(Items: PSItem): Word;                  {calculate number of strings in chain}
  function  MaxLenSItems(Items: PSItem): Integer;               {get max length of string in chain}
  procedure DisposeSItem(Items: PSItem);                        {free all chain}
  function  CopySItem(Source: PSItem): PSItem;                  {make copy of all chain}

const
  MaxWord = 65528;
  TabMask = '        ';

Type
  TAlignment = (LeftAlign, RightAlign, CenterAlign);

  PByteArray = ^TByteArray;
  TByteArray = array[0..MaxWord] of Byte;

  PWordArray = ^TWordArray;
  TWordArray = array[0..MaxWord div SizeOf(Word)] of Word;

  PLondArray = ^TLongArray;
  TLongArray = array[0..MaxWord div SizeOf(Longint)] of Longint;

  function  ExpandStr(S: String; Len: Integer; Align: TAlignment): String;
                                               {expand string to need length with alignment}
  function  TabInsertStr(S: String): String;   {replace #9 with TabMask}
  function  Trim(S: String): String;      {cut spaces from begin and end}
  function  Replace(S, SubFrom, SubTo: String): String; {replace SubFrom on SubTo in S}

  procedure SetMode(h:byte);                   {set text mode by BIOS number}
  procedure SetModeSave(h:byte);               {set mode without image corruption}

  procedure BlinkON;                           {set blink mode}
  procedure BlinkOFF;                          {set light mode (blink off)}
  procedure HideCursor;                        {hide cursor}
  procedure ShowCursor;                        {show cursor}
  procedure SetCursor(Hi,Lo:byte);             {set cursor size Hi end Lo in [0..F]}
  procedure PutCursor(x,y:byte);               {put cursor in x,y position 1..132 or more,1..43 or more}
  procedure GetCursor(var x,y:byte);           {get cursor position}
  procedure GetCursorSize(var Hi, Lo:byte);    {get cursor metrics}
  function  CursorVisible: Boolean;            {get cursor visibility}
  function  CursorX:byte;                      {get x cursor position}
  function  CursorY:byte;                      {get y cursor position}
  function  InsDown:boolean;                   {get Ins state, true if down}
  function  ShiftDown:boolean;                 {get Shift state, true if down}
  function  LShiftDown:boolean;                {get LeftShift state, true if down}
  function  RShiftDown:boolean;                {get RightShift state, true if down}
  function  AltDown:boolean;                   {get Alt state, true if down}
  function  CtrlDown:boolean;                  {get Ctrl state, true if down}
  function  NumLockDown:boolean;               {get NumLock state, true if down}
  function  CapsLockDown:boolean;              {get CapsLock state, true if down}
  function  ScrollLockDown:boolean;            {get ScrollLock state, true if down}
  function  InsON:boolean;                     {get Ins state, true if ON}
  function  NumLockON:boolean;                 {get NumLock state, true if ON}
  function  CapsLockON:boolean;                {get CapsLock state, true if ON}
  function  ScrollLockON:boolean;              {get ScrollLock state, true if ON}
  function  CheckKeyb:boolean;                 {аналогичная KeyPressed; ture is char in}
  procedure VSync;                             {wait for vertical synhronize}

  procedure Beep;                              {make a beep with PC internal speaker}
  procedure DeathSound;                        {make a strange sound}

const YearFormat: TYearFormat = yfShort;

  function  Date: String;             {get current date with format 10.12.1998}
  function  Time: String;             {get current time with format 12:43:51}
  function  Year: String;             {get current year}

  procedure WritelnX( N: Integer);    {make writeln N times}
  procedure WritelnColor(S: String; TAttr1, TAttr2: Byte);

  function  PosExt(Substr, S: String; BeginPos: Byte): Byte;
  function  SplitLine(S, SubStr: String; var LeftPart, RightPart: String): Boolean;
  function  PosCase(Substr, S: String): Byte; {аналог Pos но case insensitive}
  function  ExtractFromStr(S, LeftS, RightS: String): String; { ('..(aabcc)..', '(', ')') --> 'aabcc' }
  function  Bool2Str(B: Boolean): String;

  procedure SwapStr(PS: PString; ANewValue: String);

  function  Date2Str(ATime: Longint): String;
  function  Time2Str(ATime: Longint): String;
  function  DateTime2StrF(ADateTime: DateTime; Format: String): String;
  function  Time2StrF(ATime: Longint; Format: String): String;
        {Format:           DD   - День    (10)
                           MM   - Месяц   (12)
                           MON  - Месяц   (Дек)
                           YY   - Год     (98)
                           YYYY - Год     (1998)
                           HH   - Час     (10)
                           MIN  - Минута  (25)
                           SS   - Секунды (43)}
  function  Size2Str(L: Longint): String;

implementation

function  dig2str(d:word):string;
 var s:string[30];
 begin
  Str(d,s);
  if (d<10) then S:='0'+S;
  dig2str:=s;
 end;

function  int2str(i:longint;d:integer):string;
 var s:string[30];
 begin
   if d=0 then Str(i,s) else Str(i:d,s);
   int2str:=s;
 end;

function  rel2str(r:real;m,f:integer):string;
var s:string[30];
begin
  if m=0 then Str(r,s) else Str(r:m:f,s);
  rel2str:=s;
end;

function  UpperCase(s:sTriNG):STRING;
 var i:integer;
     sn:string;
 begin
   sn:=s;
   for i:=1 to byte(s[0]) do
    begin
      if (s[i] in ['a'..'z'])or(s[i] in ['а'..'п']) then sn[i]:=char(byte(s[i])-$20) else
      if (s[i] in ['р'..'я']) then sn[i]:=char(byte(s[i])-$50) else sn[i]:=s[i];
    end;
   UpperCase:=sn;
 end;

function  LowerCase(s:sTriNG):STRING;
 var i:integer;
     sn:string;
 begin
   sn[0]:=s[0];
   for i:=1 to byte(s[0]) do
    begin
      if (s[i] in ['A'..'Z'])or(s[i] in ['А'..'П']) then sn[i]:=char(byte(s[i])+$20) else
      if (s[i] in ['Р'..'Я']) then sn[i]:=char(byte(s[i])+$50) else sn[i]:=s[i];
    end;
   LowerCase:=sn;
 end;

function  CheckKeyb:boolean;
 begin
   if Mem[Seg0040:$001A]=Mem[Seg0040:$001C] then CheckKeyb:=false else CheckKeyb:=true;
 end;

procedure ClearKeyb;
begin
  while CheckKeyb do Wait;
end;

procedure Wait;
begin
  if ReadKey=#0 then ReadKey;
end;

function  YNWait:boolean;
var ch:char;
begin
  YNWait := false;
  repeat ch := ReadKey until (ch='y') or (ch='Y') or (ch='n') or (ch='N') or (ch=#13) or (ch=#27);
  if (ch='y') or (ch='Y') or (ch=#13) then YNWait:=true;
end;

function  rec_size(x1,y1,x2,y2:integer):word;
begin
   MakeValidRect(X1, Y1, X2, Y2);
   rec_size := (x2 - x1 + 1) * (y2 - y1 + 1) shl 1;
end;

procedure save_old(var p:pointer;x1,y1,x2,y2:integer);
var size, s, o, count, offs:word;
    i:integer;
begin
   MakeValidRect(X1, Y1, X2, Y2);
   count:=(x2-x1+1) shl 1;
   size:=count*(y2-y1+1);
   GetMem(p, size);
   s:=seg(p^);
   o:=ofs(p^);              {*80 = *(64+16)=*64+*16=shl 6 + shl 4}
   x1 := x1 - 1;
   for i := y1 to y2 do
   begin
     offs := i - 1;
     offs := ((offs shl 6) + (offs shl 4) + x1) shl 1;
     Move(Mem[SegB800:offs], Mem[s:o+(i-y1) * count], count);
   end;
end;

procedure restore_old(var p:pointer;x1,y1,x2,y2:integer);
var s, o, count, offs:word;
    i:integer;
begin
  MakeValidRect(X1, Y1, X2, Y2);
  if p<>nil then
  begin
    count:=(x2-x1+1) shl 1;
    s:=seg(p^);
    o:=ofs(p^);
    x1 := x1 - 1;
    VSync;
    for i:=y1 to y2 do
    begin
      offs := i - 1;
      offs := ((offs shl 6) + (offs shl 4) + x1) shl 1;
      Move(Mem[s:o+(i-y1)*count], Mem[SegB800:offs], count);
    end;
  end;
end;

procedure cls(attr:byte);
begin
  ClearWindow(0, 0, 80, 25, attr);
end;

procedure SetMode(h:byte);
begin
  asm
    mov ah,00h
    mov al,h
    int 10h
  end;
end;

procedure SetModeSave(h:byte);
begin
  asm
    mov ah,00h
    mov dl,h
    or  dl,10000000b
    mov al,dl
    int 10h
  end;
end;

procedure BlinkON;
   begin
    asm
      mov ah,10h
      mov al,03h
      mov bl,01h
      int 10h
    end;
   end;

procedure BlinkOFF;
   begin
    asm
      mov ah,10h
      mov al,03h
      mov bl,0h
      int 10h
    end;
   end;

procedure HideCursor;
begin
  asm
    mov ah,03h
    mov bh,0h
    int 10h
    or  ch,00100000b
    mov ah,01h
    int 10h
  end;
end;

procedure ShowCursor;
begin
  asm
    mov ah,03h
    mov bh,0h
    int 10h
    and ch,10011111b
    mov ah,01h
    int 10h
  end;
end;

function CursorVisible: Boolean;
var Hi, Lo: Byte;
begin
  GetCursorSize(Hi, Lo);
  CursorVisible := (Hi AND $20 = 0);
end;

procedure SetCursor(Hi, Lo:byte);
   begin
    asm
      mov ah,01h
      mov ch,Hi
      mov cl,Lo
      and ch,00011111b
      int 10h
    end;
   end;

procedure PutCursor(x,y:byte);
   begin
    Dec(x);
    Dec(y);
    asm
      mov ah,02h
      mov bh,0h
      mov dh,y
      mov dl,x
      int 10h
    end;
   end;

procedure GetCursor(var x,y:byte);
 var rx,ry:byte;
 begin
   asm
     mov ah,03h
     mov bh,0h
     int 10h
     mov rx,dl
     mov ry,dh
   end;
   x:=rx+1;
   y:=ry+1;
 end;

procedure GetCursorSize(var Hi, Lo:byte);
 var rx,ry:byte;
 begin
   asm
     mov ah,03h
     mov bh,0h
     int 10h
     mov rx,cl
     mov ry,ch
   end;
   Hi:=ry+1;
   Lo:=rx+1;
 end;

function  CursorX:byte;
   var x:byte;
   begin
    asm
      mov ah,03h
      mov bh,0h
      int 10h
      mov x,dl
    end;
    CursorX:=x+1;
   end;

function  CursorY:byte;
   var y:byte;
   begin
    asm
      mov ah,03h
      mov bh,0h
      int 10h
      mov y,dh
    end;
    CursorY:=y+1;
   end;

function  InsDown:boolean;
begin
  InsDown := (Mem[Seg0040:$18] AND 128)=128;
end;

function  LShiftDown:boolean;
begin
  LShiftDown := (Mem[Seg0040:$17] AND 2) = 2;
end;

function  RShiftDown:boolean;
begin
  RShiftDown := (Mem[Seg0040:$17] AND 1) = 1;
end;

function  ShiftDown:boolean;
begin
   ShiftDown := LShiftDown or RShiftDown;
end;

function  AltDown:boolean;
begin
  AltDown := (Mem[Seg0040:$17] AND 8) = 8;
end;

function  CtrlDown:boolean;
begin
  CtrlDown := (Mem[Seg0040:$17] AND 4) = 4;
end;

function  NumLockDown:boolean;
begin
  NumLockDown := (Mem[Seg0040:$18] AND 32) = 32;
end;

function  CapsLockDown:boolean;
begin
  CapsLockDown := (Mem[Seg0040:$18] AND 64) = 64;
end;

function  ScrollLockDown:boolean;
begin
  ScrollLockDown := (Mem[Seg0040:$18] AND 16) = 16;
end;

function  InsON:boolean;
 begin
   if (Mem[Seg0040:$17] AND 128)=128 then InsON:=true else InsOn:=false;
 end;

function  NumLockON:boolean;
begin
  NumLockOn := (Mem[Seg0040:$17] AND 32) = 32;
end;

function  CapsLockON:boolean;
begin
  CapsLockOn := (Mem[Seg0040:$17] AND 64) = 64;
end;

function  ScrollLockON:boolean;
begin
  ScrollLockOn := (Mem[Seg0040:$17] AND 16) = 16;
end;

procedure Beep;
begin
  Sound(220);
  Delay(200);
  NoSound;
end;

procedure DeathSound;
var i:integer;
begin
  for i:=1 to 10 do begin
     Sound(random(8000)+5000);
     Delay(random(100));
     NoSound;
  end;
end;

procedure ScrollWindowUp;
 begin
   dec(x1);dec(x2);dec(y1);dec(y2);
   asm
     mov ah,6
     mov al,Count
     mov ch,Y1
     mov cl,X1
     mov dh,Y2
     mov dl,X2
     mov bh,C
     int 10h
   end;
 end;

procedure ScrollWindowDown(X1,Y1,X2,Y2,Count,C:byte);
 begin
   dec(x1);dec(x2);dec(y1);dec(y2);
   asm
     mov ah,7
     mov al,Count
     mov ch,Y1
     mov cl,X1
     mov dh,Y2
     mov dl,X2
     mov bh,C
     int 10h
   end;
 end;

procedure ClearWindow(X1, Y1, X2, Y2: Integer; C: Byte);
 var r: Registers;
 begin
   MakeValidRect(X1, Y1, X2, Y2);
   with r do
   begin
     ah := 6;
     al := 0;
     ch := Y1 - 1;
     cl := X1 - 1;
     dh := Y2 - 1;
     dl := X2 - 1;
     bh := C;
   end;
   Intr($10, r);
 end;

procedure WaitTick(n:word);
 var Timer:longint absolute $0040:$006C;
     OldTime:longint;
 begin
   OldTime:=Timer;
   while n>0 do if OldTime<>Timer then begin OldTime:=Timer;dec(n);end;
 end;

procedure GetCode(var code:word);
 begin
  if KeyPressed then WaitCode(code) else code:=0;
 end;

procedure WaitCode(var code:word);
 var ch:byte;
 begin
   code:=0;
   ch:=byte(ReadKey);
   if ch=0 then code:=word(ReadKey) shl 8
   else code:=ch;
 end;

function  NewStr(S: String): PString;
var P: PString;
begin
  if S = '' then P := nil else
  begin
    GetMem(P, Length(S) + 1);
    P^ := S;
  end;
  NewStr := P;
end;

procedure DisposeStr(var P: PString);
begin
  if P <> Nil then FreeMem(P, Length(P^) + 1);
  P := Nil;
end;

procedure VSync;
begin
  while (Port[$3DA] AND $08)<>0 do;
  while not ((Port[$3DA] AND $08)<>0) do;
end;

procedure RealVal(S: String; var R: Real; var Error: Integer);
var S2   : String;
    P, N : Integer;
    Pow  : Longint;
    RR   : Real;
begin
  S2 := LowerCase(S) + '*';
  Val(S2, R, Error);
  if (Error = Length(S2)) and (S2[Error] = '*') then
  begin
    P := Pos('e', S);
    if P = 0 then Val(S, R, Error) else
    begin
      S2 := Copy(S, P + 1, Length(S) - P);
      Val(S2, Pow, Error);
      if Error = 0 then
      begin
        S2 := Copy(S, 1, P - 1);
        Val(S2, RR, Error);
        if Error = 0 then
        begin
          N := 0;
          while ABS(RR) > 9 do
          begin
            RR := RR / 10;
            Inc(N);
          end;
          Pow := Pow + N;
          if Abs(Pow) <= 37 then Val(S, R, Error)
          else Error := Length(S);
        end else Error := Length(S);
      end else Error := Length(S);
    end;
  end;
end;

function ExpandStr(S: String; Len: Integer; Align: TAlignment): String;
var I: Integer;
    PS: String;
begin
  if Length(S) >= Len then ExpandStr := Copy(S, 1, Len) else begin
    PS := '';
    for I := 1 to Len - Length(S) do PS := PS + ' ';
    case Align of
      LeftAlign: ExpandStr := S + PS;
      RightAlign: ExpandStr := PS + S;
      CenterAlign: begin
          I := Length(PS) div 2;
          ExpandStr := Copy(PS, 1, I) + S + Copy(PS, 1, Length(PS) - I);
        end;
    end;
  end;
end;

function  TabInsertStr(S: String): String;
var P: Integer;
begin
  repeat
    P := Pos(#9, S);
    if P <> 0 then S := Copy(S, 1, P - 1) + TabMask + Copy(S, P + 1, Length(S) - P);
  until P = 0;
  TabInsertStr := S;
end;

function  NewSItem(AString: String; Next: PSItem): PSItem;
var SI: PSItem;
begin
  New(SI);
  with SI^ do
  begin
    S := NewStr(AString);
    N := Next;
  end;
  NewSItem := SI;
end;

function  NumberSItems(Items: PSItem): Word;
var P: PSItem;
    N: Word;
begin
  N := 0;
  P := Items;
  while P <> Nil do
  begin
    Inc(N);
    P := P^.N;
  end;
  NumberSItems := N;
end;

function  MaxLenSItems(Items: PSItem): Integer;
var P: PSItem;
    N: Integer;
begin
  N := 0;
  P := Items;
  while P <> Nil do
  begin
    if N < Length(P^.S^) then N := Length(P^.S^);
    P := P^.N;
  end;
  MaxlenSItems := N;
end;

procedure DisposeSItem(Items: PSItem);
var P1, P2: PSItem;
begin
  if Items = Nil then Exit;
  P1 := Items;
  repeat
    P2 := P1^.N;
    DisposeStr(P1^.S);
    Dispose(P1);
    P1 := P2;
  until (P1 = Nil);
end;

function  CopySItem(Source: PSItem): PSItem;
var P1, P2, N, N2: PSItem;
begin
  P1 := Source;
  P2 := Source^.N;
  N := NewSItem(P1^.S^, Nil);
  CopySItem := N;
  while P2 <> Nil do
  begin
    P1 := P2;
    P2 := P1^.N;
    N2 := NewSItem(P1^.S^, Nil);
    N^.N := N2;
    N := N2;
  end;
end;

function  Trim(S: String): String;
var I, N: Integer;
begin
  N := 0;
  I := 1;
  while S[I] = #32 do begin
    Inc(N);
    Inc(I);
  end;
  if N > 0 then S := Copy(S, N + 1, Length(S) - N);
  N := 0;
  I := Length(S);
  while S[I] = #32 do begin
    Inc(N);
    Dec(I);
  end;
  if N > 0 then S := Copy(S, 1, Length(S) - N);
  Trim := S;
end;

function  Replace(S, SubFrom, SubTo: String): String;
var P: Integer;
begin
  repeat
    P := Pos(SubFrom, S);
    if P <> 0 then begin
      S := Copy(S, 1, P - 1) + SubTo + Copy(S, P + Length(SubFrom), Length(S) - P - Length(SubFrom) + 1);
    end;
  until (P = 0);
  Replace := S;
end;

function  Date: String;
var Year, Month, Day, DayOfWeek: Word;
    S: String;
begin
  GetDate(Year, Month, Day, DayOfWeek);
  S := Dig2Str(Year);
  if YearFormat = yfShort then S := Copy(S, 3, 2);
  Date := Dig2Str(Day) + '.' + Dig2Str(Month) + '.' + S;
end;

function  Time: String;
var Hour, Minute, Second, Sec100: Word;
begin
  GetTime(Hour, Minute, Second, Sec100);
  Time := Dig2Str(Hour) + ':' + Dig2Str(Minute) + ':' + Dig2Str(Second);
end;

procedure WritelnX( N: Integer);
var I: Integer;
begin
  for I := 1 to N do Writeln;
end;

procedure WritelnColor(S: String; TAttr1, TAttr2: Byte);
begin
  TextAttr := TAttr1;
  Write(S);
  TextAttr := TAttr2;
  Writeln;
end;

function  PosExt(Substr, S: String; BeginPos: Byte): Byte;
var P: Integer;
begin
  if BeginPos = 0 then BeginPos := 1;
  S := Copy(S, BeginPos, Length(S) - BeginPos + 1);
  P := Pos(SubStr, S);
  if P <> 0 then P := P + BeginPos - 1;
  PosExt := P;
end;

function  SplitLine(S, SubStr: String; var LeftPart, RightPart: String): Boolean;
var P: Integer;
begin
  LeftPart  := '';
  RightPart := '';
  SplitLine := False;
  P := Pos(SubStr, S);
  if P = 0 then Exit;
  SplitLine := True;
  LeftPart  := Copy(S, 1, P - 1);
  RightPart := Copy(S, P + Length(SubStr), Length(S) - P - Length(SubStr) + 1);
end;

function  PosCase(Substr, S: String): Byte;
begin
  PosCase := Pos(UpperCase(SubStr), UpperCase(S));
end;

function  ExtractFromStr(S, LeftS, RightS: String): String;
var LP, RP: Integer;
begin
  LP := Pos(LeftS, S);
  RP := PosExt(RightS, S, LP + 1);
  if (LP <> 0) and (RP <> 0) then
    ExtractFromStr := Copy(S, LP + 1, RP - LP - 1)
  else ExtractFromStr := '';
end;

function  Bool2Str(B: Boolean): String;
const BoolStr: array[False..True] of String[5] = ('FALSE', 'TRUE');
begin
  Bool2Str := BoolStr[B];
end;

procedure SwapStr(PS: PString; ANewValue: String);
begin
  DisposeStr(PS);
  PS := NewStr(ANewValue);
end;

function  Year: String;
var AYear, Month, Day, DayOfWeek: Word;
begin
  GetDate(AYear, Month, Day, DayOfWeek);
  Year := Int2Str(AYear, 4);
end;

function  Date2Str(ATime: Longint): String;
var DT: DateTime;
    S: String;
begin
  UnpackTime(ATime, DT);
  S := Dig2Str(DT.Year);
  if YearFormat = yfShort then S := Copy(S, 3, 2);
  Date2Str := Dig2Str(DT.Day) + '.' + Dig2Str(DT.Month) + '.' + S;
end;

function  Time2Str(ATime: Longint): String;
var DT: DateTime;
begin
  UnpackTime(ATime, DT);
  Time2Str := Dig2Str(DT.Hour) + ':' + Dig2Str(DT.Min) + ':' + Dig2Str(DT.Sec);
end;

function  Time2StrF(ATime: Longint; Format: String): String;
var DT: DateTime;
begin
  UnpackTime(ATime, DT);
  Time2StrF := DateTime2StrF(DT, Format);
end;

function  DateTime2StrF(ADateTime: DateTime; Format: String): String;
var S: String;
    DD, MM, MON, YY, YYYY, HH, MIN, SS, DW: String;
const
    SMonth: array[1..12] of String[3] = ('Янв', 'Фев', 'Мар', 'Апр', 'Мая',
                             'Июн', 'Июл', 'Авг', 'Сен', 'Окт', 'Ноя', 'Дек');
begin
  DD := Dig2Str(ADateTime.Day);
  MM := Dig2Str(ADateTime.Month);
  HH := Dig2Str(ADateTime.Hour);
  MIN := Dig2Str(ADateTime.Min);
  SS := Dig2Str(ADateTime.Sec);
  YYYY := Dig2Str(ADateTime.Year);
  YY := Copy(YYYY, 3, 2);
  if ADateTime.Month in [1..12] then MON := SMonth[ADateTime.Month] else MON := '';
  S := Format;
  S := Replace(S, 'YYYY', YYYY);
  S := Replace(S, 'YY', YY);
  S := Replace(S, 'MM', MM);
  S := Replace(S, 'MON', MON);
  S := Replace(S, 'DD', DD);
  S := Replace(S, 'HH', HH);
  S := Replace(S, 'MIN', MIN);
  S := Replace(S, 'SS', SS);
  DateTime2StrF := S;
end;

function  Size2Str(L: Longint): String;
var S, SS: String;
begin
  S := Int2Str(L, 1);
  SS := '';
  while S <> '' do begin
    if Length(S) > 3 then begin
      if SS <> '' then SS := '`' + SS;
      SS := Copy(S, Length(S) - 2, 3) + SS;
      S := Copy(S, 1, Length(S) - 3);
    end else begin
      if SS <> '' then SS := '`' + SS;
      SS := S + SS;
      S := '';
    end;
  end;
  Size2Str := SS;
end;

end.
