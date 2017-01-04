{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source 1.51.05/DOS
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
//////////////////////////////////////////////////////////////////////////
//
//      LFNизация выполнена Антоном Федоpовым aka DataCompBoy'ем.
//                  В честь Матаpыкиной Ульяны...
//////////////////////////////////////////////////////////////////////////}

{$o+}
unit Advance2; {File related functions}
interface
uses advance, dos, lfn, UFnMatch, objects;

function  ExistFile(const FName : String) : Boolean; {DataCompBoy}
function  FileTime(FileA: String): LongInt;
function  FileNewer(FileA, FileB : String) : Boolean;
function  CalcTmpFName(Id: LongInt; const AExt: String; ANew: Boolean): String; {DataCompBoy}
function  CalcTmpId:LongInt;
function  IsDriveCDROM(Drive : Char) : Boolean;
procedure lChDir(S: string); {DataCompBoy}
procedure EraseByName(const FName: String); {DataCompBoy}
Procedure EraseFile(const N: String); {DataCompBoy}
Function  ValidDrive(dr : char) : Boolean;
Function  GetDrive : byte;
Procedure SetDrive(a : byte);
Procedure GetMask(var m : string);
function  GetCurDrive: Char;
procedure SetCurDrive(C: Char);

FUNCTION  GetExt(const s:string):string;
FUNCTION  Norm12(const s:string):Str12;
       {-DataCompBoy-}
FUNCTION  DelSquashes(s: string): string; {yбиpает из S кавычки}
FUNCTION  GetURZ(const s: string): Str12; {возвpащает имя,ypезанное до 8.3}
FUNCTION  GetfURZ(const s: string):String;{возвpащает пyть+имя,ypезанное до 8.3}
FUNCTION  SquashesName(const s:string):string;{если s содеpжит пpобелы или
                               невеpные сомволы, то возвpащает "s" иначе - s}
FUNCTION  InMask(Name, Mask: string):Boolean; {попадает ли Name в Mask}
FUNCTION  InFilter(Name, Filter: string):Boolean; {попадает ли Name в Filter}
FUNCTION  InExtFilter(Name, Filter: string):Boolean; {попадает ли pасшиpение от Name в Filter}
FUNCTION  InOldMask(Name,Mask: string):Boolean;     {DataCompBoy}
FUNCTION  InOldFilter(Name,Filter: string):Boolean; {DataCompBoy}
FUNCTION  InSpaceMask(Name,Mask: string;ValidSpace: Boolean):Boolean;
FUNCTION  InSpaceFilter(Name,Filter: string):Boolean;

FUNCTION  IsDir(const s: string): boolean; {возвp. калог это, или нет}
function  MkName(const Nm, Mask: String): String;{возвp. имя, собp из маски и
                                                 имени}
FUNCTION  GetPath(const S: String): String;
FUNCTION  GetName(const S: String): String;
FUNCTION  GetSName(const S: String): String;
FUNCTION  GetIName(const S: String): String;
FUNCTION  GetAttrF(const S: String): Word;
FUNCTION  GetAttrStr(Attr: Word): Str6;
FUNCTION  GetShortRelPath(Path: string): string;
FUNCTION  GetLongRelPath(Path: string): string;
       {-DataCompBoy-}
FUNCTION  MakeFileName(S: string): string;
FUNCTION  MakeNormName(const S, S1: string): string; {DataCompBoy}
function  FileNameOf(var F: File): string;

implementation
uses advance1, advance3;

        {-DataCompBoy-}
function ExistFile(const FName : String) : Boolean;
var DirInfo:lSearchRec;
begin
 lFindFirst(FName,Archive+ReadOnly+Hidden+SysFile,DirInfo);
 ExistFile:=DosError=0 ;
 lFindClose(DirInfo);
end;
        {-DataCompBoy-}

function FileTime(FileA: String): LongInt;
var
  FA : lfile;
  TA : LongInt;
begin
  lAssignFile(FA, FileA);
  lResetFileReadOnly(FA,1);
  GetFtime(FA.F, TA);
  Close(FA.F);
  FileTime:=TA;
end;

function FileNewer(FileA, FileB : String) : Boolean;
  {-Return true if FileA is newer than FileB, both known to exist}
begin
  FileNewer := FileTime(FileA) > FileTime(FileB);
end;

        {-DataCompBoy-}
function CalcTmpFName;
 var
   I: Integer;
   S: String;
 begin
  I:=0;
  while I<10000 do
   begin
    S:=MakeNormName(SwpDir,Hex8(Id)+'.'+AExt);
    if not ExistFile(S) or not ANew then break;
    Inc(Id); Inc(I);
   end;
  CalcTmpFName:=S;
 end;
        {-DataCompBoy-}

const
   LastTmpId : LongInt = -1;

function  GetTmpId:LongInt;
var
        IdL,lm,ld,lh,lmin      : LongInt;
        y,m,d,dow,h,min,s,hund : Word;
begin
 GetDate(y,m,d,dow);
 GetTime(h,min,s,hund);
 lm:=m and 7;ld:=d;lh:=h;lmin:=min;
 GetTmpId:=       (lm    * 259200000 +
                   ld    * 8640000   +
                   lh    * 360000    +
                   lmin  * 6000      +
                   s     * 100       +
                   hund  + Random(4))+$DEADFACE;
end;

function CalcTmpId:LongInt;
var Id:LongInt;
    s:string;
begin
 Id:=GetTmpId;
 if (LastTmpId<>-1) and (LastTmpId>=Id) then Id:=LastTmpId+1;
 LastTmpId:=Id;
 CalcTmpId:=Id;
end;

function IsDriveCDROM(Drive : Char) : Boolean;
var
  R : Registers;
begin
  FillChar(R, SizeOf(R), 0);
  with R do begin
    AX := $150B;
    CX := Ord(Upcase(Drive))-Ord('A');
    Intr($2F, R);
    IsDriveCDRom := (AX <> 0) and (BX = $ADAD);
  end;
end;

        {-DataCompBoy-}
procedure lChDir;
begin
   if (S[0] > #3) and (S[Length(S)] in ['\','/']) then Dec(S[0]);
   if PosChar(':', S) > 2 then
     begin InOutRes := 666; Exit; end;
   LFN.lChDir(S);
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
procedure EraseByName;
var
  F: lFile;
begin
  lAssignFile(F, FName); lEraseFile(F);
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
procedure EraseFile;
var F: lFile;
begin
  ClrIO;
  FileMode := $42;
  lAssignFile(F, N);
  lEraseFile(F);
  case IOResult of
    5 : begin
          lSetFAttr(F, Archive);
          lEraseFile(F);
        end;
   19 : asm;
          mov  ah,0dh { Reset Disks }
          int  21h
        end;
  end;
  ClrIO;
end;

function ValidDrive(dr : char) : Boolean;
var s,s1 : string[40];
    B,B1: Byte;
    r: registers;
begin
 ValidDrive := Off;
 if dr='*' then begin ValidDrive:=On; exit end;
 if dr<'A' then exit;
 if (dr < 'C') then
   begin
     ValidDrive := (NumFloppy >= Byte(dr)-64);
     Exit;
   end;
 B := GetDrive;
 SetDrive(Byte(Dr)-65);
 B1 := GetDrive;
 SetDrive(B);
 if Byte(dr)-65 <> b1 then Exit;
 s:=dr+':\QQP.OBJ'#0;
 r.ax:=$2900;
 r.ds:=seg(s);
 r.si:=ofs(s[1]);
 r.es:=seg(s1);
 r.di:=ofs(s1);
 intr($21,r);
 if r.ax<>$ff then ValidDrive:=true else ValidDrive:=false;
{ asm
    lea  di,s1
    push ss
    pop  es
    lea  si,s+1
    push ds
    push ss
    pop  ds
    mov  ax,2900h
    int  21h
    pop  ds
    cmp  al,0FFh
    mov  al,1
    jnz  @LocEx
    xor  al,al
@LocEx:
    mov  [bp-1],al
 end;}
end;

FUNCTION GetDrive : byte; assembler; asm mov ah,$19; int 21h; end;
PROCEDURE SetDrive(a : byte); assembler; asm mov ah,$0E; mov dl,a; int $21; end;

procedure GetMask(var m : string);
 var q : string[12];
     i : Byte;
     b : Boolean;
begin
 q:='????????.???';
 i:=Pos('.',m);
 if i>0 then if i>9 then Delete(m,9,i-9) else Insert(Copy('        ',1,9-i),m,i)
   else m:=Copy(m+Strg(' ',8),1,8)+'.   ';
 i:=1;b:=On;
 while m[i]<>'.' do
 begin
  if b then
   begin
    b:=b and (m[i]<>'*');
    if b then q[i]:=m[i];
   end;
   Inc(i);
 end;
 Delete(m,1,i);
 i:=1;b:=On;
 while (i<=3) and (m[0]>=Char(i)) do
 begin
  if b then
   begin
    b:=b and (m[i]<>'*');
    if b then q[i+9]:=m[i];
   end;
   Inc(i);
 end;
 m:=LowStrg(q);
end;

function GetCurDrive; assembler;
asm
 mov ah, 19h
 int 21h
 xor ah, ah
 add al, 65
end;

procedure SetCurDrive; assembler;
asm
 mov ah, 0Eh
 mov dl, C
 sub dl, 65
 int 21h
end;

        {-DataCompBoy-}
FUNCTION GetExt;
var i:byte;
begin
 for i:=length(s) downto 1 do if s[i] in ['.','\','/'] then break;
 if (i>1) and not (s[i] in ['/','\']) then GetExt:=Copy(s, i, 255) else GetExt:='.';
end;
        {-DataCompBoy-}

FUNCTION Norm12;
var R: string[12]; I: Byte; L: Byte absolute S;
begin
  {if S[0]=#12 then begin Norm12:=S; Exit end;}
  System.FillChar(R[1],12,' '); R[0]:=#12;
  if s[1]='.' then begin Norm12:=AddSpace(s,12); Exit end;
  R[9]:='.'; i:=PosChar('.',s);
  if i=0 then i:=succ(l) else move(s[succ(i)],r[10],Min(l-i,3));
  if i>8 then i:=8 else dec(i);
  move(s[1],r[1],i);
  i:=1;
  while i<=12 do
    if r[i]='*'
      then while (i<>9) and (i<=12) do begin
        r[i]:='?';
        inc(i)
      end else inc(i);
  Norm12:=R
end;

        {-DataCompBoy-}
FUNCTION DelSquashes(s: string): string;
 Var i:byte;
 Begin
  i:=1;
  while i<=length(s) do if s[i]='"' then delete(s, i, 1) else inc(i);
  DelSquashes:=s;
 End;
        {-DataCompBoy-}

        {-DataCompBoy-}
FUNCTION GetURZ;
 var a,aa,aaa:string;
 Begin
  lFSplit(S, a, aa, aaa);
  GetURZ:=Copy(aa, 1, 8)+'.'+Copy(aaa, 2, 3);
 End;
        {-DataCompBoy-}

        {-DataCompBoy-}
FUNCTION GetfURZ;
 var a,aa,aaa:string;
 Begin
  lFSplit(S, a, aa, aaa);
  GetfURZ:=a+Copy(aa, 1, 8)+'.'+Copy(aaa, 2, 3);
 End;
        {-DataCompBoy-}

        {-DataCompBoy-}
FUNCTION SquashesName;
 Begin
  if (pos(' ', s)>0) or
     (pos('+', s)>0) or
     (pos(';', s)>0) or
     (pos(',', s)>0) or
     (pos('[', s)>0) or
     (pos(']', s)>0)
      then SquashesName:='"'+s+'"' else SquashesName:=s;
 End;
        {-DataCompBoy-}

        {-DataCompBoy-}
FUNCTION InMask;
 var k:byte;
     j:boolean;
     l:byte;
     ext, maskext:string;
Begin
 if Mask='*.*' then begin InMask:=true; exit; end;

 UpStr(Name); ext:='';
 l:=length(Name); while (l>0) and (not (Name[l] in ['\','/'])) and (Name[l]<>'.') do inc(l);
 if Name[l]='.' then begin ext:=copy(Name, l+1, 255);delete(Name, l, 255); end;

 UpStr(Mask); maskext:='';
 l:=length(mask); while (l>0) and (not (mask[l] in ['\','/'])) and (mask[l]<>'.') do inc(l);
 if mask[l]='.' then begin maskext:=copy(mask, l+1, 255);delete(mask, l, 255); end;

 if ext<>'' then InMask:=FnMatch(mask, Name) and FnMatch(maskext, ext)
            else InMask:=FnMatch(mask, Name);
End;
        {-DataCompBoy-}

        {-DataCompBoy-}
FUNCTION InFilter;
var i: byte; k: byte absolute Filter;
    S: string;
    B: Boolean;
    j:boolean;
begin
  InFilter:=On;
  while k>0 do
   begin
    i:=k+1;
    j:=Off;
    repeat
     dec(i);
     if Filter[i]='"' then j:=not j;
    until (i=1) or ((Filter[i] in [',',';']) and not j);
    if Filter[i] in [',',';'] then S := Copy(Filter,i+1,255)
                              else S := Copy(Filter,i,255);
    B := S[1] <> '-'; k:=i-1;
    InFilter := B;
    if not B then DelFC(S);
    DelLeft(S); DelRight(S);
    if (S <> '') and InMask(Name, DelSquashes(S)) then Exit;
   end;
 InFilter:=Off;
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
FUNCTION InExtFilter;
var i: byte; k: byte absolute Filter;
    S: string;
    B: Boolean;
    j:boolean;
begin
  DelLeft(Filter);
  if Filter[1]=';' then begin InExtFilter:=Off; exit end;
  InExtFilter:=On;
  while k>0 do
   begin
    i:=k+1;
    j:=Off;
    repeat
     dec(i);
     if Filter[i]='"' then j:=not j;
    until (i=1) or ((Filter[i] in [',',';']) and not j);
    if Filter[i] in [',',';'] then S := Copy(Filter,i+1,255)
                              else S := Copy(Filter,i,255);
    B := S[1] <> '-'; k:=i-1;
    InExtFilter := B;
    if not B then DelFC(S);
    DelLeft(S); DelRight(S);
    if (S <> '') and InMask(Name, '*.'+DelSquashes(S)) then Exit;
   end;
 InExtFilter:=Off;
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
FUNCTION InOldMask;
var i:byte;
begin
  i:=13;
  repeat
    dec(i);
    if (Mask[i]<>'?') and (UpCase(Mask[i])<>UpCase(Name[i]))
      and (I <> 9)
      then begin InOldMask:=Off; Exit end
  until i=0; InOldMask:=On
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
FUNCTION InOldFilter;
var i:byte; l:byte absolute Filter;
    S: string[13];
    B: Boolean;
begin
  InOldFilter:=On; if Pos(' ',Filter) > 0 then Filter := DelSpaces(Filter);
  UpStr(Filter); UpStr(Name);
  if Filter='' then Exit; Name:=Norm12(Name);
  repeat if Filter[l]=';' then dec(l);
    if l<>0 then begin
      i:=l; while (i>1)and(Filter[pred(i)]<>';') do dec(i);
      S := Copy(Filter,i,succ(l-i)); B := S[1] = '-';
      InOldFilter := not B;
      if B then DelFC(S);
      DelLeft(S);
      if (S <> '') and InMask(Name, Norm12(S)) then Exit;
      l:=pred(i);
    end
  until l=0; InOldFilter:=Off
end;
        {-DataCompBoy-}

FUNCTION InSpaceMask;
var i:byte;
    j:Boolean;
begin
  i:=13;
  repeat
    dec(i);
    if (Mask[i]='?') or ((Mask[I]=' ') and ValidSpace) or (UpCase(Mask[i])=UpCase(Name[i]))
      or (I = 9) then else begin InSpaceMask:=Off; Exit end
  until i=0; InSpaceMask:=On
end;

FUNCTION InSpaceFilter;
var i:byte; l:byte absolute Filter;
    S: string[13];
    B: Boolean;
begin
  InSpaceFilter:=On; if Pos(' ',Filter) > 0 then Filter := DelSpaces(Filter);
  UpStr(Filter); UpStr(Name);
  if Filter='' then Exit; Name:=Norm12(Name);
  repeat if Filter[l]=';' then dec(l);
    if l<>0 then begin
      i:=l; while (i>1)and(Filter[pred(i)]<>';') do dec(i);
      S := Copy(Filter,i,succ(l-i)); B := S[1] = '-';
      InSpaceFilter := not B;
      if B then DelFC(S);
      DelLeft(S);
      if (S <> '') and InSpaceMask(Name, Norm12(S), On) then Exit;
      l:=pred(i);
    end
  until l=0; InSpaceFilter:=Off
end;

        {-DataCompBoy-}
function IsDir(const S: String): Boolean;
var
  SR: lSearchRec;
begin
  lFindFirst(S, Directory+Hidden, SR);
  if DosError = 0 then
    IsDir := SR.SR.Attr and Directory <> 0
  else IsDir := False;
  lFindClose(SR);
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
function MkName(const Nm, Mask: String): String;
 var aa,aaa: pstring;
     bb,bbb: pstring;
     os: pstring;
     i:byte;
begin
 new(aa); new(aaa);
 new(bb); new(bbb);
 new(os);
 lFSplit(Nm,   os^, aa^, aaa^);
 lFSplit(Mask, os^, bb^, bbb^);
 os^:='';
 for i:=1 to length(bb^) do
  case bb^[i] of
   '?': os^:=os^+aa^[i];
   '*': begin
         os^:=os^+copy(aa^, i, 255)+copy(bb^,i+1,255);
         while pos('?', os^)<>0 do delete(os^, pos('?', os^), 1);
         while pos('*', os^)<>0 do delete(os^, pos('*', os^), 1);
         break;
        end;
   else os^:=os^+bb^[i];
  end;
 for i:=1 to length(bbb^) do
  case bbb^[i] of
   '?': os^:=os^+aaa^[i];
   '*': begin
         os^:=os^+copy(aaa^, i, 255)+copy(bbb^,i+1,255);
         while pos('?', os^)<>0 do delete(os^, pos('?', os^), 1);
         while pos('*', os^)<>0 do delete(os^, pos('*', os^), 1);
         break;
        end;
   else os^:=os^+bbb^[i];
  end;
 MkName:=os^;
 dispose(os);
 dispose(bbb);dispose(bb);
 dispose(aaa);dispose(aa);
end;
        {-DataCompBoy-}

       {-DataCompBoy-}
FUNCTION GetPath;
var B: Byte;
begin
  B := Length(S); while (B > 0) and not (S[B] in ['\','/']) do Dec(B);
  If B=0 then GetPath:=S else GetPath:=Copy(S, 1, B)
end;
       {-DataCompBoy-}

       {-DataCompBoy-}
FUNCTION GetName;
var B: Byte;
begin
 B:=Length(S);
 For B:=length(S) downto 1 do If S[B] in ['\','/']
   then begin GetName:=Copy(S, B+1, 255); Exit end;
 GetName:=S;
end;
       {-DataCompBoy-}

       {-DataCompBoy-}
FUNCTION GetSName;
var B: Byte;
    Pe: byte;
begin
 Pe:=Length(S)+1;
 For B:=length(S) downto 1 do
  begin
   If (S[B] = '.') and (Pe=Length(S)+1) Then Pe:=B;
   If S[B] In ['\','/'] Then Break;
  end;
 GetSName:=copy(S, B, Pe-B);
end;
       {-DataCompBoy-}

       {-DataCompBoy-}
FUNCTION GetIName;
begin
  GetIName:=SquashesName(GetName(S));
end;
       {-DataCompBoy-}

       {-DataCompBoy-}
FUNCTION GetAttrF(const S: String): Word;
var ATF: lFile; w: word;
begin lAssignFile(ATF, s); w:=0; lGetFAttr(ATF, w); GetAttrF:=w end;
       {-DataCompBoy-}

       {-DataCompBoy-}
FUNCTION GetAttrStr(Attr: Word): Str6;
var AttrStr: Str6;
begin
 AttrStr:='RHSVDA';
 if Attr and $01 = 0 then AttrStr[1]:='-';
 if Attr and $02 = 0 then AttrStr[2]:='-';
 if Attr and $04 = 0 then AttrStr[3]:='-';
 if Attr and $08 = 0 then AttrStr[4]:='-';
 if Attr and $10 = 0 then AttrStr[5]:='-';
 if Attr and $20 = 0 then AttrStr[6]:='-';
 GetAttrStr:=AttrStr;
end;

        {-DataCompBoy-}
FUNCTION GetShortRelPath(Path: string): string;
 var CD: String;
 begin
  if Path[length(Path)] in ['\','/'] then dec(Path[0]);
  Path:=lfGetShortFileName(Path);
  lGetDir(0,CD);
  if CD[length(CD)] in ['\','/'] then dec(CD[0]);
  CD:=lfGetShortFileName(CD);
  if copy(Path, 1, length(CD))=CD then delete(Path, 1, length(CD));
  if Path[1] in ['\','/'] then DelFC(Path);
  GetShortRelPath:=Path;
 end;
        {-DataCompBoy-}

        {-DataCompBoy-}
FUNCTION GetLongRelPath(Path: string): string;
 var CD: String;
 begin
  if Path[length(Path)] in ['\','/'] then dec(Path[0]);
  Path:=lfGetLongFileName(Path);
  lGetDir(0,CD);
  if CD[length(CD)] in ['\','/'] then dec(CD[0]);
  if copy(Path, 1, length(CD))=CD then delete(Path, 1, length(CD));
  if Path[1] in ['\','/'] then DelFC(Path);
  GetLongRelPath:=Path;
 end;
        {-DataCompBoy-}

Function MakeFileName(S: string): string;
 var I: Integer;
     L: Byte absolute S;
begin
 S[9] := '.';
 I := 8;
 While (I > 0) and (S[I] = ' ') do begin Delete(S, I, 1); Dec(I); end;
 While S[L] = ' ' do Dec(L);
 if S[Length(S)] = '.' then Dec(S[0]);
 MakeFileName := S;
end;

        {-DataCompBoy-}
FUNCTION MakeNormName(const S, S1: string): string;
var i, j: byte;
begin
 i:=Length(S ); While S [i] = ' ' do Dec(i);
 j:=Length(S1); While S1[j] = ' ' do Dec(j);
 if i > 0 then
 begin
   if (S[i] in ['\','/']) then MakeNormName := copy(S,1,i) + copy(S1,1,j)
                          else MakeNormName := copy(S,1,i) + '\' + copy(S1,1,j);
 end else MakeNormName := S1;
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
function FileNameOf(var F: File): string;
var
  S: string;
begin
  S := FileRec(F).Name;
  S[0] := Char(Pos(#0, S)-1);
  FileNameOf := S;
end;
        {-DataCompBoy-}

end.