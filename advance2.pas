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

unit advance2; {File related functions}

interface

uses
  Defines, Streams, advance, Dos, Lfn, UFNMatch
  ;

var
  ErrorFCode: Byte;

function ExistFile(const FName: String): Boolean; {DataCompBoy}
//function  ExistDir(const DName: string): Boolean; { VK }
function FileTime(FileA: String): LongInt;
function FileNewer(FileA, FileB: String): Boolean;
function CalcTmpFName(Id: LongInt; const AExt: String; ANew: Boolean)
  : String; {DataCompBoy}
function CalcTmpId: LongInt;
function IsDriveCDROM(Drive: Char): Boolean;
procedure lChDir(S: String); {DataCompBoy}
procedure EraseByName(const FName: String); {DataCompBoy}
procedure EraseFile(const N: String); {DataCompBoy}
procedure EraseTempFile(S: String); {piwamoto}
{JO}
function ValidDrive(dr: Char): Boolean;
function GetDrive: Byte;
procedure SetDrive(a: Byte);
procedure GetMask(var m: String);
function GetCurDrive: Char;
procedure SetCurDrive(C: Char);

function GetExt(const s: String): String;
function Norm12(const s: String): Str12;
{-DataCompBoy-}
function DelSquashes(s: String): String; {removes quotes}
{$IFNDEF OS2}
function GetURZ(const s: String): Str12; {cuts name to 8.3}
{$ENDIF}
function GetfURZ(const s: String): String; {cuts name and path to 8.3}
function IsSeparatingChars(const s: String): Boolean;
{JO: проверяет на наличие }
{ пробелов и символов '+' ';' ',' '[' ']' '&' '^'}
function SquashesName(const s: String): String; {quotes name if needed}
function InMask(Name, Mask: String): Boolean; {does Name match Mask? }
function InFilter(Name, Filter: String): Boolean;
{does Name match Filter? }
function InDirFilter(Name, Filter: String): Boolean; {JO}
{FUNCTION  InExtMask(Name, Mask: string):Boolean;}
{does Ext match Mask? }

type
  TMaskPos = packed record
    CurPos: Byte;
    InSq: Boolean;
    end;
  PMasksPos = ^TMasksPos;
  TMasksPos = packed array[0..255] of TMaskPos;
  TMasksData = packed record
    Num: Byte;
    case Boolean of
      True: (P: Pointer; );
      False: (A: array[1..2] of TMaskPos; );
    end;
  TMasksDataArr = packed array[32..255] of TMasksData;
  PMaskData = ^TMaskData;
  TMaskData = packed record
    Filter: String;
    MP: TMasksDataArr;
    end;

procedure FreeTMaskData(var MD: TMaskData); {Cat}
procedure MakeTMaskData(var MD: TMaskData); {Fills TMaskData members}
{for work of InExtFilter}
function InExtFilter(Name: String; const F: TMaskData): Boolean;
{does extension of  }
{Name match Filter? }
(*
FUNCTION  InOldMask(Name,Mask: string):Boolean;     {DataCompBoy}
FUNCTION  InOldFilter(Name,Filter: string):Boolean; {DataCompBoy}
*)
function InSpaceMask(Name, Mask: String; ValidSpace: Boolean): Boolean;
function InSpaceFilter(Name, Filter: String): Boolean;

function IsMixedCase(const Name: String): Boolean; {JO}

function IsDir(const s: String): Boolean; {is this a directory? }
function MkName(const Nm, Mask: String): String;
{modifies name to fit Mask}
function GetPath(const S: String): String;
function GetName(const S: String): String;
function GetSName(const S: String): String;
function GetIName(const S: String): String;
function GetAttrStr(Attr: word): Str6;
{$IFNDEF OS2}
function GetShortRelPath(Path: String): String;
{$ENDIF}
function GetLongRelPath(Path: String): String;

{-DataCompBoy-}
function MakeFileName(S: String): String;
function MakeNormName(const S, S1: String): String; {DataCompBoy}
function GetFileAttr(const S: String): word;
function SetFileAttr(const S: String; Attr: word): word;
function CorrectFile(const N: String): Boolean;
function PathExist(s: String): Boolean; {Is path exist}

function PathFoundInArc(S: String): Boolean; {JO}

{-DataCompBoy-}
procedure GetFTimeSizeAttr(const A: String; var ATime: LongInt;
    var ASize: TSize; var AAttr: word);
{-DataCompBoy-}

{-DataCompBoy-}
type
  TQuickSearchData = record
    Mask: String;
    NumExt: word;
    ExtD: word;
    end;

function InQSMask(Name, Mask: String): Boolean; {JO}

procedure InitQuickSearch(var QS: TQuickSearchData);
procedure DoQuickSearch(var QS: TQuickSearchData; Key: word);
function GetCursorPos(const QS: TQuickSearchData; const Name: String;
    Size, ExtSize: word): word;
procedure FileChanged(const Name: String);
{-DataCompBoy-}

{$IFDEF OS_DOS}
type
  PTempFile = ^TTempFile;
  TTempFile = object(TBufStream)
    constructor Init(const AExt: String; ABufSize: SW_Word);
    destructor Done; virtual;
    end;
  {$ENDIF}

function PackMask(const Mask: String; var PM: String {$IFDEF OS_DOS};
     LFNDis: Boolean {$ENDIF}): Boolean; {DataCompBoy}

function CompareFiles(const N1, N2: String): Boolean;

implementation
uses
  Drivers,
  advance1, advance3, Strings,
  Commands, DNApp, DnIni, Memory
  , VpSysLow, VPUtils
  ;

{$IFDEF OS_DOS}
constructor TTempFile.Init(const AExt: String; ABufSize: SW_Word);
  var
    S: FNameStr;
    L: LongInt;
  begin
  L := CalcTmpId;
  S := CalcTmpFName(L, AExt, True);
  inherited Init(S, (stCreate and fmDeny) or fmDenyAll or fmDenyChild,
     ABufSize);
  end;

destructor TTempFile.Done;
  var
    S: FNameStr;
  begin
  S := StrPas(FName);
  inherited Done;
  EraseFile(S);
  end;
{$ENDIF}

function CorrectFile(const N: String): Boolean;
  var
    I: integer;
  begin
  CorrectFile := False;
  for I := 1 to Length(N) do
    if N[I] in IllegalCharSet then
      exit; {DataCompBoy}
  CorrectFile := True;
  end;

{-DataCompBoy-}
function ExistFile(const FName: String): Boolean;
  var
    DirInfo: lSearchRec;
  begin
  lFindFirst(FName, Archive+ReadOnly+Hidden+SysFile, DirInfo);
  lFindClose(DirInfo);
  if DosError = 0 then
    ExistFile := True
  else
    begin
    ExistFile := False;
    ErrorFCode := DosError;
    end;
  end;
{-DataCompBoy-}
{AK155 21-01-2002 Эта программа дублирует PathExist, все ее вызовы
 (в dn1 и startup) заменил на вызовы PathExist}
(*
{ VK/ }
function  ExistDir(const DName: string): Boolean; {based on ExistFile}
var
 Dirinfo:lsearchrec;
begin
  If (DName<>'') and (DName[Length(DName)]='\')
  then lFindFirst(Copy(DName, 1, Length(DName)-1),Directory,DirInfo)
  else lFindFirst(DName,Directory,DirInfo);
 lFindClose(DirInfo);
 if DosError=0 then existdir:=True
 else begin
  Existdir:=False;
  ErrorFCode:=DosError;
 end;
end;
{ /VK }
*) {/AK155}
function FileTime(FileA: String): LongInt;
  var
    FA: lFile;
    TA: LongInt;
  begin
  lAssignFile(FA, FileA);
  lResetFileReadOnly(FA, 1);
  GetFtime(FA.F, TA);
  Close(FA.F);
  FileTime := TA;
  end;

function FileNewer(FileA, FileB: String): Boolean;
  {-Return true if FileA is newer than FileB, both known to exist}
  begin
  FileNewer := FileTime(FileA) > FileTime(FileB);
  end;

{-DataCompBoy-}
function CalcTmpFName;
  var
    I: integer;
    S: String;
  begin
  I := 0;
  while I < 10000 do
    begin
    S := MakeNormName(SwpDir, Hex8(Id)+'.'+AExt);
    if not ExistFile(S) or not ANew then
      break;
    Inc(Id);
    Inc(I);
    end;
  CalcTmpFName := S;
  end;
{-DataCompBoy-}

const
  LastTmpId: LongInt = -1;

function GetTmpId: LongInt;
  var
    IdL, lm, ld, lh, lmin: LongInt;
    y, m, d, dow, h, min, s, hund: word;
  begin
  GetDate(y, m, d, dow);
  GetTime(h, min, s, hund);
  lm := m and 7;
  ld := d;
  lh := h;
  lmin := min;
  GetTmpId := (lm*259200000+
      ld*8640000+
      lh*360000+
      lmin*6000+
      s*100+
      hund+Random(4))+$DEADFACE;
  end;

function CalcTmpId: LongInt;
  var
    Id: LongInt;
    s: String;
  begin
  Id := GetTmpId;
  if  (LastTmpId <> -1) and (LastTmpId >= Id) then
    Id := LastTmpId+1;
  LastTmpId := Id;
  CalcTmpId := Id;
  end;

function IsDriveCDROM(Drive: Char): Boolean;
  begin
  IsDriveCDROM := SysGetDriveType(Drive) = dtCDRom;
  end;

{-DataCompBoy-}
procedure lChDir;
  begin
  if  (Length(S) > 3) and (S[Length(S)] in ['\', '/']) then
    SetLength(S, Length(S)-1);
  if PosChar(':', S) > 2 then
    begin
    InOutRes := 666;
    exit;
    end;
  Lfn.lChDir(S);
  end;
{-DataCompBoy-}

{-DataCompBoy-}
procedure EraseByName;
  var
    F: lFile;
  begin
  lAssignFile(F, FName);
  lEraseFile(F);
  end;
{-DataCompBoy-}

{-DataCompBoy-}
procedure EraseFile;
  var
    F: lFile;
    rc: LongInt;
  label TryDel;
  begin
  ClrIO;
  FileMode := $42;
  lAssignFile(F, N);
TryDel:
  lEraseFile(F);
  rc := IOResult;
  case rc of
    0, 2:
      begin
      end;
    5:
      begin
      lSetFAttr(F, Archive);
      lEraseFile(F);
      end;
    else {case}
      if SysErrorFunc(rc, Byte(N[1])-Byte('A')) = 1 then
        goto TryDel;
  end {case};
  ClrIO;
  end { EraseFile };

procedure EraseTempFile;
  var
    Dir: String;
    F: lFile;
  begin
  {JO: проверочка файла на нахождение во временном каталоге не помешает}
  Dir := GetPath(S);
  if  (TempDir[Length(TempDir)] <> '\') and (Dir[Length(Dir)] = '\')
  then
    SetLength(Dir, Length(Dir)-1);
  if UpStrg(Dir) = UpStrg(TempDir) then
    begin
    ClrIO;
    FileMode := $42;
    lAssignFile(F, S);
    lEraseFile(F);
    if IOResult = 5 then
      begin
      lSetFAttr(F, Archive);
      lEraseFile(F);
      end;
    ClrIO;
    end;
  end { EraseTempFile };

function ValidDrive(dr: Char): Boolean;
  var
    DriveNum, LogDrvMap: LongInt;
  begin { LogDrvMap: Bit 0: 'A:', Bit 1: 'B:', etc }
  LogDrvMap := SysGetValidDrives;
  ValidDrive := ((1 shl (Ord(dr)-Ord('A'))) and LogDrvMap) <> 0;
  end;

function GetDrive: Byte;
  var
    S: String;
  begin
  GetDir(0, S);
  GetDrive := Byte(S[1])-Byte('A');
  end;

procedure SetDrive(a: Byte);
  {var S: String;}
  begin
  {Cat: здесь ничего, кроме глюков, не делается}
  (*
  GetDir(0, S);
  ChDir(S);
*)
  end;

procedure GetMask(var m: String);
  var
    q: String[12];
    i: Byte;
    b: Boolean;
  begin
  {Cat:warn}
  q := '????????.???';
  i := Pos('.', m);
  if i > 0 then
    if i > 9 then
      Delete(m, 9, i-9)
    else
      Insert(Copy('        ', 1, 9-i), m, i)
  else
    m := Copy(m+Strg(' ', 8), 1, 8)+'.   ';
  i := 1;
  b := True;
  while m[i] <> '.' do
    begin
    if b then
      begin
      b := b and (m[i] <> '*');
      if b then
        q[i] := m[i];
      end;
    Inc(i);
    end;
  Delete(m, 1, i);
  i := 1;
  b := True;
  while (i <= 3) and (Length(m) >= i) do
    begin
    if b then
      begin
      b := b and (m[i] <> '*');
      if b then
        q[i+9] := m[i];
      end;
    Inc(i);
    end;
  m := LowStrg(q);
  end { GetMask };

function GetCurDrive;
  begin
  GetCurDrive := Char(GetDrive+65)
  end;

procedure SetCurDrive;
  begin
  SetDrive(Byte(C)-65)
  end;

{-DataCompBoy-}
function GetExt;
  var
    i: Byte;
  begin
  for i := Length(s) DownTo 1 do
    if s[i] in ['.', '\', '/'] then
      break;
  if  ( (i > 1) or (s[1] = '.')) and not (s[i] in ['/', '\']) then
    GetExt := Copy(s, i, MaxStringLength)
  else
    GetExt := '.';
  end;
{-DataCompBoy-}

function Norm12;
  var
    R: String[12];
    I: Byte;
  begin
  if s[1] = '.' then
    begin
    Norm12 := AddSpace(s, 12);
    exit
    end;
  System.FillChar(R[1], 12, ' ');
  R[0] := #12;
  R[9] := '.';
  I := PosChar('.', s);
  if I = 0 then
    I := Succ(Length(s))
  else
    Move(s[Succ(I)], R[10], Min(Length(s)-I, 3));
  if I > 8 then
    I := 8
  else
    Dec(I);
  Move(s[1], R[1], I);
  I := 1;
  while I <= 12 do
    if R[I] = '*'
    then
      while (I <> 9) and (I <= 12) do
        begin
        R[I] := '?';
        Inc(I)
        end
    else
      Inc(I);
  Norm12 := R
  end { Norm12 };

{-DataCompBoy-}
function DelSquashes(s: String): String;
  var
    i: Byte;
  begin
  i := 1;
  while i <= Length(s) do
    if s[i] = '"' then
      Delete(s, i, 1)
    else
      Inc(i);
  DelSquashes := s;
  end;
{-DataCompBoy-}

{-DataCompBoy-}
{$IFNDEF OS2}
function GetURZ;
  var
    a, aa, aaa: String;
  begin
  lFSplit(s, a, aa, aaa);
  GetURZ := Copy(aa, 1, 8)+'.'+Copy(aaa, 2, 3);
  end;

{-DataCompBoy-}
{$ENDIF}
{-DataCompBoy-}
function GetfURZ;
  var
    a, aa, aaa: String;
  begin
  lFSplit(s, a, aa, aaa);
  GetfURZ := a+Copy(aa, 1, 8)+'.'+Copy(aaa, 2, 3);
  end;
{-DataCompBoy-}
{JO}
function IsSeparatingChars(const s: String): Boolean;
  var
    i: Byte;
  begin
  IsSeparatingChars := False;
  for i := 1 to Length(s) do
    if s[i] in [' ', '+', ';', ',', '[', ']', '&', '^'] then
      IsSeparatingChars := True;
  end;
{/JO}
{-DataCompBoy-}
function SquashesName;
  begin
  {JO}
  if IsSeparatingChars(s) then
    SquashesName := '"'+s+'"'
  else
    SquashesName := s;
  {/JO}
  end;
{-DataCompBoy-}

{-DataCompBoy-}
function InMask;
  var
    k: Byte;
    j: Boolean;
    l: Byte;
    ext, maskext: String;
  begin
  if  (Mask = x_x) or (Mask = '*') then
    begin
    InMask := True;
    exit;
    end;

  j := Mask[Length(Mask)] = '.';
  UpStr(Mask);
  maskext := '';
  l := Length(Mask);
  while (l > 0) and (not (Mask[l] in ['\', '/'])) and (Mask[l] <> '.')
  do
    Dec(l);
  if Mask[l] = '.' then
    begin
    maskext := Copy(Mask, l+1, MaxStringLength);
    Delete(Mask, l, MaxStringLength);
    end;

  ext := '';
  UpStr(Name);
  if  (maskext <> '') or j then
    begin
    l := Length(Name);
    while (l > 0) and (not (Name[l] in ['\', '/'])) and (Name[l] <> '.')
    do
      Dec(l);
    if Name[l] = '.' then
      begin
      ext := Copy(Name, l+1, MaxStringLength);
      Delete(Name, l, MaxStringLength);
      end;
    if  (ext = '') and not j then
      Mask := Mask+'.'+maskext;
    end;
  if ext <> '' then
    InMask := FnMatch(Mask, Name) and FnMatch(maskext, ext)
  else
    InMask := FnMatch(Mask, Name);
  end { InMask };
{-DataCompBoy-}

{-DataCompBoy-}
function InFilter;
  var
    i: Byte;
    S: String;
    B: Boolean;
    j: Boolean;
  begin
  InFilter := True;
  while Length(Filter) > 0 do
    begin
    i := Length(Filter)+1;
    j := False;
    repeat
      Dec(i);
      if Filter[i] = '"' then
        j := not j;
    until (i = 1) or ((Filter[i] in [';', ',']) and not j);
    if Filter[i] in [';', ','] then
      S := Copy(Filter, i+1, MaxStringLength)
    else
      S := Filter;
    B := S[1] <> '-';
    SetLength(Filter, i-1);
    InFilter := B;
    if not B then
      Delete(S, 1, 1); {DelFC(S);}
    DelLeft(S);
    DelRight(S);
    if  (S <> '') and InMask(Name, S) then
      exit;
    end;
  InFilter := False;
  end { InFilter };
{-DataCompBoy-}
{JO}
function InDirFilter;
  var
    i: Byte;
    S: String;
    B: Boolean;
    j: Boolean;
  begin
  InDirFilter := True;
  while Length(Filter) > 0 do
    begin
    i := Length(Filter)+1;
    j := False;
    repeat
      Dec(i);
      if Filter[i] = '"' then
        j := not j;
    until (i = 1) or ((Filter[i] in [';', ',']) and not j);
    if Filter[i] in [';', ','] then
      S := Copy(Filter, i+1, MaxStringLength)
    else
      S := Filter;
    B := S[1] <> '-';
    SetLength(Filter, i-1);
    InDirFilter := B;
    if not B then
      Delete(S, 1, 1); {DelFC(S);}
    DelLeft(S);
    DelRight(S);
    if  (S <> '') and (S[Length(S)] = '\')
           and InMask(Name, Copy(S, 1, Length(S)-1))
    then
      exit;
    end;
  InDirFilter := False;
  end { InDirFilter };
{/JO}
(*
        {-DataCompBoy-}
FUNCTION  InExtMask(Name, Mask: string):Boolean;
var mp, np, i, e: byte;
    sq,sq2: boolean;
    nt: boolean;
label R;
begin
 if Boolean(PosChar('*',Mask))
 then InExtMask := InMask(Name, '*'+Mask)
 else begin
  InExtMask:=false;
  sq:=false;
  mp:=length(Mask); np:=length(Name);
  while (np>0) and (mp>0) do begin
   if Mask[mp]='"' then sq:=not sq
   else if not sq and (Mask[mp]=']') then begin
    sq2:=sq;
    for i:=mp downto 1 do
     if Mask[i]='"' then sq2:=not sq2 else
     if not sq2 and (Mask[i]='[') then break;
    if Mask[i]<>'[' then break;
    e:=mp-1;
    mp:=i;
    inc(i);
    nt := Mask[i]='^';
    if nt then inc(i);
    for i:=i to e do
     if Mask[i+1]<>'-' then
      if nt xor (UpCaseArray[Mask[i]]=UpCaseArray[Name[np]])
       then exit
       else begin dec(np); goto R end
     else begin
      if nt xor ((UpCaseArray[Name[np]] >= UpCaseArray[Mask[i]]) and
                 (UpCaseArray[Name[np]] <= UpCaseArray[Mask[i+2]]))
       then begin dec(np); goto R end
       else exit;
      inc(i, 2);
     end;
   end
   else if (Mask[mp]<>'?') and (UpCaseArray[Mask[mp]]<>UpCaseArray[Name[np]]) then exit
   else dec(np);
R: dec(mp);
  end;
  InExtMask:=true;
 end
end;
        {-DataCompBoy-}
*)

{Cat}
procedure FreeTMaskData(var MD: TMaskData);
  var
    I: Byte;
  begin
  with MD do
    begin
    for I := 32 to 255 do
      with MP[I] do
        if Num > 2 then
          FreeMem(P, Num*SizeOf(TMaskPos));
    FillChar(MP, SizeOf(MP), #0);
    end;
  end;
{/Cat}

procedure MakeTMaskData(var MD: TMaskData);
  var
    i, j, l, e, k, o, NumMP: Byte;
    Dina1: array[0..255] of record
      CurPos: Byte;
      Meta: Boolean;
      end;
    InSq: Boolean;
    InS: Boolean;
    Pss: array[1..255] of TMaskPos;
  label Ok, Ne;
  begin
  with MD do
    begin
    FreeTMaskData(MD);

    if Filter = '' then
      exit;
    if Filter[1] in [' ', #9] then
      DelLeft(Filter);
    if not (Filter[1] in [';', ',']) then
      Filter := ';'+Filter;
    (*
    begin
      SetLength(Filter, Length(Filter)+1);
      move(Filter[1], Filter[2], Length(Filter));
      Filter[1] := ';';
    end;
    *)
    if not (Filter[Length(Filter)] in [';', ',']) then
      Filter := Filter+';';
    (*
    begin
      SetLength(Filter, Length(Filter)+1);
      Filter[Length(Filter)]:=';';
    end;
    *)

    InSq := False;
    Dina1[0].CurPos := 0;
    NumMP := 0;

    for i := Length(Filter) DownTo 2 do
      if  (Filter[i] in [';', ',']) and not InSq then
        if not (Filter[i-1] in [';', ',']) then
          begin
          Inc(NumMP);
          Dina1[NumMP].CurPos := i-1;
          Dina1[NumMP].Meta := False;
          end
        else
      else if (Filter[i] = '"') then
        InSq := not InSq
      else if (Filter[i] = '*') then
        Dina1[NumMP].Meta := True;

    for j := 32 to 255 do
      if UpCaseArray[Char(j)] = Char(j) then
        begin
        l := 0;
        if j <> Byte('*') then
          begin
          for i := 1 to NumMP do
            if not Dina1[i].Meta then
              begin
              e := Dina1[i].CurPos;
              if Filter[e] = '"'
              then
                begin
                InSq := True;
                Dec(e);
                end
              else
                InSq := False;

              if  (Filter[e] = ']') and not InSq then
                begin
                Dec(e);
                InS := False;
                for k := e DownTo 1 do
                  if  (Filter[k] = '[') and not InS
                  then
                    break
                  else if Filter[k] = '"' then
                    InS := not InS;
                o := k-1;
                Inc(k);
                if Filter[k] = '^' then
                  begin
                  InS := True;
                  Inc(k)
                  end
                else
                  InS := False;
                for k := k to e do
                  if Filter[k+1] = '-' then
                    if InS xor ((UpCaseArray[Char(j)] >=
                           UpCaseArray[Filter[k]]) and
                          (UpCaseArray[Char(j)] <=
                           UpCaseArray[Filter[k+2]]))
                    then
                      goto Ok
                    else
                      Inc(k, 2)
                  else if InS xor (UpCaseArray[Char(j)] =
                       UpCaseArray[Filter[k]])
                  then
                    goto Ok;
                goto Ne;
                end;

              if  (UpCaseArray[Filter[e]] = UpCaseArray[Char(j)]) or
                  (Filter[e] = '.') or (Filter[e] = '?')
              then
                begin
                if Filter[e] <> '.' then
                  Dec(e);
                o := e;
Ok:
                Inc(l);
                Pss[l].CurPos := o;
                Pss[l].InSq := InSq;
                end;
Ne:
              end;
          end
        else
          for i := 1 to NumMP do
            if Dina1[i].Meta then
              begin
              Inc(l);
              Pss[l].CurPos := Dina1[i].CurPos;
              end;
        if l <= 2 then
          begin
          Move(Pss[1], MP[j].A[1], l*SizeOf(TMaskPos));
          MP[j].Num := l;
          end
        else
          begin
          GetMem(MP[j].P, l*SizeOf(TMaskPos));
          Move(Pss[1], MP[j].P^, l*SizeOf(TMaskPos));
          MP[j].Num := l;
          end;
        end;
    end
  end { MakeTMaskData };

{-DataCompBoy-}
function InExtFilter;
  var
    i, j, l, e: Byte;
    Dina1, Dina2: TMasksPos;
    Num1, Num2: Byte;
    InSq: Boolean;
  label Try2, q;
  begin
  if Byte(Name[Length(Name)]) < 32 then
    begin
    InExtFilter := False;
    exit
    end;
  InExtFilter := True; {JO}
  Num1 := F.MP[Byte(UpCaseArray[Name[Length(Name)]])].Num;
  if Num1 = 0 then
    goto q;
  Dina1[0].CurPos := 1;
  if Num1 <= 2 then
    Move(F.MP[Byte(UpCaseArray[Name[Length(Name)]])].A,
      Dina1[1],
      Num1*SizeOf(TMaskPos))
  else
    Move(PMasksPos(F.MP[Byte(UpCaseArray[Name[Length(Name)]])].P)^,
      Dina1[1],
      Num1*SizeOf(TMaskPos));
  {InExtFilter := True;} {JO}
  j := Length(Name)-1;
  while Num1 > 0 do
    begin
    Num2 := 0;
    for i := 1 to Num1 do
Try2:
      begin
      if F.Filter[Dina1[i].CurPos] = '"' then
        begin
        Dina1[i].InSq := not Dina1[i].InSq;
        Dec(Dina1[i].CurPos);
        goto Try2
        end
      else if (F.Filter[Dina1[i].CurPos] in [';', ',']) and
        not Dina1[i].InSq
      then
        begin
        if Name[j] = '.' then
          exit;
        end
      else if (F.Filter[Dina1[i].CurPos] = '-') and
          (F.Filter[Dina1[i].CurPos-1] in [';', ',']) and
        not Dina1[i].InSq
      then
        begin
        if Name[j] = '.' then
          begin
          InExtFilter := False;
          exit;
          end
        end
      else if (F.Filter[Dina1[i].CurPos] = '.') and
          (F.Filter[Dina1[i].CurPos-1] in [';', ',']) and
        not Dina1[i].InSq
      then
        begin
        if  (PosChar('.', Name) = 0) then
          exit;
        end
      else if (F.Filter[Dina1[i].CurPos] = ']') and
        not Dina1[i].InSq
      then
        begin
        InSq := Dina1[i].InSq;
        for l := Dina1[i].CurPos-1 DownTo 1 do
          if F.Filter[l] = '"' then
            InSq := not InSq
          else if not InSq and (F.Filter[l] = '[') then
            break
          else if F.Filter[l] = ';' then
            break;
        if F.Filter[l] <> '[' then
          continue;
        e := Dina1[i].CurPos-1;
        Inc(Num2);
        Dina2[Num2].CurPos := l-1;
        Dina2[Num2].InSq := Dina1[i].InSq;
        Inc(l);
        InSq := F.Filter[l] = '^';
        if InSq then
          Inc(l);
        for l := l to e do
          if F.Filter[l+1] <> '-' then
            if Dina2[Num2].InSq xor (UpCaseArray[F.Filter[l]] =
                 UpCaseArray[Name[j]])
            then
              begin
              Dec(Num2);
              break
              end
            else
              break
          else
            begin
            if Dina2[Num2].InSq xor
                ( (UpCaseArray[Name[j]] >= UpCaseArray[F.Filter[l]]) and
                  (UpCaseArray[Name[j]] <= UpCaseArray[F.Filter[l+2]])
                )
            then
              break
            else
              begin
              Dec(Num2);
              break
              end;
            Inc(l, 2);
            end;
        end
      else if (F.Filter[Dina1[i].CurPos] = '?') or
          (UpCaseArray[F.Filter[Dina1[i].CurPos]] =
           UpCaseArray[Name[j]])
      then
        begin
        Inc(Num2);
        Dina2[Num2].CurPos := Dina1[i].CurPos-1;
        Dina2[Num2].InSq := Dina1[i].InSq;
        end;
      end;
    Num1 := Num2;
    Move(Dina2[1], Dina1[1], Num1*SizeOf(TMaskPos));
    Dec(j);
    end;
q:
  if F.MP[Byte('*')].Num <> 0
  then
    if F.MP[Byte('*')].Num <= 2 then
      for i := 1 to F.MP[Byte('*')].Num do
        begin
        for j := F.MP[Byte('*')].A[i].CurPos DownTo 1 do
          if F.Filter[j] = ';' then
            break;
        if F.Filter[j] = ';' then
          Inc(j);
        if InMask(Name, '*.'+Copy(F.Filter, j,
              F.MP[Byte('*')].A[i].CurPos-j+1))
        then
          exit;
        end
    else
      for i := 1 to F.MP[Byte('*')].Num do
        begin
        for j := PMasksPos(F.MP[Byte('*')].P)^[i].CurPos DownTo 1 do
          if F.Filter[j] = ';' then
            break;
        if F.Filter[j] = ';' then
          Inc(j);
        if InMask(Name, '*.'+Copy(F.Filter, j,
              PMasksPos(F.MP[Byte('*')].P)^[i].CurPos-j))
        then
          exit;
        end;
  InExtFilter := False
  end { InExtFilter };
{-DataCompBoy-}
(*
        {-DataCompBoy-}
FUNCTION InOldMask;
var i:byte;
begin
  i:=13;
  repeat
    dec(i);
    if (Mask[i]<>'?') and (UpCase(Mask[i])<>UpCase(Name[i]))
      and (I <> 9)
      then begin InOldMask:=false; Exit end
  until i=0; InOldMask:=true
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
FUNCTION InOldFilter;
var i:byte;
    S: string[13];
    B: Boolean;
begin
  InOldFilter:=true; if Pos(' ',Filter) > 0 then Filter := DelSpaces(Filter);
  UpStr(Filter); UpStr(Name);
  if Filter='' then Exit;
{$IFNDEF OS2}
  Name:=Norm12(Name);
{$ENDIF}
  repeat if Filter[Length(Filter)]=';' then SetLength(Filter, Length(Filter)-1);
    if Length(Filter) <> 0 then begin
      i := Length(Filter); while (i>1)and(Filter[pred(i)]<>';') do dec(i);
      S := Copy(Filter, i, succ(Length(Filter) - i)); B := S[1] = '-';
      InOldFilter := not B;
      if B then DelFC(S);
      DelLeft(S);
{$IFNDEF OS2}
      if (S <> '') and InMask(Name, Norm12(S)) then Exit;
{$ELSE}
      if (S <> '') and InMask(Name, S) then Exit;
{$ENDIF}
      SetLength(Filter, pred(i));
    end
  until Length(Filter) = 0; InOldFilter:=false
end;
        {-DataCompBoy-}
*)
function InSpaceMask;
  var
    i: Byte;
    j: Boolean;
  begin
  i := 13;
  repeat
    Dec(i);
    if  (Mask[i] = '?') or ((Mask[i] = ' ') and ValidSpace)
           or (UpCase(Mask[i]) = UpCase(Name[i]))
      or (i = 9)
    then
    else
      begin
      InSpaceMask := False;
      exit
      end
  until i = 0;
  InSpaceMask := True
  end;

function InSpaceFilter;
  var
    i: Byte;
    S: String[13];
    B: Boolean;
  begin
  InSpaceFilter := True;
  if Pos(' ', Filter) > 0 then
    Filter := DelSpaces(Filter);
  UpStr(Filter);
  UpStr(Name);
  if Filter = '' then
    exit;
  Name := Norm12(Name);
  repeat
    if Filter[Length(Filter)] = ';' then
      SetLength(Filter, Length(Filter)-1);
    if Length(Filter) <> 0 then
      begin
      i := Length(Filter);
      while (i > 1) and (Filter[pred(i)] <> ';') do
        Dec(i);
      S := Copy(Filter, i, Succ(Length(Filter)-i));
      B := S[1] = '-';
      InSpaceFilter := not B;
      if B then
        Delete(S, 1, 1); {DelFC(S);}
      DelLeft(S);
      if  (S <> '') and InSpaceMask(Name, Norm12(S), True) then
        exit;
      SetLength(Filter, pred(i));
      end
  until Length(Filter) = 0;
  InSpaceFilter := False
  end { InSpaceFilter };

{JO} {piwamoto}
function IsMixedCase(const Name: String): Boolean;
  var
    MixedDir, MixedName, MixedExt: String;
  begin
  lFSplit(Name, MixedDir, MixedName, MixedExt);
  if  ( (UpStrg(MixedName) = MixedName) or (LowStrg(MixedName) =
         MixedName)) and
      ( (UpStrg(MixedExt) = MixedExt) or (LowStrg(MixedExt) = MixedExt))
  then
    IsMixedCase := False
  else
    IsMixedCase := True;
  end;
{JO} {piwamoto}

{-DataCompBoy-}
function IsDir(const s: String): Boolean;
  var
    SR: lSearchRec;
  begin
  lFindFirst(s, Directory shl 8 or AnyFile, SR);
  if DosError = 0 then
    IsDir := SR.SR.Attr and Directory <> 0
  else
    IsDir := False;
  lFindClose(SR);
  end;
{-DataCompBoy-}

{-DataCompBoy-}
function MkName(const Nm, Mask: String): String;
  var
    aa, aaa: String;
    bb, bbb: String;
    os: String;
    i: Byte;
    fp: Byte;
  begin
  lFSplit(Nm, os, aa, aaa);
  lFSplit(Mask, os, bb, bbb);
  os := '';
  fp := 0;
  for i := 1 to Length(bb) do
    begin
    Inc(fp);
    case bb[i] of
      '?':
        if fp <= Length(aa) then
          os := os+aa[fp];
      '*':
        begin
        os := os+Copy(aa, fp, MaxStringLength)
            +Copy(bb, i+1, MaxStringLength);
        if Pos('*', os) <> 0 then
          {Pavel Anufrikov -> }
          begin
          Insert(Copy(aaa, 2, MaxStringLength), os, Pos('*', os));
          aaa := '';
          end; { <- Pavel Anufrikov}
        while Pos('?', os) <> 0 do
          Delete(os, Pos('?', os), 1);
        while Pos('*', os) <> 0 do
          Delete(os, Pos('*', os), 1);
        break;
        end;
      '>':
        if fp > 2 then
          Dec(fp, 2);
      '<':
        ;
      else {case}
        os := os+bb[i];
    end {case};
    end;
  fp := 0;
  for i := 1 to Length(bbb) do
    begin
    Inc(fp);
    case bbb[i] of
      '?':
        if fp <= Length(aaa) then
          os := os+aaa[fp];
      '*':
        begin
        os := os+Copy(aaa, fp, MaxStringLength)
            +Copy(bbb, i+1, MaxStringLength);
        while Pos('?', os) <> 0 do
          Delete(os, Pos('?', os), 1);
        while Pos('*', os) <> 0 do
          Delete(os, Pos('*', os), 1);
        break;
        end;
      '>':
        if fp > 2 then
          Dec(fp, 2);
      '<':
        ;
      else {case}
        os := os+bbb[i];
    end {case};
    end;
  MkName := os;
  end { MkName };
{-DataCompBoy-}

{-DataCompBoy-}
function GetPath;
  var
    B: Byte;
  begin
  B := Length(S);
  while (B > 0) and not (S[B] in ['\', '/']) do
    Dec(B);
  if B = 0 then
    GetPath := S
  else
    GetPath := Copy(S, 1, B)
  end;
{-DataCompBoy-}

{-DataCompBoy-}
function GetName;
  var
    B: Byte;
  begin
  for B := Length(S) DownTo 1 do
    if S[B] in ['\', '/']
    then
      begin
      GetName := Copy(S, B+1, MaxStringLength);
      exit
      end;
  GetName := S;
  end;
{-DataCompBoy-}

{-DataCompBoy-}
function GetSName;
  var
    B: Byte;
    Pe: Byte;
  begin
  Pe := Length(S)+1;
  for B := Length(S) DownTo 1 do
    begin
    if  (S[B] = '.') and (Pe = Length(S)+1) then
      Pe := B;
    if S[B] in ['\', '/'] then
      break;
    end;
  if S[B] in ['\', '/'] then
    B := B+1
  else
    Pe := Pe-1; {JO}
  GetSName := Copy(S, B, Pe-B);
  end;
{-DataCompBoy-}

{-DataCompBoy-}
function GetIName;
  begin
  GetIName := SquashesName(GetName(S));
  end;
{-DataCompBoy-}

{-DataCompBoy-}
function GetAttrStr(Attr: word): Str6;
  var
    AttrStr: Str6;
  begin
  AttrStr := 'RHSVDA';
  if Attr and $01 = 0 then
    AttrStr[1] := '-';
  if Attr and $02 = 0 then
    AttrStr[2] := '-';
  if Attr and $04 = 0 then
    AttrStr[3] := '-';
  if Attr and $08 = 0 then
    AttrStr[4] := '-';
  if Attr and $10 = 0 then
    AttrStr[5] := '-';
  if Attr and $20 = 0 then
    AttrStr[6] := '-';
  GetAttrStr := AttrStr;
  end;

{$IFNDEF OS2}
{-DataCompBoy-}
function GetShortRelPath(Path: String): String;
  var
    CD: String;
  begin
  if Path[Length(Path)] in ['\', '/'] then
    SetLength(Path, Length(Path)-1);
  Path := lfGetShortFileName(Path);
  lGetDir(0, CD);
  if CD[Length(CD)] in ['\', '/'] then
    SetLength(CD, Length(CD)-1);
  CD := lfGetShortFileName(CD);
  if UpStrg(Copy(Path, 1, Length(CD))) = UpStrg(CD)
  then
    Delete(Path, 1, Length(CD));
  if Path[1] in ['\', '/'] then
    Delete(Path, 1, 1); {DelFC(Path);}
  GetShortRelPath := Path;
  end;
{-DataCompBoy-}
{$ENDIF}
{-DataCompBoy-}
function GetLongRelPath(Path: String): String;
  var
    CD: String;
  begin
  if Path[Length(Path)] in ['\', '/'] then
    SetLength(Path, Length(Path)-1);
  {$IFDEF OS_DOS}
  Path := lfGetLongFileName(Path);
  {$ENDIF}
  lGetDir(0, CD);
  if CD[Length(CD)] in ['\', '/'] then
    SetLength(CD, Length(CD)-1);
  if UpStrg(Copy(Path, 1, Length(CD))) = UpStrg(CD)
  then
    Delete(Path, 1, Length(CD));
  if Path[1] in ['\', '/'] then
    Delete(Path, 1, 1); {DelFC(Path);}
  GetLongRelPath := Path;
  end;
{-DataCompBoy-}

function MakeFileName(S: String): String;
  var
    I: integer;
  begin
  if  (S <> '..') and (S[Length(S)] = '.') then
    SetLength(S, Length(S)-1);
  MakeFileName := S;
  end;

{-DataCompBoy-}
function MakeNormName(const S, S1: String): String;
  var
    i, j: Byte;
  begin
  i := Length(S);
  while S[i] = ' ' do
    Dec(i);
  j := Length(S1);
  while S1[j] = ' ' do
    Dec(j);
  if i > 0 then
    begin
    if  (S[i] in ['\', '/']) then
      MakeNormName := Copy(S, 1, i)+Copy(S1, 1, j)
    else
      MakeNormName := Copy(S, 1, i)+'\'+Copy(S1, 1, j);
    end
  else
    MakeNormName := S1;
  end;
{-DataCompBoy-}

{-DataCompBoy-}
function SetFileAttr(const S: String; Attr: word): word;
  var
    F: lFile;
  begin
  lAssignFile(F, S);
  lSetFAttr(F, Attr);
  SetFileAttr := DosError;
  end;
{-DataCompBoy-}

{-DataCompBoy-}
function GetFileAttr(const S: String): word;
  var
    F: lFile;
    Attr: word;
  begin
  Attr := 0;
  lAssignFile(F, S);
  lGetFAttr(F, Attr);
  GetFileAttr := Attr;
  end;
{-DataCompBoy-}

{-DataCompBoy-}
function PathExist(s: String): Boolean;
  var
    lSR: lSearchRec;
  begin
  lFindFirst(MakeNormName(s, '*.*'), AnyFile, lSR);
  lFindClose(lSR);
  PathExist := DosError = 0;
  end;
{-DataCompBoy-}

{JO}
function PathFoundInArc(S: String): Boolean;
  begin
  if PosChar(':', Copy(S, 3, MaxStringLength)) > 0 then
    PathFoundInArc := True
  else
    PathFoundInArc := False;
  end;
{/JO}

{-DataCompBoy-}
procedure GetFTimeSizeAttr(const A: String; var ATime: LongInt;
    var ASize: TSize; var AAttr: word);
  var
    SR: lSearchRec;
  begin
  ClrIO;
  lFindFirst(A, AnyFile and not VolumeID, SR);
  ATime := SR.SR.Time;
  ASize := SR.FullSize;
  AAttr := SR.SR.Attr;
  lFindClose(SR);
  ClrIO;
  end;
{-DataCompBoy-}

function InQSMask(Name, Mask: String): Boolean; {JO}
  begin
  if QuickSearchType = 1 then
    InQSMask := InMask(Name, Mask)
  else
    InQSMask := (UpStrg(Copy(Name, 1, Length(Mask))) = UpStrg(Mask));
  end;

procedure InitQuickSearch(var QS: TQuickSearchData);
  begin
  if QuickSearchType = 1 then
    QS.Mask := '*'
  else
    QS.Mask := '';
  QS.NumExt := 0;
  QS.ExtD := 0;
  end;

procedure DoQuickSearch(var QS: TQuickSearchData; Key: word);
  begin
  if QuickSearchType = 1 then
    if Key = kbBack then
      if QS.Mask <> '*' then
        if QS.Mask[Length(QS.Mask)-1] <> '.' then
          begin
          if QS.Mask[Length(QS.Mask)-1] = '"' then
            SetLength(QS.Mask, Length(QS.Mask)-3)
          else
            SetLength(QS.Mask, Length(QS.Mask)-1);
          QS.Mask[Length(QS.Mask)] := '*';
          Dec(QS.ExtD);
          end
        else
          begin
          SetLength(QS.Mask, Length(QS.Mask)-2);
          QS.Mask[Length(QS.Mask)] := '*';
          Dec(QS.NumExt);
          QS.ExtD := 0;
          for Key := Length(QS.Mask)-1 DownTo 1 do
            if QS.Mask[Key] = '.' then
              break
            else if QS.Mask[Key] <> '"' then
              Inc(QS.ExtD);
          end
      else
    else
      case Char(Lo(Key)) of
        '*':
          ;
        '[', ']':
          begin
          SetLength(QS.Mask, Length(QS.Mask)-1);
          QS.Mask := QS.Mask+'"'+Char(Lo(Key))+'"*';
          Inc(QS.ExtD);
          end;
        '.':
          begin
          Inc(QS.NumExt);
          QS.Mask := QS.Mask+'.*';
          QS.ExtD := 0;
          end;
        else {case}
          begin
          SetLength(QS.Mask, Length(QS.Mask)-1);
          QS.Mask := QS.Mask+Char(Lo(Key))+'*';
          Inc(QS.ExtD);
          end;
      end

  else if Key = kbBack then
    begin
    if Length(QS.Mask) > 0 then
      begin
      SetLength(QS.Mask, Length(QS.Mask)-1);
      Dec(QS.ExtD);
      end;
    end
  else
    begin
    QS.Mask := QS.Mask+Char(Lo(Key));
    Inc(QS.ExtD);
    end;
  end { DoQuickSearch };

function GetCursorPos(const QS: TQuickSearchData; const Name: String;
    Size, ExtSize: word): word;
  var
    O: word;
    D: word;
  begin
  D := 1;

  if QuickSearchType = 1 then
    begin
    if not InMask(Name, QS.Mask) then
      begin
      GetCursorPos := 0;
      exit
      end;
    for O := 1 to QS.NumExt do
      begin
      while Name[D] <> '.' do
        Inc(D);
      Inc(D);
      end;
    end

  else
    begin
    if not InQSMask(Name, QS.Mask) then
      begin
      GetCursorPos := QS.ExtD+1;
      exit
      end;
    end;

  D := D+QS.ExtD;
  if ExtSize = 0 then
    GetCursorPos := Min(D, Size)
  else
    begin
    O := Length(GetSName(Name));
    if D > O+1 then
      begin
      D := D-O-1;
      GetCursorPos := Min(Size, Size-ExtSize+D);
      end
    else
      GetCursorPos := Min(Size-ExtSize, D);
    end;
  end { GetCursorPos };

{-DataCompBoy-}
function PackMask(const Mask: String; var PM: String {$IFDEF OS_DOS};
     LFNDis: Boolean {$ENDIF}): Boolean;
  label mc, fc;
  var
    k, k2, k3: Byte;
  begin
  if  (Mask = x_x) or (Mask = '*') then
    begin
    PM := x_x;
    PackMask := False;
    end
  else
    begin
    PackMask := True;
    {!}
    if  (PosChar(';', Mask) = 0) and (PosChar('[', Mask) = 0) then
      begin
      {$IFDEF OS_DOS}
      if LFNDis then
        begin
        {Check for correct DOS mask}
        if  (CharCount('.', Mask) < 2) and (CharCount('*', Mask) < 3)
        then
          begin
          k := PosChar('.', Mask);
          if k = 0 then
            k := Length(Mask);
          k2 := PosChar('*', Mask);
          if k2 > 0 then
            k3 := PosChar('*', Copy(Mask, k2+1, 255))
          else
            k3 := 0;
          if k3 > 0 then
            Inc(k3, k2);
          if  ( (k2 = 0) or
                (k2 = Length(Mask)) or
                (k2 = k-1)
              ) and
              ( (k3 = 0) or
                (k3 = Length(Mask))
              )
          then
            begin
            PM := Mask;
            PackMask := False;
            end
          else
            goto mc;
          end
        else
          begin
mc: {Place mask comressing here, but do not touch NormMask variable}
          goto fc; {Here is temporary code}
          end;
        end
      else {не LFNDis}
        begin
        {$ENDIF}
        PM := Mask;
        PackMask := False;
        {$IFDEF OS_DOS}
        end;
      {$ENDIF}
      end
    else {в Mask есть ';' или '[' }
      begin
fc: {Place filter comressing here, but do not touch NormMask variable}
      PM := x_x;
      end;
    end;
  end { PackMask };
{-DataCompBoy-}

{-DataCompBoy-}
procedure FileChanged(const Name: String);
  var
    Dr: String;
    Nm: String;
    Xt: String;
  begin
  lFSplit(Name, Dr, Nm, Xt);
  Abort := False;
  GlobalMessage(evCommand, cmRereadDir, @Dr);
  GlobalMessage(evCommand, cmRereadInfo, nil);
  GlobalMessage(evCommand, cmRereadTree, @Dr);
  end;
{-DataCompBoy-}

function CompareFiles(const N1, N2: String): Boolean;
  label Finish;
  const
    BufSize = 2048;
  var
    S1, S2: TDOSStream;
    B1, B2: Pointer;
    B: Boolean;
    I: LongInt;
  begin
  CompareFiles := False;
  B := False;
  B1 := nil;
  B2 := nil;
  S1.Init(N1, stOpenRead);
  if S1.Status <> stOK then
    begin
    S1.Done;
    exit
    end;
  S2.Init(N2, stOpenRead);
  if  (S2.Status <> stOK) or (S1.GetSize <> S2.GetSize) then
    goto Finish;
  B1 := MemAlloc(BufSize);
  if B1 = nil then
    goto Finish;
  B2 := MemAlloc(BufSize);
  if B2 = nil then
    goto Finish;
  I := BufSize;
  CompareFiles := True;
  while (S1.Status = stOK) and (S2.Status = stOK) and (I > 0) and not B
  do
    begin
    I := BufSize;
    if S1.GetSize-S1.GetPos < I then
      I := S1.GetSize-S1.GetPos;
    if I = 0 then
      break;
    S1.Read(B1^, I);
    S2.Read(B2^, I);
    asm
        push esi
        push edi
        mov  edi, B1
        mov  esi, B2
        cld
        mov  ecx, I
        rep  cmpsb
        jz   @1
        xor  al, al
        mov  @Result, al
        inc  al
        mov  B, al
       @1:
        pop  edi
        pop  esi
      end;
    end;
  if not B then
    CompareFiles := (S1.Status = stOK) and (S2.Status = stOK);
Finish:
  if B1 <> nil then
    FreeMem(B1, BufSize);
  if B2 <> nil then
    FreeMem(B2, BufSize);
  S1.Done;
  S2.Done;
  end { CompareFiles };

end.
