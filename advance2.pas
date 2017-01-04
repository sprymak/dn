{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source
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
//  Version history:
//
//  1.6.RC1
//  dn16rc1-move_COMBINE_MIXED_from_sources_to_Options_menu_diff140byMV.patch
//  dn16rc1-Novell_and_empty_directory_fix_diff144byMV.patch
//  dn16rc1-OS2_compatibility_related_fix2_diff148byMV.patch
//  dn16rc1-file_masks_and_advanced_filter_fix.patch
//  dn16rc1-W2K_compatibility_fix-diff156byMV.patch
//
//  2.0.0
//  dn200-delete_file_from_temp_fix-diff161byMV.patch
//
//  2.3.0
//  dn269-make_del_dir_on_network_resource.patch
//
//  2.7.0
//  dn270-search_in_archives.patch
//  dn270-mark_dirs-jo20707a.patch
//  dn270-nameless_files_are_extension.patch
//  dn2922-show_size_before_name_if_it_longer_250.patch
//  dn21128-show_LFN_and_desc_in_bottom.patch
//  dn3421-FileOpenDialog(f)-quick_search_fix.patch
//  dn368-Kernel(f)-GetSName_function_fix.patch
//
//  3.7.0
//  dn31005-bp_to_vp_on_off_true_false.patch
//
//  4.9.0
//  dn50208-cleanup.patch
//
//  5.9.0
//
//////////////////////////////////////////////////////////////////////////}
{$I STDEFINE.INC}

unit Advance2; {File related functions}
interface
uses advance, dos, lfn, UFnMatch, objects;

var
  ErrorFCode: Byte;

function  ExistFile(const FName : String) : Boolean; {DataCompBoy}
function  ExistDir(const DName: string): Boolean; { VK }
function  FileTime(FileA: String): LongInt;
function  FileNewer(FileA, FileB : String) : Boolean;
function  CalcTmpFName(Id: LongInt; const AExt: String; ANew: Boolean): String; {DataCompBoy}
function  CalcTmpId:LongInt;
function  IsDriveCDROM(Drive : Char) : Boolean;
procedure lChDir(S: string); {DataCompBoy}
procedure EraseByName(const FName: String); {DataCompBoy}
Procedure EraseFile(const N: String); {DataCompBoy}
procedure EraseTempFile(S: String); {piwamoto}{JO}
Function  ValidDrive(dr : char) : Boolean;
Function  GetDrive : byte;
Procedure SetDrive(a : byte);
Procedure GetMask(var m : string);
function  GetCurDrive: Char;
procedure SetCurDrive(C: Char);
function  GetRoot(const S: String): string;
function  GetCurrentDir: string;
function  lFExpand(const S: string): string;
{DataCompBoy: Make shure, that you use this version of lFExpand when you}
{expands names from dialogs...}

FUNCTION  GetExt(const s:string):string;
{$IFNDEF OS2}
FUNCTION  Norm12(const s:string):Str12;
{$ENDIF}
       {-DataCompBoy-}
FUNCTION  DelSquashes(s: string): string;           {removes quotes}
FUNCTION  GetURZ(const s: string): Str12;           {cuts name to 8.3}
FUNCTION  GetfURZ(const s: string):String;          {cuts name and path to 8.3}
FUNCTION  SquashesName(const s:string):string;      {quotes name if needed}
FUNCTION  InMask(Name, Mask: string):Boolean;       {does Name match Mask? }
FUNCTION  InFilter(Name, Filter: string):Boolean;   {does Name match Filter? }
FUNCTION  InExtMask(Name, Mask: string):Boolean;    {does Ext match Mask? }
FUNCTION  InDirFilter(Name, Filter: string):Boolean; {JO}{piwamoto}

type TMaskPos = record
                 CurPos: Byte;
                 InSq: boolean;
                end;
     PMasksPos = ^TMasksPos;
     TMasksPos = array[0..255] of TMaskPos;
     TMasksData = Record
                   Num: Byte;
                  case boolean of
                   true: (P: pointer;);
                   false:(A: Array[1..2] of TMaskPos;);
                  end;
     TMasksDataArr = array[32..255] of TMasksData;
     PMaskData = ^TMaskData;
     TMaskData = record
                  Filter: String;
                  MP: TMasksDataArr;
                 end;

PROCEDURE MakeTMaskData(var MD: TMaskData);         {Fills TMaskData members}
                                                    {for work of InExtFilter}
FUNCTION  InExtFilter(Name: string; const F: TMaskData):Boolean;
                                                    {does extension of  }
                                                    {Name match Filter? }

FUNCTION  InOldMask(Name,Mask: string):Boolean;     {DataCompBoy}
(*FUNCTION  InOldFilter(Name,Filter: string):Boolean; {DataCompBoy}*)
FUNCTION  InSpaceMask(Name,Mask: string;ValidSpace: Boolean):Boolean;
FUNCTION  InSpaceFilter(Name,Filter: string):Boolean;
function  IsMixedCase(const Name: string): boolean; {JO}{piwamoto}

FUNCTION  IsDir(const s: string): boolean; {is this a directory? }
function  MkName(const Nm, Mask: String): String;{modifies name to fit Mask}
FUNCTION  GetPath(const S: String): String;
FUNCTION  GetName(const S: String): String;
FUNCTION  GetSName(const S: String): String;
FUNCTION  GetIName(const S: String): String;
FUNCTION  GetAttrStr(Attr: Word): Str6;
{$IFNDEF OS2}
FUNCTION  GetShortRelPath(Path: string): string;
{$ENDIF}
FUNCTION  GetLongRelPath(Path: string): string;
       {-DataCompBoy-}
FUNCTION  MakeFileName(S: string): string;
FUNCTION  MakeNormName(const S, S1: string): string; {DataCompBoy}
function  GetFileAttr(const S: String): Word;
function  SetFileAttr(const S: String; Attr: Word):Word;
function  CorrectFile(const N: String): Boolean;
function  PathExist(s: string): boolean; {Is path exist}
function  GetNormPath(s: string): string; {Get correct path}
function  PathFoundInArc(S: String): Boolean; {JO}

{-DataCompBoy-}
type TQuickSearchData = record
      Mask: string;
      NumExt: Word;
      ExtD: Word;
     end;

function  InQSMask(Name, Mask: string):Boolean; {JO}

procedure InitQuickSearch(var QS: TQuickSearchData);
procedure DoQuickSearch(var QS: TQuickSearchData; Key: Word);
function  GetCursorPos(const QS: TQuickSearchData; const Name: String;
                       Size, ExtSize: word): word;
procedure FileChanged (const Name: String);
{-DataCompBoy-}

type
   PTempFile = ^TTempFile;
   TTempFile = object(TBufStream)
     constructor Init(const AExt: String; ABufSize: SW_Word);
     destructor  Done; virtual;
   end;

function  PackMask(const Mask: string; var PM: String; LFNDis: boolean): boolean; {DataCompBoy}

function CompareFiles(const N1, N2: String): Boolean;

implementation
uses advance1, advance3,
     {$IFNDEF NONBP}BStrings{$ELSE}Strings{$ENDIF},
     Commands, dnapp, memory, drivers, drvtypes, views
     , DnIni
     {$IFDEF VIRTUALPASCAL}, VpSysLow {$ENDIF}
     {$IFDEF DPMI}, Dpmi {$ENDIF}
     ;

constructor TTempFile.Init(const AExt: String; ABufSize: SW_Word);
var
  L: LongInt;
  S: FNameStr;
begin
  L:=CalcTmpId;
  S:=CalcTmpFName(L, AExt, True);
  inherited Init(s, (stCreate and fmDeny) or fmDenyAll or fmDenyChild, ABufSize);
end;

destructor TTempFile.Done;
var
  S: FNameStr;
begin
  S:=lFileNameOf(FHandle);
  inherited Done;
  EraseFile(s);
end;


function CorrectFile(const N: String): Boolean;
   var I: Integer;
begin
   CorrectFile := False;
   for I := 1 to Length(N) do
       if N[I] in IllegalCharSet then Exit; {DataCompBoy}
   CorrectFile := True;
end;

        {-DataCompBoy-}
function ExistFile(const FName: String): Boolean;
var
  DirInfo:lSearchRec;
begin
 NeedAbort:=True; (* X-Man *)
 lFindFirst(FName,Archive+ReadOnly+Hidden+SysFile,DirInfo);
 NeedAbort:=False; (* X-Man *)
 if DosError=0 then ExistFile:=True
 else begin
  ExistFile:=False;
  ErrorFCode:=DosError;
 end;
 lFindClose(DirInfo);
end;
        {-DataCompBoy-}

{ VK/ }
function  ExistDir(const DName: string): Boolean; {based on ExistFile}
var
 Dirinfo:lsearchrec;
begin
 NeedAbort:=True; (* X-Man *)
 If DName[Length(DName)]='\'
  then lFindFirst(Copy(DName, 1, Length(DName)-1),Directory,DirInfo)
  else lFindFirst(DName,Directory,DirInfo);
 NeedAbort:=False; (* X-Man *)
 if DosError=0 then existdir:=True
 else begin
  Existdir:=False;
  ErrorFCode:=DosError;
 end;
 lFindClose(DirInfo);
end;
{ /VK }

function FileTime(FileA: String): LongInt;
var
  TA : LongInt;
  FA : lfile;
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
        lm,ld,lh,lmin: LongInt;
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
begin
 Id:=GetTmpId;
 if (LastTmpId<>-1) and (LastTmpId>=Id) then Id:=LastTmpId+1;
 LastTmpId:=Id;
 CalcTmpId:=Id;
end;

function IsDriveCDROM(Drive : Char) : Boolean;
begin
{$IFNDEF VIRTUALPASCAL}
 IsDriveCDROM := GetDriveType(Ord(UpCase(Drive))-64) = dtCDRom; (* X-Man *)
{$ELSE}
 IsDriveCDROM := SysGetDriveType(Drive) = dtCDRom;
{$ENDIF}
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
   {$IFNDEF NONBP}
   19 : asm;
          mov  ah,0dh { Reset Disks }
          int  21h
        end;
   {$ENDIF}
  end;
  ClrIO;
end;

procedure EraseTempFile; {JO: проверочка файла на нахождение во временном каталоге не помешает}
begin
 if UpStrg(GetPath(S)) = UpStrg(Tempdir) then EraseFile(S);
end;

function ValidDrive(dr : char) : Boolean;
{$IFDEF VIRTUALPASCAL}
var
  DriveNum, LogDrvMap: Longint;
begin                   { LogDrvMap: Bit 0: 'A:', Bit 1: 'B:', etc }
  LogDrvMap := SysGetValidDrives;
  ValidDrive := ((1 shl (Ord(Dr) - Ord('A'))) and LogDrvMap) <> 0;
end;
{$ELSE}
var
    B,B1: Byte;
    r: {$IFDEF DPMI}DPMIRegisters{$ELSE}Registers{$ENDIF};
    s,s1 : string[40];
begin
 ValidDrive := False;
 if dr='*' then begin ValidDrive:=True; exit end;
 if dr<'A' then exit;
 dr:=UpCase(dr); {John_SW  07-05-2003 drive letter may be in low case!}
 if (dr < 'C') then
   begin
     ValidDrive := (NumFloppy >= Byte(dr)-64);
     Exit;
   end;
 B := GetDrive;
 NeedAbort:=True; (* X-Man *)
 SetDrive(Byte(Dr)-65);
 B1 := GetDrive;
 NeedAbort:=True; (* X-Man *)
 SetDrive(B);
 NeedAbort:=False; (* X-Man *)
 if Byte(dr)-65 <> b1 then Exit;
 s:=dr+':\QQP.OBJ'#0;
{$IFDEF NOASM}
 r.ax:=$2900;
 r.ds:=seg(s);
 r.si:=ofs(s[1]);
 r.es:=seg(s1);
 r.di:=ofs(s1);
 NeedAbort:=True; (* X-Man *)
 {$IFDEF DPMI}SimulateRealModeInt{$ELSE}Intr{$ENDIF}($21,r);
 NeedAbort:=False; (* X-Man *)
 if r.ax<>$ff then ValidDrive:=true else ValidDrive:=false;
{$ELSE}
 NeedAbort:=True; (* X-Man *)
 asm
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
 end;
 NeedAbort:=False; (* X-Man *)
{$ENDIF}
end;
{$ENDIF}

FUNCTION GetDrive : byte;
{$IFNDEF VIRTUALPASCAL}
assembler; asm mov ah,$19; int 21h; end;
{$ELSE}
var S: String;
begin
  GetDir(0, S);
  GetDrive := Byte(S[1]) - $41 {Byte('A')};
end;
{$ENDIF}

PROCEDURE SetDrive(a : byte);
{$IFNDEF VIRTUALPASCAL}
assembler; asm mov ah,$0E; mov dl,a; int $21; end;
{$ELSE}
var S: String;
begin GetDir(0, S); ChDir(S); end;
{$ENDIF}

procedure GetMask(var m : string);
 var
     i : Byte;
     b : Boolean;
     q : string[12];
begin
 q:='????????.???';
 i:=Pos('.',m);
 if i>0 then if i>9 then Delete(m,9,i-9) else Insert(Copy('        ',1,9-i),m,i)
   else m:=Copy(m+Strg(' ',8),1,8)+'.   ';
 i:=1;b:=True;
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
 i:=1;b:=True;
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

function GetCurDrive;
{$IFNDEF VIRTUALPASCAL}
assembler;
asm
 mov ah, 19h
 int 21h
 xor ah, ah
 add al, 65
end;
{$ELSE}
begin GetCurDrive := Char(GetDrive+65) end;
{$ENDIF}

procedure SetCurDrive;
{$IFNDEF VIRTUALPASCAL}
assembler;
asm
 mov ah, 0Eh
 mov dl, C
 sub dl, 65
 int 21h
end;
{$ELSE}
begin SetDrive(Byte(C)-65) end;
{$ENDIF}

FUNCTION GetExt;{DataCompBoy}{fixed by piwamoto}
var i:byte;
begin
 for i:=length(s) downto 1 do if s[i] in ['.','\','/'] then break;
 if s[i] = '.' then GetExt:=Copy(s, i, 255) else GetExt:='.';
end;

{$IFNDEF OS2}
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
{$ENDIF}

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
 var j:boolean;
     l:byte;
     ext, maskext:string;
Begin
 if (Mask=x_x) or (Mask='*') then begin InMask:=true; exit; end;

 j := Mask[Length(Mask)] = '.';  {JO}
 UpStr(Mask); maskext:='';
 l:=length(mask); while (l>0) and (not (mask[l] in ['\','/'])) and (mask[l]<>'.') do dec(l);
 if mask[l]='.' then begin maskext:=copy(mask, l+1, 255);delete(mask, l, 255); end;

 ext:=''; UpStr(Name);
 if (MaskExt<>'') or j then      {JO}
   begin
     l:=length(Name); while (l>0) and (not (Name[l] in ['\','/'])) and (Name[l]<>'.') do dec(l);
     if Name[l]='.' then begin ext:=copy(Name, l+1, 255);delete(Name, l, 255); end;
     if (Ext='') and not j then mask:=mask+'.'+maskext;  {JO}
   end;
 if ext<>'' then InMask := FnMatch(mask, Name) and FnMatch(maskext, ext)
            else InMask := FnMatch(mask, Name);
End;
        {-DataCompBoy-}

        {-DataCompBoy-}
FUNCTION InFilter;
var i: byte; k: byte absolute Filter;
    B: Boolean;
    j:boolean;
    S: string;
begin
  InFilter:=True;
  while k>0 do
   begin
    i:=k+1;
    j:=False;
    repeat
     dec(i);
     if Filter[i]='"' then j:=not j;
    until (i=1) or ((Filter[i] in [';',',']) and not j);
    if Filter[i] in [';',','] then S := Copy(Filter,i+1,255)
                              else S := Filter;
    B := S[1] <> '-'; k:=i-1;
    InFilter := B;
    if not B then DelFC(S);
    DelLeft(S); DelRight(S);
    if (S <> '') and InMask(Name, S) then Exit;
   end;
 InFilter:=False;
end;
        {-DataCompBoy-}

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

{JO}
function InDirFilter;
var
    B,J: Boolean;
    I: byte;
    S: string;
begin
  InDirFilter:=True;
  while Length(Filter) > 0 do
   begin
    i := Length(Filter) + 1;
    j := False;
    repeat
     dec(i);
     if Filter[i]='"' then j:=not j;
    until (i=1) or ((Filter[i] in [';',',']) and not j);
    if Filter[i] in [';',','] then S := Copy(Filter, i+1, 255)
                              else S := Filter;
    B := S[1] <> '-';
    Filter[0] := Char(i-1);
    InDirFilter := B;
    if not B then Delete(S, 1, 1); {DelFC(S);}
    DelLeft(S); DelRight(S);
    if (S <> '') and (S[Length(S)] in ['\', '/']) and
       InMask(Name, Copy(S, 1, Length(S)-1))
      then Exit;
   end;
 InDirFilter:=False;
end;
{/JO}

procedure MakeTMaskData(var MD: TMaskData);
var i, j, l, e, k, o, NumMP: byte;
    Dina1: array[0..255] of record
                             CurPos: Byte;
                             Meta: Boolean;
                            end;
    InSq: Boolean;
    InS: Boolean;
    Pss: array[1..255] of TMaskPos;
label Ok, Ne;
begin with MD do begin
  for I:=32 to 255 do with MP[I] do if Num>4 then FreeMem(P, Num);
  FillChar(MP, SizeOf(MP), 0);

  if Filter = '' then exit;
  if Filter[1] in [' ',#9] then DelLeft(Filter);
  if not (Filter[1] in [';',',']) then begin
   Inc(Filter[0]);
   move(Filter[1], Filter[2], Byte(Filter[0]));
   Filter[1] := ';';
  end;
  If not (Filter[Length(Filter)] in [';',',']) then begin
   Inc(Filter[0]);
   Filter[Length(Filter)]:=';';
  end;

  InSq := false;
  Dina1[0].CurPos:=0;
  NumMP := 0;

  for i:=length(Filter) downto 2 do
   if (Filter[i] in [';', ',']) and not InSq then
    if not (Filter[i-1] in [';', ',']) then begin
     Inc(NumMP);
     Dina1[NumMP].CurPos := i-1;
     Dina1[NumMP].Meta := False;
    end else
   else
   if (Filter[i] = '"') then InSq:=not InSq
   else
   if (Filter[i] = '*') then Dina1[NumMP].Meta := True;

  For j := 32 to 255 do if UpCaseArray[Char(j)]=Char(j) then begin
   l := 0;
   if j <> Byte('*') then begin
    For I := 1 to NumMP do if not Dina1[I].Meta then begin
     E := Dina1[I].CurPos;
     if Filter[e] = '"'
      then begin InSq:=true; dec(e); end
      else InSq := False;

     if (Filter[e] = ']') and not InSq then begin
      Dec(e);
      InS := False;
      For K:=e downto 1 do
       if (Filter[k] = '[') and not InS
        then break
        else if Filter[k] = '"' then InS := not InS;
      O:=K-1;
      inc(k);
      if Filter[k] = '^' then begin InS := true; inc(k) end else InS := false;
      for k:=k to e do
       if Filter[k+1] = '-' then
        if InS xor ((UpCaseArray[Char(J)] >= UpCaseArray[Filter[k]]) and
                    (UpCaseArray[Char(J)] <= UpCaseArray[Filter[k+2]]))
         then goto Ok
         else inc(k,2)
       else
        if InS xor (UpCaseArray[Char(J)] = UpCaseArray[Filter[k]])
         then goto Ok;
      Goto Ne;
     end;

     if (UpCaseArray[Filter[e]] = UpCaseArray[Char(j)]) or
        (Filter[e]='.') or (Filter[e]='?') then begin
      if Filter[e]<>'.' then Dec(e);
      O := e;
Ok:   Inc(l);
      Pss[l].CurPos:=o;
      Pss[l].InSq:=InSq;
     end;
Ne: end;
   end else
    For I := 1 to NumMP do
     if Dina1[I].Meta then begin
      Inc(l);
      Pss[l].CurPos:=Dina1[I].CurPos;
     end;
   if l<=2 then begin
    move(Pss[1], MP[j].A[1], l*SizeOf(TMaskPos));
    MP[j].Num := L;
   end else begin
    GetMem(MP[j].P, l*SizeOf(TMaskPos));
    move(Pss[1], MP[j].P^, l*SizeOf(TMaskPos));
    MP[j].Num := L;
   end;
  end;
end end;

        {-DataCompBoy-}
FUNCTION InExtFilter;
var i, j, l, e: byte;
    Dina1, Dina2: TMasksPos;
    Num1, Num2: Byte;
    InSq: Boolean;
label Try2, q;
begin
  if Byte(Name[Length(Name)]) < 32 then begin InExtFilter:=false; exit end;
  Num1:=F.MP[Byte(UpCaseArray[Name[Length(Name)]])].Num;
  if Num1=0 then goto q;
  Dina1[0].CurPos := 1;
  if Num1<=2 then
   move(F.MP[Byte(UpCaseArray[Name[Length(Name)]])].A,
        Dina1[1],
        Num1*SizeOf(TMaskPos))
  else
   move(PMasksPos(F.MP[Byte(UpCaseArray[Name[Length(Name)]])].P)^,
        Dina1[1],
        Num1*SizeOf(TMaskPos));
  InExtFilter := True;
  j:=Length(Name) - 1;
  while Num1>0 do begin
   Num2 := 0;
   for i:=1 to Num1 do
Try2: begin
    if F.Filter[Dina1[i].CurPos] = '"' then begin
     Dina1[i].InSq:=not Dina1[i].InSq;
     Dec(Dina1[i].CurPos);
     Goto Try2
    end else
    if (F.Filter[Dina1[i].CurPos] in [';', ',']) and
       not Dina1[i].InSq then begin
     if Name[j]='.' then exit;
    end else
    if (F.Filter[Dina1[i].CurPos] = '-') and
       (F.Filter[Dina1[i].CurPos-1] in [';', ',']) and
       not Dina1[i].InSq then begin
     if Name[j]='.' then begin
      InExtFilter:=false;
      exit;
     end
    end else
    if (F.Filter[Dina1[i].CurPos] = '.') and
       (F.Filter[Dina1[i].CurPos-1] in [';', ',']) and
       not Dina1[i].InSq then begin
     if (PosChar('.', Name)=0) then exit;
    end else
    if (F.Filter[Dina1[i].CurPos] = ']') and
       not Dina1[i].InSq then begin
     InSq := Dina1[i].InSq;
     for l := Dina1[i].CurPos-1 downto 1 do
      if F.Filter[l] = '"' then InSq:=not InSq else
      if not InSq and (F.Filter[l]='[') then break else
      if F.Filter[l] = ';' then break;
     if F.Filter[l] <> '[' then Continue;
     e := Dina1[i].CurPos-1;
     inc(Num2);
     Dina2[Num2].CurPos := l-1;
     Dina2[Num2].InSq := Dina1[i].InSq;
     inc(l);
     InSq := F.Filter[l] = '^';
     if InSq then inc(l);
     for l:=l to e do
      if F.Filter[l+1] <> '-' then
       if Dina2[Num2].InSq xor (UpCaseArray[F.Filter[l]]=UpCaseArray[Name[j]])
        then begin dec(Num2); break end
        else break
      else begin
       if Dina2[Num2].InSq xor
          ((UpCaseArray[Name[j]] >= UpCaseArray[F.Filter[l]]) and
           (UpCaseArray[Name[j]] <= UpCaseArray[F.Filter[l+2]])
          )
        then break
        else begin dec(Num2); break end;
       inc(l, 2);
      end;
    end else
    if (F.Filter[Dina1[i].CurPos]='?') or
       (UpCaseArray[F.Filter[Dina1[i].CurPos]]=UpCaseArray[Name[j]]) then begin
     inc(Num2);
     Dina2[Num2].CurPos := Dina1[i].CurPos - 1;
     Dina2[Num2].InSq := Dina1[i].InSq;
    end;
   end;
   Num1:=Num2;
   move(Dina2[1], Dina1[1], Num1*SizeOf(TMaskPos));
   Dec(j);
  end;
q:if F.MP[Byte('*')].Num<>0
   then
    if F.MP[Byte('*')].Num <= 2 then
     for I:=1 to F.MP[Byte('*')].Num do begin
      for J:=F.MP[Byte('*')].A[I].CurPos downto 1 do
       if F.Filter[J]=';' then break;
      if F.Filter[J]=';' then inc(J);
      if InMask(Name, '*.' + Copy(F.Filter, J,
                                  F.MP[Byte('*')].A[I].CurPos - J + 1))
       then exit;
     end
    else
     for I:=1 to F.MP[Byte('*')].Num do begin
      for J:=PMasksPos(F.MP[Byte('*')].P)^[I].CurPos downto 1 do
       if F.Filter[J]=';' then break;
      if F.Filter[J]=';' then inc(J);
      if InMask(Name, '*.' + Copy(F.Filter, J,
                                  PMasksPos(F.MP[Byte('*')].P)^[I].CurPos - J))
       then exit;
     end;
  InExtFilter := false
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
      then begin InOldMask:=False; Exit end
  until i=0; InOldMask:=True
end;
        {-DataCompBoy-}

(*        {-DataCompBoy-}
FUNCTION InOldFilter;
var i:byte; l:byte absolute Filter;
    B: Boolean;
    S: string[13];
begin
  InOldFilter:=True; if Pos(' ',Filter) > 0 then Filter := DelSpaces(Filter);
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
  until l=0; InOldFilter:=False
end;
        {-DataCompBoy-}*)

FUNCTION InSpaceMask;
var i:byte;
begin
  i:=13;
  repeat
    dec(i);
    if (Mask[i]='?') or ((Mask[I]=' ') and ValidSpace) or (UpCase(Mask[i])=UpCase(Name[i]))
      or (I = 9) then else begin InSpaceMask:=False; Exit end
  until i=0; InSpaceMask:=True
end;

FUNCTION InSpaceFilter;
var i:byte; l:byte absolute Filter;
    B: Boolean;
    S: string[13];
begin
  InSpaceFilter:=True; if Pos(' ',Filter) > 0 then Filter := DelSpaces(Filter);
  UpStr(Filter); UpStr(Name);
  if Filter='' then Exit; Name:=Norm12(Name);
  repeat if Filter[l]=';' then dec(l);
    if l<>0 then begin
      i:=l; while (i>1)and(Filter[pred(i)]<>';') do dec(i);
      S := Copy(Filter,i,succ(l-i)); B := S[1] = '-';
      InSpaceFilter := not B;
      if B then DelFC(S);
      DelLeft(S);
      if (S <> '') and InSpaceMask(Name, Norm12(S), True) then Exit;
      l:=pred(i);
    end
  until l=0; InSpaceFilter:=False
end;


function IsMixedCase(const Name: String): Boolean;
var
 MixedDir, MixedName, MixedExt : string;
begin
 lFsplit(Name, MixedDir, MixedName, MixedExt);
 if ((UpStrg(MixedName) = MixedName) or (LowStrg(MixedName) = MixedName)) and
    ((UpStrg(MixedExt) = MixedExt) or (LowStrg(MixedExt) = MixedExt))
 then
   IsMixedCase := False
 else
   IsMixedCase := True;
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
     fp:byte;
begin
 new(aa); new(aaa);
 new(bb); new(bbb);
 new(os);
 lFSplit(Nm,   os^, aa^, aaa^);
 lFSplit(Mask, os^, bb^, bbb^);
 os^:='';
 fp:=0;
 for i:=1 to length(bb^) do begin
  inc(fp);
  case bb^[i] of
   '?': if fp<=byte(aa^[0]) then os^:=os^+aa^[fp];
   '*': begin
         os^:=os^+copy(aa^, fp, 255)+copy(bb^,i+1,255);
          if pos('*', os^)<>0 then                        {Pavel Anufrikov -> }
           begin
            insert(copy(aaa^,2,255),os^,pos('*', os^));
            aaa^:='';
           end;                                           { <- Pavel Anufrikov}
         while pos('?', os^)<>0 do delete(os^, pos('?', os^), 1);
         while pos('*', os^)<>0 do delete(os^, pos('*', os^), 1);
         break;
        end;
   '>': if fp>2 then dec(fp,2);
   '<': ;
   else os^:=os^+bb^[i];
  end;
 end;
 fp:=0;
 for i:=1 to length(bbb^) do begin
  inc(fp);
  case bbb^[i] of
   '?': if fp<=byte(aaa^[0]) then os^:=os^+aaa^[fp];
   '*': begin
         os^:=os^+copy(aaa^, fp, 255)+copy(bbb^,i+1,255);
         while pos('?', os^)<>0 do delete(os^, pos('?', os^), 1);
         while pos('*', os^)<>0 do delete(os^, pos('*', os^), 1);
         break;
        end;
   '>': if fp>2 then dec(fp,2);
   '<': ;
   else os^:=os^+bbb^[i];
  end;
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
{--- start -------- Eugeny Zvyagintzev ---- 15-05-2003 ----}
 If (Copy(S,Length(S)-2,2) = '..') Or (S[Length(S)] = '.') Then
  Begin
   GetSName:=S;
   Exit;
  End;
{--- finish -------- Eugeny Zvyagintzev ---- 15-05-2003 ----}
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

{$IFNDEF OS2}
        {-DataCompBoy-}
FUNCTION GetShortRelPath(Path: string): string;
 var CD: String;
 begin
  if Path[length(Path)] in ['\','/'] then dec(Path[0]);
  Path:=lfGetShortFileName(Path);
  lGetDir(0,CD);
  if CD[length(CD)] in ['\','/'] then dec(CD[0]);
  CD:=lfGetShortFileName(CD);
  if UpStrg(copy(Path, 1, length(CD)))=UpStrg(CD)
   then Delete(Path, 1, length(CD));
  if Path[1] in ['\','/'] then DelFC(Path);
  GetShortRelPath:=Path;
 end;
        {-DataCompBoy-}
{$ENDIF}

        {-DataCompBoy-}
FUNCTION GetLongRelPath(Path: string): string;
 var CD: String;
 begin
  if Path[length(Path)] in ['\','/'] then dec(Path[0]);
  {$IFNDEF OS2}
  Path:=lfGetLongFileName(Path);
  {$ENDIF}
  lGetDir(0,CD);
  if CD[length(CD)] in ['\','/'] then dec(CD[0]);
  if UpStrg(copy(Path, 1, length(CD)))=UpStrg(CD)
   then delete(Path, 1, length(CD));
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
function SetFileAttr(const S: String; Attr: Word) : Word;
var F:lfile;
begin
 lAssignFile(F, s);
 lSetFAttr(F, Attr);
 SetFileAttr:=DosError;
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
function GetFileAttr(const S: String): Word;
var
    Attr:Word;
    F:lfile;
begin
 Attr := 0;
 lAssignFile(F, s);
 lGetFAttr(F, Attr);
 GetFileAttr:=Attr;
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
function  PathExist(s: string): boolean;
var lSR: lSearchRec;
begin
 NeedAbort:=True; (* X-Man *)
 lFindFirst(MakeNormName(S,'*.*'), AnyFile, lSR);
 if DosError<>0 then
   begin
     lFindClose(lSR);
     {piwamoto: additional check for Novell compatibility}
     lFindFirst(MakeNormName(S,'nul'), AnyFile, lSR);
   end;
 NeedAbort:=False; (* X-Man *)
 PathExist:=DosError=0;
 lFindClose(lSR);
end;
        {-DataCompBoy-}

function  GetNormPath(s: string): string; {Get correct path}
var q: byte absolute s;
begin
 GetNormPath:=s;
 S:=MakeNormName(s,'');
 for q:=length(s) downto 3 do
  if (s[q] in ['\','/']) and
     PathExist(S) then begin
   GetNormPath:=s;
   exit;
  end;
end;

{JO}
function  PathFoundInArc(S: String): Boolean;
begin
 if PosChar(':', Copy(S, 3, 255)) > 0 then
   PathFoundInArc := True
  else
   PathFoundInArc := False;
end;
{/JO}

function  InQSMask(Name, Mask: String):Boolean; {JO}
begin
  if QuickSearchType then
    InQSMask:=InMask(Name, Mask)
  else
    InQSMask:=(UpStrg(Copy(Name, 1, Length(Mask)))=UpStrg(Mask));
end;

procedure InitQuickSearch(var QS: TQuickSearchData);
begin
  if QuickSearchType then
    QS.Mask:='*'
  else
    QS.Mask:='';
  QS.NumExt:=0;
  QS.ExtD := 0;
end;

procedure DoQuickSearch(var QS: TQuickSearchData; Key: Word);
begin
  if QuickSearchType then
    if Key=kbBack then
      if QS.Mask<>'*' then
        if QS.Mask[Length(QS.Mask)-1] <> '.' then
          begin
            if QS.Mask[Length(QS.Mask)-1] = '"' then
              QS.Mask[0] := Char(Length(QS.Mask)-3)
            else
              Dec(QS.Mask[0]);
            QS.Mask[Length(QS.Mask)]:='*';
            Dec(QS.ExtD);
          end
        else
          begin
            QS.Mask[0] := Char(Length(QS.Mask)-2);
            QS.Mask[Length(QS.Mask)]:='*';
            Dec(QS.NumExt);
            QS.ExtD:=0;
            for Key:=Length(QS.Mask)-1 downto 1 do
              if QS.Mask[Key]='.' then
                break
              else
                if QS.Mask[Key]<>'"' then
                  Inc(QS.ExtD);
          end
      else
    else
      case Char(Lo(Key)) of
        '*':
          ;
        '[',']':
          begin
            Dec(QS.Mask[0]);
            QS.Mask:=QS.Mask+'"'+Char(Lo(Key))+'"*';
            Inc(QS.ExtD);
          end;
        '.':
          begin
            Inc(QS.NumExt);
            QS.Mask:=QS.Mask+'.*';
            QS.ExtD:=0;
          end;
        else
          begin
            Dec(QS.Mask[0]);
            QS.Mask:=QS.Mask+Char(Lo(Key))+'*';
            Inc(QS.ExtD);
          end;
      end

  else
    if Key=kbBack then
      begin
        if Length(QS.Mask) > 0 then
          begin
            Dec(QS.Mask[0]);
            Dec(QS.ExtD);
          end;
      end
    else
      begin
        QS.Mask := QS.Mask+Char(Lo(Key));
        Inc(QS.ExtD);
      end;
end;

function  GetCursorPos(const QS: TQuickSearchData; const Name: String;
                       Size, ExtSize: word): word;
var
  O: Word;
  D: Word;
begin
  D:=1;

  if QuickSearchType then
    begin
      if not InMask(Name, QS.Mask) then
        begin
          GetCursorPos:=0;
          exit
        end;
      for O:=1 to QS.NumExt do
        begin
          while Name[D]<>'.' do
            Inc(D);
          Inc(D);
        end;
    end

  else
    begin
      if not InQSMask(Name, QS.Mask) then
        begin
          GetCursorPos:=QS.ExtD+1;
          exit
        end;
    end;

  D:=D+QS.ExtD;
  if ExtSize=0 then
    GetCursorPos:=Min(D, Size)
  else
    begin
      O:=Length(GetSName(Name));
      if D>O+1 then
        begin
          D:=D-O-1;
          GetCursorPos:=Min(Size, Size-ExtSize+D);
        end
      else
        GetCursorPos:=Min(Size-ExtSize, D);
    end;
end;

{-DataCompBoy-}
function  PackMask(const Mask: string; var PM: String; LFNDis: boolean): boolean;
 label mc, fc;
 var k, k2, k3: byte;
 begin
  if (Mask = x_x) or (Mask='*') then begin
   PM:=x_x;
   PackMask:=False;
  end else begin
   PackMask:=True;
   {!}
   if (PosChar(';', Mask)=0) and (PosChar('[', Mask)=0) then begin
    if LFNDis then begin
     {Check for correct DOS mask}
     if (CharCount('.', Mask)<2) and
        (CharCount('*', Mask)<3) then begin
      k:=PosChar('.', Mask);
      if k=0 then k:=Byte(Mask[0]);
      k2:=PosChar('*', Mask);
      if k2>0 then k3:=PosChar('*', Copy(Mask, k2+1, 255)) else k3:=0;
      if k3>0 then inc(k3, k2);
      if ((k2=0) or
          (k2=Byte(Mask[0])) or
          (k2=k-1)
         ) and
         ((k3=0) or
          (k3=Byte(Mask[0]))
         ) then begin
                 PM := Mask;
                 PackMask := False;
                end
           else goto mc;
     end else begin
mc:   {Place mask comressing here, but do not touch NormMask variable}
      goto fc; {Here is temporary code}
     end;
    end else begin
     PM := Mask;
     PackMask := False;
    end;
   end else begin
fc: {Place filter comressing here, but do not touch NormMask variable}
    PM:=x_x;
   end;
  end;
 end;
{-DataCompBoy-}

        {-DataCompBoy-}
procedure FileChanged(const Name: String);
 var Dr: String;
     Nm: String;
     Xt: String;
begin
 lFSplit(Name, Dr, Nm, Xt); Abort := False;
 GlobalMessage(evCommand, cmRereadDir, @Dr);
 GlobalMessage(evCommand, cmRereadInfo, nil);
 GlobalMessage(evCommand, cmRereadTree, @Dr);
end;
        {-DataCompBoy-}

function CompareFiles(const N1, N2: String): Boolean;
  label Finish;
  const BufSize = 2048;
  var
      I: LongInt;
      B1, B2: Pointer;
      B: Boolean;
      S1, S2: TDOSStream;
begin
  CompareFiles := False;
  B := False;
  B1 := nil; B2 := nil;
  S1.Init(N1, stOpenRead);
  if S1.Status <> stOK then begin S1.Done; Exit end;
  S2.Init(N2, stOpenRead);
  if (S2.Status <> stOK) or (S1.GetSize <> S2.GetSize) then Goto Finish;
  B1 := MemAlloc(BufSize); if B1 = nil then Goto Finish;
  B2 := MemAlloc(BufSize); if B2 = nil then Goto Finish;
  I := BufSize;
  CompareFiles := True;
  while (S1.Status = stOK) and (S2.Status = stOK) and (I > 0) and not B do
    begin
      I := BufSize;
      if S1.GetSize - S1.GetPos < I then I := S1.GetSize - S1.GetPos;
      if I = 0 then Break;
      S1.Read(B1^, I);
      S2.Read(B2^, I);
      {$IFNDEF BIT_32}
      asm
        push ds
        les  di, B1
        lds  si, B2
        cld
        mov  cx, word ptr I
        rep  cmpsb
        pop  ds
        jz   @1
        xor  al, al
        mov  @Result, al
        inc  al
        mov  B, al
       @1:
      end;
      {$ELSE BIT_32}
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
      {$ENDIF}
    end;
  if not B then CompareFiles := (S1.Status = stOK) and (S2.Status = stOK);
Finish:
  if B1 <> nil then FreeMem(B1, BufSize);
  if B2 <> nil then FreeMem(B2, BufSize);
  S1.Done;
  S2.Done;
end;

function GetRoot(const S: String): string;
var i, c: byte;
begin
  if Not ((word((@S[1])^)=$2F2F{//}) or (word((@S[1])^)=$5C5C))
    then GetRoot:=Copy(S, 1, 3)
    else begin
      c:=4; i:=0;
      while (i<length(S)) and (c>0) do begin
        inc(i);
        if S[i] in ['\', '/'] then dec(c);
      end;
      GetRoot:=Copy(S, 1, i);
    end;
end;

function  GetCurrentDir: string;
var P: PView;
    S: String;
begin
 P:=Message( Application, evBroadcast, cmLookForPanels, NIL );
 if P=nil then lGetDir(0, S)
 else Message(P, evBroadCast, cmGetDirName, @S);

 GetCurrentDir:=S;
end;

function  lFExpand(const S: string): string;
var DD: String;
begin
  if (length(S) < 2) or ((S[2] <> ':') and (Copy(S, 1, 2) <> '\\')){piwamoto}
  then begin
    DD:=GetCurrentDir;
    if (DD[1]='\') then begin
      lFExpand:=MakeNormName(DD, S);
      exit
    end;
  end;
  lFExpand:=LFN.lFExpand(S);
end;

end.
