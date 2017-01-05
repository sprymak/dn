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
{DataCompBoy = Anton Fedorov, 2:5000/111.33@fidonet}
{JO = Jaroslaw Osadtchiy, 2:5030/1082.53@fidonet}
{AK155 = Alexey Korop, 2:461/155@fidonet}
{Cat = Aleksej Kozlov, 2:5030/1326.13@fidonet}
{Interface part from LFN.PAS}
{$AlignRec-}

{Cat
   05/12/2001 - попытка бороться с виндозной глюкофичей: запоминается текущий
   каталог не для всех дисков, а только для текущего диска, что приводит к
   различным мелким неприятностям; чтобы это пофиксить, при lChDir сохраняем
   устанавливаемый путь в массиве, а при lGetDir - извлекаем оттуда
}

unit LFNVP;

interface

uses
  {$IFDEF VIRTUALPASCAL} VpSysLow, VpSysLo2, {$ENDIF}
  Dos, DnINI;

type

  TSize = Comp; {64 bit integer type for file sizes}

 {SearchRecNew = TOSSearchRecNew;}{Cat: непонятно, зачем тут перечислять все поля ещё   }
                                  {     раз, а глюки из этого могут возникнуть запросто }
(*
  SearchRecNew = record        {JO: расширенный аналог SearchRec из Dos с        }
    Handle: Longint;           {    некоторыми дополнительными полями,           }
    Filler1: Longint;          {    в настоящий момент ShortName в Win32 версии, }
    Attr: Byte;                {    время создания и последнего доступа          }
    Time: Longint;
    Size: Longint;
    Name: ShortString;
    CreationTime: Longint;
    LastAccessTime: Longint;
    Filler2: array[0..3] of Char;
{$IFDEF WIN32}
    ShortName: ShortString;
    Filler3: array[0..321] of Char;
{$ENDIF}
{$IFDEF DPMI32}
    Private_data: array[1..sizeof(TOSSearchRec)-4-4-1-4-4-256-4] of Byte;
{$ENDIF}
{$IFDEF LINUX}
    Pattern: ShortString;
    FileMode: LongInt;
    Directory: ShortString;
{$ENDIF}
  end;
*)

   {Extended search structure to be used instead of SearchRec}
  lSearchRec = record
    SR: TOSSearchRecNew;   {Basic field set}
   {FileHandle: Word;}  {Search handle, undefined in lDOS mode}
    FullSize: TSize; {True file size}
(*  LoCreationTime: Longint; {Time created (low-order byte)}
    HiCreationTime: Longint; {Time created (high-order byte)}
    LoLastAccessTime: Longint; {Time accessed (low-order byte)}
    HiLastAccessTime: Longint; {Time accessed (high-order byte)}
    LoLastModificationTime: Longint; {Time modified (low-order byte)}
    HiLastModificationTime: Longint; {Time modified (high-order byte)} *)
    FullName: string;  {True file name or short name if LFNs not available}
   {$IFDEF OS2}
    PrevName: string;
   {$ENDIF}
    { Other fields will be added later }
  end;

  TNameZ = Array[0..259] of Char;
  {The type for file names. (String is suitable for most purpuses)}

  lFile = record {Extended file record to be used instead of File}
    F: File;
    { Other fields will be added later }
  end;

  lText = record {Extended text file record to be used instead of Text}
    T: Text;
    { Other fields will be added later }
  end;

        {   Basic parameters   }
const

(*
  ltMod = 0;    {Store time modified}
  ltAcc = 1;    {Store time accessed}
  ltCre = 2;    {Store time created}
  LFNTimes: Byte = ltMod; { What time info to store in lSearchRec.SR.Time? }
*)
  MaxPathLen: Byte = 255;  { Maximum name length for the present moment }

  Const IllegalChars:String[15] = '<>|:'; { Characters invalid for short names }
      IllegalCharSet:Set Of Char = ['<','>','|',':']; { Characters invalid for short names }

  Const IllegalCharsDos:String[15] = ';,=+<>|"[]'; { Characters invalid for DOS names }
      IllegalCharSetDos:Set Of Char = [';',',','=','+','<','>','|','"','[',']']; { Characters invalid for DOS names }

       { File searching routines. lFindClose must be called /in all cases/ }
procedure lFindFirst(const Path: String; Attr: Word; var R: lSearchRec);
procedure lFindNext(var R: lSearchRec);
procedure lFindClose(var R: lSearchRec);

{$IFDEF WIN32}
        { Name retrieval functions }
function lfGetShortFileName(const Name: String): String;
{$ENDIF}
{$IFDEF DPMI32}
function lfGetShortFileName(const Name: String): String;
{$ENDIF}
{$IFDEF OS_DOS}
function lfGetLongFileName(const Name: String): String;
{$ENDIF}

        { Name correction routine }
procedure lTrueName(const Name: String; var S: String);


   { Basic file operation routines. To use IO functions from standard units,
              specify lFile.F or lText.T }

procedure lAssignFile(var F: lFile; const Name: String);
procedure lAssignText(var T: lText; const Name: String);
procedure lResetFile(var F: lFile; RecSize: Word);
procedure lResetFileReadOnly(var F: lFile; RecSize: Word);
procedure lRewriteFile(var F: lFile; RecSize: Word);
procedure lResetText(var F: lText); inline; begin Reset(F.T) end;
procedure lResetTextReadOnly(var F: lText);
procedure lRewriteText(var F: lText);
procedure lAppendText(var T: lText); inline; begin Append(T.T); end;
procedure lEraseFile(var F: lFile); inline; begin Erase(F.F); end;
procedure lEraseText(var T: lText); inline; begin Erase(T.T); end;
procedure lRenameFile(var F: lFile; const NewName: String);
procedure lRenameText(var T: lText; const NewName: String);
procedure lChangeFileName(const Name, NewName: String);
function  lFileNameOf(var lF: lFile): string;
function  lTextNameOf(var lT: lText): string;

                 { File attributes manipulation }
procedure lGetFAttr(var F: lFile; var Attr: Word);
procedure lSetFAttr(var F: lFile; Attr: Word);
procedure lGetTAttr(var T: lText; var Attr: Word);
procedure lSetTAttr(var T: lText; Attr: Word);

                           { Directory manipulation }
procedure lMkDir(const Path: String);
procedure lRmDir(const Path: String);
procedure lChDir(const Path: String);
procedure lGetDir(D: Byte; var Path: String);

         { Name expansion and splitting }
function lFExpand(const Path: String): String;
procedure lFSplit(const Path: String; var Dir, Name, Ext: String);

{$IFDEF WIN32}
var
  OemToCharArray: array[Char] of Char;
  CharToOemArray: array[Char] of Char;

procedure OemToCharSt(var OemS: String); {JO}
procedure CharToOemSt(var CharS: String);{JO}
function OemToCharStr(OemS: String): String;
function CharToOemStr(CharS: String): String;
{$ENDIF}

implementation

uses
  {$IFDEF WIN32} Windows, {$ENDIF}
  Strings, Commands{Cat};

{$IFDEF WIN32}
const
  NoShortName: string[12] = #22#22#22#22#22#22#22#22'.'#22#22#22;
{JO, AK155: зачем нужны эти #22:
  В виндах функции API FindFileFirst и FindFileNext отдают два имени
файла: основное и альтернативное (короткое).
  Под НТ возможна ненормальная (с моей точки зрения,
по крайней мере) ситуация, когда для файла с длинным (не укладывающимся
в 8.3) имением файла альтернатвное имя недоступно. Так бывает, если
файловая система в принципе не поддерживает двухименности (HPFS), или
если формирование коротких имен отключено (на NTFS и FAT32).
  Тогда в режиме показа коротких имён мы будем иметь для таких файлов
обрезанные как попало имена в панели, не соответствующие
действительности. Для этого JO и придумал этот условный заменитель
недоступного короткого имени, так как символ #22 заведомо не
может быть в принципе в реальном имени файла. Так показывать в
режиме коротких имён файлы, для которых доступно только длинное имя, -
честнее, чем с обрезанным именем, под которым файл недоступен.
}
{$ENDIF}

 Function StrPas_(S: Array Of Char): String;
  var ss: string;
      i: word;
  Begin
   ss:='';
   For i:=Low(S) to High(S) do
    If (i<255) and (s[i]<>#0)
     then ss:=ss+s[i]
     else break;
   StrPas_:=ss;
  End;

{$IFNDEF OS2}
procedure NameToNameZ(const Name: String; var NameZ: TNameZ);
begin
  Move(Name[1], NameZ, Length(Name));
  NameZ[Length(Name)] := #0;
end;
{$ENDIF}

{$IFDEF Win32}
{JO}
procedure OemToCharSt(var OemS: String);
 var I: Byte;
begin
  for I := 1 to Length(OemS) do
    OemS[I] := OemToCharArray[OemS[I]];
end;

procedure CharToOemSt(var CharS: String);
 var I: Byte;
begin
  for I := 1 to Length(CharS) do
    CharS[I] := CharToOemArray[CharS[I]];
end;

function OemToCharStr(OemS: String): String;
{var l: integer;
begin
l := length(OemS);
Setlength(Result, l);
OemToCharBuff(@OemS[1], @Result[1], l);
end;}
begin
 OemToCharSt(OemS);
 OemToCharStr := OemS;
end;

function CharToOemStr(CharS: String): String;
{var l: integer;
begin
l := length(CharS);
Setlength(Result, l);
CharToOemBuff(@CharS[1], @Result[1], l);
end;}
begin
 CharToOemSt(CharS);
 CharToOemStr := CharS;
end;
{/JO}
{$ENDIF}

procedure CheckColonAndSlash(const Name: String; var S: String);
var
  ColonPos: Integer;
begin
  ColonPos := Pos(':', S);
  if (ColonPos > 2) and (Name[2] = ':') then
  begin
    Delete(S, 1, ColonPos - 1);
    S := Name[1] + S;
  end;

  if Name[Length(Name)] <> '\' then
    while S[Length(S)] = '\' do SetLength(S, Length(S)-1)
  else if (Name[Length(Name)] = '\') and
    (S[Length(S)] <> '\') and (Length(S) < 255) then
  begin
    SetLength(S, Length(S)+1);
    S[Length(S)] := '\';
  end;
end;

(*
 Offset  Size    Description
  00h    DWORD   file attributes
                 bits 0-6 standard DOS attributes
                 bit 8: temporary file
  04h    QWORD   file creation time
                 (number of 100ns intervals since 1/1/1601)
  0Ch    QWORD   last access time
  14h    QWORD   last modification time
  1Ch    DWORD   file size (high 32 bits)
  20h    DWORD   file size (low 32 bits)
  24h  8 BYTEs   reserved
  2Ch 260 BYTEs  ASCIZ full filename
 130h 14 BYTEs   ASCIZ short filename (for backward compatibility)
*)

function SetDosError(ErrCode: Integer): Integer;
begin
  DosError := ErrCode;
  SetDosError := ErrCode;
end;

{JO: функции, аналогичные FindFirst, FindNext и FindClose из модуля DOS, }
{    но предназначенные для работы с SearchRecNew}
(*
procedure FindFirstNew(const Path: PathStr; Attr: Word; var F: SearchRecNew);
var
  PathBuf: array [0..SizeOf(PathStr)-1] of Char;
begin
  SetDosError(SysFindFirstNew(StrPCopy(PathBuf, Path), Attr, TOSSearchRecNew(F), False));
end;

procedure FindNextNew(var F: SearchRecNew);
begin
  SetDosError(SysFindNextNew(TOSSearchRecNew(F), False));
end;

procedure FindCloseNew(var F: SearchRecNew);
begin
  SetDosError(SysFindCloseNew(TOSSearchRecNew(F)));
end;
*)

{AK155}
{$IFNDEF OS2}
function NotShortName(const S: string): boolean;
  var
    i, l: integer;
    iPoint: integer;
  begin
  NotShortName := true;
  if S[1] = '.' then exit;
  l := length(S);
  if l > 12 then exit;
  iPoint := 0;
  for I := 1 to l do
   begin
    if S[i] = '.' then
     begin
      if (iPoint <> 0) or (I>9) then exit;
      iPoint := i;
     end
    else if S[I] in IllegalCharSet then Exit; {DataCompBoy}
   end;
  if (iPoint = 0) and (l > 8) then exit;
  if (iPoint <> 0) and (l-iPoint > 3) then exit;
  NotShortName := false;
  end;
{$ENDIF}

procedure CorrectSearchRec(var R: lSearchRec);
begin
  R.FullName := R.SR.Name;
{$IFDEF Win32}
  if (R.SR.Name <> '.') and (R.SR.Name <> '..') then
    begin
    if (R.SR.ShortName <> '') then
      R.SR.Name := R.SR.ShortName
    else if NotShortName(R.FullName) then
      R.SR.Name := NoShortName;
    end;
{$ENDIF}
(*R.LoCreationTime:= R.SR.Time;
  R.HiCreationTime:= 0;
  R.LoLastAccessTime:= R.SR.Time;
  R.HiLastAccessTime:= 0;
  R.LoLastModificationTime:= R.SR.Time;
  R.HiLastModificationTime:= 0; *)
  R.FullSize:=R.SR.Size;
end;

procedure lFindFirst(const Path: String; Attr: Word; var R: lSearchRec);
var
  PathBuf: array [0..SizeOf(PathStr)-1] of Char;
begin
  R.FullName := '';
  SetDosError(SysFindFirstNew(StrPCopy(PathBuf, Path), Attr, R.SR, False));
  CorrectSearchRec(R);
{$IFDEF OS2}
  R.PrevName := R.FullName;
{$ENDIF}
end;

procedure lFindNext(var R: lSearchRec);
begin
  R.FullName := '';
  SetDosError(SysFindNextNew(R.SR, False));
  CorrectSearchRec(R);
 {JO: ошибка 49 в оси зарезервирована; мы её будем использовать для}
 {    отлова дупов на HPFS}
{$IFDEF OS2}
  if (DOSError = 0) and (R.FullName <> '') and (R.FullName <> '.') and (R.FullName <> '..') then
    begin
      if R.PrevName = R.FullName then DOSError := 49;
      R.PrevName := R.FullName;
    end;
{$ENDIF}
end;

procedure lFindClose(var R: lSearchRec);
 var DEr: Longint;
begin
 DEr := DOSError; {JO}
 SysFindCloseNew(R.SR);
 DOSError := DEr; {JO}
end;


{$IFDEF WIN32}
function lfGetShortFileName(const Name: String): String;
var NZ, NZ2: TNameZ;
  l: longint;
begin
  if (Name = '.') or (Name = '..') then begin lfGetShortFileName := Name; Exit; end;
  NameToNameZ(Name, NZ2);
  l := GetShortPathName(@NZ2, @NZ, SizeOf(NZ));
  if l = 0 then lfGetShortFileName := NoShortName
    else lfGetShortFileName := StrPas_(NZ);
end;
{$ENDIF}
{$IFDEF DPMI32}
function lfGetShortFileName(const Name: String): String;
begin
  lfGetShortFileName:=Name; {Cat:todo DPMI32}
end;
{$ENDIF}
{$IFDEF OS_DOS}
function lfGetLongFileName(const Name: String): String;
begin
 lfGetLongFileName:=Name;
end;
{$ENDIF}


procedure lTrueName(const Name: String; var S: String);
 begin S:=FExpand(Name); end;

procedure lFSplit(const Path: String; var Dir, Name, Ext: String);
var
  HasColon: Boolean;
  DotPos, SlashPos, B: Byte;
  D:String;
  N:String;
  E:String;
begin
      begin
        Dir := '';
        Name := '';
        Ext := '';
        DotPos := 0;
        SlashPos := 0;
        HasColon := (Length(Path) > 1) and (Path[2] = ':');

        for B := Length(Path) downto 1 do
        begin
          if (Path[B] = '.') and (DotPos = 0) {and (B>1)} then DotPos := B; {JO: имена могут состоять только из расширения}
          if ((Path[B] = ':') or (Path[B] = '\')) and (SlashPos = 0) and
             ((B > 2) or not HasColon) then SlashPos := B;
          if (DotPos <> 0) and (SlashPos <> 0) then Break;
        end;

        if DotPos + SlashPos = 0 then
          if HasColon then Dir := Path
          else Name := Path
        else
        begin
          if DotPos > SlashPos then Ext := Copy(Path, DotPos, MaxStringLength)
          else DotPos := 255;

          if SlashPos <> 0 then Dir := Copy(Path, 1, SlashPos);

          Name := Copy(Path, SlashPos + 1, DotPos - SlashPos - 1);
        end;
      end;
end;

function lFileNameOf(var lF: lFile): string;
begin
  lFileNameOf := StrPas_(FileRec(lF.F).Name);
end;

function lTextNameOf(var lT: lText): string;
begin
  lTextNameOf := StrPas_(TextRec(lT.T).Name);
end;

procedure lResetFileReadOnly(var F: lFile; RecSize: Word);
var SaveMode: Byte;
begin SaveMode := FileMode;
      FileMode := 64;
      lResetFile(F, RecSize);
      FileMode := SaveMode;
end;

procedure lRewriteFile(var F: lFile; RecSize: Word);
var OldMode: byte;
begin OldMode:= FileMode;
      FileMode:=FileMode and $FC or 2;
      Rewrite(F.F, RecSize);
      FileMode:=OldMode;
end;

procedure lResetTextReadOnly(var F: lText);
var
  SaveMode: Byte;
begin
 SaveMode := FileMode;
 FileMode := 64;
 lResetText(F);
 FileMode := SaveMode;
end;

procedure lRewriteText(var F: lText);
var OldMode: byte;
begin OldMode:= FileMode;
      FileMode:=FileMode and $FC or 2;
      Rewrite(F.T);
      FileMode:=OldMode;
end;

{ Inline functions, which temporary compiled as not inline, because VP are   }
{ crased on compiling 8(                                                     }

procedure lAssignFile(var F: lFile; const Name: String); begin Assign(F.F, Name); end;
procedure lAssignText(var T: lText; const Name: String); begin Assign(T.T, Name); end;
procedure lResetFile(var F: lFile; RecSize: Word); begin Reset(F.F, RecSize); end;
procedure lRenameFile(var F: lFile; const NewName: String); begin Rename(F.F, NewName); end;
procedure lRenameText(var T: lText; const NewName: String); begin Rename(T.T, NewName); end;
procedure lChangeFileName(const Name, NewName: String);
  var F: File;
 begin
  {$IFDEF Win32}
   {$IFNDEF RecodeWhenDraw}
  if RecodeCyrillicNames = 1 then
    begin
      Assign(F, OemToCharStr(Name));
      Rename(F, OemToCharStr(NewName));
    end
   else
    begin
   {$ENDIF}
  {$ENDIF}
     Assign(F, Name);
     Rename(F, NewName);
  {$IFDEF Win32}
   {$IFNDEF RecodeWhenDraw}
    end;
   {$ENDIF}
  {$ENDIF}
 end;
procedure lGetFAttr(var F: lFile; var Attr: Word); begin DOS.GetFAttr(F.F, Attr); end;
procedure lSetFAttr(var F: lFile; Attr: Word);     begin DOS.SetFAttr(F.F, Attr); end;
procedure lGetTAttr(var T: lText; var Attr: Word); begin DOS.GetFAttr(T.T, Attr); end;
procedure lSetTAttr(var T: lText; Attr: Word);     begin DOS.SetFAttr(T.T, Attr); end;
procedure lMkDir(const Path: String); begin MkDir(Path); end;

{AK155: В DN/2, если каталог имеет атрибут ReadOnly, то на FAT или HPFS
он удаляется нормально, а на FAT32 - не удаляется. Так что на всякий
случай надо ReadOnly снять. }
procedure lRmDir(const Path: String);
  var
    f: file;
    Attr: word;
  begin
  assign(f, Path); Dos.GetFAttr(f, Attr);
  if Attr and ReadOnly <> 0 then
    Dos.SetFAttr(f, Attr and not ReadOnly);
  if DosError <> 0 then
    begin InOutRes := DosError; exit; end;
  RmDir(Path);
  end;
{/AK155}

{Cat: Windows запоминает текущий каталог только для текущего диска;
запоминание остальных текущих каталогов приходится брать на себя}
{$IFDEF WIN32}
var
  CurrentPaths: array[1..1+Byte('Z')-Byte('A')] of PathStr;
{$ENDIF}

function lFExpand(const Path: String): String;
var
  D: Byte;
begin
{$IFDEF WIN32}
  if (Length(Path) = 2) and (Path[2] = ':') then
    begin
      D := Byte(UpCase(Path[1]))-Byte('A')+1;
      if CurrentPaths[D] = '' then
        lFExpand := Path[1]+':\' {GetDir(D, Path)}
      else
        lFExpand := CurrentPaths[D]
    end
  else
{$ENDIF}
    lFExpand := FExpand(Path);
end;

procedure lChDir(const Path: String);
begin
  ChDir(Path);
{$IFDEF WIN32}
  if (InOutRes = 0) and (Length(Path) > 2) and (Path[2] = ':') then
    CurrentPaths[Byte(UpCase(Path[1]))-Byte('A')+1] := Path;
{$ENDIF}
end;

procedure lGetDir(D: Byte; var Path: String);
begin
{$IFDEF WIN32}
  if D = 0 then
    begin
      GetDir(0, Path);
      if Path[1] = '\' then
        Exit;
      D := Byte(Upcase(Path[1]))-Byte('A')+1;
    end;
  if CurrentPaths[D] = '' then
    Path := Char(D+Byte('A')-1)+':\' {GetDir(D, Path)}
  else
    Path := CurrentPaths[D];
{$ELSE}
  GetDir(D, Path);
{$ENDIF}
end;
{/Cat}

{JO}
{$IFDEF Win32}
procedure PrepareArrays;
 var I: Char;
 begin
  for I := #0 to #255 do
   begin
    OemToCharArray[I] := I;
    CharToOemArray[I] := I;
   end;
  OemToCharBuff(@OemToCharArray, @OemToCharArray, 256);
  CharToOemBuff(@CharToOemArray, @CharToOemArray, 256);
 end;

Begin {инициализация модуля}
 PrepareArrays;
{$ENDIF}
{/JO}
End.
