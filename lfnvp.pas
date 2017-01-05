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
   05/12/2001 - ����⪠ ������� � ��������� ���祩: ������������ ⥪�騩
   ��⠫�� �� ��� ��� ��᪮�, � ⮫쪮 ��� ⥪�饣� ��᪠, �� �ਢ���� �
   ࠧ���� ������ �����⭮���; �⮡� �� ��䨪���, �� lChDir ��࠭塞
   ��⠭��������� ���� � ���ᨢ�, � �� lGetDir - ��������� ���㤠
}

unit LFNVP;

interface

uses
  VpSysLow, VPSysLo2,
  Dos, DnIni
  ;

type

  TSize = Comp; {64 bit integer type for file sizes}

  {Extended search structure to be used instead of SearchRec}
  lSearchRec = record
    SR: TOSSearchRecNew; {Basic field set}
    {FileHandle: Word;} {Search handle, undefined in lDOS mode}
    FullSize: TSize; {True file size}
    (*  LoCreationTime: Longint; {Time created (low-order byte)}
    HiCreationTime: Longint; {Time created (high-order byte)}
    LoLastAccessTime: Longint; {Time accessed (low-order byte)}
    HiLastAccessTime: Longint; {Time accessed (high-order byte)}
    LoLastModificationTime: Longint; {Time modified (low-order byte)}
    HiLastModificationTime: Longint; {Time modified (high-order byte)} *)
    FullName: String;
    {True file name or short name if LFNs not available}
    {$IFDEF OS2}
    PrevName: String;
    {$ENDIF}
    { Other fields will be added later }
    end;

  TNameZ = array[0..259] of Char;
  {The type for file names. (String is suitable for most purpuses)}

  lFile = record
    {Extended file record to be used instead of File}
    F: file;
    { Other fields will be added later }
    end;

  lText = record
    {Extended text file record to be used instead of Text}
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
  MaxPathLen: Byte = 255; { Maximum name length for the present moment }

const
  IllegalChars: String[15] = '<>|:';
  { Characters invalid for short names }
  IllegalCharSet: set of Char = ['<', '>', '|', ':'];
  { Characters invalid for short names }

const
  IllegalCharsDos: String[15] = ';,=+<>|"[]';
  { Characters invalid for DOS names }
  IllegalCharSetDos: set of Char = [';', ',', '=', '+', '<', '>', '|',
   '"', '[', ']']; { Characters invalid for DOS names }

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

{AK155 22-11-2003 ���� ����� ��� � ���. 0 - �᫨ �� �� UNC-����}
function GetShareEnd(const S: String): Integer;

{ Basic file operation routines. To use IO functions from standard units,
              specify lFile.F or lText.T }

procedure lAssignFile(var F: lFile; const Name: String);
procedure lAssignText(var T: lText; const Name: String);
procedure lResetFile(var F: lFile; RecSize: Word);
procedure lResetFileReadOnly(var F: lFile; RecSize: Word);
procedure lReWriteFile(var F: lFile; RecSize: Word);
procedure lResetText(var F: lText);
  inline;
  begin
  Reset(F.T)
  end;
procedure lResetTextReadOnly(var F: lText);
procedure lRewriteText(var F: lText);
procedure lAppendText(var T: lText);
  inline;
  begin
  Append(T.T);
  end;
procedure lEraseFile(var F: lFile);
  inline;
  begin
  Erase(F.F);
  end;
procedure lEraseText(var T: lText);
  inline;
  begin
  Erase(T.T);
  end;
procedure lRenameFile(var F: lFile; const NewName: String);
procedure lRenameText(var T: lText; const NewName: String);
procedure lChangeFileName(const Name, NewName: String);
function lFileNameOf(var lF: lFile): String;
function lTextNameOf(var lT: lText): String;

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
procedure lFSplit(const Path: String; var Dir, Name, ext: String);

{$IFDEF WIN32}
const
  NoShortName: String[12] = #22#22#22#22#22#22#22#22'.'#22#22#22;
  {JO, AK155: ��祬 �㦭� �� #22:
  � ������ �㭪樨 API FindFileFirst � FindFileNext �⤠�� ��� �����
䠩��: �᭮���� � ����ୠ⨢��� (���⪮�).
  ��� �� �������� ����ଠ�쭠� (� ���� �窨 �७��,
�� �ࠩ��� ���) �����, ����� ��� 䠩�� � ������ (�� 㪫��뢠�騬��
� 8.3) ������� 䠩�� ����ୠ⢭�� ��� ������㯭�. ��� �뢠��, �᫨
䠩����� ��⥬� � �ਭ樯� �� �����ন���� ���娬������ (HPFS), ���
�᫨ �ନ஢���� ���⪨� ���� �⪫�祭� (�� NTFS � FAT32).
  ����� � ०��� ������ ���⪨� ��� �� �㤥� ����� ��� ⠪�� 䠩���
��१���� ��� ������ ����� � ������, �� ᮮ⢥�����騥
����⢨⥫쭮��. ��� �⮣� JO � �ਤ㬠� ��� �᫮��� ������⥫�
������㯭��� ���⪮�� �����, ⠪ ��� ᨬ��� #22 �������� ��
����� ���� � �ਭ樯� � ॠ�쭮� ����� 䠩��. ��� �����뢠�� �
०��� ���⪨� ��� 䠩��, ��� ������ ����㯭� ⮫쪮 ������� ���, -
��⭥�, 祬 � ��१���� ������, ��� ����� 䠩� ������㯥�.
}
  {$ENDIF}

implementation

uses
  {$IFDEF WIN32}Windows, {$ENDIF}
  Strings, Commands {Cat}
  ;

function StrPas_(S: array of Char): String;
  var
    ss: String;
    i: Word;
  begin
  ss := '';
  for i := Low(S) to High(S) do
    if  (i < 255) and (S[i] <> #0)
    then
      ss := ss+S[i]
    else
      Break;
  StrPas_ := ss;
  end;

{$IFNDEF OS2}
procedure NameToNameZ(const Name: String; var NameZ: TNameZ);
  begin
  Move(Name[1], NameZ, Length(Name));
  NameZ[Length(Name)] := #0;
  end;
{$ENDIF}

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

{AK155}
{$IFNDEF OS2}
function NotShortName(const S: String): Boolean;
  var
    i, l: Integer;
    iPoint: Integer;
  begin
  NotShortName := True;
  if S[1] = '.' then
    Exit;
  l := Length(S);
  if l > 12 then
    Exit;
  iPoint := 0;
  for i := 1 to l do
    begin
    if S[i] = '.' then
      begin
      if  (iPoint <> 0) or (i > 9) then
        Exit;
      iPoint := i;
      end
    else if S[i] in IllegalCharSet then
      Exit; {DataCompBoy}
    end;
  if  (iPoint = 0) and (l > 8) then
    Exit;
  if  (iPoint <> 0) and (l-iPoint > 3) then
    Exit;
  NotShortName := False;
  end { NotShortName };
{$ENDIF}

procedure CorrectSearchRec(var R: lSearchRec);
  begin
  R.FullName := R.SR.Name;
  {$IFDEF Win32}
  if  (R.SR.Name <> '.') and (R.SR.Name <> '..') then
    begin
    if  (R.SR.ShortName <> '') then
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
  R.FullSize := R.SR.Size;
  end;

procedure lFindFirst(const Path: String; Attr: Word; var R: lSearchRec);
  var
    PathBuf: array[0..SizeOf(PathStr)-1] of Char;
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
  {JO: �訡�� 49 � �� ��१�ࢨ஢���; �� �� �㤥� �ᯮ�짮���� ���}
  {    �⫮�� �㯮� �� HPFS}
  {$IFDEF OS2}
  if  (DosError = 0) and (R.FullName <> '') and (R.FullName <> '.')
       and (R.FullName <> '..')
  then
    begin
    if R.PrevName = R.FullName then
      DosError := 49;
    R.PrevName := R.FullName;
    end;
  {$ENDIF}
  end;

procedure lFindClose(var R: lSearchRec);
  var
    DEr: LongInt;
  begin
  DEr := DosError; {JO}
  SysFindCloseNew(R.SR);
  DosError := DEr; {JO}
  end;

{$IFDEF WIN32}
function lfGetShortFileName(const Name: String): String;
  var
    NZ, NZ2: TNameZ;
    l: LongInt;
  begin
  if  (Name = '.') or (Name = '..') then
    begin
    lfGetShortFileName := Name;
    Exit;
    end;
  NameToNameZ(Name, NZ2);
  if SysPlatformId = 1 then
    OemToChar(@NZ2, @NZ2);
  {AK155 18.07.2003 ��� � ���� ��室���� ��ࠫ��� ��� Win9x,
      � ������ GetShortPathName ࠡ�⠥� � ����஢�� ANSI ��ᬮ��� ��
      SetFileApisToOEM
    }
  l := GetShortPathName(@NZ2, @NZ, SizeOf(NZ));
  if l = 0 then
    lfGetShortFileName := NoShortName
  else
    begin
    if SysPlatformId = 1 then
      CharToOEM(@NZ, @NZ);
    lfGetShortFileName := StrPas_(NZ);
    end;
  end { lfGetShortFileName };
{$ENDIF}
{$IFDEF DPMI32}
function lfGetShortFileName(const Name: String): String;
  begin
  lfGetShortFileName := Name; {Cat:todo DPMI32}
  end;
{$ENDIF}
{$IFDEF OS_DOS}
function lfGetLongFileName(const Name: String): String;
  begin
  lfGetLongFileName := Name;
  end;
{$ENDIF}

procedure lTrueName(const Name: String; var S: String);
  begin
  S := FExpand(Name);
  end;

{AK155 22-11-2003 ���� ����� ��� � ���. 0 - �᫨ �� �� UNC-����
}
function GetShareEnd(const S: String): Integer;
  var
    SlashFound: Boolean;
  begin
  Result := 0;
  if Copy(S, 1, 2) <> '\\' then
    Exit;
  { �饬 '\' ��᫥ '\\', � ����� �� ���� ��� �� ��ண� '\' }
  Result := 3;
  SlashFound := False;
  while Result < Length(S) do
    begin
    if S[Result+1] = '\' then
      begin
      if SlashFound then
        Exit; // �ᯥ�. ����� Copy(S, 1, i) - �� '\\server\share'
      SlashFound := True;
      end;
    Inc(Result);
    end;
  if not SlashFound then
    Result := 0;
  { ���ࠢ���� �� ����: '\\' � ��砫� ����,
      � '\' ��⮬ - ���. ���� �� ���-� �ਧ��� �訡�� ���⠢���,
      �� ������⭮ ��� � ��� ����. }
  end { GetShareEnd };
{/AK155 22-11-2003}

{AK155 22-11-2003 ��ࠡ�⠭� � ���⮬ UNC-��⥩ }
procedure lFSplit(const Path: String; var Dir, Name, ext: String);
  var
    DriveEnd: Integer;
    DotPos, SlashPos, B: Byte;
    D: String;
    N: String;
    E: String;
  begin
  begin
  Dir := '';
  Name := '';
  ext := '';
  DotPos := 0;
  SlashPos := 0;
  if  (Length(Path) > 1) and (Path[2] = ':') then
    DriveEnd := 2
  else
    DriveEnd := GetShareEnd(Path);

  for B := Length(Path) downto DriveEnd+1 do
    begin
    if  (Path[B] = '.') and (DotPos = 0) then
      begin
      DotPos := B; {JO: ����� ����� ������ ⮫쪮 �� ���७��}
      if SlashPos <> 0 then
        Break;
      end;
    if  (Path[B] = '\') and (SlashPos = 0) then
      begin
      SlashPos := B;
      if DotPos <> 0 then
        Break;
      end;
    end;

  if DotPos+SlashPos = 0 then
    if DriveEnd <> 0 then
      Dir := Path
    else
      Name := Path
  else
    begin
    if DotPos > SlashPos then
      ext := Copy(Path, DotPos, MaxStringLength)
    else
      DotPos := 255;

    if SlashPos <> 0 then
      Dir := Copy(Path, 1, SlashPos);

    Name := Copy(Path, SlashPos+1, DotPos-SlashPos-1);
    end;
  end;
  end { lFSplit };

function lFileNameOf(var lF: lFile): String;
  begin
  lFileNameOf := StrPas_(FileRec(lF.F).Name);
  end;

function lTextNameOf(var lT: lText): String;
  begin
  lTextNameOf := StrPas_(TextRec(lT.T).Name);
  end;

procedure lResetFileReadOnly(var F: lFile; RecSize: Word);
  var
    SaveMode: Byte;
  begin
  SaveMode := FileMode;
  FileMode := 64;
  lResetFile(F, RecSize);
  FileMode := SaveMode;
  end;

procedure lReWriteFile(var F: lFile; RecSize: Word);
  var
    OldMode: Byte;
  begin
  OldMode := FileMode;
  FileMode := FileMode and $FC or 2;
  Rewrite(F.F, RecSize);
  FileMode := OldMode;
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
  var
    OldMode: Byte;
  begin
  OldMode := FileMode;
  FileMode := FileMode and $FC or 2;
  Rewrite(F.T);
  FileMode := OldMode;
  end;

{ Inline functions, which temporary compiled as not inline, because VP are   }
{ crased on compiling 8(                                                     }

procedure lAssignFile(var F: lFile; const Name: String);
  begin
  Assign(F.F, Name);
  end;
procedure lAssignText(var T: lText; const Name: String);
  begin
  Assign(T.T, Name);
  end;
procedure lResetFile(var F: lFile; RecSize: Word);
  begin
  Reset(F.F, RecSize);
  end;
procedure lRenameFile(var F: lFile; const NewName: String);
  begin
  Rename(F.F, NewName);
  end;
procedure lRenameText(var T: lText; const NewName: String);
  begin
  Rename(T.T, NewName);
  end;
procedure lChangeFileName(const Name, NewName: String);
  var
    F: file;
  begin
  Assign(F, Name);
  Rename(F, NewName);
  end;
procedure lGetFAttr(var F: lFile; var Attr: Word);
  begin
  Dos.GetFAttr(F.F, Attr);
  end;
procedure lSetFAttr(var F: lFile; Attr: Word);
  begin
  Dos.SetFAttr(F.F, Attr);
  end;
procedure lGetTAttr(var T: lText; var Attr: Word);
  begin
  Dos.GetFAttr(T.T, Attr);
  end;
procedure lSetTAttr(var T: lText; Attr: Word);
  begin
  Dos.SetFAttr(T.T, Attr);
  end;
procedure lMkDir(const Path: String);
  begin
  MkDir(Path);
  end;

{AK155: � DN/2, �᫨ ��⠫�� ����� ��ਡ�� ReadOnly, � �� FAT ��� HPFS
�� 㤠����� ��ଠ�쭮, � �� FAT32 - �� 㤠�����. ��� �� �� ��直�
��砩 ���� ReadOnly ����. }
procedure lRmDir(const Path: String);
  var
    f: file;
    Attr: Word;
  begin
  Assign(f, Path);
  Dos.GetFAttr(f, Attr);
  if Attr and ReadOnly <> 0 then
    Dos.SetFAttr(f, Attr and not ReadOnly);
  if DosError <> 0 then
    begin
    InOutRes := DosError;
    Exit;
    end;
  RmDir(Path);
  end;
{/AK155}

{Cat: Windows ���������� ⥪�騩 ��⠫�� ⮫쪮 ��� ⥪�饣� ��᪠;
����������� ��⠫��� ⥪��� ��⠫���� ��室���� ���� �� ᥡ�}
{$IFDEF WIN32}
var
  CurrentPaths: array[1..1+Byte('Z')-Byte('A')] of PathStr;
  {$ENDIF}

function lFExpand(const Path: String): String;
  var
    D: Byte;
  begin
  {$IFDEF WIN32}
  if  (Length(Path) = 2) and (Path[2] = ':') then
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
  if  (InOutRes = 0) and (Length(Path) > 2) and (Path[2] = ':') then
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
    D := Byte(UpCase(Path[1]))-Byte('A')+1;
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

end.
