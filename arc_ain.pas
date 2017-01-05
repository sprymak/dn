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
unit Arc_AIN; {AIN}

interface

uses
  Archiver, arc_uc2;

Type
     PAINArchive = ^TAINArchive;
     TAINArchive = object(TUC2Archive)
      constructor Init;
      procedure GetFile; virtual;
      function GetID: Byte; virtual;
      function GetSign: TStr4; virtual;
    end;

implementation

uses
  Objects, Advance2, Advance, DNApp, DNExec, Commands, Advance1, Messages,
  {$IFDEF MIRRORVARS} Vars, {$ENDIF}
  Dos;

{ ------------------------------- AIN ------------------------------------- }

constructor TAINArchive.Init;
var Sign: TStr5;
    q: String;
begin
  Sign := GetSign; SetLength(Sign, Length(Sign)-1); Sign := Sign+#0;
  FreeStr := SourceDir + DNARC;
  TObject.Init;
  Packer                := NewStr(GetVal(@Sign[1], @FreeStr[1], PPacker,             'AIN.EXE'));
  UnPacker              := NewStr(GetVal(@Sign[1], @FreeStr[1], PUnPacker,           'AIN.EXE'));
  Extract               := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtract,            'e'));
  ExtractWP             := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtractWP,          'x'));
  Add                   := NewStr(GetVal(@Sign[1], @FreeStr[1], PAdd,                'a'));
  Move                  := NewStr(GetVal(@Sign[1], @FreeStr[1], PMove,               'm'));
  Delete                := NewStr(GetVal(@Sign[1], @FreeStr[1], PDelete,             'd'));
  Test                  := NewStr(GetVal(@Sign[1], @FreeStr[1], PTest,               't'));
  Garble                := NewStr(GetVal(@Sign[1], @FreeStr[1], PGarble,             '-g'));
  IncludePaths          := NewStr(GetVal(@Sign[1], @FreeStr[1], PIncludePaths,       ''));
  ExcludePaths          := NewStr(GetVal(@Sign[1], @FreeStr[1], PExcludePaths,       ''));
  ForceMode             := NewStr(GetVal(@Sign[1], @FreeStr[1], PForceMode,          '-y'));
  RecoveryRec           := NewStr(GetVal(@Sign[1], @FreeStr[1], PRecoveryRec,        ''));
  SelfExtract           := NewStr(GetVal(@Sign[1], @FreeStr[1], PSelfExtract,        '-e'));
  Solid                 := NewStr(GetVal(@Sign[1], @FreeStr[1], PSolid,              ''));
  RecurseSubDirs        := NewStr(GetVal(@Sign[1], @FreeStr[1], PRecurseSubDirs,     ''));
  StoreCompression      := NewStr(GetVal(@Sign[1], @FreeStr[1], PStoreCompression,   '-m4'));
  FastestCompression    := NewStr(GetVal(@Sign[1], @FreeStr[1], PFastestCompression, '-m3'));
  FastCompression       := NewStr(GetVal(@Sign[1], @FreeStr[1], PFastCompression,    '-m3'));
  NormalCompression     := NewStr(GetVal(@Sign[1], @FreeStr[1], PNormalCompression,  '-m2'));
  GoodCompression       := NewStr(GetVal(@Sign[1], @FreeStr[1], PGoodCompression,    '-m1'));
  UltraCompression      := NewStr(GetVal(@Sign[1], @FreeStr[1], PUltraCompression,   '-m1'));
  ComprListchar         := NewStr(GetVal(@Sign[1], @FreeStr[1], PComprListchar,      '@'));
  ExtrListchar          := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtrListchar,       '@'));

  q := GetVal(@Sign[1], @FreeStr[1], PAllVersion, '0');
  AllVersion := q <> '0';
  q := GetVal(@Sign[1], @FreeStr[1], PPutDirs, '1');
  PutDirs := q <> '0';
{$IFDEF OS_DOS}
  q := GetVal(@Sign[1], @FreeStr[1], PSwap, '1');
  Swap := q <> '0';
{$ELSE}
  q := GetVal(@Sign[1], @FreeStr[1], PShortCmdLine, '1');
  ShortCmdLine := q <> '0';
{$ENDIF}
{$IFNDEF OS2}
  q := GetVal(@Sign[1], @FreeStr[1], PUseLFN, '0');
  UseLFN := q <> '0';
{$ENDIF}
end;

function TAINArchive.GetID;
begin
  GetID := arcAIN;
end;

function TAINArchive.GetSign;
begin
  GetSign := sigAIN;
end;

{
����� ����ࠨ����� �� ain 2.2

��� 䠩��� � �� �祭� �����묨 ������� �ଠ� ���������.
� �⮬ �� �ਬ�� ����� '�襭��' �஡���� y2k

TEMP\WINL                   5883  18.01.101  20:35:50

��� 䠩��� � ����� �����묨 ������� �ଠ� ���������:

TEMP\KBM35012\KMBR.BIN
                             338  20.08.97  20:43:38

}
Procedure TAINArchive.GetFile;
  var
    l: longint;
    DT:  DateTime;
    s: string;
    s1: string;
  begin
  if TextRec(ListFile).Handle = 0 then
    begin { ���� �맮�: �맮� ��娢��� ��� �뢮�� ���������� }
    FileInfo.Last := 2;
    ArcFile^.Close;
    ListFileName := MakeNormName(TempDir,'!!!DN!!!.TMP');
    S := '/C ' + SourceDir + 'dndosout.bat ' + ListFileName + ' '
       + UNPACKER^ + ' v '+ArcFileName;
    if Length(S) < 100 then
      AnsiExec(GetEnv('COMSPEC'), S)
    else
      MessageBox(^C+GetString(dlCmdLineTooLong), nil, mfOKButton+mfError);
    system.Assign(ListFile, ListFileName);
    system.Reset(ListFile);
    if IOResult <> 0 then
      exit;
    { �ய�� 蠯�� � �⥭�� ��ࢮ� ��ப� 䠩��� }
    repeat
      if eof(ListFile) then
        exit;
      readln(ListFile, s);
      if IOResult <> 0 then
        exit;
    until Pos('File name', s) <> 0;
    repeat
      if eof(ListFile) then
        exit;
      readln(ListFile, s);
      if IOResult <> 0 then
        exit;
    until s <> '';
    end
  else
    system.readln(ListFile, s);
  l := Pos(' ', s);
  if l = 1 then
    begin
    Close(ListFile);
    EraseFile(ListFileName);
    TextRec(ListFile).Handle := 0;
    FileInfo.Last := 1;
    exit;
    end;
  FileInfo.Last := 0;

  { �⥭�� ������ �� ��।��� 䠩��}
  if l = 0 then
    begin { ����� � ��祥 � ᫥���饩 ��ப� }
    FileInfo.FName := s;
    readln(ListFile, s);
    end
  else
    begin
    FileInfo.FName := Copy(s, 1, l-1);
    system.delete(s, 1, l);
    end;
  DelLeft(s);
  l := Pos(' ', s);
  FileInfo.USize := StoI(Copy(s, 1, l-1));
  FileInfo.PSize := FileInfo.USize;
  system.delete(s, 1, l); DelLeft(s);
  DT.Day := StoI(Copy(S,1,2));
  DT.Month := StoI(Copy(S,4,2));
  s1 := Copy(S,7,4); DelRight(s1);
  DT.Year := StoI(s1);
  if DT.Year < 1900 then
    inc(DT.Year, 1900);
  system.delete(s, 1, 10); DelLeft(s);
  DT.Hour := StoI(Copy(S,1,2));
  DT.Min := StoI(Copy(S,4,2));
  DT.Sec := StoI(Copy(S,7,4));
  PackTime(DT, FileInfo.Date);
  end;

end.
