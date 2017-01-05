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
unit Arc_UC2; {UC2}

interface
uses
  Archiver
  ;

type
    PUC2Archive = ^TUC2Archive;
    TUC2Archive = object(TARJArchive)
        ListFileName: string;
        ListFile: system.text;
        BaseDir: string;
        constructor Init;
        procedure GetFile; virtual;
        function GetID: Byte; virtual;
        function GetSign: TStr4; virtual;
        destructor Done; virtual;
    end;

implementation

uses
  objects, advance2, advance, dnapp
  , commands, Advance1, messages
  , Dos
{$IFDEF VIRTUALPASCAL}
  , VPSysLow
{$ENDIF}
;
{ ----------------------------- UC2 ------------------------------------}

constructor TUC2Archive.Init;
var Sign: TStr5;
    q: String;
begin
  Sign := GetSign; SetLength(Sign, Length(Sign)-1); Sign := Sign+#0;
  FreeStr := SourceDir + DNARC;
  TObject.Init;
  Packer                := NewStr(GetVal(@Sign[1], @FreeStr[1], PPacker,             'UC.EXE'));
  UnPacker              := NewStr(GetVal(@Sign[1], @FreeStr[1], PUnPacker,           'UC.EXE'));
  Extract               := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtract,            'E'));
  ExtractWP             := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtractWP,          'E !NOF ##.'));
  Add                   := NewStr(GetVal(@Sign[1], @FreeStr[1], PAdd,                'A'));
  Move                  := NewStr(GetVal(@Sign[1], @FreeStr[1], PMove,               'AM'));
  Delete                := NewStr(GetVal(@Sign[1], @FreeStr[1], PDelete,             'D'));
  Garble                := NewStr(GetVal(@Sign[1], @FreeStr[1], PGarble,             ''));
  Test                  := NewStr(GetVal(@Sign[1], @FreeStr[1], PTest,               'T'));
  IncludePaths          := NewStr(GetVal(@Sign[1], @FreeStr[1], PIncludePaths,       ''));
  ExcludePaths          := NewStr(GetVal(@Sign[1], @FreeStr[1], PExcludePaths,       ''));
  ForceMode             := NewStr(GetVal(@Sign[1], @FreeStr[1], PForceMode,          '-F'));
  RecoveryRec           := NewStr(GetVal(@Sign[1], @FreeStr[1], PRecoveryRec,        ''));
  SelfExtract           := NewStr(GetVal(@Sign[1], @FreeStr[1], PSelfExtract,        ''));
  Solid                 := NewStr(GetVal(@Sign[1], @FreeStr[1], PSolid,              ''));
  RecurseSubDirs        := NewStr(GetVal(@Sign[1], @FreeStr[1], PRecurseSubDirs,     ''));
  StoreCompression      := NewStr(GetVal(@Sign[1], @FreeStr[1], PStoreCompression,   ''));
  FastestCompression    := NewStr(GetVal(@Sign[1], @FreeStr[1], PFastestCompression, '-TF'));
  FastCompression       := NewStr(GetVal(@Sign[1], @FreeStr[1], PFastCompression,    '-TF'));
  NormalCompression     := NewStr(GetVal(@Sign[1], @FreeStr[1], PNormalCompression,  '-TN'));
  GoodCompression       := NewStr(GetVal(@Sign[1], @FreeStr[1], PGoodCompression,    '-TT'));
  UltraCompression      := NewStr(GetVal(@Sign[1], @FreeStr[1], PUltraCompression,   '-TT'));
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

function TUC2Archive.GetID;
begin
  GetID := arcUC2;
end;

function TUC2Archive.GetSign;
begin
  GetSign := sigUC2;
end;

Procedure TUC2Archive.GetFile;
  const
    FuckName = 'U$~RESLT.OK';
  var
    S: String;
    s1: string;

  procedure ReadName;
    begin
    system.readln(ListFile, s); {NAME=[*]}
    FileInfo.FName := BaseDir + Copy(s, 13, length(s)-13);
    system.readln(ListFile, s);
    if s[7] = 'L' then
      system.readln(ListFile, s);
    end;

  procedure ReadDTA;
    var
      DT:  DateTime;
    begin
    { DATE(MDY)= }
    DT.Month := StoI(Copy(S,17,2));
    DT.Day := StoI(Copy(S,20,2));
    DT.Year := StoI(Copy(S,23,4));
    system.readln(ListFile, s);
    { TIME(HMS)= }
    DT.Hour := StoI(Copy(S,17,2));
    DT.Min := StoI(Copy(S,20,2));
    DT.Sec := StoI(Copy(S,23,4));
    PackTime(DT, FileInfo.Date);
    system.readln(ListFile, s);
    { ATTRIB= }
    FileInfo.Attr := 0;
    end;
  label
    NextRecord;
  begin
  if TextRec(ListFile).Handle = 0 then
    begin { первый вызов: вызов архиватора для вывода оглавления }
    ListFileName := MakeNormName(TempDir,'!!!DN!!!.TMP');
    S := '/C ' + SourceDir + 'dndosout.bat ' + ListFileName + ' '
       + UNPACKER^ + ' ~D '+ArcFileName;
    if Length(S) < 100 then
      begin
        exec(GetEnv('Comspec'), S);
       {$IFDEF VIRTUALPASCAL}
        SysTVKbdInit;
       {$ENDIF}
      end
        else messagebox(^C+GetString(dlCmdLineTooLong), nil, mfOKButton+mfError);
    if not ExistFile(FuckName) then
    begin
      FileInfo.Last := 1;
      MessageBox(GetString(dlArcMsg6),NIL,mfOkButton or mfError);
      Exit;
    end;
    EraseFile(FuckName);
    system.Assign(ListFile, ListFileName);
    system.Reset(ListFile);
    end;
  FileInfo.Last := 0;
  { чтение данных об очередном файле}
NextRecord:
  repeat
    system.readln(ListFile, s); s1 := Copy(s, 1, 7);
  until s1 <> '      T'; {TAG=[]}
//  if s1 = 'LIST [\' then
  while s1 = 'LIST [\' do
    begin
    BaseDir := Copy(s, 7, length(s)-7);
    system.readln(ListFile, s); s1 := Copy(s, 1, 7);
{    FileInfo.FName := BaseDir;
    FileInfo.USize := 0;
    FileInfo.PSize := 0;
    exit;}
    end;
  if s1 = 'END' then
    begin
    Close(ListFile);
    EraseFile(ListFileName);
    TextRec(ListFile).Handle := 0;
    FileInfo.Last := 1;
    exit;
    end;
  if s1 = '   DIR' then
    begin
    ReadName;
    ReadDTA;
//    FileInfo.Attr := Directory;
    FileInfo.FName := FileInfo.FName + '\';
    FileInfo.USize := 0;
    FileInfo.PSize := 0;
    end
  else if s1 = '   FILE' then
    begin
    ReadName;
    {VERSION=}
    system.delete(s, 1, 14);
    if s <> '0' then
      begin
      if not AllVersion then
        begin { игнорируем этот файл }
          repeat readln(ListFile, s);
          until s[7] = 'A';
          goto NextRecord;
        end;
      if length(s)=1 then
        S := '0'+S;
      FileInfo.FName := FileInfo.FName + ';' + s;
      end;
    {SIZE=}
    system.readln(ListFile, s);
    system.delete(s, 1, 11);
    FileInfo.USize := StoI(S);
    FileInfo.PSize := FileInfo.USize;
    system.readln(ListFile, s);
    if s[7] = 'C' then
      system.readln(ListFile, s); {CHECK=...}
        { uc 2.0 этой строки не формирует }
    ReadDTA;
    end
  else
    FileInfo.Last := 2;

  end;

destructor TUC2Archive.Done;
  begin
  if TextRec(ListFile).Handle <> 0 then
    begin
    system.Close(ListFile);
    EraseFile(ListFileName);
    end;
  inherited Done;
  end;

end.

