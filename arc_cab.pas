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
//  dn16rc1-Archivers_Optimization-diff154byMV.patch
//
//  2.0.0
//  dn223-Archivers_Optimization.patch
//
//  2.3.0
//  dn328-ARJ_ACE_defaults_remove_EXE_from_names.patch
//
//  3.7.0
//  dn370-archives(if)-improve_and_fix.patch
//
//  4.9.0
//
//////////////////////////////////////////////////////////////////////////}
{$I STDEFINE.INC}
unit Arc_CAB; {CAB}

interface
 uses Archiver, Advance, Advance1, Objects, LFNCol, Dos;

type
  PCABArchive = ^TCABArchive;
  TCABArchive = object(TARJArchive)
    FilesNumber:  LongInt;
    constructor Init;
    procedure GetFile; virtual;
    function GetID: Byte; virtual;
    function GetSign: TStr4; virtual;
  end;

type
  PCFHEADER = ^TCFHEADER;
  TCFHEADER = record
    signature:  LongInt;
    reserved1:  LongInt;
    cbCabinet:  LongInt;
    reserved2:  LongInt;
    coffFiles:  LongInt;
    reserved3:  LongInt;
    versionMinor: Byte;
    versionMajor: Byte;
    cFolders:   AWord;
    cFiles:     AWord;
    flags:      AWord;
    setID:      AWord;
    iCabinet:   AWord;
  end;


implementation
{ ---------------------- CAB (by Neverowsky A.)---------------------------}

constructor TCABArchive.Init;
var Sign: TStr5;
    q: String;
begin
  Sign := GetSign; Dec(Sign[0]); Sign := Sign+#0;
  FreeStr := SourceDir + DNARC;
  TObject.Init;
  Packer                := NewStr(GetVal(@Sign[1], @FreeStr[1], PPacker,             ''));
  UnPacker              := NewStr(GetVal(@Sign[1], @FreeStr[1], PUnPacker,           'EXTRACT'));
  Extract               := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtract,            '/A /L .\'));
  ExtractWP             := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtractWP,          '/A /L .\'));
  Add                   := NewStr(GetVal(@Sign[1], @FreeStr[1], PAdd,                ''));
  Move                  := NewStr(GetVal(@Sign[1], @FreeStr[1], PMove,               ''));
  Delete                := NewStr(GetVal(@Sign[1], @FreeStr[1], PDelete,             ''));
  Garble                := NewStr(GetVal(@Sign[1], @FreeStr[1], PGarble,             ''));
  Test                  := NewStr(GetVal(@Sign[1], @FreeStr[1], PTest,               ''));
  IncludePaths          := NewStr(GetVal(@Sign[1], @FreeStr[1], PIncludePaths,       ''));
  ExcludePaths          := NewStr(GetVal(@Sign[1], @FreeStr[1], PExcludePaths,       ''));
  ForceMode             := NewStr(GetVal(@Sign[1], @FreeStr[1], PForceMode,          ''));
  RecoveryRec           := NewStr(GetVal(@Sign[1], @FreeStr[1], PRecoveryRec,        ''));
  SelfExtract           := NewStr(GetVal(@Sign[1], @FreeStr[1], PSelfExtract,        ''));
  Solid                 := NewStr(GetVal(@Sign[1], @FreeStr[1], PSolid,              ''));
  RecurseSubDirs        := NewStr(GetVal(@Sign[1], @FreeStr[1], PRecurseSubDirs,     ''));
  StoreCompression      := NewStr(GetVal(@Sign[1], @FreeStr[1], PStoreCompression,   ''));
  FastestCompression    := NewStr(GetVal(@Sign[1], @FreeStr[1], PFastestCompression, ''));
  FastCompression       := NewStr(GetVal(@Sign[1], @FreeStr[1], PFastCompression,    ''));
  NormalCompression     := NewStr(GetVal(@Sign[1], @FreeStr[1], PNormalCompression,  ''));
  GoodCompression       := NewStr(GetVal(@Sign[1], @FreeStr[1], PGoodCompression,    ''));
  UltraCompression      := NewStr(GetVal(@Sign[1], @FreeStr[1], PUltraCompression,   ''));
  q := GetVal(@Sign[1], @FreeStr[1], PListChar, ' ');
  if q<>'' then ListChar := q[1] else ListChar:=' ';
  q := GetVal(@Sign[1], @FreeStr[1], PPassDirNames, '0');
  if q='0' then PassDirNames := False else PassDirNames := True;
  q := GetVal(@Sign[1], @FreeStr[1], PUseLFN, '1');
  if q='0' then UseLFN := False else UseLFN := True;
  FilesNumber := -1;
end;

function TCABArchive.GetID;
begin
  GetID := arcCAB;
end;

function TCABArchive.GetSign;
begin
  GetSign := sigCAB;
end;

Procedure TCABArchive.GetFile;
var
  C:    Char;
  FH: record
      cbFile:   LongInt;
      uoffFolderStart:  LongInt;
      iFolder:  AWord;
{     date:     AWord;
      time:     AWord; }
      DateTime: LongInt;
      attribs:  AWord;
{     u1  szName[]; }
    end;
  CFHEADER: TCFHEADER;
begin
  if (FilesNumber < 0) then begin
   ArcFile^.Read(CFHEADER,SizeOf(CFHEADER));
   FilesNumber := CFHeader.cFiles;
   ArcFile^.Seek(ArcPos+CFHeader.coffFiles);
  end;
  if (FilesNumber = 0) then begin FileInfo.Last := 1; Exit; end;
  Dec(FilesNumber);
  ArcFile^.Read(FH, SizeOf(FH));
  if (ArcFile^.Status <> 0) then begin FileInfo.Last:=2;Exit;end;
  FileInfo.FName := '';
  repeat
    ArcFile^.Read(C, 1);
    if C <> #0 then FileInfo.FName := FileInfo.FName + C;
  until (C = #0) or (Length(FileInfo.FName) > 100);
  if (Length(FileInfo.FName) > 100) or (FileInfo.FName = '')
    then begin FileInfo.Last := 2; Exit; end;
  FileInfo.Attr := FH.attribs and not Hidden;
  FileInfo.USize := FH.cbFile;
  FileInfo.PSize := FH.cbFile;
  FileInfo.Date := (FH.datetime shr 16) or (FH.DateTime shl 16);
  FileInfo.Last := 0;
end;

end.
