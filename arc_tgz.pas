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
unit Arc_tgz; {TGZ & TAZ & TAR.GZ}

interface

uses
  Archiver, Advance, Advance1, Objects, {$IFNDEF OS2}LFNCol,{$ENDIF} Dos, xTime, gzIO;

type
    PTGZArchive = ^TTGZArchive;
    TTGZArchive = object(TARJArchive)
        gzf: gzFile;
        constructor Init;
        destructor Done; virtual;
        procedure GetFile; virtual;
        function GetID: Byte; virtual;
        function GetSign: TStr4; virtual;
    end;

const MaxTName = 100;
      Txt_Word = 8;
      Txt_Long = 12;
      BLKSIZE  = 512;

type
     TARHdr = record
       FName: Array[1..MaxTName] of Char;
       Mode:  Array[1..TXT_WORD] of Char;
       uid:   Array[1..TXT_WORD] of Char;
       gid:   Array[1..TXT_WORD] of Char;
       size:  Array[1..TXT_LONG] of Char;
       mtime: Array[1..TXT_LONG] of Char;
       chksum: Array[1..TXT_WORD] of Char;
       filetype: Char;
       linkname: Array[1..MAXTNAME] of Char;
       case Byte of
        0: (
            (* old-fashion data & padding *)
             comment: Array[1..BLKSIZE-MAXTNAME-8-8-8-12-12-8-1-MAXTNAME-12-12] of Char;
             srcsum: Array[1..TXT_LONG] of Char;
             srclen: Array[1..TXT_LONG] of Char;
          );
        1: (
             (* System V extensions *)
             extent: Array[1..4] of Char;
             allext: Array[1..4] of Char;
             total: Array[1..TXT_LONG] of Char;
            );
         2: (
            (* P1003 & GNU extensions *)
             magic: Array[1..8] of Char;
             uname: Array[1..32] of Char;
             gname: Array[1..32] of Char;
             devmajor: Array[1..TXT_WORD] of Char;
             devminor: Array[1..TXT_WORD] of Char;
             (* the following fields are added gnu and NOT standard *)
             atime: Array[1..12] of Char;
             ctime: Array[1..12] of Char;
             offset: Array[1..12] of Char;
            );
     end;

implementation
{ ----------------------------- TAR ------------------------------------}

constructor TTGZArchive.Init;
var Sign: TStr5;
    q: String;
begin
  TObject.Init;
  FreeObject(ArcFile);
  gzf := gzOpen(ArcFileName, 'r');
  Sign := GetSign; SetLength(Sign, Length(Sign)-1); Sign := Sign+#0;
  FreeStr := SourceDir + DNARC;
{$IFDEF WIN32}
  Packer                := NewStr(GetVal(@Sign[1], @FreeStr[1], PPacker,             '7Z.EXE'));
  UnPacker              := NewStr(GetVal(@Sign[1], @FreeStr[1], PUnPacker,           '7Z.EXE'));
  Extract               := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtract,            'e'));
  ExtractWP             := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtractWP,          'x'));
  Add                   := NewStr(GetVal(@Sign[1], @FreeStr[1], PAdd,                'a -tgzip'));
  Move                  := NewStr(GetVal(@Sign[1], @FreeStr[1], PMove,               ''));
  Delete                := NewStr(GetVal(@Sign[1], @FreeStr[1], PDelete,             'd'));
  Garble                := NewStr(GetVal(@Sign[1], @FreeStr[1], PGarble,             '-p'));
  Test                  := NewStr(GetVal(@Sign[1], @FreeStr[1], PTest,               't'));
  IncludePaths          := NewStr(GetVal(@Sign[1], @FreeStr[1], PIncludePaths,       ''));
  ExcludePaths          := NewStr(GetVal(@Sign[1], @FreeStr[1], PExcludePaths,       ''));
  ForceMode             := NewStr(GetVal(@Sign[1], @FreeStr[1], PForceMode,          '-y'));
  RecoveryRec           := NewStr(GetVal(@Sign[1], @FreeStr[1], PRecoveryRec,        ''));
  SelfExtract           := NewStr(GetVal(@Sign[1], @FreeStr[1], PSelfExtract,        ''));
  Solid                 := NewStr(GetVal(@Sign[1], @FreeStr[1], PSolid,              ''));
  RecurseSubDirs        := NewStr(GetVal(@Sign[1], @FreeStr[1], PRecurseSubDirs,     '-r0'));
  StoreCompression      := NewStr(GetVal(@Sign[1], @FreeStr[1], PStoreCompression,   ''));
  FastestCompression    := NewStr(GetVal(@Sign[1], @FreeStr[1], PFastestCompression, ''));
  FastCompression       := NewStr(GetVal(@Sign[1], @FreeStr[1], PFastCompression,    ''));
  NormalCompression     := NewStr(GetVal(@Sign[1], @FreeStr[1], PNormalCompression,  ''));
  GoodCompression       := NewStr(GetVal(@Sign[1], @FreeStr[1], PGoodCompression,    ''));
  UltraCompression      := NewStr(GetVal(@Sign[1], @FreeStr[1], PUltraCompression,   '-mx'));
  ComprListchar         := NewStr(GetVal(@Sign[1], @FreeStr[1], PComprListchar,      '@'));
  ExtrListchar          := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtrListchar,       '@'));
{$ELSE}
  Packer                := NewStr(GetVal(@Sign[1], @FreeStr[1], PPacker,             'TAR.EXE'));
  UnPacker              := NewStr(GetVal(@Sign[1], @FreeStr[1], PUnPacker,           'TAR.EXE'));
  Extract               := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtract,            'xfz'));
  ExtractWP             := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtractWP,          'xfz'));
  Add                   := NewStr(GetVal(@Sign[1], @FreeStr[1], PAdd,                'cvzf'));
  Move                  := NewStr(GetVal(@Sign[1], @FreeStr[1], PMove,               'cvzf'));
  Delete                := NewStr(GetVal(@Sign[1], @FreeStr[1], PDelete,             'df'));
  Garble                := NewStr(GetVal(@Sign[1], @FreeStr[1], PGarble,             ''));
  Test                  := NewStr(GetVal(@Sign[1], @FreeStr[1], PTest,               'tzf'));
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
  ComprListchar         := NewStr(GetVal(@Sign[1], @FreeStr[1], PComprListchar,      ' '));
  ExtrListchar          := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtrListchar,       ' '));
{$ENDIF}

  q := GetVal(@Sign[1], @FreeStr[1], PAllVersion, '0');
  AllVersion := q <> '0';
  q := GetVal(@Sign[1], @FreeStr[1], PPutDirs, '0');
  PutDirs := q <> '0';
{$IFDEF OS_DOS}
  q := GetVal(@Sign[1], @FreeStr[1], PSwap, '1');
  Swap := q <> '0';
{$ELSE}
  q := GetVal(@Sign[1], @FreeStr[1], PShortCmdLine, '0');
  ShortCmdLine := q <> '0';
{$ENDIF}
{$IFNDEF OS2}
  q := GetVal(@Sign[1], @FreeStr[1], PUseLFN, '1');
  UseLFN := q <> '0';
{$ENDIF}
end;

destructor TTGZArchive.Done;
begin
 gzClose(gzf);
 inherited Done;
end;

function TTGZArchive.GetID;
begin
  GetID := arcTGZ;
end;

function TTGZArchive.GetSign;
begin
  GetSign := sigTGZ;
end;

Procedure TTGZArchive.GetFile;
  var
      Buffer: Array [0..BlkSize - 1] of Char;
      Hdr: TARHdr absolute Buffer;
      DT: DateTime;
      qq: longint;
begin
  if gzEOF(gzf) then begin FileInfo.Last := 1; Exit end;
  qq:=gzRead(gzf, @Buffer, BlkSize);
  if qq<>BlkSize then begin FileInfo.Last := 2; Exit end;
  FileInfo.Last := 0;
  FileInfo.FName := Hdr.FName + #0;
  SetLength(FileInfo.FName, PosChar(#0, FileInfo.FName)-1);
  if FileInfo.FName = '' then begin FileInfo.Last := 1; Exit end;
  FileInfo.USize := FromOct(Hdr.Size);
  FileInfo.PSize := FileInfo.USize;
  GetUNIXDate(FromOct(Hdr.mTime), DT.Year, DT.Month, DT.Day, DT.Hour, DT.Min, DT.Sec);
  PackTime(DT, FileInfo.Date);
  gzSeek(gzf, ((FileInfo.PSize + BlkSize - 1) div BlkSize) * BlkSize, SEEK_CUR);
end;

end.
