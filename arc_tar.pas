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
//  dn328-ARJ_ACE_defaults_remove_EXE_from_names.patch
//
//  3.7.0
//
//////////////////////////////////////////////////////////////////////////}
{$I STDEFINE.INC}
unit Arc_tar; {TAR}

interface
 uses Archiver, Advance, Advance1, Objects, LFNCol, Dos, xTime;

type
    PTARArchive = ^TTARArchive;
    TTARArchive = object(TARJArchive)
        constructor Init;
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

constructor TTARArchive.Init;
var Sign: TStr5;
    q: String;
begin
  Sign := GetSign; Dec(Sign[0]); Sign := Sign+#0;
  FreeStr := SourceDir + DNARC;
  TObject.Init;
  Packer                := NewStr(GetVal(@Sign[1], @FreeStr[1], PPacker,             'TAR'));
  UnPacker              := NewStr(GetVal(@Sign[1], @FreeStr[1], PUnPacker,           'TAR'));
  Extract               := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtract,            'xf'));
  ExtractWP             := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtractWP,          'xf'));
  Add                   := NewStr(GetVal(@Sign[1], @FreeStr[1], PAdd,                'cvf'));
  Move                  := NewStr(GetVal(@Sign[1], @FreeStr[1], PMove,               'cvf'));
  Delete                := NewStr(GetVal(@Sign[1], @FreeStr[1], PDelete,             'df'));
  Garble                := NewStr(GetVal(@Sign[1], @FreeStr[1], PGarble,             ''));
  Test                  := NewStr(GetVal(@Sign[1], @FreeStr[1], PTest,               'tf'));
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
  q := GetVal(@Sign[1], @FreeStr[1], PSwap, '1');
  if q='0' then Swap := False else Swap := True;
  q := GetVal(@Sign[1], @FreeStr[1], PUseLFN, '0');
  if q='0' then UseLFN := False else UseLFN := True;
end;

function TTARArchive.GetID;
begin
  GetID := arcTAR;
end;

function TTARArchive.GetSign;
begin
  GetSign := sigTAR;
end;

Procedure TTARArchive.GetFile;
  var
      Buffer: Array [0..BlkSize - 1] of Char;
      Hdr: TARHdr absolute Buffer;
      DT: DateTime;
begin
  if ArcFile^.GetPos = ArcFile^.GetSize then begin FileInfo.Last := 1; Exit end;
  ArcFile^.Read(Buffer, BlkSize);
  if ArcFile^.Status <> stOK then begin FileInfo.Last := 2; Exit end;
  FileInfo.Last := 0;
  FileInfo.FName := Hdr.FName + #0;
  Byte(FileInfo.FName[0]) := PosChar(#0, FileInfo.FName)-1;
  if FileInfo.FName = '' then begin FileInfo.Last := 1; Exit end;
  FileInfo.USize := FromOct(Hdr.Size);
  FileInfo.PSize := FileInfo.USize;
  GetUNIXDate(FromOct(Hdr.mTime), DT.Year, DT.Month, DT.Day, DT.Hour, DT.Min, DT.Sec);
  PackTime(DT, FileInfo.Date);
  ArcFile^.Seek(ArcFile^.GetPos + ((FileInfo.PSize + BlkSize - 1) div BlkSize) * BlkSize);
end;

end.
