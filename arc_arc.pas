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
//
//////////////////////////////////////////////////////////////////////////}
{$I STDEFINE.INC}
unit Arc_ARC;{ARC}
interface
uses Archiver, Advance, Advance1, Objects, lfncol;

Type
    PARCArchive = ^TARCArchive;
    TARCArchive = object(TARJArchive)
        constructor Init;
        procedure GetFile; virtual;
        function GetID: Byte; virtual;
        function GetSign: TStr4; virtual;
    end;

type
     ARCHdr = record
      Mark: Char;
      Version: Byte;
      Name: Array[1..13] of Char;
      PackedSize: LongInt;
      Date: LongInt;
      CRC: AWord;
      OriginSize: LongInt;
     end;

implementation
{ ----------------------------- ARC ------------------------------------}

constructor TARCArchive.Init;
var Sign: TStr5;
    q: String;
begin
  Sign := GetSign; Dec(Sign[0]); Sign := Sign+#0;
  FreeStr := SourceDir + DNARC;
  TObject.Init;
  Packer                := NewStr(GetVal(@Sign[1], @FreeStr[1], PPacker,             'PAK.EXE'));
  UnPacker              := NewStr(GetVal(@Sign[1], @FreeStr[1], PUnPacker,           'PAK.EXE'));
  Extract               := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtract,            'E'));
  ExtractWP             := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtractWP,          'E /WA'));
  Add                   := NewStr(GetVal(@Sign[1], @FreeStr[1], PAdd,                'A'));
  Move                  := NewStr(GetVal(@Sign[1], @FreeStr[1], PMove,               'A /M'));
  Delete                := NewStr(GetVal(@Sign[1], @FreeStr[1], PDelete,             'D'));
  Garble                := NewStr(GetVal(@Sign[1], @FreeStr[1], PGarble,             '/G'));
  Test                  := NewStr(GetVal(@Sign[1], @FreeStr[1], PTest,               'T'));
  IncludePaths          := NewStr(GetVal(@Sign[1], @FreeStr[1], PIncludePaths,       ''));
  ExcludePaths          := NewStr(GetVal(@Sign[1], @FreeStr[1], PExcludePaths,       ''));
  ForceMode             := NewStr(GetVal(@Sign[1], @FreeStr[1], PForceMode,          ''));
  RecoveryRec           := NewStr(GetVal(@Sign[1], @FreeStr[1], PRecoveryRec,        ''));
  SelfExtract           := NewStr(GetVal(@Sign[1], @FreeStr[1], PSelfExtract,        '/EXE'));
  Solid                 := NewStr(GetVal(@Sign[1], @FreeStr[1], PSolid,              ''));
  RecurseSubDirs        := NewStr(GetVal(@Sign[1], @FreeStr[1], PRecurseSubDirs,     '/I'));
  StoreCompression      := NewStr(GetVal(@Sign[1], @FreeStr[1], PStoreCompression,   ''));
  FastestCompression    := NewStr(GetVal(@Sign[1], @FreeStr[1], PFastestCompression, '/C'));
  FastCompression       := NewStr(GetVal(@Sign[1], @FreeStr[1], PFastCompression,    '/S'));
  NormalCompression     := NewStr(GetVal(@Sign[1], @FreeStr[1], PNormalCompression,  '/ZS /BUGS'));
  GoodCompression       := NewStr(GetVal(@Sign[1], @FreeStr[1], PGoodCompression,    '/CR'));
  UltraCompression      := NewStr(GetVal(@Sign[1], @FreeStr[1], PUltraCompression,   '/O'));
  q := GetVal(@Sign[1], @FreeStr[1], PListChar, '@');
  if q<>'' then ListChar := q[1] else ListChar:=' ';
  q := GetVal(@Sign[1], @FreeStr[1], PSwap, '1');
  if q='0' then Swap := False else Swap := True;
  q := GetVal(@Sign[1], @FreeStr[1], PUseLFN, '0');
  if q='0' then UseLFN := False else UseLFN := True;
end;

function TARCArchive.GetID;
begin
  GetID := arcARC;
end;

function TARCArchive.GetSign;
begin
  GetSign := sigARC;
end;

Procedure TARCArchive.GetFile;
var i    : AWord;
    P    : ARCHdr;
begin
 ArcFile^.Read(P,2);
 if (P.Mark = ^Z) and (P.Version <> 0) and (ArcFile^.Status = stOK) then ArcFile^.Read(P.Name,SizeOf(P)-2);
 if (P.Version = 0) then begin FileInfo.Last:=1;Exit;end;
 if (ArcFile^.Status <> stOK) then begin FileInfo.Last:=2;Exit;end;
 i := 1;
 FileInfo.FName := '';
 While (I < 14) and (P.Name[I] <> #0) do
  begin FileInfo.FName := FileInfo.FName + P.Name[I]; Inc(I); end;
 FileInfo.Last := 0;
 FileInfo.Attr := 0;
 FileInfo.USize := P.OriginSize;
 FileInfo.PSize := P.PackedSize;
 FileInfo.Date := (P.Date shr 16) or (P.Date shl 16);
 ArcFile^.Seek(ArcFile^.GetPos + P.PackedSize);
end;

end.
