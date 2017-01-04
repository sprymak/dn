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
//  dn230-remove_GZip_compression_and_change_external_filter_view.patch
//
//  2.7.0
//
//////////////////////////////////////////////////////////////////////////}
{$I STDEFINE.INC}
unit Arc_tgz; {TGZ & TAZ & TAR.GZ}

interface
 uses Archiver, Advance, Advance1, Objects, LFNCol, Dos, xTime, Advance2;

type
    PTGZArchive = ^TTGZArchive;
    TTGZArchive = object(TARJArchive)
        constructor Init;
        procedure GetFile; virtual;
        function GetID: Byte; virtual;
        function GetSign: TStr4; virtual;
    end;

implementation
{ ----------------------------- TAR ------------------------------------}

constructor TTGZArchive.Init;
var Sign: TStr5;
    q: String;
begin
  Sign := GetSign; Dec(Sign[0]); Sign := Sign+#0;
  FreeStr := SourceDir + DNARC;
  TObject.Init;
{$IFNDEF OS2}
  Packer                := NewStr(GetVal(@Sign[1], @FreeStr[1], PPacker,             ''));
  UnPacker              := NewStr(GetVal(@Sign[1], @FreeStr[1], PUnPacker,           'UNTGZ.EXE'));
{$ELSE}
  Packer                := NewStr(GetVal(@Sign[1], @FreeStr[1], PPacker,             ''));
  UnPacker              := NewStr(GetVal(@Sign[1], @FreeStr[1], PUnPacker,           'UNTGZOS2.EXE'));
{$ENDIF}
  Extract               := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtract,            '-d'));
  ExtractWP             := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtractWP,          '-d'));
  Add                   := NewStr(GetVal(@Sign[1], @FreeStr[1], PAdd,                ''));
  Move                  := NewStr(GetVal(@Sign[1], @FreeStr[1], PMove,               ''));
  Delete                := NewStr(GetVal(@Sign[1], @FreeStr[1], PDelete,             ''));
  Garble                := NewStr(GetVal(@Sign[1], @FreeStr[1], PGarble,             ''));
  Test                  := NewStr(GetVal(@Sign[1], @FreeStr[1], PTest,               '-t'));
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
    Time: Longint;
    DT:   DateTime;
    Flag: Byte;
    C:    Char;
begin
 ArcFile^.Read(Time, SizeOf(Time));
 if ArcFile^.EOF then begin FileInfo.Last := 1; Exit; end;
 Flag := Time shr 24;
 ArcFile^.Read(Time, SizeOf(Time));
 GetUNIXDate(Time, DT.Year, DT.Month, DT.Day, DT.Hour, DT.Min, DT.Sec);
 PackTime(DT, FileInfo.Date);
 FileInfo.FName := '';
 if Flag and 8 = 0 then FileInfo.FName := GetSName(VArcFileName)
   else begin
    if Flag and 4 = 0 then Time := 10{skip 10 bytes}
      else begin
       ArcFile^.Read(Time, SizeOf(Time));
       Time := Time shr 16 + 12;
      end;
    ArcFile^.Seek(ArcPos + Time);
    repeat
     ArcFile^.Read(C, 1);
     if C <> #0 then FileInfo.FName := FileInfo.FName + C else Break;
    until ArcFile^.Status <> stOK;
   end;
 FileInfo.PSize := ArcFile^.GetSize;
 ArcFile^.Seek(FileInfo.PSize - 4);
 ArcFile^.Read(FileInfo.USize, SizeOf(FileInfo.USize));
 FileInfo.Attr := 0;
 FileInfo.Last := 0;
end;

end.
