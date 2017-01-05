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
unit arc_ZXZ; {ZXZ}

interface

uses
  Archiver, advance, advance1, Objects;

type
  PZXZArchive = ^TZXZArchive;
  TZXZArchive = object(TARJArchive)
    Constructor Init;
    procedure GetFile; virtual;
    function GetID: byte; virtual;
    function GetSign: TStr4; virtual;
    end;

type
  ZXZHdr = record
    Name: array[0..7] of Char;
    Extension: array[0..2] of Char;
    OriginSize: AWord;
    SectorSize: byte;
    PackedSize: AWord;
    CRC32: longInt;
    MethodID: byte;
    Flags: byte;
    end;

implementation

{ ------------------------------ ZXZip aka $Z ----------------------------- }

Constructor TZXZArchive.Init;
  var
    Sign: TStr5;
    Q: String;
  begin
    Sign := GetSign;
    SetLength(Sign, Length(Sign)-1);
    Sign := Sign+#0;
    FreeStr := SourceDir+DNARC;
    TObject.Init;
    {$IFNDEF OS2}
    Packer := NewStr(GetVal(@Sign[1], @FreeStr[1], PPacker,
      'ZXZIP386.EXE'));
    UnPacker := NewStr(GetVal(@Sign[1], @FreeStr[1], PUnPacker,
      'ZXUNZIP.EXE'));
    {$ELSE}
    Packer := NewStr(GetVal(@Sign[1], @FreeStr[1], PPacker,
      'ZXZIP2.EXE'));
    UnPacker := NewStr(GetVal(@Sign[1], @FreeStr[1], PUnPacker,
      'ZXUNZIP2.EXE'));
    {$ENDIF}
    Extract := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtract, ''));
    ExtractWP := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtractWP, ''));
    Add := NewStr(GetVal(@Sign[1], @FreeStr[1], PAdd, '-start8224'));
    Move := NewStr(GetVal(@Sign[1], @FreeStr[1], PMove, ''));
    Delete := NewStr(GetVal(@Sign[1], @FreeStr[1], PDelete, ''));
    Garble := NewStr(GetVal(@Sign[1], @FreeStr[1], PGarble, ''));
    Test := NewStr(GetVal(@Sign[1], @FreeStr[1], PTest, '-t'));
    IncludePaths := NewStr(GetVal(@Sign[1], @FreeStr[1],
      PIncludePaths, ''));
    ExcludePaths := NewStr(GetVal(@Sign[1], @FreeStr[1],
      PExcludePaths, ''));
    ForceMode := NewStr(GetVal(@Sign[1], @FreeStr[1], PForceMode, ''));
    RecoveryRec := NewStr(GetVal(@Sign[1], @FreeStr[1], PRecoveryRec, ''
      ));
    SelfExtract := NewStr(GetVal(@Sign[1], @FreeStr[1], PSelfExtract, ''
      ));
    Solid := NewStr(GetVal(@Sign[1], @FreeStr[1], PSolid, ''));
    RecurseSubDirs := NewStr(GetVal(@Sign[1], @FreeStr[1],
      PRecurseSubDirs, ''));
    SetPathInside := NewStr(GetVal(@Sign[1], @FreeStr[1],
      PSetPathInside, ''));
    StoreCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
      PStoreCompression, ''));
    FastestCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
      PFastestCompression, ''));
    FastCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
      PFastCompression, ''));
    NormalCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
      PNormalCompression, ''));
    GoodCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
      PGoodCompression, ''));
    UltraCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
      PUltraCompression, ''));
    ComprListChar := NewStr(GetVal(@Sign[1], @FreeStr[1],
      PComprListChar, ' '));
    ExtrListChar := NewStr(GetVal(@Sign[1], @FreeStr[1],
      PExtrListChar, ' '));

    Q := GetVal(@Sign[1], @FreeStr[1], PAllVersion, '0');
    AllVersion := Q <> '0';
    Q := GetVal(@Sign[1], @FreeStr[1], PPutDirs, '0');
    PutDirs := Q <> '0';
    {$IFDEF OS_DOS}
    Q := GetVal(@Sign[1], @FreeStr[1], PSwap, '1');
    Swap := Q <> '0';
    {$ELSE}
    Q := GetVal(@Sign[1], @FreeStr[1], PShortCmdLine, '1');
    ShortCmdLine := Q <> '0';
    {$ENDIF}
    {$IFNDEF OS2}
    Q := GetVal(@Sign[1], @FreeStr[1], PUseLFN, '0');
    UseLFN := Q <> '0';
    {$ENDIF}
  end { TZXZArchive.Init };

function TZXZArchive.GetID;
  begin
    GetID := arcZXZ;
  end;

function TZXZArchive.GetSign;
  begin
    GetSign := sigZXZ;
  end;

procedure TZXZArchive.GetFile;
  var
    fp: longInt;
    P: ZXZHdr;
    len: AWord;
  begin
    ArcFile^.Read(P, SizeOf(P));
    fp := ArcFile^.GetPos;
    if (ArcFile^.Status <> stOK) or (fp > 65280) then
      begin
        FileInfo.Last := 2;
        exit;
      end;
    if (P.Name[0] < #32) or
      (P.PackedSize > P.SectorSize*256) or
      ((P.PackedSize+fp-SizeOf(P)-17) > ArcFile^.GetSize) or
      (P.MethodID > 3) or
      (P.SectorSize = 0)
    then
      begin
        FileInfo.Last := 1;
        exit;
      end;
    FileInfo.FName := P.Name;
    DelRight(FileInfo.FName);
    if (P.Extension[0] <> 'B') and
      (P.Extension[1] >= #32) and (P.Extension[1] <= #127) and
      (P.Extension[2] >= #32) and (P.Extension[2] <= #127)
    then
      FileInfo.FName := FileInfo.FName+'.'+P.Extension
    else
      FileInfo.FName := FileInfo.FName+'.'+P.Extension[0];
    DelRight(FileInfo.FName);
    FileInfo.Last := 0;
    FileInfo.Attr := 0;
    if (P.OriginSize and $ff) = 0 then
      len := 0
    else
      len := 256;
    len := Trunc((len+P.OriginSize)/256);
    if len <> P.SectorSize then
      len := P.SectorSize*256
    else
      begin
        len := P.OriginSize;
        if P.Extension[0] = 'B' then
          len := 4+Ord(P.Extension[1])+Ord(P.Extension[2])*256;
      end;
    FileInfo.USize := longInt(len);
    FileInfo.PSize := longInt(P.PackedSize);
    FileInfo.Date := 0;
    ArcFile^.Seek(fp+P.PackedSize);
  end { TZXZArchive.GetFile };

end.
