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
unit arc_ACE; {ACE}

interface

uses
  Archiver, advance, advance1, Objects, Dos;

type
  PACEArchive = ^TACEArchive;
  TACEArchive = object(TARJArchive)
    Constructor Init;
    procedure GetFile; virtual;
    function GetID: byte; virtual;
    function GetSign: TStr4; virtual;
    end;

type
  ACEFileHdr = record
    HeadCRC: AWord;
    HeadSize: AWord;
    HeadType: byte;
    HeadFlags: AWord;
    PackedSize: longInt;
    OriginSize: longInt;
    DateTime: longInt;
    Attr: longInt;
    CRC32: longInt;
    TechInfo: longInt;
    Reserved: AWord;
    NameLen: AWord;
    end;
  //  HeadFlags:
  //  bit  description
  //   0   1 (ADDSIZE field present)
  //   1   presence of file comment
  //
  //   12  file continued from previous volume
  //   13  file continues on the next volume
  //   14  file encrypted with password
  //   15  solid-flag: file compressed using data
  //       of previous files of the archive

var
  ACEVerToExtr: byte;

implementation
{ ---------------------------------- ACE --------------------------------- }

Constructor TACEArchive.Init;
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
      'ACE.EXE'));
    UnPacker := NewStr(GetVal(@Sign[1], @FreeStr[1], PUnPacker,
      'ACE.EXE'));
    {$ELSE}
    Packer := NewStr(GetVal(@Sign[1], @FreeStr[1], PPacker,
      'ACE2.EXE'));
    UnPacker := NewStr(GetVal(@Sign[1], @FreeStr[1], PUnPacker,
      'ACE2.EXE'));
    {$ENDIF}
    Extract := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtract, 'e'));
    ExtractWP := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtractWP,
      'x'));
    Add := NewStr(GetVal(@Sign[1], @FreeStr[1], PAdd, 'a'));
    Move := NewStr(GetVal(@Sign[1], @FreeStr[1], PMove, 'm'));
    Delete := NewStr(GetVal(@Sign[1], @FreeStr[1], PDelete, 'd'));
    Test := NewStr(GetVal(@Sign[1], @FreeStr[1], PTest, 't'));
    Garble := NewStr(GetVal(@Sign[1], @FreeStr[1], PGarble, '-p'));
    IncludePaths := NewStr(GetVal(@Sign[1], @FreeStr[1],
      PIncludePaths, ''));
    ExcludePaths := NewStr(GetVal(@Sign[1], @FreeStr[1],
      PExcludePaths, '-ep'));
    ForceMode := NewStr(GetVal(@Sign[1], @FreeStr[1], PForceMode, ''));
    RecoveryRec := NewStr(GetVal(@Sign[1], @FreeStr[1], PRecoveryRec,
      '-rr'));
    SelfExtract := NewStr(GetVal(@Sign[1], @FreeStr[1], PSelfExtract,
      '-sfx'));
    Solid := NewStr(GetVal(@Sign[1], @FreeStr[1], PSolid, '-s'));
    RecurseSubDirs := NewStr(GetVal(@Sign[1], @FreeStr[1],
      PRecurseSubDirs, ''));
    SetPathInside := NewStr(GetVal(@Sign[1], @FreeStr[1],
      PSetPathInside, ''));
    StoreCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
      PStoreCompression, '-m0'));
    FastestCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
      PFastestCompression, '-m1'));
    FastCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
      PFastCompression, '-m2'));
    NormalCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
      PNormalCompression, '-m3'));
    GoodCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
      PGoodCompression, '-m4'));
    UltraCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
      PUltraCompression, '-m5'));
    ComprListChar := NewStr(GetVal(@Sign[1], @FreeStr[1],
      PComprListChar, '@'));
    ExtrListChar := NewStr(GetVal(@Sign[1], @FreeStr[1],
      PExtrListChar, '@'));

    Q := GetVal(@Sign[1], @FreeStr[1], PAllVersion, '0');
    AllVersion := Q <> '0';
    Q := GetVal(@Sign[1], @FreeStr[1], PPutDirs, '1');
    PutDirs := Q <> '0';
    {$IFDEF OS_DOS}
    Q := GetVal(@Sign[1], @FreeStr[1], PSwap, '1');
    Swap := Q <> '0';
    {$ELSE}
    Q := GetVal(@Sign[1], @FreeStr[1], PShortCmdLine, '0');
    ShortCmdLine := Q <> '0';
    {$ENDIF}
    {$IFNDEF OS2}
    Q := GetVal(@Sign[1], @FreeStr[1], PUseLFN, '1');
    UseLFN := Q <> '0';
    {$ENDIF}
  end { TACEArchive.Init };

function TACEArchive.GetID;
  begin
    GetID := arcACE;
  end;

function TACEArchive.GetSign;
  begin
    GetSign := sigACE;
  end;

procedure TACEArchive.GetFile;
  label 1;
  var
    fp: longInt;
    P: ACEFileHdr;
    C: Char;
  begin
1:
    fp := ArcFile^.GetPos;
    if (fp = ArcFile^.GetSize) then
      begin
        FileInfo.Last := 1;
        exit;
      end;
    ArcFile^.Read(P, SizeOf(P));
    if (ArcFile^.Status <> stOK) then
      begin
        FileInfo.Last := 2;
        exit;
      end;
    if P.HeadType = 1 then
      begin
        FileInfo.FName := '';
        repeat
          ArcFile^.Read(C, 1);
          Dec(P.NameLen);
          FileInfo.FName := FileInfo.FName+C;
        until P.NameLen = 0;
        FileInfo.Last := 0;
        FileInfo.USize := P.OriginSize;
        FileInfo.PSize := P.PackedSize;
        FileInfo.Date := P.DateTime;
        FileInfo.Attr := byte(P.Attr and not Hidden);
        if (P.HeadFlags and $4000) <> 0 then
          FileInfo.Attr := FileInfo.Attr or Hidden;
      end;
    ArcFile^.Seek(fp+P.HeadSize+P.PackedSize+4);
    if P.HeadType <> 1 then
      goto 1;
  end { TACEArchive.GetFile };

end.
