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
unit arc_HA; {HA}

interface

uses
  Archiver, advance, advance1, Objects, Dos, xTime;

type
  PHAArchive = ^THAArchive;
  THAArchive = object(TARJArchive)
    Constructor Init;
    procedure GetFile; virtual;
    function GetID: byte; virtual;
    function GetSign: TStr4; virtual;
    end;

type
  HAHdr = record
    Method: byte;
    PackedSize: longInt;
    OriginSize: longInt;
    CRC: longInt;
    Date: longInt;
    end;

implementation
{ ----------------------------- HA ------------------------------------}

Constructor THAArchive.Init;
  var
    Sign: TStr5;
    Q: String;
  begin
    Sign := GetSign;
    SetLength(Sign, Length(Sign)-1);
    Sign := Sign+#0;
    FreeStr := SourceDir+DNARC;
    TObject.Init;
    Packer := NewStr(GetVal(@Sign[1], @FreeStr[1], PPacker, 'HA.EXE'));
    UnPacker := NewStr(GetVal(@Sign[1], @FreeStr[1], PUnPacker,
      'HA.EXE'));
    Extract := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtract, 'e'));
    ExtractWP := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtractWP,
      'x'));
    Add := NewStr(GetVal(@Sign[1], @FreeStr[1], PAdd, 'a'));
    Move := NewStr(GetVal(@Sign[1], @FreeStr[1], PMove, 'am'));
    Delete := NewStr(GetVal(@Sign[1], @FreeStr[1], PDelete, 'd'));
    Garble := NewStr(GetVal(@Sign[1], @FreeStr[1], PGarble, ''));
    Test := NewStr(GetVal(@Sign[1], @FreeStr[1], PTest, 't'));
    IncludePaths := NewStr(GetVal(@Sign[1], @FreeStr[1],
      PIncludePaths, '+d'));
    ExcludePaths := NewStr(GetVal(@Sign[1], @FreeStr[1],
      PExcludePaths, '+e'));
    ForceMode := NewStr(GetVal(@Sign[1], @FreeStr[1], PForceMode, ''));
    RecoveryRec := NewStr(GetVal(@Sign[1], @FreeStr[1], PRecoveryRec, ''
      ));
    SelfExtract := NewStr(GetVal(@Sign[1], @FreeStr[1], PSelfExtract, ''
      ));
    Solid := NewStr(GetVal(@Sign[1], @FreeStr[1], PSolid, ''));
    RecurseSubDirs := NewStr(GetVal(@Sign[1], @FreeStr[1],
      PRecurseSubDirs, '+r'));
    SetPathInside := NewStr(GetVal(@Sign[1], @FreeStr[1],
      PSetPathInside, ''));
    StoreCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
      PStoreCompression, '+0'));
    FastestCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
      PFastestCompression, '+1'));
    FastCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
      PFastCompression, '+1'));
    NormalCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
      PNormalCompression, '+1'));
    GoodCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
      PGoodCompression, '+2'));
    UltraCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
      PUltraCompression, '+2'));
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
    {$IFDEF OS2}
    Q := GetVal(@Sign[1], @FreeStr[1], PShortCmdLine, '0');
    {$ELSE}
    Q := GetVal(@Sign[1], @FreeStr[1], PShortCmdLine, '1');
    {$ENDIF}
    ShortCmdLine := Q <> '0';
    {$ENDIF}
    {$IFNDEF OS2}
    Q := GetVal(@Sign[1], @FreeStr[1], PUseLFN, '0');
    UseLFN := Q <> '0';
    {$ENDIF}
  end { THAArchive.Init };

function THAArchive.GetID;
  begin
    GetID := arcHA;
  end;

function THAArchive.GetSign;
  begin
    GetSign := sigHA;
  end;

procedure THAArchive.GetFile;
  var
    fp: longInt;
    P: HAHdr;
    C: Char;
    DT: DateTime;
  begin
    ArcFile^.Read(P, SizeOf(P));
    if (ArcFile^.Status <> stOK) then
      begin
        FileInfo.Last := 1;
        exit;
      end;
    FileInfo.Last := 0;
    FileInfo.Attr := 0;
    FileInfo.USize := P.OriginSize;
    FileInfo.PSize := P.PackedSize;
    GetUNIXDate(P.Date-14400, DT.Year, DT.Month, DT.Day, DT.Hour, DT.
      Min, DT.Sec);
    PackTime(DT, FileInfo.Date);
    FileInfo.FName := '';
    repeat
      ArcFile^.Read(C, 1);
      if C <> #0 then
        FileInfo.FName := FileInfo.FName+C;
    until (C = #0) or (Length(FileInfo.FName) > 77);
    repeat
      ArcFile^.Read(C, 1);
      if C <> #0 then
        FileInfo.FName := FileInfo.FName+C;
    until (C = #0) or (Length(FileInfo.FName) > 78);
    if Length(FileInfo.FName) > 79 then
      begin
        FileInfo.Last := 2;
        exit;
      end;
    Replace(#255, '\', FileInfo.FName);
    if P.Method and $0f = $0e then
      FileInfo.Attr := Directory;
    ArcFile^.Read(C, 1);
    fp := ArcFile^.GetPos+P.PackedSize+byte(C);
    if (_Cardinal(fp) > ArcFile^.GetSize) or (ArcFile^.Status <>
        stOK)
    then
      begin
        FileInfo.Last := 2;
        exit;
      end;
    ArcFile^.Seek(fp);
  end { THAArchive.GetFile };

end.
