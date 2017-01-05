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
unit arc_CHZ; {CHZ}

interface

uses
  Archiver, advance, advance1, Objects;

type
  PCHZArchive = ^TCHZArchive;
  TCHZArchive = object(TARJArchive)
    Constructor Init;
    procedure GetFile; virtual;
    function GetID: byte; virtual;
    function GetSign: TStr4; virtual;
    end;

type
  CHZHdr = record
    Id: array[1..4] of Char;
    PackedSize: longInt;
    OriginSize: longInt;
    Data: array[1..4] of byte;
    Date: longInt;
    QQQ: AWord;
    NameLen: AWord;
    end;

implementation

{ ----------------------------- CHZ ------------------------------------}

Constructor TCHZArchive.Init;
  var
    Sign: TStr5;
    Q: String;
  begin
    Sign := GetSign;
    SetLength(Sign, Length(Sign)-1);
    Sign := Sign+#0;
    FreeStr := SourceDir+DNARC;
    TObject.Init;
    Packer := NewStr(GetVal(@Sign[1], @FreeStr[1], PPacker,
      'CHARC.EXE'));
    UnPacker := NewStr(GetVal(@Sign[1], @FreeStr[1], PUnPacker,
      'CHARC.EXE'));
    Extract := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtract, '-E'));
    ExtractWP := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtractWP,
      '-E'));
    Add := NewStr(GetVal(@Sign[1], @FreeStr[1], PAdd, '-A -T'));
    Move := NewStr(GetVal(@Sign[1], @FreeStr[1], PMove, '-A -T -M'));
    Delete := NewStr(GetVal(@Sign[1], @FreeStr[1], PDelete, '-D'));
    Garble := NewStr(GetVal(@Sign[1], @FreeStr[1], PGarble, ''));
    Test := NewStr(GetVal(@Sign[1], @FreeStr[1], PTest, ''));
    IncludePaths := NewStr(GetVal(@Sign[1], @FreeStr[1],
      PIncludePaths, ''));
    ExcludePaths := NewStr(GetVal(@Sign[1], @FreeStr[1],
      PExcludePaths, ''));
    ForceMode := NewStr(GetVal(@Sign[1], @FreeStr[1], PForceMode,
      '-Y'));
    RecoveryRec := NewStr(GetVal(@Sign[1], @FreeStr[1], PRecoveryRec, ''
      ));
    SelfExtract := NewStr(GetVal(@Sign[1], @FreeStr[1], PSelfExtract,
      '-S'));
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
    Q := GetVal(@Sign[1], @FreeStr[1], PPutDirs, '1');
    PutDirs := Q <> '0';
    {$IFDEF OS_DOS}
    Q := GetVal(@Sign[1], @FreeStr[1], PSwap, '1');
    Swap := Q <> '0';
    {$ELSE}
    Q := GetVal(@Sign[1], @FreeStr[1], PShortCmdLine, '2');
    ShortCmdLine := Q <> '0';
    {$ENDIF}
    {$IFNDEF OS2}
    Q := GetVal(@Sign[1], @FreeStr[1], PUseLFN, '0');
    UseLFN := Q <> '0';
    {$ENDIF}
  end { TCHZArchive.Init };

function TCHZArchive.GetID;
  begin
    GetID := arcCHZ;
  end;

function TCHZArchive.GetSign;
  begin
    GetSign := sigCHZ;
  end;

procedure TCHZArchive.GetFile;
  var
    fp: longInt;
    P: CHZHdr;
    s: String;
    C: Char;

  label 1;
  begin
1:
    fp := ArcFile^.GetPos;
    if fp = ArcFile^.GetSize then
      begin
        FileInfo.Last := 1;
        exit;
      end;
    ArcFile^.Read(P, 4);
    if (ArcFile^.Status <> stOK) or (Copy(P.Id, 1, 3) <> 'SCh')
    then
      begin
        FileInfo.Last := 2;
        exit;
      end;
    if P.Id[4] = 'D' then
      begin
        ArcFile^.Seek(fp+9);
        ArcFile^.Read(s[0], 1);
        ArcFile^.Read(s[1], Length(s));
        CDir := CDir+s+'\';
        goto 1;
      end
    else if P.Id[4] = 'd' then
      begin
        if CDir <> '' then
          begin
            SetLength(CDir, Length(CDir)-1);
            while (CDir <> '') and (CDir[Length(CDir)] <> '\') do
                SetLength(CDir, Length(CDir)-1);
          end;
        goto 1;
      end;
    ArcFile^.Read(P.PackedSize, SizeOf(P)-4);
    if (ArcFile^.Status <> stOK) then
      begin
        FileInfo.Last := 2;
        exit;
      end;
    {if (P.Method > 20) then begin FileInfo.Last:=2;Exit;end;}
    FileInfo.Last := 0;
    FileInfo.Attr := 0;
    FileInfo.USize := P.OriginSize;
    FileInfo.PSize := P.PackedSize;
    FileInfo.Date := P.Date {P.Date shl 16) or (P.Date shr 16)};
    if P.NameLen > 255 then
      P.NameLen := 255;
    FileInfo.FName[0] := Char(P.NameLen);
    ArcFile^.Read(FileInfo.FName[1], P.NameLen);
    FileInfo.FName := CDir+FileInfo.FName;
    ArcFile^.Seek(fp+P.PackedSize);
  end { TCHZArchive.GetFile };

end.
