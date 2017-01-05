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
unit arc_lim; {LIM}

interface

uses
  Archiver, advance, advance1, Objects, Dos;

type
  PLIMArchive = ^TLIMArchive;
  TLIMArchive = object(TARJArchive)
    Constructor Init;
    procedure GetFile; virtual;
    function GetID: byte; virtual;
    function GetSign: TStr4; virtual;
    end;

type
  LIMHdr = record
    Id: AWord;
    Method: byte;
    ThreeZeros: array[1..3] of byte;
    Date: longInt;
    ThreeSym: array[1..3] of byte;
    OriginSize: longInt;
    PackedSize: longInt;
    Data: longInt;
    end;

implementation

{ ----------------------------- LIM ------------------------------------}

Constructor TLIMArchive.Init;
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
      'LIMIT.EXE'));
    UnPacker := NewStr(GetVal(@Sign[1], @FreeStr[1], PUnPacker,
      'LIMIT.EXE'));
    Extract := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtract, 'e'));
    ExtractWP := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtractWP,
      'e -p'));
    Add := NewStr(GetVal(@Sign[1], @FreeStr[1], PAdd, 'a -whs'));
    Move := NewStr(GetVal(@Sign[1], @FreeStr[1], PMove, 'm -whs'));
    Delete := NewStr(GetVal(@Sign[1], @FreeStr[1], PDelete, 'Del'));
    Garble := NewStr(GetVal(@Sign[1], @FreeStr[1], PGarble, ''));
    Test := NewStr(GetVal(@Sign[1], @FreeStr[1], PTest, 't'));
    IncludePaths := NewStr(GetVal(@Sign[1], @FreeStr[1],
      PIncludePaths, '-p'));
    ExcludePaths := NewStr(GetVal(@Sign[1], @FreeStr[1],
      PExcludePaths, ''));
    ForceMode := NewStr(GetVal(@Sign[1], @FreeStr[1], PForceMode,
      '-y'));
    RecoveryRec := NewStr(GetVal(@Sign[1], @FreeStr[1], PRecoveryRec, ''
      ));
    SelfExtract := NewStr(GetVal(@Sign[1], @FreeStr[1], PSelfExtract, ''
      ));
    Solid := NewStr(GetVal(@Sign[1], @FreeStr[1], PSolid, ''));
    RecurseSubDirs := NewStr(GetVal(@Sign[1], @FreeStr[1],
      PRecurseSubDirs, '-r'));
    SetPathInside := NewStr(GetVal(@Sign[1], @FreeStr[1],
      PSetPathInside, ''));
    StoreCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
      PStoreCompression, '-m0'));
    FastestCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
      PFastestCompression, '-ms'));
    FastCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
      PFastCompression, '-ms'));
    NormalCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
      PNormalCompression, '-m1'));
    GoodCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
      PGoodCompression, '-mx'));
    UltraCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
      PUltraCompression, '-mx'));
    ComprListChar := NewStr(GetVal(@Sign[1], @FreeStr[1],
      PComprListChar, '@'));
    ExtrListChar := NewStr(GetVal(@Sign[1], @FreeStr[1],
      PExtrListChar, '@'));

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
  end { TLIMArchive.Init };

function TLIMArchive.GetID;
  begin
    GetID := arcLIM;
  end;

function TLIMArchive.GetSign;
  begin
    GetSign := sigLIM;
  end;

procedure TLIMArchive.GetFile;
  var
    i: AWord;
    P: LIMHdr;
    C: Char;
  label 1;

  procedure GetName;
    begin
      i := 1;
      FileInfo.FName := '';
      C := #1;
      while (i < 80) and (C <> #0) and not Abort and (ArcFile^.
          Status = stOK)
      do
        begin
          ArcFile^.Read(C, 1);
          if C <> #0 then
            FileInfo.FName := FileInfo.FName+C;
          Inc(i);
        end;
    end;

  begin { TLIMArchive.GetFile }
1:
    ArcFile^.Read(P, 4);
    if (ArcFile^.Status = stOK) and (P.Id = $F813) and (P.Method = 5)
    then
      begin
        FileInfo.Last := 1;
        exit;
      end;
    if (ArcFile^.Status = stOK) and (P.Id = $D180)
    then
      begin
        GetName;
        CDir := FileInfo.FName;
        goto 1;
      end;
    if (ArcFile^.Status <> stOK) or (P.Id <> $f123) then
      begin
        FileInfo.Last := 2;
        exit;
      end;
    ArcFile^.Read(P.ThreeZeros[2], SizeOf(P)-4);
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
    GetName;
    FileInfo.FName := CDir+'\'+FileInfo.FName;
    if P.ThreeZeros[3] and Directory <> 0 then
      goto 1;
    ArcFile^.Seek(ArcFile^.GetPos+P.PackedSize);
  end { TLIMArchive.GetFile };

end.
