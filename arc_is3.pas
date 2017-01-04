{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source 1.51.10
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
unit Arc_IS3; {IS3}

interface
 uses Archiver, Advance1, Objects{, FViewer}, Advance, LFNCol, Dos, lfn;

type
  PIS3Archive = ^TIS3Archive;
  TIS3Archive = object(TARJArchive)
    FoldersOffs : LongInt;
    FilesNumber : LongInt;
    constructor Init;
    procedure GetFile; virtual;
    function GetID: Byte; virtual;
    function GetSign: TStr4; virtual;
  end;

type IS3FileHdr = record
      HZ1        : Byte;
      FolderNum  : AWord;
      OriginSize : LongInt;
      PackedSize : LongInt;
      HZ2        : LongInt;
      DateTime   : LongInt;
      Attr       : LongInt;
      HZ3        : LongInt;
      HZ4        : AWord;
      NameLen    : Byte;
     end;

type IS3FolderHdr = record
      FileNumber : AWord;
      SizeOfHdr  : AWord;
      SizeOfName : AWord;
     end;

implementation
{ --- Z --- aka LIB --- aka InstallShield 3.00.xxx --- by piwamoto ------- }

constructor TIS3Archive.Init;
var Sign: TStr5;
    q: String;
begin
  Sign := GetSign; Dec(Sign[0]); Sign := Sign+#0;
  FreeStr := SourceDir + DNARC;
  TObject.Init;
  Packer                := NewStr(GetVal(@Sign[1], @FreeStr[1], PPacker,             'ICOMP.EXE'));
  UnPacker              := NewStr(GetVal(@Sign[1], @FreeStr[1], PUnPacker,           'ICOMP.EXE'));
  Extract               := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtract,            '-d'));
  ExtractWP             := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtractWP,          '-d -i'));
  Add                   := NewStr(GetVal(@Sign[1], @FreeStr[1], PAdd,                '-c'));
  Move                  := NewStr(GetVal(@Sign[1], @FreeStr[1], PMove,               '-c'));
  Delete                := NewStr(GetVal(@Sign[1], @FreeStr[1], PDelete,             '-r'));
  Garble                := NewStr(GetVal(@Sign[1], @FreeStr[1], PGarble,             ''));
  Test                  := NewStr(GetVal(@Sign[1], @FreeStr[1], PTest,               '-dt -i'));
  IncludePaths          := NewStr(GetVal(@Sign[1], @FreeStr[1], PIncludePaths,       ''));
  ExcludePaths          := NewStr(GetVal(@Sign[1], @FreeStr[1], PExcludePaths,       ''));
  ForceMode             := NewStr(GetVal(@Sign[1], @FreeStr[1], PForceMode,          ''));
  RecoveryRec           := NewStr(GetVal(@Sign[1], @FreeStr[1], PRecoveryRec,        ''));
  SelfExtract           := NewStr(GetVal(@Sign[1], @FreeStr[1], PSelfExtract,        ''));
  Solid                 := NewStr(GetVal(@Sign[1], @FreeStr[1], PSolid,              ''));
  RecurseSubDirs        := NewStr(GetVal(@Sign[1], @FreeStr[1], PRecurseSubDirs,     '-i'));
  StoreCompression      := NewStr(GetVal(@Sign[1], @FreeStr[1], PStoreCompression,   '-sn'));
  FastestCompression    := NewStr(GetVal(@Sign[1], @FreeStr[1], PFastestCompression, '-sl'));
  FastCompression       := NewStr(GetVal(@Sign[1], @FreeStr[1], PFastCompression,    '-sm'));
  NormalCompression     := NewStr(GetVal(@Sign[1], @FreeStr[1], PNormalCompression,  '-sh'));
  GoodCompression       := NewStr(GetVal(@Sign[1], @FreeStr[1], PGoodCompression,    '-sh'));
  UltraCompression      := NewStr(GetVal(@Sign[1], @FreeStr[1], PUltraCompression,   '-sh'));
  q := GetVal(@Sign[1], @FreeStr[1], PListChar, ' ');
  if q<>'' then ListChar := q[1] else ListChar:=' ';
  q := GetVal(@Sign[1], @FreeStr[1], PSwap, '1');
  if q='0' then Swap := False else Swap := True;
  q := GetVal(@Sign[1], @FreeStr[1], PUseLFN, '1');
  if q='0' then UseLFN := False else UseLFN := True;
  FoldersOffs:=-1;
  FilesNumber:=-1;
end;

function TIS3Archive.GetID;
begin
  GetID := arcIS3;
end;

function TIS3Archive.GetSign;
begin
  GetSign := sigIS3;
end;

Procedure TIS3Archive.GetFile;
var
    S,S1  : String;
    P     : IS3FileHdr;
    P1    : IS3FolderHdr;
    FP,FO : Longint;
    C     : Char;
    I     : Integer;
begin
 if FoldersOffs<0 then begin
   ArcFile^.Seek(ArcPos+$c);
   ArcFile^.Read(FP, SizeOf(FP));
   FilesNumber := FP and $ffff;
   ArcFile^.Seek(ArcPos+$29);
   ArcFile^.Read(FP, SizeOf(FP));
   FoldersOffs:=FP + ArcPos;
   ArcFile^.Seek(ArcPos+$33);
   ArcFile^.Read(FP, SizeOf(FP));
   FP:=FP + ArcPos;
   ArcFile^.Seek(FP);
 end;
 FP := ArcFile^.GetPos;
 if (FilesNumber = 0) then begin FileInfo.Last:=1;Exit;end;
 ArcFile^.Read(P, SizeOf(P));
 if (ArcFile^.Status <> stOK) then begin FileInfo.Last:=2;Exit;end;
 FO := FoldersOffs; S1 := ''; S := '';

 for I := 1 to P.NameLen do
  begin
   ArcFile^.Read(C,1);
   S1 := S1 + C;
  end;

 for I := 0 to P.FolderNum do
  begin
   ArcFile^.Seek(FO);
   ArcFile^.Read(P1, SizeOf(P1));
   FO := FO + P1.SizeOfHdr;
  end;
 FO := FO - P1.SizeOfHdr + SizeOf(P1);
 S[0] := Char(P1.SizeOfName and $ff);
 if S[0] = #0 then S := S1
   else
    begin
     ArcFile^.Seek(FO);
     ArcFile^.Read(S[1],P1.SizeOfName and $ff);
     S := S + '\' + S1;
    end;

 FileInfo.FName := S;
 FileInfo.LFN   := AddLFN(S); {DataCompBoy}
 FileInfo.Last  := 0;
 FileInfo.USize := P.OriginSize;
 FileInfo.PSize := P.PackedSize;
 FileInfo.Attr  := 0;
 FileInfo.Date  := (P.DateTime shr 16) or (P.DateTime shl 16);
 Dec (FilesNumber);
 ArcFile^.Seek(FP + SizeOf(P) + P.NameLen + 13);
end;

end.
