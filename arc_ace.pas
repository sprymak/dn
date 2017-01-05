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
unit Arc_ACE; {ACE}

interface
 uses Archiver, Advance1, Objects{, FViewer}, Advance, {$IFNDEF OS2}LFNCol,{$ENDIF} Dos;

type
  PACEArchive = ^TACEArchive;
  TACEArchive = object(TARJArchive)
    constructor Init;
    procedure GetFile; virtual;
    function GetID: Byte; virtual;
    function GetSign: TStr4; virtual;
  end;

type ACEFileHdr = record
      HeadCRC    : AWord;
      HeadSize   : AWord;
      HeadType   : Byte;
      HeadFlags  : AWord;
      PackedSize : LongInt;
      OriginSize : LongInt;
      DateTime   : LongInt;
      Attr       : LongInt;
      CRC32      : LongInt;
      TechInfo   : LongInt;
      Reserved   : AWord;
      NameLen    : AWord;
     end;

implementation
{ ---------------------------------- ACE --------------------------------- }

constructor TACEArchive.Init;
var Sign: TStr5;
    q: String;
begin
  Sign := GetSign; SetLength(Sign, Length(Sign)-1); Sign := Sign+#0;
  FreeStr := SourceDir + DNARC;
  TObject.Init;
{$IFNDEF OS2}
  Packer                := NewStr(GetVal(@Sign[1], @FreeStr[1], PPacker,             'ACE.EXE'));
  UnPacker              := NewStr(GetVal(@Sign[1], @FreeStr[1], PUnPacker,           'ACE.EXE'));
{$ELSE}
  Packer                := NewStr(GetVal(@Sign[1], @FreeStr[1], PPacker,             'ACE2.EXE'));
  UnPacker              := NewStr(GetVal(@Sign[1], @FreeStr[1], PUnPacker,           'ACE2.EXE'));
{$ENDIF}
  Extract               := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtract,            'e'));
  ExtractWP             := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtractWP,          'x'));
  Add                   := NewStr(GetVal(@Sign[1], @FreeStr[1], PAdd,                'a'));
  Move                  := NewStr(GetVal(@Sign[1], @FreeStr[1], PMove,               'm'));
  Delete                := NewStr(GetVal(@Sign[1], @FreeStr[1], PDelete,             'd'));
  Test                  := NewStr(GetVal(@Sign[1], @FreeStr[1], PTest,               't'));
  Garble                := NewStr(GetVal(@Sign[1], @FreeStr[1], PGarble,             '-p'));
  IncludePaths          := NewStr(GetVal(@Sign[1], @FreeStr[1], PIncludePaths,       ''));
  ExcludePaths          := NewStr(GetVal(@Sign[1], @FreeStr[1], PExcludePaths,       '-ep'));
  ForceMode             := NewStr(GetVal(@Sign[1], @FreeStr[1], PForceMode,          ''));
  RecoveryRec           := NewStr(GetVal(@Sign[1], @FreeStr[1], PRecoveryRec,        '-rr'));
  SelfExtract           := NewStr(GetVal(@Sign[1], @FreeStr[1], PSelfExtract,        '-sfx'));
  Solid                 := NewStr(GetVal(@Sign[1], @FreeStr[1], PSolid,              '-s'));
  RecurseSubDirs        := NewStr(GetVal(@Sign[1], @FreeStr[1], PRecurseSubDirs,     ''));
  StoreCompression      := NewStr(GetVal(@Sign[1], @FreeStr[1], PStoreCompression,   '-m0'));
  FastestCompression    := NewStr(GetVal(@Sign[1], @FreeStr[1], PFastestCompression, '-m1'));
  FastCompression       := NewStr(GetVal(@Sign[1], @FreeStr[1], PFastCompression,    '-m2'));
  NormalCompression     := NewStr(GetVal(@Sign[1], @FreeStr[1], PNormalCompression,  '-m3'));
  GoodCompression       := NewStr(GetVal(@Sign[1], @FreeStr[1], PGoodCompression,    '-m4'));
  UltraCompression      := NewStr(GetVal(@Sign[1], @FreeStr[1], PUltraCompression,   '-m5'));
  ComprListchar         := NewStr(GetVal(@Sign[1], @FreeStr[1], PComprListchar,      '@'));
  ExtrListchar          := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtrListchar,       '@'));

  q := GetVal(@Sign[1], @FreeStr[1], PAllVersion, '0');
  AllVersion := q <> '0';
  q := GetVal(@Sign[1], @FreeStr[1], PPutDirs, '1');
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

function TACEArchive.GetID;
begin
  GetID := arcACE;
end;

function TACEArchive.GetSign;
begin
  GetSign := sigACE;
end;

Procedure TACEArchive.GetFile;
label 1;
var FP  : Longint;
    P   : ACEFileHdr;
    S   : String;
    C   : Char;
    I   : Integer;
begin
1:
 FP := ArcFile^.GetPos;
 if (FP = ArcFile^.GetSize) then begin FileInfo.Last:=1;Exit;end;
 ArcFile^.Read(P, SizeOf(P));
 if (ArcFile^.Status <> stOK) then begin FileInfo.Last:=2;Exit;end;
 if P.HeadType=1 then
  begin
   I := P.NameLen;
   S := '';
   repeat
    ArcFile^.Read(C,1);
    I := I - 1;
    S := S + C;
   until I=0;
   if P.Attr and Directory <> 0 then S := S + '\';
   FileInfo.FName := S; FileInfo.Last := 0;
{$IFNDEF OS2}
   FileInfo.LFN   := AddLFN(S); {DataCompBoy}
{$ENDIF}
   FileInfo.USize := P.OriginSize;
   FileInfo.PSize := P.PackedSize;
   FileInfo.Date := P.DateTime;
   FileInfo.Attr := Byte(P.Attr and not Hidden);
   if (P.HeadFlags and $4000)<>0 then FileInfo.Attr := FileInfo.Attr or Hidden;
  end;
 ArcFile^.Seek(FP + P.HeadSize + P.PackedSize + 4);
 if P.HeadType<>1 then goto 1;
end;

end.
