{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source 1.6.RC1
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
unit Arc_Zip; {ZIP}

interface
 uses Archiver, Advance1, Objects, FViewer, Advance, LFNCol, Dos, lfn;

type
 PZIPArchive = ^TZIPArchive;
 TZIPArchive = object(TARJArchive)
     constructor Init;
     procedure GetFile; virtual;
     function GetID: Byte; virtual;
     function GetSign: TStr4; virtual;
 end;

type
    TZIPLocalHdr = record
     ID: LongInt;
     Extract: AWord;
     GeneralPurpose: AWord;
     Method: AWord;
     LastModDate: LongInt;
     CRC32: LongInt;
     CompressedSize: LongInt;
     OriginalSize: LongInt;
     FNameLength: AWord;
     ExtraField: AWord;
    end;

implementation
uses U_Keymap;

{ ----------------------------- ZIP ------------------------------------}

constructor TZIPArchive.Init;
var Sign: TStr5;
    q: String;
begin
  Sign := GetSign; Dec(Sign[0]); Sign := Sign+#0;
  FreeStr := SourceDir + DNARC;
  TObject.Init;
{$IFNDEF OS2}
  Packer                := NewStr(GetVal(@Sign[1], @FreeStr[1], PPacker,             'PKZIP.EXE'));
  UnPacker              := NewStr(GetVal(@Sign[1], @FreeStr[1], PUnPacker,           'PKUNZIP.EXE'));
  Extract               := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtract,            ''));
  ExtractWP             := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtractWP,          '-d'));
  Add                   := NewStr(GetVal(@Sign[1], @FreeStr[1], PAdd,                '-a -wsh'));
  Move                  := NewStr(GetVal(@Sign[1], @FreeStr[1], PMove,               '-m -wsh'));
  Delete                := NewStr(GetVal(@Sign[1], @FreeStr[1], PDelete,             '-d'));
  Garble                := NewStr(GetVal(@Sign[1], @FreeStr[1], PGarble,             '-s'));
  Test                  := NewStr(GetVal(@Sign[1], @FreeStr[1], PTest,               '-t'));
  IncludePaths          := NewStr(GetVal(@Sign[1], @FreeStr[1], PIncludePaths,       '-P'));
  ExcludePaths          := NewStr(GetVal(@Sign[1], @FreeStr[1], PExcludePaths,       '-p'));
  ForceMode             := NewStr(GetVal(@Sign[1], @FreeStr[1], PForceMode,          ''));
  RecoveryRec           := NewStr(GetVal(@Sign[1], @FreeStr[1], PRecoveryRec,        ''));
  SelfExtract           := NewStr(GetVal(@Sign[1], @FreeStr[1], PSelfExtract,        ''));
  Solid                 := NewStr(GetVal(@Sign[1], @FreeStr[1], PSolid,              ''));
  RecurseSubDirs        := NewStr(GetVal(@Sign[1], @FreeStr[1], PRecurseSubDirs,     ''));
  StoreCompression      := NewStr(GetVal(@Sign[1], @FreeStr[1], PStoreCompression,   '-e0'));
  FastestCompression    := NewStr(GetVal(@Sign[1], @FreeStr[1], PFastestCompression, '-es'));
  FastCompression       := NewStr(GetVal(@Sign[1], @FreeStr[1], PFastCompression,    '-ef'));
  NormalCompression     := NewStr(GetVal(@Sign[1], @FreeStr[1], PNormalCompression,  '-en'));
  GoodCompression       := NewStr(GetVal(@Sign[1], @FreeStr[1], PGoodCompression,    '-ex'));
  UltraCompression      := NewStr(GetVal(@Sign[1], @FreeStr[1], PUltraCompression,   '-exx'));
  q := GetVal(@Sign[1], @FreeStr[1], PListChar, '@');
  if q<>'' then ListChar := q[1] else ListChar:=' ';
{$ELSE}
  Packer                := NewStr(GetVal(@Sign[1], @FreeStr[1], PPacker,             'ZIP.EXE'));
  UnPacker              := NewStr(GetVal(@Sign[1], @FreeStr[1], PUnPacker,           'UNZIP.EXE'));
  Extract               := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtract,            '-j -C'));
  ExtractWP             := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtractWP,          '-C'));
  Add                   := NewStr(GetVal(@Sign[1], @FreeStr[1], PAdd,                '-S'));
  Move                  := NewStr(GetVal(@Sign[1], @FreeStr[1], PMove,               '-m -S'));
  Delete                := NewStr(GetVal(@Sign[1], @FreeStr[1], PDelete,             '-d'));
  Garble                := NewStr(GetVal(@Sign[1], @FreeStr[1], PGarble,             '-P'));
  Test                  := NewStr(GetVal(@Sign[1], @FreeStr[1], PTest,               '-t -C'));
  IncludePaths          := NewStr(GetVal(@Sign[1], @FreeStr[1], PIncludePaths,       ''));
  ExcludePaths          := NewStr(GetVal(@Sign[1], @FreeStr[1], PExcludePaths,       '-j'));
  ForceMode             := NewStr(GetVal(@Sign[1], @FreeStr[1], PForceMode,          '-q'));
  RecoveryRec           := NewStr(GetVal(@Sign[1], @FreeStr[1], PRecoveryRec,        ''));
  SelfExtract           := NewStr(GetVal(@Sign[1], @FreeStr[1], PSelfExtract,        ''));
  Solid                 := NewStr(GetVal(@Sign[1], @FreeStr[1], PSolid,              ''));
  RecurseSubDirs        := NewStr(GetVal(@Sign[1], @FreeStr[1], PRecurseSubDirs,     '-r'));
  StoreCompression      := NewStr(GetVal(@Sign[1], @FreeStr[1], PStoreCompression,   '-0'));
  FastestCompression    := NewStr(GetVal(@Sign[1], @FreeStr[1], PFastestCompression, '-1'));
  FastCompression       := NewStr(GetVal(@Sign[1], @FreeStr[1], PFastCompression,    '-3'));
  NormalCompression     := NewStr(GetVal(@Sign[1], @FreeStr[1], PNormalCompression,  '-6'));
  GoodCompression       := NewStr(GetVal(@Sign[1], @FreeStr[1], PGoodCompression,    '-8'));
  UltraCompression      := NewStr(GetVal(@Sign[1], @FreeStr[1], PUltraCompression,   '-9'));
  q := GetVal(@Sign[1], @FreeStr[1], PListChar, ' ');
  if q<>'' then ListChar := q[1] else ListChar:=' ';
{$ENDIF}

  q := GetVal(@Sign[1], @FreeStr[1], PSwap, '1');
  if q='0' then Swap := False else Swap := True;
  q := GetVal(@Sign[1], @FreeStr[1], PUseLFN, '1');
  if q='0' then UseLFN := False else UseLFN := True;
end;

function TZIPArchive.GetID;
begin
  GetID := arcZIP;
end;

function TZIPArchive.GetSign;
begin
  GetSign := sigZIP;
end;

Procedure TZIPArchive.GetFile;
label 1;
var HS, I: AWord;
    FP,FPP: Longint;
    P: TZIPLocalHdr;
    s: String;
    nxl: TXLat;
begin
   FP := ArcFile^.GetPos;
1:
   ArcFile^.Read(P.ID,4);
   if (P.ID and $FFFF <> $4B50) or (ArcFile^.Status <> stOK) then
    begin
      FPP := FP;
      NullXLAT ( NXL );
      FP := SearchFileStr(@ArcFile^, NXL, 'PK'#03#04, FP, On, Off, Off, Off, Off);
      if FP > 0 then begin ArcFile^.Seek(FP); Goto 1 end;
      FP := SearchFileStr(@ArcFile^, NXL, 'PK'#01#02, FPP, On, Off, Off, Off, Off);
      if FP > 0 then begin ArcFile^.Seek(FP); Goto 1 end;
      FP := SearchFileStr(@ArcFile^, NXL, 'PK'#05#06, FPP, On, Off, Off, Off, Off);
      FileInfo.Last:=2;Exit;
    end;
   if (P.ID = $06054B50) or (P.ID = $02014b50) then begin FileInfo.Last:=1;Exit;end;
   ArcFile^.Read(P.Extract,SizeOf(P)-4);
   if (ArcFile^.Status <> stOK) or (P.FNameLength > 255) then
    begin FileInfo.Last:=2;Exit;end;
   ArcFile^.Read(S[1], P.FNameLength); S[0] := Char(P.FNameLength);
   While Pos('/', S) > 0 do S[Pos('/', S)] := '\';
   FileInfo.LFN := AddLFN(s);   {DataCompBoy}
   FileInfo.FName := S; {DataCompBoy}
   FileInfo.Last := 0;
   FileInfo.Attr := (P.GeneralPurpose and 1) * Hidden;
   FileInfo.USize := P.OriginalSize;
   FileInfo.PSize := P.CompressedSize;
   FileInfo.Date := P.LastModDate;
   ArcFile^.Seek(ArcFile^.GetPos + P.ExtraField + P.CompressedSize);
end;

end.
